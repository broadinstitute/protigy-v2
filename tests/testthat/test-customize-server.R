# Tests for the customize tab module's reactive logic via shiny::testServer.
#
# Covers:
#   * Initialization from globals$colors, structural-refresh invariant
#   * Restore / Reset / Import / Undo handlers
#   * Per-picker observers (multi-ome sync vs per-ome)
#   * Apply preset palette
#   * Cross-tab return contract (returns a reactive that mirrors current_colors)

# ---------------------------------------------------------------------------
# Fixture builders
# ---------------------------------------------------------------------------

make_fixture_colors <- function() {
  list(
    multi_ome = list(
      treatment = list(is_discrete = TRUE,
                       vals = c("control", "drug_A", "drug_B"),
                       colors = c("#4477AA", "#EE6677", "#228833")),
      tissue = list(is_discrete = TRUE,
                    vals = c("normal", "tumor"),
                    colors = c("#CCBB44", "#AA3377"))
    ),
    proteome = list(
      treatment = list(is_discrete = TRUE,
                       vals = c("control", "drug_A", "drug_B"),
                       colors = c("#4477AA", "#EE6677", "#228833")),
      tissue = list(is_discrete = TRUE,
                    vals = c("normal", "tumor"),
                    colors = c("#CCBB44", "#AA3377"))
    )
  )
}

# Bare-bones GCTs_and_params reactive value builder. Some handlers (Reset)
# call make_custom_colors(), so we provide GCTs that exercise that path.
make_fixture_gcts_and_params <- function() {
  cdesc <- data.frame(
    treatment = c("control", "drug_A", "drug_B"),
    tissue    = c("normal", "tumor", "normal"),
    row.names = paste0("sample_", 1:3),
    stringsAsFactors = FALSE
  )
  rdesc <- data.frame(id = paste0("gene_", 1:3))
  mat   <- matrix(seq_len(9), nrow = 3, ncol = 3)
  rownames(mat) <- paste0("gene_", 1:3)
  colnames(mat) <- paste0("sample_", 1:3)

  gct <- new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc,
             rid = paste0("gene_", 1:3), cid = paste0("sample_", 1:3))

  list(GCTs = list(proteome = gct),
       GCTs_merged = gct,
       parameters = list())
}

# Stub shinyalert so callbackR is invoked synchronously with TRUE.
# Using `local_mocked_bindings` keeps the stub scoped to a single test.
stub_shinyalert <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    shinyalert = function(..., callbackR = NULL) {
      if (!is.null(callbackR)) callbackR(TRUE)
      invisible()
    },
    .package = "shinyalert",
    .env = env
  )
}

# Helper to create the reactive args testServer needs.
make_test_args <- function() {
  gcts_rv <- shiny::reactiveVal(make_fixture_gcts_and_params())
  globals <- shiny::reactiveValues(
    colors = make_fixture_colors(),
    default_annotations = list(proteome = "treatment")
  )
  list(GCTs_and_params = function() gcts_rv(), globals = globals,
       gcts_rv = gcts_rv)  # raw rv exposed so tests can mutate
}


# ---------------------------------------------------------------------------
# Init / structural refresh
# ---------------------------------------------------------------------------

test_that("init: globals$colors propagates to current_colors and restore_target", {
  args <- make_test_args()
  expected <- make_fixture_colors()
  shiny::testServer(
    customizeTabServer,
    args = list(GCTs_and_params = args$GCTs_and_params, globals = args$globals),
    {
      session$flushReact()
      expect_equal(current_colors(), expected)
      expect_equal(restore_target(), expected)
    }
  )
})


test_that("structural refresh: adding an ome refreshes current_colors", {
  args <- make_test_args()
  shiny::testServer(
    customizeTabServer,
    args = list(GCTs_and_params = args$GCTs_and_params, globals = args$globals),
    {
      session$flushReact()
      original <- current_colors()
      expect_true("proteome" %in% names(original))
      expect_false("phosphoproteome" %in% names(original))

      # Add a new ome to globals$colors — structural change.
      new_colors <- args$globals$colors
      new_colors$phosphoproteome <- new_colors$proteome
      args$globals$colors <- new_colors
      session$flushReact()

      expect_true("phosphoproteome" %in% names(current_colors()))
    }
  )
})


test_that("color-only edit on globals$colors does NOT clobber pending edits", {
  # Simulates app_server.R:39 writing back our own current_colors. Should be
  # a no-op (same structure → no refresh).
  args <- make_test_args()
  shiny::testServer(
    customizeTabServer,
    args = list(GCTs_and_params = args$GCTs_and_params, globals = args$globals),
    {
      session$flushReact()

      # Simulate user edit: change current_colors directly.
      cc <- current_colors()
      cc$multi_ome$treatment$colors[1] <- "#FF0000"
      current_colors(cc)
      session$flushReact()
      expect_equal(current_colors()$multi_ome$treatment$colors[1], "#FF0000")

      # Now simulate the parent writing globals$colors back from
      # current_colors() — same structure, different value at [1].
      args$globals$colors <- cc
      session$flushReact()

      # Edit must survive.
      expect_equal(current_colors()$multi_ome$treatment$colors[1], "#FF0000")
    }
  )
})


# ---------------------------------------------------------------------------
# Restore / Reset / Import
# ---------------------------------------------------------------------------

test_that("restore_defaults snaps current_colors back to restore_target", {
  args <- make_test_args()
  shiny::testServer(
    customizeTabServer,
    args = list(GCTs_and_params = args$GCTs_and_params, globals = args$globals),
    {
      stub_shinyalert()
      session$flushReact()
      saved <- restore_target()

      cc <- current_colors()
      cc$multi_ome$treatment$colors[1] <- "#FF0000"
      current_colors(cc)
      session$flushReact()
      expect_equal(current_colors()$multi_ome$treatment$colors[1], "#FF0000")

      session$setInputs(restore_defaults = 1)
      session$flushReact()
      expect_equal(current_colors(), saved)
    }
  )
})


test_that("reset_to_app_defaults regenerates from GCTs and overwrites restore_target", {
  args <- make_test_args()
  shiny::testServer(
    customizeTabServer,
    args = list(GCTs_and_params = args$GCTs_and_params, globals = args$globals),
    {
      stub_shinyalert()
      session$flushReact()
      session$setInputs(reset_to_app_defaults = 1)
      session$flushReact()

      # current_colors should now be a freshly-generated app default scheme,
      # which has the structure produced by make_custom_colors (multi_ome key).
      expect_true("multi_ome" %in% names(current_colors()))
      # restore_target should equal current_colors after reset.
      expect_equal(current_colors(), restore_target())
    }
  )
})


test_that("import: malformed YAML leaves current_colors unchanged", {
  args <- make_test_args()
  shiny::testServer(
    customizeTabServer,
    args = list(GCTs_and_params = args$GCTs_and_params, globals = args$globals),
    {
      stub_shinyalert()
      session$flushReact()
      before <- current_colors()

      # Write malformed YAML
      tf <- tempfile(fileext = ".yaml")
      writeLines("colors: [this is: not valid: yaml", tf)

      session$setInputs(import_yaml = list(
        name = "bad.yaml",
        size = file.size(tf),
        type = "application/yaml",
        datapath = tf
      ))
      session$flushReact()

      expect_equal(current_colors(), before)
      unlink(tf)
    }
  )
})


test_that("import: valid YAML updates current_colors AND restore_target", {
  args <- make_test_args()
  shiny::testServer(
    customizeTabServer,
    args = list(GCTs_and_params = args$GCTs_and_params, globals = args$globals),
    {
      stub_shinyalert()
      session$flushReact()

      tf <- tempfile(fileext = ".yaml")
      writeLines("
colors:
  multi_ome:
    treatment:
      control: '#000001'
      drug_A: '#000002'
      drug_B: '#000003'
", tf)

      session$setInputs(import_yaml = list(
        name = "good.yaml", size = file.size(tf),
        type = "application/yaml", datapath = tf
      ))
      session$flushReact()

      expect_equal(current_colors()$multi_ome$treatment$colors,
                   c("#000001", "#000002", "#000003"))
      expect_equal(restore_target()$multi_ome$treatment$colors,
                   c("#000001", "#000002", "#000003"))
      expect_false(is.null(import_meta()))
      expect_equal(import_meta()$format, "ProTIGY")
      unlink(tf)
    }
  )
})


test_that("import: zero-match warns but does not change current_colors", {
  args <- make_test_args()
  shiny::testServer(
    customizeTabServer,
    args = list(GCTs_and_params = args$GCTs_and_params, globals = args$globals),
    {
      stub_shinyalert()
      session$flushReact()
      before <- current_colors()

      tf <- tempfile(fileext = ".yaml")
      writeLines("
colors:
  proteomeXX:
    treatment:
      control: '#FF0000'
", tf)

      session$setInputs(import_yaml = list(
        name = "nomatch.yaml", size = file.size(tf),
        type = "application/yaml", datapath = tf
      ))
      session$flushReact()

      expect_equal(current_colors(), before)
      unlink(tf)
    }
  )
})


# ---------------------------------------------------------------------------
# Per-picker observers — multi-ome sync vs per-ome scope
# ---------------------------------------------------------------------------

test_that("per-picker observe in multi_ome mode syncs across all omes", {
  args <- make_test_args()
  shiny::testServer(
    customizeTabServer,
    args = list(GCTs_and_params = args$GCTs_and_params, globals = args$globals),
    {
      session$flushReact()
      session$setInputs(
        color_mode = "multi_ome",
        selected_annotation_column = "treatment"
      )
      session$flushReact()
      # Force renderUI evaluation so per-picker observers register
      invisible(output$color_pickers_ui)
      session$flushReact()

      session$setInputs(color_multi_ome_treatment_1 = "#ABCDEF")
      session$flushReact()

      expect_equal(toupper(current_colors()$multi_ome$treatment$colors[1]), "#ABCDEF")
      expect_equal(toupper(current_colors()$proteome$treatment$colors[1]), "#ABCDEF")
    }
  )
})


test_that("per-picker observe in per_ome mode touches only the selected ome", {
  args <- make_test_args()
  shiny::testServer(
    customizeTabServer,
    args = list(GCTs_and_params = args$GCTs_and_params, globals = args$globals),
    {
      session$flushReact()
      session$setInputs(color_mode = "per_ome", selected_ome = "proteome",
                        selected_annotation_column = "treatment")
      session$flushReact()
      invisible(output$color_pickers_ui)
      session$flushReact()

      session$setInputs(color_proteome_treatment_2 = "#123456")
      session$flushReact()

      expect_equal(toupper(current_colors()$proteome$treatment$colors[2]), "#123456")
      # multi_ome (and other omes) untouched
      expect_equal(toupper(current_colors()$multi_ome$treatment$colors[2]), "#EE6677")
    }
  )
})


test_that("per-picker observe normalizes 3-digit hex on input", {
  args <- make_test_args()
  shiny::testServer(
    customizeTabServer,
    args = list(GCTs_and_params = args$GCTs_and_params, globals = args$globals),
    {
      session$flushReact()
      session$setInputs(color_mode = "multi_ome",
                        selected_annotation_column = "treatment")
      session$flushReact()
      invisible(output$color_pickers_ui)
      session$flushReact()

      session$setInputs(color_multi_ome_treatment_1 = "#abc")
      session$flushReact()
      expect_equal(toupper(current_colors()$multi_ome$treatment$colors[1]),
                   "#AABBCC")
    }
  )
})


test_that("per-picker observe ignores no-op same-color reassignment", {
  args <- make_test_args()
  shiny::testServer(
    customizeTabServer,
    args = list(GCTs_and_params = args$GCTs_and_params, globals = args$globals),
    {
      session$flushReact()
      session$setInputs(color_mode = "multi_ome",
                        selected_annotation_column = "treatment")
      session$flushReact()
      invisible(output$color_pickers_ui)
      session$flushReact()
      before <- current_colors()
      lc_before <- last_change()

      session$setInputs(color_multi_ome_treatment_1 = "#4477AA")
      session$flushReact()

      expect_identical(current_colors(), before)
      expect_identical(last_change(), lc_before)
    }
  )
})


# ---------------------------------------------------------------------------
# Undo
# ---------------------------------------------------------------------------

test_that("undo + immediate edit: deferred clear does not stomp the new change", {
  # Regression for review finding M6: after Undo, the deferred onFlushed
  # callback used to unconditionally clear last_change. If the user edits
  # a picker before that callback fires, the new last_change was clobbered.
  # The identity-check guard fixes this.
  args <- make_test_args()
  shiny::testServer(
    customizeTabServer,
    args = list(GCTs_and_params = args$GCTs_and_params, globals = args$globals),
    {
      session$flushReact()
      session$setInputs(color_mode = "multi_ome",
                        selected_annotation_column = "treatment")
      session$flushReact()
      invisible(output$color_pickers_ui)
      session$flushReact()

      # Edit, undo, edit again — all in quick succession.
      session$setInputs(color_multi_ome_treatment_1 = "#AAAAAA")
      session$flushReact()
      session$setInputs(undo_last_change = 1)
      session$flushReact()
      session$setInputs(color_multi_ome_treatment_2 = "#BBBBBB")
      session$flushReact()

      # The most recent edit should win and last_change should reflect it.
      lc <- last_change()
      expect_false(is.null(lc))
      expect_true(grepl("#BBBBBB|drug_A", lc$desc))
      expect_equal(toupper(current_colors()$multi_ome$treatment$colors[2]),
                   "#BBBBBB")
    }
  )
})


test_that("undo restores prior colors after a picker change", {
  args <- make_test_args()
  shiny::testServer(
    customizeTabServer,
    args = list(GCTs_and_params = args$GCTs_and_params, globals = args$globals),
    {
      session$flushReact()
      session$setInputs(color_mode = "multi_ome",
                        selected_annotation_column = "treatment")
      session$flushReact()
      invisible(output$color_pickers_ui)
      session$flushReact()

      before <- current_colors()
      session$setInputs(color_multi_ome_treatment_1 = "#AAAAAA")
      session$flushReact()
      expect_equal(toupper(current_colors()$multi_ome$treatment$colors[1]),
                   "#AAAAAA")

      session$setInputs(undo_last_change = 1)
      session$flushReact()
      expect_equal(current_colors(), before)
    }
  )
})


# ---------------------------------------------------------------------------
# Apply preset palette
# ---------------------------------------------------------------------------

test_that("apply preset Viridis updates all pickers in current annotation column", {
  args <- make_test_args()
  shiny::testServer(
    customizeTabServer,
    args = list(GCTs_and_params = args$GCTs_and_params, globals = args$globals),
    {
      session$flushReact()
      session$setInputs(color_mode = "multi_ome",
                        selected_annotation_column = "treatment",
                        preset_palette = "Viridis",
                        reverse_palette = FALSE,
                        apply_preset = 1)
      session$flushReact()

      cols <- current_colors()$multi_ome$treatment$colors
      expect_length(cols, 3)
      expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
      # Sync across omes: proteome should match
      expect_equal(current_colors()$proteome$treatment$colors, cols)
    }
  )
})


test_that("apply preset with no selection no-ops with a warning notification", {
  args <- make_test_args()
  shiny::testServer(
    customizeTabServer,
    args = list(GCTs_and_params = args$GCTs_and_params, globals = args$globals),
    {
      session$flushReact()
      before <- current_colors()
      session$setInputs(color_mode = "multi_ome",
                        selected_annotation_column = "treatment",
                        preset_palette = "(custom)",
                        apply_preset = 1)
      session$flushReact()
      expect_identical(current_colors(), before)
    }
  )
})


# ---------------------------------------------------------------------------
# Cross-tab contract
# ---------------------------------------------------------------------------

test_that("module returns a reactive equal to current_colors", {
  args <- make_test_args()
  expected_colors <- make_fixture_colors()  # known-good snapshot
  returned_value <- NULL
  shiny::testServer(
    customizeTabServer,
    args = list(GCTs_and_params = args$GCTs_and_params, globals = args$globals),
    {
      session$flushReact()
      # The module returns the `current_colors` reactiveVal directly.
      # session$returned() resolves it and gives us the current value.
      returned_value <<- session$returned()
    }
  )
  expect_equal(returned_value, expected_colors)
})
