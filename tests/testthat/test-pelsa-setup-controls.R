################################################################################
# Tests for the PELSA Setup section controls + marker table (Task 5A).
#
# Covers the PURE helpers in tab_pelsa_section1_helpers.R (closed-form, against
# the REAL inst/pelsa/compound_markers.yaml + a temp database dir of fake
# species), plus a no-browser UI-presence check that the Setup UI exposes the
# expected control ids.
################################################################################

library(testthat)

# ---- pelsa_list_species ------------------------------------------------------

test_that("pelsa_list_species returns sorted subfolder names, dirs only", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "mouse"))
  dir.create(file.path(tmp, "human"))
  dir.create(file.path(tmp, "zebrafish"))
  writeLines("not a dir", file.path(tmp, "readme.txt"))

  expect_identical(
    pelsa_list_species(tmp),
    c("human", "mouse", "zebrafish")
  )
})

test_that("pelsa_list_species picks up a newly added species (read live)", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "human"))
  expect_identical(pelsa_list_species(tmp), "human")

  dir.create(file.path(tmp, "rat"))
  expect_identical(pelsa_list_species(tmp), c("human", "rat"))
})

test_that("pelsa_list_species returns character(0) for missing/empty/'' dir", {
  expect_identical(pelsa_list_species(""), character(0))
  expect_identical(pelsa_list_species(tempfile()), character(0)) # nonexistent
  empty <- withr::local_tempdir()
  expect_identical(pelsa_list_species(empty), character(0))
})

test_that("pelsa_list_species fails fast on non-scalar input", {
  expect_error(pelsa_list_species(c("a", "b")), "single string")
  expect_error(pelsa_list_species(1L), "single string")
})

# ---- pelsa_read_compound_markers (REAL yaml) ---------------------------------

test_that("pelsa_read_compound_markers parses the real preset file", {
  path <- system.file("pelsa", "compound_markers.yaml", package = "Protigy")
  skip_if(path == "", "compound_markers.yaml not installed")

  cm <- pelsa_read_compound_markers(path)
  expect_true(is.list(cm))
  expect_true(is.list(cm$compounds))

  # The three documented example compounds must be present.
  expect_true(all(c("Rapamycin", "AY9944", "U-18666A") %in% names(cm$compounds)))

  # Rapamycin's markers carry the expected accession/gene.
  rapa <- cm$compounds$Rapamycin$markers
  accs <- vapply(rapa, function(m) m$accession, character(1))
  expect_true(all(c("P42345", "P62942", "Q13451") %in% accs))
})

test_that("pelsa_read_compound_markers returns empty for a missing file", {
  expect_identical(
    pelsa_read_compound_markers(tempfile(fileext = ".yaml")),
    list(compounds = list())
  )
  expect_identical(pelsa_read_compound_markers(""), list(compounds = list()))
})

test_that("pelsa_read_compound_markers errors on a marker missing accession", {
  bad <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c(
    "compounds:",
    "  Foo:",
    "    markers:",
    "      - gene: 'ABC'"
  ), bad)
  expect_error(pelsa_read_compound_markers(bad), "accession")
})

test_that("pelsa_read_compound_markers errors clearly on a bare-scalar marker", {
  # A markers list whose ELEMENT is an atomic scalar (not a mapping). yaml parses
  # the mixed list so `markers` is a list but one `mk` is atomic -> exercises the
  # per-marker is.list() guard.
  bad <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c(
    "compounds:",
    "  Foo:",
    "    markers:",
    "      - accession: 'P00001'",
    "      - 'P12345'"
  ), bad)
  # Must give a clear message, NOT "$ operator invalid for atomic vectors".
  expect_error(pelsa_read_compound_markers(bad), "non-list marker entry")
})

test_that("pelsa_read_compound_markers errors when compounds is not a named list", {
  bad <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c(
    "compounds:",
    "  - 'Foo'",
    "  - 'Bar'"
  ), bad)
  expect_error(pelsa_read_compound_markers(bad), "named list")
})

test_that("pelsa_read_compound_markers errors on genuinely unparseable YAML", {
  bad <- withr::local_tempfile(fileext = ".yaml")
  # Unbalanced/illegal YAML (bad indentation + stray flow chars) -> parse error.
  writeLines(c(
    "compounds: [unterminated",
    "  : : :"
  ), bad)
  expect_error(pelsa_read_compound_markers(bad), "failed to parse YAML")
})

test_that("pelsa_read_compound_markers errors on a non-list markers field", {
  bad <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c(
    "compounds:",
    "  Foo:",
    "    markers: 'P12345'"
  ), bad)
  expect_error(pelsa_read_compound_markers(bad), "non-list `markers`")
})

# ---- pelsa_compound_marker_rows ----------------------------------------------

test_that("pelsa_compound_marker_rows returns accession+gene rows for a compound", {
  path <- system.file("pelsa", "compound_markers.yaml", package = "Protigy")
  skip_if(path == "", "compound_markers.yaml not installed")
  cm <- pelsa_read_compound_markers(path)

  rows <- pelsa_compound_marker_rows(cm, "Rapamycin")
  expect_identical(names(rows), c("accession", "gene"))
  expect_setequal(rows$accession, c("P42345", "P62942", "Q13451"))
  expect_true("MTOR" %in% rows$gene)
})

test_that("pelsa_compound_marker_rows honors aliases", {
  path <- system.file("pelsa", "compound_markers.yaml", package = "Protigy")
  skip_if(path == "", "compound_markers.yaml not installed")
  cm <- pelsa_read_compound_markers(path)

  # "Sirolimus" is an alias of "Rapamycin".
  rows <- pelsa_compound_marker_rows(cm, "Sirolimus")
  expect_setequal(rows$accession, c("P42345", "P62942", "Q13451"))
})

test_that("pelsa_compound_marker_rows returns empty frame for unknown compound", {
  cm <- list(compounds = list())
  rows <- pelsa_compound_marker_rows(cm, "Nonexistent")
  expect_identical(rows, pelsa_empty_marker_rows())
})

test_that("pelsa_compound_marker_rows: missing gene -> NA", {
  cm <- list(compounds = list(
    X = list(markers = list(list(accession = "P00001")))
  ))
  rows <- pelsa_compound_marker_rows(cm, "X")
  expect_identical(rows$accession, "P00001")
  expect_true(is.na(rows$gene))
})

# ---- pelsa_marker_rows_from_input --------------------------------------------

test_that("pelsa_marker_rows_from_input: gene NA when resolver is NULL", {
  rows <- pelsa_marker_rows_from_input(c("P12345", "Q99999"))
  expect_identical(rows$accession, c("P12345", "Q99999"))
  expect_true(all(is.na(rows$gene)))
})

test_that("pelsa_marker_rows_from_input de-dups + drops empty tokens", {
  rows <- pelsa_marker_rows_from_input(c("P1", " P1 ", "", NA, "Q2"))
  expect_identical(rows$accession, c("P1", "Q2"))
})

test_that("pelsa_marker_rows_from_input empty input -> empty frame", {
  expect_identical(pelsa_marker_rows_from_input(character(0)),
                   pelsa_empty_marker_rows())
  expect_identical(pelsa_marker_rows_from_input(NULL),
                   pelsa_empty_marker_rows())
})

test_that("pelsa_marker_rows_from_input uses a resolver when supplied (5D seam)", {
  resolver <- function(accs) toupper(sub("p", "GENE_", accs))
  rows <- pelsa_marker_rows_from_input(c("p1", "p2"), resolver = resolver)
  expect_identical(rows$gene, c("GENE_1", "GENE_2"))
})

test_that("pelsa_marker_rows_from_input errors when resolver mis-sizes output", {
  resolver <- function(accs) "only-one"
  expect_error(
    pelsa_marker_rows_from_input(c("p1", "p2"), resolver = resolver),
    "one gene"
  )
})

# ---- pelsa_merge_marker_rows -------------------------------------------------

test_that("pelsa_merge_marker_rows unions by accession, existing wins", {
  existing <- data.frame(accession = c("P1", "P2"),
                         gene = c("G1", "G2"), stringsAsFactors = FALSE)
  new <- data.frame(accession = c("P2", "P3"),
                    gene = c(NA, "G3"), stringsAsFactors = FALSE)

  merged <- pelsa_merge_marker_rows(existing, new)
  expect_identical(merged$accession, c("P1", "P2", "P3"))
  # P2's existing gene "G2" is preserved (not clobbered by the new NA).
  expect_identical(merged$gene, c("G1", "G2", "G3"))
})

test_that("pelsa_merge_marker_rows handles empty new (returns existing)", {
  existing <- data.frame(accession = "P1", gene = "G1", stringsAsFactors = FALSE)
  expect_identical(
    pelsa_merge_marker_rows(existing, pelsa_empty_marker_rows()),
    existing
  )
})

test_that("pelsa_merge_marker_rows accession matching is exact (isoform-sensitive)", {
  existing <- data.frame(accession = "P1", gene = "G1", stringsAsFactors = FALSE)
  new <- data.frame(accession = "P1-2", gene = NA_character_,
                    stringsAsFactors = FALSE)
  merged <- pelsa_merge_marker_rows(existing, new)
  # "P1-2" is a DISTINCT table row from "P1" (exact key, not isoform-base).
  expect_identical(merged$accession, c("P1", "P1-2"))
})

# ---- No-browser UI presence --------------------------------------------------

test_that("Setup Tab UI renders the expected control output ids", {
  html <- as.character(PELSASection1_Tab_UI("PELSASection1Tab"))
  # The Tab UI only wires the setup_box uiOutput; the controls themselves are
  # rendered server-side (active-dataset driven). The seam id is present.
  expect_match(html, "setup_box")
})

test_that("Setup control ids are namespaced + wired in the module server", {
  # The control ids are emitted inside renderUI() server-side, so assert against
  # the deparsed module-server body (closed-form, non-flaky) that each expected
  # control id is present.
  ids <- c(
    "pelsa_datasets", "pelsa_species", "pelsa_compound",
    "pelsa_marker_input", "pelsa_add_markers", "pelsa_marker_table",
    "pelsa_remove_markers", "pelsa_clear_markers",
    # 5B: per-dataset config replaces the shared condition/replicate selects.
    "pelsa_apply_all", "pelsa_perdataset_config"
  )
  fn_body <- paste(deparse(body(PELSASection1_Tab_Server)), collapse = "\n")
  for (id in ids) {
    expect_match(fn_body, id, fixed = TRUE,
                 info = paste("control id missing from Setup server:", id))
  }
})

test_that("app_UI() still evaluates after adding Setup controls (construct smoke)", {
  ui <- app_UI(request = list())
  expect_s3_class(ui, "shiny.tag.list")
  expect_true(nchar(as.character(ui)) > 0)
})

# ---- Module server behavior (testServer; no browser) -------------------------

# Build a minimal GCTs_and_params with one ome whose GCT carries cdesc columns,
# using the bundled test data. Skips if the data is unavailable.
.setup_test_gp <- function(env = parent.frame()) {
  ok <- tryCatch({
    utils::data("brca_retrospective_v5.0_proteome_gct", package = "Protigy",
                envir = env)
    TRUE
  }, error = function(e) FALSE)
  skip_if_not(ok, "brca proteome test data not available")
  gct <- get("brca_retrospective_v5.0_proteome_gct", envir = env)
  list(
    gct = gct,
    gp = list(
      GCTs = list(proteome = gct),
      parameters = list(proteome = list(annotation_column = NA))
    )
  )
}

test_that("Tab_Server returns list(exports=<reactive>, setup_state=<reactiveValues>)", {
  fx <- .setup_test_gp()
  GCTs_and_params <- shiny::reactiveVal(fx$gp)
  globals <- shiny::reactiveValues(default_ome = "proteome",
                                   colors = list(proteome = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("proteome")

  ret <- NULL
  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      ret <<- session$returned
    }
  )
  expect_true(is.list(ret))
  expect_named(ret, c("exports", "setup_state"), ignore.order = TRUE)
  expect_true(is.function(ret$exports))                 # a reactiveVal IS a function
  expect_s3_class(ret$setup_state, "reactivevalues")
})

test_that("marker table: compound autofill, add, remove, clear all flow", {
  fx <- .setup_test_gp()
  GCTs_and_params <- shiny::reactiveVal(fx$gp)
  globals <- shiny::reactiveValues(default_ome = "proteome",
                                   colors = list(proteome = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("proteome")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      session$setInputs(pelsa_compound = "Rapamycin")
      expect_equal(nrow(setup_state$marker_rows), 3L)

      session$setInputs(pelsa_marker_input = "P99999 Q88888")
      session$setInputs(pelsa_add_markers = 1)
      expect_equal(nrow(setup_state$marker_rows), 5L)

      session$setInputs(pelsa_marker_table_rows_selected = 1)
      session$setInputs(pelsa_remove_markers = 1)
      expect_equal(nrow(setup_state$marker_rows), 4L)

      session$setInputs(pelsa_clear_markers = 1)
      expect_equal(nrow(setup_state$marker_rows), 0L)
    }
  )
})

test_that("compound autofill merges into existing user-pasted rows", {
  fx <- .setup_test_gp()
  GCTs_and_params <- shiny::reactiveVal(fx$gp)
  globals <- shiny::reactiveValues(default_ome = "proteome",
                                   colors = list(proteome = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("proteome")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      # User pastes a marker first.
      session$setInputs(pelsa_marker_input = "P55555")
      session$setInputs(pelsa_add_markers = 1)
      expect_equal(nrow(setup_state$marker_rows), 1L)

      # Selecting a compound MERGES presets into the existing user row.
      session$setInputs(pelsa_compound = "Rapamycin")
      expect_equal(nrow(setup_state$marker_rows), 4L)
      expect_true("P55555" %in% setup_state$marker_rows$accession)
      expect_true("P42345" %in% setup_state$marker_rows$accession)
    }
  )
})

test_that("cleared markers STAY cleared across a compound re-render echo (no resurrection)", {
  fx <- .setup_test_gp()
  GCTs_and_params <- shiny::reactiveVal(fx$gp)
  globals <- shiny::reactiveValues(default_ome = "proteome",
                                   colors = list(proteome = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("proteome")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      # 1. Pick Rapamycin -> autofills 3.
      session$setInputs(pelsa_compound = "Rapamycin")
      expect_equal(nrow(setup_state$marker_rows), 3L)

      # 2. Clear all -> 0 (and tracker reset).
      session$setInputs(pelsa_clear_markers = 1)
      expect_equal(nrow(setup_state$marker_rows), 0L)

      # 3. Simulate a re-render echo: the selectInput re-emits the SAME value.
      #    Bump a co-input so the observer fires, then re-assert pelsa_compound.
      session$setInputs(pelsa_compound = "Rapamycin")
      expect_equal(nrow(setup_state$marker_rows), 0L,
                   info = "markers must NOT resurrect on a same-value re-emit")

      # 4. A genuine NEW selection still autofills.
      session$setInputs(pelsa_compound = "AY9944")
      expect_gt(nrow(setup_state$marker_rows), 0L)
    }
  )
})

test_that("setup_state datasets wiring populates + per-dataset cond/rep (5B)", {
  fx <- .setup_test_gp()
  GCTs_and_params <- shiny::reactiveVal(fx$gp)
  globals <- shiny::reactiveValues(default_ome = "proteome",
                                   colors = list(proteome = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("proteome")
  cdesc <- names(fx$gct@cdesc)
  skip_if(length(cdesc) == 0L, "test GCT has no cdesc columns")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      session$setInputs(pelsa_datasets = "proteome")
      session$flushReact()
      expect_identical(setup_state$datasets, "proteome")

      # 5B: condition/replicate are now PER-DATASET named lists; the per-dataset
      # selectInput id is index-encoded (proteome is dataset 1 -> _d1).
      expect_true(is.list(setup_state$condition_col))
      session$setInputs(pelsa_condition_col_d1 = cdesc[[1]],
                        pelsa_replicate_col_d1 = cdesc[[1]])
      session$flushReact()
      expect_identical(setup_state$condition_col[["proteome"]], cdesc[[1]])
      expect_identical(setup_state$replicate_col[["proteome"]], cdesc[[1]])
    }
  )
})

test_that("setup_box render gates on a valid active dataset (NULL / unknown -> no error)", {
  fx <- .setup_test_gp()
  GCTs_and_params <- shiny::reactiveVal(fx$gp)
  globals <- shiny::reactiveValues(default_ome = "proteome",
                                   colors = list(proteome = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  # active_dataset() is NULL -> the renderUI req() must gate cleanly.
  active_dataset <- shiny::reactiveVal(NULL)

  expect_no_error(
    shiny::testServer(
      PELSASection1_Tab_Server,
      args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                  GCTs_original = GCTs_original, active_dataset = active_dataset),
      {
        # No active dataset -> the reactive is silent (req gate), no crash.
        active_dataset("not_an_ome")  # unknown ome also gates via %in% check
        session$flushReact()
      }
    )
  )
})
