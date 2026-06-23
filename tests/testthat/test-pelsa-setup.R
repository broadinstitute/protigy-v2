################################################################################
# PELSA Section 1 / Setup suite (merged).
#
# Consolidates three former files into one:
#   - test-pelsa-setup-controls.R   (Setup section controls + marker table; 5A)
#   - test-pelsa-setup-ordering.R   (per-dataset scope + ordering; 5B)
#   - test-pelsa-refresh-observer.R (5C species UniProt-refresh observer; P3.3)
#
# All drive PELSASection1_Tab_Server via shiny::testServer with a brca-data-gated
# harness (GCTs_and_params / globals / GCTs_original / active_dataset).
#
# Helpers kept distinct on purpose:
#   * .setup_test_gp / .ordering_test_gp / .refresh_test_gp differ in body, so
#     they are NOT unified (see each definition).
#   * `%||%` is defined once (from the refresh-observer suite).
################################################################################

library(testthat)

`%||%` <- function(a, b) if (is.null(a)) b else a

# =============================================================================
# --- from setup-controls (Task 5A) ---
# =============================================================================

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

test_that("pelsa_read_compound_markers preserves the metadata block", {
  path <- system.file("pelsa", "compound_markers.yaml", package = "Protigy")
  skip_if(path == "", "compound_markers.yaml not installed")
  cm <- pelsa_read_compound_markers(path)
  expect_false(is.null(cm$metadata))
  expect_equal(cm$metadata$schema_version, 1)
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

# ---- .pelsa_resolve_compound_name (case-insensitive, no aliases) -------------

test_that(".pelsa_resolve_compound_name matches case-insensitively on key", {
  cm <- list(compounds = list(Rapamycin = list(markers = list()),
                              AY9944    = list(markers = list())))
  expect_identical(.pelsa_resolve_compound_name(cm, "Rapamycin"), "Rapamycin")
  expect_identical(.pelsa_resolve_compound_name(cm, "rapamycin"), "Rapamycin")
  expect_identical(.pelsa_resolve_compound_name(cm, "RAPAMYCIN"), "Rapamycin")
})

test_that(".pelsa_resolve_compound_name ignores aliases and returns NA on miss", {
  cm <- list(compounds = list(
    Rapamycin = list(aliases = list("Sirolimus"), markers = list())
  ))
  expect_true(is.na(.pelsa_resolve_compound_name(cm, "Sirolimus")))
  expect_true(is.na(.pelsa_resolve_compound_name(cm, "Nonexistent")))
})

test_that(".pelsa_resolve_compound_name returns NA for empty compound set", {
  expect_true(is.na(.pelsa_resolve_compound_name(list(compounds = list()), "X")))
})

# ---- pelsa_validate_compound_name --------------------------------------------

test_that("pelsa_validate_compound_name trims and accepts a valid name", {
  res <- pelsa_validate_compound_name("  AY-9944  ")
  expect_true(res$ok)
  expect_identical(res$name, "AY-9944")
})

test_that("pelsa_validate_compound_name rejects empty / whitespace-only", {
  for (x in list("", "   ", NA_character_, NULL)) {
    res <- pelsa_validate_compound_name(x)
    expect_false(res$ok)
    expect_match(res$message, "Enter a compound name")
  }
})

test_that("pelsa_validate_compound_name rejects internal whitespace", {
  res <- pelsa_validate_compound_name("U 18666A")
  expect_false(res$ok)
  expect_match(res$message, "cannot contain spaces")
})

test_that("pelsa_validate_compound_name rejects non-ASCII", {
  # Build a non-ASCII name via a code-point escape so this source stays ASCII.
  res <- pelsa_validate_compound_name(paste0("Rapamycin", "\u00b5"))
  expect_false(res$ok)
  expect_match(res$message, "ASCII only")
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

# ---- pelsa_compound_exists ---------------------------------------------------

test_that("pelsa_compound_exists matches case-insensitively, key only", {
  cm <- list(compounds = list(Rapamycin = list(markers = list())))
  expect_true(pelsa_compound_exists(cm, "Rapamycin"))
  expect_true(pelsa_compound_exists(cm, "rapamycin"))
  expect_false(pelsa_compound_exists(cm, "AY9944"))
  expect_false(pelsa_compound_exists(list(compounds = list()), "X"))
})

# ---- pelsa_add_compound ------------------------------------------------------

test_that("pelsa_add_compound adds a compound with empty markers", {
  cm  <- list(compounds = list())
  out <- pelsa_add_compound(cm, "NewCmpd")
  expect_true("NewCmpd" %in% names(out$compounds))
  expect_identical(out$compounds$NewCmpd$markers, list())
  # immutability: original untouched
  expect_length(cm$compounds, 0L)
})

test_that("pelsa_add_compound errors when the compound already exists", {
  cm <- list(compounds = list(Rapamycin = list(markers = list())))
  expect_error(pelsa_add_compound(cm, "rapamycin"), "already exists")
})

# ---- pelsa_set_compound_markers ----------------------------------------------

test_that("pelsa_set_compound_markers fully replaces markers in place", {
  cm <- list(compounds = list(
    Rapamycin = list(markers = list(list(accession = "OLD", gene = "OLDGENE")))
  ))
  rows <- data.frame(accession = c("P1", "P2"),
                     gene       = c("G1", NA_character_),
                     stringsAsFactors = FALSE)
  out <- pelsa_set_compound_markers(cm, "rapamycin", rows)  # case-insensitive

  mk <- out$compounds$Rapamycin$markers
  expect_length(mk, 2L)
  expect_identical(mk[[1]]$accession, "P1")
  expect_identical(mk[[1]]$gene, "G1")
  expect_identical(mk[[2]]$accession, "P2")
  expect_null(mk[[2]]$gene)  # NA gene is dropped, not written as NA
  # immutability
  expect_identical(cm$compounds$Rapamycin$markers[[1]]$accession, "OLD")
})

test_that("pelsa_set_compound_markers accepts an empty table (clears markers)", {
  cm  <- list(compounds = list(X = list(markers = list(list(accession = "P1")))))
  out <- pelsa_set_compound_markers(cm, "X", pelsa_empty_marker_rows())
  expect_identical(out$compounds$X$markers, list())
})

test_that("pelsa_set_compound_markers errors on unknown compound", {
  cm <- list(compounds = list())
  expect_error(
    pelsa_set_compound_markers(cm, "Ghost", pelsa_empty_marker_rows()),
    "unknown compound"
  )
})

# ---- pelsa_write_compound_markers --------------------------------------------

test_that("pelsa_write_compound_markers round-trips and preserves metadata", {
  tmp  <- withr::local_tempdir()
  path <- file.path(tmp, "compound_markers.yaml")
  cm <- list(
    metadata  = list(description = "test", schema_version = 1),
    compounds = list(Rapamycin = list(markers = list(
      list(accession = "P42345", gene = "MTOR")
    )))
  )
  expect_true(pelsa_write_compound_markers(path, cm))

  back <- pelsa_read_compound_markers(path)
  rows <- pelsa_compound_marker_rows(back, "Rapamycin")
  expect_identical(rows$accession, "P42345")
  expect_identical(rows$gene, "MTOR")

  raw <- yaml::read_yaml(path)
  expect_identical(raw$metadata$description, "test")
  expect_equal(raw$metadata$schema_version, 1)

  # schema_version serializes as the YAML integer "1" (not "1.0"), so a
  # read->write round-trip is byte-stable.
  expect_true(any(grepl("schema_version: 1$", readLines(path))))
  back <- pelsa_read_compound_markers(path)
  tmp2 <- file.path(tmp, "roundtrip.yaml")
  expect_true(pelsa_write_compound_markers(tmp2, back))
  expect_true(any(grepl("schema_version: 1$", readLines(tmp2))))
})

test_that("pelsa_write_compound_markers returns FALSE for a non-writable dir", {
  cm <- list(metadata = list(), compounds = list())
  # A path inside a directory that does not exist -> dirname not a real dir.
  bad <- file.path(tempfile(), "nope", "compound_markers.yaml")
  expect_false(pelsa_write_compound_markers(bad, cm))
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

# ---- pelsa_analyzed_omes (non-skipped set) -----------------------------------

test_that("pelsa_analyzed_omes returns the non-skipped omes in all_omes order", {
  all_omes <- c("A", "B", "C")
  skip <- list(A = FALSE, B = TRUE, C = FALSE)
  expect_identical(pelsa_analyzed_omes(skip, all_omes), c("A", "C"))
})

test_that("pelsa_analyzed_omes treats a missing/NULL skip entry as NOT skipped", {
  # A dataset never toggled has no skip entry; default is analyzed (not skipped).
  all_omes <- c("A", "B")
  expect_identical(pelsa_analyzed_omes(list(), all_omes), c("A", "B"))
  expect_identical(pelsa_analyzed_omes(list(A = TRUE), all_omes), "B")
})

test_that("pelsa_analyzed_omes returns character(0) when all are skipped", {
  all_omes <- c("A", "B")
  skip <- list(A = TRUE, B = TRUE)
  expect_identical(pelsa_analyzed_omes(skip, all_omes), character(0))
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
    "pelsa_skip", "pelsa_fasta", "pelsa_self_curated", "pelsa_annotation",
    "pelsa_compound",
    "pelsa_marker_input", "pelsa_add_markers", "pelsa_marker_table",
    "pelsa_remove_markers", "pelsa_clear_markers",
    # per-dataset config + apply-to-all button (the datasets checkbox is gone;
    # the per-tab Skip toggle is the single opt-out).
    "pelsa_apply_all", "pelsa_perdataset_config"
  )
  fn_body <- paste(deparse(body(PELSASection1_Tab_Server)), collapse = "\n")
  for (id in ids) {
    expect_match(fn_body, id, fixed = TRUE,
                 info = paste("control id missing from Setup server:", id))
  }
})

test_that("Issue A: compound is handled by a SINGLE guarded observer (merged)", {
  # The record (set_ds("compound", ...)) and the marker autofill must live in
  # ONE guarded observeEvent(input$pelsa_compound) so a setup-box re-render
  # re-emit cannot redundantly re-record or clobber edited markers. The control
  # ids are emitted server-side, so assert against the deparsed module body.
  fn_body <- paste(deparse(body(PELSASection1_Tab_Server)), collapse = "\n")
  n_obs <- length(gregexpr("observeEvent(input$pelsa_compound", fn_body,
                           fixed = TRUE)[[1]])
  expect_equal(n_obs, 1L)
  # the record side-effect is preserved (inside the merged handler).
  expect_match(fn_body, "set_ds(\"compound\"", fixed = TRUE)
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

test_that("Tab_Server returns list(exports, setup_state, analysis)", {
  fx <- .setup_test_gp()
  GCTs_and_params <- shiny::reactiveVal(fx$gp)
  globals <- shiny::reactiveValues(default_ome = "proteome",
                                   colors = list(proteome = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("proteome")

  ret <- NULL
  snap <- NULL
  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      ret <<- session$returned
      snap <<- session$returned$setup_state()  # call the seam -> snapshot list
    }
  )
  expect_true(is.list(ret))
  # 5D extended the contract with $analysis (the Start-Analysis cache reactiveVal
  # Phases 6/7 read); exports + setup_state are unchanged.
  expect_named(ret, c("exports", "setup_state", "analysis"),
               ignore.order = TRUE)
  expect_true(is.function(ret$exports))                 # a reactiveVal IS a function
  # SEAM: setup_state is a REACTIVE (is.function TRUE) that yields a plain
  # snapshot LIST  -  NOT a bare reactiveValues (which would fail the consumers'
  # is.function() guard and be downgraded to reactive(NULL) in production).
  expect_true(is.function(ret$setup_state))
  expect_true(shiny::is.reactive(ret$setup_state))
  expect_false(shiny::is.reactivevalues(ret$setup_state))
  expect_true(is.list(snap) && !shiny::is.reactivevalues(snap))
  expect_true(all(c("datasets", "fasta_path", "annotation_path", "self_curated",
                    "marker_rows", "condition_col",
                    "condition_order", "sample_order") %in% names(snap)))
  expect_true(is.function(ret$analysis))                # a reactiveVal IS a function
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
      expect_equal(nrow(setup_state$marker_rows[["proteome"]]), 3L)

      session$setInputs(pelsa_marker_input = "P99999 Q88888")
      session$setInputs(pelsa_add_markers = 1)
      expect_equal(nrow(setup_state$marker_rows[["proteome"]]), 5L)

      session$setInputs(pelsa_marker_table_rows_selected = 1)
      session$setInputs(pelsa_remove_markers = 1)
      expect_equal(nrow(setup_state$marker_rows[["proteome"]]), 4L)

      session$setInputs(pelsa_clear_markers = 1)
      expect_equal(nrow(setup_state$marker_rows[["proteome"]]), 0L)
    }
  )
})

test_that("Issue A: a compound selection records it AND autofills markers", {
  fx <- .setup_test_gp()
  GCTs_and_params <- shiny::reactiveVal(fx$gp)
  globals <- shiny::reactiveValues(default_ome = "proteome",
                                   colors = list(proteome = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("proteome")
  path <- pelsa_compound_markers_path()
  skip_if(path == "", "compound_markers.yaml not installed")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      # genuine selection: record + autofill happen together (lockstep).
      session$setInputs(pelsa_compound = "Rapamycin")
      expect_identical(setup_state$compound[["proteome"]], "Rapamycin")
      expect_equal(nrow(setup_state$marker_rows[["proteome"]]), 3L)

      # "(none)" records "" AND clears the table, together.
      session$setInputs(pelsa_compound = "")
      expect_identical(setup_state$compound[["proteome"]], "")
      expect_equal(nrow(setup_state$marker_rows[["proteome"]]), 0L)
    }
  )
})

test_that("marker_add_request channel: Volcano-requested accession merges in", {
  fx <- .setup_test_gp()
  GCTs_and_params <- shiny::reactiveVal(fx$gp)
  globals <- shiny::reactiveValues(default_ome = "proteome",
                                   colors = list(proteome = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("proteome")
  marker_add_request <- shiny::reactiveVal(NULL)

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset,
                marker_add_request = marker_add_request),
    {
      expect_null(setup_state$marker_rows[["proteome"]])

      # Volcano pushes an accession (PER-OME payload list(ome, rows)) -> Section 1
      # observes and merges it into THAT ome's marker list.
      marker_add_request(list(ome = "proteome",
        rows = data.frame(accession = "P77777", gene = "GENEX",
                          stringsAsFactors = FALSE)))
      session$flushReact()
      expect_equal(nrow(setup_state$marker_rows[["proteome"]]), 1L)
      expect_true("P77777" %in% setup_state$marker_rows[["proteome"]]$accession)

      # Re-requesting the SAME accession is idempotent (merge dedupes).
      marker_add_request(list(ome = "proteome",
        rows = data.frame(accession = "P77777", gene = "GENEX",
                          stringsAsFactors = FALSE)))
      session$flushReact()
      expect_equal(nrow(setup_state$marker_rows[["proteome"]]), 1L)

      # A different accession adds another row.
      marker_add_request(list(ome = "proteome",
        rows = data.frame(accession = "Q11111", gene = "GENEY",
                          stringsAsFactors = FALSE)))
      session$flushReact()
      expect_equal(nrow(setup_state$marker_rows[["proteome"]]), 2L)
    }
  )
})

test_that("M6: re-adding the same accession after removal re-fires the channel", {
  fx <- .setup_test_gp()
  GCTs_and_params <- shiny::reactiveVal(fx$gp)
  globals <- shiny::reactiveValues(default_ome = "proteome",
                                   colors = list(proteome = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("proteome")
  marker_add_request <- shiny::reactiveVal(NULL)

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset,
                marker_add_request = marker_add_request),
    {
      req <- list(ome = "proteome",
                  rows = data.frame(accession = "P88888", gene = "GENEZ",
                                    stringsAsFactors = FALSE))
      # 1) Add from the volcano.
      marker_add_request(req); session$flushReact()
      expect_true("P88888" %in% setup_state$marker_rows[["proteome"]]$accession)
      # M6 fix: the consumer resets the channel to NULL after merging.
      expect_null(marker_add_request())

      # 2) Remove it here in Setup (clear the active ome).
      session$setInputs(pelsa_clear_markers = 1); session$flushReact()
      expect_equal(nrow(setup_state$marker_rows[["proteome"]]), 0L)

      # 3) Re-add the SAME accession. Pre-fix the identical value would not
      #    re-fire the observer (silent drop); post-fix the NULL reset makes it
      #    a fresh change so it re-adds.
      marker_add_request(req); session$flushReact()
      expect_true("P88888" %in% setup_state$marker_rows[["proteome"]]$accession)
      expect_equal(nrow(setup_state$marker_rows[["proteome"]]), 1L)
    }
  )
})

test_that("add-compound rejects an invalid name without writing the YAML", {
  fx <- .setup_test_gp()
  GCTs_and_params <- shiny::reactiveVal(fx$gp)
  globals <- shiny::reactiveValues(default_ome = "proteome",
                                   colors = list(proteome = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("proteome")

  path <- pelsa_compound_markers_path()
  skip_if(path == "", "compound_markers.yaml not installed")
  before <- readLines(path)

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      # A name with a space is rejected by pelsa_validate_compound_name; the
      # handler returns before any write.
      session$setInputs(pelsa_new_compound = "Bad Name")
      session$setInputs(pelsa_add_compound_btn = 1)
      session$flushReact()
    }
  )
  expect_identical(readLines(path), before)
})

test_that("add-compound blocks a duplicate name and selects the existing one", {
  fx <- .setup_test_gp()
  GCTs_and_params <- shiny::reactiveVal(fx$gp)
  globals <- shiny::reactiveValues(default_ome = "proteome",
                                   colors = list(proteome = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("proteome")

  path <- pelsa_compound_markers_path()
  skip_if(path == "", "compound_markers.yaml not installed")
  before <- readLines(path)

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      # "rapamycin" already exists (case-insensitive) -> blocked, no write.
      session$setInputs(pelsa_new_compound = "rapamycin")
      session$setInputs(pelsa_add_compound_btn = 1)
      session$flushReact()
    }
  )
  expect_identical(readLines(path), before)
})

test_that("set-default with no compound selected does not write the YAML", {
  fx <- .setup_test_gp()
  GCTs_and_params <- shiny::reactiveVal(fx$gp)
  globals <- shiny::reactiveValues(default_ome = "proteome",
                                   colors = list(proteome = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("proteome")

  path <- pelsa_compound_markers_path()
  skip_if(path == "", "compound_markers.yaml not installed")
  before <- readLines(path)

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      # No compound selected ("(none)" = "") -> handler notifies + returns.
      session$setInputs(pelsa_compound = "")
      session$setInputs(pelsa_set_default_markers_btn = 1)
      session$flushReact()
    }
  )
  expect_identical(readLines(path), before)
})

# The two WRITE-success paths (add-compound, set-default confirm) target the
# live preset file via pelsa_compound_markers_path(). We redirect that resolver
# to a writable tempdir (seeded from the real file) so the handlers exercise a
# genuine round-trip WITHOUT mutating the committed inst/pelsa/compound_markers.yaml.
test_that("add-compound success path persists a new compound to the YAML", {
  real <- system.file("pelsa", "compound_markers.yaml", package = "Protigy")
  skip_if(real == "", "compound_markers.yaml not installed")

  tmpdir <- withr::local_tempdir()
  tmp    <- file.path(tmpdir, "compound_markers.yaml")
  file.copy(real, tmp)
  testthat::local_mocked_bindings(
    pelsa_compound_markers_path = function() tmp,
    .package = "Protigy"
  )

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
      session$setInputs(pelsa_new_compound = "MyNewCompound")
      session$setInputs(pelsa_add_compound_btn = 1)
      session$flushReact()
    }
  )

  written <- pelsa_read_compound_markers(tmp)
  expect_true("MyNewCompound" %in% names(written$compounds))
  # A brand-new compound starts with no preset markers.
  expect_length(written$compounds$MyNewCompound$markers, 0L)
})

test_that("set-default confirm persists the current table as the compound preset", {
  real <- system.file("pelsa", "compound_markers.yaml", package = "Protigy")
  skip_if(real == "", "compound_markers.yaml not installed")

  tmpdir <- withr::local_tempdir()
  tmp    <- file.path(tmpdir, "compound_markers.yaml")
  file.copy(real, tmp)
  testthat::local_mocked_bindings(
    pelsa_compound_markers_path = function() tmp,
    .package = "Protigy"
  )

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
      # Select Rapamycin (replaces table with its 3 presets), then paste one more
      # marker, then confirm "set as default" -> the preset becomes those 4.
      session$setInputs(pelsa_compound = "Rapamycin")
      session$flushReact()
      session$setInputs(pelsa_marker_input = "P99999")
      session$setInputs(pelsa_add_markers = 1)
      session$flushReact()
      expect_equal(nrow(setup_state$marker_rows[["proteome"]]), 4L)

      session$setInputs(pelsa_set_default_markers_btn = 1)
      session$setInputs(pelsa_confirm_set_default = 1)
      session$flushReact()
    }
  )

  written <- pelsa_read_compound_markers(tmp)
  rows <- pelsa_compound_marker_rows(written, "Rapamycin")
  expect_setequal(rows$accession, c("P42345", "P62942", "Q13451", "P99999"))
})

test_that("compound selection REPLACES existing user-pasted rows", {
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
      expect_equal(nrow(setup_state$marker_rows[["proteome"]]), 1L)

      # Selecting a compound REPLACES the table with that compound's presets
      # (the manually-pasted P55555 is wiped).
      session$setInputs(pelsa_compound = "Rapamycin")
      expect_equal(nrow(setup_state$marker_rows[["proteome"]]), 3L)
      expect_false("P55555" %in% setup_state$marker_rows[["proteome"]]$accession)
      expect_true("P42345" %in% setup_state$marker_rows[["proteome"]]$accession)

      # Switching to another compound fully replaces again (AY9944 has 2).
      session$setInputs(pelsa_compound = "AY9944")
      expect_equal(nrow(setup_state$marker_rows[["proteome"]]), 2L)
      expect_false("P42345" %in% setup_state$marker_rows[["proteome"]]$accession)

      # Selecting "(none)" clears the table.
      session$setInputs(pelsa_compound = "")
      expect_equal(nrow(setup_state$marker_rows[["proteome"]]), 0L)
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
      expect_equal(nrow(setup_state$marker_rows[["proteome"]]), 3L)

      # 2. Clear all -> 0 (and tracker NOT reset, per the echo-safety design).
      session$setInputs(pelsa_clear_markers = 1)
      expect_equal(nrow(setup_state$marker_rows[["proteome"]]), 0L)

      # 3. Simulate a re-render echo: the selectInput re-emits the SAME value.
      session$setInputs(pelsa_compound = "Rapamycin")
      expect_equal(nrow(setup_state$marker_rows[["proteome"]]), 0L,
                   info = "markers must NOT resurrect on a same-value re-emit")

      # 4. A genuine NEW selection still autofills.
      session$setInputs(pelsa_compound = "AY9944")
      expect_gt(nrow(setup_state$marker_rows[["proteome"]]), 0L)
    }
  )
})

test_that("per-dataset cond/rep wiring + skip toggle (no checkbox group)", {
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
      session$flushReact()
      # No checkbox group: a non-skipped dataset is analyzed by default.
      expect_identical(checked_datasets(), "proteome")

      # condition/replicate are PER-DATASET named lists; the per-dataset
      # selectInput id is index-encoded (proteome is dataset 1 -> _d1).
      expect_true(is.list(setup_state$condition_col))
      session$setInputs(pelsa_condition_col_d1 = cdesc[[1]],
                        pelsa_replicate_col_d1 = cdesc[[1]])
      session$flushReact()
      expect_identical(setup_state$condition_col[["proteome"]], cdesc[[1]])
      expect_identical(setup_state$replicate_col[["proteome"]], cdesc[[1]])

      # Skip toggle: setting it flips the per-ome flag and removes the dataset
      # from the analyzed (checked) set.
      session$setInputs(pelsa_skip = TRUE)
      session$flushReact()
      expect_true(isTRUE(setup_state$skip[["proteome"]]))
      expect_identical(checked_datasets(), character(0))
    }
  )
})

test_that("per-dataset upload/compound wiring writes the active ome's slot", {
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
      session$setInputs(pelsa_fasta = data.frame(
        name = "human.fasta", size = 1, type = "",
        datapath = "/tmp/human.fasta", stringsAsFactors = FALSE))
      session$setInputs(pelsa_compound = "Rapamycin")
      session$flushReact()
      expect_identical(setup_state$fasta_path[["proteome"]], "/tmp/human.fasta")
      expect_identical(setup_state$fasta_name[["proteome"]], "human.fasta")
      expect_identical(setup_state$compound[["proteome"]], "Rapamycin")
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

# ---------------------------------------------------------------------------
# pelsa_setup_box_ui: per-dataset form; re-render must preserve selections
#
# output$setup_box depends on setup_active_dataset(), so switching the active
# setup tab re-renders the box. The builder must honor seeded selections so a
# re-render preserves what the user chose for THIS dataset (else the re-emitted
# values clobber the per-ome setup_state).
# ---------------------------------------------------------------------------
test_that("pelsa_setup_box_ui renders FASTA + annotation uploaders and the self-curated checkbox", {
  ns <- shiny::NS("x")
  html <- as.character(pelsa_setup_box_ui(
    compounds = c("CompoundA" = "CompoundA"),
    ns        = ns
  ))
  expect_true(grepl("x-pelsa_fasta", html))
  expect_true(grepl("x-pelsa_annotation", html))
  expect_true(grepl("x-pelsa_self_curated", html))
  # default compound is the "(none)" = "" entry
  expect_true(grepl("<option value=\"\" selected>", html))
  # Skip toggle present and NOT checked by default
  expect_true(grepl("pelsa_skip", html))
  expect_false(grepl("pelsa_skip[^>]*checked", html))
  # self-curated unchecked by default
  expect_false(grepl("pelsa_self_curated[^>]*checked", html))
})

test_that("pelsa_setup_box_ui no longer renders the species selector or refresh controls", {
  ns <- shiny::NS("x")
  html <- as.character(pelsa_setup_box_ui(
    compounds = character(0),
    ns        = ns
  ))
  expect_false(grepl("x-pelsa_species", html))
  expect_false(grepl("x-pelsa_refresh_species", html))
  expect_false(grepl("x-pelsa_refresh_btn", html))
  expect_false(grepl("x-pelsa_incremental_btn", html))
})

test_that("pelsa_setup_box_ui honors seeded self_curated + skip", {
  ns <- shiny::NS("x")
  html <- as.character(pelsa_setup_box_ui(
    compounds = c("CompoundA" = "CompoundA"),
    ns        = ns,
    selected_compound = "CompoundA",    # NOT "(none)"
    selected_skip     = TRUE,           # this dataset skipped
    self_curated      = TRUE            # self-curated database
  ))
  # the chosen compound is selected
  expect_true(grepl("<option value=\"CompoundA\" selected>", html))
  # Skip toggle checked
  expect_true(grepl("pelsa_skip[^>]*checked", html))
  # self-curated checkbox checked
  expect_true(grepl("pelsa_self_curated[^>]*checked", html))
  # annotation uploader greyed out from the FIRST render when self-curated
  expect_true(grepl("pelsa_annotation_wrap\" class=\"shinyjs-disabled\"", html))
})

test_that("pelsa_setup_box_ui leaves the annotation uploader enabled when not self-curated", {
  ns <- shiny::NS("x")
  html <- as.character(pelsa_setup_box_ui(
    compounds = character(0), ns = ns, self_curated = FALSE
  ))
  expect_false(grepl("pelsa_annotation_wrap\" class=\"shinyjs-disabled\"", html))
})

test_that("pelsa_fileinput_path/name extract datapath and name, NULL-safe", {
  fi <- data.frame(name = "human.fasta", size = 1, type = "",
                   datapath = "/tmp/abc", stringsAsFactors = FALSE)
  expect_equal(pelsa_fileinput_path(fi), "/tmp/abc")
  expect_equal(pelsa_fileinput_name(fi), "human.fasta")
  expect_null(pelsa_fileinput_path(NULL))
  expect_null(pelsa_fileinput_name(NULL))
  expect_null(pelsa_fileinput_path(fi[0, ]))
})

test_that("pelsa_setup_box_ui exposes add-compound + set-default controls", {
  ns <- shiny::NS("x")
  html <- as.character(pelsa_setup_box_ui(
    compounds = c("CompoundA" = "CompoundA"),
    ns        = ns
  ))
  expect_true(grepl("x-pelsa_new_compound", html))
  expect_true(grepl("x-pelsa_add_compound_btn", html))
  expect_true(grepl("x-pelsa_set_default_markers_btn", html))
})

# =============================================================================
# --- from setup-ordering (Task 5B) ---
# =============================================================================

# A tiny cdesc fixture: 2 conditions x 2-3 replicates, rownames = sample names.
#   ctrl: s_ctrl_2, s_ctrl_1   (replicate ids 2,1 -> default sort gives 1 then 2)
#   drug: s_drug_b, s_drug_a, s_drug_c
.ordering_cdesc <- function() {
  data.frame(
    cond = c("ctrl", "ctrl", "drug", "drug", "drug"),
    rep  = c("r2", "r1", "rB", "rA", "rC"),
    row.names = c("s_ctrl_2", "s_ctrl_1", "s_drug_b", "s_drug_a", "s_drug_c"),
    stringsAsFactors = FALSE
  )
}

# ---- pelsa_distinct_conditions -----------------------------------------------

test_that("pelsa_distinct_conditions returns first-seen order, drops NA", {
  cdesc <- .ordering_cdesc()
  expect_identical(pelsa_distinct_conditions(cdesc, "cond"), c("ctrl", "drug"))

  cdesc$cond[2] <- NA
  expect_identical(pelsa_distinct_conditions(cdesc, "cond"), c("ctrl", "drug"))
})

test_that("pelsa_distinct_conditions errors on missing column", {
  expect_error(pelsa_distinct_conditions(.ordering_cdesc(), "nope"),
               "not in cdesc")
})

# ---- pelsa_samples_for_condition ---------------------------------------------

test_that("pelsa_samples_for_condition sorts samples by replicate column", {
  cdesc <- .ordering_cdesc()
  # ctrl: rep ids r2,r1 -> sorted r1,r2 -> samples s_ctrl_1, s_ctrl_2
  expect_identical(
    pelsa_samples_for_condition(cdesc, "cond", "rep", "ctrl"),
    c("s_ctrl_1", "s_ctrl_2")
  )
  # drug: rep ids rB,rA,rC -> sorted rA,rB,rC -> s_drug_a, s_drug_b, s_drug_c
  expect_identical(
    pelsa_samples_for_condition(cdesc, "cond", "rep", "drug"),
    c("s_drug_a", "s_drug_b", "s_drug_c")
  )
})

test_that("pelsa_samples_for_condition returns empty for absent condition", {
  expect_identical(
    pelsa_samples_for_condition(.ordering_cdesc(), "cond", "rep", "ghost"),
    character(0)
  )
})

test_that("pelsa_samples_for_condition falls back to sample-name order when replicate col is unset", {
  # When the condition column is chosen but the replicate column is still the
  # blank "(none)" default (or any non-cdesc value), the function must NOT throw
  # - it falls back to ordering by sample NAME. This is the runtime path when a
  # user sets the condition column before the replicate column.
  cdesc <- .ordering_cdesc()
  # ctrl samples (rownames) sorted by NAME: s_ctrl_1, s_ctrl_2.
  expect_identical(
    pelsa_samples_for_condition(cdesc, "cond", "(none)", "ctrl"),
    c("s_ctrl_1", "s_ctrl_2")
  )
  # drug samples sorted by NAME: s_drug_a, s_drug_b, s_drug_c.
  expect_identical(
    pelsa_samples_for_condition(cdesc, "cond", "(none)", "drug"),
    c("s_drug_a", "s_drug_b", "s_drug_c")
  )
  # NULL / NA replicate col also degrade gracefully (no throw).
  expect_identical(
    pelsa_samples_for_condition(cdesc, "cond", NULL, "ctrl"),
    c("s_ctrl_1", "s_ctrl_2")
  )
})

# ---- pelsa_default_replicate_order -------------------------------------------

test_that("pelsa_default_replicate_order is a named list keyed by condition", {
  cdesc <- .ordering_cdesc()
  def <- pelsa_default_replicate_order(cdesc, "cond", "rep")
  expect_identical(names(def), c("ctrl", "drug"))
  expect_identical(def$ctrl, c("s_ctrl_1", "s_ctrl_2"))
  expect_identical(def$drug, c("s_drug_a", "s_drug_b", "s_drug_c"))
})

# ---- pelsa_merge_ordering ----------------------------------------------------

test_that("pelsa_merge_ordering keeps saved, appends new, drops removed", {
  # saved order [B, A], available [A, B, C] -> keep [B, A], append C
  expect_identical(
    pelsa_merge_ordering(c("B", "A"), c("A", "B", "C")),
    c("B", "A", "C")
  )
  # saved includes a removed item X -> X dropped
  expect_identical(
    pelsa_merge_ordering(c("X", "A"), c("A", "B")),
    c("A", "B")
  )
  # NULL saved -> available order
  expect_identical(pelsa_merge_ordering(NULL, c("A", "B")), c("A", "B"))
  # de-dups saved
  expect_identical(pelsa_merge_ordering(c("A", "A", "B"), c("A", "B")),
                   c("A", "B"))
})

# ---- pelsa_prune_perdataset_state --------------------------------------------

test_that("pelsa_prune_perdataset_state drops unchecked datasets from every field", {
  state <- list(
    condition_col = list(A = "x", B = "y", C = "z"),
    replicate_col = list(A = "x", B = "y", C = "z"),
    replicate_order = list(A = list(c1 = "sa"), B = list(c1 = "sb"),
                           C = list(c1 = "sc")),
    sample_order = list(A = "sa", B = "sb", C = "sc")
  )
  pruned <- pelsa_prune_perdataset_state(state, checked = c("B", "C"))

  # A dropped from EVERY field; B/C retained with their values intact.
  for (f in names(state)) {
    expect_false("A" %in% names(pruned[[f]]), info = f)
    expect_setequal(names(pruned[[f]]), c("B", "C"))
  }
  expect_identical(pruned$condition_col$B, "y")
  expect_identical(pruned$replicate_order$C, list(c1 = "sc"))
})

test_that("pelsa_prune_perdataset_state: empty/NULL fields -> empty list", {
  pruned <- pelsa_prune_perdataset_state(
    list(a = list(), b = NULL, c = list(X = 1)), checked = "Z"
  )
  expect_identical(pruned$a, list())
  expect_identical(pruned$b, list())
  expect_identical(pruned$c, list())  # X not in checked
})

test_that("pelsa_prune_perdataset_state does not mutate its input", {
  state <- list(condition_col = list(A = "x", B = "y"))
  before <- state
  invisible(pelsa_prune_perdataset_state(state, checked = "A"))
  expect_identical(state, before)
})

# ---- pelsa_build_sample_order (closed-form, the canonical contract) ----------

test_that("pelsa_build_sample_order yields the exact ordered sample vector", {
  cdesc <- .ordering_cdesc()
  # condition_order drug-first; within drug pick rep order [c, a, b]; ctrl default.
  out <- pelsa_build_sample_order(
    condition_order = c("drug", "ctrl"),
    replicate_order_by_condition = list(
      drug = c("s_drug_c", "s_drug_a", "s_drug_b")
    ),
    cdesc = cdesc,
    condition_col = "cond",
    replicate_col = "rep"
  )
  expect_identical(
    out,
    c("s_drug_c", "s_drug_a", "s_drug_b",  # drug, explicit order
      "s_ctrl_1", "s_ctrl_2")              # ctrl, default replicate sort
  )
})

test_that("pelsa_build_sample_order: NULL orders -> full natural order", {
  cdesc <- .ordering_cdesc()
  out <- pelsa_build_sample_order(NULL, NULL, cdesc, "cond", "rep")
  expect_identical(
    out,
    c("s_ctrl_1", "s_ctrl_2", "s_drug_a", "s_drug_b", "s_drug_c")
  )
})

test_that("pelsa_build_sample_order reconciles removed/added samples + conditions", {
  cdesc <- .ordering_cdesc()
  # Saved condition order references a condition that no longer exists; saved
  # replicate order references a sample that no longer exists + omits one.
  out <- pelsa_build_sample_order(
    condition_order = c("ghost", "drug"),  # ghost dropped, ctrl appended
    replicate_order_by_condition = list(
      drug = c("s_drug_z", "s_drug_b")     # z dropped; a,c appended in default
    ),
    cdesc = cdesc,
    condition_col = "cond",
    replicate_col = "rep"
  )
  # drug first (saved), ctrl appended; drug: b (saved-kept), then a,c default-appended
  expect_identical(
    out,
    c("s_drug_b", "s_drug_a", "s_drug_c", "s_ctrl_1", "s_ctrl_2")
  )
})

test_that("pelsa_build_sample_order handles a single-replicate condition", {
  cdesc <- data.frame(
    cond = c("a", "b", "b"),
    rep  = c("r1", "r1", "r2"),
    row.names = c("sa1", "sb1", "sb2"),
    stringsAsFactors = FALSE
  )
  out <- pelsa_build_sample_order(c("a", "b"), NULL, cdesc, "cond", "rep")
  expect_identical(out, c("sa1", "sb1", "sb2"))
})

# ---- testServer: per-dataset state + apply-all + sample_order ----------------

.ordering_test_gp <- function() {
  # Two omes with DIFFERENT cdesc column names to exercise per-dataset scope.
  cdesc1 <- data.frame(
    grp = c("ctrl", "ctrl", "drug", "drug"),
    rid = c("2", "1", "B", "A"),
    row.names = c("p_c2", "p_c1", "p_dB", "p_dA"),
    stringsAsFactors = FALSE
  )
  cdesc2 <- data.frame(
    grp = c("ctrl", "drug"),
    rid = c("1", "1"),
    row.names = c("r_c1", "r_d1"),
    stringsAsFactors = FALSE
  )
  mk_gct <- function(cdesc) {
    mat <- matrix(rnorm(2 * nrow(cdesc)), nrow = 2,
                  dimnames = list(c("f1", "f2"), rownames(cdesc)))
    methods::new("GCT", mat = mat,
                 cdesc = cdesc,
                 rdesc = data.frame(id = c("f1", "f2"), stringsAsFactors = FALSE),
                 cid = rownames(cdesc), rid = c("f1", "f2"))
  }
  list(
    GCTs = list(prot = mk_gct(cdesc1), rna = mk_gct(cdesc2)),
    parameters = list(prot = list(annotation_column = NA),
                      rna  = list(annotation_column = NA))
  )
}

test_that("Issue A: the merged compound handler is per-ome isolated", {
  gp <- .ordering_test_gp()
  GCTs_and_params <- shiny::reactiveVal(gp)
  globals <- shiny::reactiveValues(default_ome = "prot",
                                   colors = list(prot = NULL, rna = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active <- shiny::reactiveVal("prot")
  active_dataset <- shiny::reactive(active())
  path <- pelsa_compound_markers_path()
  skip_if(path == "", "compound_markers.yaml not installed")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset,
                setup_active_dataset = active_dataset),
    {
      # Record + autofill for "prot" must not touch "rna".
      session$setInputs(pelsa_compound = "Rapamycin")
      expect_identical(setup_state$compound[["prot"]], "Rapamycin")
      expect_equal(nrow(setup_state$marker_rows[["prot"]]), 3L)
      expect_null(setup_state$compound[["rna"]])

      # Switch the active dataset to "rna"; a selection records into "rna" only,
      # leaving "prot"'s recorded compound + markers intact.
      active("rna"); session$flushReact()
      session$setInputs(pelsa_compound = "")
      expect_identical(setup_state$compound[["rna"]], "")
      expect_equal(nrow(setup_state$marker_rows[["rna"]]), 0L)
      expect_identical(setup_state$compound[["prot"]], "Rapamycin")
      expect_equal(nrow(setup_state$marker_rows[["prot"]]), 3L)
    }
  )
})

test_that("per-dataset condition/replicate columns are stored as named lists", {
  gp <- .ordering_test_gp()
  GCTs_and_params <- shiny::reactiveVal(gp)
  globals <- shiny::reactiveValues(default_ome = "prot",
                                   colors = list(prot = NULL, rna = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("prot")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      # No checkbox group: both uploaded datasets are non-skipped (analyzed) by
      # default. Columns default to "(none)" (no auto-seed) - set them per ome.
      session$flushReact()
      expect_setequal(checked_datasets(), c("prot", "rna"))
      expect_true(is.list(setup_state$condition_col))

      # Set the per-dataset column inputs (observers are registered for all
      # non-skipped omes at startup). prot is dataset 1 (_d1), rna is 2 (_d2).
      session$setInputs(pelsa_condition_col_d1 = "grp",
                        pelsa_replicate_col_d1 = "rid",
                        pelsa_condition_col_d2 = "grp",
                        pelsa_replicate_col_d2 = "rid")
      session$flushReact()
      expect_identical(setup_state$condition_col[["prot"]], "grp")
      expect_identical(setup_state$condition_col[["rna"]], "grp")

      # sample_order computed for prot from its chosen columns.
      expect_true(!is.null(setup_state$sample_order[["prot"]]))
      expect_identical(
        sort(setup_state$sample_order[["prot"]]),
        sort(c("p_c2", "p_c1", "p_dB", "p_dA"))
      )
    }
  )
})

test_that("skipping a dataset PRESERVES its per-dataset config (greying only)", {
  gp <- .ordering_test_gp()
  GCTs_and_params <- shiny::reactiveVal(gp)
  globals <- shiny::reactiveValues(default_ome = "prot",
                                   colors = list(prot = NULL, rna = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("prot")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      # Both non-skipped by default; give rna a condition column.
      session$flushReact()
      session$setInputs(pelsa_condition_col_d2 = "grp",
                        pelsa_replicate_col_d2 = "rid")
      session$flushReact()
      expect_identical(setup_state$condition_col[["rna"]], "grp")

      # SKIP rna: it leaves the analyzed (checked) set, BUT its config column
      # choices are PRESERVED (greying is purely visual; un-skip restores it).
      setup_state$skip <- list(rna = TRUE)
      session$flushReact()
      expect_false("rna" %in% checked_datasets())
      expect_identical(setup_state$condition_col[["rna"]], "grp")
      expect_identical(setup_state$replicate_col[["rna"]], "rid")
    }
  )
})

test_that("a new upload removing a dataset prunes its per-dataset state", {
  gp <- .ordering_test_gp()
  GCTs_and_params <- shiny::reactiveVal(gp)
  globals <- shiny::reactiveValues(default_ome = "prot",
                                   colors = list(prot = NULL, rna = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("prot")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      session$flushReact()
      session$setInputs(pelsa_condition_col_d2 = "grp",
                        pelsa_replicate_col_d2 = "rid")
      session$flushReact()
      expect_true("rna" %in% names(setup_state$condition_col))

      # A NEW upload drops rna from the uploaded set (all_omes()) -> its
      # per-dataset state is pruned (keyed off all_omes(), not skip).
      GCTs_and_params(list(GCTs = list(prot = gp$GCTs$prot),
                           parameters = list(prot = gp$parameters$prot)))
      session$flushReact()
      expect_false("rna" %in% names(setup_state$condition_col))
      expect_false("rna" %in% names(setup_state$sample_order))
    }
  )
})

test_that("changing a dataset's condition_col reseeds its condition order", {
  gp <- .ordering_test_gp()
  GCTs_and_params <- shiny::reactiveVal(gp)
  globals <- shiny::reactiveValues(default_ome = "prot",
                                   colors = list(prot = NULL, rna = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("prot")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      session$flushReact()
      # Choose grp -> conditions ctrl, drug.
      session$setInputs(pelsa_condition_col_d1 = "grp",
                        pelsa_replicate_col_d1 = "rid")
      session$flushReact()
      expect_setequal(setup_state$condition_order[["prot"]], c("ctrl", "drug"))

      # Switch cond col to rid -> conditions become the rid values.
      session$setInputs(pelsa_condition_col_d1 = "rid")
      session$flushReact()
      expect_setequal(setup_state$condition_order[["prot"]],
                      unique(gp$GCTs$prot@cdesc$rid))
    }
  )
})

test_that("apply-all copies source dataset config to compatible datasets", {
  gp <- .ordering_test_gp()  # both have cols grp, rid -> compatible
  GCTs_and_params <- shiny::reactiveVal(gp)
  globals <- shiny::reactiveValues(default_ome = "prot",
                                   colors = list(prot = NULL, rna = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("prot")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      session$flushReact()  # both datasets non-skipped by default
      # Source (active=prot): set FASTA/compound/cols explicitly.
      session$setInputs(pelsa_fasta = data.frame(
                          name = "human.fasta", size = 1, type = "",
                          datapath = "/tmp/human.fasta", stringsAsFactors = FALSE),
                        pelsa_compound = "Rapamycin",
                        pelsa_condition_col_d1 = "grp",
                        pelsa_replicate_col_d1 = "rid")
      session$flushReact()

      # Give the source a non-default condition order.
      session$setInputs(pelsa_condition_order_d1 = c("drug", "ctrl"))
      session$flushReact()

      session$setInputs(pelsa_apply_all = 1)
      session$flushReact()

      # FASTA / compound / markers transfer VERBATIM.
      expect_identical(setup_state$fasta_path[["rna"]], "/tmp/human.fasta")
      expect_identical(setup_state$compound[["rna"]], "Rapamycin")
      expect_equal(nrow(setup_state$marker_rows[["rna"]]),
                   nrow(setup_state$marker_rows[["prot"]]))
      # Condition + replicate COLUMNS transfer (column names are shared).
      expect_identical(setup_state$condition_col[["rna"]], "grp")
      expect_identical(setup_state$replicate_col[["rna"]], "rid")
      # Condition ORDER transfers (condition VALUES are shared across datasets).
      expect_identical(setup_state$condition_order[["rna"]], c("drug", "ctrl"))

      # Honest apply-all: replicate ordering uses each dataset's own default - so
      # rna's replicate_order references ITS OWN samples (r_c1 / r_d1), never the
      # source's (p_c1 / p_c2 / p_dA / p_dB).
      rna_rep <- setup_state$replicate_order[["rna"]]
      rna_samples <- unlist(rna_rep, use.names = FALSE)
      src_samples <- c("p_c1", "p_c2", "p_dA", "p_dB")
      expect_false(any(src_samples %in% rna_samples))
      expect_true(all(rna_samples %in% c("r_c1", "r_d1")))
    }
  )
})

test_that("observer-dedup registry stays bounded across repeated re-renders", {
  gp <- .ordering_test_gp()
  GCTs_and_params <- shiny::reactiveVal(gp)
  globals <- shiny::reactiveValues(default_ome = "prot",
                                   colors = list(prot = NULL, rna = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("prot")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      # Choose prot's condition/replicate columns so its multi-replicate
      # conditions exist (columns default to "(none)" now -> no cond observers
      # until a real column is chosen).
      session$flushReact()
      session$setInputs(pelsa_condition_col_d1 = "grp",
                        pelsa_replicate_col_d1 = "rid")
      session$flushReact()

      # Toggle the analyzed (non-skipped) set many times via the per-ome skip
      # flag (the checkbox group is gone; checked_datasets() derives from skip).
      for (k in 1:6) {
        setup_state$skip <- list()            # both analyzed
        session$flushReact()
        setup_state$skip <- list(rna = TRUE)  # rna skipped
        session$flushReact()
      }
      reg <- setup_observer_registry()
      # Registry is keyed by dataset/condition INDEX, so it is bounded by the
      # number of distinct datasets+conditions regardless of toggle count.
      expect_lt(length(reg), 12L)
      # And it must NOT have grown unboundedly (no per-toggle duplication).
      expect_true(length(reg) == length(unique(reg)))

      # prot has 2 MULTI-replicate conditions (ctrl=2, drug=2) -> 2 cond keys.
      # rna's conditions are SINGLE-replicate -> NO cond observers registered
      # (item 3: single-rep conditions collapse to a static label, no controls).
      cond_keys <- grep("_cond_", reg, value = TRUE)
      expect_setequal(cond_keys, c("ds_1_cond_1", "ds_1_cond_2"))
    }
  )
})

test_that("apply-all (button) leaves a target dataset editable afterward", {
  gp <- .ordering_test_gp()  # both compatible (cols grp, rid)
  GCTs_and_params <- shiny::reactiveVal(gp)
  globals <- shiny::reactiveValues(default_ome = "prot",
                                   colors = list(prot = NULL, rna = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("prot")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      session$flushReact()  # both datasets non-skipped
      session$setInputs(pelsa_condition_col_d1 = "grp",
                        pelsa_replicate_col_d1 = "rid")
      session$flushReact()

      session$setInputs(pelsa_apply_all = 1)
      session$flushReact()
      # apply-all (a one-shot button) copied prot's cols to rna.
      expect_identical(setup_state$condition_col[["rna"]], "grp")

      # After apply-all, a direct edit to the TARGET dataset (rna, index 2) still
      # takes hold (the button does not lock the target).
      session$setInputs(pelsa_condition_col_d2 = "rid")
      session$flushReact()
      expect_identical(setup_state$condition_col[["rna"]], "rid")
    }
  )
})

# ---- REGRESSION: the setup_state SEAM (CRITICAL) -----------------------------
# The Tab server must return setup_state as a REACTIVE that yields a plain
# snapshot LIST (not the bare reactiveValues). The consumers (Sections 2 & 3)
# guard with is.function() and CALL the seam with (); a bare reactiveValues is
# NOT a function, so it would be silently downgraded to reactive(NULL) in
# production (markers / ordering / species lost). This test would have caught
# that: it asserts the returned seam is.function() AND that calling it exposes
# the live setup_state fields.
test_that("Tab server returns setup_state as a reactive yielding the live snapshot", {
  gp <- .ordering_test_gp()
  GCTs_and_params <- shiny::reactiveVal(gp)
  globals <- shiny::reactiveValues(default_ome = "prot",
                                   colors = list(prot = NULL, rna = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("prot")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      session$flushReact()  # prot non-skipped by default
      session$setInputs(pelsa_condition_col_d1 = "grp",
                        pelsa_replicate_col_d1 = "rid",
                        pelsa_fasta = data.frame(
                          name = "human.fasta", size = 1, type = "",
                          datapath = "/tmp/human.fasta", stringsAsFactors = FALSE))
      session$flushReact()

      # Set a marker so we can prove it flows through the seam (per-ome).
      session$setInputs(pelsa_marker_input = "P12345")
      session$setInputs(pelsa_add_markers = 1)
      session$flushReact()

      seam <- session$returned$setup_state

      # (1) The seam is a FUNCTION / reactive (is.function TRUE)  -  the exact
      # property the consumer is.function() guards on. A bare reactiveValues
      # would FAIL this and be downgraded to reactive(NULL) in production.
      expect_true(is.function(seam))

      # (2) Calling it yields a plain LIST populated from the live reactiveValues.
      snap <- seam()
      expect_true(is.list(snap))
      expect_false(shiny::is.reactivevalues(snap))

      # (3) Every field the consumers read is present + populated from live state
      # (fasta_path / marker_rows are now PER-OME named lists).
      expect_identical(snap$fasta_path[["prot"]], "/tmp/human.fasta")
      expect_identical(snap$condition_col[["prot"]], "grp")
      expect_setequal(snap$condition_order[["prot"]], c("ctrl", "drug"))
      expect_true(!is.null(snap$sample_order[["prot"]]))
      expect_true("P12345" %in% snap$marker_rows[["prot"]]$accession)
    }
  )
})

# ---- REGRESSION: H1  -  replicate order survives a condition-column switch -----
# Switching a dataset's condition column must RE-WIRE the per-condition replicate
# observers to the NEW column's conditions, so user replicate ordering for the
# new column is RETAINED (not silently dropped to default because a stale
# positional observer wrote to the OLD condition value).
test_that("H1: replicate order is retained after switching the condition column", {
  # cdesc where BOTH candidate condition columns yield multi-replicate conditions
  # so the per-condition replicate observers exist under each column.
  cdesc <- data.frame(
    colA = c("a1", "a1", "a2", "a2"),  # 2 conds x 2 reps
    colB = c("b1", "b2", "b1", "b2"),  # 2 conds x 2 reps (orthogonal split)
    rid  = c("r1", "r2", "r1", "r2"),
    row.names = c("s1", "s2", "s3", "s4"),
    stringsAsFactors = FALSE
  )
  mat <- matrix(rnorm(2 * nrow(cdesc)), nrow = 2,
                dimnames = list(c("f1", "f2"), rownames(cdesc)))
  gct <- methods::new("GCT", mat = mat, cdesc = cdesc,
                      rdesc = data.frame(id = c("f1", "f2"),
                                         stringsAsFactors = FALSE),
                      cid = rownames(cdesc), rid = c("f1", "f2"))
  gp <- list(GCTs = list(prot = gct),
             parameters = list(prot = list(annotation_column = NA)))

  GCTs_and_params <- shiny::reactiveVal(gp)
  globals <- shiny::reactiveValues(default_ome = "prot", colors = list(prot = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("prot")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      session$flushReact()

      # Column A: conditions a1 (s1,s2), a2 (s3,s4). Set a custom replicate order
      # for a1's card (position j=1): reverse it to s2, s1.
      session$setInputs(pelsa_condition_col_d1 = "colA",
                        pelsa_replicate_col_d1 = "rid")
      session$flushReact()
      session$setInputs(pelsa_replicate_order_d1_c1 = c("s2", "s1"))
      session$flushReact()
      expect_identical(setup_state$replicate_order[["prot"]][["a1"]],
                       c("s2", "s1"))

      # Switch to column B: conditions b1 (s1,s3), b2 (s2,s4). Set a custom
      # replicate order for b1's card (position j=1): reverse to s3, s1.
      session$setInputs(pelsa_condition_col_d1 = "colB")
      session$flushReact()
      session$setInputs(pelsa_replicate_order_d1_c1 = c("s3", "s1"))
      session$flushReact()

      # H1: the NEW column's replicate order is RETAINED under the NEW condition
      # name b1 (NOT silently written to the stale a1 / dropped to default).
      expect_identical(setup_state$replicate_order[["prot"]][["b1"]],
                       c("s3", "s1"))
      # And the canonical sample order honors it (b1 first if condition order
      # keeps b1 first; assert b1's samples appear reversed within the order).
      so <- setup_state$sample_order[["prot"]]
      expect_true(which(so == "s3") < which(so == "s1"))

      # Switch BACK to A and confirm no corruption: a fresh order takes hold.
      session$setInputs(pelsa_condition_col_d1 = "colA")
      session$flushReact()
      session$setInputs(pelsa_replicate_order_d1_c1 = c("s1", "s2"))
      session$flushReact()
      expect_identical(setup_state$replicate_order[["prot"]][["a1"]],
                       c("s1", "s2"))

      # No observer leak across the switches: the registry is bounded + unique.
      reg <- setup_observer_registry()
      expect_true(length(reg) == length(unique(reg)))
      # Only 2 positions ever (i=1, j in {1,2}) -> at most 2 cond keys.
      cond_keys <- grep("_cond_", reg, value = TRUE)
      expect_true(length(cond_keys) <= 2L)
    }
  )
})

test_that("an NA condition value is dropped from the wired sample_order", {
  # cdesc with an NA in the condition column: the NA-row sample must NOT appear
  # in the canonical sample_order (the pure helper drops NA; assert the SERVER
  # path too).
  cdesc <- data.frame(
    grp = c("ctrl", NA, "drug", "drug"),
    rid = c("1", "1", "A", "B"),
    row.names = c("s_ctrl", "s_na", "s_dA", "s_dB"),
    stringsAsFactors = FALSE
  )
  mat <- matrix(rnorm(2 * nrow(cdesc)), nrow = 2,
                dimnames = list(c("f1", "f2"), rownames(cdesc)))
  gct <- methods::new("GCT", mat = mat, cdesc = cdesc,
                      rdesc = data.frame(id = c("f1", "f2"),
                                         stringsAsFactors = FALSE),
                      cid = rownames(cdesc), rid = c("f1", "f2"))
  gp <- list(GCTs = list(prot = gct),
             parameters = list(prot = list(annotation_column = NA)))

  GCTs_and_params <- shiny::reactiveVal(gp)
  globals <- shiny::reactiveValues(default_ome = "prot",
                                   colors = list(prot = NULL))
  GCTs_original <- shiny::reactiveVal(NULL)
  active_dataset <- shiny::reactive("prot")

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset),
    {
      session$flushReact()
      session$setInputs(pelsa_condition_col_d1 = "grp",
                        pelsa_replicate_col_d1 = "rid")
      session$flushReact()

      so <- setup_state$sample_order[["prot"]]
      expect_false("s_na" %in% so)  # NA-condition sample dropped
      expect_setequal(so, c("s_ctrl", "s_dA", "s_dB"))
    }
  )
})

