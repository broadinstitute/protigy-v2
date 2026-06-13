################################################################################
# Tests for the PELSA Start-Analysis validation + compute-pipeline assembly (5D).
#
#   pelsa_validate_setup(setup_snapshot, gcts, database_dir)
#       -> list(ok, errors)  — closed-form pre-flight checklist.
#   pelsa_run_analysis(gcts, gcts_original, setup_snapshot, fasta_map, feat_df)
#       -> named-by-dataset list of per-dataset cache objects.
#   pelsa_run_analysis_one(...) — the single-dataset assembly.
#   pelsa_condition_map_for / pelsa_dataset_peptide_frame — assembly seams.
#
# NO LIVE NETWORK: the pipeline uses an INJECTED fasta_map + a hand-set feat_df
# (and never calls pelsa_fetch_uniprot — cache-as-is). Validation tests use a
# temp database dir with fake species/fasta folders (no network, no real DB).
################################################################################

library(testthat)
source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))

# ---- shared fixtures ---------------------------------------------------------

# A temp database dir with a species that HAS a fasta, and one that does NOT.
# Created under tempfile() (auto-cleaned at session end); cheap and isolated.
.mk_database_dir <- function() {
  tmp <- tempfile("pelsa_db_")
  dir.create(file.path(tmp, "human", "fasta"), recursive = TRUE)
  writeLines(c(">sp|P1|X", "MABC"), file.path(tmp, "human", "fasta", "x.fasta"))
  dir.create(file.path(tmp, "fishless", "fasta"), recursive = TRUE)  # empty
  tmp
}

# Build a cmapR GCT from the synthetic peptide frame (rdesc = annotation cols,
# mat = intensity cols, cdesc = sample -> condition).
.mk_gct <- function(syn) {
  peptides <- syn$peptides
  sc <- syn$sample_cols
  rids <- paste0("pep", seq_len(nrow(peptides)))
  mat <- as.matrix(peptides[, sc])
  rownames(mat) <- rids
  rdesc <- peptides[, setdiff(colnames(peptides), sc), drop = FALSE]
  rownames(rdesc) <- rids
  cdesc <- data.frame(
    condition = sub("_R[0-9]+$", "", sc),
    row.names = sc, stringsAsFactors = FALSE
  )
  cmapR::GCT(mat = mat, rdesc = rdesc, cdesc = cdesc)
}

# A small hand-set feature cache (NO network).
.mk_feat_df <- function() {
  data.frame(
    accession     = c("SHARED1", "DUPPROT", "TIEPROT"),
    start         = c(1L, 1L, 1L),
    end           = c(50L, 60L, 40L),
    feature_class = c("domain", "domain", "domain"),
    stringsAsFactors = FALSE
  )
}

# ---- pelsa_species_fasta_path ------------------------------------------------

test_that("pelsa_species_fasta_path finds a fasta or returns NA", {
  db <- .mk_database_dir()
  expect_true(!is.na(pelsa_species_fasta_path(db, "human")))
  expect_true(is.na(pelsa_species_fasta_path(db, "fishless")))   # no fasta file
  expect_true(is.na(pelsa_species_fasta_path(db, "absent")))     # no folder
  expect_true(is.na(pelsa_species_fasta_path("", "human")))
})

# ---- pelsa_validate_setup (closed-form) --------------------------------------

test_that("validate fails when no dataset is checked", {
  db <- .mk_database_dir()
  snap <- list(datasets = character(0), species = "human",
               condition_col = list(), condition_order = list())
  v <- pelsa_validate_setup(snap, gcts = NULL, database_dir = db)
  expect_false(v$ok)
  expect_true(any(grepl("at least one dataset", v$errors)))
})

test_that("validate fails when a checked dataset lacks a condition column", {
  db <- .mk_database_dir()
  snap <- list(datasets = "ds1", species = "human",
               condition_col = list(), condition_order = list(ds1 = "A"))
  v <- pelsa_validate_setup(snap, gcts = NULL, database_dir = db)
  expect_false(v$ok)
  expect_true(any(grepl("condition grouping column", v$errors)))
})

test_that("validate fails when condition order is not confirmed", {
  db <- .mk_database_dir()
  snap <- list(datasets = "ds1", species = "human",
               condition_col = list(ds1 = "cond"),
               condition_order = list())          # no order
  v <- pelsa_validate_setup(snap, gcts = NULL, database_dir = db)
  expect_false(v$ok)
  expect_true(any(grepl("confirm the condition order", v$errors)))
})

test_that("validate emits the No-FASTA message when the species has no fasta", {
  db <- .mk_database_dir()
  snap <- list(datasets = "ds1", species = "fishless",
               condition_col = list(ds1 = "cond"),
               condition_order = list(ds1 = "A"))
  v <- pelsa_validate_setup(snap, gcts = NULL, database_dir = db)
  expect_false(v$ok)
  expect_true(any(grepl("^No FASTA for fishless", v$errors)))
})

test_that("validate fails when no species is selected", {
  db <- .mk_database_dir()
  snap <- list(datasets = "ds1", species = NULL,
               condition_col = list(ds1 = "cond"),
               condition_order = list(ds1 = "A"))
  v <- pelsa_validate_setup(snap, gcts = NULL, database_dir = db)
  expect_false(v$ok)
  expect_true(any(grepl("Select a species", v$errors)))
})

test_that("validate flags a condition column missing from a dataset's cdesc", {
  db <- .mk_database_dir()
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 2)
  gct <- .mk_gct(syn)  # cdesc has only 'condition'
  snap <- list(datasets = "ds1", species = "human",
               condition_col = list(ds1 = "NOT_A_COLUMN"),
               condition_order = list(ds1 = "A"))
  v <- pelsa_validate_setup(snap, gcts = list(ds1 = gct), database_dir = db)
  expect_false(v$ok)
  expect_true(any(grepl("is not in its annotations", v$errors)))
})

test_that("validate passes with everything present (empty markers still ok)", {
  db <- .mk_database_dir()
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 2)
  gct <- .mk_gct(syn)
  snap <- list(datasets = "ds1", species = "human",
               marker_rows = pelsa_empty_marker_rows(),   # EMPTY markers
               condition_col = list(ds1 = "condition"),
               condition_order = list(ds1 = c("AY9944_10uM", "DMSO", "LowN")))
  v <- pelsa_validate_setup(snap, gcts = list(ds1 = gct), database_dir = db)
  expect_true(v$ok)
  expect_length(v$errors, 0L)
})

test_that("validate accumulates ALL failures at once", {
  db <- .mk_database_dir()
  snap <- list(datasets = c("ds1", "ds2"), species = "fishless",
               condition_col = list(ds1 = "cond"),    # ds2 missing
               condition_order = list())               # both missing order
  v <- pelsa_validate_setup(snap, gcts = NULL, database_dir = db)
  expect_false(v$ok)
  # ds2 missing column + both missing order + no fasta = several errors
  expect_gt(length(v$errors), 3L)
})

# ---- pelsa_condition_map_for -------------------------------------------------

test_that("condition map keys samples to conditions and drops absent/NA", {
  cdesc <- data.frame(
    condition = c("A", "A", "B", NA),
    row.names = c("s1", "s2", "s3", "s4"),
    stringsAsFactors = FALSE
  )
  cm <- pelsa_condition_map_for(cdesc, c("s1", "s2", "s3", "s4", "s5"),
                                "condition")
  expect_identical(unname(cm), c("A", "A", "B"))   # s4 NA, s5 absent dropped
  expect_identical(names(cm), c("s1", "s2", "s3"))
})

test_that("condition map fails fast on a missing column", {
  cdesc <- data.frame(condition = "A", row.names = "s1")
  expect_error(pelsa_condition_map_for(cdesc, "s1", "nope"), "not in cdesc")
})

# ---- pelsa_dataset_peptide_frame ---------------------------------------------

test_that("peptide frame = cbind(rdesc, mat) for a GCT; df passes through", {
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 3)
  gct <- .mk_gct(syn)
  pf <- pelsa_dataset_peptide_frame(gct)
  expect_true("PG.ProteinAccessions" %in% colnames(pf))
  expect_true(all(syn$sample_cols %in% colnames(pf)))
  expect_equal(nrow(pf), nrow(syn$peptides))

  # data.frame seam: returned unchanged.
  expect_identical(pelsa_dataset_peptide_frame(syn$peptides), syn$peptides)
})

# ---- pelsa_run_analysis_one (assembly, injected fasta + feat) ----------------

test_that("run_analysis_one builds all cache components with sane shapes", {
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 10)
  gct <- .mk_gct(syn)
  feat_df <- .mk_feat_df()

  one <- pelsa_run_analysis_one(
    gct = gct, gct_original = gct,
    fasta_map = syn$fasta, feat_df = feat_df,
    condition_col = "condition"
  )

  # All documented components present.
  expect_setequal(
    names(one),
    c("matched", "unmatched", "cv", "n_quantified", "depth_summary",
      "coverage", "peptide_metrics", "annotation", "unannotated", "qc")
  )

  # matched / unmatched (2B).
  expect_s3_class(one$matched, "data.frame")
  expect_true(all(c("pep_start", "pep_end", "accession") %in%
                    colnames(one$matched)))
  expect_gt(nrow(one$matched), 0L)
  # The seeded absent + bad-seq peptides land in unmatched.
  expect_true("sequence_not_found" %in% one$unmatched$reason ||
                "accession_absent" %in% one$unmatched$reason)
  expect_true("bad_sequence_format" %in% one$unmatched$reason)

  # CV (2D) on the raw intensities: one row per (peptide, condition).
  expect_s3_class(one$cv, "data.frame")
  expect_true(all(c("row_id", "condition", "cv_pct", "cv_status") %in%
                    colnames(one$cv)))
  expect_setequal(unique(one$cv$condition), c("AY9944_10uM", "DMSO", "LowN"))
  expect_equal(nrow(one$cv), nrow(syn$peptides) * 3L)
  # The LowN edge case (first row forced to 1 non-NA) -> insufficient_replicates.
  low1 <- one$cv[one$cv$row_id == 1L & one$cv$condition == "LowN", ]
  expect_identical(low1$cv_status, "insufficient_replicates")

  # Depth (2E).
  expect_named(one$n_quantified)
  expect_length(one$n_quantified, length(syn$sample_cols))
  expect_true(all(c("mean_n", "median_n", "cv_pct", "total_n_peptides") %in%
                    colnames(one$depth_summary)))
  expect_equal(one$depth_summary$total_n_peptides, nrow(syn$peptides))

  # Coverage (2F): [0,1] bounds where defined.
  expect_true(all(c("accession", "coverage", "protein_length") %in%
                    colnames(one$coverage)))
  cov <- one$coverage$coverage[!is.na(one$coverage$coverage)]
  expect_true(all(cov >= 0 & cov <= 1))

  # Missed cleavage + peptide length (2C).
  expect_equal(nrow(one$peptide_metrics), nrow(syn$peptides))
  expect_true(all(c("missed_cleavages", "peptide_length") %in%
                    colnames(one$peptide_metrics)))

  # Annotation (2I): the volcano feature-color column is present.
  expect_true("feature_class_primary" %in% colnames(one$annotation))

  # QC counts.
  expect_true(all(c("n_peptides", "n_matched_rows", "n_unmatched_rows",
                    "n_unannotated_accessions") %in% names(one$qc)))
  expect_equal(one$qc$n_peptides, nrow(syn$peptides))
})

test_that("run_analysis_one skips CV when no original GCT is supplied", {
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 5)
  gct <- .mk_gct(syn)
  one <- pelsa_run_analysis_one(
    gct = gct, gct_original = NULL,
    fasta_map = syn$fasta, feat_df = .mk_feat_df(),
    condition_col = "condition"
  )
  expect_null(one$cv)
  # Everything else still computes.
  expect_gt(nrow(one$matched), 0L)
  expect_false(is.null(one$depth_summary))
})

test_that("run_analysis_one is GRACEFUL when no peptide FASTA-maps (no error)", {
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 6)
  gct <- .mk_gct(syn)
  # An EMPTY fasta_map -> every peptide is accession_absent -> zero matched, but
  # this is NOT an error: a valid cache with qc$n_matched_rows == 0L.
  one <- pelsa_run_analysis_one(
    gct = gct, gct_original = gct,
    fasta_map = list(), feat_df = .mk_feat_df(),
    condition_col = "condition"
  )
  expect_false(pelsa_analysis_failed(one))
  expect_equal(one$qc$n_matched_rows, 0L)
  expect_equal(nrow(one$matched), 0L)
  expect_equal(nrow(one$coverage), 0L)
  # The rest still computes (depth/peptide_metrics are FASTA-independent).
  expect_false(is.null(one$depth_summary))
  expect_equal(nrow(one$peptide_metrics), nrow(syn$peptides))
})

test_that("run_analysis_one returns cv = NULL when the condition column is all-NA", {
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 6)
  # Build a GCT whose cdesc condition column is entirely NA.
  peptides <- syn$peptides; sc <- syn$sample_cols
  rids <- paste0("pep", seq_len(nrow(peptides)))
  mat <- as.matrix(peptides[, sc]); rownames(mat) <- rids
  rdesc <- peptides[, setdiff(colnames(peptides), sc), drop = FALSE]
  rownames(rdesc) <- rids
  cdesc <- data.frame(condition = rep(NA_character_, length(sc)),
                      row.names = sc, stringsAsFactors = FALSE)
  gct <- cmapR::GCT(mat = mat, rdesc = rdesc, cdesc = cdesc)

  one <- pelsa_run_analysis_one(
    gct = gct, gct_original = gct,
    fasta_map = syn$fasta, feat_df = .mk_feat_df(),
    condition_col = "condition"
  )
  expect_null(one$cv)               # no assignable condition -> CV skipped
  expect_gt(nrow(one$matched), 0L)  # the rest computes
  expect_false(is.null(one$depth_summary))
})

# ---- pelsa_analysis_failed (canonical discriminator) -------------------------

test_that("pelsa_analysis_failed distinguishes success from failure entries", {
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 4)
  ok <- pelsa_run_analysis_one(
    gct = .mk_gct(syn), gct_original = NULL,
    fasta_map = syn$fasta, feat_df = .mk_feat_df(), condition_col = "condition"
  )
  expect_false(pelsa_analysis_failed(ok))
  expect_true(pelsa_analysis_failed(list(error = "boom", stage = "Computing CV")))
})

# ---- pelsa_run_analysis (multi-dataset keyed list, NO network) ---------------

test_that("run_analysis returns a per-dataset keyed list for >1 dataset", {
  syn1 <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 8)
  syn2 <- pelsa_make_synthetic(seed = 2, n_extra_peptides = 8)
  g1 <- .mk_gct(syn1); g2 <- .mk_gct(syn2)
  feat_df <- .mk_feat_df()

  snap <- list(
    datasets = c("dsA", "dsB"), species = "human",
    condition_col = list(dsA = "condition", dsB = "condition")
  )
  res <- pelsa_run_analysis(
    gcts = list(dsA = g1, dsB = g2),
    gcts_original = list(dsA = g1, dsB = g2),
    setup_snapshot = snap,
    fasta_map = syn1$fasta,   # same species -> shared map (injected)
    feat_df = feat_df
  )
  expect_setequal(names(res), c("dsA", "dsB"))
  expect_true("matched" %in% names(res$dsA))
  expect_true("matched" %in% names(res$dsB))
  expect_null(res$dsA$error)
})

test_that("run_analysis only analyzes checked datasets present in gcts", {
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 4)
  g <- .mk_gct(syn)
  snap <- list(datasets = c("dsA", "ghost"), species = "human",
               condition_col = list(dsA = "condition"))
  res <- pelsa_run_analysis(
    gcts = list(dsA = g), gcts_original = list(dsA = g),
    setup_snapshot = snap, fasta_map = syn$fasta, feat_df = .mk_feat_df()
  )
  expect_identical(names(res), "dsA")  # ghost (absent from gcts) dropped
})

test_that("run_analysis captures a per-dataset error (with stage) without aborting others", {
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 4)
  g <- .mk_gct(syn)
  snap <- list(datasets = c("good", "bad"), species = "human",
               condition_col = list(good = "condition", bad = "condition"))
  # 'bad' gets a non-GCT/non-df value -> pelsa_dataset_peptide_frame stop ->
  # captured as list(error=, stage=).
  res <- pelsa_run_analysis(
    gcts = list(good = g, bad = 42L),
    gcts_original = list(good = g, bad = 42L),
    setup_snapshot = snap, fasta_map = syn$fasta, feat_df = .mk_feat_df()
  )
  expect_false(pelsa_analysis_failed(res$good))
  expect_true(pelsa_analysis_failed(res$bad))
  # The failure carries the stage it reached (the discriminator + stage seam).
  expect_true("stage" %in% names(res$bad))
  expect_false(is.na(res$bad$stage))
})

test_that("run_analysis captures the failing STAGE on a deep mid-pipeline error", {
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 4)
  # A peptide frame missing PG.ProteinAccessions passes the read stage but throws
  # inside the explode/map stage -> stage == "Mapping peptide positions".
  pf <- syn$peptides
  pf$PG.ProteinAccessions <- NULL
  snap <- list(datasets = "d", species = "human",
               condition_col = list(d = "condition"))
  res <- pelsa_run_analysis(
    gcts = list(d = pf), gcts_original = list(d = NULL),
    setup_snapshot = snap, fasta_map = syn$fasta, feat_df = .mk_feat_df()
  )
  expect_true(pelsa_analysis_failed(res$d))
  expect_identical(res$d$stage, "Mapping peptide positions")
})

test_that("run_analysis errors when no checked dataset is present", {
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 2)
  snap <- list(datasets = character(0), species = "human",
               condition_col = list())
  expect_error(
    pelsa_run_analysis(gcts = list(), gcts_original = list(),
                       setup_snapshot = snap, fasta_map = syn$fasta,
                       feat_df = .mk_feat_df()),
    "no checked datasets"
  )
})

# ---- observer: invalid setup -> errors shown, NO compute, seam not driven ----
# (The VALID compute path is exercised directly via pelsa_run_analysis above —
# driving the real observer's compute would read the large bundled FASTA. We
# test the observer's GATE: an invalid click must surface validation + not
# touch the analysis cache nor the analyzed-datasets seam.)

test_that("Start-Analysis observer gates on validation (no compute when invalid)", {
  fx <- tryCatch({
    utils::data("brca_retrospective_v5.0_proteome_gct", package = "Protigy")
    gct <- get("brca_retrospective_v5.0_proteome_gct")
    list(gp = list(GCTs = list(proteome = gct),
                   parameters = list(proteome = list(annotation_column = NA))))
  }, error = function(e) NULL)
  skip_if(is.null(fx), "brca proteome test data not available")

  GCTs_and_params <- shiny::reactiveVal(fx$gp)
  globals <- shiny::reactiveValues(default_ome = "proteome",
                                   colors = list(proteome = NULL))
  GCTs_original <- shiny::reactiveVal(fx$gp$GCTs)
  active_dataset <- shiny::reactive("proteome")

  seam_calls <- new.env()
  seam_calls$n <- 0L
  set_analyzed <- function(ds) seam_calls$n <- seam_calls$n + 1L

  shiny::testServer(
    PELSASection1_Tab_Server,
    args = list(GCTs_and_params = GCTs_and_params, globals = globals,
                GCTs_original = GCTs_original, active_dataset = active_dataset,
                set_analyzed_datasets = set_analyzed),
    {
      # Force an INVALID setup: no datasets checked.
      setup_state$datasets <- character(0)
      setup_state$species <- NULL
      # Two ticks: testServer treats the first value of an ignoreInit observer
      # as the init it skips; a second distinct value is the real click.
      session$setInputs(pelsa_start = 1)
      session$flushReact()
      session$setInputs(pelsa_start = 2)
      session$flushReact()

      # No compute happened: the analysis cache stays NULL.
      expect_null(session$returned$analysis())
      # The seam was NOT driven.
      expect_equal(seam_calls$n, 0L)
      # Validation errors were recorded for inline display.
      expect_false(last_validation()$ok)
      expect_gt(length(last_validation()$errors), 0L)
    }
  )
})

# ---- pelsa_validation_msg_ui (pure tag) --------------------------------------

test_that("validation msg UI is NULL when ok, a tag with errors otherwise", {
  expect_null(pelsa_validation_msg_ui(list(ok = TRUE, errors = character(0))))
  ui <- pelsa_validation_msg_ui(list(ok = FALSE, errors = c("e1", "e2")))
  html <- as.character(ui)
  expect_true(grepl("e1", html) && grepl("e2", html))
})
