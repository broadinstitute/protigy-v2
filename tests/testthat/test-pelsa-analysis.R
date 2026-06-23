################################################################################
# Tests for the PELSA Start-Analysis validation + compute-pipeline assembly (5D).
#
#   pelsa_validate_setup(setup_snapshot, gcts, database_dir)
#       -> list(ok, errors)   -  closed-form pre-flight checklist.
#   pelsa_run_analysis(gcts, gcts_original, setup_snapshot, fasta_map, feat_df)
#       -> named-by-dataset list of per-dataset cache objects.
#   pelsa_run_analysis_one(...)  -  the single-dataset assembly.
#   pelsa_condition_map_for / pelsa_dataset_peptide_frame  -  assembly seams.
#
# NO LIVE NETWORK: the pipeline uses an INJECTED fasta_map + a hand-set feat_df
# (and never calls pelsa_fetch_uniprot  -  cache-as-is). Validation tests use a
# temp database dir with fake species/fasta folders (no network, no real DB).
################################################################################

library(testthat)
source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))

# ---- shared fixtures ---------------------------------------------------------

# A temp database dir with a species that HAS a fasta, and one that does NOT.
# Created under tempfile() (auto-cleaned at session end); cheap and isolated.
.mk_database_dir <- function() {
  tmp <- tempfile("pelsa_db_")
  dir.create(file.path(tmp, "9606", "fasta"), recursive = TRUE)
  writeLines(c(">sp|P1|X", "MABC"), file.path(tmp, "9606", "fasta", "x.fasta"))
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

# ---- pelsa_setup_snapshot (per-dataset species/compound/markers/skip) --------

test_that("snapshot carries per-ome species/compound/marker_rows/skip", {
  # Per-dataset setup: species/compound/markers are NAMED LISTS keyed by ome
  # (joining the already-per-ome condition/replicate fields), plus a skip flag.
  state <- list(
    datasets        = c("A", "B"),
    species         = list(A = "9606", B = "10090"),
    compound        = list(A = "Rapamycin", B = "AY9944"),
    marker_rows     = list(
      A = data.frame(accession = "P1", gene = "G1", stringsAsFactors = FALSE),
      B = data.frame(accession = "Q2", gene = "G2", stringsAsFactors = FALSE)
    ),
    skip            = list(A = FALSE, B = TRUE),
    condition_col   = list(A = "cond", B = "cond"),
    replicate_col   = list(A = "rep", B = "rep"),
    condition_order = list(A = c("X", "Y"), B = c("X", "Y")),
    replicate_order = list(),
    sample_order    = list()
  )
  snap <- pelsa_setup_snapshot(state)

  expect_identical(snap$species[["A"]], "9606")
  expect_identical(snap$species[["B"]], "10090")
  expect_identical(snap$compound[["A"]], "Rapamycin")
  expect_identical(snap$marker_rows[["A"]]$accession, "P1")
  expect_identical(snap$marker_rows[["B"]]$accession, "Q2")
  expect_false(snap$skip[["A"]])
  expect_true(snap$skip[["B"]])
})

test_that("snapshot defaults per-ome fields to empty lists when unset", {
  # A fresh setup_state with nothing configured: the per-ome fields must be
  # empty lists (not NULL, not a scalar), so downstream [[ome]] indexing is safe.
  state <- list(
    datasets        = character(0),
    species         = NULL,
    compound        = NULL,
    marker_rows     = NULL,
    skip            = NULL,
    condition_col   = NULL,
    replicate_col   = NULL,
    condition_order = NULL,
    replicate_order = NULL,
    sample_order    = NULL
  )
  snap <- pelsa_setup_snapshot(state)

  expect_identical(snap$species, list())
  expect_identical(snap$compound, list())
  expect_identical(snap$marker_rows, list())
  expect_identical(snap$skip, list())
  # indexing a missing ome yields NULL (safe), not an error
  expect_null(snap$species[["nope"]])
})

# ---- pelsa_species_fasta_path ------------------------------------------------

test_that("pelsa_species_fasta_path finds a fasta or returns NA", {
  db <- .mk_database_dir()
  expect_true(!is.na(pelsa_species_fasta_path(db, "9606")))
  expect_true(is.na(pelsa_species_fasta_path(db, "fishless")))   # no fasta file
  expect_true(is.na(pelsa_species_fasta_path(db, "absent")))     # no folder
  expect_true(is.na(pelsa_species_fasta_path("", "9606")))
})

# ---- pelsa_validate_setup (closed-form) --------------------------------------

test_that("validate fails when no dataset is checked", {
  db <- .mk_database_dir()
  snap <- list(datasets = character(0), species = list(),
               condition_col = list(), replicate_col = list(),
               condition_order = list())
  v <- pelsa_validate_setup(snap, gcts = NULL, database_dir = db)
  expect_false(v$ok)
  expect_true(any(grepl("at least one", v$errors)))
})

test_that("validate fails when a checked dataset lacks a condition column", {
  db <- .mk_database_dir()
  snap <- list(datasets = "ds1", species = list(ds1 = "9606"),
               condition_col = list(), replicate_col = list(ds1 = "cond"),
               condition_order = list(ds1 = "A"))
  v <- pelsa_validate_setup(snap, gcts = NULL, database_dir = db)
  expect_false(v$ok)
  expect_true(any(grepl("condition grouping column", v$errors)))
})

test_that("validate fails when condition order is not confirmed", {
  db <- .mk_database_dir()
  snap <- list(datasets = "ds1", species = list(ds1 = "9606"),
               condition_col = list(ds1 = "cond"),
               replicate_col = list(ds1 = "cond"),
               condition_order = list())          # no order
  v <- pelsa_validate_setup(snap, gcts = NULL, database_dir = db)
  expect_false(v$ok)
  expect_true(any(grepl("confirm the condition order", v$errors)))
})

test_that("validate emits the No-FASTA message when the species has no fasta", {
  db <- .mk_database_dir()
  snap <- list(datasets = "ds1", species = list(ds1 = "fishless"),
               condition_col = list(ds1 = "cond"),
               replicate_col = list(ds1 = "cond"),
               condition_order = list(ds1 = "A"))
  v <- pelsa_validate_setup(snap, gcts = NULL, database_dir = db)
  expect_false(v$ok)
  expect_true(any(grepl("^No FASTA for fishless", v$errors)))
})

test_that("validate fails when no species is selected for a dataset", {
  db <- .mk_database_dir()
  snap <- list(datasets = "ds1", species = list(),     # no species for ds1
               condition_col = list(ds1 = "cond"),
               replicate_col = list(ds1 = "cond"),
               condition_order = list(ds1 = "A"))
  v <- pelsa_validate_setup(snap, gcts = NULL, database_dir = db)
  expect_false(v$ok)
  expect_true(any(grepl("species", v$errors)))
})

test_that("validate flags a condition column missing from a dataset's cdesc", {
  db <- .mk_database_dir()
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 2)
  gct <- .mk_gct(syn)  # cdesc has only 'condition'
  snap <- list(datasets = "ds1", species = list(ds1 = "9606"),
               condition_col = list(ds1 = "NOT_A_COLUMN"),
               replicate_col = list(ds1 = "condition"),
               condition_order = list(ds1 = "A"))
  v <- pelsa_validate_setup(snap, gcts = list(ds1 = gct), database_dir = db)
  expect_false(v$ok)
  expect_true(any(grepl("is not in its annotations", v$errors)))
})

test_that("validate passes with everything present (empty markers still ok)", {
  db <- .mk_database_dir()
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 2)
  gct <- .mk_gct(syn)
  snap <- list(datasets = "ds1", species = list(ds1 = "9606"),
               marker_rows = list(ds1 = pelsa_empty_marker_rows()),  # EMPTY ok
               condition_col = list(ds1 = "condition"),
               replicate_col = list(ds1 = "condition"),
               condition_order = list(ds1 = c("AY9944_10uM", "DMSO", "LowN")))
  v <- pelsa_validate_setup(snap, gcts = list(ds1 = gct), database_dir = db)
  expect_true(v$ok)
  expect_length(v$errors, 0L)
})

test_that("validate accumulates ALL failures at once", {
  db <- .mk_database_dir()
  snap <- list(datasets = c("ds1", "ds2"),
               species = list(ds1 = "fishless", ds2 = "fishless"),
               condition_col = list(ds1 = "cond"),    # ds2 missing
               replicate_col = list(),                 # both missing
               condition_order = list())               # both missing order
  v <- pelsa_validate_setup(snap, gcts = NULL, database_dir = db)
  expect_false(v$ok)
  # ds2 missing column + both missing replicate + both missing order + no fasta
  expect_gt(length(v$errors), 3L)
})

# ---- pelsa_validate_setup: per-ome species, "(none)", replicate, skip --------

test_that("validate treats \"(none)\" species/condition/replicate as unset", {
  db <- .mk_database_dir()
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 2)
  gct <- .mk_gct(syn)
  snap <- list(
    datasets        = "ds1",
    species         = list(ds1 = "(none)"),
    condition_col   = list(ds1 = "(none)"),
    replicate_col   = list(ds1 = "(none)"),
    condition_order = list(ds1 = c("AY9944_10uM", "DMSO"))
  )
  v <- pelsa_validate_setup(snap, gcts = list(ds1 = gct), database_dir = db)
  expect_false(v$ok)
  expect_true(any(grepl("ds1", v$errors) & grepl("species", v$errors)))
  expect_true(any(grepl("ds1", v$errors) & grepl("condition grouping", v$errors)))
  expect_true(any(grepl("ds1", v$errors) & grepl("replicate", v$errors)))
})

test_that("validate fails when a non-skipped dataset lacks a replicate column", {
  db <- .mk_database_dir()
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 2)
  gct <- .mk_gct(syn)
  snap <- list(
    datasets        = "ds1",
    species         = list(ds1 = "9606"),
    condition_col   = list(ds1 = "condition"),
    replicate_col   = list(),                      # missing
    condition_order = list(ds1 = c("AY9944_10uM", "DMSO", "LowN"))
  )
  v <- pelsa_validate_setup(snap, gcts = list(ds1 = gct), database_dir = db)
  expect_false(v$ok)
  expect_true(any(grepl("replicate", v$errors)))
})

test_that("validate blocks when all datasets are skipped (empty analyzed set)", {
  db <- .mk_database_dir()
  # datasets = the NON-SKIPPED set; all-skipped => empty.
  snap <- list(
    datasets        = character(0),
    species         = list(),
    condition_col   = list(),
    replicate_col   = list(),
    condition_order = list()
  )
  v <- pelsa_validate_setup(snap, gcts = NULL, database_dir = db)
  expect_false(v$ok)
  expect_true(any(grepl("at least one", v$errors)))
})

test_that("validate ignores skipped datasets (only non-skipped are checked)", {
  db <- .mk_database_dir()
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 2)
  gct <- .mk_gct(syn)
  # ds2 is invalid but SKIPPED, so it is absent from `datasets`; only ds1 (valid)
  # is checked => ok.
  snap <- list(
    datasets        = "ds1",
    species         = list(ds1 = "9606", ds2 = "(none)"),
    condition_col   = list(ds1 = "condition", ds2 = "(none)"),
    replicate_col   = list(ds1 = "condition", ds2 = "(none)"),
    condition_order = list(ds1 = c("AY9944_10uM", "DMSO", "LowN"))
  )
  v <- pelsa_validate_setup(snap, gcts = list(ds1 = gct), database_dir = db)
  expect_true(v$ok)
  expect_length(v$errors, 0L)
})

test_that("validate uses per-ome species FASTA (one ds ok, another no-fasta)", {
  db <- .mk_database_dir()
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 2)
  g1 <- .mk_gct(syn); g2 <- .mk_gct(syn)
  snap <- list(
    datasets        = c("ds1", "ds2"),
    species         = list(ds1 = "9606", ds2 = "fishless"),  # ds2 has no fasta
    condition_col   = list(ds1 = "condition", ds2 = "condition"),
    replicate_col   = list(ds1 = "condition", ds2 = "condition"),
    condition_order = list(ds1 = c("AY9944_10uM", "DMSO", "LowN"),
                           ds2 = c("AY9944_10uM", "DMSO", "LowN"))
  )
  v <- pelsa_validate_setup(snap, gcts = list(ds1 = g1, ds2 = g2),
                            database_dir = db)
  expect_false(v$ok)
  expect_true(any(grepl("^No FASTA for fishless", v$errors)))
  # ds1 (9606, has fasta) must NOT trigger a no-fasta error
  expect_false(any(grepl("No FASTA for 9606", v$errors)))
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

test_that("peptide frame synthesizes PEP.StrippedSequence from the rid when absent", {
  # A peptide GCT whose stripped sequence sits in the id column (rid) and has NO
  # PEP.StrippedSequence column. pelsa_dataset_peptide_frame() must backfill it
  # from the rid so downstream position-mapping has a sequence to match.
  syn <- pelsa_make_synthetic(seed = 5, n_extra_peptides = 2)
  peptides <- syn$peptides
  peptides$PEP.StrippedSequence <- NULL          # drop it (PELSA-style id-as-seq)
  sc   <- syn$sample_cols
  rids <- peptides$PG.ProteinAccessions          # any per-row identifier; here accessions
  rids <- make.unique(as.character(rids))        # rid must be unique
  mat  <- as.matrix(peptides[, sc]); rownames(mat) <- rids
  rdesc <- peptides[, setdiff(colnames(peptides), sc), drop = FALSE]
  rownames(rdesc) <- rids
  cdesc <- data.frame(condition = sub("_R[0-9]+$", "", sc),
                      row.names = sc, stringsAsFactors = FALSE)
  gct <- cmapR::GCT(mat = mat, rdesc = rdesc, cdesc = cdesc)

  pf <- pelsa_dataset_peptide_frame(gct)
  expect_true("PEP.StrippedSequence" %in% colnames(pf))
  expect_identical(pf$PEP.StrippedSequence, rids)  # copied from the rid, row-aligned
})

test_that("peptide frame keeps a real PEP.StrippedSequence over the rid", {
  # When PEP.StrippedSequence already exists it is authoritative - the rid must
  # NOT overwrite it.
  syn <- pelsa_make_synthetic(seed = 6, n_extra_peptides = 1)
  gct <- .mk_gct(syn)                              # rids are "pep1","pep2",...
  pf  <- pelsa_dataset_peptide_frame(gct)
  expect_identical(pf$PEP.StrippedSequence, syn$peptides$PEP.StrippedSequence)
  expect_false(any(grepl("^pep[0-9]+$", pf$PEP.StrippedSequence)))
})

# ---- pelsa_delinearize (closed-form: recover raw linear from log) ------------

test_that("pelsa_delinearize inverts log2 (2^x) and log10 (10^x)", {
  m <- matrix(c(3, 0, 1, 2), nrow = 2)            # log2 of c(8,1,2,4)
  expect_equal(pelsa_delinearize(m, "log2"), 2 ^ m)
  expect_equal(pelsa_delinearize(m, "log2")[1, 1], 8)   # 2^3 == 8 (known value)

  m10 <- matrix(c(1, 2, 0, 3), nrow = 2)           # log10 of c(10,100,1,1000)
  expect_equal(pelsa_delinearize(m10, "log10"), 10 ^ m10)
  expect_equal(pelsa_delinearize(m10, "log10")[2, 1], 100)  # 10^2 == 100
})

test_that("pelsa_delinearize passes through 'None'/NA/NULL unchanged (no exp)", {
  m <- matrix(c(100, 200, 50, 400), nrow = 2)      # already LINEAR
  expect_identical(pelsa_delinearize(m, "None"), m)
  expect_identical(pelsa_delinearize(m, NA_character_), m)
  expect_identical(pelsa_delinearize(m, NULL), m)
  expect_identical(pelsa_delinearize(m, character(0)), m)
})

test_that("pelsa_delinearize preserves NA (2^NA == NA) and coerces data.frame", {
  m <- matrix(c(3, NA, 1, 2), nrow = 2)
  out <- pelsa_delinearize(m, "log2")
  expect_true(is.na(out[2, 1]))
  expect_equal(out[1, 1], 8)
  # data.frame intensity block is coerced to a numeric matrix.
  df <- as.data.frame(matrix(c(3, 1), nrow = 1))
  expect_equal(pelsa_delinearize(df, "log2"), 2 ^ as.matrix(df))
})

test_that("pelsa_delinearize rejects an unknown base", {
  expect_error(pelsa_delinearize(matrix(1), "ln"), "unknown log_base")
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
      "coverage", "coverage_by_condition", "n_peptides_by_condition",
      "peptide_metrics", "length_by_condition", "annotation_features",
      "unannotated", "qc")
  )
  # The full-duplicate `annotation` frame is NOT stored (memory win).
  expect_false("annotation" %in% names(one))

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

  # Per-condition length / coverage (Summary toggle): correct columns, finite
  # values, conditions a subset of the cv conditions (synthetic has 3).
  expect_setequal(colnames(one$length_by_condition),
                  c("condition", "peptide_length"))
  expect_setequal(colnames(one$coverage_by_condition),
                  c("condition", "coverage"))
  expect_gt(nrow(one$length_by_condition), 0L)
  expect_gt(nrow(one$coverage_by_condition), 0L)
  expect_true(all(is.finite(one$coverage_by_condition$coverage)))
  expect_true(all(one$coverage_by_condition$coverage >= 0 &
                    one$coverage_by_condition$coverage <= 1))
  expect_true(all(unique(one$length_by_condition$condition) %in%
                    unique(one$cv$condition)))

  # n_peptides_by_condition: a NAMED integer vector of QUANTIFIED-in->=1-sample
  # peptide counts (the membership coverage/length use). Each condition's count
  # must be <= that condition's total CV rows (CV has one row per peptide x
  # condition for ALL peptides, including non-quantified ones), and > 0 here.
  expect_type(one$n_peptides_by_condition, "integer")
  expect_true(length(one$n_peptides_by_condition) > 0L)
  expect_true(all(one$n_peptides_by_condition > 0L))
  cv_rows_per_cond <- table(as.character(one$cv$condition))
  shared <- intersect(names(one$n_peptides_by_condition), names(cv_rows_per_cond))
  expect_true(length(shared) > 0L)
  expect_true(all(one$n_peptides_by_condition[shared] <=
                    as.integer(cv_rows_per_cond[shared])))

  # Annotation (2I): only the 3 feature columns are stored, row-aligned to
  # matched; the full annotated frame is reconstructable via the accessor.
  expect_setequal(
    colnames(one$annotation_features),
    c("feature_class_primary", "winning_accession", "winning_gene")
  )
  expect_equal(nrow(one$annotation_features), nrow(one$matched))
  ann <- pelsa_annotation_frame(one)
  expect_s3_class(ann, "data.frame")
  expect_equal(nrow(ann), nrow(one$matched))
  expect_true(all(c("feature_class_primary", "winning_accession",
                    "winning_gene") %in% colnames(ann)))
  expect_true(all(colnames(one$matched) %in% colnames(ann)))

  # QC counts (incl. the three-way annotation breakdown).
  expect_true(all(c("n_peptides", "n_matched_rows", "n_unmatched_rows",
                    "n_unannotated_accessions", "n_annotated_with_features",
                    "n_annotated_zero_feature") %in% names(one$qc)))
  expect_equal(one$qc$n_peptides, nrow(syn$peptides))
  # The failed bucket still equals the legacy unannotated length.
  expect_identical(one$qc$n_unannotated_accessions, length(one$unannotated))
})

test_that("run_analysis_one counts zero-feature (sentinel) accessions in QC", {
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 10)
  gct <- .mk_gct(syn)
  # Take the standard feat_df and add a SENTINEL row for one matched accession
  # so it is "resolved with 0 features" rather than absent.
  base <- .mk_feat_df()
  # Pick an accession that actually appears in the matched peptides (SHARED1 is
  # already a real feature; add a sentinel for a DIFFERENT matched accession).
  one0 <- pelsa_run_analysis_one(gct = gct, gct_original = gct,
                                 fasta_map = syn$fasta, feat_df = base,
                                 condition_col = "condition")
  # An accession present in the data but absent from feat_df -> currently failed.
  failed_acc <- one0$unannotated
  skip_if(length(failed_acc) == 0L, "no unannotated accession to convert")
  sentinel <- data.frame(
    accession = failed_acc[[1]], start = NA_integer_, end = NA_integer_,
    feature_class = "none", stringsAsFactors = FALSE)
  feat_df <- rbind(base, sentinel)

  one <- pelsa_run_analysis_one(gct = gct, gct_original = gct,
                                fasta_map = syn$fasta, feat_df = feat_df,
                                condition_col = "condition")
  # That accession moved from failed -> zero-feature.
  expect_gte(one$qc$n_annotated_zero_feature, 1L)
  expect_identical(one$qc$n_unannotated_accessions,
                   one0$qc$n_unannotated_accessions - 1L)
})

# ---- M8/M9: align CV source to the processed peptide set BY id ---------------

# Minimal GCT from explicit rids (rdesc/cdesc carry an `id` col like real GCTs).
.mk_rid_gct <- function(rids, samples = c("s1", "s2", "s3", "s4")) {
  n <- length(rids); k <- length(samples)
  mat <- matrix(as.double(seq_len(n * k)), nrow = n, ncol = k,
                dimnames = list(rids, samples))
  genes <- if (n == 0L) character(0) else paste0("G_", rids)
  rdesc <- data.frame(id = rids, gene = genes, stringsAsFactors = FALSE)
  rownames(rdesc) <- rids
  cdesc <- data.frame(id = samples,
                      condition = rep(c("A", "B"), length.out = k),
                      stringsAsFactors = FALSE)
  rownames(cdesc) <- samples
  cmapR::GCT(mat = mat, rdesc = rdesc, cdesc = cdesc, rid = rids, cid = samples)
}

test_that("pelsa_align_original_to_processed subsets + reorders the original by rid", {
  orig <- .mk_rid_gct(c("pA", "pB", "pC", "pD", "pE"))

  # identical set/order -> unchanged
  same <- pelsa_align_original_to_processed(orig, .mk_rid_gct(c("pA","pB","pC","pD","pE")))
  expect_identical(same@rid, c("pA","pB","pC","pD","pE"))

  # rows dropped during processing -> only survivors remain, values BY id
  drop <- pelsa_align_original_to_processed(orig, .mk_rid_gct(c("pA","pC","pE")))
  expect_identical(drop@rid, c("pA","pC","pE"))
  expect_identical(unname(drop@mat["pC", ]), unname(orig@mat["pC", ]))

  # reordered processing -> original follows the processed order, values BY id
  reord <- pelsa_align_original_to_processed(orig, .mk_rid_gct(c("pD","pA","pE","pB","pC")))
  expect_identical(reord@rid, c("pD","pA","pE","pB","pC"))
  expect_identical(unname(reord@mat[1, ]), unname(orig@mat["pD", ]))

  # zero processed peptides -> empty CV source, no error
  empty <- pelsa_align_original_to_processed(orig, .mk_rid_gct(character(0)))
  expect_equal(nrow(empty@mat), 0L)

  # data.frame seam (test fixtures may pass plain frames)
  o <- data.frame(s1 = 1:4, s2 = 5:8, row.names = c("pA","pB","pC","pD"))
  p <- data.frame(s1 = c(0,0), s2 = c(0,0), row.names = c("pC","pA"))
  out <- pelsa_align_original_to_processed(o, p)
  expect_identical(rownames(out), c("pC","pA"))
  expect_equal(out["pC", "s1"], 3)
})

test_that("pelsa_align_original_to_processed stops on duplicate original ids", {
  dup <- .mk_rid_gct(c("pA","pX","pB"))
  methods::slot(dup, "rid") <- c("pA","pA","pB")   # forced invalid id namespace
  expect_error(
    pelsa_align_original_to_processed(dup, .mk_rid_gct(c("pA","pB"))),
    "duplicate ids"
  )
})

test_that("run_analysis_one computes CV over the PROCESSED peptides, not the original superset", {
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 6)
  gct <- .mk_gct(syn)                              # processed: pep1..pepP
  proc_rids <- gct@rid
  sc <- syn$sample_cols

  # Original = processed PLUS 5 extra peptide rows the filters would have dropped,
  # and shuffled, so positional alignment to `gct` would be wrong.
  extra_rids <- paste0("extra", seq_len(5))
  extra_mat  <- matrix(stats::runif(length(extra_rids) * length(sc), 10, 20),
                       nrow = length(extra_rids),
                       dimnames = list(extra_rids, sc))
  orig_mat   <- rbind(gct@mat, extra_mat)
  shuffle    <- sample(nrow(orig_mat))
  orig_mat   <- orig_mat[shuffle, , drop = FALSE]
  orig_rdesc <- data.frame(id = rownames(orig_mat), stringsAsFactors = FALSE)
  rownames(orig_rdesc) <- rownames(orig_mat)
  gct_original <- cmapR::GCT(mat = orig_mat, rdesc = orig_rdesc,
                             cdesc = gct@cdesc, rid = rownames(orig_mat), cid = sc)

  one <- pelsa_run_analysis_one(
    gct = gct, gct_original = gct_original,
    fasta_map = syn$fasta, feat_df = .mk_feat_df(),
    condition_col = "condition"
  )

  # CV is long (one row per peptide x condition). It must describe exactly the
  # PROCESSED peptide count (3 conditions), NOT the original superset.
  expect_equal(nrow(one$cv), length(proc_rids) * 3L)
  expect_lt(nrow(one$cv), nrow(orig_mat) * 3L)     # superset would be larger
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

# ---- CV DELINEARIZE wiring (the regression test that proves the bug fixed) ---
# GCTs_original is LOG-transformed; the CV must run on raw LINEAR intensities, so
# the pipeline must delinearize (2^x for log2) BEFORE sum-norm + CV. We feed a
# known log2 matrix through the analysis path and assert the CV equals the
# closed-form CV of 2^m (NOT of the log-space m).

# Build a GCT around an arbitrary (already-built) intensity matrix + two-row
# peptide annotation, with a simple 2-condition cdesc (3 reps each).
.mk_gct_from_mat <- function(mat) {
  rids <- paste0("pep", seq_len(nrow(mat)))
  rownames(mat) <- rids
  rdesc <- data.frame(
    PG.ProteinAccessions = rep("SHARED1", nrow(mat)),
    PG.Genes             = rep("SHAREDGENE", nrow(mat)),
    PEP.StrippedSequence = rep("SHAREDPEPTIDEK", nrow(mat)),
    PEP.PeptidePosition  = rep("5", nrow(mat)),
    row.names = rids, stringsAsFactors = FALSE, check.names = FALSE
  )
  sc <- colnames(mat)
  cdesc <- data.frame(
    condition = sub("_R[0-9]+$", "", sc),
    row.names = sc, stringsAsFactors = FALSE
  )
  cmapR::GCT(mat = mat, rdesc = rdesc, cdesc = cdesc)
}

test_that("CV is computed on DELINEARIZED (linear) intensities for a log2 dataset", {
  sc <- c("A_R1", "A_R2", "A_R3", "B_R1", "B_R2", "B_R3")
  # A LOG2-space matrix (these are log2 intensities, as stored in GCTs_original).
  log_mat <- matrix(
    c(3, 4, 5,   2, 2, 3,
      6, 6, 7,   4, 5, 5),
    nrow = 2, byrow = TRUE, dimnames = list(NULL, sc)
  )
  cmap <- stats::setNames(sub("_R[0-9]+$", "", sc), sc)

  gct <- .mk_gct_from_mat(log_mat)
  feat_df <- .mk_feat_df()

  one <- pelsa_run_analysis_one(
    gct = gct, gct_original = gct,
    fasta_map = list(SHARED1 = "MKLVSHAREDPEPTIDEK"),
    feat_df = feat_df, condition_col = "condition",
    min_nonNA = 3L, log_base = "log2"
  )

  # Closed-form expectations: CV on the LINEAR (2^m) matrix, and on the (wrong)
  # log-space matrix. min_nonNA = 3 so all rows are "ok" (3 reps per condition).
  expected_linear <- pelsa_within_condition_cv(2 ^ log_mat, cmap, min_nonNA = 3L)
  wrong_log       <- pelsa_within_condition_cv(log_mat,     cmap, min_nonNA = 3L)

  # The pipeline CV must MATCH the linear-world CV ...
  got <- one$cv[order(one$cv$row_id, one$cv$condition),
                c("row_id", "condition", "cv_pct")]
  exp <- expected_linear[order(expected_linear$row_id, expected_linear$condition),
                         c("row_id", "condition", "cv_pct")]
  rownames(got) <- NULL; rownames(exp) <- NULL
  expect_equal(got, exp)

  # ... and be DEMONSTRABLY DIFFERENT from the (buggy) log-world CV.
  wrong <- wrong_log[order(wrong_log$row_id, wrong_log$condition), "cv_pct"]
  expect_false(isTRUE(all.equal(got$cv_pct, wrong)))
})

test_that("CV is computed AS-IS for a 'None' (already-linear) dataset (no exp)", {
  sc <- c("A_R1", "A_R2", "A_R3", "B_R1", "B_R2", "B_R3")
  # An already-LINEAR matrix (raw intensities, log_transformation == "None").
  lin_mat <- matrix(
    c(100, 200, 300,   10, 20, 30,
      400, 500, 600,   40, 50, 60),
    nrow = 2, byrow = TRUE, dimnames = list(NULL, sc)
  )
  cmap <- stats::setNames(sub("_R[0-9]+$", "", sc), sc)

  gct <- .mk_gct_from_mat(lin_mat)
  one <- pelsa_run_analysis_one(
    gct = gct, gct_original = gct,
    fasta_map = list(SHARED1 = "MKLVSHAREDPEPTIDEK"),
    feat_df = .mk_feat_df(), condition_col = "condition",
    min_nonNA = 3L, log_base = "None"
  )

  # No exponentiation: CV equals the closed-form CV on the raw matrix as-is.
  expected <- pelsa_within_condition_cv(lin_mat, cmap, min_nonNA = 3L)
  got <- one$cv[order(one$cv$row_id, one$cv$condition),
                c("row_id", "condition", "cv_pct")]
  exp <- expected[order(expected$row_id, expected$condition),
                  c("row_id", "condition", "cv_pct")]
  rownames(got) <- NULL; rownames(exp) <- NULL
  expect_equal(got, exp)
})

test_that("run_analysis threads per-ds log_base into the CV delinearize", {
  sc <- c("A_R1", "A_R2", "A_R3", "B_R1", "B_R2", "B_R3")
  log_mat <- matrix(
    c(3, 4, 5,   2, 2, 3,
      6, 6, 7,   4, 5, 5),
    nrow = 2, byrow = TRUE, dimnames = list(NULL, sc)
  )
  cmap <- stats::setNames(sub("_R[0-9]+$", "", sc), sc)
  gct <- .mk_gct_from_mat(log_mat)

  snap <- list(datasets = "ds1", species = "9606",
               condition_col = list(ds1 = "condition"))
  res <- pelsa_run_analysis(
    gcts = list(ds1 = gct), gcts_original = list(ds1 = gct),
    setup_snapshot = snap, fasta_map = list(SHARED1 = "MKLVSHAREDPEPTIDEK"),
    feat_df = .mk_feat_df(),
    log_base_by_ds = list(ds1 = "log2")
  )
  expected_linear <- pelsa_within_condition_cv(2 ^ log_mat, cmap, min_nonNA = 3L)
  got <- res$ds1$cv[order(res$ds1$cv$row_id, res$ds1$cv$condition), "cv_pct"]
  exp <- expected_linear[order(expected_linear$row_id, expected_linear$condition),
                         "cv_pct"]
  expect_equal(got, exp)
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
    datasets = c("dsA", "dsB"), species = "9606",
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

test_that("run_analysis surfaces a checked dataset absent from gcts as a failure entry", {
  # A requested dataset missing from `gcts` must NOT be silently dropped (the
  # caller advertises snapshot$datasets to the switcher, so a dropped one would
  # show in the UI with a NULL cache and no explanation). It is surfaced as a
  # structured failure entry, like a compute failure.
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 4)
  g <- .mk_gct(syn)
  snap <- list(datasets = c("dsA", "ghost"), species = "9606",
               condition_col = list(dsA = "condition"))
  res <- pelsa_run_analysis(
    gcts = list(dsA = g), gcts_original = list(dsA = g),
    setup_snapshot = snap, fasta_map = syn$fasta, feat_df = .mk_feat_df()
  )
  # both requested datasets are present in the result, in request order
  expect_identical(names(res), c("dsA", "ghost"))
  expect_false(pelsa_analysis_failed(res$dsA))   # dsA analyzed normally
  expect_true(pelsa_analysis_failed(res$ghost))  # ghost -> structured failure
  expect_match(res$ghost$error, "ghost", fixed = TRUE)
  expect_true("stage" %in% names(res$ghost))
})

test_that("run_analysis resolves FASTA + feat PER DATASET by species", {
  # Two datasets of DIFFERENT species. dsA's species FASTA contains its peptides;
  # dsB's species FASTA is EMPTY. If the loop used ONE shared map for both, dsB
  # would (wrongly) match against dsA's map. Per-species keying => dsB matches 0.
  syn1 <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 8)
  syn2 <- pelsa_make_synthetic(seed = 2, n_extra_peptides = 8)
  g1 <- .mk_gct(syn1); g2 <- .mk_gct(syn2)

  snap <- list(
    datasets      = c("dsA", "dsB"),
    species       = list(dsA = "sp1", dsB = "sp2"),
    condition_col = list(dsA = "condition", dsB = "condition")
  )

  seen_species <- character(0)
  resolve_fasta <- function(species) {
    seen_species[[length(seen_species) + 1L]] <<- species
    if (identical(species, "sp1")) syn1$fasta else list()  # sp2 -> empty map
  }
  resolve_feat <- function(species) .mk_feat_df()

  res <- pelsa_run_analysis(
    gcts = list(dsA = g1, dsB = g2),
    gcts_original = list(dsA = g1, dsB = g2),
    setup_snapshot = snap,
    resolve_fasta = resolve_fasta,
    resolve_feat  = resolve_feat
  )

  expect_false(pelsa_analysis_failed(res$dsA))
  expect_false(pelsa_analysis_failed(res$dsB))
  expect_gt(res$dsA$qc$n_matched_rows, 0L)     # matched against its own fasta
  expect_equal(res$dsB$qc$n_matched_rows, 0L)   # empty fasta -> nothing matches
  # both species were resolved (per dataset), not just one shared read
  expect_setequal(unique(seen_species), c("sp1", "sp2"))
})

test_that("run_analysis memoizes the per-species resolver (one read per species)", {
  # Two datasets of the SAME species must resolve that species' FASTA ONCE.
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 6)
  g1 <- .mk_gct(syn); g2 <- .mk_gct(syn)
  snap <- list(
    datasets      = c("dsA", "dsB"),
    species       = list(dsA = "sp1", dsB = "sp1"),
    condition_col = list(dsA = "condition", dsB = "condition")
  )
  calls <- 0L
  resolve_fasta <- function(species) { calls <<- calls + 1L; syn$fasta }
  resolve_feat  <- function(species) .mk_feat_df()

  res <- pelsa_run_analysis(
    gcts = list(dsA = g1, dsB = g2),
    gcts_original = list(dsA = g1, dsB = g2),
    setup_snapshot = snap,
    resolve_fasta = resolve_fasta, resolve_feat = resolve_feat
  )
  expect_false(pelsa_analysis_failed(res$dsA))
  expect_false(pelsa_analysis_failed(res$dsB))
  expect_equal(calls, 1L)   # same species -> resolved once (memoized)
})

test_that("run_analysis captures a per-dataset error (with stage) without aborting others", {
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 4)
  g <- .mk_gct(syn)
  snap <- list(datasets = c("good", "bad"), species = "9606",
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
  snap <- list(datasets = "d", species = "9606",
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
  snap <- list(datasets = character(0), species = "9606",
               condition_col = list())
  expect_error(
    pelsa_run_analysis(gcts = list(), gcts_original = list(),
                       setup_snapshot = snap, fasta_map = syn$fasta,
                       feat_df = .mk_feat_df()),
    "no checked datasets"
  )
})

# ---- observer: invalid setup -> errors shown, NO compute, seam not driven ----
# (The VALID compute path is exercised directly via pelsa_run_analysis above  - 
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
