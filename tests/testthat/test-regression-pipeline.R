################################################################################
# Regression tests: end-to-end computation pipeline vs gold standard
#
# These tests guard against silent regressions in the core analysis logic.
# They run processGCTs(), summary_dataset(), and stat.testing() on the
# built-in BRCA proteome dataset and compare numerical outputs against
# gold-standard snapshots saved in tests/testthat/fixtures/.
#
# To regenerate the gold standard after an intentional algorithm change:
#   Rscript tests/testthat/fixtures/generate-gold-standard.R
# then commit the updated .rds files.
################################################################################

# ---------------------------------------------------------------------------
# Shared fixtures (loaded once per file, not per test)
# ---------------------------------------------------------------------------

data(brca_retrospective_v5.0_proteome_gct)
gct_raw <- brca_retrospective_v5.0_proteome_gct

std_params <- list(
  Proteome = list(
    gct_file_name              = "brca_proteome.gct",
    annotation_column          = "PAM50",
    gene_symbol_column         = "GeneSymbol",
    intensity_data             = "No",
    log_transformation         = "None",
    data_normalization         = "Median",
    group_normalization        = FALSE,
    group_normalization_column = NULL,
    data_filter                = "None",
    data_filter_sd_pct         = 80,
    max_missing                = 50,
    sample_filter_enabled      = FALSE,
    sample_filter_column       = "",
    sample_filter_values       = character(0),
    row_filter_enabled         = FALSE,
    row_filter_column          = "",
    row_filter_values          = character(0),
    convert_ids_to_gene_symbol = FALSE,
    id_source_column           = "",
    id_mapping_species         = "Homo sapiens"
  )
)

# Helper: execute code with Shiny session functions mocked out.
# processGCTs / stat.testing use withProgress, incProgress, setProgress, and
# showNotification  -  these all require a live Shiny session.
# suppressWarnings() prevents known benign validateGCT warnings (e.g. "Gene
# symbol column already exists") from failing the CI check (error_on="warning").
mock_shiny <- function(code) {
  suppressWarnings(
    with_mocked_bindings(
      withProgress     = function(expr, ...) { force(expr) },
      incProgress      = function(...) invisible(NULL),
      setProgress      = function(...) invisible(NULL),
      showNotification = function(...) invisible(NULL),
      code,
      .package = "Protigy"
    )
  )
}

# Helper: load a gold-standard RDS from the fixtures directory.
load_gold <- function(filename) {
  path <- test_path("fixtures", filename)
  if (!file.exists(path)) {
    skip(paste0(
      "Gold-standard file '", filename, "' not found. ",
      "Run tests/testthat/fixtures/generate-gold-standard.R on the main branch."
    ))
  }
  readRDS(path)
}

# ---------------------------------------------------------------------------
# Test 1: processGCTs output matches gold standard
# ---------------------------------------------------------------------------

test_that("processGCTs produces numerically identical output to gold standard", {
  gold_mat <- load_gold("gold_processed_mat.rds")

  actual <- mock_shiny(
    processGCTs(list(Proteome = gct_raw), std_params)
  )
  actual_mat <- actual$GCTs$Proteome@mat

  # Dimensions must be identical
  expect_equal(
    dim(actual_mat), dim(gold_mat),
    info = "Processed matrix dimensions differ from gold standard"
  )

  # Row and column identity
  expect_equal(
    rownames(actual_mat), rownames(gold_mat),
    info = "Processed matrix row names differ from gold standard"
  )
  expect_equal(
    colnames(actual_mat), colnames(gold_mat),
    info = "Processed matrix column names differ from gold standard"
  )

  # Numerical values (allow floating-point noise up to 1e-10)
  expect_equal(
    actual_mat, gold_mat,
    tolerance = 1e-10,
    info = "Processed numeric matrix values differ from gold standard"
  )
})

# ---------------------------------------------------------------------------
# Test 2: processGCTs produces consistent NA pattern
# ---------------------------------------------------------------------------

test_that("processGCTs NA pattern matches gold standard", {
  gold_mat <- load_gold("gold_processed_mat.rds")

  actual <- mock_shiny(
    processGCTs(list(Proteome = gct_raw), std_params)
  )
  actual_mat <- actual$GCTs$Proteome@mat

  expect_equal(
    is.na(actual_mat), is.na(gold_mat),
    info = "NA pattern after processGCTs differs from gold standard"
  )
})

# ---------------------------------------------------------------------------
# Test 3: summary_dataset output matches gold standard
# ---------------------------------------------------------------------------

test_that("summary_dataset produces identical counts to gold standard", {
  gold_summary <- load_gold("gold_summary_dataset.rds")

  actual_processed <- mock_shiny(
    processGCTs(list(Proteome = gct_raw), std_params)
  )
  actual_summary <- summary_dataset(
    params        = actual_processed$parameters$Proteome,
    gct_original  = gct_raw,
    gct_processed = actual_processed$GCTs$Proteome
  )

  expect_equal(
    actual_summary, gold_summary,
    info = "summary_dataset output differs from gold standard"
  )
})

# ---------------------------------------------------------------------------
# Test 4: stat.testing logFC values match gold standard
# ---------------------------------------------------------------------------

test_that("stat.testing logFC values match gold standard (Basal vs LumA)", {
  gold_stat <- load_gold("gold_stat_results.rds")

  actual_processed <- mock_shiny(
    processGCTs(list(Proteome = gct_raw), std_params)
  )
  all_groups <- sort(unique(actual_processed$GCTs$Proteome@cdesc[["PAM50"]]))

  actual_stat <- mock_shiny(stat.testing(
    test               = "Two-sample Moderated T-test",
    annotation_col     = "PAM50",
    chosen_omes        = "Proteome",
    gct                = actual_processed$GCTs,
    chosen_groups      = all_groups,
    selected_contrasts = list(c("Basal", "LumA")),
    p.value.alpha      = 0.05,
    use.adj.pvalue     = TRUE,
    apply.log          = FALSE,
    intensity          = FALSE
  ))

  expect_equal(
    actual_stat$Proteome$logFC.Basal_over_LumA,
    gold_stat$Proteome$logFC.Basal_over_LumA,
    tolerance = 1e-10,
    info = "logFC values differ from gold standard"
  )
})

# ---------------------------------------------------------------------------
# Test 5: stat.testing p-values match gold standard
# ---------------------------------------------------------------------------

test_that("stat.testing adjusted p-values match gold standard (Basal vs LumA)", {
  gold_stat <- load_gold("gold_stat_results.rds")

  actual_processed <- mock_shiny(
    processGCTs(list(Proteome = gct_raw), std_params)
  )
  all_groups <- sort(unique(actual_processed$GCTs$Proteome@cdesc[["PAM50"]]))

  actual_stat <- mock_shiny(stat.testing(
    test               = "Two-sample Moderated T-test",
    annotation_col     = "PAM50",
    chosen_omes        = "Proteome",
    gct                = actual_processed$GCTs,
    chosen_groups      = all_groups,
    selected_contrasts = list(c("Basal", "LumA")),
    p.value.alpha      = 0.05,
    use.adj.pvalue     = TRUE,
    apply.log          = FALSE,
    intensity          = FALSE
  ))

  expect_equal(
    actual_stat$Proteome$adj.P.Val.Basal_over_LumA,
    gold_stat$Proteome$adj.P.Val.Basal_over_LumA,
    tolerance = 1e-10,
    info = "Adjusted p-values differ from gold standard"
  )
})

# ---------------------------------------------------------------------------
# Test 6: stat.testing significant-feature count matches gold standard
# ---------------------------------------------------------------------------

test_that("stat.testing significant feature count matches gold standard", {
  gold_stat <- load_gold("gold_stat_results.rds")
  gold_n_sig <- sum(gold_stat$Proteome$significant.Basal_over_LumA, na.rm = TRUE)

  actual_processed <- mock_shiny(
    processGCTs(list(Proteome = gct_raw), std_params)
  )
  all_groups <- sort(unique(actual_processed$GCTs$Proteome@cdesc[["PAM50"]]))

  actual_stat <- mock_shiny(stat.testing(
    test               = "Two-sample Moderated T-test",
    annotation_col     = "PAM50",
    chosen_omes        = "Proteome",
    gct                = actual_processed$GCTs,
    chosen_groups      = all_groups,
    selected_contrasts = list(c("Basal", "LumA")),
    p.value.alpha      = 0.05,
    use.adj.pvalue     = TRUE,
    apply.log          = FALSE,
    intensity          = FALSE
  ))
  actual_n_sig <- sum(actual_stat$Proteome$significant.Basal_over_LumA, na.rm = TRUE)

  expect_equal(
    actual_n_sig, gold_n_sig,
    info = sprintf(
      "Significant feature count changed: was %d, now %d",
      gold_n_sig, actual_n_sig
    )
  )
})
