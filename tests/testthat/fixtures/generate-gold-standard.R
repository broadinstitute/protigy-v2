################################################################################
# Generate gold-standard regression snapshots.
#
# Run ONCE from the repo root on the main branch:
#   Rscript tests/testthat/fixtures/generate-gold-standard.R
#
# Saves three RDS files used by test-regression-pipeline.R:
#   - gold_processed_mat.rds   : numeric matrix after processGCTs()
#   - gold_summary_dataset.rds : data frame from summary_dataset()
#   - gold_stat_results.rds    : data frame from stat.testing()
#
# Re-run and commit when main intentionally changes computation logic and you
# want to accept the new results as the new baseline.
################################################################################

devtools::load_all(".", quiet = TRUE)
library(testthat)

outdir <- file.path("tests", "testthat", "fixtures")

# ---------------------------------------------------------------------------
# Shared setup
# ---------------------------------------------------------------------------

data(brca_retrospective_v5.0_proteome_gct)
gct_raw <- brca_retrospective_v5.0_proteome_gct

GCTs <- list(Proteome = gct_raw)

params <- list(
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

# Helper: run code with Shiny session functions mocked out.
# processGCTs / stat.testing use withProgress, incProgress, setProgress,
# showNotification — all require a live Shiny session.
mock_shiny <- function(code) {
  testthat::with_mocked_bindings(
    withProgress    = function(expr, ...) { force(expr) },
    incProgress     = function(...) invisible(NULL),
    setProgress     = function(...) invisible(NULL),
    showNotification = function(...) invisible(NULL),
    code,
    .package = "Protigy"
  )
}

# ---------------------------------------------------------------------------
# 1. processGCTs → gold_processed_mat.rds
# ---------------------------------------------------------------------------

message("Running processGCTs...")
processed <- mock_shiny(processGCTs(GCTs, params))

gold_mat <- processed$GCTs$Proteome@mat
saveRDS(gold_mat, file.path(outdir, "gold_processed_mat.rds"))
message(sprintf("Saved gold_processed_mat.rds  (%d x %d)", nrow(gold_mat), ncol(gold_mat)))

# ---------------------------------------------------------------------------
# 2. summary_dataset → gold_summary_dataset.rds
# ---------------------------------------------------------------------------

message("Running summary_dataset...")
gold_summary <- summary_dataset(
  params        = processed$parameters$Proteome,
  gct_original  = gct_raw,
  gct_processed = processed$GCTs$Proteome
)
saveRDS(gold_summary, file.path(outdir, "gold_summary_dataset.rds"))
message(sprintf("Saved gold_summary_dataset.rds  (%d rows)", nrow(gold_summary)))

# ---------------------------------------------------------------------------
# 3. stat.testing → gold_stat_results.rds  (Basal vs LumA, T-test)
# ---------------------------------------------------------------------------

message("Running stat.testing...")
all_groups <- sort(unique(processed$GCTs$Proteome@cdesc[["PAM50"]]))
gold_stat <- mock_shiny(stat.testing(
  test               = "Two-sample Moderated T-test",
  annotation_col     = "PAM50",
  chosen_omes        = "Proteome",
  gct                = processed$GCTs,
  chosen_groups      = all_groups,
  selected_contrasts = list(c("Basal", "LumA")),
  p.value.alpha      = 0.05,
  use.adj.pvalue     = TRUE,
  apply.log          = FALSE,
  intensity          = FALSE
))
saveRDS(gold_stat, file.path(outdir, "gold_stat_results.rds"))
message(sprintf(
  "Saved gold_stat_results.rds  (%d features, %d significant)",
  nrow(gold_stat$Proteome),
  sum(gold_stat$Proteome$significant.Basal_over_LumA, na.rm = TRUE)
))

message("\nDone. Commit these .rds files on the main branch to lock in the gold standard.")
