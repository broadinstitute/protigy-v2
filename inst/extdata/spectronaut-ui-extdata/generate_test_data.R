################################################################################
# generate_test_data.R
#
# Purpose:
#   Regenerate the two synthetic Spectronaut fixture TSVs used by the
#   shinytest2 integration tests for the Spectronaut UI feature flag.
#   Real sample names, project codes, and intensity values from the original
#   Spectronaut export have been replaced with purely synthetic data.
#
# Usage (run from this directory, or from the repo root):
#   source("inst/extdata/spectronaut-ui-extdata/generate_test_data.R")
#
# Output files (overwritten in place):
#   - spectronaut_test_condition_setup.tsv
#   - spectronaut_test_pivot.tsv
#
# Notes:
#   - set.seed(42) ensures byte-identical output on every run.
#   - Fixture filenames are kept verbatim so shinytest2 test references do not need
#     to change (see tests/testthat/test-spectronaut-flag-shinytest2.R).
#   - The pivot report retains only the 5 required metadata columns plus the 6
#     per-sample PG.Quantity columns (Log2Quantity and IBAQ columns are omitted
#     as they are not needed for the setup workflow under test).
################################################################################

set.seed(42)

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

N_PROTEINS <- 200
N_SAMPLES  <- 6

# Synthetic run labels -- no real project codes, dates, or instrument prefixes.
sample_labels <- sprintf("SYN_sample_%02d", seq_len(N_SAMPLES))

# Conditions: 3 groups x 2 replicates each, assigned round-robin.
condition_names <- c("cond_A", "cond_A", "cond_B", "cond_B", "ctrl", "ctrl")
replicate_nums  <- c(1L, 2L, 1L, 2L, 1L, 2L)
sample_colors   <- c("#E41A1C", "#E41A1C", "#377EB8", "#377EB8", "#4DAF4A", "#4DAF4A")

# Output paths (relative to this file's directory; script can be sourced from
# the repo root because write paths are constructed to be repo-root-relative
# when chdir = TRUE is not used).
out_dir <- dirname(sys.frame(1)$ofile %||% normalizePath("inst/extdata/spectronaut-ui-extdata"))

# Fallback: if sourced without chdir, write relative to working directory
this_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile)),
  error = function(e) file.path(getwd(), "inst", "extdata", "spectronaut-ui-extdata")
)

condition_setup_file <- file.path(
  this_dir,
  "spectronaut_test_condition_setup.tsv"
)

pivot_file <- file.path(
  this_dir,
  "spectronaut_test_pivot.tsv"
)

# ---------------------------------------------------------------------------
# 1. ConditionSetup TSV
#    Columns (matching real Spectronaut ConditionSetup column order):
#    #  Reference  Run Label  Condition  Fraction  Replicate
#    Quantity Correction Factor  Label  Color  File Name
# ---------------------------------------------------------------------------

condition_setup <- data.frame(
  `#`                              = seq_len(N_SAMPLES),
  Reference                        = rep("False", N_SAMPLES),
  `Run Label`                      = paste0(sample_labels, ".htrms"),
  Condition                        = condition_names,
  Fraction                         = rep("NA", N_SAMPLES),
  Replicate                        = replicate_nums,
  `Quantity Correction Factor`     = rep(1L, N_SAMPLES),
  Label                            = condition_names,
  Color                            = sample_colors,
  `File Name`                      = sample_labels,
  check.names                      = FALSE,
  stringsAsFactors                 = FALSE
)

write.table(
  condition_setup,
  file      = condition_setup_file,
  sep       = "\t",
  quote     = FALSE,
  row.names = FALSE,
  na        = "NaN"
)

message("Written: ", condition_setup_file)

# ---------------------------------------------------------------------------
# 2. Pivot report TSV
#    5 metadata columns + 6 PG.Quantity columns (one per sample).
#    Column name format: [N] <run_label>.htrms.PG.Quantity
# ---------------------------------------------------------------------------

# Metadata
protein_ids <- sprintf("SYN%05d", seq_len(N_PROTEINS))

n_peptides <- sample(2:40, N_PROTEINS, replace = TRUE)

metadata <- data.frame(
  PG.ProteinGroups     = protein_ids,
  PG.ProteinAccessions = protein_ids,
  PG.Organisms         = rep("Synthetic_organism", N_PROTEINS),
  PG.ProteinNames      = protein_ids,
  `PG.NrOfStrippedSequencesIdentified (Experiment-wide)` = n_peptides,
  check.names          = FALSE,
  stringsAsFactors     = FALSE
)

# Quantity columns: log-normal intensities with ~10 % NaN
quantity_cols <- lapply(seq_len(N_SAMPLES), function(i) {
  vals <- 10 ^ rnorm(N_PROTEINS, mean = 3.5, sd = 1)
  missing_idx <- sample(N_PROTEINS, size = round(0.10 * N_PROTEINS))
  vals[missing_idx] <- NA_real_
  vals
})

quantity_df <- as.data.frame(
  setNames(quantity_cols, sprintf("[%d] %s.htrms.PG.Quantity", seq_len(N_SAMPLES), sample_labels)),
  check.names = FALSE
)

pivot_report <- cbind(metadata, quantity_df)

write.table(
  pivot_report,
  file      = pivot_file,
  sep       = "\t",
  quote     = FALSE,
  row.names = FALSE,
  na        = "NaN"
)

message("Written: ", pivot_file)
message("Done. Pivot: ", N_PROTEINS, " proteins x ", N_SAMPLES, " samples.")
