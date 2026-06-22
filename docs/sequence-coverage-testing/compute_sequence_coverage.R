#!/usr/bin/env Rscript
# =============================================================================
# Standalone validation: PELSA per-protein sequence coverage
# (experiment-level + per-condition), reproducing the Protigy Shiny app.
#
# Run from the repo root:
#   Rscript docs/sequence-coverage-testing/compute_sequence_coverage.R
#
# Faithful to (cited inline):
#   R/tab_pelsa_fasta_helpers.R     pelsa_read_fasta / map_peptide_positions
#   R/tab_pelsa_explode_helpers.R   pelsa_explode_accessions
#   R/tab_pelsa_coverage_helpers.R  pelsa_sequence_coverage / .pelsa_union_length
#   R/tab_pelsa_analysis_helpers.R  pelsa_coverage_by_condition / membership
#
# This script depends ONLY on base R + readr + stringi (NOT the Protigy pkg).
# Written sequentially (top to bottom) for easy proofreading.
# =============================================================================

suppressPackageStartupMessages({
  library(readr)    # read_tsv / read_csv -> missing cell is NA, matching the app
  library(stringi)  # stri_locate_all_fixed: vectorized substring match
})

# ---- Paths (relative to repo root) ------------------------------------------
REPO_ROOT <- normalizePath(".", winslash = "/")
OUT_DIR   <- file.path(REPO_ROOT, "docs", "sequence-coverage-testing")
PEPTIDE_TSV <- file.path(
  OUT_DIR,
  "20260508_121354_20240426_PELSASPARC_Hung_scriz_AY9944_U18666A_combined_imputation_Report_peptide.tsv"
)
DESIGN_CSV  <- file.path(OUT_DIR, "experimental_design_template_AY9944.csv")
FASTA_PATH  <- file.path(
  REPO_ROOT, "inst", "database", "9606", "fasta",
  "UniProt.human.20210902.RInrNF.602contams.fasta"
)

stopifnot(file.exists(PEPTIDE_TSV), file.exists(DESIGN_CSV), file.exists(FASTA_PATH))

# ---- Logger: print to console AND append to run_log.txt ---------------------
LOG_PATH <- file.path(OUT_DIR, "run_log.txt")
cat("", file = LOG_PATH)  # truncate any previous run
log_line <- function(...) {
  msg <- paste0(...)
  cat(msg, "\n", sep = "")
  cat(msg, "\n", file = LOG_PATH, sep = "", append = TRUE)
}
log_line("[run] sequence-coverage validation started")

# ---- Block 1: read inputs ---------------------------------------------------
# readr renders a missing intensity cell as NA (NOT ""), matching how the app
# ingests uploads (R/sidebar_setup_helpers_csv-excel-processing.R). read.delim
# would give "" and hide the NA-vs-blank divergence, so we use read_tsv.
peptides <- readr::read_tsv(
  PEPTIDE_TSV,
  col_types = readr::cols(.default = readr::col_character()),
  progress = FALSE
)
peptides <- as.data.frame(peptides, check.names = FALSE, stringsAsFactors = FALSE)
log_line("[read] peptide rows: ", nrow(peptides),
         " | columns: ", ncol(peptides))

design <- readr::read_csv(DESIGN_CSV,
                          col_types = readr::cols(.default = readr::col_character()),
                          progress = FALSE)
design <- as.data.frame(design, check.names = FALSE, stringsAsFactors = FALSE)
# The design template carries trailing rows that list TSV columns with NA
# condition; keep only rows with a real condition (the 9 analysis samples).
design <- design[!is.na(design$condition) & nzchar(design$condition), , drop = FALSE]
log_line("[read] design sample rows: ", nrow(design))

# ---- Block 2: samples + condition map (design CSV drives both) --------------
sample_cols <- intersect(design$columnName, colnames(peptides))
missing_samples <- setdiff(design$columnName, colnames(peptides))
if (length(missing_samples) > 0L) {
  log_line("[warn] design samples not found in TSV (ignored): ",
           paste(missing_samples, collapse = ", "))
}
stopifnot(length(sample_cols) > 0L)

cond_map <- setNames(design$condition, design$columnName)[sample_cols]  # sample -> condition
log_line("[samples] using ", length(sample_cols), " samples across ",
         length(unique(cond_map)), " conditions: ",
         paste(unique(cond_map), collapse = ", "))

# ---- Block 3: read FASTA -> named list accession -> AA string ---------------
# Mirrors R/tab_pelsa_fasta_helpers.R::pelsa_read_fasta(mode = "uniprot"):
#   * key = middle pipe field of the first header token (sp|P12345|NAME -> P12345),
#     else the bare first token;
#   * sequence = concatenated, upper-cased residue lines;
#   * first-wins on duplicate accession keys.
read_fasta_uniprot <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  is_header <- startsWith(lines, ">")
  header_idx <- which(is_header)
  group <- cumsum(is_header)

  headers   <- sub("^>", "", lines[header_idx])
  first_tok <- sub("\\s.*$", "", headers)
  pipe_acc  <- sub("^[^|]*\\|([^|]*)\\|.*$", "\\1", first_tok)
  keys      <- ifelse(grepl("\\|", first_tok), pipe_acc, first_tok)

  seq_lines  <- lines[!is_header]
  seq_groups <- group[!is_header]
  seq_by_group <- tapply(
    seq_lines, factor(seq_groups, levels = seq_along(header_idx)),
    FUN = function(x) toupper(paste0(x, collapse = "")), simplify = TRUE
  )
  seqs <- as.character(seq_by_group)
  seqs[is.na(seqs)] <- ""

  keep <- !duplicated(keys)   # first-wins
  out  <- as.list(seqs[keep])
  names(out) <- keys[keep]
  out
}

fasta_map <- read_fasta_uniprot(FASTA_PATH)
log_line("[fasta] sequences parsed: ", length(fasta_map))
