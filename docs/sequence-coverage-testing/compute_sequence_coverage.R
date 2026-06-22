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

# ---- Block 4: explode multi-accession rows ----------------------------------
# Mirrors R/tab_pelsa_explode_helpers.R::pelsa_explode_accessions:
#   * split PG.ProteinAccessions on ";", trim, strip a leading ">",
#     drop empty/NA tokens;
#   * one exploded row per kept accession, carrying a stable .row_id (1-based
#     index into the original peptide frame) plus all original columns.
# Gene tokens are aligned to accession order best-effort (coverage keys only on
# accession, so gene is informational here).
ACC_COL  <- "PG.ProteinAccessions"
GENE_COL <- "PG.Genes"
stopifnot(ACC_COL %in% colnames(peptides))

peptides$.row_id <- seq_len(nrow(peptides))

acc_split <- strsplit(as.character(peptides[[ACC_COL]]), ";", fixed = TRUE)
n_acc_raw <- lengths(acc_split)
flat_acc  <- trimws(unlist(acc_split, use.names = FALSE))
flat_acc  <- sub("^>", "", flat_acc)               # strip stray FASTA '>'
flat_row  <- rep.int(seq_len(nrow(peptides)), n_acc_raw)

keep      <- !is.na(flat_acc) & nzchar(flat_acc)
accession <- flat_acc[keep]
row_idx   <- flat_row[keep]

exploded <- peptides[row_idx, , drop = FALSE]
rownames(exploded) <- NULL
exploded$accession <- accession

# Best-effort gene alignment by the same kept slots.
if (GENE_COL %in% colnames(peptides)) {
  gene_split <- strsplit(as.character(peptides[[GENE_COL]]), ";", fixed = TRUE)
  flat_gene  <- unlist(
    mapply(function(g, n) {
      if (length(g) == 0L) rep(NA_character_, n)
      else if (length(g) == n) g
      else rep(g[[1]], n)          # single shared gene recycles across accessions
    }, gene_split, n_acc_raw, SIMPLIFY = FALSE),
    use.names = FALSE
  )
  exploded$gene <- trimws(flat_gene[keep])
} else {
  exploded$gene <- NA_character_
}

log_line("[explode] exploded (peptide x accession) rows: ", nrow(exploded))

# ---- Block 5: map peptides to FASTA substring positions ---------------------
# Mirrors R/tab_pelsa_fasta_helpers.R::pelsa_map_peptide_positions:
#   1. valid sequence = ^[A-Z]+$ (NA/malformed -> reason "bad_sequence_format");
#   2. resolve FASTA seq with isoform-base fallback (P12345-2 -> P12345);
#      no key -> reason "accession_absent";
#   3. exact substring match, overlap = TRUE, one matched row per occurrence;
#      verbatim sequences (NO I->L isobaric retry); a miss -> "sequence_not_found".
SEQ_COL <- "PEP.StrippedSequence"
stopifnot(SEQ_COL %in% colnames(exploded))

# Flatten the (possibly ragged) map to a length-1-per-entry named character
# vector, then resolve exact key first and isoform base second.
fasta_vec <- vapply(
  fasta_map,
  function(s) if (length(s) >= 1L) as.character(s)[[1L]] else NA_character_,
  character(1)
)
resolve_fasta_seq <- function(acc) {
  seq_exact <- unname(fasta_vec[acc])
  base_acc  <- sub("-[0-9]+$", "", acc)
  need      <- is.na(seq_exact) & base_acc != acc
  if (any(need)) seq_exact[need] <- unname(fasta_vec[base_acc[need]])
  seq_exact
}

seqs <- as.character(exploded[[SEQ_COL]])
accs <- as.character(exploded$accession)
n    <- nrow(exploded)

is_valid_seq <- !is.na(seqs) & grepl("^[A-Z]+$", seqs)
fasta_seq    <- resolve_fasta_seq(accs)
has_fasta    <- !is.na(fasta_seq)

reason <- rep(NA_character_, n)
reason[!is_valid_seq]               <- "bad_sequence_format"
reason[is_valid_seq & !has_fasta]   <- "accession_absent"
candidate <- is_valid_seq & has_fasta

# Vectorized overlapping substring locate over candidate rows only.
starts_list <- vector("list", n)
if (any(candidate)) {
  locs <- stringi::stri_locate_all_fixed(
    fasta_seq[candidate], seqs[candidate],
    opts_fixed = stringi::stri_opts_fixed(overlap = TRUE)
  )
  starts_list[candidate] <- lapply(locs, function(m) {
    s <- m[, "start"]; as.integer(s[!is.na(s)])
  })
}
n_hits <- vapply(starts_list, length, integer(1))
reason[candidate & n_hits == 0L] <- "sequence_not_found"

# Build matched rows: one per occurrence.
matched_mask <- candidate & n_hits > 0L
if (any(matched_mask)) {
  ridx      <- rep.int(which(matched_mask), n_hits[matched_mask])
  pep_start <- unlist(starts_list[matched_mask], use.names = FALSE)
  pep_len   <- nchar(seqs[ridx])
  matched   <- exploded[ridx, , drop = FALSE]
  rownames(matched) <- NULL
  matched$pep_start <- as.integer(pep_start)
  matched$pep_end   <- as.integer(pep_start + pep_len - 1L)
} else {
  matched <- exploded[0L, , drop = FALSE]
  matched$pep_start <- integer(0)
  matched$pep_end   <- integer(0)
}

unmatched_mask <- !is.na(reason)
unmatched <- data.frame(
  peptide_sequence = seqs[unmatched_mask],
  accession        = accs[unmatched_mask],
  reason           = reason[unmatched_mask],
  stringsAsFactors = FALSE
)

log_line("[map] matched (peptide x accession x occurrence) rows: ", nrow(matched))
log_line("[map] unmatched (peptide x accession) pairs: ", nrow(unmatched))

# ---- Block 6: per-protein interval-union coverage ---------------------------
# Mirrors R/tab_pelsa_coverage_helpers.R::.pelsa_union_length + pelsa_sequence_coverage.
# Union of 1-based inclusive spans, counted ONCE (overlaps not double-counted).

# Union length of inclusive intervals sorted by (start, end). Sweep-line: a span
# opens a new merged block when its start exceeds the running max-end of prior
# spans; block length summed gives covered residues. Touching spans merge.
union_length <- function(start, end) {
  prior_max_end <- cummax(c(-Inf, head(end, -1L)))
  block_id   <- cumsum(start > prior_max_end)
  block_start <- tapply(start, block_id, min)
  block_end   <- tapply(end,   block_id, max)
  as.integer(sum(block_end - block_start + 1L))
}

# Resolve each accession's FASTA length with isoform-base fallback (NA if absent).
resolve_fasta_length <- function(acc) {
  seq_exact <- unname(fasta_vec[acc])
  base_acc  <- sub("-[0-9]+$", "", acc)
  need      <- is.na(seq_exact) & base_acc != acc
  if (any(need)) seq_exact[need] <- unname(fasta_vec[base_acc[need]])
  as.integer(nchar(seq_exact))  # nchar(NA) -> NA
}

sequence_coverage <- function(matched_df) {
  if (nrow(matched_df) == 0L) {
    return(data.frame(accession = character(0), covered_residues = integer(0),
                      protein_length = integer(0), coverage = numeric(0),
                      over_length_flag = logical(0), stringsAsFactors = FALSE))
  }
  acc   <- as.character(matched_df$accession)
  start <- as.integer(matched_df$pep_start)
  end   <- as.integer(matched_df$pep_end)

  # Order by (accession, start, end), then union per accession.
  o <- order(acc, start, end)
  acc <- acc[o]; start <- start[o]; end <- end[o]
  covered_residues <- tapply(seq_along(acc), acc,
                             function(i) union_length(start[i], end[i]))
  acc_vec <- names(covered_residues)
  covered_residues <- as.integer(covered_residues)

  protein_length <- resolve_fasta_length(acc_vec)
  resolved <- !is.na(protein_length)

  # Clamp an over-length union to protein length and flag it (soft-fail posture).
  over <- resolved & protein_length > 0L & covered_residues > protein_length
  if (any(over)) {
    log_line("[coverage][warn] union exceeds protein length for ", sum(over),
             " accession(s); clamping (e.g. ",
             paste(head(acc_vec[over], 5L), collapse = ", "), ")")
    covered_residues[over] <- protein_length[over]
  }

  coverage <- rep(NA_real_, length(acc_vec))
  ok <- resolved & protein_length > 0L
  coverage[ok] <- covered_residues[ok] / protein_length[ok]

  data.frame(accession = acc_vec, covered_residues = covered_residues,
             protein_length = protein_length, coverage = coverage,
             over_length_flag = over, stringsAsFactors = FALSE)
}

# ---- Block 7: experiment-level coverage (ALL matched peptides) ---------------
# This is the app's experiment-wide metric: pelsa_sequence_coverage(matched, fasta)
# over every matched peptide (R/tab_pelsa_analysis_helpers.R line ~879).
cov_experiment <- sequence_coverage(matched)
cov_experiment <- cov_experiment[order(cov_experiment$accession), , drop = FALSE]

readr::write_csv(cov_experiment,
                 file.path(OUT_DIR, "coverage_experiment_level.csv"))
log_line("[out] coverage_experiment_level.csv: ", nrow(cov_experiment),
         " accessions; ",
         sum(is.finite(cov_experiment$coverage)), " with finite coverage; ",
         "mean coverage = ",
         round(mean(cov_experiment$coverage[is.finite(cov_experiment$coverage)]), 4))

# Unmatched reason breakdown (informational).
if (nrow(unmatched) > 0L) {
  unmatched_summary <- as.data.frame(table(reason = unmatched$reason),
                                     stringsAsFactors = FALSE)
  names(unmatched_summary) <- c("reason", "n_pairs")
} else {
  unmatched_summary <- data.frame(reason = character(0), n_pairs = integer(0),
                                  stringsAsFactors = FALSE)
}
readr::write_csv(unmatched_summary,
                 file.path(OUT_DIR, "unmatched_peptides_summary.csv"))
log_line("[out] unmatched_peptides_summary.csv written")
