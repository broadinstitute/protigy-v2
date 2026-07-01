#!/usr/bin/env Rscript
################################################################################
# TDD acceptance test: mapping the PELSA dataset to the feature-annotation file
# must report ZERO unaccounted accessions once the annotation is self-describing.
#
# RED   (current app logic, 6-col annotation.tsv)                 -> 198 unaccounted
# GREEN (revised reader + disposition-enhanced annotation.tsv)    -> 0   unaccounted
#
# Sources the app's REAL helpers so it exercises production code. Uses the REAL
# dataset + REAL annotation.tsv, and SYNTHESIZES the disposition-enhanced
# annotation from the existing workflow side files (input_to_primary.csv,
# deleted.txt, demerged.txt) -- proving the end state without re-running fetch.
################################################################################

suppressWarnings(suppressMessages(library(readr)))

APP <- "/Users/cameronlian/git/protigy-v2"
DIR <- "/Users/cameronlian/Library/CloudStorage/Dropbox-Glian.cameron/Cameron Lian/ProTIGY_PELSA_database/9606_human_terraWorkflow_featureAnn"
TSV <- "/Users/cameronlian/Library/CloudStorage/Dropbox-Glian.cameron/Cameron Lian/ProTIGY_PELSA_test_data/human/20260508_121354_20240426_PELSASPARC_Hung_scriz_AY9944_U18666A_combined_imputation_Report_peptide.tsv"
SCRATCH <- "/private/tmp/claude-501/-Users-cameronlian-git-uniprot-feature-annotation/f97c5887-b2bd-4525-9fa8-35b76cb24785/scratchpad/tdd"

# The app's real helpers (production code under test).
source(file.path(APP, "R", "tab_pelsa_annotation_helpers.R"))
source(file.path(APP, "R", "tab_pelsa_annotation_io.R"))

pass <- TRUE
say  <- function(...) cat(sprintf(...), "\n")

# ---- Dataset accession tokens ------------------------------------------------
ds  <- readr::read_tsv(TSV, show_col_types = FALSE, progress = FALSE,
                       col_select = "PG.ProteinAccessions")
raw <- as.character(ds[["PG.ProteinAccessions"]])
say("Dataset unique accession tokens: %d",
    length(unique(trimws(unlist(strsplit(raw, ";", fixed = TRUE))))))

########################  RED: current 6-col annotation  #######################
say("\n===== RED: CURRENT logic on the 6-col annotation.tsv =====")
feat_current <- pelsa_read_annotation_file(file.path(DIR, "annotation.tsv"))
unacc_red <- pelsa_unannotated_accessions(raw, feat_current)
say("unaccounted (RED): %d", length(unacc_red))
if (length(unacc_red) == 198L) {
  say("[RED OK] reproduced the 198-unaccounted problem.")
} else {
  say("[RED MISMATCH] expected 198, got %d.", length(unacc_red)); pass <- FALSE
}

################  Synthesize the disposition-enhanced annotation  ##############
# Build a 10-col annotation.tsv = existing rows (disposition="resolved") +
# merged sentinels (keyed on the secondary INPUT accession, primary set) +
# deleted + demerged sentinels. Mirrors what the fixed workflow will emit.
ann <- readr::read_tsv(file.path(DIR, "annotation.tsv"),
                       show_col_types = FALSE, progress = FALSE,
                       col_types = cols(.default = col_character()))
ann$disposition       <- "resolved"
ann$primary_accession <- ""

read_lines_clean <- function(p) {
  if (!file.exists(p)) return(character(0))
  x <- trimws(gsub("\r", "", readLines(p, warn = FALSE)))
  unique(x[nzchar(x)])
}

# Merged sentinels from input_to_primary.csv (merged == True). CRLF-safe read.
itp <- readr::read_csv(file.path(DIR, "input_to_primary.csv"),
                       show_col_types = FALSE, progress = FALSE)
merged <- itp[as.character(itp$merged) %in% c("True", "TRUE", "true"), , drop = FALSE]
sent <- function(acc, disp, primary = "") {
  n <- length(acc)
  data.frame(accession = acc, feature_type = rep("", n),
             start = rep(NA_character_, n), end = rep(NA_character_, n),
             description = rep("", n), coord_quality = rep("", n),
             disposition = rep(disp, n), primary_accession = primary,
             stringsAsFactors = FALSE)
}
merged_rows   <- sent(as.character(merged$input_accession), "merged",
                      as.character(merged$primary_accession))
deleted_rows  <- sent(read_lines_clean(file.path(DIR, "deleted.txt")),  "deleted")
demerged_rows <- sent(read_lines_clean(file.path(DIR, "demerged.txt")), "demerged")

enhanced <- rbind(
  ann[, c("accession","feature_type","start","end","description",
          "coord_quality","disposition","primary_accession")],
  merged_rows, deleted_rows, demerged_rows)
say("\nSynthesized enhanced annotation rows: %d (added %d merged + %d deleted + %d demerged sentinels)",
    nrow(enhanced), nrow(merged_rows), nrow(deleted_rows), nrow(demerged_rows))

enhanced_path <- file.path(SCRATCH, "annotation_enhanced.tsv")
readr::write_tsv(enhanced, enhanced_path)

######################  GREEN: revised reader + logic  ########################
say("\n===== GREEN: REVISED reader on the disposition-enhanced annotation =====")
feat_green <- pelsa_read_annotation_file(enhanced_path)

# REGRESSION GUARD: readr's type guessing must NOT nullify the sparse
# primary_accession column (blank on all leading resolved rows). Every merged
# row must retain its primary.
mrows <- feat_green[!is.na(feat_green$disposition) &
                      feat_green$disposition == "merged", , drop = FALSE]
n_with_primary <- sum(!is.na(mrows$primary_accession) & nzchar(mrows$primary_accession))
say("merged rows: %d | with primary preserved: %d", nrow(mrows), n_with_primary)
if (nrow(mrows) > 0L && n_with_primary != nrow(mrows)) {
  say("[FAIL] %d merged rows lost their primary_accession (readr guess bug).",
      nrow(mrows) - n_with_primary); pass <- FALSE
}

unacc_green <- pelsa_unannotated_accessions(raw, feat_green)
say("unaccounted (GREEN): %d", length(unacc_green))

counts <- pelsa_annotation_status_counts(raw, feat_green)
say("buckets: with_features=%d zero_feature=%d merged=%d demerged=%d deleted=%d failed=%d",
    counts$n_with_features, counts$n_zero_feature, counts$n_merged,
    counts$n_demerged, counts$n_deleted, counts$n_failed)

# Invariant: buckets sum to the unique token count.
n_tok <- length(unique(trimws(unlist(strsplit(raw, ";", fixed = TRUE)))))
bsum  <- counts$n_with_features + counts$n_zero_feature + counts$n_merged +
         counts$n_demerged + counts$n_deleted + counts$n_failed
if (bsum != n_tok) { say("[FAIL] buckets sum %d != token count %d", bsum, n_tok); pass <- FALSE }

# THE ACCEPTANCE ASSERTION.
if (length(unacc_green) == 0L) {
  say("\n[GREEN OK] 0 unaccounted -- every dataset accession is accounted.")
} else {
  say("\n[GREEN FAIL] %d still unaccounted:", length(unacc_green))
  print(utils::head(unacc_green, 30)); pass <- FALSE
}

# Sanity: the previously-failing accessions are now bucketed, not failed.
if (counts$n_failed != 0L) { say("[FAIL] n_failed=%d, expected 0", counts$n_failed); pass <- FALSE }

say("\n==================== %s ====================", if (pass) "ALL PASS" else "FAILURES")
quit(status = if (pass) 0L else 1L)
