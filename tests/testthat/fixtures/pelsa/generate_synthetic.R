################################################################################
# Seeded synthetic PELSA peptide-frame generator (Phase 1).
#
# Single, deterministic, edge-case-rich generator that mirrors the real
# Spectronaut peptide-level export. It is the shared input for ALL Phase-2 PELSA
# helper parity tests (explode, FASTA position mapping, missed-cleavage, CV,
# depth, coverage, best-peptide rollup, annotation, marker matching).
#
# Pure base R + stats only. set.seed() makes every frame fully reproducible.
#
# IMPORTANT: this file is *sourced* directly by tests
# (source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))). It must
# not depend on package load_all -- it defines a single function.
#
# ------------------------------------------------------------------------------
# CLOSED-FORM GROUND TRUTH (the comments here ARE the reference for tests)
# ------------------------------------------------------------------------------
# FASTA strings below are built by concatenating known flank residues around the
# seeded peptides at chosen offsets, so every position is exact by construction.
# Positions are 1-based (R / UniProt convention: first residue is position 1).
#
# Seeded edge cases and their ground-truth coordinates:
#
#   $shared_peptide  "SHAREDPEPTIDEK"
#       Maps to 3 accessions "SHARED1;SHARED2;SHARED3" (explode/coverage test).
#       FASTA SHARED1: flank "MKLV" (4) + peptide -> start 5.
#       FASTA SHARED2: flank "MAAAAAAAAA" (10) + peptide -> start 11.
#       FASTA SHARED3: flank "M" (1) + peptide -> start 2.
#
#   $dup_peptide  "DUPLICATEK"  (occurs EXACTLY TWICE in ONE protein "DUPPROT")
#       FASTA DUPPROT = "MGG" + DUP + "WWWWW" + DUP + "QQ"
#       First  occurrence start = 4   (after "MGG", len 3).
#       Second occurrence start = 4 + nchar(DUP) + 5 = 4 + 10 + 5 = 19.
#       $dup_peptide_starts = c(4L, 19L). Non-overlapping; verified by gregexpr.
#
#   $overlap_peptide  "AAA"  (overlapping repeat inside region "AAAA")
#       FASTA OVERLAPPROT = "MCDEF" (5) + "AAAA" + "GH"
#       "AAA" occurs at positions 6 and 7 (overlapping by one residue).
#       $overlap_peptide_starts = c(6L, 7L). gregexpr() (greedy, non-overlapping)
#       finds only the first; the overlap is verified by manual sliding scan.
#       $overlap_peptide_accession = "OVERLAPPROT".
#
#   $absent_peptide  "GHOSTPEPTIDEK"
#       Present in $peptides, annotated to accession "ABSENTPROT", but
#       ABSENTPROT's FASTA = "MQWERTYQWERTYQWERTY" deliberately does NOT
#       contain it -> FASTA mapper must drop it. $absent_peptide_accession =
#       "ABSENTPROT".
#
#   $il_peptide  "PEPTIDEWLTHISO"   (note: contains L and I)
#       Differs from its FASTA region ONLY by I<->L swaps, so it does NOT
#       exact-substring-match. The mapper trusts sequences verbatim and does
#       NOT do I->L (Leu/Ile) isobaric reconciliation, so this peptide is
#       intentionally UNMATCHABLE -> unmatched reason "sequence_not_found".
#       Used to prove no isobaric retry exists. $il_peptide_accession = "ILPROT".
#       FASTA ILPROT = "MK" (2) + "PEPTIDEWITHISO" + "RR"
#         (FASTA spells the region with I where the peptide has L, and vice
#          versa, so the exact forms never match.)
#       $il_peptide_position = 3L is retained only to document where the region
#       WOULD sit; it is NOT an emitted match position.
#
#   $isoform_accession  "P12345-2"  (UniProt isoform suffix)
#       A peptide "ISOPEPTIDEK" is annotated to "P12345-2". DECISION: we key the
#       $fasta map under the BASE accession "P12345" ONLY (isoforms are not
#       separate FASTA entries in a canonical UniProt proteome FASTA download).
#       This lets downstream code test base-fallback resolution: strip the
#       "-<n>" suffix, then look up "P12345". Exposed:
#         $isoform_accession      = "P12345-2"
#         $isoform_base_accession = "P12345"
#       FASTA P12345 = "MSTART" (6) + "ISOPEPTIDEK" + "END" -> peptide start 7.
#
#   $tie_peptides  c("TIEPEPONEK", "TIEPEPTWOK")
#       Two rows, SAME accession ($tie_accession = "TIEPROT"), given IDENTICAL
#       (adj.P.Val, logFC) for every contrast (deterministic-tiebreak test).
#       Both peptides exist in TIEPROT's FASTA at distinct positions.
#       FASTA TIEPROT = "MA" (2) + "TIEPEPONEK" (10) + "GG" (2) + "TIEPEPTWOK"
#         -> TIEPEPONEK start 3, TIEPEPTWOK start 15.
#
#   $bad_seq_peptide  "PEPT(ox)IDE"   (contains non-[A-Z] mod token)
#       Present in $peptides so the FASTA mapper routes it to
#       reason = "bad_sequence_format". Annotated to "BADPROT" (FASTA present
#       but irrelevant because the sequence is rejected before lookup).
#
#   $low_n_condition  "LowN"
#       A real condition with 3 replicate columns, but at least one peptide row
#       has fewer than 3 non-NA values across those columns (so CV computation
#       yields reason = "insufficient_replicates"). The FIRST data row is forced
#       to have 2 NAs (only 1 non-NA) in the LowN columns.
#
# Conditions / contrast:
#   Conditions: "AY9944_10uM", "DMSO", "LowN" -- each with 3 replicates
#   (R1..R3). Sample columns are "<condition>_R<n>".
#   Contrast: "AY9944_vs_DMSO" -> columns logFC.AY9944_vs_DMSO,
#   P.Value.AY9944_vs_DMSO, adj.P.Val.AY9944_vs_DMSO. $contrasts = that key.
################################################################################

# Build a synthetic Spectronaut-like PELSA peptide frame plus FASTA map and
# per-contrast statistic columns.
#
# @param seed              integer seed for full determinism (default 1)
# @param n_extra_peptides  number of generic filler peptide rows to append
# @return named list (see header + Required API in the task spec)
pelsa_make_synthetic <- function(seed = 1, n_extra_peptides = 50) {
  set.seed(seed)

  # ---- Conditions, replicates, sample columns -----------------------------
  conditions <- c("AY9944_10uM", "DMSO", "LowN")
  n_rep <- 3L
  sample_cols <- as.vector(t(outer(
    conditions, seq_len(n_rep),
    FUN = function(cond, r) paste0(cond, "_R", r)
  )))
  # Derive the condition for each sample column by stripping the _R<n> suffix.
  condition_map <- sub("_R[0-9]+$", "", sample_cols)
  names(condition_map) <- sample_cols

  low_n_condition <- "LowN"
  low_n_cols <- names(condition_map)[condition_map == low_n_condition]

  # ---- Seeded edge-case peptides (see header for ground truth) -------------
  shared_peptide  <- "SHAREDPEPTIDEK"
  dup_peptide     <- "DUPLICATEK"
  overlap_peptide <- "AAA"
  absent_peptide  <- "GHOSTPEPTIDEK"
  il_peptide      <- "PEPTIDEWLTHISO"
  iso_peptide     <- "ISOPEPTIDEK"
  tie_pep_one     <- "TIEPEPONEK"
  tie_pep_two     <- "TIEPEPTWOK"
  bad_seq_peptide <- "PEPT(ox)IDE"

  isoform_accession      <- "P12345-2"
  isoform_base_accession <- "P12345"
  tie_accession          <- "TIEPROT"
  absent_accession       <- "ABSENTPROT"
  il_accession           <- "ILPROT"
  overlap_accession      <- "OVERLAPPROT"

  # ---- FASTA map (built so every position is exact by construction) --------
  fasta <- list()
  # shared peptide in 3 accessions at known starts
  fasta[["SHARED1"]] <- paste0("MKLV", shared_peptide)              # start 5
  fasta[["SHARED2"]] <- paste0("MAAAAAAAAA", shared_peptide)        # start 11
  fasta[["SHARED3"]] <- paste0("M", shared_peptide)                 # start 2
  # dup peptide twice in one protein
  fasta[["DUPPROT"]] <- paste0("MGG", dup_peptide, "WWWWW", dup_peptide, "QQ")
  dup_peptide_starts <- c(4L, 4L + nchar(dup_peptide) + 5L) # c(4L, 19L)
  # overlapping repeat
  fasta[[overlap_accession]] <- paste0("MCDEF", "AAAA", "GH")
  overlap_peptide_starts <- c(6L, 7L)
  # absent: FASTA deliberately lacks the peptide
  fasta[[absent_accession]] <- "MQWERTYQWERTYQWERTY"
  # I->L case: FASTA region differs from peptide only by I<->L
  fasta[[il_accession]] <- paste0("MK", "PEPTIDEWITHISO", "RR")
  il_peptide_position <- 3L
  # isoform: keyed under BASE accession only (see header DECISION comment)
  fasta[[isoform_base_accession]] <- paste0("MSTART", iso_peptide, "END")
  # tie peptides both present at distinct positions
  fasta[[tie_accession]] <- paste0("MA", tie_pep_one, "GG", tie_pep_two)
  # bad-seq peptide's protein exists but seq is rejected before lookup
  fasta[["BADPROT"]] <- paste0("MZ", "PEPTIDE", "ZZ")

  # ---- Assemble the seeded rows -------------------------------------------
  # Each row: accession (;-delimited), genes (;-aligned), positions (;-aligned),
  # stripped sequence, proteotypic flag.
  seeded <- list(
    # shared peptide -> 3 accessions; single gene token for many accessions
    list(acc = "SHARED1;SHARED2;SHARED3", gene = "SHAREDGENE",
         pos = "5;11;2", seq = shared_peptide, proteotypic = FALSE),
    # dup peptide -> one protein, two positions ;-joined
    list(acc = "DUPPROT", gene = "DUPGENE",
         pos = paste(dup_peptide_starts, collapse = ";"),
         seq = dup_peptide, proteotypic = TRUE),
    # overlap peptide
    list(acc = overlap_accession, gene = "OVERLAPGENE",
         pos = paste(overlap_peptide_starts, collapse = ";"),
         seq = overlap_peptide, proteotypic = TRUE),
    # absent peptide (annotated but not in FASTA)
    list(acc = absent_accession, gene = "ABSENTGENE",
         pos = "999", seq = absent_peptide, proteotypic = TRUE),
    # I->L peptide
    list(acc = il_accession, gene = "ILGENE",
         pos = "3", seq = il_peptide, proteotypic = TRUE),
    # isoform peptide -> accession with -2 suffix
    list(acc = isoform_accession, gene = "ISOGENE",
         pos = "7", seq = iso_peptide, proteotypic = TRUE),
    # tie peptide one (same accession as tie two)
    list(acc = tie_accession, gene = "TIEGENE",
         pos = "3", seq = tie_pep_one, proteotypic = TRUE),
    # tie peptide two
    list(acc = tie_accession, gene = "TIEGENE",
         pos = "15", seq = tie_pep_two, proteotypic = TRUE),
    # bad-sequence peptide (non-[A-Z] mod token)
    list(acc = "BADPROT", gene = "BADGENE",
         pos = "3", seq = bad_seq_peptide, proteotypic = TRUE),
    # multi-gene aligned tokens (2 accessions, 2 genes)
    list(acc = "MULTI1;MULTI2", gene = "GENEA;GENEB",
         pos = "10;20", seq = "MULTIGENEPEPK", proteotypic = FALSE),
    # empty gene token (accession present, gene blank)
    list(acc = "NOGENEPROT", gene = "",
         pos = "5", seq = "NOGENEPEPTIDEK", proteotypic = TRUE)
  )

  # FASTA entries for the auxiliary single-occurrence peptides so coverage tests
  # have a target. Positions match the ;-aligned annotation above.
  fasta[["MULTI1"]] <- paste0(strrep("Z", 9L), "MULTIGENEPEPK") # start 10
  fasta[["MULTI2"]] <- paste0(strrep("Z", 19L), "MULTIGENEPEPK") # start 20
  fasta[["NOGENEPROT"]] <- paste0("MQRS", "NOGENEPEPTIDEK")      # start 5

  n_seeded <- length(seeded)

  build_seeded_df <- function() {
    data.frame(
      PG.ProteinGroups    = vapply(seeded, function(x) x$acc, character(1)),
      PG.ProteinAccessions = vapply(seeded, function(x) x$acc, character(1)),
      PG.Genes            = vapply(seeded, function(x) x$gene, character(1)),
      PG.Organisms        = rep("Homo sapiens", n_seeded),
      PG.ProteinNames     = vapply(seeded, function(x) paste0(x$gene, "_HUMAN"),
                                   character(1)),
      PEP.StrippedSequence = vapply(seeded, function(x) x$seq, character(1)),
      PEP.IsProteotypic   = vapply(seeded, function(x) x$proteotypic, logical(1)),
      PEP.PeptidePosition = vapply(seeded, function(x) x$pos, character(1)),
      stringsAsFactors    = FALSE,
      check.names         = FALSE
    )
  }

  # ---- Generic filler peptides --------------------------------------------
  aa <- strsplit("ACDEFGHIKLMNPQRSTVWY", "")[[1]]
  make_pep <- function() {
    len <- sample(7:15, 1L)
    paste0(paste0(sample(aa, len, replace = TRUE), collapse = ""), "K")
  }
  n_extra <- max(0L, as.integer(n_extra_peptides))
  extra_seqs <- vapply(seq_len(n_extra), function(i) make_pep(), character(1))
  extra_acc <- sprintf("FILLER%04d", seq_len(n_extra))

  extra_df <- if (n_extra > 0L) {
    data.frame(
      PG.ProteinGroups     = extra_acc,
      PG.ProteinAccessions = extra_acc,
      PG.Genes             = sprintf("GENE%04d", seq_len(n_extra)),
      PG.Organisms         = rep("Homo sapiens", n_extra),
      PG.ProteinNames      = sprintf("FILLER%04d_HUMAN", seq_len(n_extra)),
      PEP.StrippedSequence = extra_seqs,
      PEP.IsProteotypic    = rep(TRUE, n_extra),
      PEP.PeptidePosition  = as.character(sample(50:500, n_extra, replace = TRUE)),
      stringsAsFactors     = FALSE,
      check.names          = FALSE
    )
  } else {
    NULL
  }
  # FASTA for filler peptides (start 11, flanked by a 10-residue M-prefix)
  for (i in seq_len(n_extra)) {
    fasta[[extra_acc[i]]] <- paste0(strrep("M", 10L), extra_seqs[i])
  }

  peptides <- rbind(build_seeded_df(), extra_df)
  n_total <- nrow(peptides)

  # ---- Intensity matrix (with deterministic NA holes) ----------------------
  intensity <- matrix(
    round(stats::rlnorm(n_total * length(sample_cols), meanlog = 12, sdlog = 1), 1),
    nrow = n_total, ncol = length(sample_cols),
    dimnames = list(NULL, sample_cols)
  )
  # Punch deterministic NA holes (~8% overall) so missingness logic is tested.
  n_cells <- length(intensity)
  na_idx <- sample.int(n_cells, size = floor(0.08 * n_cells))
  intensity[na_idx] <- NA_real_

  # Force the FIRST data row to have <3 non-NA in the LowN condition:
  # set 2 of its 3 LowN replicates to NA (leaving exactly 1 non-NA).
  intensity[1L, low_n_cols[1:2]] <- NA_real_
  intensity[1L, low_n_cols[3]] <- round(stats::rlnorm(1, 12, 1), 1)

  intensity_df <- as.data.frame(intensity, check.names = FALSE)
  peptides <- cbind(peptides, intensity_df)

  # ---- Per-contrast statistic columns -------------------------------------
  contrast <- "AY9944_vs_DMSO"
  contrasts <- contrast

  logfc <- round(stats::rnorm(n_total, 0, 1.5), 4)
  pval <- round(stats::runif(n_total, 0, 1), 6)
  adjp <- round(stats::p.adjust(pval, method = "BH"), 6)

  # Force tie rows (seeded indices 7 and 8: TIEPEPONEK, TIEPEPTWOK) to share
  # identical (adj.P.Val, logFC) for the contrast (deterministic-tiebreak test).
  tie_idx <- which(peptides$PEP.StrippedSequence %in% c(tie_pep_one, tie_pep_two))
  logfc[tie_idx] <- 1.2345
  adjp[tie_idx] <- 0.0420
  pval[tie_idx] <- 0.0011

  peptides[[paste0("logFC.", contrast)]] <- logfc
  peptides[[paste0("P.Value.", contrast)]] <- pval
  peptides[[paste0("adj.P.Val.", contrast)]] <- adjp

  # ---- Return ground-truth handles ----------------------------------------
  list(
    peptides       = peptides,
    fasta          = fasta,
    sample_cols    = sample_cols,
    condition_map  = condition_map,
    contrasts      = contrasts,

    shared_peptide = shared_peptide,

    dup_peptide        = dup_peptide,
    dup_peptide_starts = dup_peptide_starts,

    overlap_peptide          = overlap_peptide,
    overlap_peptide_starts   = overlap_peptide_starts,
    overlap_peptide_accession = overlap_accession,

    absent_peptide           = absent_peptide,
    absent_peptide_accession = absent_accession,

    il_peptide          = il_peptide,
    il_peptide_accession = il_accession,
    il_peptide_position = il_peptide_position,

    isoform_accession      = isoform_accession,
    isoform_base_accession = isoform_base_accession,

    tie_peptides  = c(tie_pep_one, tie_pep_two),
    tie_accession = tie_accession,

    bad_seq_peptide = bad_seq_peptide,

    shared_peptide_accessions = c("SHARED1", "SHARED2", "SHARED3"),

    # ;-aligned multi-gene row (acc "MULTI1;MULTI2", gene "GENEA;GENEB")
    multi_gene_peptide = "MULTIGENEPEPK",
    # empty-gene row (acc "NOGENEPROT", gene "")
    no_gene_peptide = "NOGENEPEPTIDEK",

    low_n_condition = low_n_condition
  )
}
