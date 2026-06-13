################################################################################
# Tests for the seeded synthetic PELSA peptide-frame generator.
#
# The generator (tests/testthat/fixtures/pelsa/generate_synthetic.R) is the
# single shared input for all Phase-2 PELSA helper parity tests. These tests
# verify the generator's contract and that every documented ground-truth
# coordinate holds by construction.
################################################################################

source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))

test_that("pelsa_make_synthetic is deterministic across calls for a fixed seed", {
  a <- pelsa_make_synthetic(seed = 1)
  b <- pelsa_make_synthetic(seed = 1)
  expect_identical(a$peptides, b$peptides)
  expect_identical(a$fasta, b$fasta)
  expect_identical(a$condition_map, b$condition_map)
})

test_that("return list exposes the required top-level fields", {
  syn <- pelsa_make_synthetic(seed = 1)
  expect_s3_class(syn$peptides, "data.frame")
  expect_type(syn$fasta, "list")
  expect_type(syn$sample_cols, "character")
  expect_type(syn$condition_map, "character")
  expect_type(syn$contrasts, "character")
  expect_gt(length(syn$contrasts), 0)
})

test_that("peptide frame carries the exact Spectronaut column names", {
  syn <- pelsa_make_synthetic(seed = 1)
  required <- c(
    "PG.ProteinGroups", "PG.ProteinAccessions", "PG.Genes", "PG.Organisms",
    "PG.ProteinNames", "PEP.StrippedSequence", "PEP.IsProteotypic",
    "PEP.PeptidePosition"
  )
  expect_true(all(required %in% colnames(syn$peptides)))
  # sample intensity columns must all be present too
  expect_true(all(syn$sample_cols %in% colnames(syn$peptides)))
})

test_that("frame is small (~30-60 base rows + n_extra_peptides)", {
  syn <- pelsa_make_synthetic(seed = 1, n_extra_peptides = 50)
  n <- nrow(syn$peptides)
  expect_gte(n, 30)
  expect_lte(n, 120)
})

test_that("shared peptide maps to >=3 accessions and contains ';'", {
  syn <- pelsa_make_synthetic(seed = 1)
  shared <- syn$shared_peptide
  expect_true(shared %in% syn$peptides$PEP.StrippedSequence)
  row <- syn$peptides[syn$peptides$PEP.StrippedSequence == shared, , drop = FALSE]
  acc <- row$PG.ProteinAccessions[1]
  expect_true(grepl(";", acc))
  expect_gte(length(strsplit(acc, ";", fixed = TRUE)[[1]]), 3)
})

test_that("dup peptide appears in frame and occurs exactly twice in its FASTA at known starts", {
  syn <- pelsa_make_synthetic(seed = 1)
  dup <- syn$dup_peptide
  expect_true(dup %in% syn$peptides$PEP.StrippedSequence)

  starts <- syn$dup_peptide_starts
  expect_type(starts, "integer")
  expect_length(starts, 2L)

  # locate the FASTA for the dup peptide's accession
  row <- syn$peptides[syn$peptides$PEP.StrippedSequence == dup, , drop = FALSE]
  acc <- strsplit(row$PG.ProteinAccessions[1], ";", fixed = TRUE)[[1]][1]
  seq <- syn$fasta[[acc]]
  expect_false(is.null(seq))

  # exactly two non-overlapping occurrences, at the documented starts
  occ <- gregexpr(dup, seq, fixed = TRUE)[[1]]
  occ <- as.integer(occ[occ > 0])
  expect_identical(sort(occ), sort(starts))
  expect_length(occ, 2L)
})

test_that("overlap peptide occurs twice at i and i+1 (overlapping) in its FASTA", {
  syn <- pelsa_make_synthetic(seed = 1)
  ov <- syn$overlap_peptide
  starts <- syn$overlap_peptide_starts
  expect_length(starts, 2L)
  expect_equal(starts[2] - starts[1], 1L) # overlapping by one residue

  acc <- syn$overlap_peptide_accession
  seq <- syn$fasta[[acc]]
  # plain substring match is non-overlapping; verify overlap by manual scan
  hits <- which(vapply(
    seq_len(nchar(seq) - nchar(ov) + 1L),
    function(i) substr(seq, i, i + nchar(ov) - 1L) == ov,
    logical(1)
  ))
  expect_true(all(starts %in% hits))
})

test_that("absent peptide is in frame but NOT in its annotated accession's FASTA", {
  syn <- pelsa_make_synthetic(seed = 1)
  pep <- syn$absent_peptide
  acc <- syn$absent_peptide_accession
  expect_true(pep %in% syn$peptides$PEP.StrippedSequence)
  seq <- syn$fasta[[acc]]
  expect_false(is.null(seq))
  expect_false(grepl(pep, seq, fixed = TRUE))
})

test_that("I->L peptide fails exact match but matches after I->L normalization", {
  syn <- pelsa_make_synthetic(seed = 1)
  pep <- syn$il_peptide
  acc <- syn$il_peptide_accession
  seq <- syn$fasta[[acc]]
  expect_false(is.null(seq))

  # no exact substring match
  expect_false(grepl(pep, seq, fixed = TRUE))

  # matches after I->L on BOTH sides
  pep_n <- gsub("I", "L", pep, fixed = TRUE)
  seq_n <- gsub("I", "L", seq, fixed = TRUE)
  expect_true(grepl(pep_n, seq_n, fixed = TRUE))

  # documented known position after I->L retry
  pos <- regexpr(pep_n, seq_n, fixed = TRUE)[[1]]
  expect_equal(as.integer(pos), syn$il_peptide_position)
})

test_that("isoform accession is exposed with base and isoform handles, and has a peptide", {
  syn <- pelsa_make_synthetic(seed = 1)
  iso <- syn$isoform_accession
  expect_true(grepl("-", iso, fixed = TRUE))
  base <- sub("-.*$", "", iso)
  expect_identical(syn$isoform_base_accession, base)
  # a peptide row references the isoform accession
  expect_true(any(grepl(iso, syn$peptides$PG.ProteinAccessions, fixed = TRUE)))
})

test_that("tie peptides share identical (adj.P.Val, logFC) for the first contrast", {
  syn <- pelsa_make_synthetic(seed = 1)
  ties <- syn$tie_peptides
  expect_length(ties, 2L)
  contrast <- syn$contrasts[1]
  logfc_col <- paste0("logFC.", contrast)
  adjp_col <- paste0("adj.P.Val.", contrast)

  rows <- syn$peptides[syn$peptides$PEP.StrippedSequence %in% ties, , drop = FALSE]
  rows <- rows[match(ties, rows$PEP.StrippedSequence), , drop = FALSE]
  expect_equal(rows[[logfc_col]][1], rows[[logfc_col]][2])
  expect_equal(rows[[adjp_col]][1], rows[[adjp_col]][2])
  # same accession
  expect_identical(rows$PG.ProteinAccessions[1], rows$PG.ProteinAccessions[2])
  expect_identical(rows$PG.ProteinAccessions[1], syn$tie_accession)
})

test_that("bad-sequence peptide is present and not pure [A-Z]", {
  syn <- pelsa_make_synthetic(seed = 1)
  bad <- syn$bad_seq_peptide
  expect_true(bad %in% syn$peptides$PEP.StrippedSequence)
  expect_true(grepl("[^A-Z]", bad))
})

test_that("low-n condition has at least one row with <3 non-NA replicates", {
  syn <- pelsa_make_synthetic(seed = 1)
  cond <- syn$low_n_condition
  expect_true(cond %in% syn$condition_map)
  cols <- names(syn$condition_map)[syn$condition_map == cond]
  expect_gte(length(cols), 3L) # condition itself has >=3 replicate columns
  mat <- as.matrix(syn$peptides[, cols, drop = FALSE])
  n_nonNA <- rowSums(!is.na(mat))
  expect_true(any(n_nonNA < 3L))
})

test_that("fasta is a named list of non-empty amino-acid strings", {
  syn <- pelsa_make_synthetic(seed = 1)
  expect_type(syn$fasta, "list")
  expect_true(length(syn$fasta) > 0)
  expect_false(is.null(names(syn$fasta)))
  expect_true(all(nzchar(names(syn$fasta))))
  for (s in syn$fasta) {
    expect_type(s, "character")
    expect_true(nchar(s) > 0)
    expect_match(s, "^[A-Z]+$")
  }
})

test_that("NA holes exist in at least one sample column", {
  syn <- pelsa_make_synthetic(seed = 1)
  mat <- as.matrix(syn$peptides[, syn$sample_cols, drop = FALSE])
  expect_true(any(is.na(mat)))
})

test_that("per-contrast statistic columns exist for every contrast", {
  syn <- pelsa_make_synthetic(seed = 1)
  for (contrast in syn$contrasts) {
    expect_true(paste0("logFC.", contrast) %in% colnames(syn$peptides))
    expect_true(paste0("adj.P.Val.", contrast) %in% colnames(syn$peptides))
    expect_true(paste0("P.Value.", contrast) %in% colnames(syn$peptides))
  }
})

test_that("PG.Genes covers single-gene-for-many, multi-gene, and empty-gene cases", {
  syn <- pelsa_make_synthetic(seed = 1)
  genes <- syn$peptides$PG.Genes
  accs <- syn$peptides$PG.ProteinAccessions

  n_acc <- vapply(strsplit(accs, ";", fixed = TRUE), length, integer(1))
  n_gene <- vapply(strsplit(genes, ";", fixed = TRUE), function(x) {
    length(x[nzchar(x)])
  }, integer(1))

  # single gene token for several accessions
  expect_true(any(n_acc >= 2L & n_gene == 1L))
  # aligned multi-gene tokens
  expect_true(any(n_acc >= 2L & n_gene >= 2L))
  # empty gene token
  expect_true(any(!nzchar(genes)))
})

test_that("seeded PEP.PeptidePosition annotations match FASTA-derived starts", {
  # Guards against silent drift between the hardcoded PEP.PeptidePosition
  # literals and the FASTA flanks that DEFINE those positions. For every seeded
  # row, split the ;-aligned accessions and positions, and for each accession
  # token that resolves to a FASTA entry (using isoform-base fallback), assert
  # the annotated position equals regexpr(peptide, fasta[[base_accession]]).
  syn <- pelsa_make_synthetic(seed = 1)

  # Rows whose annotated position is NOT defined by an exact FASTA substring
  # match are excluded (each is covered by its own dedicated test):
  #   - bad_seq_peptide:  sequence rejected before FASTA lookup
  #   - absent_peptide:   FASTA present but deliberately lacks the peptide
  #   - il_peptide:       only matches after I->L normalization, not exact
  skip_seqs <- c(syn$bad_seq_peptide, syn$absent_peptide, syn$il_peptide)

  # Seeded rows are those NOT generated as generic fillers.
  is_filler <- grepl("^FILLER[0-9]+$", syn$peptides$PG.ProteinAccessions)
  seeded_rows <- syn$peptides[!is_filler, , drop = FALSE]

  checked <- 0L
  for (i in seq_len(nrow(seeded_rows))) {
    pep <- seeded_rows$PEP.StrippedSequence[i]
    if (pep %in% skip_seqs) next

    accs <- strsplit(seeded_rows$PG.ProteinAccessions[i], ";", fixed = TRUE)[[1]]
    poss <- strsplit(seeded_rows$PEP.PeptidePosition[i], ";", fixed = TRUE)[[1]]

    # Multi-position / single-accession rows (dup, overlap) use ";" to list
    # several occurrences WITHIN one protein, not to align to accessions; they
    # have dedicated tests, so skip them here.
    if (length(accs) == 1L && length(poss) > 1L) next

    expect_length(poss, length(accs)) # positions are ;-aligned to accessions

    for (j in seq_along(accs)) {
      base_acc <- sub("-.*$", "", accs[j]) # isoform-base fallback (P12345-2 -> P12345)
      seq <- syn$fasta[[base_acc]]
      if (is.null(seq)) next # accession genuinely absent from FASTA (e.g. ABSENTPROT)

      fasta_start <- as.integer(regexpr(pep, seq, fixed = TRUE))
      annotated <- as.integer(poss[j])
      expect_equal(
        annotated, fasta_start,
        info = paste0("acc=", accs[j], " pep=", pep)
      )
      checked <- checked + 1L
    }
  }
  # Ensure the cross-check actually exercised multiple seeded positions.
  expect_gt(checked, 5L)
})

test_that("condition_map maps every sample column to a condition", {
  syn <- pelsa_make_synthetic(seed = 1)
  expect_setequal(names(syn$condition_map), syn$sample_cols)
  expect_true(all(nzchar(syn$condition_map)))
})
