################################################################################
# Tests for PELSA per-protein sequence coverage (interval union).
#
#   pelsa_sequence_coverage(matched_cache, fasta_map, ...) -> data.frame
#     columns: accession, covered_residues, protein_length, coverage
#
# Coverage = union of a protein's mapped peptide [pep_start, pep_end] spans
# (overlaps counted ONCE, NOT summed) / FASTA length. Input is the $matched
# cache from pelsa_map_peptide_positions() (2B) -- FASTA-mapped peptides only,
# no Spectronaut fallback. FASTA length resolves via the SAME isoform-base
# fallback 2B uses (exact key, then strip "-<digits>").
#
# Ground truth is closed-form (see each block). Integration test runs the REAL
# explode + FASTA-map helpers from the synthetic generator and asserts shape +
# [0,1] bounds + shared-peptide contribution.
################################################################################

source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))

# Hand-built matched_cache helper: one row per (accession, span).
.mk_matched <- function(accession, pep_start, pep_end) {
  data.frame(
    accession = accession,
    pep_start = as.integer(pep_start),
    pep_end = as.integer(pep_end),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# ---- closed-form coverage ----------------------------------------------------

test_that("overlapping spans union (counted once, not summed)", {
  # A: len 20, spans [1,10] + [5,15] -> union [1,15] = 15 residues -> 15/20.
  mc <- .mk_matched(c("A", "A"), c(1L, 5L), c(10L, 15L))
  fa <- list(A = strrep("X", 20L))

  res <- pelsa_sequence_coverage(mc, fa)
  row <- res[res$accession == "A", , drop = FALSE]

  expect_equal(nrow(row), 1L)
  expect_equal(row$covered_residues, 15L)
  expect_equal(row$protein_length, 20L)
  expect_equal(row$coverage, 0.75)
})

test_that("disjoint spans sum (no overlap, no merge of adjacent)", {
  # B: len 10, spans [1,5] + [6,10] -> 5 + 5 = 10 residues (6 > 5, disjoint).
  mc <- .mk_matched(c("B", "B"), c(1L, 6L), c(5L, 10L))
  fa <- list(B = strrep("X", 10L))

  res <- pelsa_sequence_coverage(mc, fa)
  row <- res[res$accession == "B", , drop = FALSE]

  expect_equal(row$covered_residues, 10L)
  expect_equal(row$coverage, 1.0)
})

test_that("touching-overlap spans merge at the shared residue", {
  # C: len 10, spans [1,5] + [5,10] -> overlap at residue 5 -> union [1,10] = 10.
  mc <- .mk_matched(c("C", "C"), c(1L, 5L), c(5L, 10L))
  fa <- list(C = strrep("X", 10L))

  res <- pelsa_sequence_coverage(mc, fa)
  row <- res[res$accession == "C", , drop = FALSE]

  expect_equal(row$covered_residues, 10L)
  expect_equal(row$coverage, 1.0)
})

test_that("single span coverage is span_length / protein_length", {
  # D: len 100, span [10,19] -> 10 residues -> 0.1.
  mc <- .mk_matched("D", 10L, 19L)
  fa <- list(D = strrep("X", 100L))

  res <- pelsa_sequence_coverage(mc, fa)
  row <- res[res$accession == "D", , drop = FALSE]

  expect_equal(row$covered_residues, 10L)
  expect_equal(row$protein_length, 100L)
  expect_equal(row$coverage, 0.1)
})

test_that("a shared peptide contributes its span to EVERY mapped accession", {
  # One peptide span maps to both A and B (two rows, different accession).
  # A: len 20, span [1,10] -> 10/20 = 0.5
  # B: len 40, span [1,10] -> 10/40 = 0.25
  mc <- .mk_matched(c("A", "B"), c(1L, 1L), c(10L, 10L))
  fa <- list(A = strrep("X", 20L), B = strrep("X", 40L))

  res <- pelsa_sequence_coverage(mc, fa)

  expect_equal(res$covered_residues[res$accession == "A"], 10L)
  expect_equal(res$coverage[res$accession == "A"], 0.5)
  expect_equal(res$covered_residues[res$accession == "B"], 10L)
  expect_equal(res$coverage[res$accession == "B"], 0.25)
})

test_that("isoform accession resolves FASTA length via base-key fallback", {
  # P12345-2 keyed under base P12345 (len 50), span [7,16] -> 10/50 = 0.2.
  mc <- .mk_matched("P12345-2", 7L, 16L)
  fa <- list(P12345 = strrep("X", 50L))

  res <- pelsa_sequence_coverage(mc, fa)
  row <- res[res$accession == "P12345-2", , drop = FALSE]

  expect_equal(row$covered_residues, 10L)
  expect_equal(row$protein_length, 50L)
  expect_equal(row$coverage, 0.2)
})

test_that("unresolved accession (no exact or base key) -> NA length and coverage", {
  mc <- .mk_matched("GHOST", 1L, 5L)
  fa <- list(OTHER = strrep("X", 30L))

  res <- pelsa_sequence_coverage(mc, fa)
  row <- res[res$accession == "GHOST", , drop = FALSE]

  expect_equal(nrow(row), 1L)
  expect_equal(row$covered_residues, 5L) # union still computable
  expect_true(is.na(row$protein_length))
  expect_true(is.na(row$coverage))
})

test_that("one row per distinct accession; multiple accessions handled together", {
  mc <- .mk_matched(
    c("A", "A", "B", "C"),
    c(1L, 5L, 1L, 10L),
    c(10L, 15L, 5L, 19L)
  )
  fa <- list(A = strrep("X", 20L), B = strrep("X", 10L), C = strrep("X", 100L))

  res <- pelsa_sequence_coverage(mc, fa)

  expect_setequal(res$accession, c("A", "B", "C"))
  expect_equal(nrow(res), 3L)
  expect_equal(res$coverage[res$accession == "A"], 0.75)
  expect_equal(res$coverage[res$accession == "B"], 0.5)
  expect_equal(res$coverage[res$accession == "C"], 0.1)
})

test_that("zero-length FASTA -> coverage NA (no divide-by-zero)", {
  mc <- .mk_matched("E", 1L, 1L)
  fa <- list(E = "")

  res <- pelsa_sequence_coverage(mc, fa)
  row <- res[res$accession == "E", , drop = FALSE]

  expect_equal(row$protein_length, 0L)
  expect_true(is.na(row$coverage))
  expect_false(row$over_length_flag) # zero-length is NOT an over-length anomaly
})

# ---- over-length anomaly: warn + clamp + flag (soft fail, no abort) ----------

test_that("over-length span clamps to 1.0, warns, and flags (does not error)", {
  # A: span [1,30] but FASTA only 20 long -> union 30 > 20 -> clamp to 20/20=1.0.
  # B: a normal accession alongside it -> unaffected, flag FALSE.
  mc <- .mk_matched(c("A", "B"), c(1L, 1L), c(30L, 5L))
  fa <- list(A = strrep("X", 20L), B = strrep("X", 10L))

  expect_warning(
    res <- pelsa_sequence_coverage(mc, fa),
    "exceed protein length"
  )

  row_a <- res[res$accession == "A", , drop = FALSE]
  expect_equal(row_a$covered_residues, 20L) # clamped to protein_length
  expect_equal(row_a$protein_length, 20L)
  expect_equal(row_a$coverage, 1.0) # clamped, stays in [0,1]
  expect_true(row_a$over_length_flag)

  row_b <- res[res$accession == "B", , drop = FALSE]
  expect_equal(row_b$coverage, 0.5)
  expect_false(row_b$over_length_flag)
})

# ---- boundary validation -----------------------------------------------------

test_that("a direct NA pep_start/pep_end input row triggers the coercion guard", {
  fa <- list(A = strrep("X", 20L))
  expect_error(
    pelsa_sequence_coverage(.mk_matched("A", NA_integer_, 10L), fa),
    "integer-coercible"
  )
  expect_error(
    pelsa_sequence_coverage(.mk_matched("A", 1L, NA_integer_), fa),
    "integer-coercible"
  )
})

test_that("missing required columns error", {
  fa <- list(A = "XXXX")
  expect_error(
    pelsa_sequence_coverage(data.frame(accession = "A", pep_start = 1L), fa),
    "pep_end"
  )
  expect_error(
    pelsa_sequence_coverage(data.frame(foo = 1), fa),
    "accession"
  )
})

test_that("a span with start > end is rejected (fail fast)", {
  mc <- .mk_matched("A", 10L, 5L)
  fa <- list(A = strrep("X", 20L))
  expect_error(pelsa_sequence_coverage(mc, fa), "start")
})

test_that("non-data.frame matched_cache and non-list fasta_map error", {
  expect_error(pelsa_sequence_coverage(list(), list(A = "X")))
  expect_error(
    pelsa_sequence_coverage(.mk_matched("A", 1L, 2L), 42),
    "fasta_map"
  )
})

test_that("empty matched_cache returns a zero-row frame with the right columns", {
  res <- pelsa_sequence_coverage(.mk_matched(character(0), integer(0), integer(0)),
                                 list(A = "XXXX"))
  expect_equal(nrow(res), 0L)
  expect_setequal(
    colnames(res),
    c("accession", "covered_residues", "protein_length", "coverage",
      "over_length_flag")
  )
})

# ---- integration with the REAL explode + FASTA-map helpers -------------------

test_that("integration: real matched cache -> valid coverage in [0,1]", {
  syn <- pelsa_make_synthetic(seed = 1)
  exploded <- pelsa_explode_accessions(syn$peptides)
  mapped <- pelsa_map_peptide_positions(exploded, syn$fasta)

  res <- pelsa_sequence_coverage(mapped$matched, syn$fasta)

  expect_setequal(
    colnames(res),
    c("accession", "covered_residues", "protein_length", "coverage",
      "over_length_flag")
  )
  # one row per distinct matched accession
  expect_setequal(res$accession, unique(mapped$matched$accession))
  expect_false(anyDuplicated(res$accession) > 0L)

  # resolved coverage in [0,1]; covered never exceeds protein length.
  resolved <- res[!is.na(res$coverage), , drop = FALSE]
  expect_true(all(resolved$coverage >= 0 & resolved$coverage <= 1))
  expect_true(all(resolved$covered_residues <= resolved$protein_length))
  # correctly-mapped real data never over-shoots -> no clamping.
  expect_false(any(res$over_length_flag))
})

test_that("integration: shared peptide contributes to ALL its accessions", {
  syn <- pelsa_make_synthetic(seed = 1)
  exploded <- pelsa_explode_accessions(syn$peptides)
  mapped <- pelsa_map_peptide_positions(exploded, syn$fasta)

  res <- pelsa_sequence_coverage(mapped$matched, syn$fasta)

  shared_accs <- syn$shared_peptide_accessions # c("SHARED1","SHARED2","SHARED3")
  expect_true(all(shared_accs %in% res$accession))

  pep_len <- nchar(syn$shared_peptide)
  for (acc in shared_accs) {
    row <- res[res$accession == acc, , drop = FALSE]
    # the shared peptide is the only peptide on these accessions in the synthetic
    # set, so covered_residues == its length.
    expect_equal(row$covered_residues, pep_len)
    expect_equal(
      row$protein_length, as.integer(nchar(syn$fasta[[acc]]))
    )
  }
})
