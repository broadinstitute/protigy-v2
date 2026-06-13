################################################################################
# Tests for the PELSA marker-matching helpers (Task 2J) — isoform-aware,
# any-token, case-insensitive accession matching + marker paste-box parsing.
#
# The matching rule under test (documented in tab_pelsa_marker_helpers.R):
#   A peptide is a marker hit if ANY of its ;-delimited accession tokens,
#   normalized to isoform-BASE (strip trailing "-<digits>") and lowercased,
#   equals ANY marker's isoform-base + lowercased. The rule is SYMMETRIC:
#   marker "P12345" hits a peptide on "P12345-2" AND marker "P12345-2" hits a
#   peptide on "P12345" (both normalize to base "p12345").
#
# pelsa_parse_markers reuses parse_protein_search_input (split on
# space/comma/semicolon/newline, trim, drop empties).
################################################################################

source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))

# ---- pelsa_isoform_base ------------------------------------------------------

test_that("pelsa_isoform_base strips UniProt isoform suffix, vectorized + NA-safe", {
  expect_identical(
    pelsa_isoform_base(c("P12345-2", "P12345", "Q9-10", NA)),
    c("P12345", "P12345", "Q9", NA)
  )
})

test_that("pelsa_isoform_base returns character(0) for empty input", {
  expect_identical(pelsa_isoform_base(character(0)), character(0))
})

test_that("pelsa_isoform_base only strips trailing -<digits>, not internal dashes", {
  expect_identical(
    pelsa_isoform_base(c("A0A-B1", "P1-2-3")),
    c("A0A-B1", "P1-2")
  )
})

# ---- pelsa_parse_markers -----------------------------------------------------

test_that("pelsa_parse_markers splits on space/comma/semicolon/newline", {
  expect_identical(
    pelsa_parse_markers("P1, Q2; R3\nS4"),
    c("P1", "Q2", "R3", "S4")
  )
})

test_that("pelsa_parse_markers returns character(0) on empty/NULL/whitespace", {
  expect_identical(pelsa_parse_markers(""), character(0))
  expect_identical(pelsa_parse_markers(NULL), character(0))
  expect_identical(pelsa_parse_markers("   \n  "), character(0))
})

# ---- pelsa_match_markers: core rule ------------------------------------------

test_that("marker matches a peptide on its isoform form (base vs -2)", {
  expect_identical(
    pelsa_match_markers("AAAAAA-2", "AAAAAA"),
    TRUE
  )
})

test_that("matching is SYMMETRIC: marker -2 matches peptide on base", {
  expect_identical(
    pelsa_match_markers("AAAAAA", "AAAAAA-2"),
    TRUE
  )
})

test_that("matching is case-insensitive", {
  expect_identical(
    pelsa_match_markers("P12345", "p12345"),
    TRUE
  )
})

test_that("ANY-TOKEN: a non-leading accession token matches", {
  expect_identical(
    pelsa_match_markers("Q99999;P12345", "P12345"),
    TRUE
  )
})

test_that("non-matching marker yields FALSE", {
  expect_identical(
    pelsa_match_markers("P12345;Q99999", "ZZZZZZ"),
    FALSE
  )
})

test_that("empty marker_accessions -> all FALSE", {
  expect_identical(
    pelsa_match_markers(c("P12345", "Q99999"), character(0)),
    c(FALSE, FALSE)
  )
})

test_that("NA / empty peptide accessions -> FALSE", {
  expect_identical(
    pelsa_match_markers(c(NA, "", "P12345"), "P12345"),
    c(FALSE, FALSE, TRUE)
  )
})

test_that("vector form returns a correct per-row logical vector", {
  res <- pelsa_match_markers(
    c("P12345;Q99999", "ZZZZZZ", "P00000;P12345-7"),
    c("P12345", "AAAAAA")
  )
  expect_identical(res, c(TRUE, FALSE, TRUE))
})

test_that("tokens are whitespace-trimmed before matching", {
  expect_identical(
    pelsa_match_markers(" P12345 ; Q99999 ", " p12345 "),
    TRUE
  )
})

test_that("list (already-split) form is accepted", {
  res <- pelsa_match_markers(
    list(c("Q99999", "P12345"), c("ZZZZZZ")),
    "P12345"
  )
  expect_identical(res, c(TRUE, FALSE))
})

test_that("NA markers are ignored, not propagated", {
  expect_identical(
    pelsa_match_markers("P12345", c(NA, "P12345")),
    TRUE
  )
})

test_that("duplicate markers do not break matching", {
  expect_identical(
    pelsa_match_markers("P12345", c("P12345", "P12345")),
    TRUE
  )
})

test_that("peptide of only delimiters (';;') yields FALSE", {
  expect_identical(
    pelsa_match_markers(";;", "P12345"),
    FALSE
  )
})

# ---- Boundary validation -----------------------------------------------------

test_that("pelsa_match_markers fails fast on bad marker type", {
  expect_error(pelsa_match_markers("P12345", 123L))
})

test_that("pelsa_match_markers fails fast on bad accession type", {
  expect_error(pelsa_match_markers(123L, "P12345"))
})

# ---- Integration: generator isoform parity -----------------------------------

test_that("generator: marker on isoform BASE matches the peptide on the isoform form", {
  syn <- pelsa_make_synthetic()

  # The seeded isoform peptide row carries accession "P12345-2"; the marker is
  # entered as the BASE accession "P12345". The symmetric isoform-base rule must
  # flag that peptide.
  acc_strings <- syn$peptides$PG.ProteinAccessions
  marker <- syn$isoform_base_accession              # "P12345"

  hits <- pelsa_match_markers(acc_strings, marker)

  iso_row <- which(syn$peptides$PG.ProteinAccessions == syn$isoform_accession)
  expect_length(iso_row, 1L)
  expect_true(hits[iso_row])

  # Exactly one row should be flagged (only the isoform peptide carries P12345*).
  expect_identical(sum(hits), 1L)
})

# ---- Performance --------------------------------------------------------------

test_that("pelsa_match_markers handles ~100k peptides quickly (vectorized)", {
  set.seed(7)
  n <- 100000L
  acc <- sprintf("ACC%05d;Q%05d", sample.int(5000L, n, replace = TRUE),
                 sample.int(5000L, n, replace = TRUE))
  acc[seq_len(50L)] <- "P12345-2;Q00001"
  markers <- c("P12345", "NOPE1", "NOPE2")

  t <- system.time(res <- pelsa_match_markers(acc, markers))
  expect_identical(sum(res), 50L)
  expect_lt(t[["elapsed"]], 0.5)
})
