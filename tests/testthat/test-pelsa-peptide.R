################################################################################
# Tests for the PELSA peptide helpers:
#   pelsa_missed_cleavages()  -  tryptic missed-cleavage count, notebook parity
#   pelsa_peptide_length()    -  peptide residue count
#   pelsa_build_multilabel()  -  canonical ;-joined gene_aa<pos> label builder
#
# Missed-cleavage is parity-gated against the analysis notebook's exact rule:
#   core = peptide[:-1]; len(re.findall(r'[KR](?!P)', core))
# i.e. drop the C-terminal residue, then count internal K/R NOT followed by P
# (K-P and R-P do NOT count). This MUST match the notebook exactly.
#
# pelsa_build_multilabel() is the single source of truth for volcano-point
# labels reused by the best-peptide rollup and volcano-df builders.
################################################################################

# --- pelsa_missed_cleavages() ------------------------------------------------

test_that("peptide ending in K with no internal K/R has 0 missed cleavages", {
  # core "PEPTIDE" has no K/R -> 0
  expect_equal(pelsa_missed_cleavages("PEPTIDEK"), 0L)
})

test_that("an internal R not before P counts as one missed cleavage", {
  # core "AAARAAA" -> one internal R -> 1
  expect_equal(pelsa_missed_cleavages("AAARAAAK"), 1L)
})

test_that("internal K immediately followed by P does NOT count", {
  # core "AAKPAAA" -> K-P excluded -> 0
  expect_equal(pelsa_missed_cleavages("AAKPAAAK"), 0L)
})

test_that("internal R immediately followed by P does NOT count", {
  # core "AARPAAA" -> R-P excluded -> 0
  expect_equal(pelsa_missed_cleavages("AARPAAAK"), 0L)
})

test_that("C-terminal K is excluded from the count", {
  # core "AAAAA" -> 0
  expect_equal(pelsa_missed_cleavages("AAAAAK"), 0L)
})

test_that("two internal counted sites give 2 missed cleavages", {
  # core "AKAAARAA" -> internal K (not before P) + internal R (not before P) -> 2
  expect_equal(pelsa_missed_cleavages("AKAAARAAK"), 2L)
})

test_that("single-residue peptide has 0 missed cleavages", {
  # nchar < 2 -> 0 (no internal positions)
  expect_equal(pelsa_missed_cleavages("K"), 0L)
})

test_that("two-residue KR follows the substr(.,1,nchar-1) rule", {
  # core "K" -> str_count("K","[KR](?!P)") = 1 (no following char -> not before P)
  expect_equal(pelsa_missed_cleavages("KR"), 1L)
})

test_that("missed cleavages is vectorized over a character vector", {
  expect_equal(
    pelsa_missed_cleavages(c("PEPTIDEK", "AAARAAAK", "AAKPAAAK")),
    c(0L, 1L, 0L)
  )
})

test_that("missed cleavages returns NA for an NA sequence", {
  # documented choice: NA sequence -> NA_integer_
  expect_equal(pelsa_missed_cleavages(NA_character_), NA_integer_)
})

test_that("missed cleavages handles NA mixed within a vector", {
  expect_equal(
    pelsa_missed_cleavages(c("AAARAAAK", NA_character_, "AAKPAAAK")),
    c(1L, NA_integer_, 0L)
  )
})

test_that("missed cleavages returns an integer vector", {
  out <- pelsa_missed_cleavages(c("PEPTIDEK", "AAARAAAK"))
  expect_type(out, "integer")
})

test_that("missed cleavages returns integer(0) for empty input", {
  expect_equal(pelsa_missed_cleavages(character(0)), integer(0))
})

test_that("missed cleavages coerces factor input like character input", {
  expect_equal(
    pelsa_missed_cleavages(factor(c("PEPTIDEK", "AAARAAAK"))),
    pelsa_missed_cleavages(c("PEPTIDEK", "AAARAAAK"))
  )
})

# --- pelsa_peptide_length() --------------------------------------------------

test_that("peptide length returns nchar with NA preserved, as integer", {
  expect_equal(
    pelsa_peptide_length(c("PEPK", "AA", NA)),
    c(4L, 2L, NA_integer_)
  )
})

test_that("peptide length returns an integer vector", {
  expect_type(pelsa_peptide_length(c("PEPK", "AA")), "integer")
})

test_that("peptide length returns integer(0) for empty input", {
  expect_equal(pelsa_peptide_length(character(0)), integer(0))
})

test_that("peptide length coerces factor input like character input", {
  expect_equal(
    pelsa_peptide_length(factor(c("PEPTIDEK", "AAARAAAK"))),
    pelsa_peptide_length(c("PEPTIDEK", "AAARAAAK"))
  )
})

# --- pelsa_build_multilabel() ------------------------------------------------

test_that("distinct genes/positions join in input order with ;", {
  expect_equal(
    pelsa_build_multilabel(c("GENEA", "GENEB"), c(120, 88), c("P1", "P2")),
    "GENEA_aa120;GENEB_aa88"
  )
})

test_that("fully-identical entries collapse to one", {
  expect_equal(
    pelsa_build_multilabel(c("GENEA", "GENEA"), c(120, 120), c("P1", "P2")),
    "GENEA_aa120"
  )
})

test_that("same gene at different positions are kept separate", {
  expect_equal(
    pelsa_build_multilabel(c("GENEA", "GENEA"), c(120, 130), c("P1", "P2")),
    "GENEA_aa120;GENEA_aa130"
  )
})

test_that("empty/NA gene falls back to accession", {
  expect_equal(
    pelsa_build_multilabel(c("", NA), c(50, 60), c("P1", "P2")),
    "P1_aa50;P2_aa60"
  )
})

test_that("a single mapping yields a label with no semicolon", {
  expect_equal(
    pelsa_build_multilabel("GENEA", 120, "P1"),
    "GENEA_aa120"
  )
})

test_that("character positions are coerced for the aa<pos> text", {
  expect_equal(
    pelsa_build_multilabel("GENEA", "120", "P1"),
    "GENEA_aa120"
  )
})

test_that("de-duplication preserves first-occurrence order", {
  # GENEA_aa120 appears first, then GENEB_aa88, then a duplicate GENEA_aa120
  expect_equal(
    pelsa_build_multilabel(
      c("GENEA", "GENEB", "GENEA"),
      c(120, 88, 120),
      c("P1", "P2", "P3")
    ),
    "GENEA_aa120;GENEB_aa88"
  )
})

test_that("empty input returns NA_character_", {
  expect_equal(pelsa_build_multilabel(character(0), integer(0), character(0)),
               NA_character_)
})

test_that("mismatched input lengths error rather than silently recycle", {
  # A scalar position recycled across two genes would emit a wrong label.
  expect_error(
    pelsa_build_multilabel(c("GENEA", "GENEB"), 120, c("P1", "P2"))
  )
  expect_error(
    pelsa_build_multilabel(c("GENEA", "GENEB"), c(120, 88), "P1")
  )
})
