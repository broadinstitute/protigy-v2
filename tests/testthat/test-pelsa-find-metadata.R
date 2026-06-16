library(testthat)
source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))

# Build a small volcano-df-shaped frame directly (find_mask/metadata read only
# id / winning_accession / PG.ProteinAccessions / winning_gene / PG.Genes /
# pep_start / pep_end / logFC / adj.P.Val).
.find_df <- function() {
  data.frame(
    id                   = c("PEP1", "PEP2", "PEP3", "ISOPEPTIDEK"),
    winning_accession    = c("P12345", "P12345", "Q99999", "P12345-2"),
    PG.ProteinAccessions = c("P12345", "P12345;EXTRA", "Q99999", "P12345-2"),
    winning_gene         = c("GA", "GA", "GB", ""),
    PG.Genes             = c("GA", "GA", "GB", ""),
    pep_start            = c(7L, 40L, 5L, 7L),
    pep_end              = c(17L, 50L, 15L, 17L),
    logFC                = c(1.1, -0.5, 2.0, 0.3),
    adj.P.Val            = c(0.01, 0.20, 0.001, 0.50),
    stringsAsFactors     = FALSE, check.names = FALSE
  )
}

test_that("find_mask: exact winning_accession match (single accession)", {
  df <- .find_df()
  out <- pelsa_volcano_find_mask(df, "Q99999")
  expect_equal(which(out$mask), 3L)
  expect_equal(out$accessions, "Q99999")
  expect_equal(out$count, 1L)
})

test_that("find_mask: case-insensitive + trims", {
  df <- .find_df()
  out <- pelsa_volcano_find_mask(df, "  q99999 ")
  expect_equal(out$count, 1L)
})

test_that("find_mask: isoform base P12345 also matches P12345-2", {
  df <- .find_df()
  out <- pelsa_volcano_find_mask(df, "P12345")
  expect_setequal(which(out$mask), c(1L, 2L, 4L))
  expect_equal(out$count, 3L)
})

test_that("find_mask: PG.ProteinAccessions token match (EXTRA)", {
  df <- .find_df()
  out <- pelsa_volcano_find_mask(df, "EXTRA")
  expect_equal(which(out$mask), 2L)
})

test_that("find_mask: no match -> empty mask, count 0", {
  df <- .find_df()
  out <- pelsa_volcano_find_mask(df, "NOPE")
  expect_equal(out$count, 0L)
  expect_false(any(out$mask))
})

test_that("find_mask: empty/NA input -> count 0", {
  df <- .find_df()
  expect_equal(pelsa_volcano_find_mask(df, "")$count, 0L)
  expect_equal(pelsa_volcano_find_mask(df, NA)$count, 0L)
})

test_that("metadata_rows: 2-col (label,value) df with the panel fields", {
  df <- .find_df()
  rows <- pelsa_pin_metadata_rows(df, row = 1L, n_peptides = 2L)
  expect_s3_class(rows, "data.frame")
  expect_named(rows, c("label", "value"))
  lv <- setNames(rows$value, rows$label)
  expect_equal(lv[["Peptide"]], "GA_aa7")
  expect_equal(lv[["Accession"]], "P12345")
  expect_equal(lv[["Gene"]], "GA")
  expect_equal(lv[["Quantified peptides (this contrast)"]], "2")
  expect_equal(lv[["Sequence"]], "PEP1")
  expect_equal(lv[["Position"]], "7-17")
  expect_match(lv[["adj.P"]], "0.01")
  expect_match(lv[["logFC"]], "1.1")
  # Sequence coverage row sits between Accession and Gene; NA by default.
  expect_equal(lv[["Sequence coverage"]], "NA")
  expect_equal(which(rows$label == "Sequence coverage"), 3L)
  expect_equal(which(rows$label == "Gene"), 4L)
})

test_that("metadata_rows: coverage_frac renders as a 1-decimal percent", {
  df <- .find_df()
  rows <- pelsa_pin_metadata_rows(df, row = 1L, n_peptides = 2L,
                                  coverage_frac = 0.4237)
  lv <- setNames(rows$value, rows$label)
  expect_equal(lv[["Sequence coverage"]], "42.4%")
  # NA / malformed coverage falls back to "NA".
  expect_equal(setNames(
    pelsa_pin_metadata_rows(df, 1L, 2L, coverage_frac = NA_real_)$value,
    pelsa_pin_metadata_rows(df, 1L, 2L)$label)[["Sequence coverage"]], "NA")
})

test_that("metadata_rows: empty gene -> accession fallback label, Gene = NA", {
  df <- .find_df()
  rows <- pelsa_pin_metadata_rows(df, row = 4L, n_peptides = 3L)
  lv <- setNames(rows$value, rows$label)
  expect_equal(lv[["Peptide"]], "P12345-2_aa7")  # gene blank -> accession
  expect_equal(lv[["Gene"]], "NA")
})
