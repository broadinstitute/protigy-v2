################################################################################
# Tests for summarize_sample_drops() - pure helper backing the "Using N of M
# samples..." caption above the design-matrix preview.
#
# Given the working cdesc, the variables that feed `complete.cases`, and the
# total cdesc row count, the helper reports:
#  - n_total: input rowcount
#  - n_used:  rowcount after complete.cases over the model + blocking vars
#  - n_dropped
#  - dropped_columns: character vector of variable names whose NAs caused
#     drops, sorted by descending NA count then alphabetical
#  - message: a one-line human-readable string used as the caption
################################################################################

library(testthat)


test_that("returns NULL on empty input", {
  expect_null(summarize_sample_drops(NULL, character(0)))
  expect_null(summarize_sample_drops(data.frame(), character(0)))
  expect_null(summarize_sample_drops(data.frame(a = 1:3), character(0)))
})


test_that("reports zero drops when every model var is complete", {
  cd <- data.frame(
    Treatment = c("Drug", "Drug", "Vehicle", "Vehicle"),
    Age = c(40, 50, 60, 70),
    stringsAsFactors = FALSE
  )
  res <- summarize_sample_drops(cd, c("Treatment", "Age"))
  expect_identical(res$n_total, 4L)
  expect_identical(res$n_used, 4L)
  expect_identical(res$n_dropped, 0L)
  expect_identical(res$dropped_columns, character(0))
  expect_match(res$message, "all 4")
})


test_that("attributes drops to the single offending column", {
  cd <- data.frame(
    Treatment = c("Drug", "Drug", "Vehicle", "Vehicle", "Drug"),
    Age = c(40, NA, 60, 70, NA),
    stringsAsFactors = FALSE
  )
  res <- summarize_sample_drops(cd, c("Treatment", "Age"))
  expect_identical(res$n_total, 5L)
  expect_identical(res$n_used, 3L)
  expect_identical(res$n_dropped, 2L)
  expect_identical(res$dropped_columns, "Age")
  expect_match(res$message, "Using 3 of 5")
  expect_match(res$message, "Age", fixed = TRUE)
})


test_that("reports multiple offending columns sorted by NA count (desc) then alpha", {
  cd <- data.frame(
    Treatment = c("Drug", NA, "Drug", "Vehicle", "Vehicle"),
    Age       = c(40, 50, NA, NA, 70),
    Batch     = c("A", "A", "B", "B", "A"),
    stringsAsFactors = FALSE
  )
  res <- summarize_sample_drops(cd, c("Treatment", "Age", "Batch"))
  expect_identical(res$n_used, 2L)
  expect_identical(res$n_dropped, 3L)
  # Age has 2 NAs, Treatment has 1; Batch has 0 -> dropped from list.
  expect_identical(res$dropped_columns, c("Age", "Treatment"))
})


test_that("ties on NA count break alphabetically", {
  cd <- data.frame(
    BetaVar = c("a", NA, "a", "b"),
    AlphaVar = c(NA, "x", "y", "z"),
    stringsAsFactors = FALSE
  )
  res <- summarize_sample_drops(cd, c("BetaVar", "AlphaVar"))
  expect_identical(res$dropped_columns, c("AlphaVar", "BetaVar"))
})


test_that("ignores requested columns that don't exist in cdesc", {
  cd <- data.frame(
    Treatment = c("Drug", "Vehicle"),
    stringsAsFactors = FALSE
  )
  # NotAColumn is silently skipped.
  res <- summarize_sample_drops(cd, c("Treatment", "NotAColumn"))
  expect_identical(res$n_used, 2L)
  expect_identical(res$n_dropped, 0L)
})


test_that("handles blocking variable as just another column to inspect", {
  cd <- data.frame(
    Treatment = c("Drug", "Drug", "Vehicle"),
    Patient = c("P1", NA, "P3"),
    stringsAsFactors = FALSE
  )
  res <- summarize_sample_drops(cd, c("Treatment", "Patient"))
  expect_identical(res$n_used, 2L)
  expect_identical(res$dropped_columns, "Patient")
})


test_that("dedupes the variable list", {
  cd <- data.frame(
    Treatment = c("Drug", NA, "Vehicle"),
    stringsAsFactors = FALSE
  )
  res <- summarize_sample_drops(cd, c("Treatment", "Treatment"))
  expect_identical(res$n_used, 2L)
})


test_that("message lists all dropped columns when there are several", {
  cd <- data.frame(
    A = c(1, NA, 3, 4),
    B = c("x", "y", NA, "z"),
    stringsAsFactors = FALSE
  )
  res <- summarize_sample_drops(cd, c("A", "B"))
  expect_match(res$message, "Using 2 of 4")
  expect_match(res$message, "dropped")
  # Both columns named in the message.
  expect_match(res$message, "A", fixed = TRUE)
  expect_match(res$message, "B", fixed = TRUE)
})
