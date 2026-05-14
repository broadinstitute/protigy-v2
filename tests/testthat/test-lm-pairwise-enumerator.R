################################################################################
# Phase 4: "Suggest all pairwise contrasts" enumerator.
################################################################################

library(testthat)


test_that("enumerate_pairwise_simple_rows yields combn pairs in canonical order", {
  rows <- enumerate_pairwise_simple_rows(c("A", "B", "C"), "treatment")
  expect_length(rows, 3)
  pairs <- vapply(rows, function(r) paste(r$num, r$den, sep = "|"), character(1))
  expect_equal(pairs, c("treatmentA|treatmentB", "treatmentA|treatmentC",
                        "treatmentB|treatmentC"))
  # All Simple-type with non-empty auto labels.
  expect_true(all(vapply(rows, function(r) r$type == "simple", logical(1))))
  expect_true(all(vapply(rows, function(r) nzchar(r$label), logical(1))))
  # No two rows share an id.
  ids <- vapply(rows, function(r) r$id, character(1))
  expect_equal(length(unique(ids)), length(ids))
})


test_that("enumerate_pairwise_simple_rows respects intercept toggle (cell-means)", {
  rows <- enumerate_pairwise_simple_rows(c("KO", "WT"), "group",
                                          include_intercept = FALSE)
  expect_length(rows, 1)
  expect_equal(rows[[1]]$num, "KO")
  expect_equal(rows[[1]]$den, "WT")
})


test_that("enumerate_pairwise_simple_rows returns empty for invalid inputs", {
  expect_equal(enumerate_pairwise_simple_rows(NULL, "x"), list())
  expect_equal(enumerate_pairwise_simple_rows(character(0), "x"), list())
  expect_equal(enumerate_pairwise_simple_rows("only_one", "x"), list())
  expect_equal(enumerate_pairwise_simple_rows(c("A", "B"), ""), list())
  expect_equal(enumerate_pairwise_simple_rows(c("A", "B"), NULL), list())
})


test_that("enumerated rows survive a roundtrip through build_simple_expr", {
  rows <- enumerate_pairwise_simple_rows(c("Drug", "Vehicle"), "treatment")
  expr <- build_simple_expr(rows[[1]]$num, rows[[1]]$den)
  expect_equal(expr, "treatmentDrug - treatmentVehicle")
})
