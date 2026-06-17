# Tests for data filtering functions

test_that("sd.filter works with basic input", {
  # Create test data
  test_tab <- data.frame(
    id = paste0("gene_", 1:5),
    sample1 = c(1, 2, 3, 4, 5),
    sample2 = c(2, 3, 4, 5, 6),
    sample3 = c(3, 4, 5, 6, 7)
  )
  
  test_grp_vec <- c("group1", "group1", "group2")
  names(test_grp_vec) <- c("sample1", "sample2", "sample3")
  
  result <- sd.filter(test_tab, test_grp_vec, "id", 50)
  
  expect_type(result, "list")
  expect_named(result, c("table", "values.filtered", "sd.perc.val"))
  expect_equal(nrow(result$table), 5)
  expect_equal(ncol(result$table), 4) # id + 3 samples
  expect_equal(result$table$id, test_tab$id)
})

test_that("sd.filter NAs more rows at a higher percentile cutoff (real effect, not nrow)", {
  # sd.filter sets rows whose SD falls below the percentile threshold to NA —
  # it NEVER drops rows, so nrow is always 4 regardless of the cutoff.
  # Asserting nrow(result_25) <= nrow(result_75) was therefore always TRUE (P2.5).
  #
  # Fix: assert on the COUNT OF NA'd ROWS inside each group (values.filtered) and
  # confirm that a higher percentile NAs more rows with known SDs.
  #
  # Fixture design (single group for simplicity):
  #   gene_1: row SD = 0   (all zeros  -> constant)
  #   gene_2: row SD = 1   (1,2,3)
  #   gene_3: row SD = 5   (1,6,11)
  #   gene_4: row SD = 10  (1,11,21)
  #   gene_5: row SD = 50  (1,51,101)
  #
  # SDs in ascending order: 0, 1, 5, 10, 50.
  # 20th percentile threshold = 0.8  -> gene_1 (SD=0) is below        -> 1 row NA'd.
  # 60th percentile threshold = 7    -> gene_1..3 (SD 0,1,5) are below -> 3 rows NA'd.
  # 80th percentile threshold = 18   -> gene_1..4 (SD 0,1,5,10) below  -> 4 rows NA'd.

  test_tab <- data.frame(
    id      = paste0("gene_", 1:5),
    sample1 = c(0,  1,  1,  1,   1),
    sample2 = c(0,  2,  6, 11,  51),
    sample3 = c(0,  3, 11, 21, 101)
  )

  grp_vec <- c("g", "g", "g")
  names(grp_vec) <- c("sample1", "sample2", "sample3")

  result_20 <- sd.filter(test_tab, grp_vec, "id", 20)
  result_60 <- sd.filter(test_tab, grp_vec, "id", 60)
  result_80 <- sd.filter(test_tab, grp_vec, "id", 80)

  # All results still have 5 rows (sd.filter never drops rows).
  expect_equal(nrow(result_20$table), 5L)
  expect_equal(nrow(result_60$table), 5L)
  expect_equal(nrow(result_80$table), 5L)

  # Count NA'd rows per result: a row is NA'd when all its sample columns are NA.
  count_na_rows <- function(res) {
    mat_cols <- setdiff(names(res$table), "id")
    sum(apply(res$table[, mat_cols, drop = FALSE], 1, function(x) all(is.na(x))))
  }

  na_20 <- count_na_rows(result_20)
  na_60 <- count_na_rows(result_60)
  na_80 <- count_na_rows(result_80)

  # A higher percentile cutoff means a higher SD threshold -> more rows are NA'd.
  expect_true(na_20 < na_60,
    info = sprintf("20th pct NA'd %d rows, 60th pct NA'd %d rows; expected 20th < 60th", na_20, na_60))
  expect_true(na_60 < na_80,
    info = sprintf("60th pct NA'd %d rows, 80th pct NA'd %d rows; expected 60th < 80th", na_60, na_80))

  # Spot-check values.filtered: it stores the indices of NA'd rows per group.
  # At the 60th percentile, genes 1, 2, and 3 (SDs 0, 1, 5 < threshold 7) are filtered.
  expect_equal(length(result_60$values.filtered[["g"]]), 3L,
    info = paste("expected 3 filtered indices at 60th pct, got",
                  length(result_60$values.filtered[["g"]])))
})

test_that("sd.filter handles missing values", {
  # Create test data with missing values
  test_tab <- data.frame(
    id = paste0("gene_", 1:3),
    sample1 = c(1, NA, 3),
    sample2 = c(2, 3, NA),
    sample3 = c(3, 4, 5)
  )
  
  test_grp_vec <- c("group1", "group1", "group2")
  names(test_grp_vec) <- c("sample1", "sample2", "sample3")
  
  result <- sd.filter(test_tab, test_grp_vec, "id", 50)
  
  expect_type(result, "list")
  expect_equal(nrow(result$table), 3)
})

test_that("sd.filter handles single group", {
  # Create test data with single group
  test_tab <- data.frame(
    id = paste0("gene_", 1:3),
    sample1 = c(1, 2, 3),
    sample2 = c(2, 3, 4),
    sample3 = c(3, 4, 5)
  )
  
  test_grp_vec <- c("group1", "group1", "group1")
  names(test_grp_vec) <- c("sample1", "sample2", "sample3")
  
  result <- sd.filter(test_tab, test_grp_vec, "id", 50)
  
  expect_type(result, "list")
  expect_equal(nrow(result$table), 3)
})

test_that("sd.filter handles edge cases", {
  # Test with single gene
  test_tab_single <- data.frame(
    id = "gene_1",
    sample1 = 1,
    sample2 = 2,
    sample3 = 3
  )
  
  test_grp_vec <- c("group1", "group1", "group2")
  names(test_grp_vec) <- c("sample1", "sample2", "sample3")
  
  result_single <- sd.filter(test_tab_single, test_grp_vec, "id", 50)
  expect_equal(nrow(result_single$table), 1)
  
  # Test with identical values (zero variance)
  test_tab_identical <- data.frame(
    id = paste0("gene_", 1:3),
    sample1 = c(1, 1, 1),
    sample2 = c(1, 1, 1),
    sample3 = c(1, 1, 1)
  )
  
  result_identical <- sd.filter(test_tab_identical, test_grp_vec, "id", 50)
  expect_equal(nrow(result_identical$table), 3)
})

test_that("sd.filter returns correct structure", {
  test_tab <- data.frame(
    id = paste0("gene_", 1:3),
    sample1 = c(1, 2, 3),
    sample2 = c(2, 3, 4),
    sample3 = c(3, 4, 5)
  )
  
  test_grp_vec <- c("group1", "group1", "group2")
  names(test_grp_vec) <- c("sample1", "sample2", "sample3")
  
  result <- sd.filter(test_tab, test_grp_vec, "id", 50)
  
  # Check table structure
  expect_equal(colnames(result$table)[1], "id")
  expect_equal(colnames(result$table)[2:4], names(test_grp_vec))
  
  # Check values.filtered structure
  expect_type(result$values.filtered, "list")
  expect_named(result$values.filtered, unique(test_grp_vec))
  
  # Check sd.perc.val
  expect_type(result$sd.perc.val, "double")
  expect_length(result$sd.perc.val, 1)
})

test_that("sd.filter handles different percentile values", {
  test_tab <- data.frame(
    id = paste0("gene_", 1:5),
    sample1 = c(1, 2, 3, 4, 5),
    sample2 = c(2, 3, 4, 5, 6),
    sample3 = c(3, 4, 5, 6, 7)
  )
  
  test_grp_vec <- c("group1", "group1", "group2")
  names(test_grp_vec) <- c("sample1", "sample2", "sample3")
  
  # Test different percentiles
  result_10 <- sd.filter(test_tab, test_grp_vec, "id", 10)
  result_50 <- sd.filter(test_tab, test_grp_vec, "id", 50)
  result_90 <- sd.filter(test_tab, test_grp_vec, "id", 90)
  
  # Lower percentile should filter more (higher threshold)
  expect_true(result_10$sd.perc.val >= result_50$sd.perc.val)
  expect_true(result_50$sd.perc.val >= result_90$sd.perc.val)
})
