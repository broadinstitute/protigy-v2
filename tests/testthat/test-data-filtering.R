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

test_that("sd.filter filters based on standard deviation percentile", {
  # Create test data with varying standard deviations
  test_tab <- data.frame(
    id = paste0("gene_", 1:4),
    sample1 = c(1, 1, 1, 10),    # Low variance
    sample2 = c(2, 2, 2, 20),    # Low variance  
    sample3 = c(3, 3, 3, 30)     # Low variance
  )
  
  test_grp_vec <- c("group1", "group1", "group2")
  names(test_grp_vec) <- c("sample1", "sample2", "sample3")
  
  # Test with 25th percentile (should filter out low variance genes)
  result_25 <- sd.filter(test_tab, test_grp_vec, "id", 25)
  
  # Test with 75th percentile (should keep more genes)
  result_75 <- sd.filter(test_tab, test_grp_vec, "id", 75)
  
  expect_true(nrow(result_25$table) <= nrow(result_75$table))
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
