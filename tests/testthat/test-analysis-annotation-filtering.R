################################################################################
# Unit Tests for Analysis Annotation Column Filtering
#
# Tests the logic that filters out ID columns (unique character data) from
# the Analysis Annotation Column dropdown choices
################################################################################

# Load required packages
library(testthat)
library(Protigy)

# Access non-exported helpers explicitly to keep this file
# runnable in both package-check and direct test_dir contexts.
getUniqueColumns <- Protigy:::getUniqueColumns
is.discrete <- Protigy:::is.discrete

# Load test data
data("brca_retrospective_v5.0_proteome_gct")
data("brca_retrospective_v5.0_phosphoproteome_gct")

################################################################################
# Test getUniqueColumns Function
################################################################################

test_that("getUniqueColumns identifies unique character columns", {
  # Create test data with mixed column types
  test_data <- data.frame(
    id = c("P1", "P2", "P3"),                    # unique character - should be identified
    gene_symbol = c("G1", "G2", "G3"),          # unique character - should be identified
    treatment = c("A", "A", "B"),               # non-unique character - should NOT be identified
    timepoint = c("T1", "T2", "T1"),            # non-unique character - should NOT be identified
    intensity = c(1.5, 2.3, 1.8),               # numeric - should NOT be identified
    stringsAsFactors = FALSE
  )
  
  unique_cols <- getUniqueColumns(test_data)
  
  # Should identify id and gene_symbol as unique character columns
  expect_equal(sort(unique_cols), c("gene_symbol", "id"))
  expect_false("treatment" %in% unique_cols)
  expect_false("timepoint" %in% unique_cols)
  expect_false("intensity" %in% unique_cols)
})

test_that("getUniqueColumns handles NA values correctly", {
  # Create test data with NA values
  test_data <- data.frame(
    id = c("P1", "P2", NA),                     # unique non-NA values
    gene_symbol = c("G1", NA, "G3"),            # unique non-NA values
    treatment = c("A", "A", NA),                 # non-unique non-NA values
    stringsAsFactors = FALSE
  )
  
  unique_cols <- getUniqueColumns(test_data)
  
  # Should identify id and gene_symbol as unique (ignoring NAs)
  expect_equal(sort(unique_cols), c("gene_symbol", "id"))
  expect_false("treatment" %in% unique_cols)
})

test_that("getUniqueColumns handles empty data", {
  # Empty data frame
  empty_data <- data.frame()
  unique_cols <- getUniqueColumns(empty_data)
  expect_equal(length(unique_cols), 0)
  
  # Data frame with all NA values
  na_data <- data.frame(
    col1 = c(NA, NA, NA),
    col2 = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )
  unique_cols <- getUniqueColumns(na_data)
  expect_equal(length(unique_cols), 0)
})

test_that("getUniqueColumns excludes numeric columns", {
  # Create test data with numeric columns that happen to be unique
  test_data <- data.frame(
    id = c("P1", "P2", "P3"),                    # unique character
    numeric_id = c(1, 2, 3),                     # unique numeric - should NOT be identified
    decimal_id = c(1.1, 2.2, 3.3),               # unique numeric - should NOT be identified
    treatment = c("A", "A", "B"),                # non-unique character
    stringsAsFactors = FALSE
  )
  
  unique_cols <- getUniqueColumns(test_data)
  
  # Should only identify character columns, not numeric ones
  expect_equal(unique_cols, "id")
  expect_false("numeric_id" %in% unique_cols)
  expect_false("decimal_id" %in% unique_cols)
  expect_false("treatment" %in% unique_cols)
})

################################################################################
# Test Analysis Annotation Column Filtering Logic
################################################################################

test_that("annotation column filtering includes all discrete columns (including ID columns)", {
  # Create mock GCT object with cdesc containing mixed column types
  # Need 25 samples for the continuous column to have >20 unique values
  n_samples <- 25
  mock_cdesc <- data.frame(
    Sample.ID = paste0("S", 1:n_samples),             # unique character (ID column) - should be included
    patient_id = paste0("P", 1:n_samples),            # unique character (ID column) - should be included
    treatment = rep(c("A", "B"), length.out = n_samples),                # non-unique character - should be included
    timepoint = rep(c("T1", "T2", "T1"), length.out = n_samples),             # non-unique character - should be included
    single_category = rep("X", n_samples),          # single category - should be included
    continuous = (1:n_samples) + (1:n_samples) * 0.1,  # continuous numeric with 25 unique values - should be excluded
    stringsAsFactors = FALSE
  )
  
  # Create mock GCT object
  mock_gct <- new("GCT")
  mock_gct@cdesc <- mock_cdesc
  
  # Mock the gctSetupUI function logic (updated to include all discrete columns)
  all_cdesc_columns <- names(mock_gct@cdesc)
  # Filter to only discrete columns (exclude continuous columns)
  groups_choices <- all_cdesc_columns[vapply(mock_gct@cdesc[all_cdesc_columns], function(col) is.discrete(col), logical(1))]
  
  # Should include all discrete columns, including ID columns
  expect_true("Sample.ID" %in% groups_choices)
  expect_true("patient_id" %in% groups_choices)
  expect_true("treatment" %in% groups_choices)
  expect_true("timepoint" %in% groups_choices)
  expect_true("single_category" %in% groups_choices)
  
  # Should exclude continuous columns (is.discrete returns FALSE for numeric columns with many unique values)
  expect_false("continuous" %in% groups_choices)
})

test_that("annotation column filtering includes ID columns when only ID columns available", {
  # Create mock GCT object with only unique character columns (ID columns)
  mock_cdesc <- data.frame(
    Sample.ID = c("S1", "S2", "S3"),             # unique character (ID column)
    patient_id = c("P1", "P2", "P3"),            # unique character (ID column)
    stringsAsFactors = FALSE
  )
  
  # Create mock GCT object
  mock_gct <- new("GCT")
  mock_gct@cdesc <- mock_cdesc
  
  # Mock the gctSetupUI function logic (updated to include all discrete columns)
  all_cdesc_columns <- names(mock_gct@cdesc)
  groups_choices <- all_cdesc_columns[vapply(mock_gct@cdesc[all_cdesc_columns], function(col) is.discrete(col), logical(1))]
  
  # Should include ID columns (they are discrete)
  expect_true("Sample.ID" %in% groups_choices)
  expect_true("patient_id" %in% groups_choices)
  expect_true(length(groups_choices) >= 2)
})

test_that("annotation column filtering provides fallback when no discrete columns", {
  # Create mock GCT object with only continuous columns (many unique numeric values)
  # Need enough unique values to trigger is.discrete() to return FALSE
  mock_cdesc <- data.frame(
    continuous1 = c(1.5, 2.3, 1.8, 4.1, 5.2, 6.3, 7.4, 8.5, 9.6, 10.7, 11.8, 12.9, 13.0, 14.1, 15.2, 16.3, 17.4, 18.5, 19.6, 20.7, 21.8),  # >20 unique values
    continuous2 = c(10.1, 20.2, 30.3, 40.4, 50.5, 60.6, 70.7, 80.8, 90.9, 100.0, 110.1, 120.2, 130.3, 140.4, 150.5, 160.6, 170.7, 180.8, 190.9, 200.0, 210.1),  # >20 unique values
    stringsAsFactors = FALSE
  )
  
  # Create mock GCT object
  mock_gct <- new("GCT")
  mock_gct@cdesc <- mock_cdesc
  
  # Mock the gctSetupUI function logic
  all_cdesc_columns <- names(mock_gct@cdesc)
  groups_choices <- all_cdesc_columns[vapply(mock_gct@cdesc[all_cdesc_columns], function(col) is.discrete(col), logical(1))]
  
  # If no suitable annotation columns remain, use Sample.ID as fallback
  if (length(groups_choices) == 0) {
    groups_choices <- "Sample.ID"
  }
  
  # Should fall back to Sample.ID
  expect_equal(groups_choices, "Sample.ID")
})

test_that("annotation column filtering includes columns with single category", {
  # Create mock GCT object with column having only one category
  mock_cdesc <- data.frame(
    single_category = c("A", "A", "A"),          # single category - should be included
    two_categories = c("A", "B", "A"),           # two categories - should be included
    treatment = c("A", "A", "B"),                # two categories - should be included
    stringsAsFactors = FALSE
  )
  
  # Create mock GCT object
  mock_gct <- new("GCT")
  mock_gct@cdesc <- mock_cdesc
  
  # Mock the gctSetupUI function logic
  all_cdesc_columns <- names(mock_gct@cdesc)
  groups_choices <- all_cdesc_columns[vapply(mock_gct@cdesc[all_cdesc_columns], function(col) is.discrete(col), logical(1))]
  
  # Should include all discrete columns, including those with single category
  expect_true("single_category" %in% groups_choices)
  expect_true("two_categories" %in% groups_choices)
  expect_true("treatment" %in% groups_choices)
})

################################################################################
# Test Edge Cases
################################################################################

test_that("getUniqueColumns handles single row data", {
  # Single row data
  single_row <- data.frame(
    id = "P1",
    treatment = "A",
    stringsAsFactors = FALSE
  )
  
  unique_cols <- getUniqueColumns(single_row)
  
  # Single values should be considered unique (both columns have only one value)
  expect_equal(sort(unique_cols), c("id", "treatment"))
})

test_that("getUniqueColumns handles duplicate values", {
  # Data with duplicate values
  test_data <- data.frame(
    id = c("P1", "P1", "P2"),                    # has duplicates - should NOT be identified
    treatment = c("A", "A", "B"),                # has duplicates - should NOT be identified
    stringsAsFactors = FALSE
  )
  
  unique_cols <- getUniqueColumns(test_data)
  
  # No columns should be identified as unique
  expect_equal(length(unique_cols), 0)
})

test_that("getUniqueColumns handles mixed data types", {
  # Data with mixed character/numeric types
  test_data <- data.frame(
    id = c("P1", "P2", "P3"),                    # character
    numeric_col = c(1, 2, 3),                     # numeric
    mixed_col = c("A", 1, "B"),                  # mixed - should be character
    stringsAsFactors = FALSE
  )
  
  unique_cols <- getUniqueColumns(test_data)
  
  # Should identify character columns only
  expect_equal(sort(unique_cols), c("id", "mixed_col"))
  expect_false("numeric_col" %in% unique_cols)
})
