################################################################################
# Unit Tests for Analysis Annotation Column Filtering
#
# Tests the logic that filters out ID columns (unique character data) from
# the Analysis Annotation Column dropdown choices
################################################################################

# Load required packages
library(testthat)
library(Protigy)

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

test_that("annotation column filtering excludes unique character columns", {
  # Create mock GCT object with cdesc containing mixed column types
  mock_cdesc <- data.frame(
    Sample.ID = c("S1", "S2", "S3"),             # unique character - should be excluded
    patient_id = c("P1", "P2", "P3"),            # unique character - should be excluded
    treatment = c("A", "A", "B"),                # non-unique character - should be included
    timepoint = c("T1", "T2", "T1"),             # non-unique character - should be included
    stringsAsFactors = FALSE
  )
  
  # Create mock GCT object
  mock_gct <- new("GCT")
  mock_gct@cdesc <- mock_cdesc
  
  # Mock the gctSetupUI function logic
  all_cdesc_columns <- names(mock_gct@cdesc)
  unique_columns <- getUniqueColumns(mock_gct@cdesc)
  groups_choices <- all_cdesc_columns[!all_cdesc_columns %in% unique_columns]
  
  # Should exclude unique character columns
  expect_false("Sample.ID" %in% groups_choices)
  expect_false("patient_id" %in% groups_choices)
  
  # Should include non-unique character columns
  expect_true("treatment" %in% groups_choices)
  expect_true("timepoint" %in% groups_choices)
})

test_that("annotation column filtering provides fallback when no suitable columns", {
  # Create mock GCT object with only unique character columns
  mock_cdesc <- data.frame(
    Sample.ID = c("S1", "S2", "S3"),             # unique character
    patient_id = c("P1", "P2", "P3"),            # unique character
    stringsAsFactors = FALSE
  )
  
  # Create mock GCT object
  mock_gct <- new("GCT")
  mock_gct@cdesc <- mock_cdesc
  
  # Mock the gctSetupUI function logic
  all_cdesc_columns <- names(mock_gct@cdesc)
  unique_columns <- getUniqueColumns(mock_gct@cdesc)
  groups_choices <- all_cdesc_columns[!all_cdesc_columns %in% unique_columns]
  
  # If no suitable annotation columns remain, use Sample.ID as fallback
  if (length(groups_choices) == 0) {
    groups_choices <- "Sample.ID"
  }
  
  # Should fall back to Sample.ID
  expect_equal(groups_choices, "Sample.ID")
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
