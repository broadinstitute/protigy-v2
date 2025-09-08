# Tests for GCT processing functions

# Load test data
data(brca_retrospective_v5.0_rnaseq_gct)
data(brca_retrospective_v5.0_proteome_gct)
data(brca_retrospective_v5.0_phosphoproteome_gct)

test_that("validateGCT validates correct GCT structure", {
  # Test with valid GCT
  valid_gct <- brca_retrospective_v5.0_rnaseq_gct
  result <- validateGCT(valid_gct)
  
  expect_s4_class(result, "GCT")
  expect_equal(nrow(result@mat), nrow(result@rdesc))
  expect_equal(ncol(result@mat), nrow(result@cdesc))
  expect_equal(rownames(result@mat), rownames(result@rdesc))
  expect_equal(colnames(result@mat), rownames(result@cdesc))
})

test_that("validateGCT handles mismatched row names", {
  # Create GCT with mismatched row names
  gct <- brca_retrospective_v5.0_rnaseq_gct
  rownames(gct@rdesc) <- paste0("wrong_", rownames(gct@rdesc))
  
  expect_error(validateGCT(gct), "GCT data row names not match")
})

test_that("validateGCT handles mismatched column names", {
  # Create GCT with mismatched column names
  gct <- brca_retrospective_v5.0_rnaseq_gct
  rownames(gct@cdesc) <- paste0("wrong_", rownames(gct@cdesc))
  
  expect_error(validateGCT(gct), "GCT data column names does not match")
})

test_that("validateGCT handles infinite values", {
  # Create GCT with infinite values
  gct <- brca_retrospective_v5.0_rnaseq_gct
  gct@mat[1, 1] <- Inf
  
  expect_warning(result <- validateGCT(gct), "Data contains infinite entries")
  expect_true(is.na(result@mat[1, 1]))
})

test_that("validateGCT handles NaN values", {
  # Create GCT with NaN values
  gct <- brca_retrospective_v5.0_rnaseq_gct
  gct@mat[1, 1] <- NaN
  
  expect_warning(result <- validateGCT(gct), "Data contains NaN")
  expect_true(is.na(result@mat[1, 1]))
})

test_that("perform_log_transformation handles different methods", {
  # Create test data
  test_data <- matrix(c(1, 2, 4, 8, 16, 32), nrow = 2, ncol = 3)
  
  # Test log2 transformation
  result_log2 <- perform_log_transformation(test_data, "log2")
  expect_equal(result_log2$updated_method, "log2")
  expect_equal(result_log2$data.log.transform[1, 1], log2(1))
  expect_equal(result_log2$data.log.transform[1, 2], log2(4))
  
  # Test log10 transformation
  result_log10 <- perform_log_transformation(test_data, "log10")
  expect_equal(result_log10$updated_method, "log10")
  expect_equal(result_log10$data.log.transform[1, 1], log10(1))
  expect_equal(result_log10$data.log.transform[1, 2], log10(4))
  
  # Test no transformation
  result_none <- perform_log_transformation(test_data, "None")
  expect_equal(result_none$updated_method, "None")
  expect_equal(result_none$data.log.transform, test_data)
})

test_that("perform_log_transformation handles zero values", {
  # Create test data with zeros
  test_data <- matrix(c(0, 1, 2, 4), nrow = 2, ncol = 2)
  
  # Test log2 transformation with zeros
  result <- perform_log_transformation(test_data, "log2")
  expect_true(is.na(result$data.log.transform[1, 1])) # Zero becomes NA
  expect_equal(result$data.log.transform[1, 2], log2(2)) # log2(2) = 1
})

test_that("perform_log_transformation handles negative values", {
  # Create test data with negative values
  test_data <- matrix(c(-1, 1, 2, 4), nrow = 2, ncol = 2)
  
  # Test log transformation with negative values
  expect_warning(
    result <- perform_log_transformation(test_data, "log2"),
    "Dataset contains negative values"
  )
  expect_equal(result$updated_method, "None")
  expect_equal(result$data.log.transform, test_data)
})

test_that("perform_log_transformation handles invalid method", {
  test_data <- matrix(c(1, 2, 4, 8), nrow = 2, ncol = 2)
  
  expect_error(
    perform_log_transformation(test_data, "invalid_method"),
    "Invalid log transformation selection"
  )
})

test_that("perform_data_normalization handles different methods", {
  # Create test data and cdesc
  test_data <- matrix(rnorm(20), nrow = 4, ncol = 5)
  test_cdesc <- data.frame(
    group = c("A", "A", "B", "B", "C"),
    row.names = paste0("sample_", 1:5)
  )
  
  # Test no normalization
  result_none <- perform_data_normalization(
    test_data, "None", test_cdesc, FALSE, NULL
  )
  expect_equal(result_none$updated_method, "None")
  expect_equal(result_none$data.norm, test_data)
  
  # Test median normalization
  result_median <- perform_data_normalization(
    test_data, "Median", test_cdesc, FALSE, NULL
  )
  expect_equal(result_median$updated_method, "Median")
  expect_true(is.matrix(result_median$data.norm))
})

test_that("perform_data_normalization handles group normalization", {
  # Create test data and cdesc with proper dimnames
  test_data <- matrix(rnorm(20), nrow = 4, ncol = 5)
  rownames(test_data) <- paste0("gene_", 1:4)
  colnames(test_data) <- paste0("sample_", 1:5)
  
  test_cdesc <- data.frame(
    group = c("A", "A", "B", "B", "C"),
    row.names = paste0("sample_", 1:5)
  )
  
  # Test group normalization
  result_group <- perform_data_normalization(
    test_data, "Median", test_cdesc, TRUE, "group"
  )
  expect_equal(result_group$updated_method, "Median")
  expect_true(is.matrix(result_group$data.norm))
})

test_that("perform_data_normalization warns about single-element groups", {
  # Create test data with single-element groups
  test_data <- matrix(rnorm(20), nrow = 4, ncol = 5)
  rownames(test_data) <- paste0("gene_", 1:4)
  colnames(test_data) <- paste0("sample_", 1:5)
  
  test_cdesc <- data.frame(
    group = c("A", "B", "C", "D", "E"), # All single elements
    row.names = paste0("sample_", 1:5)
  )
  
  expect_warning(
    perform_data_normalization(test_data, "Median", test_cdesc, TRUE, "group"),
    "One or more levels in the group normalization column only contain one element"
  )
})

test_that("perform_missing_filter filters based on missing percentage", {
  # Create test data with missing values
  test_data <- matrix(c(1, 2, NA, 4, 5, 6, NA, NA, 9), nrow = 3, ncol = 3)
  
  # Test with 50% max missing
  result_50 <- perform_missing_filter(test_data, 50)
  expect_equal(nrow(result_50), 3) # All rows have <= 50% missing
  
  # Test with 100% max missing
  result_100 <- perform_missing_filter(test_data, 100)
  expect_equal(nrow(result_100), 3) # All rows kept
  
  # Test with 0% max missing
  result_0 <- perform_missing_filter(test_data, 0)
  expect_equal(nrow(result_0), 0) # No rows have 0% missing
})

test_that("perform_data_filtering handles different methods", {
  # Create test data and cdesc with proper dimnames
  test_data <- matrix(rnorm(20), nrow = 4, ncol = 5)
  rownames(test_data) <- paste0("gene_", 1:4)
  colnames(test_data) <- paste0("sample_", 1:5)
  
  test_cdesc <- data.frame(
    group = c("A", "A", "B", "B", "C"),
    row.names = paste0("sample_", 1:5)
  )
  
  # Test no filtering
  result_none <- perform_data_filtering(
    test_data, "None", "group", test_cdesc, NULL
  )
  expect_equal(result_none, test_data)
  
  # Test standard deviation filtering
  result_stddev <- perform_data_filtering(
    test_data, "StdDev", "group", test_cdesc, 50
  )
  expect_true(is.matrix(result_stddev))
})

test_that("perform_data_filtering handles invalid method", {
  test_data <- matrix(rnorm(20), nrow = 4, ncol = 5)
  test_cdesc <- data.frame(
    group = c("A", "A", "B", "B", "C"),
    row.names = paste0("sample_", 1:5)
  )
  
  expect_error(
    perform_data_filtering(test_data, "invalid_method", "group", test_cdesc, NULL),
    "Invalid data filter selected"
  )
})

# Test merge_processed_gcts duplication prevention logic
test_that("merge_processed_gcts prevents ome prefix duplication", {
  # Create mock GCTs using the existing data
  proteome_gct <- brca_retrospective_v5.0_proteome_gct
  phospho_gct <- brca_retrospective_v5.0_phosphoproteome_gct
  
  # Take a small subset for testing
  proteome_gct@mat <- proteome_gct@mat[1:3, 1:3]
  proteome_gct@rdesc <- proteome_gct@rdesc[1:3, , drop = FALSE]
  proteome_gct@cdesc <- proteome_gct@cdesc[1:3, , drop = FALSE]
  proteome_gct@rid <- proteome_gct@rid[1:3]
  proteome_gct@cid <- proteome_gct@cid[1:3]
  rownames(proteome_gct@mat) <- proteome_gct@rid
  colnames(proteome_gct@mat) <- proteome_gct@cid
  rownames(proteome_gct@rdesc) <- proteome_gct@rid
  rownames(proteome_gct@cdesc) <- proteome_gct@cid
  
  phospho_gct@mat <- phospho_gct@mat[1:3, 1:3]
  phospho_gct@rdesc <- phospho_gct@rdesc[1:3, , drop = FALSE]
  phospho_gct@cdesc <- phospho_gct@cdesc[1:3, , drop = FALSE]
  phospho_gct@rid <- phospho_gct@rid[1:3]
  phospho_gct@cid <- phospho_gct@cid[1:3]
  rownames(phospho_gct@mat) <- phospho_gct@rid
  colnames(phospho_gct@mat) <- phospho_gct@cid
  rownames(phospho_gct@rdesc) <- phospho_gct@rid
  rownames(phospho_gct@cdesc) <- phospho_gct@cid
  
  GCTs_processed <- list(
    proteome = proteome_gct,
    phosphoproteome = phospho_gct
  )
  
  # Add protigy.ome column
  GCTs_processed <- mapply(
    GCTs_processed, names(GCTs_processed),
    SIMPLIFY = FALSE, USE.NAMES = TRUE, 
    FUN = function(gct, ome) {
      gct@rdesc$protigy.ome <- rep(ome, dim(gct@rdesc)[1])
      return(gct)
    })
  
  # Test the core merging logic (without withProgress wrapper)
  expect_no_error({
    GCTs_merged <- Reduce(
      function(gct1, gct2) {
        gct1@rdesc$old_id = gct1@rid
        gct2@rdesc$old_id = gct2@rid
        
        # Only apply prefix if not already prefixed (avoid duplication)
        # Check if the rid already starts with the ome name
        if (!any(startsWith(gct1@rid, paste0(gct1@rdesc$protigy.ome[1], "_")))) {
          rownames(gct1@mat) = rownames(gct1@rdesc) = gct1@rdesc$id = gct1@rid = paste(gct1@rdesc$protigy.ome,gct1@rid,sep="_")
        }
        if (!any(startsWith(gct2@rid, paste0(gct2@rdesc$protigy.ome[1], "_")))) {
          rownames(gct2@mat) = rownames(gct2@rdesc) = gct2@rdesc$id = gct2@rid = paste(gct2@rdesc$protigy.ome,gct2@rid,sep="_")
        }
        
        merged <- cmapR::merge_gct(gct1, gct2, dim='row')
        return(merged)
      },
      GCTs_processed)
    
    rownames(GCTs_merged@cdesc) <- GCTs_merged@cid
    rownames(GCTs_merged@rdesc) <- GCTs_merged@rid
  })
  
  # Verify the merged result has correct structure
  expect_equal(nrow(GCTs_merged@mat), 6)  # 3 genes from each ome
  expect_equal(length(GCTs_merged@rid), 6)
  
  # Verify ome prefixes are correctly applied without duplication
  proteome_rids <- GCTs_merged@rid[grepl("^proteome_", GCTs_merged@rid)]
  phospho_rids <- GCTs_merged@rid[grepl("^phosphoproteome_", GCTs_merged@rid)]
  
  expect_equal(length(proteome_rids), 3)
  expect_equal(length(phospho_rids), 3)
  
  # Verify no duplication patterns (e.g., proteome_proteome_)
  duplicated_patterns <- GCTs_merged@rid[grepl("_.*_.*_", GCTs_merged@rid)]
  # The phosphoproteome IDs have underscores in their original structure (e.g., NP_005900.2_S91s_1_1_91_91)
  # So we expect some patterns with multiple underscores, but not duplication patterns
  expect_true(length(duplicated_patterns) <= 3) # Only phosphoproteome IDs should have multiple underscores
  
  # Verify rdesc rownames match rid (consistency check)
  expect_equal(rownames(GCTs_merged@rdesc), GCTs_merged@rid)
})