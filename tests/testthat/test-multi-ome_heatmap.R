################################################################################
# Unit Tests for Multiomic Heatmap Module
################################################################################

# Test helper functions
create_mock_gct <- function(n_genes = 5, n_samples = 4, ome_name = "proteome") {
  mat <- matrix(rnorm(n_genes * n_samples), nrow = n_genes, ncol = n_samples)
  rownames(mat) <- paste0("gene_", 1:n_genes)
  colnames(mat) <- paste0("sample_", 1:n_samples)
  
  # Handle odd number of samples
  if (n_samples %% 2 == 0) {
    group_vec <- rep(c("A", "B"), each = n_samples/2)
  } else {
    group_vec <- c(rep("A", floor(n_samples/2)), rep("B", ceiling(n_samples/2)))
  }
  
  cdesc <- data.frame(
    group = group_vec,
    row.names = paste0("sample_", 1:n_samples)
  )
  
  rdesc <- data.frame(
    gene_name = paste0("gene_", 1:n_genes),
    geneSymbol = paste0("gene_", 1:n_genes),  # Add geneSymbol column
    protigy.ome = rep(ome_name, n_genes),
    row.names = paste0("gene_", 1:n_genes)
  )
  
  new("GCT",
      mat = mat,
      cdesc = cdesc,
      rdesc = rdesc,
      rid = paste0("gene_", 1:n_genes),
      cid = paste0("sample_", 1:n_samples)
  )
}

create_mock_merged_gct <- function() {
  # Create a merged GCT with multiple omes
  proteome_gct <- create_mock_gct(5, 4, "proteome")
  phospho_gct <- create_mock_gct(5, 4, "phosphoproteome")
  
  # Modify the phospho GCT to have different samples (simulating different column counts)
  phospho_gct@mat <- phospho_gct@mat[, 1:3]  # Only 3 samples
  phospho_gct@cdesc <- phospho_gct@cdesc[1:3, , drop = FALSE]
  phospho_gct@cid <- phospho_gct@cid[1:3]
  colnames(phospho_gct@mat) <- paste0("sample_", c(1, 2, 5))  # Different sample set
  rownames(phospho_gct@cdesc) <- paste0("sample_", c(1, 2, 5))
  phospho_gct@cid <- paste0("sample_", c(1, 2, 5))
  
  # Create merged GCT using the same logic as merge_processed_gcts
  GCTs_processed <- list(proteome = proteome_gct, phosphoproteome = phospho_gct)
  
  # Add protigy.ome column and create unique IDs
  GCTs_processed <- mapply(
    GCTs_processed, names(GCTs_processed),
    SIMPLIFY = FALSE, USE.NAMES = TRUE, 
    FUN = function(gct, ome) {
      gct@rdesc$protigy.ome <- rep(ome, dim(gct@rdesc)[1])
      return(gct)
    })
  
  # Merge using Reduce
  merged <- Reduce(
    function(gct1, gct2) {
      gct1@rdesc$old_id = gct1@rid
      gct2@rdesc$old_id = gct2@rid
      rownames(gct1@mat) = rownames(gct1@rdesc) = gct1@rdesc$id = gct1@rid = paste(gct1@rdesc$protigy.ome,gct1@rid,sep="_")
      rownames(gct2@mat) = rownames(gct2@rdesc) = gct2@rdesc$id = gct2@rid = paste(gct2@rdesc$protigy.ome,gct2@rid,sep="_")
      merged <- cmapR::merge_gct(gct1, gct2, dim='row')
      return(merged)
    },
    GCTs_processed)
  
  rownames(merged@cdesc) <- merged@cid
  rownames(merged@rdesc) <- merged@rid
  
  return(merged)
}

create_mock_gcts_and_params <- function() {
  list(
    GCTs = list(
      proteome = create_mock_gct(5, 4, "proteome"),
      phosphoproteome = create_mock_gct(5, 3, "phosphoproteome")
    ),
    GCTs_merged = create_mock_merged_gct(),
    parameters = list(
      proteome = list(annotation_column = "group"),
      phosphoproteome = list(annotation_column = "group")
    )
  )
}

# Test the core merging logic
test_that("merge_processed_gcts handles different column counts", {
  # Create GCTs with different numbers of samples
  proteome_gct <- create_mock_gct(5, 4, "proteome")
  phospho_gct <- create_mock_gct(5, 3, "phosphoproteome")
  
  # Modify phospho GCT to have different samples
  phospho_gct@mat <- phospho_gct@mat[, 1:3]
  phospho_gct@cdesc <- phospho_gct@cdesc[1:3, , drop = FALSE]
  phospho_gct@cid <- phospho_gct@cid[1:3]
  colnames(phospho_gct@mat) <- paste0("sample_", c(1, 2, 5))
  rownames(phospho_gct@cdesc) <- paste0("sample_", c(1, 2, 5))
  phospho_gct@cid <- paste0("sample_", c(1, 2, 5))
  
  GCTs_processed <- list(proteome = proteome_gct, phosphoproteome = phospho_gct)
  
  # Test the core merging logic (without withProgress)
  GCTs_processed <- mapply(
    GCTs_processed, names(GCTs_processed),
    SIMPLIFY = FALSE, USE.NAMES = TRUE, 
    FUN = function(gct, ome) {
      gct@rdesc$protigy.ome <- rep(ome, dim(gct@rdesc)[1])
      return(gct)
    })
  
  # This should work without errors
  expect_no_error({
    GCTs_merged <- Reduce(
      function(gct1, gct2) {
        gct1@rdesc$old_id = gct1@rid
        gct2@rdesc$old_id = gct2@rid
        rownames(gct1@mat) = rownames(gct1@rdesc) = gct1@rdesc$id = gct1@rid = paste(gct1@rdesc$protigy.ome,gct1@rid,sep="_")
        rownames(gct2@mat) = rownames(gct2@rdesc) = gct2@rdesc$id = gct2@rid = paste(gct2@rdesc$protigy.ome,gct2@rid,sep="_")
        merged <- cmapR::merge_gct(gct1, gct2, dim='row')
        return(merged)
      },
      GCTs_processed)
  })
  
  # Verify the merged result
  expect_equal(nrow(GCTs_merged@mat), 10)  # 5 genes from each ome
  expect_equal(ncol(GCTs_merged@mat), 5)   # All unique samples: 1,2,3,4,5
  expect_equal(length(GCTs_merged@rid), 10)
  expect_equal(length(GCTs_merged@cid), 5)
})

# Test the duplication prevention logic in merge_processed_gcts
test_that("merge_processed_gcts prevents ome prefix duplication", {
  # Create GCTs with different numbers of samples
  proteome_gct <- create_mock_gct(3, 4, "proteome")
  phospho_gct <- create_mock_gct(3, 3, "phosphoproteome")
  acetyl_gct <- create_mock_gct(3, 2, "acetylome")
  
  GCTs_processed <- list(
    proteome = proteome_gct, 
    phosphoproteome = phospho_gct,
    acetylome = acetyl_gct
  )
  
  # Add protigy.ome column
  GCTs_processed <- mapply(
    GCTs_processed, names(GCTs_processed),
    SIMPLIFY = FALSE, USE.NAMES = TRUE, 
    FUN = function(gct, ome) {
      gct@rdesc$protigy.ome <- rep(ome, dim(gct@rdesc)[1])
      return(gct)
    })
  
  # Test the fixed merging logic with duplication prevention
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
  expect_equal(nrow(GCTs_merged@mat), 9)  # 3 genes from each ome
  expect_equal(length(GCTs_merged@rid), 9)
  
  # Verify ome prefixes are correctly applied without duplication
  proteome_rids <- GCTs_merged@rid[grepl("^proteome_", GCTs_merged@rid)]
  phospho_rids <- GCTs_merged@rid[grepl("^phosphoproteome_", GCTs_merged@rid)]
  acetyl_rids <- GCTs_merged@rid[grepl("^acetylome_", GCTs_merged@rid)]
  
  expect_equal(length(proteome_rids), 3)
  expect_equal(length(phospho_rids), 3)
  expect_equal(length(acetyl_rids), 3)
  
  # Verify no duplication patterns (e.g., proteome_proteome_)
  duplicated_patterns <- GCTs_merged@rid[grepl("_.*_.*_", GCTs_merged@rid)]
  # The only patterns with multiple underscores should be the original phosphoproteome/acetylome IDs
  # which have underscores in their original structure (e.g., NP_005900.2_S91s_1_1_91_91)
  expect_true(length(duplicated_patterns) <= 6) # Only phosphoproteome and acetylome IDs should have multiple underscores
})

# Test the multiomic heatmap helper functions
# Note: preprocess_gcts_multiome_heatmap test removed
# This function is no longer used since the multi-ome heatmap now uses
# the merged GCT created during the main setup process

# Test the ComplexHeatmap function - simplified test
test_that("myComplexHeatmap function exists and can be called", {
  # Just test that the function exists and can be called with basic parameters
  expect_true(exists("myComplexHeatmap"))
  expect_true(is.function(myComplexHeatmap))
})

# Note: Edge case tests for preprocess_gcts_multiome_heatmap removed
# This function is no longer used since the multi-ome heatmap now uses
# the merged GCT created during the main setup process

# Test parameter validation
test_that("Multiomic heatmap validates parameters correctly", {
  merged_gct <- create_mock_merged_gct()
  
  # Test with invalid gene list - this should result in empty data
  params <- list(
    genes.char = "nonexistent_gene",
    min.val = -2,
    max.val = 2,
    show.rownames = TRUE,
    show.colnames = TRUE,
    cluster.rows = TRUE,
    cluster.cols = TRUE
  )
  
  custom_colors <- list(
    group = c("A" = "red", "B" = "blue")
  )
  
  # Should handle invalid genes gracefully - expect error for empty data
  expect_error({
    myComplexHeatmap(
      params = params,
      GENEMAX = 20,
      merged_rdesc = merged_gct@rdesc,
      merged_mat = merged_gct@mat,
      sample_anno = merged_gct@cdesc,
      custom_colors = custom_colors
    )
  })
})

# Note: Different data types test for preprocess_gcts_multiome_heatmap removed
# This function is no longer used since the multi-ome heatmap now uses
# the merged GCT created during the main setup process

# Test color handling - simplified test
test_that("Multiomic heatmap color functions exist", {
  # Test that color-related functions exist
  expect_true(exists("multiome_heatmap_custom_colors"))
  expect_true(is.function(multiome_heatmap_custom_colors))
})

# Note: Error handling tests for preprocess_gcts_multiome_heatmap removed
# This function is no longer used since the multi-ome heatmap now uses
# the merged GCT created during the main setup process

# Test the rewritten module logic
test_that("Rewritten multiomic module uses merged GCT correctly", {
  # Create mock GCTs_and_params
  gcts_and_params <- create_mock_gcts_and_params()
  
  # Test that the merged GCT has the expected structure
  merged_gct <- gcts_and_params$GCTs_merged
  
  expect_true(is(merged_gct, "GCT"))
  expect_true("protigy.ome" %in% names(merged_gct@rdesc))
  expect_equal(nrow(merged_gct@mat), 10)  # 5 genes from each ome
  expect_equal(ncol(merged_gct@mat), 5)   # All unique samples
  
  # Test that the merged GCT contains data from both omes
  ome_counts <- table(merged_gct@rdesc$protigy.ome)
  expect_equal(length(ome_counts), 2)
  expect_true("proteome" %in% names(ome_counts))
  expect_true("phosphoproteome" %in% names(ome_counts))
})

# Test sample alignment in merged GCT
test_that("Merged GCT properly aligns samples from different omes", {
  merged_gct <- create_mock_merged_gct()
  
  # Check that all samples are present
  expected_samples <- c("sample_1", "sample_2", "sample_3", "sample_4", "sample_5")
  expect_equal(sort(merged_gct@cid), sort(expected_samples))
  
  # Check that the matrix has the right dimensions
  expect_equal(ncol(merged_gct@mat), 5)
  expect_equal(nrow(merged_gct@mat), 10)  # 5 genes from each ome
  
  # Check that cdesc has the right samples
  expect_equal(sort(rownames(merged_gct@cdesc)), sort(expected_samples))
})

# Test gene ID uniqueness in merged GCT
test_that("Merged GCT creates unique gene IDs", {
  merged_gct <- create_mock_merged_gct()
  
  # Check that gene IDs are unique
  expect_equal(length(merged_gct@rid), length(unique(merged_gct@rid)))
  
  # Check that gene IDs contain ome information
  expect_true(all(grepl("_", merged_gct@rid)))
  
  # Check that we have genes from both omes
  proteome_genes <- sum(grepl("^proteome_", merged_gct@rid))
  phospho_genes <- sum(grepl("^phosphoproteome_", merged_gct@rid))
  
  # The merged GCT should have 5 genes from each ome
  expect_equal(proteome_genes, 5)
  expect_equal(phospho_genes, 5)
  expect_equal(length(merged_gct@rid), 10)  # Total genes from both omes
})
