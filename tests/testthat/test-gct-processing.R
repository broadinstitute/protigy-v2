# Tests for GCT processing functions

# Note: Tests that call transformGCTs or processGCTs may trigger showNotification
# which requires a Shiny session. These tests use testthat::with_mocked_bindings to mock showNotification

# Load test data
data(brca_retrospective_v5.0_rnaseq_gct)
data(brca_retrospective_v5.0_phosphoproteome_gct)
data(brca_retrospective_v5.0_proteome_gct)

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
  expect_equal(result$data.log.transform[1, 2], log2(2))
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
  # Create test data with single-element groups and proper dimnames
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

test_that("perform_data_normalization disables 2-component for datasets with >20 samples", {
  # Create test data with 25 samples (>20)
  test_data <- matrix(rnorm(100), nrow = 4, ncol = 25)
  rownames(test_data) <- paste0("gene_", 1:4)
  colnames(test_data) <- paste0("sample_", 1:25)
  
  test_cdesc <- data.frame(
    group = rep(c("A", "B", "C"), length.out = 25),
    row.names = paste0("sample_", 1:25)
  )
  
  # Test that 2-component is disabled and method is set to None
  expect_warning(
    result <- perform_data_normalization(
      test_data, "2-component", test_cdesc, FALSE, NULL
    ),
    "Two-component normalization is disabled for datasets with more than 20 samples"
  )
  
  expect_equal(result$updated_method, "None")
  expect_equal(result$data.norm, test_data)
})

test_that("perform_data_normalization does NOT disable 2-component for datasets with <=20 samples (boundary)", {
  # FIX C (P2.5): the previous tests ended in `expect_true(TRUE)` or an either/or
  # `%in% c("2-component","None")` that passes regardless of what the function does.
  # The guard's documented contract is:
  #   ncol > 20  -> emit a warning AND set updated_method = "None"
  #   ncol <= 20 -> NO warning about sample count; 2-component is attempted
  #
  # This test verifies the <=20 side: the sample-count warning must NOT fire
  # for a 20-column dataset.  The test will fail if the guard threshold is moved
  # (e.g. to <= 20 triggers the disable), confirming real coverage.
  test_data <- matrix(rnorm(80), nrow = 4, ncol = 20)
  rownames(test_data) <- paste0("gene_", 1:4)
  colnames(test_data) <- paste0("sample_", 1:20)

  test_cdesc <- data.frame(
    group = rep(c("A", "B"), each = 10),
    row.names = paste0("sample_", 1:20)
  )

  # The sample-count warning must NOT fire for exactly 20 samples.
  # Capture all warnings and assert the guard-specific one is absent.
  warns <- character(0)
  withCallingHandlers(
    result <- perform_data_normalization(
      test_data, "2-component", test_cdesc, FALSE, NULL
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  guard_msg <- "Two-component normalization is disabled for datasets with more than 20 samples"
  expect_false(
    any(grepl(guard_msg, warns, fixed = TRUE)),
    info = paste("Guard warning must be absent for 20-sample dataset; got:", paste(warns, collapse = " | "))
  )
})

test_that("perform_data_normalization does NOT disable 2-component for datasets with 19 samples", {
  # Belt-and-suspenders: strictly fewer than 20 samples  -  the guard must not fire.
  test_data <- matrix(rnorm(76), nrow = 4, ncol = 19)
  rownames(test_data) <- paste0("gene_", 1:4)
  colnames(test_data) <- paste0("sample_", 1:19)

  test_cdesc <- data.frame(
    group = rep(c("A", "B"), length.out = 19),
    row.names = paste0("sample_", 1:19)
  )

  warns <- character(0)
  withCallingHandlers(
    perform_data_normalization(test_data, "2-component", test_cdesc, FALSE, NULL),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  guard_msg <- "Two-component normalization is disabled for datasets with more than 20 samples"
  expect_false(
    any(grepl(guard_msg, warns, fixed = TRUE)),
    info = paste("Guard warning must be absent for 19-sample dataset; got:", paste(warns, collapse = " | "))
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

test_that("fix_gene_symbols replaces semicolons with pipes", {
  rdesc <- data.frame(
    geneSymbol = c("EGFR;ERBB1", "TP53;P53", "BRCA1"),
    row.names = paste0("gene_", 1:3)
  )
  
  result <- fix_gene_symbols(rdesc)
  
  expect_equal(result$rdesc$geneSymbol, c("EGFR|ERBB1", "TP53|P53", "BRCA1"))
  expect_equal(length(result$removed_rids), 0)
})

test_that("fix_gene_symbols removes blank symbols within strings", {
  rdesc <- data.frame(
    geneSymbol = c("EGFR| |ERBB1", "TP53| | |P53", "BRCA1| |"),
    row.names = paste0("gene_", 1:3)
  )
  
  result <- fix_gene_symbols(rdesc)
  
  expect_equal(result$rdesc$geneSymbol, c("EGFR|ERBB1", "TP53|P53", "BRCA1"))
  expect_equal(length(result$removed_rids), 0)
})

test_that("fix_gene_symbols converts blank gene symbols to NA and keeps all rows", {
  rdesc <- data.frame(
    geneSymbol = c("EGFR", "", "BRCA1", NA),
    row.names = paste0("gene_", 1:4)
  )
  
  result <- fix_gene_symbols(rdesc)
  
  # All rows should be kept - blank values converted to NA
  expect_equal(nrow(result$rdesc), 4)
  expect_equal(result$rdesc$geneSymbol, c("EGFR", NA_character_, "BRCA1", NA_character_))
  expect_equal(length(result$removed_rids), 0)  # No rows removed
})

test_that("fix_gene_symbols removes leading and trailing pipes", {
  rdesc <- data.frame(
    geneSymbol = c("|EGFR|ERBB1|", "|TP53", "BRCA1|"),
    row.names = paste0("gene_", 1:3)
  )
  
  result <- fix_gene_symbols(rdesc)
  
  expect_equal(result$rdesc$geneSymbol, c("EGFR|ERBB1", "TP53", "BRCA1"))
  expect_equal(length(result$removed_rids), 0)
})

test_that("fix_gene_symbols handles list columns", {
  rdesc <- data.frame(
    geneSymbol = I(list("EGFR", "TP53", "BRCA1")),
    row.names = paste0("gene_", 1:3)
  )
  
  result <- fix_gene_symbols(rdesc)
  
  expect_true(is.character(result$rdesc$geneSymbol))
  expect_equal(result$rdesc$geneSymbol, c("EGFR", "TP53", "BRCA1"))
  expect_equal(length(result$removed_rids), 0)
})

test_that("fix_gene_symbols handles list columns with multiple values", {
  rdesc <- data.frame(
    geneSymbol = I(list(c("EGFR", "ERBB1"), "TP53", c("BRCA1", "BRCA2"))),
    row.names = paste0("gene_", 1:3)
  )
  
  result <- fix_gene_symbols(rdesc)
  
  expect_true(is.character(result$rdesc$geneSymbol))
  expect_equal(result$rdesc$geneSymbol, c("EGFR|ERBB1", "TP53", "BRCA1|BRCA2"))
  expect_equal(length(result$removed_rids), 0)
})

test_that("fix_gene_symbols handles missing geneSymbol column", {
  rdesc <- data.frame(
    id = paste0("gene_", 1:3),
    row.names = paste0("gene_", 1:3)
  )
  
  result <- fix_gene_symbols(rdesc)
  
  expect_equal(result$rdesc, rdesc)
  expect_equal(length(result$removed_rids), 0)
})

test_that("fix_gene_symbols handles complex cases and converts blank to NA", {
  rdesc <- data.frame(
    geneSymbol = c("EGFR;ERBB1| |", "|TP53| |P53|", "BRCA1;BRCA2;BRCA3", "| |", ""),
    row.names = paste0("gene_", 1:5)
  )
  
  result <- fix_gene_symbols(rdesc)
  
  # All rows should be kept - blank values (| | and "") converted to NA
  expect_equal(nrow(result$rdesc), 5)
  expect_equal(result$rdesc$geneSymbol, c("EGFR|ERBB1", "TP53|P53", "BRCA1|BRCA2|BRCA3", NA_character_, NA_character_))
  expect_equal(length(result$removed_rids), 0)  # No rows removed
})

test_that("validateGCT creates Sample.ID when cdesc is null", {
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  rownames(mat) <- paste0("gene_", 1:3)
  colnames(mat) <- paste0("sample_", 1:3)
  
  rdesc <- data.frame(id = paste0("gene_", 1:3), row.names = paste0("gene_", 1:3))
  cdesc <- NULL
  
  gct <- new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc)
  
  result <- validateGCT(gct)
  
  expect_true("Sample.ID" %in% names(result@cdesc))
  expect_equal(result@cdesc$Sample.ID, paste0("sample_", 1:3))
  expect_equal(rownames(result@cdesc), paste0("sample_", 1:3))
})

test_that("validateGCT creates Sample.ID when cdesc is empty", {
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  rownames(mat) <- paste0("gene_", 1:3)
  colnames(mat) <- paste0("sample_", 1:3)
  
  rdesc <- data.frame(id = paste0("gene_", 1:3), row.names = paste0("gene_", 1:3))
  cdesc <- data.frame(row.names = paste0("sample_", 1:3))
  
  gct <- new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc)
  
  result <- validateGCT(gct)
  
  expect_true("Sample.ID" %in% names(result@cdesc))
  expect_equal(result@cdesc$Sample.ID, paste0("sample_", 1:3))
  expect_equal(rownames(result@cdesc), paste0("sample_", 1:3))
})

test_that("validateGCT creates Sample.ID when cdesc only has id column", {
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  rownames(mat) <- paste0("gene_", 1:3)
  colnames(mat) <- paste0("sample_", 1:3)
  
  rdesc <- data.frame(id = paste0("gene_", 1:3), row.names = paste0("gene_", 1:3))
  cdesc <- data.frame(
    id = paste0("sample_", 1:3),
    row.names = paste0("sample_", 1:3)
  )
  
  gct <- new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc)
  
  result <- validateGCT(gct)
  
  expect_true("Sample.ID" %in% names(result@cdesc))
  expect_equal(result@cdesc$Sample.ID, paste0("sample_", 1:3))
  expect_equal(rownames(result@cdesc), paste0("sample_", 1:3))
  expect_false("id" %in% names(result@cdesc))
})

test_that("validateGCT preserves cdesc with real metadata", {
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  rownames(mat) <- paste0("gene_", 1:3)
  colnames(mat) <- paste0("sample_", 1:3)
  
  rdesc <- data.frame(id = paste0("gene_", 1:3), row.names = paste0("gene_", 1:3))
  cdesc <- data.frame(
    group = c("A", "B", "C"),
    row.names = paste0("sample_", 1:3)
  )
  
  gct <- new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc)
  
  result <- validateGCT(gct)
  
  expect_true("group" %in% names(result@cdesc))
  expect_equal(result@cdesc$group, c("A", "B", "C"))
  expect_false("Sample.ID" %in% names(result@cdesc))
})

test_that("validateGCT errors when cdesc has metadata but rownames don't match", {
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  rownames(mat) <- paste0("gene_", 1:3)
  colnames(mat) <- paste0("sample_", 1:3)
  
  rdesc <- data.frame(id = paste0("gene_", 1:3), row.names = paste0("gene_", 1:3))
  cdesc <- data.frame(
    group = c("A", "B", "C"),
    row.names = paste0("wrong_sample_", 1:3)
  )
  
  gct <- new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc)
  
  expect_error(
    validateGCT(gct),
    "GCT data column names does not match `cdesc` row names"
  )
})

test_that("validateGCT does not create Sample.ID when cdesc has id and other columns", {
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  rownames(mat) <- paste0("gene_", 1:3)
  colnames(mat) <- paste0("sample_", 1:3)
  
  rdesc <- data.frame(id = paste0("gene_", 1:3), row.names = paste0("gene_", 1:3))
  cdesc <- data.frame(
    id = paste0("sample_", 1:3),
    group = c("A", "B", "C"),
    row.names = paste0("sample_", 1:3)
  )
  
  gct <- new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc)
  
  result <- validateGCT(gct)
  
  expect_true("id" %in% names(result@cdesc))
  expect_true("group" %in% names(result@cdesc))
  expect_false("Sample.ID" %in% names(result@cdesc))
})

test_that("read_gct_cdesc_as_character preserves leading zeros and text", {
  tf <- tempfile(fileext = ".gct")
  writeLines(
    c(
      "#1.3",
      "2\t2\t0\t2",
      "id\tS1\tS2",
      "barcode\t001\t010",
      "condition\tA\tB",
      "r1\t5\t6",
      "r2\t7\t8"
    ),
    tf
  )

  cdesc <- read_gct_cdesc_as_character(tf)

  expect_identical(rownames(cdesc), c("S1", "S2"))
  expect_identical(cdesc$barcode, c("001", "010"))
  expect_identical(cdesc$condition, c("A", "B"))
  expect_identical(cdesc$id, c("S1", "S2"))
})

test_that("parse_gctx_preserve_cdesc restores raw cdesc values for .gct", {
  tf <- tempfile(fileext = ".gct")
  writeLines(
    c(
      "#1.3",
      "2\t2\t0\t1",
      "id\tS1\tS2",
      "barcode\t001\t010",
      "r1\t5\t6",
      "r2\t7\t8"
    ),
    tf
  )

  parsed_default <- cmapR::parse_gctx(tf)
  parsed_preserved <- parse_gctx_preserve_cdesc(tf)

  # Default cmapR parser coerces numeric-looking values and drops leading zeros.
  expect_identical(as.character(parsed_default@cdesc$barcode), c("1", "10"))

  # Our wrapper restores raw annotation strings exactly as in file.
  expect_identical(parsed_preserved@cdesc$barcode, c("001", "010"))
  expect_identical(rownames(parsed_preserved@cdesc), parsed_preserved@cid)
})

test_that("apply_gene_symbol_from_params creates geneSymbol from selected column when geneSymbol doesn't exist", {
  # FIX D (P2.3): the previous test re-implemented the assignment inline
  #   (`test_rdesc$geneSymbol <- test_rdesc[[gene_symbol_col]]`)
  # and asserted on its own copy, never calling apply_gene_symbol_from_params.
  # This rewrite calls the REAL function so the test fails if that function
  # is removed or broken.
  test_rdesc <- data.frame(
    id        = paste0("gene_", 1:3),
    gene_name = c("GENE1", "GENE2", "GENE3"),
    row.names = paste0("gene_", 1:3)
  )

  params <- list(
    gene_symbol_column         = "gene_name",
    convert_ids_to_gene_symbol = FALSE
  )

  out <- apply_gene_symbol_from_params(rdesc = test_rdesc, params = params, ome = "test_ome")

  # Function must create geneSymbol from the selected column.
  expect_true("geneSymbol" %in% names(out$rdesc))
  expect_equal(out$rdesc$geneSymbol, c("GENE1", "GENE2", "GENE3"))

  # Source column is preserved unchanged.
  expect_true("gene_name" %in% names(out$rdesc))
  expect_equal(out$rdesc$gene_name, c("GENE1", "GENE2", "GENE3"))
})

test_that("apply_gene_symbol_from_params preserves original geneSymbol as geneSymbol_original when overwriting", {
  # FIX D (P2.3): same class of tautological re-implementation as above.
  test_rdesc <- data.frame(
    id         = paste0("gene_", 1:3),
    geneSymbol = c("OLD1", "OLD2", "OLD3"),
    gene_name  = c("NEW1", "NEW2", "NEW3"),
    row.names  = paste0("gene_", 1:3)
  )

  params <- list(
    gene_symbol_column         = "gene_name",
    convert_ids_to_gene_symbol = FALSE
  )

  out <- apply_gene_symbol_from_params(rdesc = test_rdesc, params = params, ome = "test_ome")

  # Original geneSymbol must be backed up.
  expect_true("geneSymbol_original" %in% names(out$rdesc))
  expect_equal(out$rdesc$geneSymbol_original, c("OLD1", "OLD2", "OLD3"))

  # geneSymbol must be overwritten with values from the selected column.
  expect_equal(out$rdesc$geneSymbol, c("NEW1", "NEW2", "NEW3"))

  # Source column is preserved unchanged.
  expect_equal(out$rdesc$gene_name, c("NEW1", "NEW2", "NEW3"))
})

test_that("apply_gene_symbol_from_params leaves rdesc unchanged when gene_symbol_column is None", {
  # Regression guard: "None" means the user did not select a gene-symbol column.
  test_rdesc <- data.frame(
    id         = paste0("gene_", 1:3),
    gene_name  = c("G1", "G2", "G3"),
    row.names  = paste0("gene_", 1:3)
  )

  params <- list(
    gene_symbol_column         = "None",
    convert_ids_to_gene_symbol = FALSE
  )

  out <- apply_gene_symbol_from_params(rdesc = test_rdesc, params = params, ome = "test_ome")

  # No geneSymbol column should have been created.
  expect_false("geneSymbol" %in% names(out$rdesc))
  expect_identical(out$rdesc, test_rdesc)
})
