# Tests for GCT processing functions

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

test_that("fix_gene_symbols removes completely blank gene symbols", {
  rdesc <- data.frame(
    geneSymbol = c("EGFR", "", "BRCA1", NA),
    row.names = paste0("gene_", 1:4)
  )
  
  result <- fix_gene_symbols(rdesc)
  
  expect_equal(nrow(result$rdesc), 2)
  expect_equal(result$rdesc$geneSymbol, c("EGFR", "BRCA1"))
  expect_equal(length(result$removed_rids), 2)
  expect_true("gene_2" %in% result$removed_rids)
  expect_true("gene_4" %in% result$removed_rids)
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

test_that("fix_gene_symbols handles complex cases", {
  rdesc <- data.frame(
    geneSymbol = c("EGFR;ERBB1| |", "|TP53| |P53|", "BRCA1;BRCA2;BRCA3", "| |", ""),
    row.names = paste0("gene_", 1:5)
  )
  
  result <- fix_gene_symbols(rdesc)
  
  expect_equal(nrow(result$rdesc), 3)
  expect_equal(result$rdesc$geneSymbol, c("EGFR|ERBB1", "TP53|P53", "BRCA1|BRCA2|BRCA3"))
  expect_equal(length(result$removed_rids), 2)
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

test_that("geneSymbol is preserved when it exists and user selects None", {
  # Create GCT with existing geneSymbol
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  rownames(mat) <- paste0("gene_", 1:3)
  colnames(mat) <- paste0("sample_", 1:3)
  
  rdesc <- data.frame(
    id = paste0("gene_", 1:3),
    geneSymbol = c("EGFR", "TP53", "BRCA1"),
    row.names = paste0("gene_", 1:3)
  )
  
  cdesc <- data.frame(
    group = c("A", "B", "C"),
    row.names = paste0("sample_", 1:3)
  )
  
  gct <- new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc)
  
  params <- list(
    gene_symbol_column = "None",
    log_transformation = "None",
    data_normalization = "None",
    group_normalization = FALSE,
    max_missing = 100,
    data_filter = "None",
    annotation_column = "group"
  )
  
  result <- transformGCTs(list(test = gct), list(test = params))
  
  expect_true("geneSymbol" %in% names(result$test@rdesc))
  expect_equal(result$test@rdesc$geneSymbol, c("EGFR", "TP53", "BRCA1"))
})

test_that("geneSymbol is preserved when it exists and user selects geneSymbol itself", {
  # Create GCT with existing geneSymbol
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  rownames(mat) <- paste0("gene_", 1:3)
  colnames(mat) <- paste0("sample_", 1:3)
  
  rdesc <- data.frame(
    id = paste0("gene_", 1:3),
    geneSymbol = c("EGFR", "TP53", "BRCA1"),
    row.names = paste0("gene_", 1:3)
  )
  
  cdesc <- data.frame(
    group = c("A", "B", "C"),
    row.names = paste0("sample_", 1:3)
  )
  
  gct <- new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc)
  
  params <- list(
    gene_symbol_column = "geneSymbol",
    log_transformation = "None",
    data_normalization = "None",
    group_normalization = FALSE,
    max_missing = 100,
    data_filter = "None",
    annotation_column = "group"
  )
  
  result <- transformGCTs(list(test = gct), list(test = params))
  
  expect_true("geneSymbol" %in% names(result$test@rdesc))
  expect_equal(result$test@rdesc$geneSymbol, c("EGFR", "TP53", "BRCA1"))
})

test_that("geneSymbol is replaced when it exists and user selects different column", {
  # Create GCT with existing geneSymbol and another gene column
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  rownames(mat) <- paste0("gene_", 1:3)
  colnames(mat) <- paste0("sample_", 1:3)
  
  rdesc <- data.frame(
    id = paste0("gene_", 1:3),
    geneSymbol = c("EGFR", "TP53", "BRCA1"),
    gene_name = c("EGFR_NEW", "TP53_NEW", "BRCA1_NEW"),
    row.names = paste0("gene_", 1:3)
  )
  
  cdesc <- data.frame(
    group = c("A", "B", "C"),
    row.names = paste0("sample_", 1:3)
  )
  
  gct <- new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc)
  
  params <- list(
    gene_symbol_column = "gene_name",
    log_transformation = "None",
    data_normalization = "None",
    group_normalization = FALSE,
    max_missing = 100,
    data_filter = "None",
    annotation_column = "group"
  )
  
  result <- transformGCTs(list(test = gct), list(test = params))
  
  expect_true("geneSymbol" %in% names(result$test@rdesc))
  expect_true("geneSymbol_original" %in% names(result$test@rdesc))
  expect_equal(result$test@rdesc$geneSymbol, c("EGFR_NEW", "TP53_NEW", "BRCA1_NEW"))
  expect_equal(result$test@rdesc$geneSymbol_original, c("EGFR", "TP53", "BRCA1"))
  expect_false("gene_name" %in% names(result$test@rdesc))
})

test_that("geneSymbol is created when it doesn't exist and user selects valid column", {
  # Create GCT without geneSymbol
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  rownames(mat) <- paste0("gene_", 1:3)
  colnames(mat) <- paste0("sample_", 1:3)
  
  rdesc <- data.frame(
    id = paste0("gene_", 1:3),
    gene_name = c("EGFR", "TP53", "BRCA1"),
    row.names = paste0("gene_", 1:3)
  )
  
  cdesc <- data.frame(
    group = c("A", "B", "C"),
    row.names = paste0("sample_", 1:3)
  )
  
  gct <- new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc)
  
  params <- list(
    gene_symbol_column = "gene_name",
    log_transformation = "None",
    data_normalization = "None",
    group_normalization = FALSE,
    max_missing = 100,
    data_filter = "None",
    annotation_column = "group"
  )
  
  result <- transformGCTs(list(test = gct), list(test = params))
  
  expect_true("geneSymbol" %in% names(result$test@rdesc))
  expect_equal(result$test@rdesc$geneSymbol, c("EGFR", "TP53", "BRCA1"))
  expect_false("gene_name" %in% names(result$test@rdesc))
})

test_that("geneSymbol is not created when it doesn't exist and user selects None", {
  # Create GCT without geneSymbol
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  rownames(mat) <- paste0("gene_", 1:3)
  colnames(mat) <- paste0("sample_", 1:3)
  
  rdesc <- data.frame(
    id = paste0("gene_", 1:3),
    row.names = paste0("gene_", 1:3)
  )
  
  cdesc <- data.frame(
    group = c("A", "B", "C"),
    row.names = paste0("sample_", 1:3)
  )
  
  gct <- new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc)
  
  params <- list(
    gene_symbol_column = "None",
    log_transformation = "None",
    data_normalization = "None",
    group_normalization = FALSE,
    max_missing = 100,
    data_filter = "None",
    annotation_column = "group"
  )
  
  result <- transformGCTs(list(test = gct), list(test = params))
  
  expect_false("geneSymbol" %in% names(result$test@rdesc))
})

test_that("geneSymbol is not created when selected column doesn't exist", {
  # Create GCT without the selected column
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  rownames(mat) <- paste0("gene_", 1:3)
  colnames(mat) <- paste0("sample_", 1:3)
  
  rdesc <- data.frame(
    id = paste0("gene_", 1:3),
    row.names = paste0("gene_", 1:3)
  )
  
  cdesc <- data.frame(
    group = c("A", "B", "C"),
    row.names = paste0("sample_", 1:3)
  )
  
  gct <- new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc)
  
  params <- list(
    gene_symbol_column = "nonexistent_column",
    log_transformation = "None",
    data_normalization = "None",
    group_normalization = FALSE,
    max_missing = 100,
    data_filter = "None",
    annotation_column = "group"
  )
  
  result <- transformGCTs(list(test = gct), list(test = params))
  
  expect_false("geneSymbol" %in% names(result$test@rdesc))
})

test_that("geneSymbol formatting is applied when it exists and user selects None", {
  # Create GCT with existing geneSymbol that needs formatting
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  rownames(mat) <- paste0("gene_", 1:3)
  colnames(mat) <- paste0("sample_", 1:3)
  
  rdesc <- data.frame(
    id = paste0("gene_", 1:3),
    geneSymbol = c("EGFR;ERBB1", "TP53| |P53", "|BRCA1|"),
    row.names = paste0("gene_", 1:3)
  )
  
  cdesc <- data.frame(
    group = c("A", "B", "C"),
    row.names = paste0("sample_", 1:3)
  )
  
  gct <- new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc)
  
  params <- list(
    gene_symbol_column = "None",
    log_transformation = "None",
    data_normalization = "None",
    group_normalization = FALSE,
    max_missing = 100,
    data_filter = "None",
    annotation_column = "group"
  )
  
  result <- transformGCTs(list(test = gct), list(test = params))
  
  expect_true("geneSymbol" %in% names(result$test@rdesc))
  expect_equal(result$test@rdesc$geneSymbol, c("EGFR|ERBB1", "TP53|P53", "BRCA1"))
})
