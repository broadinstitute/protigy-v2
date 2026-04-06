################################################################################
# Unit Tests for CSV/Excel/TSV/SSV Processing Module
#
# Tests the core functionality of CSV/Excel/TSV/SSV file processing including:
# - Experimental design template generation
# - File reading and validation
# - GCT conversion
# - Column classification
################################################################################

# Load required packages
library(testthat)
library(Protigy)

# Load test data
data("brca_retrospective_v5.0_proteome_gct")
data("brca_retrospective_v5.0_phosphoproteome_gct")

################################################################################
# Test Experimental Design Functions
################################################################################

test_that("generateExperimentalDesignTemplate creates correct structure", {
  # Create mock data files
  mock_files <- data.frame(
    name = c("test1.csv", "test2.csv"),
    datapath = c("path1", "path2"),
    stringsAsFactors = FALSE
  )
  
  # Create temporary files for testing
  csv_file <- tempfile(fileext = ".csv")
  csv_file2 <- tempfile(fileext = ".csv")  # Use CSV instead of Excel for simplicity
  
  # Write test data
  write.csv(data.frame(col1 = 1, col2 = 2, col3 = 3), csv_file, row.names = FALSE)
  write.csv(data.frame(col1 = 1, col2 = 2, col3 = 3), csv_file2, row.names = FALSE)
  
  # Update mock files with real paths
  mock_files$datapath <- c(csv_file, csv_file2)
  
  template <- generateExperimentalDesignTemplate(mock_files)
  
  # Check structure
  expect_equal(ncol(template), 4)
  expect_equal(colnames(template), c("column_name", "experiment", "condition", "replicate"))
  expect_equal(nrow(template), 6) # 3 columns from each file
  expect_true(all(is.na(template$experiment)))
  expect_true(all(is.na(template$condition)))
  expect_true(all(is.na(template$replicate)))
  
  # Clean up
  unlink(c(csv_file, csv_file2))
})

test_that("validateExperimentalDesign validates required columns", {
  # Valid experimental design
  valid_design <- data.frame(
    columnName = c("Sample1", "Sample2"),
    Experiment = c("Control", "Treatment"),
    Group = c("Group1", "Group2"),
    stringsAsFactors = FALSE
  )
  
  expect_true(validateExperimentalDesign(valid_design))
  
  # Missing columnName column
  invalid_design <- data.frame(
    Column = c("Sample1", "Sample2"),
    Experiment = c("Control", "Treatment"),
    stringsAsFactors = FALSE
  )
  
  expect_error(validateExperimentalDesign(invalid_design), "Missing required column: columnName")
  
  # Empty string in columnName is allowed by the new validation (only NA and
  # duplicates are rejected; empty-string checking was removed when the function
  # was updated to permit NA in factor columns for metadata rows).
  empty_design <- data.frame(
    columnName = c("Sample1", ""),
    Experiment = c("Control", "Treatment"),
    stringsAsFactors = FALSE
  )

  expect_true(validateExperimentalDesign(empty_design))
})

test_that("readExperimentalDesign handles different file formats", {
  # Test CSV reading
  csv_file <- tempfile(fileext = ".csv")
  csv_data <- data.frame(
    columnName = c("Sample1", "Sample2"),
    Experiment = c("Control", "Treatment"),
    Group = c("Group1", "Group2"),
    stringsAsFactors = FALSE
  )
  write.csv(csv_data, csv_file, row.names = FALSE)
  
  result <- readExperimentalDesign(csv_file)
  expect_equal(nrow(result), 2)
  expect_equal(colnames(result), c("columnName", "Experiment", "Group"))
  
  # Clean up
  unlink(csv_file)
  
  # Test TSV reading
  tsv_file <- tempfile(fileext = ".tsv")
  tsv_data <- data.frame(
    columnName = c("Sample1", "Sample2"),
    Experiment = c("Control", "Treatment"),
    Group = c("Group1", "Group2"),
    stringsAsFactors = FALSE
  )
  write.table(tsv_data, tsv_file, sep = "\t", row.names = FALSE, quote = FALSE)
  
  result_tsv <- readExperimentalDesign(tsv_file)
  expect_equal(nrow(result_tsv), 2)
  expect_equal(colnames(result_tsv), c("columnName", "Experiment", "Group"))
  
  # Clean up
  unlink(tsv_file)
  
  # Test SSV reading (semicolon-separated)
  ssv_file <- tempfile(fileext = ".ssv")
  ssv_data <- data.frame(
    columnName = c("Sample1", "Sample2"),
    Experiment = c("Control", "Treatment"),
    Group = c("Group1", "Group2"),
    stringsAsFactors = FALSE
  )
  write.table(ssv_data, ssv_file, sep = ";", row.names = FALSE, quote = FALSE)
  
  result_ssv <- readExperimentalDesign(ssv_file)
  expect_equal(nrow(result_ssv), 2)
  expect_equal(colnames(result_ssv), c("columnName", "Experiment", "Group"))
  
  # Clean up
  unlink(ssv_file)
})

################################################################################
# Test CSV/Excel Processing Functions
################################################################################

test_that("validateIdentifierColumn handles user-specified column", {
  test_data <- data.frame(
    protein_id = c("P1", "P2", "P3"),
    gene_symbol = c("G1", "G2", "G3"),
    sample1 = c(1, 2, 3),
    sample2 = c(4, 5, 6),
    stringsAsFactors = FALSE
  )
  
  result <- validateIdentifierColumn(test_data, "protein_id")
  
  expect_equal(result, "protein_id")
})

test_that("validateIdentifierColumn handles missing column", {
  test_data <- data.frame(
    protein_id = c("P1", "P2", "P3"),
    sample1 = c(1, 2, 3),
    sample2 = c(4, 5, 6),
    stringsAsFactors = FALSE
  )
  
  expect_error(validateIdentifierColumn(test_data, "nonexistent_column"), 
               "Identifier column 'nonexistent_column' not found in data")
})

test_that("validateIdentifierColumn handles empty identifier", {
  test_data <- data.frame(
    id = c("ID1", "ID2", "ID3"),
    sample1 = c(1, 2, 3),
    sample2 = c(4, 5, 6),
    stringsAsFactors = FALSE
  )
  
  expect_error(validateIdentifierColumn(test_data, ""), 
               "Identifier column must be specified")
  expect_error(validateIdentifierColumn(test_data, NULL), 
               "Identifier column must be specified")
})


test_that("classifyColumns separates sample and metadata columns", {
  sample_ids <- c("Sample1", "Sample2", "MetadataCol")
  experimental_design <- data.frame(
    columnName = c("Sample1", "Sample2"),
    Experiment = c("Control", "Treatment"),
    Group = c("Group1", "Group2"),
    stringsAsFactors = FALSE
  )
  
  result <- classifyColumns(sample_ids, experimental_design)
  
  expect_equal(result$sample_columns, c("Sample1", "Sample2"))
  expect_equal(result$rdesc_columns, "MetadataCol")
})

test_that("classifyColumns handles blank metadata entries", {
  sample_ids <- c("Sample1", "Sample2", "BlankCol")
  experimental_design <- data.frame(
    columnName = c("Sample1", "Sample2", "BlankCol"),
    Experiment = c("Control", "Treatment", ""),
    Group = c("Group1", "Group2", ""),
    stringsAsFactors = FALSE
  )
  
  result <- classifyColumns(sample_ids, experimental_design)
  
  expect_equal(result$sample_columns, c("Sample1", "Sample2"))
  expect_equal(result$rdesc_columns, "BlankCol")
})

################################################################################
# Test GCT Conversion Functions
################################################################################

test_that("convertToGCT creates valid GCT object", {
  # Create test data
  test_data <- data.frame(
    protein_id = c("P1", "P2", "P3"),
    gene_symbol = c("G1", "G2", "G3"),
    Sample1 = c(1, 2, 3),
    Sample2 = c(4, 5, 6),
    stringsAsFactors = FALSE
  )
  
  experimental_design <- data.frame(
    columnName = c("Sample1", "Sample2"),
    Experiment = c("Control", "Treatment"),
    Group = c("Group1", "Group2"),
    stringsAsFactors = FALSE
  )
  
  gct_obj <- convertToGCT(test_data, experimental_design, "test_file.csv", "protein_id")
  
  # Check GCT structure
  expect_s4_class(gct_obj, "GCT")
  expect_equal(nrow(gct_obj@mat), 3) # 3 features
  expect_equal(ncol(gct_obj@mat), 2) # 2 samples
  expect_equal(length(gct_obj@rid), 3) # 3 row IDs
  expect_equal(length(gct_obj@cid), 2) # 2 column IDs
  
  # Check that protein_id became the row ID
  expect_equal(gct_obj@rid, c("P1", "P2", "P3"))
  
  # Check that gene_symbol is in rdesc (since it's not in experimental design, it becomes rdesc)
  expect_true("gene_symbol" %in% colnames(gct_obj@rdesc))
  expect_equal(gct_obj@rdesc$gene_symbol, c("G1", "G2", "G3"))
  
  # Check that experimental design is in cdesc
  expect_true("Experiment" %in% colnames(gct_obj@cdesc))
  expect_true("Group" %in% colnames(gct_obj@cdesc))
})

test_that("createCdesc creates correct column descriptions", {
  experimental_design <- data.frame(
    columnName = c("Sample1", "Sample2"),
    Experiment = c("Control", "Treatment"),
    Group = c("Group1", "Group2"),
    stringsAsFactors = FALSE
  )
  
  sample_columns <- c("Sample1", "Sample2")
  
  cdesc <- createCdesc(sample_columns, experimental_design, "test_file.csv")
  
  expect_equal(nrow(cdesc), 2)
  expect_equal(rownames(cdesc), c("Sample1", "Sample2"))
  expect_equal(cdesc$Experiment, c("Control", "Treatment"))
  expect_equal(cdesc$Group, c("Group1", "Group2"))
})

test_that("filterExperimentalColumns filters correctly", {
  experimental_design <- data.frame(
    columnName = c("Sample1", "Sample2"),
    Experiment = c("Control", "Treatment"),
    Group = c("Group1", "Group2"),
    stringsAsFactors = FALSE
  )
  
  all_columns <- c("Sample1", "Sample2", "MetadataCol")
  
  filtered <- filterExperimentalColumns(all_columns, experimental_design)
  
  expect_equal(filtered, c("Sample1", "Sample2"))
})

################################################################################
# Test Edge Cases and Error Handling
################################################################################

test_that("validateIdentifierColumn handles missing columns gracefully", {
  test_data <- data.frame(
    sample1 = c(1, 2, 3),
    sample2 = c(4, 5, 6),
    stringsAsFactors = FALSE
  )
  
  expect_error(validateIdentifierColumn(test_data, "nonexistent_column"))
})

test_that("convertToGCT handles empty data gracefully", {
  empty_data <- data.frame()
  experimental_design <- data.frame(
    columnName = character(0),
    Experiment = character(0),
    stringsAsFactors = FALSE
  )
  
  expect_error(convertToGCT(empty_data, experimental_design, "empty.csv", "id"))
})

test_that("classifyColumns handles empty experimental design", {
  sample_ids <- c("Sample1", "Sample2")
  empty_design <- data.frame(
    columnName = character(0),
    Experiment = character(0),
    stringsAsFactors = FALSE
  )
  
  result <- classifyColumns(sample_ids, empty_design)
  
  expect_equal(result$sample_columns, character(0))
  expect_equal(result$rdesc_columns, sample_ids)
})

test_that("readExperimentalDesign handles unsupported file formats", {
  unsupported_file <- tempfile(fileext = ".txt")
  writeLines("test content", unsupported_file)
  
  expect_error(readExperimentalDesign(unsupported_file), "Unsupported file format")
  
  unlink(unsupported_file)
})

################################################################################
# Test SSV File Processing
################################################################################

test_that("SSV files can be read and processed like CSV/TSV", {
  # Create test SSV file
  ssv_file <- tempfile(fileext = ".ssv")
  ssv_data <- data.frame(
    protein_id = c("P1", "P2", "P3"),
    gene_symbol = c("G1", "G2", "G3"),
    Sample1 = c(1, 2, 3),
    Sample2 = c(4, 5, 6),
    stringsAsFactors = FALSE
  )
  write.table(ssv_data, ssv_file, sep = ";", row.names = FALSE, quote = FALSE)
  
  # Read SSV file using readr::read_delim
  result <- readr::read_delim(ssv_file, delim = ";", show_col_types = FALSE)
  
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 4)
  expect_equal(colnames(result), c("protein_id", "gene_symbol", "Sample1", "Sample2"))
  expect_equal(result$protein_id, c("P1", "P2", "P3"))
  
  # Clean up
  unlink(ssv_file)
})

test_that("SSV files work with convertToGCT", {
  # Create test SSV data
  ssv_data <- data.frame(
    protein_id = c("P1", "P2", "P3"),
    gene_symbol = c("G1", "G2", "G3"),
    Sample1 = c(1, 2, 3),
    Sample2 = c(4, 5, 6),
    stringsAsFactors = FALSE
  )
  
  experimental_design <- data.frame(
    columnName = c("Sample1", "Sample2"),
    Experiment = c("Control", "Treatment"),
    Group = c("Group1", "Group2"),
    stringsAsFactors = FALSE
  )
  
  gct_obj <- convertToGCT(ssv_data, experimental_design, "test_file.ssv", "protein_id")
  
  # Check GCT structure
  expect_s4_class(gct_obj, "GCT")
  expect_equal(nrow(gct_obj@mat), 3) # 3 features
  expect_equal(ncol(gct_obj@mat), 2) # 2 samples
  expect_equal(length(gct_obj@rid), 3) # 3 row IDs
  expect_equal(length(gct_obj@cid), 2) # 2 column IDs
  
  # Check that protein_id became the row ID
  expect_equal(gct_obj@rid, c("P1", "P2", "P3"))
  
  # Check that gene_symbol is in rdesc
  expect_true("gene_symbol" %in% colnames(gct_obj@rdesc))
  expect_equal(gct_obj@rdesc$gene_symbol, c("G1", "G2", "G3"))
  
  # Check that experimental design is in cdesc
  expect_true("Experiment" %in% colnames(gct_obj@cdesc))
  expect_true("Group" %in% colnames(gct_obj@cdesc))
})

test_that("SSV experimental design files are read correctly", {
  # Create SSV experimental design file
  ssv_exp_design_file <- tempfile(fileext = ".ssv")
  exp_design_data <- data.frame(
    columnName = c("Sample1", "Sample2", "Sample3"),
    Experiment = c("Control", "Treatment", "Control"),
    Group = c("Group1", "Group2", "Group1"),
    stringsAsFactors = FALSE
  )
  write.table(exp_design_data, ssv_exp_design_file, sep = ";", row.names = FALSE, quote = FALSE)
  
  # Read using readExperimentalDesign
  result <- readExperimentalDesign(ssv_exp_design_file)
  
  expect_equal(nrow(result), 3)
  expect_equal(colnames(result), c("columnName", "Experiment", "Group"))
  expect_equal(result$columnName, c("Sample1", "Sample2", "Sample3"))
  expect_equal(result$Experiment, c("Control", "Treatment", "Control"))
  expect_equal(result$Group, c("Group1", "Group2", "Group1"))
  
  # Clean up
  unlink(ssv_exp_design_file)
})

test_that("SSV files with semicolons in data are handled correctly", {
  # Create SSV file with semicolons in quoted strings (if applicable)
  # Note: readr::read_delim should handle this automatically
  ssv_file <- tempfile(fileext = ".ssv")
  ssv_data <- data.frame(
    id = c("ID1", "ID2", "ID3"),
    description = c("Item; A", "Item: B", "Item C"),
    value1 = c(1, 2, 3),
    value2 = c(4, 5, 6),
    stringsAsFactors = FALSE
  )
  write.table(ssv_data, ssv_file, sep = ";", row.names = FALSE, quote = TRUE)
  
  # Read SSV file
  result <- readr::read_delim(ssv_file, delim = ";", show_col_types = FALSE)
  
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 4)
  expect_equal(result$id, c("ID1", "ID2", "ID3"))
  
  # Clean up
  unlink(ssv_file)
})

################################################################################
# Test Integration with Existing GCT Workflow
################################################################################

test_that("processCSVExcelWorkflowWithPerDatasetIdentifiers returns compatible format", {
  # Create mock data files
  mock_files <- data.frame(
    name = c("test1.csv"),
    datapath = c("path1"),
    stringsAsFactors = FALSE
  )
  
  experimental_design <- data.frame(
    columnName = c("Sample1", "Sample2"),
    Experiment = c("Control", "Treatment"),
    Group = c("Group1", "Group2"),
    stringsAsFactors = FALSE
  )
  
  identifier_columns <- c("protein_id")
  labels <- c("test_dataset")
  
  # Create temporary CSV file for testing
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    protein_id = c("P1", "P2"),
    gene_symbol = c("G1", "G2"),
    Sample1 = c(1, 2),
    Sample2 = c(3, 4),
    stringsAsFactors = FALSE
  )
  write.csv(test_data, temp_csv, row.names = FALSE)
  
  # Update mock_files with actual file path
  mock_files$datapath <- temp_csv
  
  # Test the function
  result <- processCSVExcelWorkflowWithPerDatasetIdentifiers(
    mock_files, experimental_design, identifier_columns, labels
  )
  
  # Check structure matches GCT workflow
  expect_type(result, "list")
  expect_equal(names(result), c("GCTs", "parameters", "warnings"))
  expect_equal(names(result$GCTs), "test_dataset")
  expect_equal(names(result$parameters), "test_dataset")
  
  # Check GCT object
  expect_s4_class(result$GCTs$test_dataset, "GCT")
  
  # Check parameters structure
  expect_true("gct_file_path" %in% names(result$parameters$test_dataset))
  expect_true("gct_file_name" %in% names(result$parameters$test_dataset))
  
  # Clean up temporary file
  unlink(temp_csv)
})

test_that("SSV files work with processCSVExcelWorkflowWithPerDatasetIdentifiers", {
  # Create mock SSV data file
  mock_files <- data.frame(
    name = c("test1.ssv"),
    datapath = c("path1"),
    stringsAsFactors = FALSE
  )

  experimental_design <- data.frame(
    columnName = c("Sample1", "Sample2"),
    Experiment = c("Control", "Treatment"),
    Group = c("Group1", "Group2"),
    stringsAsFactors = FALSE
  )

  identifier_columns <- c("protein_id")
  labels <- c("test_dataset_ssv")

  # Create temporary SSV file for testing
  temp_ssv <- tempfile(fileext = ".ssv")
  test_data <- data.frame(
    protein_id = c("P1", "P2"),
    gene_symbol = c("G1", "G2"),
    Sample1 = c(1, 2),
    Sample2 = c(3, 4),
    stringsAsFactors = FALSE
  )
  write.table(test_data, temp_ssv, sep = ";", row.names = FALSE, quote = FALSE)

  # Update mock_files with actual file path
  mock_files$datapath <- temp_ssv

  # Test the function with SSV file
  result <- processCSVExcelWorkflowWithPerDatasetIdentifiers(
    mock_files, experimental_design, identifier_columns, labels
  )

  # Check structure matches GCT workflow
  expect_type(result, "list")
  expect_equal(names(result), c("GCTs", "parameters", "warnings"))
  expect_equal(names(result$GCTs), "test_dataset_ssv")
  expect_equal(names(result$parameters), "test_dataset_ssv")

  # Check GCT object
  expect_s4_class(result$GCTs$test_dataset_ssv, "GCT")
  expect_equal(nrow(result$GCTs$test_dataset_ssv@mat), 2) # 2 features
  expect_equal(ncol(result$GCTs$test_dataset_ssv@mat), 2) # 2 samples

  # Check parameters structure
  expect_true("gct_file_path" %in% names(result$parameters$test_dataset_ssv))
  expect_true("gct_file_name" %in% names(result$parameters$test_dataset_ssv))
  expect_equal(result$parameters$test_dataset_ssv$gct_file_name, "test1.ssv")

  # Clean up temporary file
  unlink(temp_ssv)
})

################################################################################
# Tests for NA handling in experimental design (new validation behavior)
################################################################################

test_that("validateExperimentalDesign allows NA in factor columns for metadata rows", {
  # This is the real-world scenario: rows with all-NA factor values are rdesc/metadata
  design_with_metadata_rows <- data.frame(
    columnName = c("PG.Genes", "PG.ProteinGroups", "Sample1", "Sample2"),
    Condition  = c(NA, NA, "Control", "Treatment"),
    Replicate  = c(NA, NA, 1L, 2L),
    stringsAsFactors = FALSE
  )
  expect_true(validateExperimentalDesign(design_with_metadata_rows))
})

test_that("validateExperimentalDesign errors on NA in columnName", {
  design_with_na_colname <- data.frame(
    columnName = c("Sample1", NA, "Sample3"),
    Condition  = c("Control", "Treatment", "Control"),
    stringsAsFactors = FALSE
  )
  expect_error(
    validateExperimentalDesign(design_with_na_colname),
    "columnName.*NA.*row"
  )
})

test_that("validateExperimentalDesign errors on duplicate columnName values", {
  design_with_dupes <- data.frame(
    columnName = c("Sample1", "Sample1", "Sample2"),
    Condition  = c("Control", "Treatment", "Control"),
    stringsAsFactors = FALSE
  )
  expect_error(
    validateExperimentalDesign(design_with_dupes),
    "Duplicate"
  )
})

test_that("validateUniqueIdentifiers errors when identifier column contains NA", {
  test_data <- data.frame(
    protein_id = c("P1", NA, "P3"),
    sample1    = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  expect_error(
    validateUniqueIdentifiers(test_data, "protein_id"),
    "NA/missing"
  )
})

test_that("classifyColumns returns diagnostic counts", {
  sample_ids <- c("Sample1", "Sample2", "PG.Genes", "UnknownCol")
  exp_design <- data.frame(
    columnName = c("Sample1", "Sample2", "PG.Genes"),
    Condition  = c("Control", "Treatment", NA),
    Replicate  = c(1L, 2L, NA),
    stringsAsFactors = FALSE
  )
  result <- classifyColumns(sample_ids, exp_design)

  # Sample1, Sample2 are data columns; PG.Genes is metadata (all-NA factors)
  expect_equal(sort(result$sample_columns), c("Sample1", "Sample2"))
  # PG.Genes (metadata row) and UnknownCol (not in design) both become rdesc
  expect_true("PG.Genes" %in% result$rdesc_columns)
  expect_true("UnknownCol" %in% result$rdesc_columns)
  # Diagnostic counts
  expect_equal(result$n_not_in_design, 1L)  # UnknownCol
  expect_equal(result$n_all_na_meta, 1L)    # PG.Genes
})

test_that("convertToGCT with metadata rows in experimental design produces correct GCT", {
  # Simulate a real-world scenario: experimental design has 8 metadata rows (all-NA factors)
  # and 2 sample rows
  test_data <- data.frame(
    protigy_id      = c("ProtA", "ProtB"),
    PG.Genes        = c("GENE1", "GENE2"),
    PG.ProteinGroups = c("ProtA;ProtB", "ProtC"),
    Sample1         = c(1.0, 2.0),
    Sample2         = c(3.0, 4.0),
    stringsAsFactors = FALSE
  )
  exp_design <- data.frame(
    columnName = c("PG.Genes", "PG.ProteinGroups", "Sample1", "Sample2"),
    Condition  = c(NA, NA, "Control", "Treatment"),
    Replicate  = c(NA, NA, 1L, 2L),
    stringsAsFactors = FALSE
  )
  gct_obj <- convertToGCT(test_data, exp_design, "test.csv", "protigy_id")

  expect_s4_class(gct_obj, "GCT")
  expect_equal(ncol(gct_obj@mat), 2)         # 2 samples
  expect_equal(nrow(gct_obj@mat), 2)         # 2 features
  expect_true("PG.Genes" %in% names(gct_obj@rdesc))
  expect_true("PG.ProteinGroups" %in% names(gct_obj@rdesc))
})

################################################################################
# Tests for Spectronaut condition setup NA handling
################################################################################

test_that("buildExpDesignFromConditionSetup errors on NA Condition", {
  condition_data <- data.frame(
    "Run Label"  = c("Run1", "Run2"),
    "Condition"  = c("Control", NA),
    "Replicate"  = c(1L, 2L),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  expect_error(
    buildExpDesignFromConditionSetup(condition_data, merge_condition_replicate = TRUE),
    "Condition column has NA"
  )
})

test_that("buildExpDesignFromConditionSetup warns on NA Replicate and uses Condition-only when unique", {
  condition_data <- data.frame(
    "Run Label" = c("Run1", "Run2"),
    "Condition" = c("Control", "Treatment"),
    "Replicate" = c(1L, NA),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  # Verify the warning is emitted
  expect_warning(
    buildExpDesignFromConditionSetup(condition_data, merge_condition_replicate = TRUE),
    "Replicate is NA"
  )
  # Capture the return value separately (expect_warning returns the warning, not the result)
  result <- suppressWarnings(
    buildExpDesignFromConditionSetup(condition_data, merge_condition_replicate = TRUE)
  )
  # Conditions are unique — all rows should use Condition only
  expect_equal(result$columnName, c("Control", "Treatment"))
})

test_that("buildExpDesignFromConditionSetup uses Run Label when Conditions not unique and Replicate NA", {
  condition_data <- data.frame(
    "Run Label" = c("Run1", "Run2"),
    "Condition" = c("Control", "Control"),
    "Replicate" = c(1L, NA),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  expect_warning(
    buildExpDesignFromConditionSetup(condition_data, merge_condition_replicate = TRUE),
    "Replicate is NA"
  )
  result <- suppressWarnings(
    buildExpDesignFromConditionSetup(condition_data, merge_condition_replicate = TRUE)
  )
  expect_equal(result$columnName, c("Run1", "Run2"))
})

test_that("read_spectronaut_condition_setup errors on NA in required columns", {
  tmp_file <- tempfile(fileext = ".tsv")
  data_with_na <- data.frame(
    "Run Label"  = c("Run1", "Run2"),
    "Condition"  = c("Control", NA),
    "Replicate"  = c(1L, 2L),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  readr::write_tsv(data_with_na, tmp_file)

  expect_error(
    read_spectronaut_condition_setup(tmp_file),
    "NA values"
  )
  unlink(tmp_file)
})
