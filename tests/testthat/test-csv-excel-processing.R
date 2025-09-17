################################################################################
# Unit Tests for CSV/Excel/TSV Processing Module
#
# Tests the core functionality of CSV/Excel/TSV file processing including:
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
  
  # Empty values
  empty_design <- data.frame(
    columnName = c("Sample1", ""),
    Experiment = c("Control", "Treatment"),
    stringsAsFactors = FALSE
  )
  
  expect_error(validateExperimentalDesign(empty_design), "Missing or empty values found")
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
  expect_equal(names(result), c("GCTs", "parameters"))
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
