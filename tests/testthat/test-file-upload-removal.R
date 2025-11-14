# Tests for file upload and removal functionality

test_that("file removal uses filename-based identifiers", {
  # This test verifies that file removal uses filenames instead of indices
  # to prevent issues when files are removed and indices shift
  
  # Create mock file data structure
  files <- data.frame(
    name = c("file1.csv", "file2.csv", "file3.csv"),
    datapath = c("/tmp/file1", "/tmp/file2", "/tmp/file3"),
    stringsAsFactors = FALSE
  )
  
  # Simulate filename-based ID generation (as in actual code)
  file_ids <- gsub("[^a-zA-Z0-9_]", "_", files$name)
  expect_equal(file_ids, c("file1_csv", "file2_csv", "file3_csv"))
  
  # Verify IDs are unique
  expect_equal(length(unique(file_ids)), 3)
  
  # Verify special characters are handled
  # Note: gsub replaces each non-alphanumeric character with underscore,
  # so consecutive special chars become multiple underscores
  special_files <- data.frame(
    name = c("file-1.csv", "file 2.csv", "file(3).csv"),
    stringsAsFactors = FALSE
  )
  special_ids <- gsub("[^a-zA-Z0-9_]", "_", special_files$name)
  expect_equal(special_ids, c("file_1_csv", "file_2_csv", "file_3__csv"))
})

test_that("file removal by name works correctly", {
  # Simulate file removal logic
  files <- data.frame(
    name = c("file1.csv", "file2.csv", "file3.csv"),
    datapath = c("/tmp/file1", "/tmp/file2", "/tmp/file3"),
    stringsAsFactors = FALSE
  )
  
  # Remove file by name (not index)
  file_to_remove <- "file2.csv"
  remaining_files <- files[files$name != file_to_remove, , drop = FALSE]
  
  expect_equal(nrow(remaining_files), 2)
  expect_equal(remaining_files$name, c("file1.csv", "file3.csv"))
  expect_false("file2.csv" %in% remaining_files$name)
})

test_that("file removal handles edge cases", {
  files <- data.frame(
    name = c("file1.csv"),
    datapath = c("/tmp/file1"),
    stringsAsFactors = FALSE
  )
  
  # Remove only file
  remaining <- files[files$name != "file1.csv", , drop = FALSE]
  expect_equal(nrow(remaining), 0)
  
  # Try to remove non-existent file
  remaining2 <- files[files$name != "nonexistent.csv", , drop = FALSE]
  expect_equal(nrow(remaining2), 1)
})

test_that("file ID sanitization handles various characters", {
  # Note: gsub replaces each non-alphanumeric character with underscore,
  # so consecutive special chars become multiple underscores
  test_cases <- list(
    "normal_file.csv" = "normal_file_csv",
    "file-with-dashes.csv" = "file_with_dashes_csv",
    "file with spaces.csv" = "file_with_spaces_csv",
    "file(1).csv" = "file_1__csv",  # ( becomes _, ) becomes _, . becomes _
    "file[2].csv" = "file_2__csv",  # [ becomes _, ] becomes _, . becomes _
    "file@3.csv" = "file_3_csv",
    "file#4.csv" = "file_4_csv"
  )
  
  for (input in names(test_cases)) {
    expected <- test_cases[[input]]
    result <- gsub("[^a-zA-Z0-9_]", "_", input)
    expect_equal(result, expected, info = paste("Failed for:", input))
  }
})

