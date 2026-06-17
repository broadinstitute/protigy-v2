# Tests for file upload and removal functionality

# ---------------------------------------------------------------------------
# FIX A (P2.4): rewritten to call the REAL gct_remove_btn_id function.
# The previous tests asserted gsub("[^a-zA-Z0-9_]","_",name) — the known-
# buggy approach that gct_remove_btn_id was written to replace.  These tests
# would pass even if gct_remove_btn_id were deleted.  The new tests drive the
# real function and assert injectivity, stability, and Shiny inputId validity.
# ---------------------------------------------------------------------------

test_that("gct_remove_btn_id is injective: colliding names under gsub get distinct ids", {
  # "a-b.gct" and "a_b.gct" both collapse to "a_b_gct" under naive gsub,
  # causing duplicate button ids.  The real function hex-encodes filenames
  # so each distinct name always produces a distinct id.
  id1 <- gct_remove_btn_id("a-b.gct")
  id2 <- gct_remove_btn_id("a_b.gct")
  expect_false(identical(id1, id2),
    info = paste("collision:", id1, "==", id2))
})

test_that("gct_remove_btn_id is deterministic / stable", {
  # Same input always yields the same id (needed for the monotonic-dedup
  # accumulator in setupSidebarServer to work reliably).
  fn <- "some_file-v1.gct"
  expect_identical(gct_remove_btn_id(fn), gct_remove_btn_id(fn))
})

test_that("gct_remove_btn_id produces a valid Shiny inputId", {
  # Shiny requires inputIds to start with a letter and contain only
  # letters, digits, underscores, hyphens, or dots.
  # The real function emits "remove_file_<hex>", satisfying those rules.
  fns <- c(
    "file1.gct",
    "file-with-dashes.gct",
    "file with spaces.gct",
    "file(1).gct",
    "file[2].gct",
    "unicodeé.gct"
  )
  for (fn in fns) {
    id <- gct_remove_btn_id(fn)
    # Must start with a letter
    expect_true(grepl("^[A-Za-z]", id),
      info = paste("id does not start with letter for input:", fn, "->", id))
    # Must contain only allowed characters
    expect_true(grepl("^[A-Za-z0-9_\\-\\.]+$", id),
      info = paste("id contains invalid characters for input:", fn, "->", id))
  }
})

test_that("gct_remove_btn_id produces unique ids across a set of typical filenames", {
  fns <- c("file1.csv", "file2.csv", "file3.csv",
           "file-1.csv", "file 2.csv", "file(3).csv")
  ids <- vapply(fns, gct_remove_btn_id, character(1), USE.NAMES = FALSE)
  expect_equal(length(unique(ids)), length(fns),
    info = paste("duplicate ids detected:", paste(ids, collapse = ", ")))
})

test_that("file removal by name works correctly", {
  # Simulate file removal logic (this tests the list-management logic,
  # independent of ID generation).
  files <- data.frame(
    name = c("file1.csv", "file2.csv", "file3.csv"),
    datapath = c("/tmp/file1", "/tmp/file2", "/tmp/file3"),
    stringsAsFactors = FALSE
  )

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
