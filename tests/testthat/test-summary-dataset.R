################################################################################
# Tests for summary_dataset() in R/tab_summary_helpers.R
#
# P1.3 regression: the "Features w/o quantification" row was never appended
# because the return value of append() was discarded.  These tests verify:
#   1. GCT with at least one all-NA row -> row present with correct count.
#   2. GCT with NO all-NA rows          -> row absent.
#   3. Row is positioned immediately after "Features (post-filtering)".
################################################################################

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Build a minimal synthetic GCT with the given matrix, groups, and annotation
# column name.  Row IDs are r1..rN, column IDs are s1..sM.
make_gct <- function(mat, groups, annotation_column = "group") {
  n_feat <- nrow(mat)
  n_samp <- ncol(mat)
  rid <- paste0("r", seq_len(n_feat))
  cid <- paste0("s", seq_len(n_samp))
  rownames(mat) <- rid
  colnames(mat) <- cid
  cdesc <- data.frame(
    setNames(list(groups), annotation_column),
    row.names = cid,
    stringsAsFactors = FALSE
  )
  new("GCT",
      mat   = mat,
      rid   = rid,
      cid   = cid,
      cdesc = cdesc,
      rdesc = data.frame(id = rid, row.names = rid, stringsAsFactors = FALSE))
}

# Minimal params list; only annotation_column is used by summary_dataset.
make_params <- function(annotation_column = "group") {
  list(annotation_column = annotation_column)
}

# ---------------------------------------------------------------------------
# P1.3 - row present when there are all-NA features
# ---------------------------------------------------------------------------

test_that("'Features w/o quantification' row appears when all-NA rows exist", {
  # Arrange: 4 features, 3 samples; row 3 is entirely NA
  mat_orig <- matrix(c(1, 2, 3, 4,
                       5, 6, 7, 8,
                       9, 10, 11, 12),
                     nrow = 4, ncol = 3)
  mat_proc <- mat_orig
  mat_proc[3, ] <- NA  # row 3: all-NA (unquantified)

  gct_orig <- make_gct(mat_orig, groups = c("A", "B", "A"))
  gct_proc <- make_gct(mat_proc, groups = c("A", "B", "A"))
  params   <- make_params()

  # Act
  result <- Protigy:::summary_dataset(params, gct_orig, gct_proc)

  # Assert: row exists
  expect_true(
    "Features w/o quantification" %in% rownames(result),
    info = "Expected 'Features w/o quantification' row to be present"
  )
})

test_that("'Features w/o quantification' count equals number of all-NA rows", {
  # Arrange: 5 features, 3 samples; rows 2 and 4 are entirely NA
  mat_orig <- matrix(seq_len(15), nrow = 5, ncol = 3)
  mat_proc <- mat_orig
  mat_proc[2, ] <- NA
  mat_proc[4, ] <- NA

  gct_orig <- make_gct(mat_orig, groups = c("A", "B", "A"))
  gct_proc <- make_gct(mat_proc, groups = c("A", "B", "A"))
  params   <- make_params()

  # Act
  result <- Protigy:::summary_dataset(params, gct_orig, gct_proc)

  # Assert: count is 2
  expect_equal(
    as.integer(result["Features w/o quantification", "Number"]),
    2L,
    info = "Expected count of 2 unquantified features"
  )
})

# ---------------------------------------------------------------------------
# P1.3 - row absent when no all-NA features
# ---------------------------------------------------------------------------

test_that("'Features w/o quantification' row is absent when no all-NA rows", {
  # Arrange: 3 features, 2 samples; all cells have values
  mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
  gct_orig <- make_gct(mat, groups = c("A", "B"))
  gct_proc <- make_gct(mat, groups = c("A", "B"))
  params   <- make_params()

  # Act
  result <- Protigy:::summary_dataset(params, gct_orig, gct_proc)

  # Assert: row absent
  expect_false(
    "Features w/o quantification" %in% rownames(result),
    info = "Row should be absent when no features are unquantified"
  )
})

# ---------------------------------------------------------------------------
# P1.3 - row ordering: immediately after "Features (post-filtering)"
# ---------------------------------------------------------------------------

test_that("'Features w/o quantification' row follows 'Features (post-filtering)'", {
  # Arrange: 3 features, 2 samples; row 1 is all-NA
  mat_orig <- matrix(c(10, 20, 30, 11, 21, 31), nrow = 3, ncol = 2)
  mat_proc <- mat_orig
  mat_proc[1, ] <- NA

  gct_orig <- make_gct(mat_orig, groups = c("A", "B"))
  gct_proc <- make_gct(mat_proc, groups = c("A", "B"))
  params   <- make_params()

  # Act
  result <- Protigy:::summary_dataset(params, gct_orig, gct_proc)
  rnames <- rownames(result)

  # Assert: position
  pos_post <- which(rnames == "Features (post-filtering)")
  pos_unq  <- which(rnames == "Features w/o quantification")

  expect_equal(
    pos_unq, pos_post + 1L,
    info = paste(
      "'Features w/o quantification' should be the row immediately after",
      "'Features (post-filtering)'; got positions", pos_post, "and", pos_unq
    )
  )
})
