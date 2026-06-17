################################################################################
# Tests for min_samples_message (R/utilities.R)
#
# Single-sample omes cannot support analyses that require >= 2 samples
# (PCA, Correlation, CV). The shared helper returns NULL when the gate is
# satisfied and an interpolated message string when it is not, so each QC
# section can grey out via validate(need(...)) with one consistent message.
################################################################################

make_gct_n_samples <- function(n_samples, n_features = 5) {
  mat <- matrix(
    seq_len(n_features * max(n_samples, 1)),
    nrow = n_features,
    ncol = max(n_samples, 1)
  )
  rownames(mat) <- paste0("gene", seq_len(n_features))
  cids <- paste0("sample", seq_len(max(n_samples, 1)))
  colnames(mat) <- cids
  if (n_samples == 0) {
    mat <- mat[, integer(0), drop = FALSE]
    cids <- character(0)
  }
  new("GCT",
      mat = mat,
      cdesc = data.frame(group = rep("A", length(cids)), row.names = cids),
      rdesc = data.frame(gene_name = rownames(mat), row.names = rownames(mat)),
      rid = rownames(mat),
      cid = cids)
}

test_that("min_samples_message returns NULL when sample count meets threshold", {
  expect_null(min_samples_message(make_gct_n_samples(2), n = 2, analysis = "PCA"))
  expect_null(min_samples_message(make_gct_n_samples(10), n = 2, analysis = "PCA"))
})

test_that("min_samples_message returns an interpolated message below threshold", {
  msg <- min_samples_message(make_gct_n_samples(1), n = 2, analysis = "PCA")
  expect_type(msg, "character")
  expect_match(msg, "PCA", fixed = TRUE)
  expect_match(msg, "at least 2 samples", fixed = TRUE)
  # reports the actual count (singular)
  expect_match(msg, "1 sample", fixed = TRUE)
})

test_that("min_samples_message uses the analysis label provided", {
  expect_match(
    min_samples_message(make_gct_n_samples(1), n = 2, analysis = "Correlation"),
    "Correlation", fixed = TRUE
  )
  expect_match(
    min_samples_message(make_gct_n_samples(1), n = 2, analysis = "CV"),
    "CV", fixed = TRUE
  )
})

test_that("min_samples_message handles zero samples defensively", {
  msg <- min_samples_message(make_gct_n_samples(0), n = 2, analysis = "PCA")
  expect_type(msg, "character")
  expect_match(msg, "0 samples", fixed = TRUE)
})

test_that("min_samples_message message is ASCII only", {
  msg <- min_samples_message(make_gct_n_samples(1), n = 2, analysis = "PCA")
  # project rule: ASCII-only strings in source
  expect_false(grepl("[^[:print:]]", msg))
})
