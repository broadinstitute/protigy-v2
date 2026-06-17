# Tests for create_corr_heatmap NULL color map guard (P1.2)
# Mirrors the existing NULL guard in create_corr_boxplot (line ~153).

# ---------------------------------------------------------------------------
# Helper: minimal synthetic GCT with a grouping column in cdesc
# ---------------------------------------------------------------------------
make_small_gct <- function(n_genes = 10L, n_samples = 8L) {
  set.seed(42L)
  mat <- matrix(rnorm(n_genes * n_samples), nrow = n_genes, ncol = n_samples)
  rownames(mat) <- paste0("gene_", seq_len(n_genes))
  colnames(mat) <- paste0("sample_", seq_len(n_samples))

  cdesc <- data.frame(
    group = rep(c("A", "B"), each = n_samples / 2L),
    row.names = paste0("sample_", seq_len(n_samples))
  )
  rdesc <- data.frame(
    id = paste0("gene_", seq_len(n_genes)),
    row.names = paste0("gene_", seq_len(n_genes))
  )

  new("GCT",
    mat    = mat,
    cdesc  = cdesc,
    rdesc  = rdesc,
    rid    = rownames(mat),
    cid    = colnames(mat)
  )
}

# Discrete color map that matches the grouping variable
make_discrete_color_map <- function() {
  list(
    is_discrete = TRUE,
    colors      = list("red", "blue"),
    vals        = c("A", "B")
  )
}

# ---------------------------------------------------------------------------
# P1.2-A: NULL custom_color_map must NOT error
# ---------------------------------------------------------------------------
test_that("create_corr_heatmap does not crash when custom_color_map is NULL", {
  gct <- make_small_gct()

  result <- tryCatch(
    create_corr_heatmap(gct, "group", "proteome", custom_color_map = NULL),
    error = function(e) e
  )

  # Must not be an error object
  expect_false(
    inherits(result, "error"),
    info = paste("Unexpected error:", if (inherits(result, "error")) conditionMessage(result) else "none")
  )

  # Must return a list with $HM and $Table slots
  expect_true(is.list(result))
  expect_true(!is.null(result$HM))
  expect_true(!is.null(result$Table))
})

# ---------------------------------------------------------------------------
# P1.2-B: non-NULL discrete color map continues to work (regression guard)
# ---------------------------------------------------------------------------
test_that("create_corr_heatmap works correctly with a non-NULL discrete color map", {
  gct     <- make_small_gct()
  cmap    <- make_discrete_color_map()

  result <- tryCatch(
    create_corr_heatmap(gct, "group", "proteome", custom_color_map = cmap),
    error = function(e) e
  )

  expect_false(
    inherits(result, "error"),
    info = paste("Unexpected error:", if (inherits(result, "error")) conditionMessage(result) else "none")
  )

  expect_true(is.list(result))
  expect_true(!is.null(result$HM))
  expect_true(!is.null(result$Table))
})

# ---------------------------------------------------------------------------
# P1.2-C: correlation matrix dimensions are correct regardless of color map
# ---------------------------------------------------------------------------
test_that("create_corr_heatmap returns a square correlation matrix", {
  gct <- make_small_gct()

  result_null <- create_corr_heatmap(gct, "group", "proteome", custom_color_map = NULL)
  result_cmap <- create_corr_heatmap(gct, "group", "proteome", custom_color_map = make_discrete_color_map())

  n <- ncol(gct@mat)
  expect_equal(dim(result_null$Table), c(n, n))
  expect_equal(dim(result_cmap$Table), c(n, n))
})

# ---------------------------------------------------------------------------
# P1.2-D: non-NULL CONTINUOUS color map must not error
# ---------------------------------------------------------------------------
make_continuous_color_map <- function() {
  list(
    is_discrete = FALSE,
    colors      = list(low = "blue", mid = "white", high = "red", na_color = "gray50"),
    vals        = c("low", "mid", "high", "na_color")
  )
}

test_that("create_corr_heatmap works correctly with a non-NULL continuous color map", {
  gct  <- make_small_gct()
  cmap <- make_continuous_color_map()

  result <- tryCatch(
    create_corr_heatmap(gct, "group", "proteome", custom_color_map = cmap),
    error = function(e) e
  )

  expect_false(
    inherits(result, "error"),
    info = paste("Unexpected error:", if (inherits(result, "error")) conditionMessage(result) else "none")
  )

  expect_true(is.list(result))
  expect_true(!is.null(result$HM))
  expect_true(!is.null(result$Table))
})
