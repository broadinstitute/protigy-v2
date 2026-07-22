################################################################################
# Tests for the Linear Model backend (lm.regression) guards added in the
# refactor of tab_lm_setup / tab_lm_setup_helpers.
################################################################################

library(testthat)

# Build a minimal GCT fixture with a few features, two factor predictors,
# and a blocking variable (subject id).
make_fixture_gct <- function(n_samples = 12, seed = 1) {
  set.seed(seed)
  mat <- matrix(rnorm(n_samples * 20), nrow = 20,
                dimnames = list(paste0("f", 1:20),
                                paste0("s", 1:n_samples)))
  cdesc <- data.frame(
    id = paste0("s", 1:n_samples),
    group = factor(rep(c("A", "B"), length.out = n_samples)),
    time = factor(rep(c("T1", "T2", "T3"), length.out = n_samples)),
    participant = factor(rep(paste0("p", 1:(n_samples / 2)), each = 2)),
    stringsAsFactors = FALSE,
    row.names = paste0("s", 1:n_samples)
  )
  rdesc <- data.frame(id = rownames(mat), row.names = rownames(mat),
                      stringsAsFactors = FALSE)
  methods::new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc,
               rid = rownames(mat), cid = colnames(mat))
}


test_that("lm.regression rejects a blocking var that is also a fixed effect", {
  gct <- make_fixture_gct()
  expect_error(
    lm.regression(
      gct = gct,
      formula_string = "~ group + time",
      variable_types = list(group = "factor", time = "factor"),
      blocking_var = "group",  # <- overlap with formula
      contrasts_list = NULL,
      intensity = FALSE
    ),
    "cannot also appear in the model formula"
  )
})


test_that("lm.regression runs with a valid blocking variable disjoint from formula", {
  gct <- make_fixture_gct()
  res <- lm.regression(
    gct = gct,
    formula_string = "~ group + time",
    variable_types = list(group = "factor", time = "factor"),
    blocking_var = "participant",
    contrasts_list = NULL,
    intensity = FALSE
  )
  expect_true(is.data.frame(res))
  expect_true("id" %in% colnames(res))
  # At least one logFC column should exist for a non-intercept coefficient
  expect_true(any(grepl("^logFC\\.", colnames(res))))
})


test_that("lm.regression warns when design matrix is rank-deficient", {
  gct <- make_fixture_gct()
  # Add a redundant column (identical to group) to force rank deficiency
  gct@cdesc$group_copy <- gct@cdesc$group
  expect_warning(
    lm.regression(
      gct = gct,
      formula_string = "~ group + group_copy",
      variable_types = list(group = "factor", group_copy = "factor"),
      blocking_var = NULL,
      contrasts_list = NULL,
      intensity = FALSE
    ),
    "rank-deficient"
  )
})


test_that("lm.regression errors when a factor collapses to one level after NA filter", {
  gct <- make_fixture_gct()
  # Remove all samples where group=='B' via NA in an unrelated column, so
  # complete-case filtering drops those rows.
  gct@cdesc$aux <- ifelse(gct@cdesc$group == "B", NA_character_, "x")
  gct@cdesc$aux <- factor(gct@cdesc$aux)
  expect_error(
    lm.regression(
      gct = gct,
      formula_string = "~ group + aux",
      variable_types = list(group = "factor", aux = "factor"),
      blocking_var = NULL,
      contrasts_list = NULL,
      intensity = FALSE
    ),
    "only one level after filtering"
  )
})


test_that("lm.regression accepts contrast strings that reference interaction terms", {
  # Interaction-term column names contain ":" (e.g., "groupB:timeT2") which is
  # not a syntactically valid R name. makeContrasts() rejects such level names,
  # so lm.regression must rename both the design and the contrast strings via
  # make.names() before calling makeContrasts.
  gct <- make_fixture_gct()
  res <- lm.regression(
    gct = gct,
    formula_string = "~ group + time + group:time",
    variable_types = list(group = "factor", time = "factor"),
    blocking_var = NULL,
    contrasts_list = list(C1 = "groupB:timeT2 - groupB:timeT3"),
    intensity = FALSE
  )
  expect_true(is.data.frame(res))
  # The contrast result column should appear under the user-supplied name "C1"
  expect_true(any(grepl("^logFC\\.C1$", colnames(res))))
})


test_that("suggest_alpha_level returns NA for empty input and accepts nominal p-values", {
  expect_equal(suggest_alpha_level(numeric(0))$alpha, NA)
  # Uniform null: no signal -> either returns an alpha or NA, but shouldn't error.
  set.seed(42)
  p_null <- runif(500)
  res <- suggest_alpha_level(p_null)
  expect_true(is.list(res))
  expect_true(all(c("alpha", "message") %in% names(res)))
})
