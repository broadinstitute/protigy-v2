################################################################################
# Phase 1: reference-level picker tests.
#
# Verifies that `lm.regression(..., reference_levels = list(var = "level"))`
# uses the user's chosen reference instead of R's default alphabetical first.
# Without this, users migrating from upstream LinearModelApp_v2 would silently
# get sign-flipped logFCs whenever their intended reference is not the
# alphabetical first level.
################################################################################

library(testthat)

make_two_group_fixture <- function(seed = 401) {
  set.seed(seed)
  n_samples <- 12
  n_feat <- 20
  cdesc <- data.frame(
    id = paste0("s", 1:n_samples),
    group = rep(c("MUT", "WT"), length.out = n_samples),
    row.names = paste0("s", 1:n_samples),
    stringsAsFactors = FALSE
  )
  mat <- matrix(rnorm(n_feat * n_samples), nrow = n_feat,
                dimnames = list(paste0("f", 1:n_feat), rownames(cdesc)))
  # Plant a real effect on f01: MUT > WT by +2.0
  mat[1, cdesc$group == "MUT"] <- mat[1, cdesc$group == "MUT"] + 2.0
  rdesc <- data.frame(id = rownames(mat), row.names = rownames(mat))
  methods::new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc,
               rid = rownames(mat), cid = colnames(mat))
}


test_that("reference_levels arg sets the factor reference, flipping coefficient sign", {
  gct <- make_two_group_fixture()

  # Default (alphabetical) reference is "MUT" (M < W). So `groupWT` coefficient
  # measures WT - MUT. Planted effect was MUT > WT by +2 => coefficient ~ -2.
  res_default <- lm.regression(
    gct = gct,
    formula_string = "~ group",
    variable_types = list(group = "factor"),
    blocking_var = NULL,
    contrasts_list = NULL,
    intensity = FALSE
  )
  default_logfc_col <- grep("^logFC\\.groupWT$", colnames(res_default), value = TRUE)
  expect_length(default_logfc_col, 1)
  default_fc <- res_default[res_default$id == "f1", default_logfc_col]
  expect_length(default_fc, 1)
  expect_lt(default_fc, 0)

  # With reference_levels = list(group = "WT"), reference flips to WT, so the
  # remaining coefficient is `groupMUT` (= MUT - WT), expected ~ +2.
  res_wt_ref <- lm.regression(
    gct = gct,
    formula_string = "~ group",
    variable_types = list(group = "factor"),
    blocking_var = NULL,
    contrasts_list = NULL,
    intensity = FALSE,
    reference_levels = list(group = "WT")
  )
  mut_logfc_col <- grep("^logFC\\.groupMUT$", colnames(res_wt_ref), value = TRUE)
  expect_length(mut_logfc_col, 1)
  wt_ref_fc <- res_wt_ref[res_wt_ref$id == "f1", mut_logfc_col]
  expect_length(wt_ref_fc, 1)
  expect_gt(wt_ref_fc, 0)

  # The magnitudes must be equal (same model, just reparameterised).
  expect_equal(abs(default_fc), abs(wt_ref_fc), tolerance = 1e-8)
})


test_that("reference_levels with an unknown level is ignored with a warning", {
  gct <- make_two_group_fixture()
  expect_warning(
    res <- lm.regression(
      gct = gct,
      formula_string = "~ group",
      variable_types = list(group = "factor"),
      reference_levels = list(group = "NotALevel"),
      intensity = FALSE
    ),
    regexp = "reference level"
  )
  # Falls through to default (alphabetical MUT) - WT coefficient appears.
  expect_true(any(grepl("^logFC\\.groupWT$", colnames(res))))
})
