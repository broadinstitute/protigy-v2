################################################################################
# Seam test: a Multi (2x2) contrast expression assembled by build_multi_expr()
# recovers a planted interaction when run through lm.regression()'s contrast
# path. This proves the AUTHORING layer connects to the frozen backend; the
# deep numeric oracles for the contrast path live in test-lm-advanced-contrast.R.
################################################################################

library(testthat)

# Balanced 2x2 factorial with a planted Drug:T2 interaction; references
# Vehicle / T1. Cell-means coded (~ 0 + treatment:timepoint) so all four cells
# are real design coefficients that build_multi_expr can reference by name.
make_2x2_cellmeans_gct <- function(seed = 202L, effect = 3.0, reps = 12L,
                                    n_null = 40L, n_true = 20L) {
  set.seed(seed)
  n_feat <- n_null + n_true
  grid <- expand.grid(
    treatment = c("Vehicle", "Drug"),
    timepoint = c("T1", "T2"),
    rep = seq_len(reps),
    stringsAsFactors = FALSE
  )
  n_samp <- nrow(grid)
  sample_ids <- paste0("s", sprintf("%03d", seq_len(n_samp)))
  feat_ids   <- paste0("F", sprintf("%03d", seq_len(n_feat)))
  cdesc <- data.frame(
    id        = sample_ids,
    treatment = factor(grid$treatment, levels = c("Vehicle", "Drug")),
    timepoint = factor(grid$timepoint, levels = c("T1", "T2")),
    row.names = sample_ids, stringsAsFactors = FALSE
  )
  rdesc <- data.frame(
    id = feat_ids, geneSymbol = paste0("G", seq_len(n_feat)),
    row.names = feat_ids, stringsAsFactors = FALSE
  )
  mat <- matrix(rnorm(n_feat * n_samp, mean = 10, sd = 1),
                nrow = n_feat, dimnames = list(feat_ids, sample_ids))
  drug_t2 <- cdesc$treatment == "Drug" & cdesc$timepoint == "T2"
  mat[seq_len(n_true), drug_t2] <- mat[seq_len(n_true), drug_t2] + effect
  gct <- methods::new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc,
                      rid = feat_ids, cid = sample_ids)
  list(gct = gct, n_true = n_true, n_null = n_null, effect = effect)
}

test_that("build_multi_expr output recovers the planted 2x2 interaction via lm.regression", {
  fx <- make_2x2_cellmeans_gct()

  # Cell-means design: coefficients are the four cells.
  # (Drug:T2 - Drug:T1) - (Vehicle:T2 - Vehicle:T1) == the interaction.
  expr <- build_multi_expr(
    "treatmentDrug:timepointT2", "treatmentDrug:timepointT1",
    "treatmentVehicle:timepointT2", "treatmentVehicle:timepointT1"
  )
  expect_equal(
    expr,
    "(treatmentDrug:timepointT2 - treatmentDrug:timepointT1) - (treatmentVehicle:timepointT2 - treatmentVehicle:timepointT1)"
  )

  res <- lm.regression(
    gct = fx$gct,
    formula_string = "~ 0 + treatment:timepoint",
    variable_types = list(treatment = "factor", timepoint = "factor"),
    contrasts_list = stats::setNames(list(expr), "DoD")
  )

  expect_true(all(c("logFC.DoD", "P.Value.DoD", "adj.P.Val.DoD") %in% colnames(res)))
  tp <- seq_len(fx$n_true)
  nl <- (fx$n_true + 1L):(fx$n_true + fx$n_null)
  # Planted interaction recovered on true features; nulls ~ 0.
  expect_equal(median(res$logFC.DoD[tp]), fx$effect, tolerance = 0.5)
  expect_lt(abs(median(res$logFC.DoD[nl])), 0.4)
  # True block well separated from nulls, and detected at BH 5%.
  expect_gt(median(res$logFC.DoD[tp]), median(res$logFC.DoD[nl]) + 1.5)
  expect_gt(mean(res$adj.P.Val.DoD[tp] < 0.05), 0.9)
})
