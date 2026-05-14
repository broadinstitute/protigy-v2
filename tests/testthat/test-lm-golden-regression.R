################################################################################
# Phase 6: golden-file regression tests.
#
# For each of the five sandbox fixtures, run `lm.regression()` (the production
# entry point) with a protigy-v2-equivalent configuration that mirrors the
# manual limma run that generated the golden RDS. Then assert per-coefficient
# numerical equivalence within tolerance.
#
# These tests validate that protigy-v2 calls limma in a way that produces the
# same numbers as a hand-run limma pipeline. The implicit assumption is that
# limma itself is correct.
################################################################################

library(testthat)

# Load tolerance helpers
source(file.path("..", "lm-sandbox", "compare", "assert_equivalent.R"))

DATA_DIR <- file.path("..", "lm-sandbox", "data")
GOLDEN_DIR <- file.path("..", "lm-sandbox", "golden")


# Helper to wrap a fixture in a GCT.
wrap_gct <- function(fx) {
  rdesc <- if (!is.null(fx$rdesc)) fx$rdesc else data.frame(
    id = rownames(fx$mat), row.names = rownames(fx$mat)
  )
  methods::new(
    "GCT",
    mat = fx$mat, cdesc = fx$cdesc, rdesc = rdesc,
    rid = rownames(fx$mat), cid = colnames(fx$mat)
  )
}


# ---- Type 3 (contrasts) ------------------------------------------------------
test_that("golden: type3 contrasts (~ 0 + condition, B-A & C-A)", {
  fx <- readRDS(file.path(DATA_DIR, "type3_contrasts.rds"))
  golden <- readRDS(file.path(GOLDEN_DIR, "type3_contrasts.rds"))
  gct <- wrap_gct(fx)

  res <- lm.regression(
    gct = gct,
    formula_string = "~ 0 + condition",
    variable_types = list(condition = "factor"),
    blocking_var = NULL,
    contrasts_list = list(B_vs_A = "conditionB - conditionA",
                          C_vs_A = "conditionC - conditionA"),
    intensity = FALSE
  )

  for (cn in c("B_vs_A", "C_vs_A")) {
    g <- golden$per_coef[[cn]]
    safe_cn <- make.names(cn)
    lf_col <- paste0("logFC.", safe_cn)
    pv_col <- paste0("P.Value.", safe_cn)
    ap_col <- paste0("adj.P.Val.", safe_cn)
    expect_true(all(c(lf_col, pv_col, ap_col) %in% colnames(res)))
    # Align by id
    res_ord <- res[match(rownames(g), res$id), , drop = FALSE]
    expect_equal(res_ord[[lf_col]], g$logFC, tolerance = 1e-6)
    expect_equal(res_ord[[pv_col]], g$P.Value, tolerance = 1e-6)
    expect_equal(res_ord[[ap_col]], g$adj.P.Val, tolerance = 1e-6)
  }
})


# ---- Continuous covariate ----------------------------------------------------
test_that("golden: continuous covariate (~ age)", {
  fx <- readRDS(file.path(DATA_DIR, "continuous_covariate.rds"))
  golden <- readRDS(file.path(GOLDEN_DIR, "continuous_covariate.rds"))
  gct <- wrap_gct(fx)

  res <- lm.regression(
    gct = gct,
    formula_string = "~ age",
    variable_types = list(age = "continuous"),
    intensity = FALSE
  )

  g <- golden$per_coef$age
  res_ord <- res[match(rownames(g), res$id), , drop = FALSE]
  expect_equal(res_ord$logFC.age, g$logFC, tolerance = 1e-6)
  expect_equal(res_ord$P.Value.age, g$P.Value, tolerance = 1e-6)
  expect_equal(res_ord$adj.P.Val.age, g$adj.P.Val, tolerance = 1e-6)
})


# ---- Type 2 (RM only, ~ time blocked on subject) -----------------------------
test_that("golden: type2 RM only (~ time + block(subject))", {
  fx <- readRDS(file.path(DATA_DIR, "type2_rm_only.rds"))
  golden <- readRDS(file.path(GOLDEN_DIR, "type2_rm_only.rds"))
  gct <- wrap_gct(fx)

  res <- lm.regression(
    gct = gct,
    formula_string = "~ time",
    variable_types = list(time = "factor"),
    blocking_var = "subject",
    intensity = FALSE,
    reference_levels = list(time = "T1")
  )

  for (cn in names(golden$per_coef)) {
    g <- golden$per_coef[[cn]]
    safe_cn <- make.names(cn)
    lf_col <- paste0("logFC.", safe_cn)
    expect_true(lf_col %in% colnames(res))
    res_ord <- res[match(rownames(g), res$id), , drop = FALSE]
    expect_equal(res_ord[[lf_col]], g$logFC, tolerance = 1e-6)
    expect_equal(res_ord[[paste0("P.Value.", safe_cn)]], g$P.Value, tolerance = 1e-6)
    expect_equal(res_ord[[paste0("adj.P.Val.", safe_cn)]], g$adj.P.Val, tolerance = 1e-6)
  }

  # Per-factor F-test for `time` (3 levels -> 2 coefs).
  expect_true("F.time" %in% colnames(res))
  expect_true("P.Value.time" %in% colnames(res))
  expect_true("adj.P.Val.time" %in% colnames(res))
  g_time <- golden$factor_F$time
  res_ord <- res[match(rownames(g_time), res$id), , drop = FALSE]
  expect_equal(res_ord$F.time, g_time$F, tolerance = 1e-4)
  expect_equal(res_ord$adj.P.Val.time, g_time$adj.P.Val, tolerance = 1e-6)
})


# ---- Type 1 (RM with groups, full interaction, blocked on subject) -----------
test_that("golden: type1 RM with groups (~ group + time + group:time, block subject)", {
  fx <- readRDS(file.path(DATA_DIR, "type1_rm_with_groups.rds"))
  golden <- readRDS(file.path(GOLDEN_DIR, "type1_rm_with_groups.rds"))
  gct <- wrap_gct(fx)

  res <- lm.regression(
    gct = gct,
    formula_string = "~ group + time + group:time",
    variable_types = list(group = "factor", time = "factor"),
    blocking_var = "subject",
    intensity = FALSE,
    reference_levels = list(group = "WT", time = "T1")
  )

  for (cn in names(golden$per_coef)) {
    g <- golden$per_coef[[cn]]
    safe_cn <- make.names(cn)
    res_ord <- res[match(rownames(g), res$id), , drop = FALSE]
    expect_equal(res_ord[[paste0("logFC.", safe_cn)]], g$logFC, tolerance = 1e-6)
    expect_equal(res_ord[[paste0("P.Value.", safe_cn)]], g$P.Value, tolerance = 1e-6)
    expect_equal(res_ord[[paste0("adj.P.Val.", safe_cn)]], g$adj.P.Val, tolerance = 1e-6)
  }

  # time has 3 levels -> F.time emitted.
  expect_true("F.time" %in% colnames(res))
  # group has 2 levels -> NO F.group.
  expect_false(any(grepl("^F\\.group$", colnames(res))))
  # Interaction has 2 coefs -> F.group.time emitted.
  expect_true("F.group.time" %in% colnames(res))
})


# ---- Intensity-trend (~ condition, intensity = TRUE) -------------------------
test_that("golden: intensity trend (~ condition, intensity=TRUE)", {
  fx <- readRDS(file.path(DATA_DIR, "intensity_trend.rds"))
  golden <- readRDS(file.path(GOLDEN_DIR, "intensity_trend.rds"))
  gct <- wrap_gct(fx)

  res_trend <- lm.regression(
    gct = gct,
    formula_string = "~ condition",
    variable_types = list(condition = "factor"),
    intensity = TRUE
  )
  g_trend <- golden$per_coef_trend$conditionTrt
  res_ord <- res_trend[match(rownames(g_trend), res_trend$id), , drop = FALSE]
  expect_equal(res_ord$logFC.conditionTrt, g_trend$logFC, tolerance = 1e-6)
  expect_equal(res_ord$P.Value.conditionTrt, g_trend$P.Value, tolerance = 1e-6)
  expect_equal(res_ord$adj.P.Val.conditionTrt, g_trend$adj.P.Val, tolerance = 1e-6)

  # And intensity=FALSE should match the no-trend golden, proving the toggle does
  # something.
  res_notrend <- lm.regression(
    gct = gct,
    formula_string = "~ condition",
    variable_types = list(condition = "factor"),
    intensity = FALSE
  )
  g_notrend <- golden$per_coef_notrend$conditionTrt
  res_ord <- res_notrend[match(rownames(g_notrend), res_notrend$id), , drop = FALSE]
  expect_equal(res_ord$logFC.conditionTrt, g_notrend$logFC, tolerance = 1e-6)
  expect_equal(res_ord$P.Value.conditionTrt, g_notrend$P.Value, tolerance = 1e-6)
})
