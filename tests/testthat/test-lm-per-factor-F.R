################################################################################
# Phase 1.2: per-factor F-test extraction.
#
# Trigger rule (reviewer option (c)):
#   - Emit F.<factor>, P.Value.<factor>, adj.P.Val.<factor> only when the factor
#     contributes more than one non-intercept coefficient to the design
#     (i.e. >2 levels, OR is bound to an interaction term).
#   - Do not emit logFC.<factor> or logSignP.<factor> for F-tests (no single
#     signed effect).
#   - The F-test p-value MUST be drawn from `topTable(coef=<vec>)$P.Value` and
#     adjusted via `adj.P.Val` in the same call (NOT the upstream's bug of
#     extracting raw P.Value and labeling it adj).
################################################################################

library(testthat)

source(file.path("..", "lm-sandbox", "compare", "assert_equivalent.R"))


make_type1_gct <- function() {
  fx <- readRDS(file.path("..", "lm-sandbox", "data", "type1_rm_with_groups.rds"))
  cdesc <- fx$cdesc
  cdesc$group <- factor(cdesc$group, levels = c("WT", "MUT"))
  cdesc$time <- factor(cdesc$time, levels = c("T1", "T2", "T3"))
  rdesc <- fx$rdesc
  methods::new("GCT", mat = fx$mat, rdesc = rdesc, cdesc = cdesc,
               rid = rownames(fx$mat), cid = colnames(fx$mat))
}


test_that("per-factor F-tests appear only for multi-coefficient variables (option c)", {
  gct <- make_type1_gct()
  res <- lm.regression(
    gct = gct,
    formula_string = "~ group + time + group:time",
    variable_types = list(group = "factor", time = "factor"),
    blocking_var = "subject",
    contrasts_list = NULL,
    intensity = FALSE,
    reference_levels = list(group = "WT", time = "T1")
  )

  # group has 2 levels -> 1 coef -> NO F column.
  expect_false(any(grepl("^F\\.group$", colnames(res))))
  # time has 3 levels -> 2 coefs -> F column present.
  expect_true(any(colnames(res) == "F.time"))
  expect_true(any(colnames(res) == "P.Value.time"))
  expect_true(any(colnames(res) == "adj.P.Val.time"))
  # Interaction has 2 coefs -> F column present.
  expect_true(any(colnames(res) == "F.group.time"))
  # No logFC / logSignP for F-tests.
  expect_false(any(colnames(res) == "logFC.time"))
  expect_false(any(colnames(res) == "logSignP.time"))
})


test_that("F-test values match a manual limma topTable run within tolerance", {
  gct <- make_type1_gct()
  res <- lm.regression(
    gct = gct,
    formula_string = "~ group + time + group:time",
    variable_types = list(group = "factor", time = "factor"),
    blocking_var = "subject",
    intensity = FALSE,
    reference_levels = list(group = "WT", time = "T1")
  )
  golden <- readRDS(file.path("..", "lm-sandbox", "golden", "type1_rm_with_groups.rds"))
  time_F_golden <- golden$factor_F$time
  # Match by id ordering
  res_ord <- res[match(rownames(time_F_golden), res$id), , drop = FALSE]
  # F values within 1e-4 absolute (slightly relaxed because protigy passes the
  # full pipeline through make.names() which doesn't affect numerics but the
  # internal sort.by="none" must be respected).
  expect_equal(res_ord$F.time, time_F_golden$F, tolerance = 1e-4)
  expect_equal(res_ord$adj.P.Val.time, time_F_golden$adj.P.Val, tolerance = 1e-6)
})
