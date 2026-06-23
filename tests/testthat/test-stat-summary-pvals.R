# Phase 3 (P3.7) -- get_pvals nominal-vs-adjusted column disambiguation
# (R/tab_stat_summary_helpers.R).
#
# get_pvals greps for "<pval_type>\\.<contrast>" in the stat-results columns. The
# nominal pattern "P.Value\\.<contrast>" is a SUBSTRING of "Log.P.Value.<contrast>",
# so the column ORDER is what keeps them apart in production (stat.testing always
# emits P.Value before its Log.P.Value). These tests pin the CURRENT behavior and
# document the order-dependency (NOT a confirmed active bug -- production ordering
# protects it; see report).

make_stat_df <- function(log_first = FALSE) {
  cols <- list(
    feature = c("g1", "g2", "g3"),
    "P.Value.A_over_B"     = c(0.01, 0.50, 0.90),
    "Log.P.Value.A_over_B" = c(2.0, 0.30, 0.05),
    "adj.P.Val.A_over_B"   = c(0.03, 0.60, 0.95)
  )
  if (log_first) {
    cols <- cols[c("feature", "Log.P.Value.A_over_B",
                   "P.Value.A_over_B", "adj.P.Val.A_over_B")]
  }
  as.data.frame(cols, check.names = FALSE, stringsAsFactors = FALSE)
}

test_that("get_pvals picks the nominal P.Value column for a two-sample test", {
  sr <- list(prot = make_stat_df())
  sp <- list(prot = list(test = "Two-sample Moderated T-test"))
  res <- get_pvals("prot", sp, sr, group = NULL, contrast = "A / B",
                   pval_type = "P.Value")
  expect_equal(res, c(0.01, 0.50, 0.90))
})

test_that("get_pvals picks the adjusted column when asked", {
  sr <- list(prot = make_stat_df())
  sp <- list(prot = list(test = "Two-sample Moderated T-test"))
  res <- get_pvals("prot", sp, sr, group = NULL, contrast = "A / B",
                   pval_type = "adj.P.Val")
  expect_equal(res, c(0.03, 0.60, 0.95))
})

test_that("get_pvals drops NA p-values", {
  df <- make_stat_df()
  df[["P.Value.A_over_B"]][2] <- NA
  sr <- list(prot = df)
  sp <- list(prot = list(test = "Two-sample Moderated T-test"))
  res <- get_pvals("prot", sp, sr, group = NULL, contrast = "A / B",
                   pval_type = "P.Value")
  expect_equal(res, c(0.01, 0.90))
})

test_that("DOCUMENTED order-dependency: nominal grep also matches Log.P.Value", {
  # When Log.P.Value precedes P.Value in column order, get_pvals' grep returns the
  # Log.P.Value column FIRST -> it is selected instead of the nominal p-value.
  # This pins the current (order-sensitive) behavior; production column order
  # (P.Value before Log.P.Value) is what avoids the collision in practice.
  sr <- list(prot = make_stat_df(log_first = TRUE))
  sp <- list(prot = list(test = "Two-sample Moderated T-test"))
  res <- get_pvals("prot", sp, sr, group = NULL, contrast = "A / B",
                   pval_type = "P.Value")
  # With Log.P.Value first, the grabbed values are the LOG values, NOT the nominal
  # p-values -- demonstrating the latent collision the column order normally hides.
  expect_equal(res, c(2.0, 0.30, 0.05))
})

test_that("get_pvals one-sample branch matches on group keyword + pval_type", {
  df <- data.frame(
    check.names = FALSE, stringsAsFactors = FALSE,
    feature = c("g1", "g2"),
    "P.Value.treated" = c(0.02, 0.40),
    "adj.P.Val.treated" = c(0.05, 0.50)
  )
  sr <- list(prot = df)
  sp <- list(prot = list(test = "One-sample Moderated T-test"))
  res <- get_pvals("prot", sp, sr, group = "treated", contrast = NULL,
                   pval_type = "P.Value")
  expect_equal(res, c(0.02, 0.40))
})

test_that("get_pvals F-test branch matches the bare pval_type pattern", {
  df <- data.frame(
    check.names = FALSE, stringsAsFactors = FALSE,
    feature = c("g1", "g2"),
    "P.Value" = c(0.07, 0.80),
    "adj.P.Val" = c(0.10, 0.90)
  )
  sr <- list(prot = df)
  sp <- list(prot = list(test = "Moderated F test"))
  res <- get_pvals("prot", sp, sr, group = NULL, contrast = NULL,
                   pval_type = "adj.P.Val")
  expect_equal(res, c(0.10, 0.90))
})
