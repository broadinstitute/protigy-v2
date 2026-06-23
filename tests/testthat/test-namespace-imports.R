################################################################################
# Regression guard for NAMESPACE / roxygen import desync.
#
# The normalization helpers in R/sidebar_setup_helpers_normalization.R call
# preprocessCore::normalize.quantiles, vsn::justvsn, mixtools::normalmixEM and
# mclust::Mclust/mclustBIC by their BARE (unqualified) names. Those four packages
# are Imports-only (loaded, not attached), so each symbol MUST be importFrom'd in
# NAMESPACE or the normalization code paths fail at runtime with
# "could not find function". A dependency cleanup once dropped these importFrom
# lines while leaving the unqualified calls in place; this test prevents that
# regression from recurring.
################################################################################

library(testthat)

test_that("normalization dependencies are imported into the Protigy namespace", {
  imports_env <- parent.env(asNamespace("Protigy"))
  needed <- c("normalize.quantiles", "justvsn", "normalmixEM", "Mclust", "mclustBIC")
  for (fn in needed) {
    expect_true(
      exists(fn, envir = imports_env, inherits = FALSE),
      info = sprintf(
        "%s is called unqualified in sidebar_setup_helpers_normalization.R but is not importFrom'd in NAMESPACE",
        fn
      )
    )
  }
})
