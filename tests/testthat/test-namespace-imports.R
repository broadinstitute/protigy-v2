################################################################################
# Regression guard for the normalization dependencies.
#
# R/sidebar_setup_helpers_normalization.R reaches its optional-engine deps via
# fully QUALIFIED calls -- preprocessCore::normalize.quantiles, vsn::justvsn,
# mixtools::normalmixEM and mclust::Mclust/mclustBIC. Those four packages are
# Imports-only (loaded, not attached), so the qualified calls fail at runtime
# with "there is no package called ..." if a package is ever dropped from
# DESCRIPTION Imports. This test pins each function to its package namespace so
# that an accidental Imports removal is caught before it reaches users.
#
# (Earlier these symbols were called unqualified and pinned via importFrom in
# NAMESPACE; the normalization helper was later refactored to qualified calls,
# so the guard now checks namespace reachability instead.)
################################################################################

library(testthat)

test_that("normalization dependencies are reachable via their namespaces", {
  needed <- list(
    normalize.quantiles = "preprocessCore",
    justvsn             = "vsn",
    normalmixEM         = "mixtools",
    Mclust              = "mclust",
    mclustBIC           = "mclust"
  )
  for (fn in names(needed)) {
    pkg <- needed[[fn]]
    expect_true(
      requireNamespace(pkg, quietly = TRUE),
      info = sprintf("package '%s' (provides %s) is not installed/available", pkg, fn)
    )
    expect_true(
      exists(fn, envir = asNamespace(pkg), inherits = FALSE),
      info = sprintf(
        "%s is called as %s::%s in sidebar_setup_helpers_normalization.R but is not exported by '%s'",
        fn, pkg, fn, pkg
      )
    )
  }
})
