################################################################################
# Tests for the PELSA Summary intensity-rank (S-plot) panel.
# PURE helpers (no Shiny) + light testServer. No network.
################################################################################

library(testthat)

test_that("S-plot constants have the expected fixed values", {
  expect_identical(.PELSA_TRYPSIN_ACCESSIONS, c("Q29463", "P00760", "P00761"))
  expect_identical(.PELSA_SPLOT_TOP_N, 3L)
  expect_match(.PELSA_SPLOT_TRYPSIN_COLOR, "^#[0-9A-Fa-f]{6}$")
  expect_identical(.PELSA_SPLOT_SUBDIR, "intensity_rank")
})
