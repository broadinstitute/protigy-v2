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

test_that("axis title reflects log base and normalization, forcing log2 for None", {
  expect_equal(
    pelsa_splot_axis_title(list(log_transformation = "log2",
                                data_normalization = "Median (non-zero)")),
    "log2(intensity), Median (non-zero) normalized")
  expect_equal(
    pelsa_splot_axis_title(list(log_transformation = "log10",
                                data_normalization = "Quantile")),
    "log10(intensity), Quantile normalized")
  # None log -> forced log2; None normalization -> clause dropped
  expect_equal(
    pelsa_splot_axis_title(list(log_transformation = "None",
                                data_normalization = "None")),
    "log2(intensity)")
  # Missing fields are tolerated
  expect_equal(pelsa_splot_axis_title(list()), "log2(intensity)")
})
