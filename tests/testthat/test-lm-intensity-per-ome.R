################################################################################
# Phase 3: intensity flag per-ome consumption.
#
# Reviewer §2.4: when `Apply to all datasets` is active, `intensity_param` is
# read once OUTSIDE the per-ome loop at tab_lm_setup.R:1077, so every ome ends
# up using the selected ome's `intensity` setting even if other omes have a
# different setting in `parameters()[[ome]]$intensity`.
#
# Extract a small pure helper so we can unit-test it.
################################################################################

library(testthat)


test_that("pick_intensity_for_ome reads the per-ome flag correctly", {
  parameters <- list(
    protein  = list(intensity = TRUE,  annotation_column = "geneSymbol"),
    peptide  = list(intensity = FALSE, annotation_column = "id")
  )
  expect_true(pick_intensity_for_ome(parameters, "protein"))
  expect_false(pick_intensity_for_ome(parameters, "peptide"))
})


test_that("pick_intensity_for_ome defaults to FALSE for missing keys", {
  expect_false(pick_intensity_for_ome(list(), "anything"))
  expect_false(pick_intensity_for_ome(list(protein = list()), "protein"))
  expect_false(pick_intensity_for_ome(list(protein = list(intensity = NULL)), "protein"))
})


test_that("pick_intensity_for_ome coerces string / numeric forms to logical", {
  parameters <- list(
    a = list(intensity = "true"),
    b = list(intensity = "yes"),
    c = list(intensity = "1"),
    d = list(intensity = 1),
    e = list(intensity = "false"),
    f = list(intensity = 0)
  )
  expect_true(pick_intensity_for_ome(parameters, "a"))
  expect_true(pick_intensity_for_ome(parameters, "b"))
  expect_true(pick_intensity_for_ome(parameters, "c"))
  expect_true(pick_intensity_for_ome(parameters, "d"))
  expect_false(pick_intensity_for_ome(parameters, "e"))
  expect_false(pick_intensity_for_ome(parameters, "f"))
})
