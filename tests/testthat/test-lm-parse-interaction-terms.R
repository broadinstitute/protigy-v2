################################################################################
# Tests for parse_interaction_terms() - the pure helper that turns the LM
# setup module's (selected_variables, interaction_terms) input pair into the
# list of interaction variable pairs consumed by build_formula_string() and the
# fit.
#
# Regression driver: unchecking one of two selected variables used to crash the
# app. The `interaction_terms` checkbox input persists STALE (non-NULL) after a
# variable is removed, and formula_string() called combn(selected_variables, 2)
# unconditionally whenever interaction_terms was non-NULL. combn(x, 2) errors
# with "n < m" when length(x) < 2, so the length-1 selection blew up the whole
# design/preview reactive chain. This helper must be crash-proof for any
# selected-variable length, including 0 and 1.
#
# Contract:
#   parse_interaction_terms(selected_variables, interaction_terms) -> list of
#   length-2 character vectors (the chosen interaction pairs), in the same order
#   combn() enumerates them. Returns an empty list when:
#     - fewer than 2 variables are selected (NOTHING to interact), OR
#     - interaction_terms is NULL / empty, OR
#     - no interaction_terms label matches a current pair (all stale).
#   The pair labels are matched against interaction_terms using the exact
#   "A : B" format the interaction picker renders (space-colon-space).
################################################################################

library(testthat)


test_that("returns empty list and does NOT call combn when a single variable is selected", {
  # This is the crash repro: 2 vars were selected (so interaction_terms is a
  # stale non-NULL label like "Subgroup : Experiment"), then one var is
  # unchecked leaving length 1. Must return list(), never error.
  expect_silent(
    res <- parse_interaction_terms(
      selected_variables = "Subgroup",
      interaction_terms  = "Subgroup : Experiment"
    )
  )
  expect_identical(res, list())
})


test_that("returns empty list for zero selected variables", {
  expect_identical(
    parse_interaction_terms(character(0), "Subgroup : Experiment"),
    list()
  )
  expect_identical(
    parse_interaction_terms(NULL, "Subgroup : Experiment"),
    list()
  )
})


test_that("returns empty list when interaction_terms is NULL or empty", {
  expect_identical(
    parse_interaction_terms(c("A", "B"), NULL),
    list()
  )
  expect_identical(
    parse_interaction_terms(c("A", "B"), character(0)),
    list()
  )
})


test_that("returns the selected interaction pair for two variables", {
  res <- parse_interaction_terms(
    selected_variables = c("Subgroup", "Experiment"),
    interaction_terms  = "Subgroup : Experiment"
  )
  expect_length(res, 1L)
  expect_identical(res[[1]], c("Subgroup", "Experiment"))
})


test_that("returns only the interaction pairs the user actually checked", {
  # 3 vars -> combn gives 3 candidate pairs; user checked only 2 of them.
  res <- parse_interaction_terms(
    selected_variables = c("A", "B", "C"),
    interaction_terms  = c("A : B", "B : C")
  )
  expect_length(res, 2L)
  expect_identical(res[[1]], c("A", "B"))
  expect_identical(res[[2]], c("B", "C"))
})


test_that("drops stale interaction labels that no longer correspond to a pair", {
  # interaction_terms still references a removed variable "C"; only the A:B
  # pair is still valid. The stale "A : C" must be silently ignored.
  res <- parse_interaction_terms(
    selected_variables = c("A", "B"),
    interaction_terms  = c("A : B", "A : C")
  )
  expect_length(res, 1L)
  expect_identical(res[[1]], c("A", "B"))
})
