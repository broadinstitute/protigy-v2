################################################################################
# Tests for the contrast-builder helpers in tab_lm_setup_helpers_contrasts.R
################################################################################

library(testthat)


test_that("strip_shared_prefix removes treatment-contrast-style variable prefix", {
  expect_equal(
    strip_shared_prefix("treatmentDrug", "treatmentVehicle"),
    c("Drug", "Vehicle")
  )
})

test_that("strip_shared_prefix is a no-op when no prefix is shared", {
  expect_equal(
    strip_shared_prefix("Drug", "Control"),
    c("Drug", "Control")
  )
})

test_that("strip_shared_prefix handles empty inputs without error", {
  expect_equal(strip_shared_prefix("", "x"), c("", "x"))
  expect_equal(strip_shared_prefix("x", ""), c("x", ""))
  expect_equal(strip_shared_prefix("", ""), c("", ""))
})

test_that("strip_shared_prefix does not over-strip when one is a prefix of the other", {
  # e.g. "Drug" and "DrugHigh" - stripping "Drug" would leave "" for the first
  expect_equal(
    strip_shared_prefix("Drug", "DrugHigh"),
    c("Drug", "DrugHigh")
  )
})


test_that("make_simple_label joins stripped tokens with '-' and no spaces", {
  expect_equal(
    make_simple_label("treatmentDrug", "treatmentVehicle"),
    "Drug-Vehicle"
  )
})

test_that("make_simple_label returns empty string when either input is empty", {
  expect_equal(make_simple_label("", "x"), "")
  expect_equal(make_simple_label("x", ""), "")
  expect_equal(make_simple_label(NULL, "x"), "")
})

test_that("make_simple_label strips whitespace defensively", {
  # Design-coef names shouldn't contain whitespace, but be safe
  expect_false(grepl("\\s", make_simple_label("a b", "c")))
})


test_that("build_simple_expr emits a limma-compatible contrast string", {
  expect_equal(
    build_simple_expr("treatmentDrug", "treatmentVehicle"),
    "treatmentDrug - treatmentVehicle"
  )
})

test_that("build_simple_expr returns empty when either input is empty", {
  expect_equal(build_simple_expr("", "x"), "")
  expect_equal(build_simple_expr(NULL, "x"), "")
})


test_that("direction_sentence_simple interprets the sign of log2FC", {
  sent <- direction_sentence_simple("Drug-Vehicle", "treatmentDrug", "treatmentVehicle")
  expect_match(sent, "Positive log2FC")
  expect_match(sent, "Drug")
  expect_match(sent, "Vehicle")
})

test_that("direction_sentence_simple is empty when label is empty", {
  expect_equal(direction_sentence_simple("", "a", "b"), "")
})


test_that("validate_advanced_expr accepts an expression referencing known coefs", {
  coefs <- c("groupA", "groupB", "groupC")
  res <- validate_advanced_expr("groupA - groupB", coefs)
  expect_true(res$ok)
  expect_length(res$unknown, 0)
})

test_that("validate_advanced_expr accepts weighted multi-coef expressions", {
  coefs <- c("groupA", "groupB", "groupC")
  res <- validate_advanced_expr("groupA - (groupB + groupC)/2", coefs)
  expect_true(res$ok)
})

test_that("validate_advanced_expr accepts interaction-style diff-of-diffs", {
  coefs <- c("groupA.X", "groupA.Y", "groupB.X", "groupB.Y")
  res <- validate_advanced_expr("(groupA.X - groupA.Y) - (groupB.X - groupB.Y)", coefs)
  expect_true(res$ok)
})

test_that("validate_advanced_expr rejects unknown coefs", {
  coefs <- c("groupA", "groupB")
  res <- validate_advanced_expr("groupA - groupZ", coefs)
  expect_false(res$ok)
  expect_true(any(grepl("groupZ", res$unknown)))
})

test_that("validate_advanced_expr handles empty/whitespace expressions", {
  res <- validate_advanced_expr("", c("groupA"))
  expect_false(res$ok)
  res <- validate_advanced_expr("   ", c("groupA"))
  expect_false(res$ok)
})

test_that("validate_advanced_expr mirrors the backend make.names() pass", {
  # Design coef "group:A" becomes "group.A" after make.names(); a user writing
  # "group.A" against a design with "group:A" should validate OK because the
  # backend applies the same normalization.
  coefs <- c("group:A", "group:B")
  res <- validate_advanced_expr("group.A - group.B", coefs)
  expect_true(res$ok)
})


test_that("sanitize_label strips all whitespace", {
  expect_equal(sanitize_label("  hello world  "), "helloworld")
  expect_equal(sanitize_label("a\tb\nc"), "abc")
})

test_that("sanitize_label handles NULL and empty", {
  expect_equal(sanitize_label(NULL), "")
  expect_equal(sanitize_label(""), "")
})


test_that("new_contrast_row_id returns distinct ids across rapid calls", {
  ids <- replicate(20, new_contrast_row_id())
  expect_equal(length(unique(ids)), length(ids))
})
