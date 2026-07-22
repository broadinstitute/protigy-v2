################################################################################
# Tests for pick_default_reference_level() - the heuristic that chooses a
# defensible default reference level for a factor variable.
#
# Rule chain:
#   1. Case-insensitive match against a token list (control, ctrl, vehicle, wt,
#      wildtype, baseline, untreated, placebo, none, healthy). First match wins,
#      ordered by the token list.
#   2. Otherwise: modal (most-frequent) level. Ties broken alphabetically.
#   3. NA/empty input -> NA_character_.
#
# Output: a list with $level (character or NA) and $reason in
#   {"control_token", "modal", "tie_alphabetical", "empty", "single"}.
################################################################################

library(testthat)


test_that("returns NA for empty input", {
  res <- pick_default_reference_level(character(0))
  expect_true(is.na(res$level))
  expect_identical(res$reason, "empty")

  res2 <- pick_default_reference_level(NULL)
  expect_true(is.na(res2$level))
  expect_identical(res2$reason, "empty")
})


test_that("returns the only level for a single-level input", {
  res <- pick_default_reference_level(c("A", "A", "A"))
  expect_identical(res$level, "A")
  expect_identical(res$reason, "single")
})


test_that("matches a control token (case-insensitive)", {
  # Lowercase "vehicle"
  res <- pick_default_reference_level(c("Drug", "vehicle", "Drug", "vehicle"))
  expect_identical(res$level, "vehicle")
  expect_identical(res$reason, "control_token")
  expect_identical(res$matched_token, "vehicle")

  # Mixed case "Vehicle"
  res2 <- pick_default_reference_level(c("Drug", "Vehicle", "Drug"))
  expect_identical(res2$level, "Vehicle")
  expect_identical(res2$reason, "control_token")

  # "WT" matches the "wt" token
  res3 <- pick_default_reference_level(c("KO", "WT", "KO", "WT"))
  expect_identical(res3$level, "WT")
  expect_identical(res3$reason, "control_token")
})


test_that("control token priority follows token order (control beats ctrl beats vehicle)", {
  # If both "Control" and "Vehicle" present, "control" token comes first in the
  # token list, so Control wins.
  res <- pick_default_reference_level(c("Control", "Vehicle", "Drug"))
  expect_identical(res$level, "Control")

  # "ctrl" comes before "vehicle" in the token list.
  res2 <- pick_default_reference_level(c("Ctrl", "Vehicle", "Drug"))
  expect_identical(res2$level, "Ctrl")
})


test_that("falls back to modal level when no control token matches", {
  # Drug = 3, Tumor = 5 -> Tumor wins
  res <- pick_default_reference_level(c("Drug", "Drug", "Drug",
                                         "Tumor", "Tumor", "Tumor", "Tumor", "Tumor"))
  expect_identical(res$level, "Tumor")
  expect_identical(res$reason, "modal")
  expect_identical(res$n, 5L)
})


test_that("modal tie breaks alphabetically", {
  # Both Drug=3 and Tumor=3 -> Drug wins alphabetically
  res <- pick_default_reference_level(c("Drug", "Drug", "Drug",
                                         "Tumor", "Tumor", "Tumor"))
  expect_identical(res$level, "Drug")
  expect_identical(res$reason, "tie_alphabetical")
})


test_that("ignores NA values in the input", {
  res <- pick_default_reference_level(c("Drug", NA, "Vehicle", NA))
  expect_identical(res$level, "Vehicle")
  expect_identical(res$reason, "control_token")
})


test_that("ignores empty strings in the input", {
  res <- pick_default_reference_level(c("Drug", "", "Vehicle", ""))
  expect_identical(res$level, "Vehicle")
  expect_identical(res$reason, "control_token")
})


test_that("works with factor input", {
  f <- factor(c("KO", "WT", "KO", "WT", "WT"))
  res <- pick_default_reference_level(f)
  # WT matches control token "wt"
  expect_identical(res$level, "WT")
  expect_identical(res$reason, "control_token")
})


test_that("control token match wins even when it's not the modal level", {
  # Drug appears 5x, Vehicle only 2x - Vehicle still wins because of token rule.
  res <- pick_default_reference_level(c(rep("Drug", 5), rep("Vehicle", 2)))
  expect_identical(res$level, "Vehicle")
  expect_identical(res$reason, "control_token")
})


test_that("recognises all defined control tokens", {
  expected_tokens <- c("control", "ctrl", "vehicle", "wt", "wildtype",
                       "baseline", "untreated", "placebo", "none", "healthy")
  for (tok in expected_tokens) {
    # Build a vector where the only level is the token (with arbitrary casing)
    levels <- c(toupper(tok), "Other", "Other", "Other")
    res <- pick_default_reference_level(levels)
    expect_identical(res$level, toupper(tok),
                     info = paste("token:", tok))
    expect_identical(res$reason, "control_token",
                     info = paste("token:", tok))
  }
})


test_that("annotation helper produces human-readable strings", {
  # Annotation reflects the reason
  ann_ctrl <- format_reference_level_annotation(list(
    level = "Vehicle", reason = "control_token", matched_token = "vehicle"
  ))
  expect_match(ann_ctrl, "matched", ignore.case = TRUE)
  expect_match(ann_ctrl, "vehicle", ignore.case = TRUE)

  ann_modal <- format_reference_level_annotation(list(
    level = "Tumor", reason = "modal", n = 5L
  ))
  expect_match(ann_modal, "modal", ignore.case = TRUE)
  expect_match(ann_modal, "5", fixed = TRUE)

  ann_tie <- format_reference_level_annotation(list(
    level = "Drug", reason = "tie_alphabetical"
  ))
  expect_match(ann_tie, "alphabetical|tie", ignore.case = TRUE)

  # Single-level / empty return empty annotation
  expect_identical(format_reference_level_annotation(list(
    level = "A", reason = "single"
  )), "")
  expect_identical(format_reference_level_annotation(list(
    level = NA_character_, reason = "empty"
  )), "")
})


test_that("intercept note is empty when the intercept is included", {
  # With an intercept, every factor is treatment-coded against its reference,
  # so the reference matters for all of them - no caveat needed.
  expect_identical(reference_level_intercept_note(TRUE, 1L), "")
  expect_identical(reference_level_intercept_note(TRUE, 3L), "")
})

test_that("intercept-off note with a single factor says the reference is inert", {
  # ~ 0 + A cell-means codes the ONLY factor: relevel just reorders columns and
  # changes no test. The note must tell the user the reference has no effect.
  note <- reference_level_intercept_note(FALSE, 1L)
  expect_true(nzchar(note))
  expect_match(note, "no effect|does not|ignored", ignore.case = TRUE)
})

test_that("intercept-off note with multiple factors scopes the caveat to the first factor", {
  # ~ 0 + A + B: only A is cell-means coded; B (and interactions) are still
  # treatment-coded, so their reference DOES matter. The note must not claim
  # the reference is globally inert.
  note <- reference_level_intercept_note(FALSE, 2L)
  expect_true(nzchar(note))
  expect_match(note, "first factor", ignore.case = TRUE)
  expect_match(note, "other", ignore.case = TRUE)
})

test_that("intercept note tolerates zero factors", {
  # Degenerate guard: no factors -> nothing to caption, regardless of intercept.
  expect_identical(reference_level_intercept_note(FALSE, 0L), "")
  expect_identical(reference_level_intercept_note(TRUE, 0L), "")
})
