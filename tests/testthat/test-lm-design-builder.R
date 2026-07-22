################################################################################
# Tests for build_lm_design(): the SINGLE source of truth for the LM design
# matrix, shared by the on-screen preview (tab_lm_setup.R) and the actual fit
# (lm.regression, tab_lm_setup_helpers.R).
#
# The preview used to re-implement design-building independently and had drifted
# from the fit in four ways, so a config could preview as fine yet stop()/drop
# samples at fit time, and a valid repeated-measures config (empty formula +
# blocking) previewed as "Could not build design matrix". This helper closes
# that gap by construction: both callers run the SAME code, so they cannot
# disagree.
#
# The helper is PURE and side-effect-light: it takes cdesc + formula + types +
# reference levels + blocking var and returns a structured result
#   list(design=, cdesc_clean=, dropped=, n_used=, n_total=, error=, warnings=,
#        repeated_measures_only=)
# reporting diagnostics as DATA (never via warning()/stop()), so the preview can
# render them and the fit can escalate them. `error` is NULL on success or a
# message string when no design can be built; `warnings` is a character vector.
################################################################################

library(testthat)

# Minimal cdesc with two factors + a blocking (subject) variable. Mirrors the
# fixture in test-lm-setup-helpers.R but exposes cdesc directly (the helper
# operates on metadata, not on a GCT).
make_cdesc <- function(n = 12) {
  data.frame(
    id          = paste0("s", seq_len(n)),
    group       = factor(rep(c("A", "B"), length.out = n)),
    time        = factor(rep(c("T1", "T2", "T3"), length.out = n)),
    participant = factor(rep(paste0("p", seq_len(n / 2)), each = 2)),
    stringsAsFactors = FALSE,
    row.names   = paste0("s", seq_len(n))
  )
}

# ---- 1. Baseline: builds a design for a plain formula ------------------------

test_that("build_lm_design builds a design matrix for a simple formula", {
  res <- build_lm_design(
    cdesc = make_cdesc(),
    formula_string = "~ group",
    variable_types = list(group = "factor")
  )
  expect_null(res$error)
  expect_true(is.matrix(res$design))
  expect_true("groupB" %in% colnames(res$design))
  expect_equal(res$n_used, 12L)
  expect_equal(res$n_total, 12L)
})

# ---- 2. complete.cases INCLUDES the blocking variable ------------------------
# The preview bug: it filtered on formula vars only, so samples missing ONLY the
# blocking value were counted as present. The fit drops them. The shared helper
# must drop them for both.

test_that("blocking variable participates in complete.cases sample dropping", {
  cd <- make_cdesc()
  # Two samples are missing ONLY the blocking value (group/time are intact).
  cd$participant[c(3, 7)] <- NA

  res <- build_lm_design(
    cdesc = cd,
    formula_string = "~ group",
    variable_types = list(group = "factor"),
    blocking_var = "participant"
  )
  expect_null(res$error)
  # Both incomplete-on-blocking samples are dropped -> 10 of 12 used.
  expect_equal(res$n_used, 10L)
  expect_equal(res$n_total, 12L)
  expect_false(any(c("s3", "s7") %in% rownames(res$design)))
})

test_that("without a blocking var, samples complete on formula vars are kept", {
  cd <- make_cdesc()
  cd$participant[c(3, 7)] <- NA  # blocking column has NAs, but it is not used

  res <- build_lm_design(
    cdesc = cd,
    formula_string = "~ group",
    variable_types = list(group = "factor")
    # no blocking_var
  )
  expect_null(res$error)
  expect_equal(res$n_used, 12L)   # blocking NAs are irrelevant here
})

# ---- 3. droplevels + single-level guard reported as an ERROR (not a crash) ---
# The fit stop()s when a factor collapses to one level after NA filtering. The
# preview silently rendered a design anyway. The shared helper reports this as a
# structured `error` so the preview can show it and the fit can escalate it.

test_that("a factor collapsing to one level after NA filter yields a structured error", {
  cd <- make_cdesc()
  # Drop every group=='B' sample via NA in an unrelated model variable, so only
  # group=='A' survives complete-case filtering.
  cd$aux <- ifelse(cd$group == "B", NA_character_, "x")

  res <- build_lm_design(
    cdesc = cd,
    formula_string = "~ group + aux",
    variable_types = list(group = "factor", aux = "factor")
  )
  expect_false(is.null(res$error))
  expect_match(res$error, "only one level", ignore.case = TRUE)
  expect_null(res$design)   # no design when a factor is degenerate
})

# ---- 4. rank-deficiency surfaced as a WARNING string (not silent) ------------
# The fit warns; the preview showed nothing. The shared helper reports it in
# `warnings` so both callers can surface it.

test_that("rank-deficient design is reported in warnings, design still returned", {
  cd <- make_cdesc()
  cd$group_copy <- cd$group   # perfectly collinear with group

  res <- build_lm_design(
    cdesc = cd,
    formula_string = "~ group + group_copy",
    variable_types = list(group = "factor", group_copy = "factor")
  )
  # Design is still built (limma would too), but the deficiency is announced.
  expect_true(is.matrix(res$design))
  expect_true(any(grepl("rank-deficient", res$warnings)))
})

# ---- 5. empty formula + blocking = valid repeated-measures preview -----------
# The preview bug: empty formula -> model.matrix errored -> "Could not build
# design matrix". But the fit fully supports empty-formula + blocking as an
# intercept-only repeated-measures design. The shared helper must build the
# ~ 1 design and flag repeated_measures_only, NOT error.

test_that("empty formula + blocking var builds an intercept-only design", {
  res <- build_lm_design(
    cdesc = make_cdesc(),
    formula_string = "",
    variable_types = list(),
    blocking_var = "participant"
  )
  expect_null(res$error)
  expect_true(is.matrix(res$design))
  expect_equal(colnames(res$design), "(Intercept)")
  expect_true(isTRUE(res$repeated_measures_only))
})

test_that("empty formula with NO blocking var is a structured error, not a crash", {
  res <- build_lm_design(
    cdesc = make_cdesc(),
    formula_string = "",
    variable_types = list()
  )
  expect_false(is.null(res$error))
  expect_match(res$error, "predictor", ignore.case = TRUE)
})

# ---- 6. reference-level releveling flows through -----------------------------
# The preview already relevels; the fit relevels; the shared helper must keep
# doing so, so previewed coefficient names match the fitted ones.

test_that("reference level is applied so the non-reference coefficient is emitted", {
  res <- build_lm_design(
    cdesc = make_cdesc(),
    formula_string = "~ group",
    variable_types = list(group = "factor"),
    reference_levels = list(group = "B")   # make B the reference
  )
  expect_null(res$error)
  # With B as reference, the emitted coefficient is groupA (not groupB).
  expect_true("groupA" %in% colnames(res$design))
  expect_false("groupB" %in% colnames(res$design))
})
