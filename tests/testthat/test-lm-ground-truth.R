################################################################################
# Ground-truth statistical-correctness tests for the Linear Model backbone.
#
# These COMPLEMENT test-lm-golden-regression.R. The golden-file suite asserts
# that lm.regression() matches isolated limma (oracle = limma; catches drift).
# This suite asserts that the pipeline is *correct against known ground truth*:
# effects we planted ourselves. A bug that survived golden regeneration (because
# the golden was produced by the same buggy path) would still be caught here.
#
# Coverage (driven by the gt_* fixtures in tests/lm-sandbox/data/):
#   1. Null calibration   — gt_pure_null: nominal p ~ Uniform(0,1); FDR control.
#   2. Sign & magnitude   — gt_sign_convention: contrast logFC == planted shift.
#   3. Recovery / power    — gt_power_recovery: sensitivity on true positives,
#                            specificity on true nulls.
#   4. Blocking behaviour  — gt_blocking: duplicateCorrelation blocking recovers
#                            within-subject effects that the unblocked fit misses.
#   5. Structural invariants — row order, NA handling, monotone FDR, sign of
#                            logSignP, reference-level algebra.
#   6. Graceful degradation — gt_rank_deficient: warns, does not crash.
#
# Thresholds are deliberately loose around the observed (seeded) values so the
# tests assert the *statistical property*, not a brittle exact number.
################################################################################

library(testthat)

DATA_DIR <- file.path("..", "lm-sandbox", "data")

# Regenerate the ground-truth fixtures if they are missing (keeps the suite
# self-contained on a fresh checkout). The generator is seeded, so this is
# deterministic.
gt_path <- function(name) file.path(DATA_DIR, paste0(name, ".rds"))
if (!file.exists(gt_path("gt_pure_null"))) {
  old <- getwd()
  # synthesize_ground_truth.R writes to tests/lm-sandbox/data relative to repo root
  setwd(file.path("..", ".."))
  on.exit(setwd(old), add = TRUE)
  sys.source("tests/lm-sandbox/synthesize_ground_truth.R", envir = new.env())
  setwd(old)
}

load_gt <- function(name) readRDS(gt_path(name))

wrap_gct <- function(fx) {
  rdesc <- if (!is.null(fx$rdesc)) fx$rdesc else
    data.frame(id = rownames(fx$mat), row.names = rownames(fx$mat))
  methods::new(
    "GCT",
    mat = fx$mat, cdesc = fx$cdesc, rdesc = rdesc,
    rid = rownames(fx$mat), cid = colnames(fx$mat)
  )
}

# Run lm.regression() while swallowing (but recording) warnings, so an expected
# warning (e.g. rank deficiency) doesn't turn into a test error.
run_quietly <- function(...) {
  warns <- character(0)
  res <- withCallingHandlers(
    lm.regression(...),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  attr(res, "warnings") <- warns
  res
}


# ============================================================================ #
# 1. NULL CALIBRATION                                                           #
# ============================================================================ #
test_that("pure null: nominal p-values are uniform and well-calibrated", {
  fx <- load_gt("gt_pure_null")
  res <- lm.regression(
    gct = wrap_gct(fx),
    formula_string = "~ condition",
    variable_types = list(condition = "factor"),
    intensity = FALSE
  )

  p <- res$P.Value.conditionTrt
  p <- p[!is.na(p)]
  expect_gt(length(p), 1000)  # the full feature set survives

  # Under the global null, nominal p-values are Uniform(0,1):
  #   - mean ~ 0.5
  #   - a KS test against the uniform should NOT reject
  #   - the fraction below alpha should be ~ alpha
  expect_equal(mean(p), 0.5, tolerance = 0.05)
  ks_p <- suppressWarnings(stats::ks.test(p, "punif"))$p.value
  expect_gt(ks_p, 0.01)                       # not rejected as non-uniform
  expect_lt(abs(mean(p < 0.05) - 0.05), 0.02) # type-I rate near nominal 0.05
})

test_that("pure null: BH-FDR controls false discoveries (≈ none expected)", {
  fx <- load_gt("gt_pure_null")
  res <- lm.regression(
    gct = wrap_gct(fx),
    formula_string = "~ condition",
    variable_types = list(condition = "factor"),
    intensity = FALSE
  )
  adj <- res$adj.P.Val.conditionTrt
  adj <- adj[!is.na(adj)]
  # Every feature is a true null; BH at 5% should yield essentially no calls.
  # Allow a tiny slack (< 0.5% of features) for the stochastic tail.
  n_false_pos <- sum(adj < 0.05)
  expect_lt(n_false_pos, 0.005 * length(adj))
})


# ============================================================================ #
# 2. SIGN & MAGNITUDE ALGEBRA                                                   #
# ============================================================================ #
test_that("sign convention: contrast logFC matches planted direction & size", {
  fx <- load_gt("gt_sign_convention")
  res <- lm.regression(
    gct = wrap_gct(fx),
    formula_string = "~ 0 + condition",
    variable_types = list(condition = "factor"),
    contrasts_list = list(
      B_vs_A = "conditionB - conditionA",
      C_vs_A = "conditionC - conditionA"
    ),
    intensity = FALSE
  )

  up <- fx$planted$B_vs_A
  dn <- fx$planted$C_vs_A
  shift_up <- fx$truth$shift_B_vs_A   # +3
  shift_dn <- fx$truth$shift_C_vs_A   # -3

  # B - A is the planted POSITIVE shift; C - A is the planted NEGATIVE shift.
  expect_equal(median(res$logFC.B_vs_A[up]), shift_up, tolerance = 0.15)
  expect_equal(median(res$logFC.C_vs_A[dn]), shift_dn, tolerance = 0.15)

  # Direction is unambiguous on planted features.
  expect_true(all(res$logFC.B_vs_A[up] > 0))
  expect_true(all(res$logFC.C_vs_A[dn] < 0))

  # Anti-symmetry of the contrast operator: A - B == -(B - A) for the same data.
  res_rev <- lm.regression(
    gct = wrap_gct(fx),
    formula_string = "~ 0 + condition",
    variable_types = list(condition = "factor"),
    contrasts_list = list(A_vs_B = "conditionA - conditionB"),
    intensity = FALSE
  )
  expect_equal(res_rev$logFC.A_vs_B, -res$logFC.B_vs_A, tolerance = 1e-8)
})

test_that("reference level controls coefficient sign (treatment parameterisation)", {
  fx <- load_gt("gt_sign_convention")

  # With A as the reference, the conditionB coef is (B - A) = +shift.
  res_ref_A <- lm.regression(
    gct = wrap_gct(fx),
    formula_string = "~ condition",
    variable_types = list(condition = "factor"),
    reference_levels = list(condition = "A"),
    intensity = FALSE
  )
  up <- fx$planted$B_vs_A
  expect_true("logFC.conditionB" %in% colnames(res_ref_A))
  expect_equal(median(res_ref_A$logFC.conditionB[up]),
               fx$truth$shift_B_vs_A, tolerance = 0.15)

  # With B as the reference, the conditionA coef is (A - B) = -shift: the sign
  # flips purely from re-leveling, with no change to the data.
  res_ref_B <- lm.regression(
    gct = wrap_gct(fx),
    formula_string = "~ condition",
    variable_types = list(condition = "factor"),
    reference_levels = list(condition = "B"),
    intensity = FALSE
  )
  expect_true("logFC.conditionA" %in% colnames(res_ref_B))
  expect_equal(median(res_ref_B$logFC.conditionA[up]),
               -fx$truth$shift_B_vs_A, tolerance = 0.15)
})


# ============================================================================ #
# 3. RECOVERY / POWER & SPECIFICITY                                             #
# ============================================================================ #
test_that("power recovery: planted features are detected; nulls are not", {
  fx <- load_gt("gt_power_recovery")
  res <- lm.regression(
    gct = wrap_gct(fx),
    formula_string = "~ condition",
    variable_types = list(condition = "factor"),
    intensity = FALSE
  )

  tp <- fx$planted$trt
  null_feat <- setdiff(seq_len(nrow(fx$mat)), tp)
  adj <- res$adj.P.Val.conditionTrt

  # Sensitivity: with shift=2, sigma=1, n=6/group, BH across 1000 features and
  # eBayes shrinkage, ~70% of true positives clear BH 5% (≈80% at BH 10%). This
  # matches the analytic power for this SNR; assert a healthy floor, not a number
  # the seed happens to hit exactly.
  sensitivity <- mean(adj[tp] < 0.05)
  expect_gt(sensitivity, 0.60)
  expect_gt(mean(adj[tp] < 0.10), 0.70)

  # Specificity: false-discovery proportion among the calls stays controlled.
  called <- which(adj < 0.05)
  fdp <- mean(!(called %in% tp))
  expect_lt(fdp, 0.05)

  # Direction is correct on the planted (+2) features.
  expect_gt(median(res$logFC.conditionTrt[tp]), 1.5)
})


# ============================================================================ #
# 4. BLOCKING / WITHIN-SUBJECT CORRELATION                                      #
# ============================================================================ #
test_that("blocking recovers paired effects that the unblocked fit misses", {
  fx <- load_gt("gt_blocking")
  tp <- fx$planted$time

  res_blk <- run_quietly(
    gct = wrap_gct(fx),
    formula_string = "~ time",
    variable_types = list(time = "factor"),
    blocking_var = "subject",
    reference_levels = list(time = "Pre"),
    intensity = FALSE
  )
  res_unb <- lm.regression(
    gct = wrap_gct(fx),
    formula_string = "~ time",
    variable_types = list(time = "factor"),
    blocking_var = NULL,
    reference_levels = list(time = "Pre"),
    intensity = FALSE
  )

  col_adj <- "adj.P.Val.timePost"
  col_p   <- "P.Value.timePost"

  blk_hits <- sum(res_blk[[col_adj]][tp] < 0.05)
  unb_hits <- sum(res_unb[[col_adj]][tp] < 0.05)

  # Strong within-subject correlation (rho=0.8): blocking should recover most of
  # the planted effects, the unblocked fit very few. Blocking must do strictly
  # (and substantially) better.
  expect_gt(blk_hits, unb_hits)
  expect_gt(blk_hits, 0.5 * length(tp))

  # Blocked p-values on true positives are systematically smaller.
  expect_lt(median(res_blk[[col_p]][tp]), median(res_unb[[col_p]][tp]))

  # logFC point estimate (Post - Pre) is unbiased either way (blocking changes
  # the variance, not the mean effect): both recover the planted +0.8.
  expect_equal(median(res_blk$logFC.timePost[tp]), fx$truth$shift, tolerance = 0.2)
  expect_equal(median(res_unb$logFC.timePost[tp]), fx$truth$shift, tolerance = 0.2)
})

test_that("repeated-measures-only mode: blocking on subject with empty formula", {
  fx <- load_gt("gt_blocking")
  # No fixed-effect predictor; subject is the block. This exercises the
  # repeated_measures_only branch (intercept-only design + duplicateCorrelation).
  res <- run_quietly(
    gct = wrap_gct(fx),
    formula_string = "",
    variable_types = list(),
    blocking_var = "subject",
    intensity = FALSE
  )
  # Should return a frame with the grand-mean coefficient reported, no crash.
  expect_s3_class(res, "data.frame")
  expect_true(any(grepl("^logFC\\.", colnames(res))))
  expect_equal(nrow(res), nrow(fx$mat))
})


# ============================================================================ #
# 5. STRUCTURAL INVARIANTS                                                      #
# ============================================================================ #
test_that("structural invariants: row order, FDR monotonicity, logSignP sign", {
  fx <- load_gt("gt_power_recovery")
  res <- lm.regression(
    gct = wrap_gct(fx),
    formula_string = "~ condition",
    variable_types = list(condition = "factor"),
    intensity = FALSE
  )

  # Every input feature appears exactly once, ids preserved (order-independent).
  expect_equal(nrow(res), nrow(fx$mat))
  expect_setequal(res$id, rownames(fx$mat))
  expect_false(any(duplicated(res$id)))

  # BH adjustment is monotone in the nominal p-value and never smaller than it.
  ord <- order(res$P.Value.conditionTrt)
  p_sorted   <- res$P.Value.conditionTrt[ord]
  adj_sorted <- res$adj.P.Val.conditionTrt[ord]
  expect_true(all(adj_sorted >= p_sorted - 1e-9))         # adj >= raw
  expect_false(is.unsorted(adj_sorted, na.rm = TRUE))      # non-decreasing
  expect_true(all(adj_sorted <= 1 + 1e-9))                 # bounded by 1

  # logSignP = -sign(logFC) * log10(p). For p in (0,1), log10(p) < 0, so an
  # up-regulated feature (logFC>0) yields logSignP = -(+1)*(negative) > 0, and a
  # down-regulated feature yields logSignP < 0. Verify on the clearly-up planted
  # block: all logFC>0 and therefore all logSignP>0 (it encodes signed
  # significance, so up-regulated == positive).
  tp <- fx$planted$trt
  expect_true(all(res$logFC.conditionTrt[tp] > 0))
  expect_true(all(res$logSignP.conditionTrt[tp] > 0))

  # Cross-check the formula directly against logFC sign and p-value magnitude.
  expect_equal(
    res$logSignP.conditionTrt,
    -sign(res$logFC.conditionTrt) * log10(pmax(res$P.Value.conditionTrt, .Machine$double.xmin)),
    tolerance = 1e-9
  )
})

test_that("NA samples in a model variable are dropped, not propagated", {
  fx <- load_gt("gt_sign_convention")
  cdesc_na <- fx$cdesc
  # Blank out the condition for two samples; those columns must be excluded from
  # the fit, and the feature count (rows) must be unchanged.
  na_samples <- rownames(cdesc_na)[c(1, 7)]
  cdesc_na[na_samples, "condition"] <- NA
  gct_na <- methods::new(
    "GCT",
    mat = fx$mat, cdesc = cdesc_na, rdesc = fx$rdesc,
    rid = rownames(fx$mat), cid = colnames(fx$mat)
  )

  res <- lm.regression(
    gct = gct_na,
    formula_string = "~ condition",
    variable_types = list(condition = "factor"),
    reference_levels = list(condition = "A"),
    intensity = FALSE
  )
  expect_equal(nrow(res), nrow(fx$mat))      # all features still reported
  expect_false(any(is.na(res$logFC.conditionB)))  # estimable despite dropped samples
})


# ============================================================================ #
# 6. GRACEFUL DEGRADATION                                                       #
# ============================================================================ #
test_that("rank-deficient design warns and degrades gracefully (no crash)", {
  fx <- load_gt("gt_rank_deficient")
  # batch is perfectly aliased with treatment -> ~ treatment + batch is singular.
  res <- run_quietly(
    gct = wrap_gct(fx),
    formula_string = "~ treatment + batch",
    variable_types = list(treatment = "factor", batch = "factor"),
    intensity = FALSE
  )
  warns <- attr(res, "warnings")

  # The rank-deficiency preflight must fire.
  expect_true(any(grepl("rank-deficient", warns, ignore.case = TRUE)))
  # The fit still returns a frame for all features (estimable coef present).
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), nrow(fx$mat))
  expect_true("logFC.treatmentDrug" %in% colnames(res))

  # The estimable treatment effect still recovers the planted +1.5 on its block.
  tp <- fx$planted$treatment
  expect_gt(median(res$logFC.treatmentDrug[tp]), 1.0)
})

test_that("blocking variable that is also a fixed effect is rejected", {
  fx <- load_gt("gt_blocking")
  # subject appears in BOTH the formula and as the block -> contract violation.
  expect_error(
    lm.regression(
      gct = wrap_gct(fx),
      formula_string = "~ subject",
      variable_types = list(subject = "factor"),
      blocking_var = "subject",
      intensity = FALSE
    ),
    regexp = "cannot also appear"
  )
})
