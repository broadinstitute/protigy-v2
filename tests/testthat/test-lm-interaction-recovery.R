################################################################################
# Ground-truth interaction recovery for lm.regression().
#
# The review flagged interaction-effect recovery as having NO independent
# statistical validation: the realistic 2x2 interaction fixture
# (inst/extdata/linear-model-test/lm_test_data.gct) was used by no automated
# test, and the golden-regression layer only checks the wrapper reproduces a
# same-author limma run (plumbing, not correctness).
#
# These tests plant a KNOWN interaction and assert the fitted interaction term
# recovers its magnitude, sign, and significance -- an analytic oracle, not a
# circular golden snapshot.
#
# Two designs are exercised:
#   1. 2x2 factorial (the packaged fixture): the interaction is a SINGLE
#      coefficient, so recovery is asserted on the interaction t-test column.
#      NOTE: a 2x2 interaction is a difference-of-differences whose standard
#      error is ~2x a simple contrast, so per-feature BH power is modest on
#      this small fixture. We assert AGGREGATE magnitude + sign + clean
#      specificity (no false positives), not "every true feature is significant".
#   2. 3x2 factorial (synthesized in-test): the interaction block spans >=2
#      coefficients, so the module emits an omnibus per-factor F-test; the
#      planted effect is sized so that F-test genuinely controls FDR with power.
################################################################################

# ---- helpers -----------------------------------------------------------------

# Locate the packaged 2x2 interaction fixture. The generator plants a ~1.5 log2
# interaction effect (Drug x T2) on the first 10 of 50 features; the remaining
# 40 are pure Gaussian noise. cmapR round-trips factor labels to integer codes,
# so the observed levels are "1"/"2" and the signal cell is
# treatment==2 & timepoint==2 (verified against the raw matrix).
load_interaction_fixture <- function() {
  path <- system.file(
    "extdata", "linear-model-test", "lm_test_data.gct",
    package = "Protigy"
  )
  if (!nzchar(path) || !file.exists(path)) {
    path <- testthat::test_path(
      "..", "..", "inst", "extdata", "linear-model-test", "lm_test_data.gct"
    )
  }
  skip_if_not(file.exists(path), "interaction fixture GCT not found")
  suppressWarnings(cmapR::parse_gctx(path))
}

fit_2x2 <- function(g) {
  lm.regression(
    gct = g,
    formula_string = "~ treatment + timepoint + treatment:timepoint",
    variable_types = list(treatment = "factor", timepoint = "factor")
  )
}

# Build a balanced 3x2 factorial GCT with a KNOWN interaction on a subset of
# features. genotype has 3 levels (WT/HET/KO) so the interaction block spans
# 2 coefficients -> triggers the omnibus per-factor F-test. The effect is sized
# (4 log2 in the KO:Yes cell) so the F-test controls FDR with real power.
make_3x2_interaction_gct <- function(seed = 7L, effect = 4.0,
                                     n_null = 60L, n_true = 20L) {
  set.seed(seed)
  n_feat   <- n_null + n_true
  genotype <- rep(c("WT", "HET", "KO"), each = 2L, times = 6L)  # 36 samples
  treated  <- rep(c("No", "Yes"), times = 18L)
  n_samp   <- length(genotype)
  sample_ids <- paste0("s", sprintf("%02d", seq_len(n_samp)))
  feat_ids   <- paste0("F", sprintf("%03d", seq_len(n_feat)))

  cdesc <- data.frame(
    id       = sample_ids,
    genotype = factor(genotype, levels = c("WT", "HET", "KO")),
    treated  = factor(treated,  levels = c("No", "Yes")),
    row.names = sample_ids,
    stringsAsFactors = FALSE
  )
  rdesc <- data.frame(
    id = feat_ids, geneSymbol = paste0("G", seq_len(n_feat)),
    row.names = feat_ids, stringsAsFactors = FALSE
  )

  mat <- matrix(rnorm(n_feat * n_samp, mean = 10, sd = 1),
                nrow = n_feat, dimnames = list(feat_ids, sample_ids))

  # Plant an interaction: only the KO:Yes cell is shifted, on the first n_true
  # features. Main effects stay null so the signal is purely the interaction.
  ko_yes <- cdesc$genotype == "KO" & cdesc$treated == "Yes"
  mat[seq_len(n_true), ko_yes] <- mat[seq_len(n_true), ko_yes] + effect

  gct <- methods::new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc,
                      rid = feat_ids, cid = sample_ids)
  list(gct = gct, n_true = n_true, n_null = n_null, effect = effect)
}

# ---- 2x2 fixture: interaction coefficient recovery ---------------------------

test_that("2x2 fixture recovers the planted interaction coefficient magnitude", {
  res  <- fit_2x2(load_interaction_fixture())
  icol <- grep("^logFC[.].*treatment.*timepoint", colnames(res), value = TRUE)

  expect_length(icol, 1)   # exactly one interaction coefficient in a 2x2
  icol <- icol[1]

  true_idx <- 1:10          # PROT_001-010 carry the ~1.5 planted effect
  null_idx <- 11:50         # PROT_011-050 are pure noise

  # Aggregate recovered interaction effect ~ +1.5 on the true block. Individual
  # difference-of-differences estimates are noisy, so we assert on the median.
  expect_equal(median(res[[icol]][true_idx]), 1.5, tolerance = 0.4)
  # Null features have no interaction on aggregate.
  expect_lt(abs(median(res[[icol]][null_idx])), 0.3)
  # Aggregate true effect is well separated from the aggregate null.
  expect_gt(median(res[[icol]][true_idx]),
            median(res[[icol]][null_idx]) + 1.0)
})

test_that("2x2 fixture: interaction has the correct (positive) sign on true features", {
  res  <- fit_2x2(load_interaction_fixture())
  icol <- grep("^logFC[.].*treatment.*timepoint", colnames(res), value = TRUE)[1]
  expect_true(all(res[[icol]][1:10] > 0))   # planted effect is positive
})

test_that("2x2 fixture: interaction is specific -- no false positives among nulls", {
  res  <- fit_2x2(load_interaction_fixture())
  acol <- grep("^adj[.]P[.]Val[.].*treatment.*timepoint", colnames(res),
               value = TRUE)[1]

  # Clean specificity: none of the 40 null features is called at BH 5%.
  expect_equal(sum(res[[acol]][11:50] < 0.05), 0L)
  # The pipeline still detects the strongest true feature (sanity that power
  # is not zero), so the specificity above is not vacuous.
  expect_true(any(res[[acol]][1:10] < 0.05))
})

# ---- 3x2 design: interaction OMNIBUS F-test recovery -------------------------

test_that("3x2 interaction block emits an omnibus F-test column", {
  fx  <- make_3x2_interaction_gct()
  res <- lm.regression(
    gct = fx$gct,
    formula_string = "~ genotype + treated + genotype:treated",
    variable_types = list(genotype = "factor", treated = "factor")
  )

  # The interaction block spans 2 coefficients (3-level x 2-level), so a
  # per-factor omnibus F-test must be emitted for the interaction key.
  expect_true("F.genotype.treated" %in% colnames(res))
  expect_true("adj.P.Val.genotype.treated" %in% colnames(res))
  # The interaction F-block carries no single signed logFC.
  expect_false("logFC.genotype.treated" %in% colnames(res))
})

test_that("3x2 interaction F-test recovers the effect with FDR control", {
  fx  <- make_3x2_interaction_gct()
  res <- lm.regression(
    gct = fx$gct,
    formula_string = "~ genotype + treated + genotype:treated",
    variable_types = list(genotype = "factor", treated = "factor")
  )

  fcol <- "F.genotype.treated"
  acol <- "adj.P.Val.genotype.treated"
  true_idx <- seq_len(fx$n_true)
  null_idx <- (fx$n_true + 1L):(fx$n_true + fx$n_null)

  # Omnibus power: F-statistics are far larger on true than on null features.
  expect_gt(median(res[[fcol]][true_idx]), 5 * median(res[[fcol]][null_idx]))
  # Most true features are called by the interaction F-test at BH 5%...
  expect_gt(mean(res[[acol]][true_idx] < 0.05), 0.7)
  # ...while the realized false-discovery proportion among nulls stays < 5%.
  expect_lt(mean(res[[acol]][null_idx] < 0.05), 0.05)
})
