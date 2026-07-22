################################################################################
# Ground-truth: two-factor ADDITIVE recovery + >2-level omnibus F-test.
#
# Two related gaps in lm.regression() coverage had only circular golden
# snapshots, never an analytic oracle:
#
#   (a) additive multi-factor recovery -- fit "~ genotype + treated" and confirm
#       each planted per-coefficient effect comes back with correct sign and
#       magnitude, with no cross-contamination between the factors;
#   (b) the per-factor omnibus F-test that the module emits for any variable
#       contributing >= 2 non-intercept coefficients (here a 3-level factor).
#       We check it is *calibrated* under the null (uniform p, FDR controlled)
#       and *powered* under a real effect -- neither of which a golden that was
#       produced by the same code path could catch.
#
# Design (built in-memory, no GCT write/parse round-trip so factor labels are
# preserved exactly, per the module's reference-level handling):
#   genotype : 3 levels WT / HET / KO  (reference WT) -> coefs genotypeHET,
#              genotypeKO -> 2 non-intercept coefs -> OMNIBUS F.genotype emitted.
#   treated  : 2 levels No / Yes       (reference No) -> coef treatedYes
#              -> 1 non-intercept coef -> NO F-column.
# Effects are ADDITIVE (no interaction term in the formula, none planted):
#   TRUE features get genotypeKO += beta1 and treatedYes += beta2; NULL features
#   get nothing.
#
# Calibration approach for F.genotype: a COMPANION pure-genotype-null design
# (genotype has no effect ANYWHERE) rather than the null features inside the
# mixed dataset. Inside the mixed dataset the 40 strong true features pull BH
# generous enough that ~6% of null features slip through adj<0.05 -- a property
# of BH under a large true fraction, not miscalibration. The companion isolates
# the omnibus null so the p-values are strictly uniform and BH yields ~no calls;
# that is the honest calibration check. (We still assert *raw* type-I on the
# mixed-dataset nulls, which stays near nominal.)
#
# Thresholds are loose around the seeded observations (seed 11 / seed 23) so the
# tests assert the statistical property, not a brittle exact number.
################################################################################

library(testthat)

# ---- fixtures ----------------------------------------------------------------

# Balanced, fully-crossed 3x2 additive design, `reps` replicates per cell.
# TRUE block: genotypeKO += beta1, treatedYes += beta2. NULL block: pure noise.
make_additive_gct <- function(seed = 11L, beta1 = 2.0, beta2 = 1.5,
                              n_null = 80L, n_true = 40L, reps = 8L) {
  set.seed(seed)
  n_feat <- n_null + n_true
  grid <- expand.grid(
    genotype = c("WT", "HET", "KO"),
    treated  = c("No", "Yes"),
    rep      = seq_len(reps),
    stringsAsFactors = FALSE
  )
  n_samp     <- nrow(grid)
  sample_ids <- paste0("s", sprintf("%03d", seq_len(n_samp)))
  feat_ids   <- paste0("F", sprintf("%03d", seq_len(n_feat)))

  cdesc <- data.frame(
    id       = sample_ids,
    genotype = factor(grid$genotype, levels = c("WT", "HET", "KO")),
    treated  = factor(grid$treated,  levels = c("No", "Yes")),
    row.names = sample_ids,
    stringsAsFactors = FALSE
  )
  rdesc <- data.frame(
    id = feat_ids, geneSymbol = paste0("G", seq_len(n_feat)),
    row.names = feat_ids, stringsAsFactors = FALSE
  )
  mat <- matrix(rnorm(n_feat * n_samp, mean = 10, sd = 1),
                nrow = n_feat, dimnames = list(feat_ids, sample_ids))

  is_ko    <- cdesc$genotype == "KO"
  is_yes   <- cdesc$treated  == "Yes"
  true_idx <- seq_len(n_true)
  mat[true_idx, is_ko]  <- mat[true_idx, is_ko]  + beta1
  mat[true_idx, is_yes] <- mat[true_idx, is_yes] + beta2

  gct <- methods::new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc,
                      rid = feat_ids, cid = sample_ids)
  list(gct = gct, true_idx = true_idx, null_idx = (n_true + 1L):n_feat,
       beta1 = beta1, beta2 = beta2)
}

# Companion design where genotype has NO effect anywhere (treated carries a
# harmless effect so the fit is non-degenerate). Used only to calibrate the
# omnibus F.genotype under a clean global null.
make_geno_null_gct <- function(seed = 23L, beta2 = 1.5, n_feat = 200L, reps = 8L) {
  set.seed(seed)
  grid <- expand.grid(
    genotype = c("WT", "HET", "KO"),
    treated  = c("No", "Yes"),
    rep      = seq_len(reps),
    stringsAsFactors = FALSE
  )
  n_samp     <- nrow(grid)
  sample_ids <- paste0("s", sprintf("%03d", seq_len(n_samp)))
  feat_ids   <- paste0("F", sprintf("%03d", seq_len(n_feat)))
  cdesc <- data.frame(
    id       = sample_ids,
    genotype = factor(grid$genotype, levels = c("WT", "HET", "KO")),
    treated  = factor(grid$treated,  levels = c("No", "Yes")),
    row.names = sample_ids, stringsAsFactors = FALSE
  )
  rdesc <- data.frame(id = feat_ids, geneSymbol = paste0("G", seq_len(n_feat)),
                      row.names = feat_ids, stringsAsFactors = FALSE)
  mat <- matrix(rnorm(n_feat * n_samp, mean = 10, sd = 1),
                nrow = n_feat, dimnames = list(feat_ids, sample_ids))
  is_yes <- cdesc$treated == "Yes"
  mat[seq_len(40), is_yes] <- mat[seq_len(40), is_yes] + beta2   # genotype null
  gct <- methods::new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc,
                      rid = feat_ids, cid = sample_ids)
  list(gct = gct, n_feat = n_feat)
}

fit_additive <- function(g) {
  lm.regression(
    gct = g,
    formula_string = "~ genotype + treated",
    variable_types = list(genotype = "factor", treated = "factor"),
    reference_levels = list(genotype = "WT", treated = "No")
  )
}

# ---- (a) additive recovery ---------------------------------------------------

test_that("additive design: reference coding emits the expected coefficient columns", {
  res <- fit_additive(make_additive_gct()$gct)

  # 3-level genotype (ref WT) -> HET and KO coefs; 2-level treated (ref No) -> Yes.
  # The reference levels (WT, No) get NO column.
  expect_true("logFC.genotypeHET" %in% colnames(res))
  expect_true("logFC.genotypeKO"  %in% colnames(res))
  expect_true("logFC.treatedYes"  %in% colnames(res))
  expect_false("logFC.genotypeWT" %in% colnames(res))
  expect_false("logFC.treatedNo"  %in% colnames(res))
})

test_that("additive design: each planted per-coefficient effect is recovered", {
  fx  <- make_additive_gct()
  res <- fit_additive(fx$gct)
  ti  <- fx$true_idx
  ni  <- fx$null_idx

  # genotypeKO recovers +beta1 (=2), treatedYes recovers +beta2 (=1.5), each on
  # the TRUE block; the HET coefficient (no planted effect) stays ~0. Tolerances
  # sit around the seeded observations (KO 1.94, treatedYes 1.58, HET 0.04).
  expect_equal(median(res$logFC.genotypeKO[ti]),  fx$beta1, tolerance = 0.2)
  expect_equal(median(res$logFC.treatedYes[ti]),  fx$beta2, tolerance = 0.2)
  expect_lt(abs(median(res$logFC.genotypeHET[ti])), 0.2)

  # Signs are unambiguous on the planted block.
  expect_true(all(res$logFC.genotypeKO[ti]  > 0))
  expect_true(all(res$logFC.treatedYes[ti]  > 0))

  # No cross-contamination: null features carry ~zero on every coefficient.
  expect_lt(abs(median(res$logFC.genotypeKO[ni])),  0.15)
  expect_lt(abs(median(res$logFC.treatedYes[ni])),  0.15)
  expect_lt(abs(median(res$logFC.genotypeHET[ni])), 0.15)
})

# ---- (b) F-test EMISSION rule (>=2 coefs) ------------------------------------

test_that("omnibus F-test is emitted for the 3-level factor, NOT the 2-level factor", {
  res <- fit_additive(make_additive_gct()$gct)

  # genotype contributes 2 non-intercept coefs -> omnibus F-block emitted.
  expect_true("F.genotype"         %in% colnames(res))
  expect_true("P.Value.genotype"   %in% colnames(res))
  expect_true("adj.P.Val.genotype" %in% colnames(res))
  # An F-block carries no single signed logFC.
  expect_false("logFC.genotype" %in% colnames(res))

  # treated contributes only 1 non-intercept coef -> NO F-block at all.
  expect_false("F.treated"         %in% colnames(res))
  expect_false("P.Value.treated"   %in% colnames(res))
  expect_false("adj.P.Val.treated" %in% colnames(res))
})

# ---- (b) F-test CALIBRATION under the null -----------------------------------

test_that("omnibus F.genotype is calibrated under a clean genotype-null", {
  fx  <- make_geno_null_gct()
  res <- fit_additive(fx$gct)

  p <- res$P.Value.genotype
  p <- p[!is.na(p)]
  expect_equal(length(p), fx$n_feat)

  # genotype has no effect anywhere: the omnibus F p-values are Uniform(0,1).
  #   - mean ~ 0.5 (seeded 0.505)
  #   - KS against uniform not rejected (seeded 0.76)
  #   - type-I fraction near nominal 0.05 (seeded 0.05)
  expect_equal(mean(p), 0.5, tolerance = 0.05)
  ks_p <- suppressWarnings(stats::ks.test(p, "punif"))$p.value
  expect_gt(ks_p, 0.01)
  expect_lt(abs(mean(p < 0.05) - 0.05), 0.03)

  # BH at 5% over a pure null yields essentially no calls (seeded 0 of 200).
  adj <- res$adj.P.Val.genotype
  expect_lt(sum(adj < 0.05, na.rm = TRUE), 0.01 * fx$n_feat)
})

# ---- (b) F-test POWER + FDR under a real effect ------------------------------

test_that("omnibus F.genotype is powered and FDR-controlled on the real effect", {
  fx  <- make_additive_gct()
  res <- fit_additive(fx$gct)
  ti  <- fx$true_idx
  ni  <- fx$null_idx

  # F-statistics are far larger on true (seeded mean ~21.6) than null (~1.05).
  expect_gt(median(res$F.genotype[ti]), 5 * median(res$F.genotype[ni]))

  # Nearly all true features are called by the omnibus F at BH 5% (seeded 1.00).
  expect_gt(mean(res$adj.P.Val.genotype[ti] < 0.05), 0.9)

  # Raw type-I among the (genotype-)null features stays near nominal. We use the
  # RAW p here, not BH-adj: within this mixed dataset the 40 strong true features
  # make BH generous, so a few nulls clear adj<0.05 -- expected BH behaviour, not
  # miscalibration. The clean FDR check lives in the companion-null test above.
  expect_lt(mean(res$P.Value.genotype[ni] < 0.05), 0.10)   # seeded 0.062
})

# ---- specificity across every per-coefficient call ---------------------------

test_that("per-coefficient specificity: near-zero false positives among nulls", {
  fx  <- make_additive_gct()
  res <- fit_additive(fx$gct)
  ni  <- fx$null_idx
  n_null <- length(ni)

  # BH at 5% over each per-coefficient column: null features are rarely called.
  # Seeded counts: genotypeKO 2, genotypeHET 0, treatedYes 1 of 80.
  expect_lt(sum(res$adj.P.Val.genotypeKO[ni]  < 0.05), 0.05 * n_null)
  expect_lt(sum(res$adj.P.Val.genotypeHET[ni] < 0.05), 0.05 * n_null)
  expect_lt(sum(res$adj.P.Val.treatedYes[ni]  < 0.05), 0.05 * n_null)

  # The true block is still detected on both real coefficients (specificity above
  # is not vacuous): planted KO and treated effects clear BH 5% en masse.
  ti <- fx$true_idx
  expect_gt(mean(res$adj.P.Val.genotypeKO[ti] < 0.05), 0.9)
  expect_gt(mean(res$adj.P.Val.treatedYes[ti] < 0.05), 0.9)
})
