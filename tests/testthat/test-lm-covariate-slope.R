################################################################################
# Ground-truth continuous-covariate slope recovery for lm.regression().
#
# The review flagged continuous covariates as having NO independent statistical
# validation: a variable typed "continuous" is coerced with
# as.numeric(as.character(x)) and enters the design as a single slope column, yet
# the only coverage was a circular golden snapshot (oracle = same-author limma;
# catches drift, not correctness). No test planted a KNOWN slope and checked
# recovery or t-test calibration.
#
# These tests plant a KNOWN per-unit slope (beta) on a block of true features and
# assert the fitted covariate coefficient recovers its magnitude, sign, and
# significance -- an analytic oracle, not a golden snapshot. Null features carry
# no slope and are used to confirm the covariate t-test is calibrated (p-values
# uniform, FDR controlled). One feature is cross-checked against a base-R
# lm(y ~ age) slope: eBayes shrinks the variance, not the coefficient, so the
# point estimates must agree to numerical precision.
#
# For a continuous covariate `age`, limma reports the coefficient as `logFC`, so
# `logFC.age` IS the per-unit slope and should recover the planted beta. `age` is
# forced continuous via variable_types (a numeric column with few unique values
# would auto-type as a factor by the app heuristic; here we pass it explicitly
# AND give age many distinct values so it is unambiguous).
#
# Thresholds are set around the observed (seeded) values with margin, so the
# tests assert the *statistical property*, not a brittle exact number.
################################################################################

library(testthat)

# ---- fixture -----------------------------------------------------------------

# Build a GCT with a continuous covariate `age` and a planted per-unit slope
# `beta` on the first `n_true` features; the remaining `n_null` are pure noise.
# `age` is CENTERED before being added to the signal so the planted slope does
# not perturb the intercept (a clean oracle: logFC.age ~ beta, intercept ~ 10).
#
# Empirical calibration (seed=101, beta=0.5, sigma=1, n=36, 100 true / 400 null):
#   TRUE : median logFC.age = 0.501, all sign > 0, 100% detected at BH 5%.
#   NULL : median logFC.age = -0.0007, mean p = 0.513, mean(p<0.05) = 0.0475,
#          KS p vs Uniform = 0.35, only 2/400 pass BH 5%.
#   base-R lm(y ~ age) slope on feature 1 matches logFC.age to 2.2e-16.
make_covariate_gct <- function(seed = 101L, beta = 0.5, sigma = 1,
                               n_null = 400L, n_true = 100L, n_samp = 36L) {
  set.seed(seed)
  n_feat     <- n_null + n_true
  sample_ids <- paste0("s", sprintf("%02d", seq_len(n_samp)))
  feat_ids   <- paste0("F", sprintf("%03d", seq_len(n_feat)))

  # Continuous covariate with many distinct values (never mistaken for a factor).
  age          <- runif(n_samp, 20, 70)
  age_centered <- age - mean(age)

  cdesc <- data.frame(
    id = sample_ids, age = age,
    row.names = sample_ids, stringsAsFactors = FALSE
  )
  rdesc <- data.frame(
    id = feat_ids, row.names = feat_ids, stringsAsFactors = FALSE
  )

  mat <- matrix(rnorm(n_feat * n_samp, mean = 10, sd = sigma),
                nrow = n_feat, dimnames = list(feat_ids, sample_ids))

  true_idx <- seq_len(n_true)
  # Plant the slope: y_true = noise + beta * age_centered. Null features untouched.
  for (i in true_idx) {
    mat[i, ] <- mat[i, ] + beta * age_centered
  }

  gct <- methods::new("GCT", mat = mat, rdesc = rdesc, cdesc = cdesc,
                      rid = feat_ids, cid = sample_ids)
  list(
    gct = gct, mat = mat, age = age,
    beta = beta, n_true = n_true, n_null = n_null,
    true_idx = true_idx, null_idx = (n_true + 1L):n_feat
  )
}

fit_covariate <- function(fx) {
  lm.regression(
    gct = fx$gct,
    formula_string = "~ age",
    variable_types = list(age = "continuous"),  # force continuous, not factor
    intensity = FALSE
  )
}

# ---- slope magnitude & sign --------------------------------------------------

test_that("continuous covariate: fitted logFC.age recovers the planted slope", {
  fx  <- make_covariate_gct()
  res <- fit_covariate(fx)

  # The continuous covariate contributes ONE slope column, named after the
  # coefficient (make.names of "age"). Confirm the exact columns exist.
  expect_true(all(c("logFC.age", "P.Value.age", "adj.P.Val.age") %in%
                    colnames(res)))
  # No spurious per-factor F-block for a single-coefficient continuous term.
  expect_false("F.age" %in% colnames(res))
  expect_equal(nrow(res), fx$n_true + fx$n_null)

  lfc <- res$logFC.age
  # Slope recovery: the median fitted logFC on true features ~ planted beta.
  # SNR here (beta=0.5, sigma=1, n=36, age range 50) gives a per-feature slope
  # SE ~0.02, so a tolerance of 0.05 clears the observed 0.501 with wide margin.
  expect_equal(median(lfc[fx$true_idx]), fx$beta, tolerance = 0.05)
  # Sign is unambiguous on the planted (positive-slope) block.
  expect_true(all(lfc[fx$true_idx] > 0))
})

test_that("continuous covariate: null features carry no slope (~ 0)", {
  fx  <- make_covariate_gct()
  res <- fit_covariate(fx)
  lfc <- res$logFC.age

  # Null features have no age dependence: their slopes scatter around zero.
  expect_equal(median(lfc[fx$null_idx]), 0, tolerance = 0.02)
  # True and null blocks are cleanly separated on aggregate slope.
  expect_gt(median(lfc[fx$true_idx]),
            median(lfc[fx$null_idx]) + 0.3)
})

# ---- t-test calibration on the null ------------------------------------------

test_that("continuous covariate: null p-values are uniform & well-calibrated", {
  fx  <- make_covariate_gct()
  res <- fit_covariate(fx)

  p <- res$P.Value.age[fx$null_idx]
  p <- p[!is.na(p)]
  expect_gt(length(p), 300)

  # Under the null (no slope), the covariate t-test yields Uniform(0,1) p-values:
  #   - mean ~ 0.5
  #   - a KS test against the uniform should NOT reject
  #   - the fraction below alpha ~ alpha (type-I rate near nominal)
  expect_equal(mean(p), 0.5, tolerance = 0.05)
  ks_p <- suppressWarnings(stats::ks.test(p, "punif"))$p.value
  expect_gt(ks_p, 0.01)                        # not rejected as non-uniform
  expect_lt(abs(mean(p < 0.05) - 0.05), 0.03)  # type-I rate near 0.05
})

# ---- FDR: power on true, specificity on null ---------------------------------

test_that("continuous covariate: FDR controls calls (power + specificity)", {
  fx  <- make_covariate_gct()
  res <- fit_covariate(fx)
  adj <- res$adj.P.Val.age

  # Specificity: essentially no null feature clears BH 5% (allow a stochastic
  # tail < 1% of the 400 nulls; observed = 2).
  n_false_pos <- sum(adj[fx$null_idx] < 0.05)
  expect_lt(n_false_pos, 0.01 * fx$n_null)

  # Sensitivity: with this SNR a large fraction of true features clear BH 5%
  # (observed = 100%), so the specificity check above is not vacuous.
  expect_gt(mean(adj[fx$true_idx] < 0.05), 0.7)

  # Realized false-discovery proportion among ALL calls stays controlled.
  called <- which(adj < 0.05)
  fdp <- mean(!(called %in% fx$true_idx))
  expect_lt(fdp, 0.05)
})

# ---- base-R lm oracle --------------------------------------------------------

test_that("continuous covariate: logFC.age matches a base-R lm(y ~ age) slope", {
  fx  <- make_covariate_gct()
  res <- fit_covariate(fx)

  # eBayes shrinks the residual variance (hence the p-value), NOT the coefficient
  # point estimate. So limma's per-feature logFC.age must equal the ordinary
  # least-squares slope from lm(y ~ age) to numerical precision. Check a true and
  # a null feature; res rows are in input order (id preserved), verified below.
  expect_equal(res$id, rownames(fx$mat))

  for (i in c(1L, fx$n_true + 1L)) {
    ols_slope <- unname(coef(lm(fx$mat[i, ] ~ fx$age))["fx$age"])
    if (is.na(ols_slope)) {
      ols_slope <- unname(coef(lm(fx$mat[i, ] ~ fx$age))[2])
    }
    expect_equal(res$logFC.age[i], ols_slope, tolerance = 1e-6)
  }
})
