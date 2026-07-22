################################################################################
# Blocked-path CALIBRATION for lm.regression().
#
# test-lm-ground-truth.R already checks that duplicateCorrelation blocking gives
# a POWER GAIN over the unblocked fit (more hits on a planted within-subject
# effect). It never checks that the blocked p-values are CALIBRATED: a fit that
# understates the residual variance would look "more powerful" while silently
# inflating the type-I error. Power without calibration is a false discovery
# machine.
#
# This suite closes that gap. We build a within-subject-CORRELATED NULL (compound
# symmetry via a per-subject random effect) in which the tested `time` factor has
# NO true effect, and assert that the blocked fit's p-values for the null `time`
# coefficient are ~Uniform(0,1): mean ~ 0.5, KS-vs-uniform not rejected, type-I
# rate ~ 0.05, and BH-FDR yields essentially no calls.
#
# A positive control (a planted within-subject effect the blocked fit recovers)
# guards against the calibration test passing vacuously on a broken/degenerate
# fit, and an independent limma::duplicateCorrelation oracle confirms the planted
# within-subject correlation is what the machinery actually sees.
#
# Design mirrors tests/lm-sandbox/synthesize_ground_truth.R build_blocking():
# N subjects x 2 timepoints, each subject's (T1,T2) pair drawn from a
# compound-symmetric MVN so samples within a subject are correlated.
#
# Thresholds are loose around the observed (seeded) values so the tests assert
# the statistical PROPERTY (calibration), not a brittle exact number. Empirical
# margins over seeds {101,202,303,404,909}: mean(p) in [0.498,0.508],
# type-I in [0.043,0.051], KS p in [0.23,0.87], BH<0.05 always 0.
################################################################################

library(testthat)

# ---- fixture builders --------------------------------------------------------

# Compound-symmetric covariance for one subject's repeated measurements: equal
# variance on the diagonal, equal within-subject covariance off-diagonal.
.cs_cov <- function(n, var = 1, rho = 0.5) {
  m <- matrix(var * rho, nrow = n, ncol = n)
  diag(m) <- var
  m
}

# Within-subject-correlated NULL: n_subj subjects each measured at 2 timepoints,
# with a per-subject random effect (compound symmetry, correlation `rho`). The
# tested `time` factor carries NO true effect -> timeT2 is a pure null
# coefficient. `subject` is the block, passed via blocking_var (NOT a formula
# variable). Many features so the calibration statistics are meaningful.
build_null_blocking <- function(seed = 909L, n_feat = 1500L, n_subj = 12L,
                                 rho = 0.7, subj_var = 1.0) {
  set.seed(seed)
  timepoints <- c("T1", "T2")
  n_time <- length(timepoints)
  n_samples <- n_subj * n_time
  subjects <- paste0("S", sprintf("%02d", seq_len(n_subj)))
  sample_ids <- paste0(rep(subjects, each = n_time), "_",
                       rep(timepoints, n_subj))

  cdesc <- data.frame(
    id = sample_ids,
    subject = factor(rep(subjects, each = n_time)),
    time = factor(rep(timepoints, n_subj), levels = timepoints),
    row.names = sample_ids, stringsAsFactors = FALSE
  )

  Sigma <- .cs_cov(n_time, var = subj_var, rho = rho)
  fids <- paste0("f", sprintf("%04d", seq_len(n_feat)))
  mat <- matrix(NA_real_, nrow = n_feat, ncol = n_samples,
                dimnames = list(fids, sample_ids))
  for (f in seq_len(n_feat)) {
    # Each subject's (T1, T2) pair is a correlated draw -> compound symmetry.
    pairs <- MASS::mvrnorm(n_subj, mu = c(0, 0), Sigma = Sigma)  # n_subj x 2
    for (i in seq_len(n_subj)) {
      mat[f, paste0(subjects[i], "_T1")] <- pairs[i, 1]
      mat[f, paste0(subjects[i], "_T2")] <- pairs[i, 2]
    }
  }
  # NO time effect is planted: timeT2 is a pure null coefficient.

  rdesc <- data.frame(id = fids, row.names = fids, stringsAsFactors = FALSE)
  list(mat = mat, cdesc = cdesc, rdesc = rdesc,
       rho = rho, subj_var = subj_var, n_feat = n_feat, n_subj = n_subj)
}

# Same correlated structure, but plant a real within-subject Post-vs-Pre effect
# on the first `n_true` features so the calibration test is not vacuously passing
# on a degenerate fit.
build_pos_blocking <- function(seed = 555L, shift = 1.2, n_true = 200L) {
  fx <- build_null_blocking(seed = seed)
  is_t2 <- fx$cdesc$time == "T2"
  fx$mat[seq_len(n_true), is_t2] <- fx$mat[seq_len(n_true), is_t2] + shift
  fx$shift <- shift
  fx$n_true <- n_true
  fx
}

wrap_gct <- function(fx) {
  methods::new("GCT", mat = fx$mat, cdesc = fx$cdesc, rdesc = fx$rdesc,
               rid = rownames(fx$mat), cid = colnames(fx$mat))
}

fit_blocked <- function(fx) {
  lm.regression(
    gct = wrap_gct(fx),
    formula_string = "~ time",
    variable_types = list(time = "factor"),
    blocking_var = "subject",              # block, NOT a formula variable
    reference_levels = list(time = "T1"),
    intensity = FALSE
  )
}


# ============================================================================ #
# 1. BLOCKED NULL CALIBRATION                                                   #
# ============================================================================ #
test_that("blocked fit under a correlated null: p-values are uniform / calibrated", {
  fx  <- build_null_blocking()   # seed 909: no time effect, rho=0.7 within-subject
  res <- fit_blocked(fx)

  p <- res$P.Value.timeT2
  p <- p[!is.na(p)]
  expect_equal(length(p), fx$n_feat)   # every feature reported, none dropped

  # Under the null, the tested coefficient's p-values are Uniform(0,1) EVEN THOUGH
  # samples are within-subject correlated -- this is exactly what blocking must
  # buy us. (Empirically mean(p)=0.498, type-I=0.049, KS p=0.23 at this seed.)
  expect_equal(mean(p), 0.5, tolerance = 0.05)
  ks_p <- suppressWarnings(stats::ks.test(p, "punif"))$p.value
  expect_gt(ks_p, 0.01)                       # not rejected as non-uniform
  expect_lt(abs(mean(p < 0.05) - 0.05), 0.02) # type-I rate near nominal 0.05
})

test_that("blocked null: BH-FDR yields essentially no false discoveries", {
  fx  <- build_null_blocking()
  res <- fit_blocked(fx)
  adj <- res$adj.P.Val.timeT2
  adj <- adj[!is.na(adj)]
  # Every feature is a true null. BH at 5% should call essentially nothing;
  # allow a tiny stochastic-tail slack (< 0.5% of features). Observed: 0.
  expect_lt(sum(adj < 0.05), 0.005 * length(adj))
})


# ============================================================================ #
# 2. POSITIVE CONTROL (calibration is not vacuous)                             #
# ============================================================================ #
test_that("blocked fit recovers a planted within-subject effect", {
  fx  <- build_pos_blocking()   # +1.2 Post-vs-Pre on the first 200 features
  res <- fit_blocked(fx)

  tp   <- seq_len(fx$n_true)
  null <- (fx$n_true + 1L):nrow(fx$mat)

  # logFC(T2 - T1) is an unbiased estimate of the planted shift on true features
  # and ~0 on nulls; direction is unambiguous.
  expect_equal(median(res$logFC.timeT2[tp]), fx$shift, tolerance = 0.2)
  expect_lt(abs(median(res$logFC.timeT2[null])), 0.1)
  expect_true(all(res$logFC.timeT2[tp] > 0))

  # Blocking accounts for the within-subject correlation, so the paired effect is
  # detected with high sensitivity while FDR on the nulls stays controlled.
  expect_gt(mean(res$adj.P.Val.timeT2[tp] < 0.05), 0.8)   # observed ~0.98
  expect_lt(mean(res$adj.P.Val.timeT2[null] < 0.05), 0.05) # observed ~0.002
})


# ============================================================================ #
# 3. INDEPENDENT ORACLE: within-subject correlation is real                    #
# ============================================================================ #
test_that("duplicateCorrelation sees the planted within-subject correlation", {
  fx <- build_null_blocking()   # planted rho = 0.7, subj_var = 1.0
  design <- stats::model.matrix(~ time, data = fx$cdesc)
  dc <- limma::duplicateCorrelation(fx$mat, design, block = fx$cdesc$subject)

  # The consensus correlation lmFit is fed must be a sane POSITIVE value near the
  # planted rho (observed ~0.71); this is what makes the blocked null calibrated
  # rather than anticonservative. A near-zero or negative estimate would mean the
  # correlation structure was ignored -- which is precisely the bug this file
  # guards against.
  expect_gt(dc$consensus.correlation, 0.4)
  expect_lt(dc$consensus.correlation, 0.9)
  expect_equal(dc$consensus.correlation, fx$rho, tolerance = 0.15)
})
