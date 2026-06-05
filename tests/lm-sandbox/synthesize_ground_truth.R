################################################################################
# tests/lm-sandbox/synthesize_ground_truth.R
#
# Extension fixtures for GROUND-TRUTH statistical-correctness testing.
#
# The original `synthesize_datasets.R` produces fixtures used for golden-file
# regression (does protigy match isolated limma?). These additional fixtures
# target a different question: is the statistical backbone *correct* against
# known ground truth? They encode effects whose truth we control, so tests can
# assert recovery, calibration, sign conventions, and blocking behaviour rather
# than mere non-drift.
#
# Each fixture is the same list shape as the originals:
#   - mat      : numeric matrix (features x samples), log2-scale-ish
#   - cdesc    : sample metadata (rownames = sample IDs)
#   - rdesc    : feature metadata (rownames = feature IDs)
#   - planted  : named list of integer feature indices with a real effect
#   - truth    : per-fixture ground-truth parameters (effect sizes, etc.)
#   - meta     : generator parameters (seed, n, ...)
#
# Seeds: 201..205 (the originals use 101..105 — no collision).
#
# Fixtures written to tests/lm-sandbox/data/gt_*.rds:
#   gt_pure_null        — no effects anywhere (p-value calibration / FDR)
#   gt_sign_convention  — large, unambiguous directional effects (sign algebra)
#   gt_power_recovery   — graded effect sizes across many features (recovery)
#   gt_blocking         — strong within-subject correlation (blocking helps)
#   gt_rank_deficient   — a collinear/aliased covariate (graceful degradation)
################################################################################

suppressPackageStartupMessages({
  library(MASS)   # mvrnorm
})

OUT_DIR <- file.path("tests", "lm-sandbox", "data")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)


# ---- helpers -----------------------------------------------------------------

cs_cov <- function(n, var = 1, rho = 0.5) {
  m <- matrix(var * rho, nrow = n, ncol = n)
  diag(m) <- var
  m
}

feat_ids <- function(n) paste0("f", sprintf("%04d", seq_len(n)))


# ---- Fixture A: pure null ----------------------------------------------------
# Many features, two balanced groups, NO planted effect anywhere. Used to verify
# that nominal p-values are ~Uniform(0,1) under the null and that BH-FDR controls
# the false-discovery proportion. Large n_feat so the KS / proportion checks are
# stable.
build_pure_null <- function() {
  set.seed(201)
  n_feat <- 2000
  conditions <- c("Ctrl", "Trt")
  n_rep <- 8
  n_samples <- length(conditions) * n_rep
  sample_ids <- paste0(rep(conditions, each = n_rep), "_R",
                       rep(seq_len(n_rep), length(conditions)))
  cdesc <- data.frame(
    sample = sample_ids,
    condition = factor(rep(conditions, each = n_rep), levels = conditions),
    row.names = sample_ids,
    stringsAsFactors = FALSE
  )

  # iid N(0,1) everywhere. No group shift, no feature-specific mean. This is the
  # global null: every feature's true logFC is exactly 0.
  mat <- matrix(rnorm(n_feat * n_samples), nrow = n_feat, ncol = n_samples,
                dimnames = list(feat_ids(n_feat), sample_ids))

  rdesc <- data.frame(id = rownames(mat), row.names = rownames(mat))

  list(
    mat = mat, cdesc = cdesc, rdesc = rdesc,
    planted = list(),                       # nothing is real
    truth = list(true_logFC = 0, n_true_positive = 0),
    meta = list(name = "gt_pure_null", seed = 201,
                n_features = n_feat, n_samples = n_samples)
  )
}


# ---- Fixture B: sign convention ----------------------------------------------
# Three conditions A/B/C, low noise, very large planted effects so the SIGN is
# unambiguous. Used to verify coefficient/contrast signs against the math:
#   B is shifted UP   vs A by +3   -> logFC(B vs A) ~ +3
#   C is shifted DOWN vs A by -3   -> logFC(C vs A) ~ -3
# Low sigma keeps the estimate tight enough for exact-ish magnitude checks.
build_sign_convention <- function() {
  set.seed(202)
  n_feat <- 40
  conditions <- c("A", "B", "C")
  n_rep <- 6
  n_samples <- length(conditions) * n_rep
  sigma <- 0.10
  sample_ids <- paste0(rep(conditions, each = n_rep), "_R",
                       rep(seq_len(n_rep), length(conditions)))
  cdesc <- data.frame(
    sample = sample_ids,
    condition = factor(rep(conditions, each = n_rep), levels = conditions),
    row.names = sample_ids,
    stringsAsFactors = FALSE
  )

  mat <- matrix(rnorm(n_feat * n_samples, sd = sigma),
                nrow = n_feat, ncol = n_samples,
                dimnames = list(feat_ids(n_feat), sample_ids))

  shift_up <- 3.0      # B vs A
  shift_dn <- -3.0     # C vs A
  up_feat <- 1:20      # B differs from A on these
  dn_feat <- 1:20      # C differs from A on these (same features, opposite dir)
  for (f in up_feat) mat[f, cdesc$condition == "B"] <- mat[f, cdesc$condition == "B"] + shift_up
  for (f in dn_feat) mat[f, cdesc$condition == "C"] <- mat[f, cdesc$condition == "C"] + shift_dn

  rdesc <- data.frame(id = rownames(mat), row.names = rownames(mat))

  list(
    mat = mat, cdesc = cdesc, rdesc = rdesc,
    planted = list(B_vs_A = up_feat, C_vs_A = dn_feat),
    truth = list(shift_B_vs_A = shift_up, shift_C_vs_A = shift_dn,
                 sigma = sigma, n_per_group = n_rep),
    meta = list(name = "gt_sign_convention", seed = 202,
                n_features = n_feat, n_samples = n_samples)
  )
}


# ---- Fixture C: power / recovery ---------------------------------------------
# Two groups, a block of clearly-differential features with a known shift, and a
# large block of true nulls. Used to verify the pipeline RECOVERS planted hits
# (high sensitivity on the true positives) while keeping the null block mostly
# non-significant (specificity). Moderate noise so it is a real test, not a
# layup.
build_power_recovery <- function() {
  set.seed(203)
  n_feat <- 1000
  conditions <- c("Ctrl", "Trt")
  n_rep <- 6
  n_samples <- length(conditions) * n_rep
  sigma <- 1.0
  sample_ids <- paste0(rep(conditions, each = n_rep), "_R",
                       rep(seq_len(n_rep), length(conditions)))
  cdesc <- data.frame(
    sample = sample_ids,
    condition = factor(rep(conditions, each = n_rep), levels = conditions),
    row.names = sample_ids,
    stringsAsFactors = FALSE
  )

  mat <- matrix(rnorm(n_feat * n_samples, sd = sigma),
                nrow = n_feat, ncol = n_samples,
                dimnames = list(feat_ids(n_feat), sample_ids))

  # First 100 features are true positives with a +2 shift in Trt (SNR = 2/sigma,
  # n=6/group -> high but not certain power). Remaining 900 are true nulls.
  shift <- 2.0
  tp_feat <- 1:100
  for (f in tp_feat) mat[f, cdesc$condition == "Trt"] <- mat[f, cdesc$condition == "Trt"] + shift

  rdesc <- data.frame(
    id = rownames(mat),
    is_true_positive = seq_len(n_feat) %in% tp_feat,
    row.names = rownames(mat),
    stringsAsFactors = FALSE
  )

  list(
    mat = mat, cdesc = cdesc, rdesc = rdesc,
    planted = list(trt = tp_feat),
    truth = list(shift = shift, sigma = sigma, n_true_positive = length(tp_feat),
                 n_true_null = n_feat - length(tp_feat)),
    meta = list(name = "gt_power_recovery", seed = 203,
                n_features = n_feat, n_samples = n_samples)
  )
}


# ---- Fixture D: blocking (within-subject correlation) ------------------------
# Crossover design: each subject measured at two timepoints (Pre/Post) with
# STRONG within-subject correlation (rho = 0.8). The Post-Pre effect is modest.
# With strong subject correlation, a blocked analysis (duplicateCorrelation on
# subject) should be MORE powerful than ignoring the pairing. Used to verify the
# blocking machinery actually accounts for correlation (more hits / smaller
# p-values on planted features when blocked).
build_blocking <- function() {
  set.seed(204)
  n_feat <- 400
  n_subj <- 10
  timepoints <- c("Pre", "Post")
  n_time <- length(timepoints)
  n_samples <- n_subj * n_time
  rho <- 0.8
  subj_var <- 1.0

  subjects <- paste0("S", sprintf("%02d", seq_len(n_subj)))
  sample_ids <- paste0(rep(subjects, each = n_time), "_",
                       rep(timepoints, n_subj))
  cdesc <- data.frame(
    sample = sample_ids,
    subject = factor(rep(subjects, each = n_time)),
    time = factor(rep(timepoints, n_subj), levels = timepoints),
    row.names = sample_ids,
    stringsAsFactors = FALSE
  )

  Sigma <- cs_cov(n_time, var = subj_var, rho = rho)
  mat <- matrix(NA_real_, nrow = n_feat, ncol = n_samples,
                dimnames = list(feat_ids(n_feat), sample_ids))
  for (f in seq_len(n_feat)) {
    # Each subject draws a correlated (Pre, Post) pair.
    pairs <- MASS::mvrnorm(n_subj, mu = c(0, 0), Sigma = Sigma)  # n_subj x 2
    for (i in seq_len(n_subj)) {
      mat[f, paste0(subjects[i], "_Pre")]  <- pairs[i, 1]
      mat[f, paste0(subjects[i], "_Post")] <- pairs[i, 2]
    }
  }

  # Planted Post-Pre effect on the first 80 features: +0.8 (modest vs subj_var).
  shift <- 0.8
  tp_feat <- 1:80
  for (f in tp_feat) {
    mat[f, cdesc$time == "Post"] <- mat[f, cdesc$time == "Post"] + shift
  }

  rdesc <- data.frame(
    id = rownames(mat),
    is_true_positive = seq_len(n_feat) %in% tp_feat,
    row.names = rownames(mat),
    stringsAsFactors = FALSE
  )

  list(
    mat = mat, cdesc = cdesc, rdesc = rdesc,
    planted = list(time = tp_feat),
    truth = list(shift = shift, rho = rho, subj_var = subj_var,
                 n_true_positive = length(tp_feat)),
    meta = list(name = "gt_blocking", seed = 204,
                n_features = n_feat, n_samples = n_samples)
  )
}


# ---- Fixture E: rank-deficient design ----------------------------------------
# A factor `treatment` (Drug/Vehicle) plus a `batch` column that is PERFECTLY
# aliased with treatment (every Drug sample is batch1, every Vehicle sample is
# batch2). A model `~ treatment + batch` is rank-deficient: limma will emit an NA
# coefficient and lm.regression() should WARN (rank-deficiency preflight) rather
# than crash. Used to verify graceful degradation, not numerical equivalence.
build_rank_deficient <- function() {
  set.seed(205)
  n_feat <- 30
  treatment <- rep(c("Drug", "Vehicle"), each = 6)
  batch     <- ifelse(treatment == "Drug", "batch1", "batch2")  # perfectly aliased
  n_samples <- length(treatment)
  sample_ids <- paste0("S", sprintf("%02d", seq_len(n_samples)))
  cdesc <- data.frame(
    sample = sample_ids,
    treatment = factor(treatment, levels = c("Vehicle", "Drug")),
    batch = factor(batch),
    row.names = sample_ids,
    stringsAsFactors = FALSE
  )

  mat <- matrix(rnorm(n_feat * n_samples), nrow = n_feat, ncol = n_samples,
                dimnames = list(feat_ids(n_feat), sample_ids))
  # A real treatment effect on a few features so the estimable coef is non-trivial.
  for (f in 1:5) mat[f, cdesc$treatment == "Drug"] <- mat[f, cdesc$treatment == "Drug"] + 1.5

  rdesc <- data.frame(id = rownames(mat), row.names = rownames(mat))

  list(
    mat = mat, cdesc = cdesc, rdesc = rdesc,
    planted = list(treatment = 1:5),
    truth = list(aliased = c("treatment", "batch"), shift = 1.5),
    meta = list(name = "gt_rank_deficient", seed = 205,
                n_features = n_feat, n_samples = n_samples)
  )
}


# ---- Build + persist ---------------------------------------------------------

fixtures <- list(
  gt_pure_null       = build_pure_null(),
  gt_sign_convention = build_sign_convention(),
  gt_power_recovery  = build_power_recovery(),
  gt_blocking        = build_blocking(),
  gt_rank_deficient  = build_rank_deficient()
)

for (nm in names(fixtures)) {
  out <- file.path(OUT_DIR, paste0(nm, ".rds"))
  saveRDS(fixtures[[nm]], out)
  cat("wrote", out, "\n")
}

cat("Done. Wrote", length(fixtures), "ground-truth fixtures to", OUT_DIR, "\n")
