################################################################################
# tests/lm-sandbox/synthesize_datasets.R
#
# Build the synthetic fixtures that the golden-file regression suite uses.
# Each fixture is a list of:
#   - mat          : numeric matrix (features x samples), already log2-scale-ish
#   - cdesc        : sample metadata data frame (rownames = sample IDs)
#   - rdesc        : feature metadata (rownames = feature IDs)
#   - planted      : integer vector of feature indices with a real effect
#   - meta         : a small list of generator parameters (seed, n, etc.)
#
# These fixtures are written to tests/lm-sandbox/data/*.rds.
#
# Conventions:
#   - Seeds: 101 (type1), 102 (type2), 103 (type3), 104 (continuous), 105 (trend).
#   - Intra-subject correlation rho = 0.5 (compound symmetric) where applicable.
#   - 30 features for the small models; 200 for the trend model so the variance
#     trend has enough points to fit.
################################################################################

suppressPackageStartupMessages({
  library(MASS)   # mvrnorm
})

OUT_DIR <- file.path("tests", "lm-sandbox", "data")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)


# ---- helpers -----------------------------------------------------------------

# Compound-symmetric covariance: var on diagonal, var*rho off-diagonal.
cs_cov <- function(n, var = 1, rho = 0.5) {
  m <- matrix(var * rho, nrow = n, ncol = n)
  diag(m) <- var
  m
}

# Sample n_subj subjects' n_time-vector responses with compound-symmetric corr.
# Returns matrix (subj x time).
rmvn_subjects <- function(n_subj, n_time, mean = 0, var = 1, rho = 0.5) {
  Sigma <- cs_cov(n_time, var = var, rho = rho)
  mu <- rep(mean, n_time)
  MASS::mvrnorm(n_subj, mu = mu, Sigma = Sigma)
}


# ---- Fixture 1: Type 1 -- RM with groups -------------------------------------
# 30 features x 12 samples; 4 subjects x 3 timepoints; each subject in one group;
# 5 group-effect features, 5 time-effect features, rest null.
build_type1 <- function() {
  set.seed(101)
  n_feat <- 30
  n_subj_per_group <- 2   # 2 per group * 2 groups = 4 subjects total
  n_time <- 3
  groups <- c("MUT", "WT")
  subjects <- c("S1", "S2", "S3", "S4")          # S1,S2 = MUT; S3,S4 = WT
  subj_group <- c(S1 = "MUT", S2 = "MUT", S3 = "WT", S4 = "WT")
  timepoints <- c("T1", "T2", "T3")
  n_samples <- length(subjects) * n_time          # 12

  sample_ids <- paste0(rep(subjects, each = n_time), "_", rep(timepoints, length(subjects)))
  cdesc <- data.frame(
    sample = sample_ids,
    subject = factor(rep(subjects, each = n_time)),
    time = factor(rep(timepoints, length(subjects)), levels = timepoints),
    group = factor(unname(subj_group[rep(subjects, each = n_time)]), levels = c("WT", "MUT")),
    row.names = sample_ids,
    stringsAsFactors = FALSE
  )

  # Build noise: for each feature, draw a 4x3 matrix of subject-correlated noise.
  mat <- matrix(NA_real_, nrow = n_feat, ncol = n_samples,
                dimnames = list(paste0("f", sprintf("%02d", 1:n_feat)), sample_ids))
  for (f in seq_len(n_feat)) {
    subj_resp <- rmvn_subjects(n_subj = length(subjects), n_time = n_time,
                                mean = 0, var = 1, rho = 0.5)
    rownames(subj_resp) <- subjects
    colnames(subj_resp) <- timepoints
    # write into matrix in sample order
    for (s in subjects) for (tt in timepoints) {
      col_name <- paste0(s, "_", tt)
      mat[f, col_name] <- subj_resp[s, tt]
    }
  }

  # Plant group-effect on features 1..5: MUT shifted by +1.5
  group_feat <- 1:5
  for (f in group_feat) {
    mut_cols <- which(cdesc$group == "MUT")
    mat[f, mut_cols] <- mat[f, mut_cols] + 1.5
  }

  # Plant time-effect on features 6..10: T2 shifted +1.0, T3 shifted +2.0 vs T1
  time_feat <- 6:10
  for (f in time_feat) {
    t2_cols <- which(cdesc$time == "T2")
    t3_cols <- which(cdesc$time == "T3")
    mat[f, t2_cols] <- mat[f, t2_cols] + 1.0
    mat[f, t3_cols] <- mat[f, t3_cols] + 2.0
  }

  rdesc <- data.frame(
    id = rownames(mat),
    row.names = rownames(mat),
    stringsAsFactors = FALSE
  )

  list(
    mat = mat, cdesc = cdesc, rdesc = rdesc,
    planted = list(group = group_feat, time = time_feat),
    meta = list(name = "type1_rm_with_groups", seed = 101, rho = 0.5,
                n_features = n_feat, n_samples = n_samples)
  )
}


# ---- Fixture 2: Type 2 -- RM only --------------------------------------------
# 30 features x 9 samples; 3 subjects x 3 timepoints; 5 time-effect features.
build_type2 <- function() {
  set.seed(102)
  n_feat <- 30
  subjects <- c("P1", "P2", "P3")
  timepoints <- c("T1", "T2", "T3")
  n_samples <- length(subjects) * length(timepoints)
  sample_ids <- paste0(rep(subjects, each = length(timepoints)), "_",
                       rep(timepoints, length(subjects)))
  cdesc <- data.frame(
    sample = sample_ids,
    subject = factor(rep(subjects, each = length(timepoints))),
    time = factor(rep(timepoints, length(subjects)), levels = timepoints),
    row.names = sample_ids,
    stringsAsFactors = FALSE
  )

  mat <- matrix(NA_real_, nrow = n_feat, ncol = n_samples,
                dimnames = list(paste0("f", sprintf("%02d", 1:n_feat)), sample_ids))
  for (f in seq_len(n_feat)) {
    subj_resp <- rmvn_subjects(n_subj = length(subjects), n_time = length(timepoints),
                                mean = 0, var = 1, rho = 0.5)
    rownames(subj_resp) <- subjects
    colnames(subj_resp) <- timepoints
    for (s in subjects) for (tt in timepoints) {
      mat[f, paste0(s, "_", tt)] <- subj_resp[s, tt]
    }
  }

  time_feat <- 1:5
  for (f in time_feat) {
    mat[f, cdesc$time == "T2"] <- mat[f, cdesc$time == "T2"] + 1.0
    mat[f, cdesc$time == "T3"] <- mat[f, cdesc$time == "T3"] + 2.0
  }

  rdesc <- data.frame(id = rownames(mat), row.names = rownames(mat))

  list(
    mat = mat, cdesc = cdesc, rdesc = rdesc,
    planted = list(time = time_feat),
    meta = list(name = "type2_rm_only", seed = 102, rho = 0.5,
                n_features = n_feat, n_samples = n_samples)
  )
}


# ---- Fixture 3: Type 3 -- contrasts ------------------------------------------
# 30 features x 12 samples; 4 conditions x 3 replicates.
# Plant 5 A-vs-B effect features and 5 A-vs-C effect features.
build_type3 <- function() {
  set.seed(103)
  n_feat <- 30
  conditions <- c("A", "B", "C", "D")
  n_rep <- 3
  n_samples <- length(conditions) * n_rep
  sample_ids <- paste0(rep(conditions, each = n_rep), "_R", rep(1:n_rep, length(conditions)))
  cdesc <- data.frame(
    sample = sample_ids,
    condition = factor(rep(conditions, each = n_rep), levels = conditions),
    row.names = sample_ids,
    stringsAsFactors = FALSE
  )

  mat <- matrix(rnorm(n_feat * n_samples), nrow = n_feat, ncol = n_samples,
                dimnames = list(paste0("f", sprintf("%02d", 1:n_feat)), sample_ids))

  ab_feat <- 1:5
  for (f in ab_feat) mat[f, cdesc$condition == "B"] <- mat[f, cdesc$condition == "B"] + 1.5

  ac_feat <- 6:10
  for (f in ac_feat) mat[f, cdesc$condition == "C"] <- mat[f, cdesc$condition == "C"] - 1.5

  rdesc <- data.frame(id = rownames(mat), row.names = rownames(mat))

  list(
    mat = mat, cdesc = cdesc, rdesc = rdesc,
    planted = list(ab = ab_feat, ac = ac_feat),
    meta = list(name = "type3_contrasts", seed = 103,
                n_features = n_feat, n_samples = n_samples)
  )
}


# ---- Fixture 4: continuous covariate -----------------------------------------
# 30 features x 16 samples; age in [25, 65]; 5 features with linear age effect.
build_continuous <- function() {
  set.seed(104)
  n_feat <- 30
  n_samples <- 16
  ages <- round(seq(25, 65, length.out = n_samples))
  sample_ids <- paste0("S", sprintf("%02d", 1:n_samples))
  cdesc <- data.frame(
    sample = sample_ids,
    age = as.numeric(ages),
    row.names = sample_ids,
    stringsAsFactors = FALSE
  )

  mat <- matrix(rnorm(n_feat * n_samples), nrow = n_feat, ncol = n_samples,
                dimnames = list(paste0("f", sprintf("%02d", 1:n_feat)), sample_ids))
  age_feat <- 1:5
  for (f in age_feat) {
    mat[f, ] <- mat[f, ] + 0.05 * (ages - mean(ages))   # ~+/- 1.0 at extremes
  }

  rdesc <- data.frame(id = rownames(mat), row.names = rownames(mat))

  list(
    mat = mat, cdesc = cdesc, rdesc = rdesc,
    planted = list(age = age_feat),
    meta = list(name = "continuous_covariate", seed = 104,
                n_features = n_feat, n_samples = n_samples)
  )
}


# ---- Fixture 5: intensity-trend ---------------------------------------------
# 200 features x 12 samples; mean abundance ~ Uniform(15, 30);
# sd decreases with mean (mean-variance trend). 6 conditions x 2 reps.
build_intensity_trend <- function() {
  set.seed(105)
  n_feat <- 200
  conditions <- c("Ctrl", "Trt")
  n_rep <- 6
  n_samples <- length(conditions) * n_rep
  sample_ids <- paste0(rep(conditions, each = n_rep), "_R", rep(1:n_rep, length(conditions)))
  cdesc <- data.frame(
    sample = sample_ids,
    condition = factor(rep(conditions, each = n_rep), levels = conditions),
    row.names = sample_ids,
    stringsAsFactors = FALSE
  )

  feature_means <- runif(n_feat, min = 15, max = 30)
  sd_floor <- 0.15
  sd_slope <- 0.6 / 15  # sd at mean=15 is 0.75; sd at mean=30 is 0.15
  feature_sds <- sd_floor + sd_slope * (30 - feature_means)

  mat <- matrix(NA_real_, nrow = n_feat, ncol = n_samples,
                dimnames = list(paste0("f", sprintf("%03d", 1:n_feat)), sample_ids))
  for (f in seq_len(n_feat)) {
    mat[f, ] <- rnorm(n_samples, mean = feature_means[f], sd = feature_sds[f])
  }

  # Plant ~10 differential features
  diff_feat <- 1:10
  for (f in diff_feat) mat[f, cdesc$condition == "Trt"] <- mat[f, cdesc$condition == "Trt"] + 1.2

  rdesc <- data.frame(
    id = rownames(mat),
    feature_mean = feature_means,
    feature_sd = feature_sds,
    row.names = rownames(mat),
    stringsAsFactors = FALSE
  )

  list(
    mat = mat, cdesc = cdesc, rdesc = rdesc,
    planted = list(diff = diff_feat),
    meta = list(name = "intensity_trend", seed = 105,
                n_features = n_feat, n_samples = n_samples,
                sd_floor = sd_floor, sd_slope = sd_slope)
  )
}


# ---- Build + persist ---------------------------------------------------------

fixtures <- list(
  type1_rm_with_groups = build_type1(),
  type2_rm_only        = build_type2(),
  type3_contrasts      = build_type3(),
  continuous_covariate = build_continuous(),
  intensity_trend      = build_intensity_trend()
)

for (nm in names(fixtures)) {
  out <- file.path(OUT_DIR, paste0(nm, ".rds"))
  saveRDS(fixtures[[nm]], out)
  cat("wrote", out, "\n")
}

cat("Done. Wrote", length(fixtures), "fixtures to", OUT_DIR, "\n")
