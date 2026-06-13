################################################################################
# Tests for the PELSA per-sample quantified-peptide depth helpers
# (R/tab_pelsa_depth_helpers.R): pelsa_peptides_per_sample() and
# pelsa_depth_summary().
#
# CLOSED-FORM GROUND TRUTH (these comments ARE the reference):
#
# Source = the PROCESSED GCT (log2) matrix. A peptide is "quantified" for a
# sample iff its value is FINITE AND strictly > 0 -- the EXACT mask the PELSA
# notebook uses (is.finite(x) & x > 0). NOTE: log2 values CAN be negative, and
# a negative log2 value is a real measurement, but the notebook mask still
# counts only strictly-positive finite values, so we follow it literally.
#
# Tiny PROCESSED (log2) matrix, 5 peptide rows x 3 sample cols:
#
#        S1     S2     S3
#   r1:   2.0    1.5    NA      (NA in S3 -> NOT counted there)
#   r2:   0.0   -1.0    3.0     (0 not >0 in S1; -1 not >0 in S2; 3 counted S3)
#   r3:   5.0    NA     0.0     (NA S2 not counted; 0 S3 not counted)
#   r4:  -2.0    4.0    1.0     (-2 not >0 in S1; 4 counted; 1 counted)
#   r5:   Inf    2.0   -0.5     (Inf not finite -> NOT counted S1)
#
# Per-sample quantified counts (finite & > 0):
#   S1: r1(2.0 yes), r2(0 no), r3(5 yes), r4(-2 no), r5(Inf no)        -> 2
#   S2: r1(1.5 yes), r2(-1 no), r3(NA no), r4(4 yes), r5(2 yes)        -> 3
#   S3: r1(NA no), r2(3 yes), r3(0 no), r4(1 yes), r5(-0.5 no)         -> 2
#   => c(S1 = 2L, S2 = 3L, S3 = 2L)
#
# --- pelsa_depth_summary over a known count vector c(100, 120, 80) -----------
#   mean_n   = 100
#   median_n = 100
#   sd (sample, ddof=1) = sqrt(((100-100)^2 + (120-100)^2 + (80-100)^2)/2)
#                       = sqrt((0 + 400 + 400)/2) = sqrt(400) = 20
#   cv_pct   = 20 / 100 * 100 = 20   (PLAIN linear CV of the COUNTS, NOT of
#                                     intensities -- the single CV definition.)
################################################################################

source(testthat::test_path("fixtures/pelsa/generate_synthetic.R"))

# Build the closed-form tiny PROCESSED (log2) matrix used across tests.
.depth_tiny_mat <- function() {
  matrix(
    c(
      2.0,  1.5,  NA,
      0.0, -1.0,  3.0,
      5.0,  NA,   0.0,
      -2.0, 4.0,  1.0,
      Inf,  2.0, -0.5
    ),
    nrow = 5, byrow = TRUE,
    dimnames = list(NULL, c("S1", "S2", "S3"))
  )
}

# --------------------------------------------------------------------------
# pelsa_peptides_per_sample
# --------------------------------------------------------------------------

test_that("pelsa_peptides_per_sample counts finite & >0 per sample (closed form)", {
  mat <- .depth_tiny_mat()
  got <- pelsa_peptides_per_sample(mat)

  exp <- c(S1 = 2L, S2 = 3L, S3 = 2L)
  expect_equal(got, exp)
  expect_type(got, "integer")
  expect_equal(names(got), c("S1", "S2", "S3"))
})

test_that("pelsa_peptides_per_sample treats NA, 0 and negative as NOT quantified", {
  # A column that is all NA / 0 / negative -> count 0; an all-positive col -> nrow.
  mat <- matrix(
    c(
      NA,  0.0, 1.0,
      0.0, -3.0, 2.0,
      -1.0, NA, 4.0
    ),
    nrow = 3, byrow = TRUE,
    dimnames = list(NULL, c("AllBad1", "AllBad2", "AllGood"))
  )
  got <- pelsa_peptides_per_sample(mat)
  expect_equal(got, c(AllBad1 = 0L, AllBad2 = 0L, AllGood = 3L))
})

test_that("pelsa_peptides_per_sample coerces a data.frame to matrix", {
  mat <- .depth_tiny_mat()
  df <- as.data.frame(mat, check.names = FALSE)
  got <- pelsa_peptides_per_sample(df)
  expect_equal(got, pelsa_peptides_per_sample(mat))
})

test_that("pelsa_peptides_per_sample validates inputs and fails fast", {
  mat <- .depth_tiny_mat()
  # character matrix -> not numeric
  bad <- matrix(as.character(mat), nrow = 5, dimnames = dimnames(mat))
  expect_error(pelsa_peptides_per_sample(bad))
  # matrix without column names
  no_names <- matrix(c(1, 2, 3, 4), nrow = 2)
  expect_error(pelsa_peptides_per_sample(no_names))
})

test_that("pelsa_peptides_per_sample errors on duplicate column (sample) names", {
  # Duplicate sample names make the named-integer return ambiguous for
  # downstream counts["S1"] selection -> fail fast.
  dup <- matrix(
    c(1.0, 2.0, 3.0, 4.0),
    nrow = 2,
    dimnames = list(NULL, c("S1", "S1"))
  )
  expect_error(pelsa_peptides_per_sample(dup))
})

# --------------------------------------------------------------------------
# pelsa_depth_summary
# --------------------------------------------------------------------------

test_that("pelsa_depth_summary computes mean/median/cv of COUNTS (closed form)", {
  n <- c(100L, 120L, 80L)
  res <- pelsa_depth_summary(n)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1L)
  expect_setequal(colnames(res),
                  c("mean_n", "median_n", "cv_pct", "total_n_peptides"))
  expect_equal(res$mean_n, 100, tolerance = 1e-8)
  expect_equal(res$median_n, 100, tolerance = 1e-8)
  # cv = sample sd (ddof=1) / mean * 100 = 20 / 100 * 100 = 20
  expect_equal(res$cv_pct, stats::sd(n) / mean(n) * 100, tolerance = 1e-8)
  expect_equal(res$cv_pct, 20, tolerance = 1e-8)
  # not supplied -> NA_integer_
  expect_true(is.na(res$total_n_peptides))
})

test_that("pelsa_depth_summary carries total_n_peptides through when supplied", {
  n <- c(100L, 120L, 80L)
  res <- pelsa_depth_summary(n, total_n_peptides = 500L)
  expect_equal(res$total_n_peptides, 500L)
  # other stats unchanged
  expect_equal(res$mean_n, 100, tolerance = 1e-8)
})

test_that("pelsa_depth_summary coerces a double total_n_peptides to integer", {
  # Caller may pass a double (500) instead of 500L; the output column type
  # must be stable (integer) either way.
  res_dbl <- pelsa_depth_summary(c(100L, 120L, 80L), total_n_peptides = 500)
  expect_type(res_dbl$total_n_peptides, "integer")
  expect_equal(res_dbl$total_n_peptides, 500L)
})

test_that("pelsa_depth_summary: an NA element propagates to NA stats (no na.rm)", {
  # pelsa_peptides_per_sample() can never emit NA, so an NA here signals a
  # caller bug; we pin the propagate (not na.rm) behavior.
  res <- pelsa_depth_summary(c(100, NA, 80))
  expect_true(is.na(res$mean_n))
  expect_true(is.na(res$median_n))
  expect_true(is.na(res$cv_pct))
})

test_that("pelsa_depth_summary integrates with pelsa_peptides_per_sample output", {
  mat <- .depth_tiny_mat()
  n <- pelsa_peptides_per_sample(mat) # c(S1=2, S2=3, S3=2)
  res <- pelsa_depth_summary(n, total_n_peptides = nrow(mat))

  expect_equal(res$mean_n, mean(c(2, 3, 2)), tolerance = 1e-8)
  expect_equal(res$median_n, stats::median(c(2, 3, 2)), tolerance = 1e-8)
  expect_equal(res$cv_pct, stats::sd(c(2, 3, 2)) / mean(c(2, 3, 2)) * 100,
               tolerance = 1e-8)
  expect_equal(res$total_n_peptides, 5L)
})

test_that("pelsa_depth_summary: empty vector -> NA stats", {
  res <- pelsa_depth_summary(integer(0))
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1L)
  expect_true(is.na(res$mean_n))
  expect_true(is.na(res$median_n))
  expect_true(is.na(res$cv_pct))
  expect_true(is.na(res$total_n_peptides))
})

test_that("pelsa_depth_summary: single sample -> cv_pct NA (sd of one value)", {
  res <- pelsa_depth_summary(c(only = 42L))
  expect_equal(res$mean_n, 42, tolerance = 1e-8)
  expect_equal(res$median_n, 42, tolerance = 1e-8)
  expect_true(is.na(res$cv_pct)) # sample sd of a single value is NA
})

# --------------------------------------------------------------------------
# Smoke / shape test against the shared synthetic generator
# --------------------------------------------------------------------------

test_that("depth helpers run on synthetic frame with correct shape/names/types", {
  syn <- pelsa_make_synthetic(seed = 1)
  # Treat the intensity block as the processed-like matrix (mask still applies).
  mat <- as.matrix(syn$peptides[, syn$sample_cols])

  n <- pelsa_peptides_per_sample(mat)
  expect_type(n, "integer")
  expect_equal(length(n), length(syn$sample_cols))
  expect_equal(names(n), syn$sample_cols)
  # Counts are bounded by the number of peptide rows.
  expect_true(all(n >= 0L & n <= nrow(mat)))

  res <- pelsa_depth_summary(n, total_n_peptides = nrow(mat))
  expect_s3_class(res, "data.frame")
  expect_setequal(colnames(res),
                  c("mean_n", "median_n", "cv_pct", "total_n_peptides"))
  expect_equal(res$total_n_peptides, nrow(mat))
  expect_false(is.na(res$cv_pct)) # >1 sample -> finite CV
})
