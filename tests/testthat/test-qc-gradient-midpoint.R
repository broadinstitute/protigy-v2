################################################################################
# Tests for continuous-color gradient midpoint bug
#
# Bug: mean(min(x), max(x)) in R treats the second positional argument as
# `trim=`, NOT as data.  So mean(1, 9) == 1 (not 5).  The midpoint must be
# computed as mean(c(min(x), max(x))) or (min(x) + max(x)) / 2.
#
# Affected helpers (pre-fix):
#   R/tab_qc_boxplots_helpers.R      line ~85
#   R/tab_qc_profile_plots_helpers.R line ~71
#   R/tab_qc_PCA_helpers.R           line ~205
################################################################################

# --------------------------------------------------------------------------- #
# Shared helpers                                                               #
# --------------------------------------------------------------------------- #

# Continuous annotation values spanning 0..10 (min=0, max=10, true midpoint=5)
CONT_VALS  <- c(0, 2, 5, 8, 10)
N_SAMP     <- length(CONT_VALS)
N_FEAT     <- 5L

make_cont_gct <- function() {
  mat <- matrix(
    seq_len(N_FEAT * N_SAMP) + 0.0,
    nrow = N_FEAT,
    dimnames = list(
      paste0("feat_", seq_len(N_FEAT)),
      paste0("s", seq_len(N_SAMP))
    )
  )
  cdesc <- data.frame(
    cont_col = CONT_VALS,
    row.names = paste0("s", seq_len(N_SAMP)),
    stringsAsFactors = FALSE
  )
  rdesc <- data.frame(
    id = paste0("feat_", seq_len(N_FEAT)),
    row.names = paste0("feat_", seq_len(N_FEAT)),
    stringsAsFactors = FALSE
  )
  new("GCT",
      mat    = mat,
      cdesc  = cdesc,
      rdesc  = rdesc,
      rid    = paste0("feat_", seq_len(N_FEAT)),
      cid    = paste0("s", seq_len(N_SAMP))
  )
}

make_cont_color_map <- function() {
  list(
    is_discrete = FALSE,
    colors      = list(low = "blue", mid = "white", high = "red", na_color = "gray50"),
    vals        = c("low", "mid", "high", "na_color")
  )
}

make_params <- function(norm = "Median") {
  list(data_normalization = norm, max_missing = 50, data_filter = "None")
}

# Extract the midpoint from a scale_colour_gradient2 scale added to a ggplot.
# The midpoint is stored inside the rescaler closure chain:
#   environment(environment(scale$rescaler)$rescaler)$mid
# Returns NA_real_ when no gradient2 scale is found.
extract_gradient2_midpoint <- function(p) {
  for (sc in p$scales$scales) {
    if (!inherits(sc, "ScaleContinuous")) next
    if (is.null(sc$rescaler))             next
    inner_fn <- tryCatch(
      get("rescaler", envir = environment(sc$rescaler), inherits = FALSE),
      error = function(e) NULL
    )
    if (is.null(inner_fn)) next
    mid_val <- tryCatch(
      environment(inner_fn)$mid,
      error = function(e) NULL
    )
    if (!is.null(mid_val)) return(as.numeric(mid_val))
  }
  NA_real_
}

TRUE_MIDPOINT  <- (min(CONT_VALS) + max(CONT_VALS)) / 2  # 5
WRONG_MIDPOINT <- min(CONT_VALS)                           # 0  (the bug)

# --------------------------------------------------------------------------- #
# create_boxplot (R/tab_qc_boxplots_helpers.R)                                #
# --------------------------------------------------------------------------- #

test_that("create_boxplot: gradient midpoint equals (min+max)/2, not min", {
  gct    <- make_cont_gct()
  cmap   <- make_cont_color_map()
  params <- make_params()

  p <- create_boxplot(gct, "cont_col", "test_ome", cmap, params, "org")
  expect_s3_class(p, "ggplot")

  mid <- extract_gradient2_midpoint(p)
  expect_false(is.na(mid),
               label = "gradient2 midpoint should be extractable from boxplot scale")

  # Must NOT equal the min value (which is what the buggy code produces)
  expect_false(
    isTRUE(all.equal(mid, WRONG_MIDPOINT, tolerance = 1e-9)),
    label = paste0("midpoint must not equal min (", WRONG_MIDPOINT, ") -- that is the bug")
  )

  # Must equal the true (min+max)/2
  expect_equal(mid, TRUE_MIDPOINT, tolerance = 1e-9,
               label = "boxplot gradient midpoint == (min+max)/2")
})

# --------------------------------------------------------------------------- #
# create_profile_plot (R/tab_qc_profile_plots_helpers.R)                      #
# --------------------------------------------------------------------------- #

test_that("create_profile_plot: gradient midpoint equals (min+max)/2, not min", {
  gct    <- make_cont_gct()
  cmap   <- make_cont_color_map()
  params <- make_params()

  p <- create_profile_plot(gct, "cont_col", "test_ome", cmap, params, "org")
  expect_s3_class(p, "ggplot")

  mid <- extract_gradient2_midpoint(p)
  expect_false(is.na(mid),
               label = "gradient2 midpoint should be extractable from profile plot scale")

  expect_false(
    isTRUE(all.equal(mid, WRONG_MIDPOINT, tolerance = 1e-9)),
    label = paste0("midpoint must not equal min (", WRONG_MIDPOINT, ") -- that is the bug")
  )

  expect_equal(mid, TRUE_MIDPOINT, tolerance = 1e-9,
               label = "profile plot gradient midpoint == (min+max)/2")
})

# --------------------------------------------------------------------------- #
# create_PCA_plot (R/tab_qc_PCA_helpers.R)                                    #
# --------------------------------------------------------------------------- #

test_that("create_PCA_plot: gradient midpoint equals (min+max)/2, not min", {
  gct  <- make_cont_gct()
  cmap <- make_cont_color_map()

  # Add small noise so features have distinct variance (required for PCA)
  set.seed(42)
  gct@mat <- gct@mat + matrix(rnorm(N_FEAT * N_SAMP, sd = 0.01), N_FEAT, N_SAMP)

  p <- create_PCA_plot(gct, "cont_col", "test_ome",
                       custom_color_map = cmap,
                       comp.x = 1L, comp.y = 2L)
  expect_s3_class(p, "ggplot")

  mid <- extract_gradient2_midpoint(p)
  expect_false(is.na(mid),
               label = "gradient2 midpoint should be extractable from PCA plot scale")

  expect_false(
    isTRUE(all.equal(mid, WRONG_MIDPOINT, tolerance = 1e-9)),
    label = paste0("midpoint must not equal min (", WRONG_MIDPOINT, ") -- that is the bug")
  )

  expect_equal(mid, TRUE_MIDPOINT, tolerance = 1e-9,
               label = "PCA plot gradient midpoint == (min+max)/2")
})

# --------------------------------------------------------------------------- #
# Regression: non-zero-based range (min=3, max=11, midpoint=7)                #
# --------------------------------------------------------------------------- #

test_that("create_boxplot: gradient midpoint correct when range does not start at zero", {
  gct           <- make_cont_gct()
  gct@cdesc$cont_col <- c(3, 5, 7, 9, 11)   # min=3, max=11, midpoint=7
  cmap          <- make_cont_color_map()
  params        <- make_params()

  p   <- create_boxplot(gct, "cont_col", "test_ome", cmap, params, "org")
  mid <- extract_gradient2_midpoint(p)
  expect_equal(mid, 7, tolerance = 1e-9,
               label = "non-zero range: gradient midpoint should be 7 not 3")
})

# --------------------------------------------------------------------------- #
# create_corr_boxplot (R/tab_qc_correlation_helpers.R)                        #
# --------------------------------------------------------------------------- #
# NOTE: create_corr_boxplot requires at least one group to have >= 2 samples.
# We use cont values c(0, 0, 10, 10) so that groups "0" and "10" each have
# 2 samples -- min=0, max=10, true midpoint=5, buggy value=0.

make_corr_cont_gct <- function() {
  # 4 samples, 6 features; cont_col has two repeated values (0 and 10)
  n_feat  <- 6L
  n_samp  <- 4L
  mat <- matrix(
    seq_len(n_feat * n_samp) + 0.0,
    nrow = n_feat,
    dimnames = list(
      paste0("f_", seq_len(n_feat)),
      paste0("s_", seq_len(n_samp))
    )
  )
  # small noise so columns are not perfectly collinear
  set.seed(7L)
  mat <- mat + matrix(rnorm(n_feat * n_samp, sd = 0.01), n_feat, n_samp)
  cdesc <- data.frame(
    cont_col = c(0, 0, 10, 10),
    row.names = paste0("s_", seq_len(n_samp)),
    stringsAsFactors = FALSE
  )
  rdesc <- data.frame(
    id = paste0("f_", seq_len(n_feat)),
    row.names = paste0("f_", seq_len(n_feat)),
    stringsAsFactors = FALSE
  )
  new("GCT",
      mat   = mat,
      cdesc = cdesc,
      rdesc = rdesc,
      rid   = paste0("f_", seq_len(n_feat)),
      cid   = paste0("s_", seq_len(n_samp))
  )
}

test_that("create_corr_boxplot: continuous color map uses default discrete fill (no gradient2)", {
  # The intra-group correlation boxplot maps fill to the group factor, so it
  # cannot carry a continuous gradient (that errored at build time). A
  # continuous-coded annotation now falls back to the default discrete fill:
  # no scale_fill_gradient2 is attached.
  gct  <- make_corr_cont_gct()
  cmap <- make_cont_color_map()
  cor_mat <- cor(gct@mat, use = "pairwise.complete.obs", method = "pearson")

  p <- create_corr_boxplot(gct, "cont_col", "test_corr_ome", cmap, "pearson", cor_mat)
  expect_s3_class(p, "ggplot")
  # No continuous gradient scale is present (would not render on a discrete fill).
  expect_true(is.na(extract_gradient2_midpoint(p)))
})

# --------------------------------------------------------------------------- #
# summary_quant_features (R/tab_summary_helpers.R)                            #
# --------------------------------------------------------------------------- #
# This helper coerces the annotation to a factor and uses its integer codes as
# the continuous gradient value (as.numeric on a factor -> 1..n). With 5 distinct
# values the codes are 1..5, so the true midpoint is 3 and the buggy value is 1.

test_that("summary_quant_features: gradient midpoint equals (min+max)/2, not min", {
  gct  <- make_cont_gct()                  # 5 distinct cont values -> codes 1..5
  cmap <- make_cont_color_map()

  p <- summary_quant_features(gct, "cont_col", "test_ome", cmap)
  expect_s3_class(p, "ggplot")

  mid <- extract_gradient2_midpoint(p)
  expect_false(is.na(mid),
               label = "gradient2 midpoint should be extractable from summary plot scale")
  # factor codes 1..5 -> wrong (min) = 1, true (min+max)/2 = 3
  expect_false(isTRUE(all.equal(mid, 1, tolerance = 1e-9)),
               label = "midpoint must not equal min (1) -- that is the bug")
  expect_equal(mid, 3, tolerance = 1e-9,
               label = "summary plot gradient midpoint == (min+max)/2 of factor codes")
})

# --------------------------------------------------------------------------- #
# create_corr_boxplot: continuous color map must still RENDER                 #
# --------------------------------------------------------------------------- #
# The intra-group correlation boxplot maps fill to the group factor (ind), so a
# continuous scale_fill_gradient2 cannot apply to it: ggplot_build() errors with
# "Discrete value supplied to a continuous scale." A continuous-coded annotation
# must fall back to the default discrete fill so the plot renders.

test_that("create_corr_boxplot with a continuous color map still builds (no discrete/continuous clash)", {
  gct  <- make_corr_cont_gct()
  cmap <- make_cont_color_map()              # is_discrete = FALSE
  cor_mat <- cor(gct@mat, use = "pairwise.complete.obs", method = "pearson")

  p <- create_corr_boxplot(gct, "cont_col", "test_corr_ome", cmap, "pearson", cor_mat)
  expect_s3_class(p, "ggplot")
  # The actual regression: rendering must not throw.
  expect_no_error(ggplot2::ggplot_build(p))
})
