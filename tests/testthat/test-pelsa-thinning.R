################################################################################
# Tests for pelsa_thin_background() — density-proportional volcano background
# thinning (Task 3B). Pure, no Shiny.
#
# The PELSA volcano can carry 100k+ points. We thin ONLY the uninformative
# background cloud — points that are ALL of: non-significant, |logFC| <= thresh,
# and NOT a marker-protein peptide. Everything else (significant peptides,
# sizeable-effect peptides with |logFC| > thresh, marker peptides) is NEVER
# thinned. Thinning is DENSITY-PROPORTIONAL (a fixed fraction kept per 2-D bin),
# so dense regions stay dense and sparse regions stay sparse — NOT a flat
# uniform decimation that would wash the cloud's shape out.
################################################################################

# ---- helpers -----------------------------------------------------------------

# Build a volcano-like data.frame carrying exactly the columns the thinner reads
# (Significant / logFC / logP / is_marker), plus an `id` so we can track which
# specific rows survive across calls.
make_volcano <- function(logFC, logP, Significant, is_marker, id = NULL) {
  n <- length(logFC)
  if (is.null(id)) id <- seq_len(n)
  data.frame(
    id          = id,
    logFC       = as.numeric(logFC),
    logP        = as.numeric(logP),
    Significant = as.logical(Significant),
    is_marker   = as.logical(is_marker),
    stringsAsFactors = FALSE
  )
}

# A mixed frame: some significant, some big-effect non-sig, some markers, and a
# pile of true background (non-sig, small |logFC|, non-marker).
mixed_volcano <- function() {
  bg <- make_volcano(
    logFC       = runif(200, -0.4, 0.4),
    logP        = runif(200, 0, 1),
    Significant = FALSE,
    is_marker   = FALSE,
    id          = paste0("bg", seq_len(200))
  )
  sig <- make_volcano(
    logFC = c(2, -2, 1.5), logP = c(5, 6, 4),
    Significant = TRUE, is_marker = FALSE,
    id = c("sig1", "sig2", "sig3")
  )
  bigeff <- make_volcano(  # non-sig but |logFC| > thresh — must be retained
    logFC = c(0.9, -1.2), logP = c(0.3, 0.4),
    Significant = FALSE, is_marker = FALSE,
    id = c("big1", "big2")
  )
  mk <- make_volcano(  # marker peptide, small effect, non-sig — must be retained
    logFC = c(0.1, -0.2), logP = c(0.2, 0.5),
    Significant = FALSE, is_marker = TRUE,
    id = c("mk1", "mk2")
  )
  rbind(bg, sig, bigeff, mk)
}

# ---- thinnable-set logic -----------------------------------------------------

test_that("significant / big-effect / marker rows are NEVER thinned", {
  df <- mixed_volcano()
  out <- pelsa_thin_background(df, keep_frac = 0.3, logfc_thresh = 0.5,
                               n_bins = 10, seed = 1)
  kept_ids <- out$df$id

  # every significant peptide retained
  expect_true(all(c("sig1", "sig2", "sig3") %in% kept_ids))
  # every |logFC| > thresh peptide retained (even non-sig)
  expect_true(all(c("big1", "big2") %in% kept_ids))
  # every marker peptide retained
  expect_true(all(c("mk1", "mk2") %in% kept_ids))
})

test_that("only non-sig & |logFC|<=thresh & non-marker rows are thinnable", {
  df <- mixed_volcano()
  out <- pelsa_thin_background(df, keep_frac = 0.3, logfc_thresh = 0.5,
                               n_bins = 10, seed = 1)
  # 200 background rows are the only thinnable set; with keep_frac 0.3 some
  # must be dropped, and the dropped rows must all come from background.
  dropped_ids <- setdiff(df$id, out$df$id)
  expect_true(length(dropped_ids) > 0)
  expect_true(all(grepl("^bg", dropped_ids)))
  expect_equal(out$n_thinnable, 200L)
})

# ---- density-proportional (THE defining test) -------------------------------

test_that("thinning is density-proportional: dense bins keep proportionally more", {
  # One DENSE cell: 1000 thinnable points at EXACTLY (0.1, 0.1).
  # One SPARSE cell: 10 thinnable points at EXACTLY (-0.4, 0.9).
  # Identical coordinates put each cluster in a SINGLE (logFC, logP) bin, so the
  # per-bin ceiling(keep_frac * n) is exact and the proportionality is crisp.
  dense <- make_volcano(
    logFC = rep(0.10, 1000), logP = rep(0.10, 1000),
    Significant = FALSE, is_marker = FALSE, id = paste0("d", seq_len(1000))
  )
  sparse <- make_volcano(
    logFC = rep(-0.40, 10), logP = rep(0.90, 10),
    Significant = FALSE, is_marker = FALSE, id = paste0("s", seq_len(10))
  )
  df <- rbind(dense, sparse)

  out <- pelsa_thin_background(df, keep_frac = 0.3, logfc_thresh = 0.5,
                               n_bins = 50, seed = 7)
  kept <- out$df$id
  n_dense_kept  <- sum(grepl("^d", kept))
  n_sparse_kept <- sum(grepl("^s", kept))

  # dense keeps ceiling(0.3 * 1000) = 300, sparse keeps ceiling(0.3 * 10) = 3
  expect_equal(n_dense_kept, 300L)
  expect_equal(n_sparse_kept, 3L)

  # PROPORTIONALITY: retained-count ratio (100:1) MIRRORS the original-count
  # ratio, NOT flattened toward 1:1 as a uniform sample would. This is the
  # defining property — dense stays dense, sparse stays sparse.
  ratio <- n_dense_kept / n_sparse_kept
  expect_true(ratio > 80 && ratio < 120)
})

# ---- counts ------------------------------------------------------------------

test_that("counts are correct and internally consistent", {
  df <- mixed_volcano()
  out <- pelsa_thin_background(df, keep_frac = 0.3, logfc_thresh = 0.5,
                               n_bins = 10, seed = 1)
  expect_equal(out$n_total, nrow(df))
  expect_equal(out$n_shown, nrow(out$df))
  expect_equal(out$n_thinnable, 200L)
  # consistency: shown = total - (thinnable - thinnable_kept)
  expect_equal(out$n_shown,
               out$n_total - (out$n_thinnable - out$n_thinnable_kept))
  expect_true(out$n_thinnable_kept < out$n_thinnable)  # actually thinned
})

# ---- keep_frac = 1 is a no-op ------------------------------------------------

test_that("keep_frac = 1 keeps everything (df identical to input)", {
  df <- mixed_volcano()
  out <- pelsa_thin_background(df, keep_frac = 1, logfc_thresh = 0.5,
                               n_bins = 10, seed = 1)
  expect_equal(out$df, df)
  expect_equal(out$n_shown, out$n_total)
  expect_equal(out$n_thinnable_kept, out$n_thinnable)
})

test_that("keep_frac >= 1 also keeps everything", {
  df <- mixed_volcano()
  out <- pelsa_thin_background(df, keep_frac = 2, logfc_thresh = 0.5,
                               n_bins = 10, seed = 1)
  expect_equal(out$df, df)
})

# ---- determinism -------------------------------------------------------------

test_that("a fixed seed yields identical kept rows across calls", {
  df <- mixed_volcano()
  a <- pelsa_thin_background(df, keep_frac = 0.3, n_bins = 10, seed = 42)
  b <- pelsa_thin_background(df, keep_frac = 0.3, n_bins = 10, seed = 42)
  expect_identical(a$df, b$df)
  expect_identical(a$df$id, b$df$id)
})

# ---- edge cases --------------------------------------------------------------

test_that("empty thinnable set (all significant) leaves df unchanged", {
  df <- make_volcano(
    logFC = c(2, -2, 1), logP = c(5, 6, 4),
    Significant = TRUE, is_marker = FALSE, id = c("a", "b", "c")
  )
  out <- pelsa_thin_background(df, keep_frac = 0.3, n_bins = 10, seed = 1)
  expect_equal(out$df, df)
  expect_equal(out$n_thinnable, 0L)
  expect_equal(out$n_thinnable_kept, 0L)
  expect_equal(out$n_shown, out$n_total)
})

test_that("all-thinnable frame thins down to the per-bin proportion", {
  df <- make_volcano(
    logFC = runif(100, -0.3, 0.3), logP = runif(100, 0, 1),
    Significant = FALSE, is_marker = FALSE
  )
  out <- pelsa_thin_background(df, keep_frac = 0.5, n_bins = 5, seed = 3)
  expect_equal(out$n_thinnable, 100L)
  expect_true(out$n_thinnable_kept < 100L)
  expect_true(out$n_thinnable_kept >= 50L)  # ceiling rounding never drops below frac
})

test_that("a singleton bin always survives (ceiling keeps >= 1)", {
  # One lone thinnable point far from any other -> its own bin -> survives.
  lone <- make_volcano(0.0, 0.0, FALSE, FALSE, id = "lone")
  cloud <- make_volcano(
    logFC = rnorm(500, 0.4, 0.01), logP = rnorm(500, 0.9, 0.01),
    Significant = FALSE, is_marker = FALSE, id = paste0("c", seq_len(500))
  )
  df <- rbind(lone, cloud)
  out <- pelsa_thin_background(df, keep_frac = 0.1, n_bins = 50, seed = 5)
  expect_true("lone" %in% out$df$id)
})

test_that("a thinnable row with NA coords is retained untouched", {
  df <- make_volcano(
    logFC = c(NA, 0.1, 0.2), logP = c(0.5, NA, 0.3),
    Significant = FALSE, is_marker = FALSE, id = c("na1", "na2", "ok")
  )
  out <- pelsa_thin_background(df, keep_frac = 0.0001, n_bins = 50, seed = 1)
  # NA-coord rows can't be binned -> retained; only "ok" was binnable.
  expect_true(all(c("na1", "na2") %in% out$df$id))
})

test_that("a thinnable row with non-finite coords does not crash and is retained", {
  # logP = Inf is reachable: 3A builds logP = -log10(P.Value), so a P.Value of 0
  # (permutation p-values / numeric underflow) yields logP = Inf. range() on it
  # would make seq(length.out=) throw — the binner must fold non-finite into the
  # "can't be binned" set and RETAIN the row untouched. logFC = -Inf likewise.
  # inf_lp: thinnable (small |logFC|) but logP = Inf -> can't be binned, retained.
  # neginf_fc: logFC = -Inf, so logP is irrelevant; with logP = 0.3 it is a true
  # background coord-wise, but a -Inf logFC's |logFC| > thresh path would mark it
  # big-effect. Use a finite small logFC here so it is genuinely thinnable and
  # the -Inf is in the COORDINATE we test (logP).
  df <- make_volcano(
    logFC = c(0.1, 0.15, -Inf, 0.2, 0.0),
    logP  = c(Inf, -Inf,  0.2, 0.4, 0.1),
    Significant = FALSE, is_marker = FALSE,
    id = c("inf_lp", "neginf_lp", "neginf_fc", "ok1", "ok2")
  )
  out <- expect_no_error(
    pelsa_thin_background(df, keep_frac = 0.0001, n_bins = 50, seed = 1)
  )
  # non-finite-coord rows are retained untouched: inf_lp / neginf_lp are
  # thinnable-but-unbinnable (retained via the coord path); neginf_fc has
  # |logFC| > thresh so it is a big-effect row retained via the non-thinnable
  # path — either way it must NOT crash and MUST survive.
  expect_true(all(c("inf_lp", "neginf_lp", "neginf_fc") %in% out$df$id))
  # 4 thinnable (the two finite-coord oks + the two with a non-finite COORD but
  # small |logFC|); neginf_fc is big-effect, not thinnable.
  expect_equal(out$n_total, 5L)
  expect_equal(out$n_thinnable, 4L)
  expect_equal(out$n_shown,
               out$n_total - (out$n_thinnable - out$n_thinnable_kept))
})

test_that("tiny keep_frac keeps at least one point per non-empty bin", {
  # Spread points across many distinct bins; with keep_frac well below 1/n per
  # bin, the ceiling(keep_frac*n) >= 1 floor means each non-empty bin keeps >= 1,
  # so n_thinnable_kept >= number of non-empty bins (sparse structure preserved).
  set.seed(11)
  df <- make_volcano(
    logFC = runif(800, -0.4, 0.4), logP = runif(800, 0, 5),
    Significant = FALSE, is_marker = FALSE
  )
  n_bins <- 20
  out <- pelsa_thin_background(df, keep_frac = 0.001, n_bins = n_bins, seed = 2)
  # recompute the non-empty bin count the same way the helper bins
  bin_axis <- function(x, nb) {
    rng <- range(x); if (rng[1] == rng[2]) return(rep(0L, length(x)))
    br <- seq(rng[1], rng[2], length.out = nb + 1)
    findInterval(x, br[-c(1, length(br))], rightmost.closed = TRUE)
  }
  n_nonempty <- length(unique(bin_axis(df$logFC, n_bins) * n_bins +
                                bin_axis(df$logP, n_bins)))
  expect_gte(out$n_thinnable_kept, n_nonempty)
})

test_that("a data.table volcano_df round-trips with all columns preserved", {
  skip_if_not_installed("data.table")
  df <- mixed_volcano()
  dt <- data.table::as.data.table(df)
  out <- pelsa_thin_background(dt, keep_frac = 0.3, n_bins = 10, seed = 1)
  # every input column survives the round-trip
  expect_true(all(names(df) %in% names(out$df)))
  # the returned df is usable: counts consistent and non-thinnable rows kept
  expect_equal(out$n_shown,
               out$n_total - (out$n_thinnable - out$n_thinnable_kept))
  expect_true(all(c("sig1", "sig2", "sig3", "big1", "big2", "mk1", "mk2") %in%
                    out$df$id))
})

# ---- boundary validation -----------------------------------------------------

test_that("missing required column errors", {
  df <- make_volcano(0.1, 0.2, FALSE, FALSE)
  df$is_marker <- NULL
  expect_error(pelsa_thin_background(df), "is_marker")
})

test_that("keep_frac out of (0,1] range errors", {
  df <- mixed_volcano()
  expect_error(pelsa_thin_background(df, keep_frac = 0), "keep_frac")
  expect_error(pelsa_thin_background(df, keep_frac = -0.5), "keep_frac")
  expect_error(pelsa_thin_background(df, keep_frac = NA), "keep_frac")
})

test_that("non-data.frame input errors", {
  expect_error(pelsa_thin_background(list(a = 1)), "data.frame")
})
