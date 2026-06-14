################################################################################
# Module: PELSA volcano background thinning (Task 3B) - pure, no Shiny.
#
# The PELSA volcano can carry 100k+ points. plotly's toWebGL renders them, but
# the dense, uninformative non-significant cloud is the part that costs the most
# to draw and adds nothing the user can act on. This helper thins ONLY that
# background - never points the user might want to click.
#
# Thinnable set (a point is thinnable IFF ALL of):
#   1. NOT significant  (Significant == FALSE), AND
#   2. abs(logFC) <= logfc_thresh, AND
#   3. NOT a marker-protein peptide (is_marker == FALSE).
# EVERYTHING ELSE IS RETAINED, NEVER THINNED:
#   - every significant peptide (Significant == TRUE),
#   - every peptide with abs(logFC) > logfc_thresh (a sizeable effect is worth a
#     click even when non-significant),
#   - every marker-protein peptide (is_marker == TRUE).
#
# Density-PROPORTIONAL (not uniform): 2-D bin the thinnable points over
# (logFC, logP) into an n_bins x n_bins grid spanning the thinnable points' own
# range, then within EACH non-empty bin keep ceiling(keep_frac * n_bin_points)
# points sampled WITHOUT replacement. A fixed fraction per bin means dense bins
# stay dense and sparse bins stay sparse - the cloud's shape/spread is preserved.
# Contrast with uniform "keep every Nth row", which flattens relative density.
#
# Cost: the only per-group work is over BINS (<= n_bins^2 <= 2500), via split();
# there is no per-point loop, so ~100k thinnable points thin in well under a
# second.
#
# Consumes the Task 3A pelsa_build_volcano_df() output columns: Significant
# (logical), logFC (numeric), logP (numeric, -log10 P.Value), is_marker
# (logical). Phase 7 calls this before plotly + toWebGL and surfaces the returned
# counts as a "showing N of M background points" honesty note.
################################################################################

# Density-proportional thinning of the PELSA volcano background cloud.
#
# Thins ONLY the non-significant, small-effect, non-marker background, keeping a
# fixed FRACTION of points per 2-D (logFC, logP) bin so relative density is
# preserved. All non-thinnable rows pass through untouched.
#
# @param volcano_df data.frame with columns Significant (logical), logFC
#   (numeric), logP (numeric), is_marker (logical). Typically the 3A
#   pelsa_build_volcano_df() output.
# @param keep_frac fraction of each bin to keep, in (0, 1]. keep_frac >= 1 is a
#   no-op (everything kept, no thinning). Default 0.3. Because each non-empty bin
#   keeps ceiling(keep_frac * n) >= 1, a tiny keep_frac (e.g. 0.001) still keeps
#   at least one point per non-empty bin, so the global n_thinnable_kept can
#   exceed keep_frac * n_thinnable - this is intended, it preserves sparse
#   structure rather than erasing whole regions.
# @param logfc_thresh abs(logFC) threshold; points with abs(logFC) above it are
#   ALWAYS retained. Default 0.5.
# @param n_bins number of bins per axis (grid is n_bins x n_bins). Default 50.
# @param seed if non-NULL, set.seed(seed) for reproducible within-bin sampling;
#   if NULL the RNG is left untouched (the caller may seed for determinism).
# @return list(df, n_shown, n_total, n_thinnable, n_thinnable_kept) where df is
#   the retained rows (all non-thinnable rows + the kept thinnable sample) in
#   ORIGINAL row order, and n_shown = n_total - (n_thinnable - n_thinnable_kept).
#   A thinnable row whose logFC/logP coordinate is NA OR non-finite (Inf/-Inf,
#   e.g. logP from a P.Value of 0) cannot be binned and is RETAINED untouched.
# @noRd
pelsa_thin_background <- function(volcano_df, keep_frac = 0.3,
                                  logfc_thresh = 0.5, n_bins = 50,
                                  seed = NULL) {
  # ---- boundary validation (fail fast) --------------------------------------
  if (!is.data.frame(volcano_df)) {
    stop("pelsa_thin_background: volcano_df must be a data.frame")
  }
  required <- c("Significant", "logFC", "logP", "is_marker")
  missing_cols <- setdiff(required, names(volcano_df))
  if (length(missing_cols) > 0) {
    stop("pelsa_thin_background: volcano_df missing required column(s): ",
         paste(missing_cols, collapse = ", "))
  }
  if (length(keep_frac) != 1L || is.na(keep_frac) || !is.numeric(keep_frac) ||
      keep_frac <= 0) {
    stop("pelsa_thin_background: keep_frac must be a single number in (0, 1]")
  }
  if (length(logfc_thresh) != 1L || is.na(logfc_thresh) ||
      !is.numeric(logfc_thresh) || logfc_thresh < 0) {
    stop("pelsa_thin_background: logfc_thresh must be a single non-negative number")
  }
  if (length(n_bins) != 1L || is.na(n_bins) || !is.numeric(n_bins) ||
      n_bins < 1) {
    stop("pelsa_thin_background: n_bins must be a single positive integer")
  }
  n_bins <- as.integer(n_bins)

  n_total <- nrow(volcano_df)

  # ---- identify the thinnable set (the 3 ANDs) ------------------------------
  thinnable <- !volcano_df$Significant &
    !is.na(volcano_df$logFC) & abs(volcano_df$logFC) <= logfc_thresh &
    !volcano_df$is_marker
  # NA in Significant/is_marker must not silently become thinnable.
  thinnable[is.na(thinnable)] <- FALSE

  n_thinnable <- sum(thinnable)

  # No-op fast paths: nothing thinnable, or keep_frac keeps everything anyway.
  if (n_thinnable == 0L || keep_frac >= 1) {
    return(list(
      df               = volcano_df,
      n_shown          = n_total,
      n_total          = n_total,
      n_thinnable      = n_thinnable,
      n_thinnable_kept = n_thinnable
    ))
  }

  if (!is.null(seed)) set.seed(seed)

  # Row indices of the thinnable cloud, then split off rows whose coords cannot
  # be binned - NA OR non-finite (Inf/-Inf, e.g. logP = -log10(0) when an
  # upstream permutation/underflow P.Value is 0). is.finite() is FALSE for both
  # NA and +/-Inf, so it covers both cases. Those rows are RETAINED untouched.
  thin_idx <- which(thinnable)
  fc <- volcano_df$logFC[thin_idx]
  lp <- volcano_df$logP[thin_idx]
  binnable_mask <- is.finite(fc) & is.finite(lp)
  binnable_idx <- thin_idx[binnable_mask]
  na_coord_idx <- thin_idx[!binnable_mask]  # retained untouched (NA/Inf coords)

  kept_thin_idx <- na_coord_idx  # start with the un-binnable thinnable rows

  if (length(binnable_idx) > 0L) {
    fc_b <- fc[binnable_mask]
    lp_b <- lp[binnable_mask]

    # 2-D bin over the thinnable points' OWN range. findInterval() with n_bins-1
    # interior breakpoints yields bin ids in 0..(n_bins-1); a degenerate
    # (zero-width) range collapses to a single bin, which is fine.
    bin_axis <- function(x, n_bins) {
      rng <- range(x)
      if (rng[1L] == rng[2L]) return(rep.int(0L, length(x)))
      breaks <- seq(rng[1L], rng[2L], length.out = n_bins + 1L)
      # interior breaks only; rightmost point lands in the last bin.
      findInterval(x, breaks[-c(1L, length(breaks))], rightmost.closed = TRUE)
    }
    bx <- bin_axis(fc_b, n_bins)
    by <- bin_axis(lp_b, n_bins)
    bin_id <- bx * n_bins + by  # unique per (bx, by) cell

    # Per-BIN sampling via split() - split() itself is O(n) but cheap; the only
    # GROUP work is over bins (<= n_bins^2 <= 2500), never a per-point loop.
    by_bin <- split(binnable_idx, bin_id)
    kept_per_bin <- lapply(by_bin, function(rows) {
      n <- length(rows)
      k <- ceiling(keep_frac * n)  # singleton bins keep >= 1, never vanish
      if (k >= n) return(rows)
      rows[sample.int(n, k)]
    })
    kept_thin_idx <- c(kept_thin_idx, unlist(kept_per_bin, use.names = FALSE))
  }

  n_thinnable_kept <- length(kept_thin_idx)

  # Retained rows = all non-thinnable rows + the kept thinnable rows, restored to
  # ORIGINAL row order so downstream tooltips/ordering are stable.
  keep_idx <- sort(c(which(!thinnable), kept_thin_idx))
  out_df <- volcano_df[keep_idx, , drop = FALSE]
  rownames(out_df) <- NULL

  list(
    df               = out_df,
    n_shown          = nrow(out_df),
    n_total          = n_total,
    n_thinnable      = n_thinnable,
    n_thinnable_kept = n_thinnable_kept
  )
}
