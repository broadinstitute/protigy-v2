################################################################################
# Module: PELSA per-sample quantified-peptide depth helpers
#
# Pure (non-reactive) helpers feeding the Summary per-sample DEPTH bar plot (one
# bar per sample = number of quantified peptides) plus a companion table
# (mean / median / CV of those per-sample COUNTS).
#
# SOURCE matrix = the PROCESSED GCT (log2) matrix (GCTs_and_params()$GCTs[[ome]]),
# NOT the raw uploaded intensities. This DIFFERS from the within-condition CV
# helper (R/tab_pelsa_cv_helpers.R), which operates on RAW linear intensities.
#
# QUANTIFIED MASK -- the canonical pelsa_quantified_mask (finite AND non-zero,
# `is.finite(x) & x != 0`). This DEPARTS from the notebook's literal `> 0`:
# the notebook ran that mask on RAW LINEAR intensities (where `> 0` correctly
# means "detected"), but Protigy applies it to the PROCESSED matrix, which is
# log-transformed and median-normalized. On a log/median-centered matrix a real
# low-abundance peptide has a NEGATIVE value and median-centering pushes ~half
# of all finite values <= 0, so `> 0` silently dropped up to ~50% of genuine
# measurements. `!= 0` keeps negatives (real, low-abundance) while still
# excluding NA (upstream maps raw 0 -> NA before logging) and exact-zero; on a
# LINEAR matrix (never negative) it is identical to the old `> 0`.
#
# CV DISTINCTION: pelsa_depth_summary()'s cv_pct is the PLAIN linear CV of the
# per-sample COUNTS (sample sd / mean * 100, ddof = 1) -- it is the CV of the
# COUNT VECTOR, NOT the CV of intensities. This is consistent with the single CV
# definition used across PELSA (sd / mean * 100); it just happens to be applied
# to the integer depth counts here.
#
# VECTORIZED ONLY. PELSA matrices are 100k+ rows. The per-sample count is a
# single colSums() over a logical matrix -- there is NO per-row/per-column
# apply() loop. Summary stats are plain base-R aggregates over the count vector.
#
# ORDERING is the CALLER's job: bars/table are ordered by the user's sample_order
# in Phase 6. These helpers return counts keyed by column name in matrix-column
# order and do not re-order. Keep free of Shiny reactivity for unit-testability.
################################################################################

# Presence/absence mask for a PELSA intensity matrix: TRUE where a value is a
# GENUINE measurement. A value is "quantified" iff it is finite AND non-zero.
#
# This is the single, canonical definition of "quantified" shared by per-sample
# depth (pelsa_peptides_per_sample), fully-quantified counts, and per-condition
# membership, so the three never drift apart.
#
# Why `!= 0` and not the notebook's `> 0`:
#   - LINEAR matrix (log_transformation = "None"): raw intensities are never
#     negative; a literal 0 means "not detected" -> excluded. So `!= 0` is
#     IDENTICAL to the old `> 0` here -- no behavior change on linear data.
#   - LOG-transformed / normalized matrix: a real low-abundance peptide has a
#     NEGATIVE log value (raw intensity < 1), and median-centering pushes ~half
#     of all finite values <= 0 BY CONSTRUCTION. The old `> 0` silently dropped
#     those genuine measurements (under-counting depth / fully-quantified /
#     membership by up to ~50%). `!= 0` keeps them. Upstream maps raw 0 -> NA
#     before logging, so NA already encodes absence; the only value `!= 0`
#     excludes on log data is an exact log == 0 (raw intensity exactly 1), a
#     negligible measure-zero edge.
# NA / NaN / Inf / -Inf -> FALSE (not finite). 0 -> FALSE (absent).
# @noRd
pelsa_quantified_mask <- function(mat) {
  is.finite(mat) & mat != 0
}

# Count, per sample column, how many peptides are "quantified" in a PROCESSED
# matrix, using the canonical pelsa_quantified_mask (finite & non-zero).
#
# Vectorized: colSums() over the logical mask matrix -- one pass, no apply loop.
#
# @param processed_mat numeric matrix (rows = peptides, cols = samples) of
#                       PROCESSED values, OR a data.frame coerced to matrix
#                       (documented). Must have UNIQUE column (sample) names
#                       (duplicates make the named-integer return ambiguous for
#                       downstream counts[name] selection).
# @return NAMED integer vector; names = sample (column) names in column order,
#         values = number of quantified peptides per sample.
# @noRd
pelsa_peptides_per_sample <- function(processed_mat) {
  # Coerce a data.frame to a numeric matrix (documented).
  if (is.data.frame(processed_mat)) processed_mat <- as.matrix(processed_mat)

  stopifnot(
    "processed_mat must be a matrix" = is.matrix(processed_mat),
    "processed_mat must be numeric" = is.numeric(processed_mat),
    "processed_mat must have column (sample) names" =
      !is.null(colnames(processed_mat)),
    "processed_mat must have unique column (sample) names" =
      !anyDuplicated(colnames(processed_mat))
  )

  counts <- colSums(pelsa_quantified_mask(processed_mat))
  storage.mode(counts) <- "integer"
  counts
}

# Companion summary statistics over the per-sample quantified-count vector.
#
# cv_pct is the PLAIN linear CV of the COUNTS (sample sd ddof=1 / mean * 100) --
# the CV of the count vector, NOT of intensities (see banner). Edge cases:
#   - empty vector       -> all stats NA (mean/median/cv).
#   - single sample      -> cv_pct NA (sample sd of one value is undefined/NA).
#   - mean count == 0    -> cv_pct NA (avoid Inf/NaN; non-finite -> NA).
#   - NA elements        -> propagate to NA mean_n/median_n/cv_pct (no na.rm).
#                           The producer pelsa_peptides_per_sample() never
#                           yields NA (colSums over a logical mask is always a
#                           finite count), so an NA here signals a CALLER BUG;
#                           we propagate rather than silently masking it.
#
# @param n_quantified     named integer vector from pelsa_peptides_per_sample()
#                          (or any numeric vector).
# @param total_n_peptides optional total peptide count carried through (the
#                          notebook sets total_n_peptides = nrow(data_df), i.e.
#                          ALL GCT rows). Defaults to NA_integer_ when not given.
# @return one-row data.frame with columns mean_n, median_n, cv_pct,
#         total_n_peptides (export-friendly).
# @noRd
pelsa_depth_summary <- function(n_quantified, total_n_peptides = NULL) {
  stopifnot(
    "n_quantified must be numeric" =
      is.numeric(n_quantified) || length(n_quantified) == 0L,
    "total_n_peptides must be NULL or a single value" =
      is.null(total_n_peptides) || length(total_n_peptides) == 1L
  )

  # Coerce supplied total to integer so the output column type is stable
  # whether the caller passes 500 or 500L.
  total <- if (is.null(total_n_peptides)) {
    NA_integer_
  } else {
    as.integer(total_n_peptides)
  }

  if (length(n_quantified) == 0L) {
    return(data.frame(
      mean_n = NA_real_,
      median_n = NA_real_,
      cv_pct = NA_real_,
      total_n_peptides = total,
      stringsAsFactors = FALSE
    ))
  }

  mean_n <- mean(n_quantified)
  median_n <- stats::median(n_quantified)
  # Sample sd (ddof = 1); NA for a single element. Guard non-finite cv -> NA.
  sd_n <- stats::sd(n_quantified)
  cv_pct <- sd_n / mean_n * 100
  if (!is.finite(cv_pct)) cv_pct <- NA_real_

  data.frame(
    mean_n = mean_n,
    median_n = median_n,
    cv_pct = cv_pct,
    total_n_peptides = total,
    stringsAsFactors = FALSE
  )
}
