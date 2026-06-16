################################################################################
# Module: PELSA Summary (Section 2) - pure plot-data / shaping helpers (Phase 6).
#
# The Summary section is a DASHBOARD that READS the per-dataset analysis cache
# built by Setup's Start-Analysis (5D) and renders metrics + plots. It NEVER
# recomputes the heavy objects in render. These pure helpers do the small,
# closed-form shaping the module server needs (the dodge offsets for overlapping
# mean/median annotations, the per-condition KDE-eligibility filter, and the
# per-sample bar ordering) so the module server stays thin and the logic is
# unit-testable with NO Shiny and NO network.
#
# All @noRd (internal). See tab_pelsa_section2.R for the module wiring and
# tab_pelsa_analysis_helpers.R (@section Cache contract) for the cache shapes.
################################################################################

# ---- vertical-dodge offsets for stat annotations -----------------------------

# Compute vertically-dodged y positions for a set of plot annotations.
#
# When a density/histogram carries several vertical reference lines (e.g. a mean
# AND a median line) their text labels collide if placed at the same height.
# This returns one y position per label, stacked DOWNWARD from `y_top` by a fixed
# fraction of the plot's y-range so the labels never overlap. Pure + closed-form.
#
# @param n      number of labels to place (>= 0).
# @param y_top  the top y position (data units) for the first label.
# @param y_range the plot's y extent (data units) used to size the step.
# @param frac   fraction of y_range between successive labels (default 0.08).
# @return numeric vector length n of y positions (top-to-bottom). length-0 when
#         n == 0.
# @noRd
pelsa_dodge_offsets <- function(n, y_top, y_range, frac = 0.08) {
  n <- as.integer(n)
  if (is.na(n) || n <= 0L) return(numeric(0))
  if (!is.finite(y_top))   y_top <- 0
  if (!is.finite(y_range) || y_range <= 0) y_range <- 1
  step <- y_range * frac
  y_top - step * (seq_len(n) - 1L)
}

# ---- per-condition CV KDE eligibility ----------------------------------------

# Partition conditions into KDE-eligible vs skipped based on finite-CV counts.
#
# 6B draws one KDE density curve per condition from the `cv` cache table, using
# only cv_status == "ok" rows. A KDE on too few points is noise, so a condition
# with fewer than `min_n` finite CVs is SKIPPED (and surfaced in a note). The
# returned `eligible` preserves the requested `condition_order` (then any extra
# conditions present in the data in natural order); `skipped` lists the rest with
# their counts. Pure: a function of the cv table + an order vector.
#
# @param cv         the cache `cv` data.frame (row_id/condition/cv_pct/cv_status)
#                   or NULL.
# @param condition_order character - the user's confirmed condition order; may be
#                   NULL/empty (then the cv table's natural condition order).
# @param min_n      minimum finite ("ok") CVs for a KDE (default 20L).
# @return list(eligible = character, skipped = data.frame(condition, n)). When cv
#         is NULL/empty both are empty.
# @noRd
pelsa_cv_kde_eligibility <- function(cv, condition_order = NULL, min_n = 20L) {
  empty <- list(
    eligible = character(0),
    skipped  = data.frame(condition = character(0), n = integer(0),
                          stringsAsFactors = FALSE)
  )
  if (is.null(cv) || !is.data.frame(cv) || nrow(cv) == 0L) return(empty)
  if (!all(c("condition", "cv_status") %in% names(cv))) return(empty)

  # Counts per condition over the OK rows only. RELIES ON the cache contract
  # invariant cv_status == "ok" => cv_pct is finite (2D / pelsa_within_
  # condition_cv). If a future contract change allows non-finite "ok" rows, this
  # would over-count and report a condition eligible whose plot (which re-filters
  # is.finite(cv_pct)) then draws no curve - filter to finite here too at that
  # point.
  ok <- cv[!is.na(cv$cv_status) & cv$cv_status == "ok", , drop = FALSE]
  counts <- table(as.character(ok$condition))

  # Determine the condition universe + display order: requested order first
  # (intersected with what's present), then any remaining conditions in the cv
  # table's natural appearance order.
  present <- unique(as.character(cv$condition))
  present <- present[!is.na(present) & nzchar(present)]
  req <- as.character(condition_order %||% character(0))
  req <- req[!is.na(req) & nzchar(req)]
  ordered <- c(intersect(req, present), setdiff(present, req))

  n_for <- function(cond) {
    v <- counts[[cond]]
    if (is.null(v)) 0L else as.integer(v)
  }

  eligible <- character(0)
  skipped_cond <- character(0)
  skipped_n    <- integer(0)
  for (cond in ordered) {
    n <- n_for(cond)
    if (n >= min_n) {
      eligible <- c(eligible, cond)
    } else {
      skipped_cond <- c(skipped_cond, cond)
      skipped_n    <- c(skipped_n, n)
    }
  }

  list(
    eligible = eligible,
    skipped  = data.frame(condition = skipped_cond, n = skipped_n,
                          stringsAsFactors = FALSE)
  )
}

# ---- per-sample bar order -----------------------------------------------------

# Order the per-sample depth bars by the canonical sample order.
#
# 6C draws one bar per sample (height = n_quantified[sample]). Bars are ordered
# by the setup's canonical `sample_order` when available; samples present in
# n_quantified but absent from sample_order are appended (alphabetically) so none
# are dropped; sample_order entries with no n_quantified value are ignored. When
# sample_order is NULL/empty, falls back to alphabetical. Pure.
#
# @param n_quantified NAMED integer vector (names = samples) from the cache.
# @param sample_order character - canonical sample order, or NULL.
# @return character - the sample names in render order (a permutation of
#         names(n_quantified)).
# @noRd
pelsa_sample_bar_order <- function(n_quantified, sample_order = NULL) {
  samples <- names(n_quantified)
  if (is.null(samples)) return(character(0))
  samples <- samples[!is.na(samples) & nzchar(samples)]
  if (length(samples) == 0L) return(character(0))

  so <- as.character(sample_order %||% character(0))
  so <- so[!is.na(so) & nzchar(so)]
  if (length(so) == 0L) return(sort(samples))

  primary <- intersect(so, samples)             # canonical order, present only
  extras  <- sort(setdiff(samples, primary))    # leftovers, alphabetical
  c(primary, extras)
}

# ---- per-sample depth plot data ----------------------------------------------

# Shape the per-sample depth bar data from the cache (ordered, ready to plot).
#
# @param n_quantified NAMED integer vector from the cache.
# @param sample_order character - canonical order, or NULL.
# @return data.frame(sample = ordered factor, n = integer). Empty when no
#         samples. The `sample` column is a factor with levels in render order so
#         the bar plot respects the ordering.
# @noRd
pelsa_depth_bar_data <- function(n_quantified, sample_order = NULL) {
  ord <- pelsa_sample_bar_order(n_quantified, sample_order)
  if (length(ord) == 0L) {
    return(data.frame(sample = factor(character(0)), n = integer(0),
                      stringsAsFactors = FALSE))
  }
  data.frame(
    sample = factor(ord, levels = ord),
    n      = as.integer(n_quantified[ord]),
    stringsAsFactors = FALSE
  )
}

# ---- coverage / length / missed-cleavage plot data ---------------------------

# Finite coverage FRACTIONS from the coverage cache (for the distribution plot).
#
# @param coverage the cache `coverage` data.frame (accession/coverage/...).
# @return numeric - finite coverage values in [0,1]. Empty when none.
# @noRd
pelsa_coverage_values <- function(coverage) {
  if (is.null(coverage) || !is.data.frame(coverage) ||
      !("coverage" %in% names(coverage))) {
    return(numeric(0))
  }
  v <- suppressWarnings(as.numeric(coverage$coverage))
  v[is.finite(v)]
}

# Count of clamped (over-length) accessions in the coverage cache.
#
# Spectronaut peptide positions can exceed the FASTA protein length (isoform /
# annotation drift); 2F flags these. The Summary surfaces the count so the user
# knows some coverage values were clamped.
#
# @param coverage the cache `coverage` data.frame.
# @return integer scalar (0 when the flag column is absent / no rows).
# @noRd
pelsa_over_length_count <- function(coverage) {
  if (is.null(coverage) || !is.data.frame(coverage) ||
      !("over_length_flag" %in% names(coverage)) || nrow(coverage) == 0L) {
    return(0L)
  }
  flag <- as.logical(coverage$over_length_flag)
  sum(!is.na(flag) & flag)
}

# Finite peptide lengths from the peptide_metrics cache.
# @noRd
pelsa_length_values <- function(peptide_metrics) {
  if (is.null(peptide_metrics) || !is.data.frame(peptide_metrics) ||
      !("peptide_length" %in% names(peptide_metrics))) {
    return(numeric(0))
  }
  v <- suppressWarnings(as.numeric(peptide_metrics$peptide_length))
  v[is.finite(v)]
}

# Finite, "ok"-status CV percentages pooled across ALL conditions.
#
# The experiment-wide CV toggle mode draws ONE density over every replicate-CV
# the per-condition KDE would otherwise split. It applies the same
# cv_status == "ok" + finite filter, but -- unlike the per-condition KDE -- does
# NOT drop conditions with < 20 CVs: pooling is exactly what makes a small
# condition's CVs usable, so the pooled view is a strict superset. The caller
# (pelsa_cv_overall_plot) discloses the pooled count in the panel subtitle.
#
# @param cv the cache `cv` data.frame (or NULL).
# @return numeric - finite ok cv_pct values. Empty when none.
# @noRd
pelsa_cv_ok_values <- function(cv) {
  if (is.null(cv) || !is.data.frame(cv) ||
      !all(c("cv_pct", "cv_status") %in% names(cv))) {
    return(numeric(0))
  }
  keep <- !is.na(cv$cv_status) & cv$cv_status == "ok"
  v <- suppressWarnings(as.numeric(cv$cv_pct[keep]))
  v[is.finite(v)]
}

# Missed-cleavage counts as an ordered factor-ready data.frame for the bar plot.
#
# @param peptide_metrics the cache `peptide_metrics` data.frame.
# @return data.frame(missed = integer, count = integer), one row per distinct
#         missed-cleavage value in ascending order. Empty when none.
# @noRd
pelsa_missed_cleavage_data <- function(peptide_metrics) {
  empty <- data.frame(missed = integer(0), count = integer(0),
                      stringsAsFactors = FALSE)
  if (is.null(peptide_metrics) || !is.data.frame(peptide_metrics) ||
      !("missed_cleavages" %in% names(peptide_metrics))) {
    return(empty)
  }
  v <- suppressWarnings(as.integer(peptide_metrics$missed_cleavages))
  v <- v[is.finite(v)]
  if (length(v) == 0L) return(empty)
  tb <- table(v)
  data.frame(
    missed = as.integer(names(tb)),
    count  = as.integer(tb),
    stringsAsFactors = FALSE
  )
}
