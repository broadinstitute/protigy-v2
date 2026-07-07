################################################################################
# Module: PELSA - Section 2 (Summary) plot + export builders
#
# Pure ggplot builders and per-ome export-bundle helpers for the Section 2
# dashboard. Split out of tab_pelsa_section2.R (which keeps the tab-level
# UI/Server + the dashboard UI markup) purely to keep both files under the
# repo's 800-line file cap -- no behavior differs from the pre-split file.
# See tab_pelsa_section2.R's header for the full Section 2 contract
# (cache shape, 6A-6E sections) and tab_pelsa_section2_helpers.R for the pure
# plot-data / shaping logic these builders consume.
################################################################################

################################################################################
# Plot builders (pure; read the small cache tables, build a ggplot)
################################################################################

# A blank placeholder plot carrying a centered message (used when a panel cannot
# be drawn, e.g. zero FASTA matches). @noRd
pelsa_blank_plot <- function(message) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = message, size = 4) +
    theme_void()
}

# Condition (or pooled "Experiment-wide") bar+error-bar plot: one bar per row
# of `bar_df` (mean), an error bar at mean +/- sd (omitted when sd is NA,
# e.g. a defensive n=1 row that slipped past the caller's min_replicates
# gate), and a value+n label above each bar/whisker. No x-axis title -- the
# bar's own tick label (condition name, or "Experiment-wide") is
# self-explanatory, mirroring pelsa_depth_bar_plot's bar-label layout but
# WITHOUT that plot's "Sample" x title (this builder's x is a condition, not
# a sample). export=TRUE applies the same static-figure styling used by the
# other Section-2 QC plots (title size 12 centered, black size-8 axis text).
# @noRd
pelsa_condition_bar_plot <- function(bar_df, y_label, title, fill,
                                     y_fmt = function(v) sprintf("%.1f", v),
                                     blank_msg = "Not enough replicate samples to plot.",
                                     export = FALSE) {
  if (is.null(bar_df) || !is.data.frame(bar_df) || nrow(bar_df) == 0L) {
    return(pelsa_blank_plot(blank_msg))
  }
  df <- bar_df
  df$condition <- factor(df$condition, levels = df$condition)
  df$ymin <- ifelse(is.na(df$sd), df$mean, df$mean - df$sd)
  df$ymax <- ifelse(is.na(df$sd), df$mean, df$mean + df$sd)
  head_room <- 0.06 * max(df$ymax, na.rm = TRUE)
  df$label_y <- df$ymax + head_room
  df$bar_label <- sprintf("%s\n(n=%d)", y_fmt(df$mean), df$n)

  label_size <- if (export) 4 else 3
  x_text_size <- if (export) 9 else 11
  p <- ggplot(df, aes(x = .data$condition, y = .data$mean)) +
    geom_col(fill = fill) +
    geom_errorbar(aes(ymin = .data$ymin, ymax = .data$ymax),
                  data = df[!is.na(df$sd), , drop = FALSE],
                  width = 0.2) +
    geom_text(aes(y = .data$label_y, label = .data$bar_label),
              vjust = 0, size = label_size, fontface = "bold") +
    scale_y_continuous(labels = scales::label_comma(),
                       expand = expansion(mult = c(0, 0.30))) +
    labs(x = NULL, y = y_label, title = title) +
    pelsa_plot_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = x_text_size,
                                     colour = if (export) "black" else NULL))
  if (export) {
    p$theme$plot.title.position <- NULL
    p <- p + ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
      axis.text  = ggplot2::element_text(size = 8, colour = "black"))
  }
  p
}

# Experiment-wide DENSITY with BOTH a dashed mean and a dashed median line, the
# text annotations vertically dodged so they don't overlap. The shared builder
# behind the coverage + length panels' "Experiment-wide" toggle mode.
#
# @param vals      numeric values (NA / non-finite dropped).
# @param value_fmt function(value) -> string used in the "mean = .." / "median =
#                  .." labels (e.g. coverage formats as a percentage).
# @noRd
#
# @param x_hi      optional numeric upper x-limit. The left edge is always
#                  clamped to 0 via coord_cartesian. When non-NULL and finite,
#                  the right edge is also clamped to x_hi so a long right tail
#                  of outliers doesn't blow out the scale (and keeps this
#                  experiment-wide mode aligned with the per-condition KDE,
#                  which clamps at the 99th percentile). NULL (the default)
#                  leaves the right edge un-clamped (auto), which the coverage
#                  and peptide-length callers rely on.
pelsa_overall_density_plot <- function(vals, x_label, title,
                                       value_fmt = function(v) sprintf("%.1f", v),
                                       fill = "#59a14f", subtitle = NULL,
                                       blank_msg = "Not enough values for a density.",
                                       x_hi = NULL, x_scale = NULL, export = FALSE) {
  vals <- vals[is.finite(vals)]
  if (length(vals) < 2L) return(pelsa_blank_plot(blank_msg))
  ann_size <- if (export) 4.2 else 3.2
  m  <- mean(vals)
  md <- stats::median(vals)
  y_top <- tryCatch(max(stats::density(vals)$y, na.rm = TRUE),
                    error = function(e) 1)
  if (!is.finite(y_top) || y_top <= 0) y_top <- 1
  ys <- pelsa_dodge_offsets(2L, y_top = y_top * 0.95, y_range = y_top)
  df <- data.frame(x = vals)

  # White halo behind the mean/median labels so they stay readable over the
  # density fill (matches the per-condition + CV panels). Build a 2-row frame in
  # the (x, y, label) shape pelsa_halo_text_layers expects; y_top drives its
  # offset scale. hjust = -0.05 in the halo mirrors the colored labels below so
  # the ring sits centered on the same glyphs.
  halo_df <- data.frame(
    x = c(m, md),
    y = c(ys[1], ys[2]),
    label = c(paste0("mean = ", value_fmt(m)),
              paste0("median = ", value_fmt(md))),
    stringsAsFactors = FALSE
  )
  p <- ggplot(df, aes(x = .data$x)) +
    geom_density(fill = fill, alpha = 0.4, color = fill) +
    geom_vline(xintercept = m,  linetype = "dashed", color = "#e15759") +
    geom_vline(xintercept = md, linetype = "dashed", color = "#4e79a7") +
    pelsa_halo_text_layers(halo_df, x_hi = max(vals), peak = y_top, size = ann_size) +
    annotate("text", x = m,  y = ys[1], label = paste0("mean = ", value_fmt(m)),
             color = "#e15759", hjust = -0.05, size = ann_size, fontface = "bold") +
    annotate("text", x = md, y = ys[2],
             label = paste0("median = ", value_fmt(md)),
             color = "#4e79a7", hjust = -0.05, size = ann_size, fontface = "bold")
  # Always clamp the left edge to 0 (vals here are always non-negative counts,
  # lengths, or fractions), mirroring pelsa_per_condition_density_plot's x_lo.
  # Without this, a floating density curve whose mass sits away from 0 can
  # silently drop 0 off the rendered x-axis. NA for the upper bound means
  # "use the data's natural extent," preserving the unclamped-right-edge
  # behavior for callers (length/coverage) that don't pass x_hi.
  right_bound <- if (!is.null(x_hi) && is.finite(x_hi) && x_hi > 0) x_hi else NA
  base_theme <- pelsa_plot_theme()
  if (export) {
    base_theme$plot.title.position <- NULL
  }
  p <- p + labs(x = x_label, y = "Density", title = title, subtitle = subtitle) +
    base_theme
  if (export) {
    p <- p + ggplot2::theme(
      plot.title    = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
      axis.text     = ggplot2::element_text(size = 8, colour = "black"))
  }
  p <- p + coord_cartesian(xlim = c(0, right_bound))
  if (!is.null(x_scale)) p <- p + x_scale
  p
}

# Per-condition DENSITY: one curve per ELIGIBLE condition (>= min_n finite
# values), a vertical dashed median line per condition with dodged white-halo
# labels, x-limit at the 99th percentile. The shared builder behind the coverage
# + length panels' "Per-condition" toggle mode (the CV panel keeps its own
# pelsa_cv_kde_plot, which carries the >=20-CV skipped-condition note).
#
# @param df         data.frame with a `condition` column + `value_col`.
# @param value_col  name of the numeric value column.
# @param value_fmt  function(value) -> string for the median labels.
# @param min_n      minimum finite values for a condition's density (default 2).
# @noRd
pelsa_per_condition_density_plot <- function(df, value_col,
                                             condition_order = NULL,
                                             x_label, title, subtitle = NULL,
                                             value_fmt = function(v) sprintf("%.1f", v),
                                             min_n = 2L,
                                             blank_msg = "No per-condition data to display.",
                                             x_scale = NULL, export = FALSE) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L ||
      !all(c("condition", value_col) %in% names(df))) {
    return(pelsa_blank_plot(blank_msg))
  }
  d <- data.frame(condition = as.character(df$condition),
                  value = suppressWarnings(as.numeric(df[[value_col]])),
                  stringsAsFactors = FALSE)
  d <- d[is.finite(d$value) & !is.na(d$condition) & nzchar(d$condition), ,
         drop = FALSE]
  if (nrow(d) == 0L) return(pelsa_blank_plot(blank_msg))

  # Eligibility + display order: requested condition_order first (present only),
  # then any remaining conditions in natural order; drop conditions with < min_n.
  counts <- table(d$condition)
  present <- unique(d$condition)
  req <- as.character(condition_order %||% character(0))
  req <- req[!is.na(req) & nzchar(req)]
  ordered <- c(intersect(req, present), setdiff(present, req))
  eligible <- ordered[vapply(ordered, function(cond) {
    as.integer(counts[[cond]] %||% 0L) >= min_n
  }, logical(1))]
  if (length(eligible) == 0L) {
    return(pelsa_blank_plot(
      sprintf("No condition has >= %d values to draw a density.", min_n)))
  }
  d <- d[d$condition %in% eligible, , drop = FALSE]
  d$condition <- factor(d$condition, levels = eligible)

  x_hi <- stats::quantile(d$value, 0.99, na.rm = TRUE, names = FALSE)
  if (!is.finite(x_hi) || x_hi <= min(d$value, na.rm = TRUE)) {
    x_hi <- max(d$value, na.rm = TRUE)
  }
  x_lo <- min(0, min(d$value, na.rm = TRUE))

  medians <- stats::aggregate(value ~ condition, data = d,
                              FUN = function(x) stats::median(x, na.rm = TRUE))
  peak <- tryCatch(max(stats::density(d$value)$y, na.rm = TRUE),
                   error = function(e) 1)
  if (!is.finite(peak) || peak <= 0) peak <- 1
  medians$y <- pelsa_dodge_offsets(nrow(medians), y_top = peak * 0.95,
                                   y_range = peak)
  medians$x <- medians$value
  # Disclose the per-condition n alongside each median so a curve drawn from a
  # handful of values is self-evidently noisy (rather than presented as an
  # authoritative median). Mirrors the CV-KDE labels.
  medians$n <- as.integer(counts[as.character(medians$condition)])
  medians$label <- vapply(seq_len(nrow(medians)), function(i) {
    if (export) {
      sprintf("median = %s", value_fmt(medians$value[i]))
    } else {
      sprintf("%s median = %s (n=%d)", medians$condition[i],
              value_fmt(medians$value[i]), medians$n[i])
    }
  }, character(1))

  base_theme <- pelsa_plot_theme()
  if (export) base_theme$plot.title.position <- NULL
  p <- ggplot(d, aes(x = .data$value, color = .data$condition,
                fill = .data$condition)) +
    geom_density(alpha = 0.15) +
    geom_vline(data = medians,
               aes(xintercept = .data$x, color = .data$condition),
               linetype = "dashed", show.legend = FALSE) +
    pelsa_halo_text_layers(medians, x_hi = x_hi, peak = peak) +
    geom_text(data = medians,
              aes(x = .data$x, y = .data$y, label = .data$label,
                  color = .data$condition),
              hjust = -0.05, size = 3, show.legend = FALSE, fontface = "bold") +
    coord_cartesian(xlim = c(x_lo, x_hi)) +
    labs(x = x_label, y = "Density", color = "Condition", fill = "Condition",
         title = title, subtitle = subtitle) +
    base_theme +
    guides(color = guide_legend(override.aes = list(size = 2)),
           fill  = guide_legend(override.aes = list(size = 2)))
  if (export) {
    p <- p + ggplot2::theme(
      plot.title    = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
      axis.text     = ggplot2::element_text(size = 8, colour = "black"))
  }
  if (!is.null(x_scale)) p <- p + x_scale
  p
}

# 6A: per-protein sequence coverage bar+error-bar (mean +/- SD across
# replicate samples per condition, or pooled experiment-wide). @noRd
pelsa_coverage_plot <- function(coverage_by_sample, condition_map,
                                condition_order = NULL,
                                mode = c("overall", "per_condition"),
                                min_replicates = 2L, export = FALSE) {
  mode <- match.arg(mode)
  bar_df <- if (identical(mode, "per_condition")) {
    pelsa_bar_error_data(coverage_by_sample, "coverage", condition_map,
                        condition_order, min_replicates)$data
  } else {
    pelsa_bar_error_data_overall(coverage_by_sample, "coverage", min_replicates)
  }
  pelsa_condition_bar_plot(
    bar_df, y_label = "Sequence coverage (%)",
    title = "Protein sequence coverage", fill = "#4e79a7",
    y_fmt = function(v) sprintf("%.1f%%", 100 * v),
    blank_msg = sprintf(
      "No condition has >= %d replicate samples with coverage data.",
      min_replicates),
    export = export)
}

# 6A: peptide-length bar+error-bar (mean +/- SD across replicate samples per
# condition, or pooled experiment-wide). @noRd
pelsa_length_plot <- function(length_by_sample, condition_map,
                              condition_order = NULL,
                              mode = c("overall", "per_condition"),
                              min_replicates = 2L, export = FALSE) {
  mode <- match.arg(mode)
  bar_df <- if (identical(mode, "per_condition")) {
    pelsa_bar_error_data(length_by_sample, "mean_length", condition_map,
                        condition_order, min_replicates)$data
  } else {
    pelsa_bar_error_data_overall(length_by_sample, "mean_length", min_replicates)
  }
  pelsa_condition_bar_plot(
    bar_df, y_label = "Peptide length (residues)",
    title = "Average peptide length", fill = "#59a14f",
    y_fmt = function(v) sprintf("%.1f", v),
    blank_msg = sprintf(
      "No condition has >= %d replicate samples with length data.",
      min_replicates),
    export = export)
}

# 6B: experiment-wide CV DENSITY (pooled across conditions). Unlike the
# per-condition KDE (which drops conditions with < 20 finite CVs), the pooled
# view intentionally includes EVERY "ok" CV -- pooling is exactly what makes a
# small condition's CVs usable. The subtitle discloses the pooled count so the
# two toggle modes are not silently describing different universes. @noRd
pelsa_cv_overall_plot <- function(cv) {
  vals <- pelsa_cv_ok_values(cv)
  subtitle <- if (length(vals) > 0L)
    sprintf("all conditions pooled (n = %d CVs)", length(vals)) else NULL
  # Clamp the pooled density to the 99th percentile of the same ok CVs, mirroring
  # the per-condition KDE (pelsa_cv_kde_plot) so the two toggle modes share a
  # scale. NULL when there are no values, leaving the blank-plot path untouched.
  x_hi <- if (length(vals) > 0L)
    stats::quantile(vals, 0.99, na.rm = TRUE, names = FALSE) else NULL
  pelsa_overall_density_plot(
    vals, x_label = "CV (%)", title = "Coefficient of variation (CV)", fill = "#af7aa1",
    value_fmt = function(v) sprintf("%.1f%%", v), subtitle = subtitle,
    blank_msg = "No CV data - a raw GCT + condition column are required.",
    x_hi = x_hi)
}

# 6A: missed-cleavage RATE bar+error-bar (mean +/- SD across replicate
# samples per condition, or pooled experiment-wide). Rate = fraction of a
# sample's quantified peptides with >= 1 missed cleavage. @noRd
pelsa_missed_cleavage_plot <- function(missed_cleavage_rate_by_sample,
                                       condition_map, condition_order = NULL,
                                       mode = c("overall", "per_condition"),
                                       min_replicates = 2L, export = FALSE) {
  mode <- match.arg(mode)
  bar_df <- if (identical(mode, "per_condition")) {
    pelsa_bar_error_data(missed_cleavage_rate_by_sample, "rate",
                        condition_map, condition_order, min_replicates)$data
  } else {
    pelsa_bar_error_data_overall(missed_cleavage_rate_by_sample, "rate",
                                 min_replicates)
  }
  pelsa_condition_bar_plot(
    bar_df, y_label = "Missed-cleavage rate (%)",
    title = "Missed-cleavage rate", fill = "#f28e2b",
    y_fmt = function(v) sprintf("%.1f%%", 100 * v),
    blank_msg = sprintf(
      "No condition has >= %d replicate samples with missed-cleavage data.",
      min_replicates),
    export = export)
}

# 6B: per-condition CV KDE. One density curve per ELIGIBLE condition (>= 20
# finite "ok" CVs), a vertical dashed median line per condition (labels dodged),
# x-limit at the 99th percentile of cv_pct. @noRd
pelsa_cv_kde_plot <- function(cv, condition_order = NULL, export = FALSE) {
  if (is.null(cv) || !is.data.frame(cv) || nrow(cv) == 0L) {
    return(pelsa_blank_plot("No CV data - a raw GCT + condition column are required."))
  }
  elig <- pelsa_cv_kde_eligibility(cv, condition_order)
  if (length(elig$eligible) == 0L) {
    return(pelsa_blank_plot(
      "No condition has >= 20 finite CVs to draw a KDE."))
  }
  ok <- cv[!is.na(cv$cv_status) & cv$cv_status == "ok" &
             cv$condition %in% elig$eligible &
             is.finite(cv$cv_pct), , drop = FALSE]
  if (nrow(ok) == 0L) return(pelsa_blank_plot("No finite CVs to display."))

  ok$condition <- factor(ok$condition, levels = elig$eligible)
  x_hi <- stats::quantile(ok$cv_pct, 0.99, na.rm = TRUE, names = FALSE)
  if (!is.finite(x_hi) || x_hi <= 0) x_hi <- max(ok$cv_pct, na.rm = TRUE)

  medians <- stats::aggregate(cv_pct ~ condition, data = ok,
                              FUN = function(x) stats::median(x, na.rm = TRUE))
  # Estimate the density peak height to anchor the dodged median labels near the
  # top, then dodge each condition's label DOWNWARD so they never overlap (the
  # same pattern the length-density plot uses).
  peak <- tryCatch(max(stats::density(ok$cv_pct)$y, na.rm = TRUE),
                   error = function(e) 1)
  if (!is.finite(peak) || peak <= 0) peak <- 1
  medians$y <- pelsa_dodge_offsets(nrow(medians), y_top = peak * 0.95,
                                   y_range = peak)
  medians$x <- medians$cv_pct
  cv_counts <- table(ok$condition)
  medians$n <- as.integer(cv_counts[as.character(medians$condition)])
  medians$label <- sprintf("%s median = %.1f%% (n=%d)", medians$condition,
                           medians$cv_pct, medians$n)

  base_theme <- pelsa_plot_theme()
  if (export) base_theme$plot.title.position <- NULL
  p <- ggplot(ok, aes(x = .data$cv_pct, color = .data$condition,
                 fill = .data$condition)) +
    geom_density(alpha = 0.15) +
    geom_vline(data = medians,
               aes(xintercept = .data$x, color = .data$condition),
               linetype = "dashed", show.legend = FALSE) +
    # White halo behind the median labels (multiple nudged white copies) so the
    # text stays readable over overlapping density curves, then the colored text.
    pelsa_halo_text_layers(medians, x_hi = x_hi, peak = peak) +
    geom_text(data = medians,
              aes(x = .data$x, y = .data$y, label = .data$label,
                  color = .data$condition),
              hjust = -0.05, size = 3, show.legend = FALSE, fontface = "bold") +
    coord_cartesian(xlim = c(0, x_hi)) +
    labs(x = "CV (%)", y = "Density", color = "Condition", fill = "Condition",
         title = "Coefficient of variation (CV)") +
    base_theme +
    guides(color = guide_legend(override.aes = list(size = 2)),
           fill  = guide_legend(override.aes = list(size = 2)))
  if (export) {
    p <- p + ggplot2::theme(
      plot.title    = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
      axis.text     = ggplot2::element_text(size = 8, colour = "black"))
  }
  p
}

# White-halo outline for the per-condition median labels. ggplot has no native
# text-halo, and shadowtext does not round-trip through ggplotly. We emulate one
# by drawing the label several times in white UNDER the colored text, each copy
# offset by a small fraction of the x/y extents. The offsets are baked into the
# DATA (new x/y columns) rather than applied with nudge_x/nudge_y: ggplotly
# silently drops position_nudge, collapsing the halo onto one point, whereas
# pre-offset coordinates survive the round-trip. Eight offsets (cardinal +
# diagonal). Returns ONE geom_text layer over the expanded frame.
# @noRd
pelsa_halo_text_layers <- function(medians, x_hi, peak, size = 3) {
  # Tight, symmetric ring: small fraction of the axis extents, eight directions
  # (cardinal + diagonal) so the white copies form a halo around the glyphs
  # rather than a one-sided shadow. Diagonal copies use 1/sqrt(2) so all eight
  # sit on a circle of roughly equal radius.
  dx <- (if (is.finite(x_hi) && x_hi > 0) x_hi else 1) * 0.0025
  dy <- (if (is.finite(peak) && peak > 0) peak else 1) * 0.006
  d <- 1 / sqrt(2)
  offs <- data.frame(
    ox = c(-1, 1,  0, 0, -d,  d, -d, d),
    oy = c( 0, 0, -1, 1, -d, -d,  d, d)
  )
  halo <- do.call(rbind, lapply(seq_len(nrow(offs)), function(i) {
    h <- medians
    h$x <- medians$x + offs$ox[i] * dx
    h$y <- medians$y + offs$oy[i] * dy
    h
  }))
  geom_text(data = halo,
            aes(x = .data$x, y = .data$y, label = .data$label),
            color = "white", hjust = -0.05, size = size,
            inherit.aes = FALSE, show.legend = FALSE, fontface = "bold")
}

# 6C: per-sample depth bar, ordered by sample_order (alphabetical fallback).
# @noRd
pelsa_depth_bar_plot <- function(n_quantified, sample_order = NULL,
                                 head_frac = 0.04, export = FALSE) {
  df <- pelsa_depth_bar_data(n_quantified, sample_order)
  if (nrow(df) == 0L) {
    return(pelsa_blank_plot("No per-sample depth data."))
  }
  # Lift each count label `head_frac` of the tallest bar ABOVE the bar top
  # (in-app default 0.04; the export path passes a smaller value). Baked into
  # label_y (ggplotly drops nudge_y); vjust = 0 anchors the label bottom.
  df$label_y <- df$n + head_frac * max(df$n, na.rm = TRUE)
  x_title <- if (export) NULL else "Sample"
  label_size <- if (export) 4 else 3
  x_text_size <- if (export) 9 else 11
  p <- ggplot(df, aes(x = .data$sample, y = .data$n)) +
    geom_col(fill = "#76b7b2") +
    geom_text(aes(y = .data$label_y, label = prettyNum(.data$n, big.mark = ",")),
              vjust = 0, size = label_size, fontface = "bold") +
    scale_y_continuous(labels = scales::label_comma(),
                       expand = expansion(mult = c(0, 0.12))) +
    labs(x = x_title, y = "Peptides quantified",
         title = "Number of quantified peptides") +
    pelsa_plot_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = x_text_size,
                                     colour = if (export) "black" else NULL))
  if (export) {
    # Deleting the list element (not assigning theme(plot.title.position=NULL))
    # is what actually drops the "plot"-wide-centering override so the title
    # falls back to ggplot2's panel-centered default -- a `+ theme(x = NULL)`
    # merge does NOT unset an already-set element in this ggplot2 version.
    p$theme$plot.title.position <- NULL
    p <- p + ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
      axis.text  = ggplot2::element_text(size = 8, colour = "black"))
  }
  p
}

################################################################################
# Export builders (6E) - re-derive each file from the cache entry
################################################################################

# Per-sample QC summary: one row per sample, the non-NA peptide count (depth).
# @noRd
pelsa_qc_sample_summary <- function(entry) {
  nq <- entry$n_quantified
  data.frame(
    sample = names(nq) %||% character(0),
    n_peptides_quantified = as.integer(nq %||% integer(0)),
    stringsAsFactors = FALSE
  )
}

# Per-condition QC summary: median/mean CV (unchanged, from the per-peptide
# cv frame), and mean/sd of the per-sample-averaged coverage, peptide-length,
# and missed-cleavage rate (matching the Summary bar+error-bar panels).
# Columns absent from the cache are simply omitted (graceful).
# @noRd
pelsa_qc_condition_summary <- function(entry, condition_order = NULL) {
  cv <- entry$cv %||% data.frame()
  agg_cv <- function(fun) {
    if (!is.data.frame(cv) || nrow(cv) == 0L ||
        !all(c("condition", "cv_pct") %in% colnames(cv))) {
      return(stats::setNames(numeric(0), character(0)))
    }
    tapply(as.numeric(cv$cv_pct), as.character(cv$condition), function(x) {
      x <- x[is.finite(x)]
      if (length(x) == 0L) NA_real_ else fun(x)
    })
  }
  med_cv  <- agg_cv(stats::median)
  mean_cv <- agg_cv(mean)

  cmap <- entry$condition_map %||% character(0)
  bar_stats <- function(per_sample_df, value_col) {
    agg <- pelsa_bar_error_data(per_sample_df %||% data.frame(), value_col,
                                cmap, condition_order, min_replicates = 1L)$data
    list(mean = stats::setNames(agg$mean, agg$condition),
        sd   = stats::setNames(agg$sd, agg$condition))
  }
  cov_stats <- bar_stats(entry$coverage_by_sample, "coverage")
  len_stats <- bar_stats(entry$length_by_sample, "mean_length")
  mc_stats  <- bar_stats(entry$missed_cleavage_rate_by_sample, "rate")

  conds <- unique(c(names(med_cv), names(cov_stats$mean), names(len_stats$mean),
                    names(mc_stats$mean)))
  if (length(conds) == 0L) return(data.frame())
  if (!is.null(condition_order)) {
    ordered <- intersect(condition_order, conds)
    conds <- c(ordered, setdiff(conds, ordered))
  }
  # n_peptides_quantified = peptides QUANTIFIED (canonical finite & non-zero) in
  # >= 1 sample of the condition, taken from the cache entry's per-condition
  # membership count (pelsa_condition_membership). This matches the per-sample
  # summary's "quantified" semantics; it is NOT a count of all CV rows (which
  # includes peptides that are non-finite / all-NA within the condition).
  n_pep <- entry$n_peptides_by_condition %||% integer(0)
  # A condition can appear in `conds` (it has CV samples) yet have ZERO quantified
  # peptides, so it is absent from the membership-derived n_pep -> n_pep[conds]
  # would be NA. That count is genuinely 0 (no peptide quantified in the
  # condition), not "unknown", so coerce the missing case to 0L.
  n_quant <- as.integer(n_pep[conds])
  n_quant[is.na(n_quant)] <- 0L

  data.frame(
    condition                    = conds,
    n_peptides_quantified        = n_quant,
    median_cv_pct                = unname(med_cv[conds]),
    mean_cv_pct                  = unname(mean_cv[conds]),
    mean_coverage                = unname(cov_stats$mean[conds]),
    sd_coverage                  = unname(cov_stats$sd[conds]),
    mean_peptide_length          = unname(len_stats$mean[conds]),
    sd_peptide_length            = unname(len_stats$sd[conds]),
    mean_missed_cleavage_rate    = unname(mc_stats$mean[conds]),
    sd_missed_cleavage_rate      = unname(mc_stats$sd[conds]),
    stringsAsFactors = FALSE
  )
}

# Experiment-wide QC summary: totals + FASTA/annotation failure counts/
# percents, plus mean/sd of the per-sample-averaged coverage, peptide-length,
# and missed-cleavage rate pooled across ALL samples (matching the Summary
# "Experiment-wide" bar+error-bar panels).
# @noRd
pelsa_qc_experiment_summary <- function(entry) {
  qc <- entry$qc %||% list()
  n_total <- as.integer(qc$n_peptides %||% NA_integer_)
  n_unmatched <- as.integer(qc$n_unmatched_rows %||%
                              nrow(entry$unmatched %||% data.frame()))
  n_unann <- as.integer(qc$n_unannotated_accessions %||%
                          length(entry$unannotated %||% character(0)))
  n_acc <- nrow(entry$coverage %||% data.frame())  # distinct matched accessions
  pct <- function(num, den) if (is.na(den) || den <= 0L) NA_real_ else 100 * num / den

  overall_stats <- function(per_sample_df, value_col) {
    agg <- pelsa_bar_error_data_overall(per_sample_df %||% data.frame(),
                                        value_col, min_replicates = 1L)
    if (nrow(agg) == 0L) list(mean = NA_real_, sd = NA_real_)
    else list(mean = agg$mean, sd = agg$sd)
  }
  cov <- overall_stats(entry$coverage_by_sample, "coverage")
  len <- overall_stats(entry$length_by_sample, "mean_length")
  mc  <- overall_stats(entry$missed_cleavage_rate_by_sample, "rate")

  data.frame(
    n_peptides_total             = n_total,
    n_unmatched_peptides         = n_unmatched,
    pct_unmatched_peptides       = pct(n_unmatched, n_total),
    n_unannotated_proteins       = n_unann,
    pct_unannotated_proteins     = pct(n_unann, n_acc),
    mean_missed_cleavage_rate    = mc$mean,
    sd_missed_cleavage_rate      = mc$sd,
    mean_coverage                = cov$mean,
    sd_coverage                  = cov$sd,
    mean_peptide_length          = len$mean,
    sd_peptide_length            = len$sd,
    stringsAsFactors = FALSE
  )
}

# Build the per-ome export bundle for ONE analyzed dataset. Returns a single
# `qc` function that writes the three summary CSVs + five figures into the
# 02_qc/ stage subfolder. condition_order / sample_order honor the user's
# confirmed ordering (NULL -> the builders' alphabetical fallback).
# @noRd
pelsa_section2_exports_for <- function(entry, ome, condition_order = NULL,
                                       sample_order = NULL, gct = NULL,
                                       marker_accs = NULL, params = NULL,
                                       custom = NULL) {
  qc_bundle <- function(dir_name) {
    out <- pelsa_export_stage_dir(dir_name, .PELSA_STAGE_QC)

    utils::write.csv(pelsa_qc_sample_summary(entry),
                     file.path(out, "qc_sample_summary.csv"), row.names = FALSE)
    utils::write.csv(pelsa_qc_condition_summary(entry, condition_order),
                     file.path(out, "qc_condition_summary.csv"), row.names = FALSE)
    utils::write.csv(pelsa_qc_experiment_summary(entry),
                     file.path(out, "qc_experiment_summary.csv"), row.names = FALSE)

    cvd <- entry$cv %||% data.frame()
    nq  <- entry$n_quantified
    save_fig <- function(p, base, w = 5.6, h = 3.5) tryCatch(
      pelsa_save_figure(p, out, base, width = w, height = h),
      error = function(e) NULL)

    cbs <- entry$coverage_by_sample %||% data.frame()
    lbs <- entry$length_by_sample %||% data.frame()
    mbs <- entry$missed_cleavage_rate_by_sample %||% data.frame()
    cmap <- entry$condition_map %||% character(0)
    if (nrow(cbs) > 0L) {
      save_fig(pelsa_coverage_plot(cbs, cmap, mode = "overall", export = TRUE),
               "coverage_distribution_experiment_wide")
      save_fig(pelsa_coverage_plot(cbs, cmap, condition_order,
                                  mode = "per_condition", export = TRUE),
               "coverage_distribution_per_condition")
    }
    if (nrow(lbs) > 0L) {
      save_fig(pelsa_length_plot(lbs, cmap, mode = "overall", export = TRUE),
               "peptide_length_density_experiment_wide")
      save_fig(pelsa_length_plot(lbs, cmap, condition_order,
                                 mode = "per_condition", export = TRUE),
               "peptide_length_density_per_condition")
    }
    if (nrow(mbs) > 0L) {
      save_fig(pelsa_missed_cleavage_plot(mbs, cmap, mode = "overall",
                                          export = TRUE),
               "missed_cleavage_rate_experiment_wide")
      save_fig(pelsa_missed_cleavage_plot(mbs, cmap, condition_order,
                                          mode = "per_condition", export = TRUE),
               "missed_cleavage_rate_per_condition")
    }
    if (is.data.frame(cvd) && nrow(cvd) > 0L)
      save_fig(pelsa_cv_kde_plot(cvd, condition_order, export = TRUE), "cv_kde")
    if (length(nq) > 0L)
      save_fig(pelsa_depth_bar_plot(nq, sample_order, head_frac = 0.02, export = TRUE),
               "n_peptides_per_sample")

    if (!is.null(gct)) {
      tryCatch(
        pelsa_splot_export_for(dir_name, gct, entry$matched, marker_accs,
                               params, custom),
        error = function(e) warning(sprintf(
          "pelsa_section2_exports_for: S-plot export failed for '%s': %s",
          ome, conditionMessage(e)), call. = FALSE))
    }

    invisible(out)
  }
  list(qc = qc_bundle)
}
