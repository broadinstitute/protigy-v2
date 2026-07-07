################################################################################
# Module: PELSA per-protein INTENSITY-LINE plot + static ggplot EXPORT helpers
# - pure, no Shiny.
#
# Split out of R/tab_pelsa_volcano_helpers.R (which grew past the repo's
# 800-line file cap) to keep the intensity-line panel and the static-export
# ggplot concerns separate from the volcano data-frame builder / plot-assembly
# helpers that remain in the sibling _helpers files.
#
# Covers: the empty matched-cache fallback frame, the per-protein intensity
# LINE ggplot/plotly (the pinned-peptide panel), and the static PNG/PDF export
# ggplot builders (color spec + full export plot) used by the PELSA export
# pipeline.
################################################################################

# ---- 7F: the static export ggplot + the empty matched-cache frame -----------

# A canonical empty matched-cache frame (the columns 3A's all-peptide join
# reads), used when the active dataset has no matched rows so 3A still runs and
# yields an unlabeled (label = NA) frame rather than erroring. @noRd
pelsa_volcano_empty_matched <- function() {
  data.frame(
    PEP.StrippedSequence = character(0),
    accession            = character(0),
    gene                 = character(0),
    pep_start            = integer(0),
    pep_end              = integer(0),
    stringsAsFactors     = FALSE,
    check.names          = FALSE
  )
}

# Assemble the per-protein intensity LINE ggplot from 3C line data (the pinned
# panel's plot). One line per (peptide_seq, pep_occurrence_idx), colored by the
# end-of-line aa_label; marker proteins facet Significant/Non-significant (>1
# panel value), a non-marker single panel. Pure ggplot - the caller wraps it in
# ggplotly.
#
# The PINNED peptide (the one the user clicked) is highlighted: its line/points
# are drawn in GOLD and its legend entry is bolded + suffixed " (selected)", so
# it is easy to tell the clicked peptide apart from the other peptides mapped to
# the same protein. The facet strip labels are bold + sit ABOVE the panel (so
# the band never overlaps the lines).
#
# @param ld a pelsa_intensity_line_data() frame (condition factor, mean_log2,
#   peptide_seq, pep_occurrence_idx, aa_label, panel).
# @param pinned_label the pinned peptide's aa_label (e.g. "aa462") to highlight,
#   or NULL for no highlight.
# @return a ggplot object.
# @noRd
pelsa_intensity_line_ggplot <- function(ld, pinned_label = NULL) {
  # Clean per-point hover tooltip (built from the RAW columns before the pinned
  # remap mangles aa_label): aa_label, position start->end, sequence, condition,
  # mean intensity. pep_end may be NA (older caches) -> show only the start.
  pos_txt <- ifelse(is.na(ld$pep_end %||% NA),
                    as.character(ld$pep_start),
                    paste0(ld$pep_start, " -> ", ld$pep_end))
  ld$.tip <- paste0(
    ld$aa_label, "<br>",
    "Position: ", pos_txt, "<br>",
    "Sequence: ", ld$peptide_seq, "<br>",
    "Condition: ", as.character(ld$condition), "<br>",
    "Mean log2 intensity: ", sprintf("%.2f", ld$mean_log2)
  )

  # Order aa_labels by residue position so the legend reads ascending.
  pos <- suppressWarnings(as.integer(sub("^aa", "", ld$aa_label)))
  raw_lvl <- unique(ld$aa_label[order(pos, ld$aa_label)])

  # Relabel the pinned key in the DATA + the factor levels (so ggplotly carries
  # the bold "(selected)" text into the trace name - ggplotly uses the factor
  # level as the legend/trace name, not scale_*'s `labels=` arg). Bold via plotly
  # HTML (<b>); harmless plain text in a static ggplot.
  pinned_disp <- if (!is.null(pinned_label) && nzchar(pinned_label)) {
    paste0("<b>", pinned_label, " (selected)</b>")
  } else NA_character_
  # Guard the no-pin case: with pinned_label NULL, `x == pinned_label` is
  # length-0 and collapses ifelse() to a 0-row result (breaks the column
  # assignment). Return x unchanged when there is nothing to remap.
  remap <- function(x) {
    if (is.null(pinned_label) || is.na(pinned_disp)) return(x)
    ifelse(x == pinned_label, pinned_disp, x)
  }
  ld$aa_label <- remap(ld$aa_label)
  lvl <- remap(raw_lvl)
  ld$aa_label <- factor(ld$aa_label, levels = lvl)

  # Per-key colors: the pinned peptide gold, the rest from the default hue
  # palette.
  is_pinned_lvl <- !is.na(pinned_disp) & lvl == pinned_disp
  others <- lvl[!is_pinned_lvl]
  hues <- scales::hue_pal()(max(length(others), 1L))
  pal <- stats::setNames(rep(NA_character_, length(lvl)), lvl)
  pal[others] <- hues[seq_along(others)]
  if (any(is_pinned_lvl)) pal[lvl[is_pinned_lvl]] <- .PELSA_VOLCANO_GOLD

  gg <- ggplot2::ggplot(
    ld,
    ggplot2::aes(x = .data$condition, y = .data$mean_log2,
                 group = interaction(.data$peptide_seq,
                                     .data$pep_occurrence_idx),
                 color = .data$aa_label, text = .data$.tip)
  ) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(na.rm = TRUE, size = 1.4) +
    ggplot2::scale_color_manual(values = pal, drop = FALSE)
  # The legend is removed (the hover tooltip identifies each line), so mark the
  # SELECTED peptide's line with black-outlined points (gold fill + black ring) so
  # the user can still tell which line is the clicked one.
  if (any(is_pinned_lvl)) {
    sel_rows <- ld[ld$aa_label %in% lvl[is_pinned_lvl], , drop = FALSE]
    if (nrow(sel_rows) > 0L) {
      # `text` is a plotly tooltip aes, not a ggplot one; geom_point() emits a
      # DEFERRED "Ignoring unknown aesthetics: text" warning at construction
      # that escapes suppressWarnings at the ggplotly/build site, so muffle it
      # here where the layer is actually built.
      gg <- gg + suppressWarnings(ggplot2::geom_point(
        data = sel_rows, na.rm = TRUE, shape = 21, size = 2.2, stroke = 0.6,
        fill = .PELSA_VOLCANO_GOLD, color = "black",
        ggplot2::aes(x = .data$condition, y = .data$mean_log2,
                     group = interaction(.data$peptide_seq,
                                         .data$pep_occurrence_idx),
                     text = .data$.tip),
        inherit.aes = FALSE, show.legend = FALSE))
    }
  }
  # Marker proteins: facet Significant/Non-significant; non-marker -> single.
  # Extra TOP headroom (mult upper = 0.22) so the facet strip sits in blank space
  # above the data instead of overlapping the lines (ggplotly renders facet strips
  # as overlaid annotations; with scales="free_y" the panel can otherwise extend
  # right under the strip). panel.spacing keeps the two panels apart.
  if (length(unique(ld$panel)) > 1L) {
    gg <- gg +
      ggplot2::facet_wrap(~ .data$panel, ncol = 1, scales = "free_y") +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0.05, 0.22)))
  }
  gg +
    ggplot2::labs(x = NULL, y = "mean log2 intensity", color = NULL) +
    protigy_plot_theme(gridlines = TRUE) +
    ggplot2::theme(
      # Legend removed: the floating hover tooltip identifies each peptide line.
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      strip.text = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_rect(fill = "grey92", color = NA),
      panel.spacing = ggplot2::unit(1.2, "lines")
    )
}

# Build the pinned intensity line PLOTLY (the render path).
#
# When a marker protein has BOTH significance groups, ggplot faceting through
# ggplotly mispositions the facet strip so it overlaps the data. To avoid that
# entirely, we render the two groups as a vertical plotly::subplot of two
# single-panel ggplots (each gets a bold title annotation in clear space, no
# strip). The single-group case is a plain ggplotly. Tooltip = the .tip column.
#
# @param ld           a pelsa_intensity_line_data() frame.
# @param pinned_label the pinned peptide's aa_label to highlight (or NULL).
# @return a plotly object.
# @noRd
pelsa_intensity_line_plot <- function(ld, pinned_label = NULL) {
  panels <- unique(as.character(ld$panel))
  if (length(panels) <= 1L) {
    # showlegend = FALSE: ggplotly does not always honor legend.position="none";
    # the floating hover tooltip identifies each peptide line.
    # plotly_build() is forced INSIDE suppressWarnings so the deferred ggplot
    # build (which emits "Ignoring unknown aesthetics: text" for the plotly
    # tooltip aes) is muffled here, not later in renderPlotly's print path.
    return(suppressWarnings(plotly::plotly_build(plotly::layout(
      plotly::ggplotly(
        pelsa_intensity_line_ggplot(ld, pinned_label = pinned_label),
        tooltip = "text"),
      showlegend = FALSE))))
  }
  # Stable order: Significant on top, Non-significant below.
  ord <- c("Significant", "Non-significant")
  panels <- c(intersect(ord, panels), setdiff(panels, ord))

  parts <- lapply(panels, function(pn) {
    sub <- ld[as.character(ld$panel) == pn, , drop = FALSE]
    # NO ggtitle here: plotly::subplot collapses a per-plot ggtitle into a SINGLE
    # overall layout$title (it keeps only the LAST plot's title, so the top
    # panel's title is silently dropped and the bottom panel's renders as one
    # centered overall title). We add the per-panel titles as paper-referenced
    # subplot annotations below instead.
    gg  <- pelsa_intensity_line_ggplot(sub, pinned_label = pinned_label) +
      ggplot2::labs(y = NULL)             # one shared y-title added below
    # Only the bottom panel keeps the x tick labels (shared axis).
    # Force the build inside suppressWarnings so the deferred ggplot build
    # (which warns "Ignoring unknown aesthetics: text" for the tooltip aes)
    # is muffled here rather than later in renderPlotly's print path.
    suppressWarnings(plotly::plotly_build(plotly::ggplotly(gg, tooltip = "text")))
  })
  # titleY = FALSE so plotly does NOT render the per-panel y-axis titles (they
  # were stripped via labs(y = NULL) but titleY = TRUE would re-add them and they
  # overlap). We add exactly ONE shared, vertically-centered y-title annotation.
  margin <- 0.06
  p <- plotly::subplot(parts, nrows = length(parts), shareX = TRUE,
                       titleY = FALSE, margin = margin)

  # Per-panel TITLE at the TOP of each panel (paper coords). subplot stacks the
  # panels top-to-bottom with `margin` between them; panel i (1-based from the
  # top) spans [top_i - h, top_i] where h is the per-panel height. The title sits
  # just above each panel's top edge. Full, unambiguous wording (the short
  # "Significant"/"Non-significant" was ambiguous about WHAT contrast).
  n_panel <- length(parts)
  h <- (1 - (n_panel - 1) * margin) / n_panel
  title_for <- function(pn) {
    if (identical(pn, "Significant")) "Significant in selected contrast"
    else if (identical(pn, "Non-significant")) "Non-significant in selected contrast"
    else pn
  }
  # The top panel's top edge is at 1, so its title (yanchor = "bottom") sits
  # flush against the panel. Adding a +0.02 offset to lower panels pushed their
  # titles farther from the panel than the top one, so the gap looked uneven.
  # Anchor every title at its own panel's top edge for matching spacing. Bold
  # via <b></b> (plotly annotation font has no `face`; text supports HTML).
  panel_titles <- lapply(seq_len(n_panel), function(i) {
    top_i <- 1 - (i - 1) * (h + margin)
    list(
      text = paste0("<b>", title_for(panels[i]), "</b>"),
      x = 0.5, y = min(top_i, 1),
      xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom",
      showarrow = FALSE, font = list(size = 13, color = "rgba(0,0,0,1)"))
  })
  y_title <- list(
    text = "mean log2 intensity", x = -0.12, y = 0.5,
    xref = "paper", yref = "paper", textangle = -90,
    showarrow = FALSE, font = list(size = 12))

  p <- plotly::layout(
    p,
    title = list(text = ""),  # no overall title (per-panel titles cover this)
    showlegend = FALSE,       # tooltip identifies each line; no legend needed
    margin = list(l = 70, t = 40),  # room for the y-title + the top panel title
    annotations = c(list(y_title), panel_titles))
  suppressWarnings(plotly::plotly_build(p))
}

# Build the STATIC export intensity-line plot (ggplot + ggrepel). Mirrors the
# notebook layout: centered bold title "GENE (ACC)", centered subtitle
# "Mapped with N peptide(s)", two shared-y facets "Significant peptides (n)" |
# "Non-significant peptides (n)", one line per peptide-occurrence with end-of-
# line "aa<pos>" labels (the static analogue of the in-app hover tooltip).
#
# @param ld   a pelsa_intensity_line_data() frame (condition factor, mean_log2,
#   peptide_seq, pep_occurrence_idx, aa_label, panel).
# @param gene/accession  title tokens.
# @param log_base  intensity transform applied at setup (2 or 10) -> y label.
# @return a ggplot.
# @noRd
pelsa_intensity_export_ggplot <- function(ld, gene, accession, log_base = 2,
                                          coverage_frac = NA_real_) {
  conds <- levels(ld$condition)
  if (is.null(conds)) {
    conds <- unique(as.character(ld$condition))
    ld$condition <- factor(as.character(ld$condition), levels = conds)
  }
  ld$grp <- interaction(ld$peptide_seq, ld$pep_occurrence_idx, drop = TRUE)

  counts <- tapply(ld$grp, ld$panel, function(x) length(unique(x)))
  panel_lab <- function(p) sprintf("%s peptides (%d)", p, counts[[p]])
  lvl <- intersect(c("Significant", "Non-significant"), names(counts))
  ld$panel_f <- factor(vapply(as.character(ld$panel), panel_lab, character(1)),
                       levels = vapply(lvl, panel_lab, character(1)))

  # one label per line, anchored at the last condition that line reaches.
  lab_rows <- do.call(rbind, lapply(split(ld, ld$grp), function(d) {
    d <- d[order(match(as.character(d$condition), conds)), , drop = FALSE]
    d[nrow(d), , drop = FALSE]
  }))

  n_total <- length(unique(ld$grp))
  pep_txt <- sprintf("%d peptide%s", n_total, if (n_total == 1L) "" else "s")
  sub_txt <- if (is.finite(coverage_frac)) {
    sprintf("%s | %.1f%% sequence coverage", pep_txt, 100 * coverage_frac)
  } else {
    pep_txt
  }
  y_lab <- sprintf("Average log%d(intensity)", as.integer(log_base))

  ggplot2::ggplot(ld, ggplot2::aes(x = .data$condition, y = .data$mean_log2,
                                   group = .data$grp, color = .data$grp)) +
    ggplot2::geom_line(linewidth = 0.5, alpha = 0.9, na.rm = TRUE) +
    ggplot2::geom_point(size = 1.4, na.rm = TRUE) +
    ggrepel::geom_text_repel(
      data = lab_rows,
      ggplot2::aes(label = .data$aa_label, color = .data$grp),
      direction = "y", hjust = 0, nudge_x = 0.12, size = 2.6,
      segment.size = 0.25, segment.color = "grey70", min.segment.length = 0,
      box.padding = 0.1, max.overlaps = Inf, na.rm = TRUE,
      xlim = c(length(conds) + 0.02, NA)) +
    ggplot2::facet_wrap(~ .data$panel_f, nrow = 1, scales = "fixed") +
    ggplot2::scale_color_hue(guide = "none") +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0.3, 0.7))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(title = sprintf("%s (%s)", gene, accession),
                  subtitle = sub_txt, x = NULL, y = y_lab) +
    protigy_plot_theme(gridlines = TRUE) +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5,
                                            color = "grey25"),
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1,
                                          colour = "black", face = "bold"),
      strip.text  = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_rect(fill = "grey92", color = NA),
      panel.spacing = ggplot2::unit(1.4, "lines"),
      # Reserve a wider left gutter so a long rotated leftmost condition label
      # (e.g. "AY9944_U18666A_DMSO") is not clipped off the panel edge.
      plot.margin = ggplot2::margin(t = 5.5, r = 5.5, b = 5.5, l = 40))
}

# Re-derive a volcano df for export (all_peptide / best_peptide), from plain
# inputs (no Shiny). Returns NULL when stats/cache/contrast are missing so the
# export caller no-ops gracefully. Mirrors the on-screen df build.
#
# @param stat_raw  stat_results()[[ome]], or NULL.
# @param matched   the cache $matched frame, or NULL.
# @param feat_df   the species feature table, or NULL (-> "none" coloring).
# @param markers   marker accessions.
# @param contrast  the contrast suffix, or NULL.
# @param panel     "all_peptide" | "best_peptide".
# @param sig_cutoff the adj.P significance threshold (drives Significant /
#                  sig_direction and the empirical y_cutoff dashed line).
# @param is_self_curated TRUE for a self-curated species: forces accession labels
#                  + blanks the gene, so the exported figure matches the on-screen
#                  volcano (the export is a SEPARATE re-derive of the same df).
# @param .stat_df  optional precomputed pelsa_volcano_stat_df(stat_raw, matched)
#                  result, reused across a contrast/panel export loop instead of
#                  recomputing it on every call (its output is contrast/panel-
#                  invariant). MUST be built from the exact same stat_raw/matched
#                  passed to this call, or the two paths silently diverge. NULL
#                  (default) recomputes internally, matching the original behavior.
# @return a 3A volcano df, or NULL.
# @noRd
pelsa_volcano_export_df <- function(stat_raw, matched, feat_df, markers,
                                    contrast, panel,
                                    sig_cutoff = .PELSA_EXPORT_SIG_CUTOFF,
                                    is_self_curated = FALSE,
                                    sig_stat = "adj.p.val",
                                    .stat_df = NULL) {
  if (!is.data.frame(stat_raw) || nrow(stat_raw) == 0L) return(NULL)
  if (is.null(contrast) ||
      !pelsa_volcano_has_contrast(stat_raw, contrast)) return(NULL)
  matched <- if (is.data.frame(matched)) matched else data.frame()
  fdf <- feat_df %||% data.frame(accession = character(0), start = integer(0),
                                 end = integer(0), feature_class = character(0))
  # `.stat_df` (when supplied by the export loop) is the SAME
  # pelsa_volcano_stat_df(stat_raw, matched) result, precomputed once and reused
  # across contrasts/panels. When NULL we recompute here -- this fallback path
  # must stay in sync with export_volcano's pre-loop guard (see tab_pelsa_section3.R):
  # both rely on safe_export's outer tryCatch as the real safety net for a
  # malformed stat_raw.
  stat_df <- .stat_df %||% pelsa_volcano_stat_df(stat_raw, matched)
  tryCatch(
    pelsa_build_volcano_df(
      stat_df = stat_df,
      matched_cache = if (nrow(matched) > 0L) matched else
        pelsa_volcano_empty_matched(),
      feat_df = fdf, markers = markers, contrast = contrast,
      opts = list(panel = panel, sig_cutoff = sig_cutoff, sig_stat = sig_stat),
      is_self_curated = is_self_curated
    ),
    error = function(e) NULL
  )
}

# Combine the 3C per-protein intensity line data for ALL plotted proteins into
# one tidy frame (the plotted_intensities.csv body). Pure: a function of its
# inputs; no Shiny. Returns NULL when any required input is missing/empty or no
# protein qualifies, so the export caller can no-op gracefully.
#
# @param stat_raw  stat_results()[[ome]] (per-peptide, contrast-suffixed).
# @param matched   the cache $matched frame (peptide x accession x occurrence).
# @param markers   marker accessions (Setup).
# @param contrast  the contrast suffix.
# @param pm        the processed/log2 GCT matrix, or NULL.
# @param cmap      sample -> condition map (named char), or NULL.
# @param corder    condition order (factor levels), or NULL/empty.
# @param sig_cutoff significance threshold on adj.P.Val. Defaults to the export
#   constant; callers in the module thread the SHARED cutoff (isolate(sig_cutoff_r()))
#   so this export matches the on-screen volcano/intensity views rather than a
#   hardcoded 0.05.
# @return tidy long data.frame (rbind of pelsa_intensity_line_data over the
#   pelsa_intensity_proteins set), or NULL.
# @noRd
pelsa_plotted_intensities_df <- function(stat_raw, matched, markers, contrast,
                                         pm, cmap, corder,
                                         sig_cutoff = .PELSA_EXPORT_SIG_CUTOFF) {
  if (!is.data.frame(stat_raw) || nrow(stat_raw) == 0L) return(NULL)
  if (!is.data.frame(matched) || nrow(matched) == 0L) return(NULL)
  if (is.null(contrast) || is.null(pm) || is.null(cmap) ||
      length(corder) == 0L) {
    return(NULL)
  }
  stat_df <- pelsa_volcano_stat_df(stat_raw, matched)
  prot <- pelsa_intensity_proteins(stat_df, matched, markers, contrast,
                                   sig_cutoff = sig_cutoff)
  if (nrow(prot) == 0L) return(NULL)
  rows <- lapply(seq_len(nrow(prot)), function(i) {
    tryCatch(
      pelsa_intensity_line_data(
        accession = prot$accession[i], stat_df = stat_df,
        matched_cache = matched, processed_mat = pm,
        condition_map = cmap, condition_order = corder,
        contrast = contrast, sig_cutoff = sig_cutoff,
        is_marker = prot$is_marker[i]
      ),
      error = function(e) NULL
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) return(NULL)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Build the per-point legend category + the manual color scale for a color mode.
# significance: the 3 fixed direction buckets, but "Non-significant" is EXCLUDED
# from the legend (breaks) - non-significant points still render in their gray
# color, they just never get their own legend key, regardless of how many rows
# fall into that bucket. feature: the 9 UniProt classes (always all listed,
# mirroring the Woods feature legend). Returns the factor category column for
# the background rows + a named values vector for the scale + the legend breaks.
# @noRd
.pelsa_export_color_spec <- function(bg, color_mode) {
  if (identical(color_mode, "feature")) {
    keys   <- names(PELSA_FEATURE_COLORS)
    labels <- unname(.PELSA_FEATURE_LABELS[keys])
    values <- stats::setNames(unname(PELSA_FEATURE_COLORS[keys]), labels)
    raw    <- as.character(bg$feature_class_primary)
    cat    <- factor(unname(.PELSA_FEATURE_LABELS[raw]), levels = labels)
    list(category = cat, values = values, breaks = labels,
         method = "feature coloring")
  } else {
    labels <- unname(.PELSA_EXPORT_SIG_LABELS[c("down", "ns", "up")])
    values <- stats::setNames(
      c(.PELSA_SIG_COLOR_DOWN, .PELSA_SIG_COLOR_NS, .PELSA_SIG_COLOR_UP), labels)
    raw    <- as.character(bg$sig_direction)
    cat    <- factor(unname(.PELSA_EXPORT_SIG_LABELS[raw]), levels = labels)
    breaks <- setdiff(labels, .PELSA_EXPORT_SIG_LABELS[["ns"]])
    list(category = cat, values = values, breaks = breaks,
         method = "significance coloring")
  }
}

# Build the static export ggplot (mirrors pelsa_volcano_build_plot's geom layout
# but returns a plain ggplot for the PDF device - no plotly / WebGL / browser).
# Color/fill are mapped INSIDE aes() so the figure carries a legend: the chosen
# color mode's categories (significance buckets or UniProt feature classes) plus
# a separate magenta "Marker" entry. A title (the contrast) and subtitle
# (<volcano type> | <coloring method>) are added when supplied.
# @param contrast       the contrast suffix, used for the title (NULL -> none).
# @param volcano_label  e.g. "All-peptide volcano" -> the subtitle prefix.
# @param sig_cutoff     the adj.P significance threshold; drives the dashed-line
#                       annotation text so it always matches the cutoff the df
#                       was built with (single source of truth, no drift).
# @noRd
.pelsa_export_ggplot <- function(df, full_df, color_mode = "significance",
                                 label_mode = character(0),
                                 n_top_adjp = 3L,
                                 n_top_markers = 3L,
                                 contrast = NULL, volcano_label = NULL,
                                 sig_cutoff = .PELSA_EXPORT_SIG_CUTOFF) {
  color_mode <- color_mode %||% "significance"
  split <- pelsa_volcano_marker_split(df)
  bg <- split$background
  mk <- split$markers
  spec <- .pelsa_export_color_spec(bg, color_mode)

  # The background layer is added even when bg is empty (0 rows -> an empty
  # layer) so the color aesthetic + its scale always exist and the direction
  # legend keys always render, regardless of how many background peptides are
  # in this view (mirrors the marker-layer handling below). show.legend must be
  # forced per-aesthetic: ggplot2 >= 3.5's guide_legend() only draws a key's
  # colored glyph when that break's value is present in the LAYER's data, so a
  # view with e.g. zero "up"/"down" rows (all non-significant) would otherwise
  # show the "Downregulated"/"Upregulated" legend TEXT with no visible dot.
  bg$legend_cat <- spec$category
  gg <- ggplot2::ggplot() + ggplot2::geom_point(
    data = bg, ggplot2::aes(x = .data$logFC, y = .data$logP,
                            color = .data$legend_cat),
    alpha = .PELSA_VOLCANO_BG_ALPHA, size = 1, show.legend = c(colour = TRUE))
  y_cut <- attr(full_df, "y_cutoff")
  if (!is.null(y_cut) && is.finite(y_cut)) {
    gg <- gg + ggplot2::geom_hline(yintercept = y_cut, linetype = "dashed",
                                   color = "grey40")
    # cutoff annotation: small + bold, flush to the right panel edge, just below
    # the dashed line. Label derives from sig_cutoff so it stays consistent with
    # the threshold the df was built with.
    gg <- gg + ggplot2::annotate(
      "text", x = Inf, y = y_cut,
      label = paste0("adj.P < ", format(sig_cutoff, scientific = FALSE,
                                        trim = TRUE)),
      hjust = 1.15, vjust = 1.5, size = 2, fontface = "bold",
      color = "grey30")
  }
  # The marker layer is added even when mk is empty (0 rows -> an empty layer)
  # so the fill aesthetic + its scale always exist and the "Marker" legend key
  # always renders, regardless of how many marker peptides are in this view.
  # show.legend forced for the same reason as the background layer above.
  gg <- gg + ggplot2::geom_point(
    data = mk, ggplot2::aes(x = .data$logFC, y = .data$logP, fill = "Marker"),
    shape = 21, size = 1, stroke = 0.5, color = .PELSA_VOLCANO_MARKER_EDGE,
    show.legend = c(fill = TRUE))
  # Bake peptide labels per the in-app label mode (the on-screen labels are
  # plotly annotations; the static export draws them as repelled boxed labels:
  # white box, black outline + text, black segment; force=20 to spread them).
  if (length(label_mode) > 0L && "label" %in% colnames(df)) {
    idx <- tryCatch(
      pelsa_volcano_label_rows(df, mode = label_mode,
                               n_top_adjp = n_top_adjp,
                               n_top_markers = n_top_markers),
      error = function(e) integer(0))
    if (length(idx) > 0L) {
      lab <- df[idx, , drop = FALSE]
      lab <- lab[!is.na(lab$label) & nzchar(lab$label), , drop = FALSE]
      if (nrow(lab) > 0L) {
        gg <- gg + ggrepel::geom_label_repel(
          data = lab,
          ggplot2::aes(x = .data$logFC, y = .data$logP, label = .data$label),
          size = 2, force = 20, max.overlaps = Inf,
          fill = "white", color = "black",
          label.size = 0.3, label.padding = 0.18,
          min.segment.length = 0, segment.size = 0.3, segment.color = "black")
      }
    }
  }

  title_txt <- if (is.null(contrast)) NULL else
    gsub("_over_", " vs ", contrast, fixed = TRUE)
  # Capitalize the first letter of the coloring-method word so the subtitle reads
  # "<volcano type> | Significance coloring" / "... | Feature coloring".
  method_cap <- sub("^(.)", "\\U\\1", spec$method, perl = TRUE)
  subtitle_txt <- if (is.null(volcano_label)) method_cap else
    paste0(volcano_label, " | ", method_cap)

  gg +
    ggplot2::scale_color_manual(name = NULL, values = spec$values,
                                breaks = spec$breaks, drop = FALSE,
                                limits = names(spec$values)) +
    ggplot2::scale_fill_manual(name = NULL,
                               values = c("Marker" = .PELSA_VOLCANO_MARKER_COLOR),
                               breaks = "Marker", limits = "Marker") +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        order = 1, override.aes = list(size = 2, alpha = 1)),
      fill  = ggplot2::guide_legend(
        order = 2,
        override.aes = list(shape = 21, size = 2,
                            color = .PELSA_VOLCANO_MARKER_EDGE))) +
    ggplot2::labs(x = "logFC", y = "-log10(P.Value)",
                  title = title_txt, subtitle = subtitle_txt) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title    = ggplot2::element_text(face = "bold", size = 14,
                                            hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, color = "grey30",
                                            hjust = 0.5),
      axis.title = ggplot2::element_text(size = 12, face = "bold"),
      axis.text  = ggplot2::element_text(size = 10),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "right",
      legend.title  = ggplot2::element_blank(),
      legend.text   = ggplot2::element_text(size = 8),
      legend.key    = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(9, "pt"),
      legend.spacing.y = ggplot2::unit(2, "pt"),
      legend.margin = ggplot2::margin(2, 4, 2, 4),
      legend.box.spacing = ggplot2::unit(4, "pt"),
      legend.box.background = ggplot2::element_rect(color = "black", fill = NA,
                                                    linewidth = 0.2),
      legend.box.margin = ggplot2::margin(2, 2, 2, 2))
}
