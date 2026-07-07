################################################################################
# Module: PELSA Woods plot - STATIC export builder + click-index resolution
# (pure data + plot helpers; split out of tab_pelsa_panel_helpers.R to stay
# under the 800-line file cap -- see that file's header for the shared
# 3-track panel design this static export mirrors).
#
# Public helpers (all @noRd):
#   pelsa_woods_export_ggplot(peptides, features, prot_len, gene, accession,
#       contrast, sig_cutoff) -> single-panel static export ggplot (peptide
#       segments + lane-packed feature band + legend), used by the PELSA
#       export pipeline (tab_pelsa_export_helpers.R).
#   .pelsa_woods_click_index(pep, ev_x, ev_y) -> resolves a plotly_click event
#       on the interactive Woods track to a peptide row index (used by
#       tab_pelsa_section3.R).
################################################################################

# ---- Helper 6: STATIC export Woods plot (single-panel ggplot) ----------------

# Greedy lane packing fallback (base R; mirrors IRanges::disjointBins) so the
# export builder works on plain feature frames without the S4 import. @noRd
.pelsa_pack_lanes <- function(starts, ends) {
  n <- length(starts)
  if (n == 0L) return(integer(0))
  ord <- order(starts, ends)
  lane_end <- numeric(0)
  lanes <- integer(n)
  for (i in ord) {
    placed <- FALSE
    for (l in seq_along(lane_end)) {
      if (starts[i] > lane_end[l]) {
        lanes[i] <- l; lane_end[l] <- ends[i]; placed <- TRUE; break
      }
    }
    if (!placed) { lane_end <- c(lane_end, ends[i]); lanes[i] <- length(lane_end) }
  }
  lanes
}

# Build the STATIC export Woods plot: a single-panel ggplot (NOT the interactive
# 3-track subplot). Peptide segments sit at y = logFC over [pep_start, pep_end],
# colored by -log10(adj.P) (grey80 -> #B2182B, capped); significant peptides
# (adj.P < sig_cutoff) get a small "*" at the segment center. ALL UniProt
# features for the protein render as a lane-packed, box-outlined band below the
# data, and the feature legend always shows all PELSA_FEATURE_COLORS classes.
#
# @param peptides  pelsa_woods_peptide_data() output (peptide_seq, pep_start,
#                  pep_end, logFC, adj.P.Val, sig).
# @param features  raw UniProt feature rows for the accession (start, end,
#                  feature_class). Any subset of classes; all are plotted.
# @param prot_len  protein length (x extent).
# @param gene/accession  title tokens.
# @param contrast  contrast label for the subtitle.
# @param sig_cutoff adj.P significance threshold (default .PELSA_EXPORT_SIG_CUTOFF).
# @return a ggplot.
# @noRd
pelsa_woods_export_ggplot <- function(peptides, features, prot_len, gene,
                                      accession, contrast,
                                      sig_cutoff = .PELSA_EXPORT_SIG_CUTOFF,
                                      sig_stat = "adj.p.val") {
  prot_len <- max(1L, as.integer(prot_len))
  # Human-readable contrast for the subtitle, matching the volcano export
  # (pelsa_volcano_export_df): "DMSO_over_VEH" -> "DMSO vs VEH".
  contrast_disp <- gsub("_over_", " vs ", contrast, fixed = TRUE)
  pk <- if (is.data.frame(peptides)) peptides else data.frame()
  if (nrow(pk) > 0L) {
    pk$.lfc  <- as.numeric(pk$logFC)
    pk$.adjp <- as.numeric(pk$adj.P.Val)
    pk <- pk[is.finite(pk$.lfc), , drop = FALSE]
  }
  title <- if (nzchar(gene)) sprintf("%s (%s), %d aa", gene, accession, prot_len)
           else sprintf("%s, %d aa", accession, prot_len)
  base_theme <- ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", hjust = 0),
      plot.subtitle = ggplot2::element_text(face = "italic", hjust = 0,
                                            color = "grey25"),
      plot.caption  = ggplot2::element_text(hjust = 0, size = 8, color = "grey30"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.key.height = ggplot2::unit(0.8, "lines"),
      legend.title  = ggplot2::element_text(size = 9),
      legend.text   = ggplot2::element_text(size = 7.5),
      legend.background = ggplot2::element_rect(color = "black", fill = NA,
                                               linewidth = 0.3),
      legend.margin = ggplot2::margin(4, 6, 4, 6),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  if (nrow(pk) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = prot_len / 2, y = 0,
                          label = "no mapped peptides", color = "grey50") +
        ggplot2::scale_x_continuous(
          limits = c(1, prot_len),
          # always keep the protein end as a break so short proteins (< 20 aa,
          # where seq(0, len, 20) is just {0} and 0 sits below the axis) are not
          # left with a bare, label-less x-axis.
          breaks = unique(c(seq(0L, prot_len, by = 20L), prot_len))) +
        ggplot2::labs(title = title,
                      subtitle = sprintf("Wood's plot: %s", contrast_disp),
                      x = "Residue position", y = "log2FC") +
        base_theme)
  }

  # Color gradient encodes the SHARED significance statistic: -log10 of the raw
  # P.Value for "nom.p.val" (when present), else -log10(adj.P.Val). Falls back to
  # adj.P.Val when the raw-p column is absent so older frames still color.
  use_nom <- identical(sig_stat, "nom.p.val") && "P.Value" %in% colnames(pk)
  pcol <- if (use_nom) as.numeric(pk$P.Value) else pk$.adjp
  p_label <- if (use_nom) "nom.P" else "adj.P"  # legend + caption stat label
  pk$neglogp <- pmin(-log10(pmax(pcol, .Machine$double.xmin)),
                     .PELSA_WOODS_NEGLOG_CAP)
  pk$neglogp[is.na(pcol)] <- 0
  # The "*" significance markers follow the SHARED stat choice. Prefer the
  # authoritative `sig` column computed upstream by pelsa_woods_peptide_data()
  # (sig_stat-aware); fall back to recomputing on adj.P.Val if it is absent.
  pk$is_sig <- if ("sig" %in% colnames(pk)) {
    isTRUE_vec <- pk$sig %in% TRUE
    isTRUE_vec
  } else {
    !is.na(pk$.adjp) & pk$.adjp < sig_cutoff
  }

  # ALL features, lane-packed.
  feats <- if (is.data.frame(features)) features else data.frame()
  has_feats <- nrow(feats) > 0L &&
    all(c("start", "end", "feature_class") %in% colnames(feats))
  any_widened <- FALSE
  if (has_feats) {
    feats <- feats[, c("start", "end", "feature_class")]
    feats$start <- as.integer(feats$start)
    feats$end   <- as.integer(feats$end)
    feats <- feats[is.finite(feats$start) & is.finite(feats$end) &
                     feats$end >= feats$start, , drop = FALSE]
  }
  has_feats <- is.data.frame(feats) && nrow(feats) > 0L
  if (has_feats) {
    feats <- pelsa_widen_point_features(feats, prot_len = prot_len)
    any_widened <- any(feats$was_widened)
    feats$feature_class <- factor(as.character(feats$feature_class),
                                  levels = names(PELSA_FEATURE_COLORS))
    feats$lane <- .pelsa_pack_lanes(feats$display_start, feats$display_end)
    n_lane <- max(1L, suppressWarnings(max(feats$lane, na.rm = TRUE)))
  } else {
    n_lane <- 1L
  }

  y_lo <- min(pk$.lfc, 0, na.rm = TRUE)
  y_hi <- max(pk$.lfc, 0, na.rm = TRUE)
  yr   <- max(y_hi - y_lo, 1e-6)
  band_gap <- yr * 0.10
  lane_h   <- yr * 0.06
  pad      <- lane_h * 0.30
  feat_top <- y_lo - band_gap
  content_top <- feat_top - pad
  if (has_feats) {
    feats$ymax <- content_top - (feats$lane - 1L) * lane_h
    feats$ymin <- feats$ymax - lane_h * 0.82
    feat_bot <- min(feats$ymin) - pad
  } else {
    feat_bot <- feat_top - lane_h - 2 * pad
  }

  intervals <- pelsa_coverage_intervals(pk$pep_start, pk$pep_end)
  cov_aa  <- if (nrow(intervals) > 0L)
    sum(intervals$end - intervals$start + 1L) else 0L
  cov_pct <- 100 * cov_aa / prot_len
  cov_txt <- sprintf("Coverage: %.1f%% (%d aa, %d pep)", cov_pct, cov_aa, nrow(pk))

  dummy <- data.frame(
    feature_class = factor(names(PELSA_FEATURE_COLORS),
                           levels = names(PELSA_FEATURE_COLORS)),
    stringsAsFactors = FALSE)

  gg <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "grey70") +
    ggplot2::geom_rect(
      data = dummy,
      ggplot2::aes(xmin = 1, xmax = 1, ymin = feat_bot, ymax = feat_bot,
                   fill = .data$feature_class), na.rm = TRUE) +
    ggplot2::annotate("rect", xmin = 1, xmax = prot_len, ymin = feat_bot,
                      ymax = feat_top, fill = NA, color = "grey40",
                      linewidth = 0.4)
  if (has_feats) {
    gg <- gg + ggplot2::geom_rect(
      data = feats,
      ggplot2::aes(xmin = .data$display_start, xmax = .data$display_end,
                   ymin = .data$ymin, ymax = .data$ymax,
                   fill = .data$feature_class))
  }
  gg <- gg +
    ggplot2::geom_segment(
      data = pk,
      ggplot2::aes(x = .data$pep_start, xend = .data$pep_end, y = .data$.lfc,
                   yend = .data$.lfc, color = .data$neglogp),
      linewidth = 2.4, lineend = "butt") +
    ggplot2::geom_point(
      data = pk[pk$is_sig, , drop = FALSE],
      ggplot2::aes(x = (.data$pep_start + .data$pep_end) / 2, y = .data$.lfc),
      shape = 8, size = 1.1, stroke = 0.4, color = "black") +
    ggplot2::annotate("text", x = 1, y = Inf, label = cov_txt, hjust = 0,
                      vjust = 1.6, color = "grey45", fontface = "bold", size = 3.4) +
    ggplot2::scale_fill_manual(
      values = PELSA_FEATURE_COLORS, limits = names(PELSA_FEATURE_COLORS),
      labels = .PELSA_FEATURE_LABELS, drop = FALSE, name = "UniProt feature",
      guide = ggplot2::guide_legend(order = 1)) +
    ggplot2::scale_color_gradient(
      low = "grey80", high = "#B2182B", limits = c(0, .PELSA_WOODS_NEGLOG_CAP),
      name = sprintf("-log10(%s)", p_label),
      guide = ggplot2::guide_colourbar(order = 2)) +
    ggplot2::scale_x_continuous(
      limits = c(1, prot_len), expand = ggplot2::expansion(mult = 0.01),
      # always keep the protein end as a break (see empty-data path above).
      breaks = unique(c(seq(0L, prot_len, by = 20L), prot_len))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.04, 0.16))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = title, subtitle = sprintf("Wood's plot: %s", contrast_disp),
      caption = paste0(
        sprintf("*Significant peptides (%s < %s)", p_label, format(sig_cutoff)),
        if (any_widened)
          sprintf("; single-residue features widened +-%d aa for visibility",
                  .PELSA_WOODS_POINT_PAD)
        else ""),
      x = "Residue position", y = "log2FC") +
    base_theme
  gg
}
# Resolve which Woods peptide a plotly_click selected, by coordinate.
#
# Candidates are the peptides whose [pep_start, pep_end] span contains the click
# x; if the click x is NULL/NA or in no span, ALL peptides are candidates. Among
# the candidates, pick the one whose logFC is nearest the click y. A NULL OR NA
# click y falls back to each candidate's own logFC (distance 0), so the first
# candidate is chosen -- never letting an NA y collapse which.min() to
# integer(0) (the bug this guards).
#
# @param pep   data.frame with pep_start, pep_end, logFC (the Woods peptide set).
# @param ev_x  click x coordinate (numeric scalar) or NULL.
# @param ev_y  click y coordinate (numeric scalar), or NULL/NA.
# @return integer row index into `pep` (length 1), or NULL when pep is empty.
# @noRd
.pelsa_woods_click_index <- function(pep, ev_x, ev_y) {
  n <- nrow(pep)
  if (is.null(n) || n == 0L) return(NULL)
  in_span <- if (is.null(ev_x) || length(ev_x) != 1L || is.na(ev_x)) {
    rep(FALSE, n)
  } else {
    pep$pep_start <= ev_x & ev_x <= pep$pep_end
  }
  cand <- which(in_span)
  if (!length(cand)) cand <- seq_len(n)
  # NA or NULL y -> use each candidate's own logFC (distance 0 everywhere).
  y_ref <- if (is.null(ev_y) || length(ev_y) != 1L || is.na(ev_y)) {
    pep$logFC[cand]
  } else {
    ev_y
  }
  j <- cand[which.min(abs(pep$logFC[cand] - y_ref))]
  # which.min returns integer(0) when every candidate distance is NA (e.g. all
  # candidate logFC are NA); honor the "length-1 or NULL" contract so the caller
  # never indexes pep with integer(0).
  if (!length(j)) return(NULL)
  j
}
