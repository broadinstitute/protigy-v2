#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# PELSA Woods-plot STATIC EXPORT prototype (single-panel, ggplot2).
#
# Goal: reproduce the notebook log2FC Wood's-plot LAYOUT (image #1) but with
#   (a) peptide segments colored by a -log10(adj.P) gradient (in-app convention,
#       grey92 -> #B2182B, capped at 5) instead of binary significant/not, and
#   (b) the UniProt feature legend showing ALL feature classes (app schema),
#       even those absent from this protein.
#
# Self-contained: synthetic data approximating FKBP2 (P26885), 142 aa, 6 peptides.
# Run:  Rscript dev/pelsa_export_prototypes/woods_export_prototype.R
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2)
})

# ---- App constants (mirrored from R/tab_pelsa_annotation_helpers.R + woods) --
PELSA_FEATURE_COLORS <- c(
  active_or_binding_site     = "#1f77b4",
  catalytic_domain           = "#ff7f0e",
  folded_domain              = "#d62728",
  region_or_motif            = "#9467bd",
  transmembrane_or_signal    = "#2ca02c",
  repeat_or_coiled_coil      = "#8c564b",
  low_complexity_or_disorder = "#7f7f7f",
  other                      = "#bcbd22",
  none                       = "#d3d3d3"
)
PELSA_FEATURE_LABELS <- c(
  active_or_binding_site     = "active / binding site",
  catalytic_domain           = "catalytic domain",
  folded_domain              = "folded domain",
  region_or_motif            = "region / motif",
  transmembrane_or_signal    = "transmembrane / signal",
  repeat_or_coiled_coil      = "repeat / coiled coil",
  low_complexity_or_disorder = "low complexity / disorder",
  other                      = "other",
  none                       = "none / unannotated"
)
WOODS_NEGLOG_CAP <- 5
WOODS_SIG_CUTOFF <- 0.05            # adj.P threshold for the significance marker

# ---- Covered-residue union (base R; mirrors IRanges::reduce intent) ----------
covered_residues <- function(starts, ends, prot_len) {
  ok <- !is.na(starts) & !is.na(ends) & ends >= starts
  starts <- pmax(1L, as.integer(starts[ok]))
  ends   <- pmin(as.integer(prot_len), as.integer(ends[ok]))
  if (!length(starts)) return(0L)
  covered <- logical(prot_len)
  for (i in seq_along(starts)) covered[starts[i]:ends[i]] <- TRUE
  sum(covered)
}

# ---- Greedy lane packing (base R; mirrors IRanges::disjointBins) -------------
pack_lanes <- function(starts, ends) {
  n <- length(starts)
  if (!n) return(integer(0))
  ord <- order(starts, ends)
  lane_end <- numeric(0)              # last occupied end per lane
  lanes <- integer(n)
  for (i in ord) {
    placed <- FALSE
    for (l in seq_along(lane_end)) {
      if (starts[i] > lane_end[l]) { lanes[i] <- l; lane_end[l] <- ends[i]; placed <- TRUE; break }
    }
    if (!placed) { lane_end <- c(lane_end, ends[i]); lanes[i] <- length(lane_end) }
  }
  lanes
}

# ---- The export builder (prototype of pelsa_woods_export_ggplot) -------------
pelsa_woods_export_ggplot <- function(peptides, features, prot_len,
                                      gene, accession, contrast) {
  prot_len <- max(1L, as.integer(prot_len))

  # peptide color = -log10(adj.P), capped; NA adj.P -> 0 (light grey).
  pk <- peptides
  pk$neglogp <- pmin(-log10(pmax(pk$adj_p, .Machine$double.xmin)), WOODS_NEGLOG_CAP)
  pk$neglogp[is.na(pk$adj_p)] <- 0
  pk$is_sig <- !is.na(pk$adj_p) & pk$adj_p < WOODS_SIG_CUTOFF

  # ALL UniProt features for this protein are plotted, lane-packed so overlapping
  # ones stack (greedy min-lane; mirrors IRanges::disjointBins in the app).
  feats <- features
  feats$feature_class <- factor(as.character(feats$feature_class),
                                levels = names(PELSA_FEATURE_COLORS))
  feats$lane <- pack_lanes(feats$start, feats$end)
  n_lane <- max(1L, suppressWarnings(max(feats$lane, na.rm = TRUE)))

  # y extent from peptide effect sizes; reserve a feature band BELOW the data,
  # tall enough to hold every lane. A box outline frames the band so it reads as
  # a separate track from the peptide panel.
  y_lo <- min(pk$log2fc, 0, na.rm = TRUE)
  y_hi <- max(pk$log2fc, 0, na.rm = TRUE)
  yr   <- max(y_hi - y_lo, 1e-6)
  band_gap <- yr * 0.10                    # gap between lowest peptide and band
  lane_h   <- yr * 0.06                    # per-lane thickness
  pad      <- lane_h * 0.30                # equal top & bottom margin inside box
  feat_top <- y_lo - band_gap              # box top edge
  # per-feature y-extents (lane 1 just below the top margin)
  content_top <- feat_top - pad
  feats$ymax <- content_top - (feats$lane - 1L) * lane_h
  feats$ymin <- feats$ymax - lane_h * 0.82 # inter-lane padding
  feat_bot <- min(feats$ymin) - pad        # box bottom: same margin as the top

  # coverage annotation text (placed in TOP headroom, grey).
  cov_aa  <- covered_residues(pk$pep_start, pk$pep_end, prot_len)
  cov_pct <- 100 * cov_aa / prot_len
  cov_txt <- sprintf("Coverage: %.1f%% (%d aa, %d pep)",
                     cov_pct, cov_aa, nrow(pk))

  # Dummy zero-area rects (one per class) guarantee every class appears in the
  # legend WITH its color, even when absent from this protein.
  dummy <- data.frame(
    feature_class = factor(names(PELSA_FEATURE_COLORS),
                           levels = names(PELSA_FEATURE_COLORS)),
    stringsAsFactors = FALSE)

  gg <- ggplot() +
    geom_hline(yintercept = 0, linewidth = 0.3, color = "grey70") +
    # invisible all-class layer -> complete colored legend
    geom_rect(
      data = dummy,
      aes(xmin = 1, xmax = 1, ymin = feat_bot, ymax = feat_bot,
          fill = feature_class), na.rm = TRUE) +
    # box outline framing the feature track (full width)
    annotate("rect", xmin = 1, xmax = prot_len, ymin = feat_bot, ymax = feat_top,
             fill = NA, color = "grey40", linewidth = 0.4) +
    # feature segments (discrete fill), lane-packed
    geom_rect(
      data = feats,
      aes(xmin = start, xmax = end, ymin = ymin, ymax = ymax,
          fill = feature_class)) +
    # peptide segments at y = log2FC, colored by -log10(adj.P)
    geom_segment(
      data = pk,
      aes(x = pep_start, xend = pep_end, y = log2fc, yend = log2fc,
          color = neglogp),
      linewidth = 2.4, lineend = "butt") +
    # significant peptides (adj.P < cutoff): small "*" at the segment center
    geom_point(
      data = pk[pk$is_sig, , drop = FALSE],
      aes(x = (pep_start + pep_end) / 2, y = log2fc),
      shape = 8, size = 1.1, stroke = 0.4, color = "black") +
    # coverage annotation in the top headroom (avoids overlap with peptides)
    annotate("text", x = 1, y = Inf, label = cov_txt,
             hjust = 0, vjust = 1.6, color = "grey45", fontface = "bold",
             size = 3.4) +
    scale_fill_manual(
      values = PELSA_FEATURE_COLORS, limits = names(PELSA_FEATURE_COLORS),
      labels = PELSA_FEATURE_LABELS, drop = FALSE, name = "UniProt feature",
      guide = guide_legend(order = 1)) +
    scale_color_gradient(
      low = "grey80", high = "#B2182B", limits = c(0, WOODS_NEGLOG_CAP),
      name = "-log10(adj.P)", guide = guide_colourbar(order = 2)) +
    scale_x_continuous(limits = c(1, prot_len), expand = expansion(mult = 0.01),
                       breaks = seq(0L, prot_len, by = 10L)) +
    # extra TOP headroom so the coverage label clears the near-zero peptides
    scale_y_continuous(expand = expansion(mult = c(0.04, 0.16))) +
    coord_cartesian(clip = "off") +
    labs(
      title = if (nzchar(gene)) sprintf("%s (%s), %d aa", gene, accession, prot_len)
              else sprintf("%s, %d aa", accession, prot_len),
      subtitle = sprintf("Wood's plot: %s", contrast),
      caption = sprintf("*Significant peptides (adj.P < %s)",
                        format(WOODS_SIG_CUTOFF)),
      x = "Residue position", y = "log2FC") +
    theme_bw(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", hjust = 0),
      plot.subtitle = element_text(face = "italic", hjust = 0, color = "grey25"),
      plot.caption  = element_text(hjust = 0, size = 8, color = "grey30"),
      panel.grid.minor = element_blank(),
      legend.key.height = unit(0.8, "lines"),
      legend.title  = element_text(size = 9),
      legend.text   = element_text(size = 7.5),
      # box each legend panel with a black outline
      legend.background = element_rect(color = "black", fill = NA, linewidth = 0.3),
      legend.margin = margin(4, 6, 4, 6))
  gg
}

# ---- Synthetic FKBP2 data (approximates image #1) ----------------------------
peptides <- data.frame(
  peptide_seq = paste0("PEP", 1:6),
  pep_start   = c(30L, 31L, 32L, 116L, 104L, 104L),
  pep_end     = c(36L, 37L, 38L, 135L, 115L, 115L),
  log2fc      = c(0.08, 0.00, -0.07, -1.25, -2.40, -3.10),
  adj_p       = c(0.80, 0.90, 0.70, 0.40, 0.001, 0.20),
  stringsAsFactors = FALSE
)
# Several features (incl. overlapping ones) to demonstrate lane-packing + the
# track box. In the app this is the full UniProt feature set for the accession.
features <- data.frame(
  feature_class = c("transmembrane_or_signal", "folded_domain",
                    "region_or_motif", "active_or_binding_site"),
  start = c(1L,  35L, 60L,  90L),
  end   = c(21L, 110L, 80L, 96L),
  stringsAsFactors = FALSE
)

p <- pelsa_woods_export_ggplot(
  peptides, features, prot_len = 142L,
  gene = "FKBP2", accession = "P26885",
  contrast = "rapamycin_1uM_over_DMSO")

out <- file.path("dev", "woods_export_example.png")
ragg::agg_png(out, width = 9, height = 4.2, units = "in", res = 200)
print(p)
invisible(dev.off())
cat("saved", out, "\n")
