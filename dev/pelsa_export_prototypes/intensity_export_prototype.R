#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# PELSA intensity-line STATIC EXPORT prototype (ggplot2 + ggrepel).
#
# Goal: reproduce the notebook intensity-line LAYOUT (image #2):
#   bold suptitle "GENE (ACC) - N peptide(s)", two shared-y panels
#   "Significant peptides (n)" | "Non-significant peptides (n)", one line per
#   peptide, end-of-line "aa<pos>" labels, x = Condition (given order),
#   y = "mean log2 intensity (processed GCT)".
#
# Difference from the in-app plot: the app drops the legend and relies on the
# interactive hover tooltip. A STATIC export has no hover, so we restore the
# notebook's end-of-line aa-position labels (ggrepel, vertical-only).
#
# Self-contained: synthetic data approximating FKBP3 (Q00688), 26 peptides,
# 8 significant + 18 non-significant, 3 conditions.
# Run:  Rscript dev/pelsa_export_prototypes/intensity_export_prototype.R
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggrepel)
})

# ---- The export builder (prototype of pelsa_intensity_export_ggplot) ---------
# ld columns: condition (factor, x order), mean_log2, peptide_seq, occ,
#             aa_label, panel ("Significant"/"Non-significant").
pelsa_intensity_export_ggplot <- function(ld, gene, accession, log_base = 2) {
  conds <- levels(ld$condition)
  last_cond <- conds[length(conds)]

  # panel labels carry the per-panel line count, like the notebook.
  counts <- tapply(
    interaction(ld$peptide_seq, ld$occ, drop = TRUE), ld$panel,
    function(x) length(unique(x)))
  panel_lab <- function(p) sprintf("%s peptides (%d)", p, counts[[p]])
  lvl <- intersect(c("Significant", "Non-significant"), names(counts))
  ld$panel_f <- factor(vapply(as.character(ld$panel), panel_lab, character(1)),
                       levels = vapply(lvl, panel_lab, character(1)))

  grp <- interaction(ld$peptide_seq, ld$occ, drop = TRUE)
  ld$grp <- grp

  # one label per line, anchored at the last condition that line reaches.
  lab_rows <- do.call(rbind, lapply(split(ld, ld$grp), function(d) {
    d <- d[order(match(as.character(d$condition), conds)), , drop = FALSE]
    d[nrow(d), , drop = FALSE]
  }))

  n_total <- length(unique(grp))
  # subtitle carries the mapped-peptide count, pluralized.
  sub_txt <- if (n_total == 1L) "Mapped with 1 peptide"
             else sprintf("Mapped with %d peptides", n_total)
  # y-axis title reflects the transform applied at analysis setup.
  y_lab <- sprintf("Average log%d(intensity)", as.integer(log_base))

  ggplot(ld, aes(x = condition, y = mean_log2, group = grp, color = grp)) +
    geom_line(linewidth = 0.5, alpha = 0.9, na.rm = TRUE) +
    geom_point(size = 1.4, na.rm = TRUE) +
    ggrepel::geom_text_repel(
      data = lab_rows, aes(label = aa_label, color = grp),
      direction = "y", hjust = 0, nudge_x = 0.12, size = 2.6,
      segment.size = 0.25, segment.color = "grey70", min.segment.length = 0,
      box.padding = 0.1, max.overlaps = Inf, na.rm = TRUE,
      xlim = c(length(conds) + 0.02, NA)) +
    facet_wrap(~ panel_f, nrow = 1, scales = "fixed") +
    scale_color_hue(guide = "none") +
    scale_x_discrete(expand = expansion(add = c(0.3, 0.7))) +
    coord_cartesian(clip = "off") +
    labs(
      title = sprintf("%s (%s)", gene, accession),
      subtitle = sub_txt,
      x = "Condition", y = y_lab) +
    theme_bw(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "grey25"),
      axis.text.x = element_text(angle = 30, hjust = 1),
      strip.text  = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey92", color = NA),
      panel.spacing = unit(1.4, "lines"),
      panel.grid.minor = element_blank())
}

# ---- Synthetic FKBP3 data (approximates image #2) ----------------------------
set.seed(2026)
conds <- c("DMSO", "rapamycin_100nM", "rapamycin_1uM")

make_lines <- function(n, panel, start_lo, start_hi, drop_lo, drop_hi, jitter) {
  do.call(rbind, lapply(seq_len(n), function(i) {
    y0   <- runif(1, start_lo, start_hi)
    drop <- runif(1, drop_lo, drop_hi)           # total decline DMSO -> 1uM
    ys   <- y0 - drop * c(0, 0.55, 1) + rnorm(3, 0, jitter)
    pos  <- sample(20:190, 1)
    data.frame(
      condition   = factor(conds, levels = conds),
      mean_log2   = ys,
      peptide_seq = sprintf("%s_%02d", panel, i),
      occ         = 1L,
      aa_label    = sprintf("aa%d", pos),
      panel       = panel,
      stringsAsFactors = FALSE)
  }))
}

ld <- rbind(
  make_lines(8,  "Significant",     start_lo = 8.7, start_hi = 10.1,
             drop_lo = 2.6, drop_hi = 6.0, jitter = 0.05),
  make_lines(18, "Non-significant", start_lo = 3.7, start_hi = 13.4,
             drop_lo = -0.2, drop_hi = 0.6, jitter = 0.08)
)

p <- pelsa_intensity_export_ggplot(ld, gene = "FKBP3", accession = "Q00688")

out <- file.path("dev", "intensity_export_example.png")
ragg::agg_png(out, width = 9, height = 5, units = "in", res = 200)
print(p)
invisible(dev.off())
cat("saved", out, "\n")
