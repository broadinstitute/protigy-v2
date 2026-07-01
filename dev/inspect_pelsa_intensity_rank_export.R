################################################################################
# Standalone inspection script: PELSA intensity-rank (S-plot) STATIC EXPORT.
#
# Renders the exported intensity-rank figure via the REAL export builder chain
# (pelsa_splot_prepare -> pelsa_splot_build_ggplot -> pelsa_save_figure) using
# synthetic-but-realistic inputs, so you can open the PNG and iterate on styling
# without a full setup + analysis run or any real data.
#
# Run from the repo root:
#   Rscript dev/inspect_pelsa_intensity_rank_export.R
# or interactively:
#   source("dev/inspect_pelsa_intensity_rank_export.R")
#
# Design: docs/superpowers/specs/2026-06-30-pelsa-intensity-rank-export-inspection-script-design.md
################################################################################

# Resolve the repo root regardless of the caller's working directory:
#   - Rscript: derive it from this script's own path (dev/ -> repo root)
#   - source() / interactive: fall back to the known repo path
.this_file <- tryCatch({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1L && nzchar(f)) normalizePath(f) else NA_character_
}, error = function(e) NA_character_)

.repo_root <- if (!is.na(.this_file)) {
  dirname(dirname(.this_file))                       # dev/<script> -> repo root
} else {
  "/Users/cameronlian/git/protigy-v2"                # source()/interactive fallback
}

suppressMessages(devtools::load_all(.repo_root))

## ---- Tweakables -----------------------------------------------------------
N_PEPTIDES         <- 4000L                 # size of the ranked background cloud
N_MARKERS          <- 4L                    # distinct marker accessions to plant
TOP_N              <- .PELSA_SPLOT_TOP_N     # labels per accession (default 3)
LABEL_TRYPSIN      <- TRUE                   # teal trypsin overlay + labels
LOG_TRANSFORMATION <- "log2"                # "log2" | "log10" | "None"
DATA_NORMALIZATION <- "Median (non-zero)"   # y-axis subtitle; "None" drops it
SAMPLE_NAME        <- "S1_condition_rep1"

# --- Title / subtitle -------------------------------------------------------
# Every other PELSA export figure carries a labs(title=, subtitle=); the S-plot
# builder currently has none -- that is the gap we are closing. The S-plot is
# PER-SAMPLE, so the sample name is the disambiguating context and goes in the
# subtitle (mirroring how QC plots put "n = .." / "N clamped" there, and the
# volcano puts its method there). Two theme conventions are compared at render
# time below (A = QC style, B = volcano style).
PLOT_TITLE     <- "Intensity rank (S-plot)"
PLOT_SUBTITLE  <- SAMPLE_NAME               # NULL to drop the subtitle line
OUT_DIR            <- file.path(
  "/private/tmp/claude-501/-Users-cameronlian-git-protigy-v2",
  "8bb29842-8584-475a-bcc2-05671cdc6156/scratchpad")

set.seed(42)  # deterministic layout so re-runs are comparable

## ---- Synthetic ranked matrix ----------------------------------------------
# A decaying intensity curve (+ noise) so the ranked "S" shape is visible. One
# sample column, row names p1..pN. Values are already-log (LOG_TRANSFORMATION),
# so the builder plots them as-is.
rowids <- paste0("p", seq_len(N_PEPTIDES))
base_curve <- 22 - 8 * (seq_len(N_PEPTIDES) / N_PEPTIDES)^0.6
intensity  <- base_curve + rnorm(N_PEPTIDES, sd = 0.4)
mat <- matrix(intensity, ncol = 1L, dimnames = list(rowids, SAMPLE_NAME))

## ---- Row-aligned peptide frame --------------------------------------------
peptide_frame <- data.frame(
  PEP.StrippedSequence = paste0("PEPTIDE", seq_len(N_PEPTIDES)),
  PG.ProteinAccessions = paste0("Q", sprintf("%05d", seq_len(N_PEPTIDES))),
  PG.Genes             = paste0("GENE", seq_len(N_PEPTIDES)),
  stringsAsFactors     = FALSE)

## ---- Matched cache: plant markers + trypsin peptides ----------------------
# Markers: spread across high-to-mid ranks so overlays + top-N labels are visible.
# Each marker accession gets TOP_N + 1 peptides (so top-N capping is exercised).
marker_accs  <- paste0("MARKER", seq_len(N_MARKERS))
marker_genes <- paste0("MK", seq_len(N_MARKERS))
per_marker   <- TOP_N + 1L

pick_rows <- function(center, k, spread = N_PEPTIDES / 120) {
  # k row indices near `center` (a rank position), clamped into range. `spread`
  # sets the per-accession spacing (larger = peptides sit farther apart).
  idx <- round(center + seq_len(k) * spread)
  pmin(pmax(idx, 1L), N_PEPTIDES)
}

marker_rows <- unlist(lapply(seq_len(N_MARKERS), function(i)
  pick_rows(center = i * (N_PEPTIDES / (N_MARKERS + 1L)), k = per_marker)))
marker_acc_col  <- rep(marker_accs,  each = per_marker)
marker_gene_col <- rep(marker_genes, each = per_marker)

# Trypsin peptides: real trypsin accessions, SPREAD across the top-third of the
# ranking (not jammed at one point) so they don't pile onto the first markers.
trypsin_rows <- round(seq(N_PEPTIDES * 0.05, N_PEPTIDES * 0.30,
                          length.out = 4L))
trypsin_acc_col  <- rep(.PELSA_TRYPSIN_ACCESSIONS,
                        length.out = length(trypsin_rows))
trypsin_gene_col <- rep(c("TRYP1", "TRYP2", "TRYP3"),
                        length.out = length(trypsin_rows))

all_rows  <- c(marker_rows, trypsin_rows)
matched <- data.frame(
  .row_id              = as.integer(all_rows),
  accession            = c(marker_acc_col, trypsin_acc_col),
  gene                 = c(marker_gene_col, trypsin_gene_col),
  pep_start            = as.integer(10L + all_rows %% 300L),
  PEP.StrippedSequence = peptide_frame$PEP.StrippedSequence[all_rows],
  check.names = FALSE, stringsAsFactors = FALSE)

## ---- Params (drives y-axis title) -----------------------------------------
params <- list(log_transformation = LOG_TRANSFORMATION,
               data_normalization = DATA_NORMALIZATION)

## ---- Build + save via the real export chain -------------------------------
prep <- pelsa_splot_prepare(
  mat            = mat,
  sample         = SAMPLE_NAME,
  peptide_frame  = peptide_frame,
  matched        = matched,
  selected_markers = marker_accs,
  trypsin_accs   = .PELSA_TRYPSIN_ACCESSIONS,
  label_trypsin  = LABEL_TRYPSIN,
  params         = params,
  top_n          = TOP_N)

message(sprintf(
  "prepared: %d background / %d marker pts / %d trypsin pts / %d marker labels / %d trypsin labels",
  nrow(prep$background), nrow(prep$marker_pts), nrow(prep$trypsin_pts),
  nrow(prep$marker_labels), nrow(prep$trypsin_labels)))

# ---------------------------------------------------------------------------
# SETTLED LAYOUT -- built from `prep` directly (NOT layered on
# pelsa_splot_build_ggplot()), because a right-hand LEGEND requires mapping
# color to a discrete series, which the builder's hardcoded-color geom_point
# layers can't express. This function IS the candidate new builder body.
#
# Series colors mirror the interactive plotly S-plot:
#   Marker  = .PELSA_VOLCANO_MARKER_COLOR (magenta)
#   Trypsin = .PELSA_SPLOT_TRYPSIN_COLOR  (teal, only when show_trypsin)
# The grey background cloud is an UNMAPPED layer, so it stays out of the legend.
#
# Layout: QC-export convention -- title left of a labs(title=, subtitle=sample);
# bold plot title + bold axis titles; no grid lines; legend on the right.
# Labels use ggrepel::geom_label_repel (white fill, colored outline + text),
# which auto-handles jitter/dodging.
# ---------------------------------------------------------------------------
SERIES_COLORS <- c(Marker  = .PELSA_VOLCANO_MARKER_COLOR,
                   Trypsin = .PELSA_SPLOT_TRYPSIN_COLOR)

# Legend breaks: Trypsin only when the overlay is on AND has points.
show_trypsin <- isTRUE(prep$show_trypsin) && nrow(prep$trypsin_pts) > 0L
legend_breaks <- if (show_trypsin) c("Marker", "Trypsin") else "Marker"

g <- ggplot2::ggplot() +
  # grey background cloud (unmapped -> not in legend)
  ggplot2::geom_point(
    data = prep$background, ggplot2::aes(x = .data$rank, y = .data$y),
    color = "grey70", size = 0.5, alpha = 0.5) +
  # marker overlay (color mapped -> legend entry)
  ggplot2::geom_point(
    data = prep$marker_pts,
    ggplot2::aes(x = .data$rank, y = .data$y, color = "Marker"), size = 1.4)

if (show_trypsin) {
  g <- g + ggplot2::geom_point(
    data = prep$trypsin_pts,
    ggplot2::aes(x = .data$rank, y = .data$y, color = "Trypsin"), size = 1.4)
}

# Repelled labels: white box + colored outline/text. Tuning:
#   box.padding / point.padding -- breathing room around each box + its point.
#   max.overlaps = .SPLOT_MAX_OVERLAPS -- a CAP (not Inf): in genuinely crowded
#     clusters ggrepel drops the least-room labels (with a warning) instead of
#     stacking overlapping boxes. Dropped points stay drawn -- only their label
#     is omitted. direction = "both" lets it dodge in x too, not just y.
.SPLOT_MAX_OVERLAPS <- 20L
repel_label <- function(g, df, color) {
  if (is.null(df) || nrow(df) == 0L) return(g)
  g + ggrepel::geom_label_repel(
    data = df,
    ggplot2::aes(x = .data$rank, y = .data$y, label = .data$label),
    color = color, fill = "white", size = 2, label.padding = 0.2, 
    box.padding = 0.1, direction = "y", force = 50, 
    min.segment.length = 0, max.overlaps = .SPLOT_MAX_OVERLAPS,
    seed = 42L, show.legend = FALSE)
}
g <- repel_label(g, prep$marker_labels, .PELSA_VOLCANO_MARKER_COLOR)
if (show_trypsin) g <- repel_label(g, prep$trypsin_labels,
                                   .PELSA_SPLOT_TRYPSIN_COLOR)

g <- g +
  ggplot2::scale_color_manual(
    name = NULL, values = SERIES_COLORS, breaks = legend_breaks) +
  ggplot2::guides(
    color = ggplot2::guide_legend(override.aes = list(size = 3))) +
  ggplot2::labs(x = "Intensity rank", y = prep$y_title,
                title = PLOT_TITLE, subtitle = PLOT_SUBTITLE) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold"),   # bold plot title
    axis.title       = ggplot2::element_text(face = "bold"),   # bold axis titles
    panel.grid.major = ggplot2::element_blank(),               # no grid lines
    panel.grid.minor = ggplot2::element_blank(),
    legend.position  = "right",                                # legend on right
    # --- Compact legend: pull it in tight to the panel + shrink internals -----
    legend.box.spacing = ggplot2::unit(4, "pt"),   # gap panel <-> legend
    legend.margin      = ggplot2::margin(0, 0, 0, 0),
    legend.key.size    = ggplot2::unit(12, "pt"),  # swatch box size
    legend.spacing.y   = ggplot2::unit(2, "pt"),   # gap between entries
    legend.text        = ggplot2::element_text(margin = ggplot2::margin(l = 2)))

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
base <- paste0("intensity_rank_", pelsa_safe_name(SAMPLE_NAME))

# 8x5in -- matches the QC family's export dimensions (save_fig w=8, h=5) and
# de-crowds the dense top-left label cluster relative to the old 9x6.
paths <- pelsa_save_figure(g, OUT_DIR, base, width = 8, height = 5)

message("Wrote:\n  ", paste(normalizePath(paths), collapse = "\n  "))
