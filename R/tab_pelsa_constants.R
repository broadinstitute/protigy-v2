################################################################################
# PELSA shared constants (sourced early; referenced across section3 + woods).
################################################################################

.PELSA_GOLD          <- "#D4AF37"   # selection fill + coverage-track fill
.PELSA_GOLD_RING_W   <- 2           # same-protein peptide ring width
.PELSA_SEL_DARK_RING <- "#333333"   # the selected peptide's dark outline
.PELSA_SEL_DARK_RING_W <- 1.2
# Clicked-peptide emphasis on the volcano: SAME gold fill as its siblings, but
# a larger dot with a thicker black outline so the clicked point stands out
# (vs. the gold overlay's size 7 / 0.5px ring). Drawn as a one-point overlay
# trace on top of the gold highlight.
.PELSA_CLICK_PT_SIZE   <- 11        # clicked-point marker size (gold dot is 7)
.PELSA_CLICK_PT_RING_W <- 2         # clicked-point black-outline width (gold is 0.5)

# ---- Export tree: stage + sub-stage folder names ----------------------------
# The PELSA export writes one nested tree per ome: <ome>/pelsa/<stage>/...
# (the three section servers are merged under a single "pelsa" tab in app_server).
.PELSA_STAGE_SETUP   <- "01_setup"
.PELSA_STAGE_QC      <- "02_qc"
.PELSA_STAGE_VOLCANO <- "03_volcano"
.PELSA_SUB_VOLCANO   <- "01_volcano"
.PELSA_SUB_INTENSITY <- "02_intensity_line"
.PELSA_SUB_WOODS     <- "03_woods"
.PELSA_GRP_MARKER    <- "01_marker"
.PELSA_GRP_SIGNIF    <- "02_significant"

# Significance threshold used across the export figures (volcano / woods / the
# intensity panel split). Kept in one place so figures + footnotes stay in sync.
.PELSA_EXPORT_SIG_CUTOFF <- 0.05

# Lenient per-export cap on the number of PROTEINS rendered as Woods/intensity
# figures. Real analyses essentially never hit it; it bounds a pathological run
# (thousands of significant proteins) so parallel rendering can't spawn an
# unbounded number of ggsave calls. Overflow proteins are recorded in
# skipped_proteins.tsv instead of rendered. Proteins is the axis that explodes
# (Woods then multiplies by contrasts). @noRd
.PELSA_EXPORT_FIGURE_CAP <- 150L

# Figure-export format switches. PNG (via the ragg AGG device) is the shipping
# format; every exported figure is rasterized at this DPI. PDF output is kept in
# the code but switched OFF by default - flip .PELSA_EXPORT_PDF to TRUE to also
# emit vector PDFs alongside the PNGs (future demand).
.PELSA_EXPORT_PDF <- FALSE
.PELSA_EXPORT_DPI <- 300

# Synthetic contrast key for "significant in ANY contrast" (the contrast-
# independent intensity panel split + significant-protein union). A min-adj.P
# column named adj.P.Val.<this> is added so the existing per-contrast helpers
# (pelsa_intensity_proteins / pelsa_intensity_line_data) reuse unchanged.
.PELSA_ANY_CONTRAST <- "any_contrast"

# ---- Significance colors (moved from tab_pelsa_volcano_helpers.R) -----------
# Two-sided significance colors (Decision #4). Down uses a disciplined blue
# (#1f4e9c) distinct from the up red so both significant directions read.
.PELSA_SIG_COLOR_UP   <- "darkred"
.PELSA_SIG_COLOR_DOWN <- "#1f4e9c"
.PELSA_SIG_COLOR_NS   <- "gray"

# ---- Volcano plot constants (moved from tab_pelsa_section3_helpers.R) -------
# The magenta marker-overlay color (Decision: marker peptides ALWAYS on top).
.PELSA_VOLCANO_MARKER_COLOR <- "#FF00FF"
.PELSA_VOLCANO_MARKER_EDGE  <- "black"

# Point sizing / opacity. Markers are only SLIGHTLY larger than the background
# cloud (was 2.4 vs 1, which over-dominated), and the background cloud is fairly
# opaque so non-marker peptides read in their real sig/feature colors (the volcano
# is about ALL peptides, not just markers).
.PELSA_VOLCANO_MARKER_SIZE  <- 1.6
.PELSA_VOLCANO_BG_SIZE      <- 1.1
.PELSA_VOLCANO_BG_ALPHA     <- 0.8

# Default per-contrast label mode: an empty vector - no labels out of the box;
# the user opts into labels via the sidebar checkbox group. label_mode is now
# a CHARACTER VECTOR (zero or more selected checkboxes), not a single value.
.PELSA_VOLCANO_DEFAULT_LABEL_MODE <- character(0)

# The gold used to highlight a selected/pinned peptide (legend entry, Woods
# cross-highlight). Distinct from the magenta marker fill.
.PELSA_VOLCANO_GOLD <- .PELSA_GOLD

.PELSA_VOLCANO_LABEL_MODES <- c("all_markers", "all_significant",
                                "top_n_adjp", "top_n_markers")

# Hard cap on the number of candidate rows the greedy label-placement loop
# (pelsa_volcano_label_annotation_list) will consider. That loop is O(n^2)
# (an inner distance scan against every already-placed label per candidate),
# and unbounded label-selection modes ("all_markers"/"all_significant") can
# hand it tens of thousands of peptide rows on a large uploaded dataset --
# without a cap this pins the single-threaded Shiny process. Candidates are
# pre-ranked by adj.P.Val (ties by P.Value) before truncating, so the most
# significant rows are always kept; on-plot labels are a display aid, not a
# completeness guarantee, so silently dropping the long tail is safe.
.PELSA_VOLCANO_MAX_LABEL_CANDIDATES <- 500L

# Significance-direction -> human legend label (fixed display order).
.PELSA_EXPORT_SIG_LABELS <- c(down = "Downregulated",
                              ns   = "Non-significant",
                              up   = "Upregulated")

# ---- Intensity-rank (S-plot) panel ------------------------------------------
# Common trypsin autolysis contaminant accessions; labeled on the S-plot when
# the "Label trypsin peptides" toggle is on (isoform-matched, like markers).
.PELSA_TRYPSIN_ACCESSIONS <- c("Q29463", "P00760", "P00761")
# Highest-intensity peptides labeled per marker / trypsin protein (per sample).
.PELSA_SPLOT_TOP_N <- 3L
# Teal overlay color for trypsin peptides (distinct from marker magenta + greys).
.PELSA_SPLOT_TRYPSIN_COLOR <- "#1B9E77"
# Export subfolder (under 02_qc) holding the per-sample intensity-rank PNGs.
.PELSA_SPLOT_SUBDIR <- "intensity_rank"

# ---- Placeholder UI helper (moved from tab_pelsa_helpers.R) -----------------
# Standard placeholder box shown in each PELSA section before its analysis has
# been implemented. Returns a shinydashboardPlus box describing the section.
#
# @param ns       the module's namespace function (session$ns)
# @param ome      character, the ome label this section is rendered for
# @param title    character, the box/section title
# @param message  character, the placeholder body text
# @return a fluidRow containing a styled box
# @noRd
pelsa_placeholder_box <- function(ns, ome, title, message) {
  fluidRow(
    shinydashboardPlus::box(
      div(
        style = paste(
          "background-color: #f8f9fa; border-left: 4px solid #007bff;",
          "padding: 12px; margin-bottom: 15px; border-radius: 0 4px 4px 0;"
        ),
        icon("info-circle", style = "color: #007bff; margin-right: 8px;"),
        strong("Coming soon: ", style = "color: #495057;"),
        span(message, style = "color: #495057;")
      ),
      p(paste0("Ome: ", ome)),
      status       = "primary",
      width        = 12,
      title        = title,
      headerBorder = TRUE,
      solidHeader  = TRUE
    )
  )
}

# ---- Shared PELSA export plot theme (typography contract) -------------------
# A single ggplot2 theme applied by the PELSA export plots (section2 static
# figures + intensity-line plots) so their titles, axes, and legends look
# identical across the PELSA figure set. Built on theme_bw(). This is scoped to
# PELSA only -- non-PELSA QC/stat plots keep their own themes. Add to any ggplot
# with `+ pelsa_plot_theme()`.
#
# Legend key-glyph size (the annotation dots / outlined swatches) is NOT a theme
# property; shrink it per-plot with
# `guides(colour = guide_legend(override.aes = list(size = 2)))`.
#
# @param gridlines logical; FALSE (default) removes major + minor grid lines.
#   TRUE keeps them (used by the PELSA intensity-line plots).
# @param base_size numeric base text size (default 12).
# @return a ggplot2 theme object.
# @noRd
pelsa_plot_theme <- function(gridlines = FALSE, base_size = 12) {
  th <- ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title    = ggplot2::element_text(size = 14, face = "bold",
                                             hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
      axis.title    = ggplot2::element_text(size = 12, face = "bold"),
      axis.text     = ggplot2::element_text(size = 10),
      legend.title  = ggplot2::element_text(size = 12, face = "bold"),
      legend.text   = ggplot2::element_text(size = 11,
                                            margin = ggplot2::margin(l = 2)),
      legend.key.size    = ggplot2::unit(12, "pt"),
      legend.spacing.y   = ggplot2::unit(2, "pt"),
      legend.box.spacing = ggplot2::unit(4, "pt"),
      legend.margin      = ggplot2::margin(0, 0, 0, 0)
    )
  if (!gridlines) {
    th <- th + ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  }
  th
}
