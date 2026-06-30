################################################################################
# PELSA Summary - Intensity rank (S-plot) panel helpers (PURE; no Shiny).
#
# For one selected sample, rank every FINITE peptide by its normalized log
# intensity and plot rank (x, 1 = highest) vs intensity (y). Marker / trypsin
# peptides are highlighted + top-N labeled (<gene>_aa<pos>, marker-scoped). See
# docs/superpowers/specs/2026-06-29-pelsa-intensity-rank-splot-design.md.
################################################################################

# Y-axis title from the dataset's setup params. The matrix is plotted as the
# single `display_intensity` (see pelsa_splot_display_intensity): already-log
# data is used as-is; `None` (linear) is forced to log2 for display, so the
# base shown is log10 only when log_transformation == "log10", else log2. The
# normalization clause is dropped when data_normalization is None/NA.
# @noRd
pelsa_splot_axis_title <- function(params) {
  lt <- tolower(as.character(params$log_transformation %||% "none"))
  base <- if (identical(lt, "log10")) "log10" else "log2"
  log_term <- paste0(base, "(intensity)")
  norm <- params$data_normalization
  norm_ok <- !is.null(norm) && length(norm) == 1L && !is.na(norm) &&
    nzchar(norm) && !identical(tolower(norm), "none")
  if (norm_ok) paste0(log_term, ", ", norm, " normalized") else log_term
}
