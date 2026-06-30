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

# The single value plotted on y / shown in the tooltip / ranked on. Already-log
# matrices (log2/log10) are used as-is; a linear matrix (log_transformation
# None/NA) is forced to log2 for display, with non-positive originals -> NA
# (they drop out of the finite filter). NEVER double-logs already-log data.
# @noRd
pelsa_splot_display_intensity <- function(values, log_transformation) {
  values <- as.numeric(values)
  lt <- tolower(as.character(log_transformation %||% "none"))
  if (lt %in% c("log2", "log10")) return(values)
  out <- suppressWarnings(log2(values))
  out[!is.finite(out)] <- NA_real_
  out
}

# Build the per-sample ranked peptide frame. `peptide_frame` MUST be row-aligned
# to `mat` (both come from the same processed GCT via
# pelsa_dataset_peptide_frame / pelsa_dataset_matrix), so row_id = matrix row
# index = entry$matched$.row_id. Finite display_intensity rows only, sorted
# highest-first (rank 1 = highest intensity).
# @noRd
pelsa_splot_rank_frame <- function(mat, sample, peptide_frame, log_transformation) {
  empty <- data.frame(row_id = integer(0), sequence = character(0),
                      accessions = character(0), genes = character(0),
                      display_intensity = numeric(0), rank = integer(0),
                      stringsAsFactors = FALSE)
  if (!is.matrix(mat) || is.null(colnames(mat)) ||
      length(sample) != 1L || !(sample %in% colnames(mat))) {
    return(empty)
  }
  n <- nrow(mat)
  col_or_na <- function(nm) {
    if (nm %in% names(peptide_frame)) as.character(peptide_frame[[nm]])
    else rep(NA_character_, n)
  }
  yi <- pelsa_splot_display_intensity(mat[, sample], log_transformation)
  df <- data.frame(
    row_id            = seq_len(n),
    sequence          = col_or_na("PEP.StrippedSequence"),
    accessions        = col_or_na("PG.ProteinAccessions"),
    genes             = col_or_na("PG.Genes"),
    display_intensity = yi,
    stringsAsFactors  = FALSE
  )
  df <- df[is.finite(df$display_intensity), , drop = FALSE]
  if (nrow(df) == 0L) return(empty)
  df <- df[order(-df$display_intensity), , drop = FALSE]
  df$rank <- seq_len(nrow(df))
  rownames(df) <- NULL
  df
}
