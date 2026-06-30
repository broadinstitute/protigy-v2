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

# Marker-scoped highlight + top-N labels for one accession set (markers OR
# trypsin). Resolves per accession from the matched cache (so positions are on
# THAT accession). Only peptides finite in the sample (present in rank_frame)
# are eligible. Returns highlight = every distinct peptide row_id mapping to any
# accession; labels = top-N peptides per accession by display_intensity, with a
# peptide that wins under >1 accession carrying a ;-joined deduped label.
# @noRd
pelsa_splot_marker_topn <- function(matched, accessions, rank_frame, n = 3L) {
  empty <- list(highlight = integer(0),
                labels = data.frame(row_id = integer(0), label = character(0),
                                    stringsAsFactors = FALSE))
  if (is.null(matched) || !is.data.frame(matched) || nrow(matched) == 0L ||
      is.null(rank_frame) || nrow(rank_frame) == 0L ||
      length(accessions) == 0L) {
    return(empty)
  }
  acc_keys <- unique(tolower(pelsa_isoform_base(trimws(as.character(accessions)))))
  acc_keys <- acc_keys[!is.na(acc_keys) & nzchar(acc_keys)]
  if (length(acc_keys) == 0L) return(empty)

  # Join key: .row_id (positional, collision-proof) else stripped sequence.
  if (".row_id" %in% colnames(matched)) {
    m_key  <- as.character(matched[[".row_id"]])
    rf_key <- as.character(rank_frame$row_id)
  } else {
    m_key  <- as.character(matched[["PEP.StrippedSequence"]])
    rf_key <- as.character(rank_frame$sequence)
  }
  idx <- match(m_key, rf_key)                      # NA where peptide not finite
  m_acc_key <- tolower(pelsa_isoform_base(trimws(as.character(matched[["accession"]]))))
  hit <- !is.na(idx) & (m_acc_key %in% acc_keys)
  if (!any(hit)) return(empty)

  sub <- data.frame(
    row_id    = rank_frame$row_id[idx[hit]],
    y         = rank_frame$display_intensity[idx[hit]],
    acc_key   = m_acc_key[hit],
    gene      = as.character(matched[["gene"]])[hit],
    accession = as.character(matched[["accession"]])[hit],
    pep_start = as.integer(matched[["pep_start"]])[hit],
    stringsAsFactors = FALSE
  )
  highlight <- unique(sub$row_id)

  # One representative row per (acc_key, row_id): smallest pep_start.
  sub <- sub[order(sub$acc_key, sub$row_id, sub$pep_start, na.last = TRUE), ,
             drop = FALSE]
  rep_rows <- sub[!duplicated(sub[, c("acc_key", "row_id")]), , drop = FALSE]

  # Top-N peptides per accession by display_intensity (desc), then a stable
  # within-group head() via split (preserves the -y order set just below).
  rep_rows <- rep_rows[order(rep_rows$acc_key, -rep_rows$y), , drop = FALSE]
  keep_idx <- unlist(lapply(
    split(seq_len(nrow(rep_rows)), rep_rows$acc_key),
    function(ix) ix[seq_len(min(n, length(ix)))]), use.names = FALSE)
  top <- rep_rows[sort(keep_idx), , drop = FALSE]
  if (nrow(top) == 0L) return(list(highlight = highlight, labels = empty$labels))

  lid <- ifelse(is.na(top$gene) | !nzchar(trimws(top$gene)),
                top$accession, top$gene)
  top$entry <- paste0(lid, "_aa", top$pep_start)

  agg <- tapply(top$entry, top$row_id,
                function(e) paste(unique(e), collapse = ";"))
  labels <- data.frame(row_id = as.integer(names(agg)),
                       label = as.character(agg), stringsAsFactors = FALSE)
  list(highlight = highlight, labels = labels)
}
