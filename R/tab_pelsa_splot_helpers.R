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

# Vectorized per-peptide hover HTML. `Maps to` lists every reported accession as
# "accession (gene)" (gene omitted when blank); accessions whose isoform key is
# in `bold_keys` are <b>-wrapped and ALWAYS shown even past `cap`, other entries
# capped with a "\u2026(+N more)" suffix. In-app only (the export has no hover).
# @noRd
pelsa_splot_tooltip <- function(rank_frame, bold_keys, cap = 8L) {
  n <- nrow(rank_frame)
  if (n == 0L) return(character(0))
  bold_keys <- unique(bold_keys[!is.na(bold_keys) & nzchar(bold_keys)])
  acc_lists  <- strsplit(as.character(rank_frame$accessions), ";", fixed = TRUE)
  gene_lists <- strsplit(as.character(rank_frame$genes), ";", fixed = TRUE)

  vapply(seq_len(n), function(i) {
    accs  <- trimws(if (length(acc_lists) >= i) acc_lists[[i]] else character(0))
    genes <- trimws(if (length(gene_lists) >= i) gene_lists[[i]] else character(0))
    keep  <- !is.na(accs) & nzchar(accs)
    accs  <- accs[keep]
    genes <- if (length(genes) >= length(accs)) genes[seq_along(accs)] else
      c(genes, rep("", length(accs)))[seq_along(accs)]
    genes[is.na(genes)] <- ""

    if (length(accs) == 0L) {
      maps <- "(unmapped)"
    } else {
      key   <- tolower(pelsa_isoform_base(accs))
      bold  <- key %in% bold_keys
      entry <- ifelse(nzchar(genes), paste0(accs, " (", genes, ")"), accs)
      entry <- ifelse(bold, paste0("<b>", entry, "</b>"), entry)
      shown <- (seq_along(entry) <= cap) | bold
      hidden <- sum(!shown)
      maps <- paste(entry[shown], collapse = "; ")
      if (hidden > 0L) maps <- paste0(maps, "; \u2026(+", hidden, " more)")
    }
    sprintf("Rank: #%d<br>Intensity: %.2f<br>Sequence: %s<br>Maps to: %s",
            rank_frame$rank[i], rank_frame$display_intensity[i],
            rank_frame$sequence[i], maps)
  }, character(1))
}

# Assemble everything both builders (plotly + ggplot) consume, from the raw
# inputs available in the Section 2 server. Pure. `selected_markers` is the
# multiselect SUBSET; `trypsin_accs` is .PELSA_TRYPSIN_ACCESSIONS (used only when
# label_trypsin). Marker > trypsin precedence: a peptide highlighted as a marker
# is removed from the trypsin overlay + labels.
# @noRd
pelsa_splot_prepare <- function(mat, sample, peptide_frame, matched,
                                selected_markers, trypsin_accs, label_trypsin,
                                params, top_n = .PELSA_SPLOT_TOP_N) {
  y_title <- pelsa_splot_axis_title(params)
  show_trypsin <- isTRUE(label_trypsin)
  empty_df3 <- data.frame(rank = integer(0), y = numeric(0),
                          hovertext = character(0), stringsAsFactors = FALSE)
  empty_lab <- data.frame(rank = integer(0), y = numeric(0),
                          label = character(0), stringsAsFactors = FALSE)

  rf <- pelsa_splot_rank_frame(mat, sample, peptide_frame,
                               params$log_transformation %||% "None")
  if (nrow(rf) == 0L) {
    return(list(background = empty_df3, marker_pts = empty_df3,
                trypsin_pts = empty_df3, marker_labels = empty_lab,
                trypsin_labels = empty_lab, y_title = y_title,
                show_trypsin = show_trypsin))
  }

  bold_keys <- tolower(pelsa_isoform_base(trimws(as.character(selected_markers))))
  if (show_trypsin) {
    bold_keys <- c(bold_keys,
                   tolower(pelsa_isoform_base(trimws(as.character(trypsin_accs)))))
  }
  bold_keys <- unique(bold_keys[!is.na(bold_keys) & nzchar(bold_keys)])
  rf$hovertext <- pelsa_splot_tooltip(rf, bold_keys)

  mk <- pelsa_splot_marker_topn(matched, selected_markers, rf, n = top_n)
  ty <- if (show_trypsin) {
    pelsa_splot_marker_topn(matched, trypsin_accs, rf, n = top_n)
  } else {
    list(highlight = integer(0),
         labels = data.frame(row_id = integer(0), label = character(0),
                             stringsAsFactors = FALSE))
  }
  ty_ids <- setdiff(ty$highlight, mk$highlight)            # marker precedence

  to_pts <- function(ids) {
    sub <- rf[rf$row_id %in% ids, , drop = FALSE]
    data.frame(rank = sub$rank, y = sub$display_intensity,
               hovertext = sub$hovertext, stringsAsFactors = FALSE)
  }
  to_lab <- function(labels_df, exclude_ids = integer(0)) {
    if (is.null(labels_df) || nrow(labels_df) == 0L) return(empty_lab)
    labels_df <- labels_df[!labels_df$row_id %in% exclude_ids, , drop = FALSE]
    if (nrow(labels_df) == 0L) return(empty_lab)
    sub <- rf[match(labels_df$row_id, rf$row_id), , drop = FALSE]
    ok  <- !is.na(sub$rank)
    data.frame(rank = sub$rank[ok], y = sub$display_intensity[ok],
               label = labels_df$label[ok], stringsAsFactors = FALSE)
  }

  list(
    background      = data.frame(rank = rf$rank, y = rf$display_intensity,
                                 hovertext = rf$hovertext,
                                 stringsAsFactors = FALSE),
    marker_pts      = to_pts(mk$highlight),
    trypsin_pts     = to_pts(ty_ids),
    marker_labels   = to_lab(mk$labels),
    trypsin_labels  = if (show_trypsin) to_lab(ty$labels, mk$highlight) else empty_lab,
    y_title         = y_title,
    show_trypsin    = show_trypsin
  )
}

# Bake top-N labels as NATIVE plotly annotations (offset boxes, border colored
# to the overlay). Native annotations survive toWebGL where a geom_text/text
# trace restyle would not (see CLAUDE.md WebGL note). @noRd
pelsa_splot_add_label_annotations <- function(p, labels, color) {
  if (is.null(labels) || nrow(labels) == 0L) return(p)
  for (i in seq_len(nrow(labels))) {
    p <- plotly::add_annotations(
      p, x = labels$rank[i], y = labels$y[i], text = labels$label[i],
      showarrow = TRUE, arrowhead = 0, arrowsize = 0.6, arrowcolor = color,
      ax = 24, ay = -22, font = list(size = 10, color = color),
      bgcolor = "rgba(255,255,255,0.7)", bordercolor = color, borderwidth = 1)
  }
  p
}

# Interactive S-plot: grey background cloud + magenta marker overlay (+ teal
# trypsin overlay when on), top-N labels baked as annotations. `use_webgl`
# switches the trace backend (scattergl GPU vs scatter SVG fallback). @noRd
pelsa_splot_build_plotly <- function(prep, use_webgl = TRUE,
                                     source_id = "pelsa_splot") {
  trace_type <- if (isTRUE(use_webgl)) "scattergl" else "scatter"
  p <- plotly::plot_ly(source = source_id)

  bg <- prep$background
  p <- plotly::add_trace(
    p, type = trace_type, mode = "markers", name = "Other peptides",
    x = bg$rank, y = bg$y, hoverinfo = "text", hovertext = bg$hovertext,
    marker = list(color = "rgba(150,150,150,0.45)", size = 4))

  mk <- prep$marker_pts
  p <- plotly::add_trace(
    p, type = trace_type, mode = "markers", name = "Marker",
    x = mk$rank, y = mk$y, hoverinfo = "text", hovertext = mk$hovertext,
    marker = list(color = .PELSA_VOLCANO_MARKER_COLOR, size = 7,
                  line = list(color = "black", width = 0.5)))

  if (isTRUE(prep$show_trypsin)) {
    ty <- prep$trypsin_pts
    p <- plotly::add_trace(
      p, type = trace_type, mode = "markers", name = "Trypsin",
      x = ty$rank, y = ty$y, hoverinfo = "text", hovertext = ty$hovertext,
      marker = list(color = .PELSA_SPLOT_TRYPSIN_COLOR, size = 7,
                    line = list(color = "black", width = 0.5)))
  }

  p <- plotly::layout(
    p,
    xaxis = list(title = "Intensity rank (highest \u2192 lowest)",
                 zeroline = FALSE, showgrid = TRUE, gridcolor = "grey92"),
    yaxis = list(title = prep$y_title, zeroline = FALSE, showgrid = TRUE,
                 gridcolor = "grey92"),
    plot_bgcolor = "white", paper_bgcolor = "white", showlegend = TRUE)

  p <- pelsa_splot_add_label_annotations(p, prep$marker_labels,
                                         .PELSA_VOLCANO_MARKER_COLOR)
  if (isTRUE(prep$show_trypsin)) {
    p <- pelsa_splot_add_label_annotations(p, prep$trypsin_labels,
                                           .PELSA_SPLOT_TRYPSIN_COLOR)
  }
  p
}

# Static S-plot for export (ggplot2 + ggrepel; saved as PNG via ragg). Mirrors
# the plotly view: grey cloud, magenta marker overlay (+ teal trypsin when on),
# repelled top-N labels. @noRd
pelsa_splot_build_ggplot <- function(prep) {
  g <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = prep$background, ggplot2::aes(x = .data$rank, y = .data$y),
      color = "grey70", size = 0.5, alpha = 0.5) +
    ggplot2::geom_point(
      data = prep$marker_pts, ggplot2::aes(x = .data$rank, y = .data$y),
      color = .PELSA_VOLCANO_MARKER_COLOR, size = 1.4)

  if (isTRUE(prep$show_trypsin) && nrow(prep$trypsin_pts) > 0L) {
    g <- g + ggplot2::geom_point(
      data = prep$trypsin_pts, ggplot2::aes(x = .data$rank, y = .data$y),
      color = .PELSA_SPLOT_TRYPSIN_COLOR, size = 1.4)
  }

  if (nrow(prep$marker_labels) > 0L) {
    g <- g + ggrepel::geom_text_repel(
      data = prep$marker_labels,
      ggplot2::aes(x = .data$rank, y = .data$y, label = .data$label),
      color = .PELSA_VOLCANO_MARKER_COLOR, size = 2.6, direction = "y",
      min.segment.length = 0, max.overlaps = Inf)
  }
  if (isTRUE(prep$show_trypsin) && nrow(prep$trypsin_labels) > 0L) {
    g <- g + ggrepel::geom_text_repel(
      data = prep$trypsin_labels,
      ggplot2::aes(x = .data$rank, y = .data$y, label = .data$label),
      color = .PELSA_SPLOT_TRYPSIN_COLOR, size = 2.6, direction = "y",
      min.segment.length = 0, max.overlaps = Inf)
  }

  g + ggplot2::labs(x = "Intensity rank (highest \u2192 lowest)",
                    y = prep$y_title) +
    ggplot2::theme_bw()
}

# Write one intensity-rank PNG per sample for ONE dataset into the
# 02_qc/intensity_rank/ subfolder, honoring `custom` (the per-ome sticky store;
# NULL -> defaults: all markers selected, trypsin off). Re-derives the matrix +
# peptide frame from the processed GCT at export time. @noRd
pelsa_splot_export_for <- function(dir_name, gct, matched, marker_accs,
                                   params, custom = NULL) {
  out <- pelsa_export_stage_dir(dir_name, .PELSA_STAGE_QC, .PELSA_SPLOT_SUBDIR)
  if (is.null(gct)) return(invisible(out))
  peptides <- pelsa_dataset_peptide_frame(gct)
  mat <- pelsa_dataset_matrix(gct, colnames(peptides))
  selected <- custom$selected_markers %||% marker_accs %||% character(0)
  label_trypsin <- isTRUE(custom$label_trypsin)
  for (s in colnames(mat)) {
    prep <- pelsa_splot_prepare(mat, s, peptides, matched, selected,
                                .PELSA_TRYPSIN_ACCESSIONS, label_trypsin, params)
    if (nrow(prep$background) == 0L) next
    g <- pelsa_splot_build_ggplot(prep)
    tryCatch(
      pelsa_save_figure(g, out, paste0("intensity_rank_", pelsa_safe_name(s)),
                        width = 9, height = 6),
      error = function(e) NULL)
  }
  invisible(out)
}
