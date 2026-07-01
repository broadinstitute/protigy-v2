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
    protein_name = if ("protein_name" %in% colnames(matched))
      as.character(matched[["protein_name"]])[hit] else
      rep(NA_character_, sum(hit)),
    pep_start = as.integer(matched[["pep_start"]])[hit],
    stringsAsFactors = FALSE
  )
  highlight <- unique(sub$row_id)

  sub <- sub[order(sub$acc_key, sub$row_id, sub$pep_start, na.last = TRUE), ,
             drop = FALSE]

  # Per-mapping label entry "<stem>_aa<pep_start>", stem = gene -> protein_name
  # -> accession (shared resolver, same fallback as the volcano labels).
  sub$lid <- pelsa_resolve_label_stem(sub$gene, sub$protein_name, sub$accession)
  sub$pos_entry <- paste0(sub$lid, "_aa", sub$pep_start)

  # One representative per (acc_key, row_id): the peptide's intensity, plus a
  # ;-joined label over ALL its positions on that accession (repeat motif), in
  # the pep_start order set by the sort above.
  grp <- paste(sub$acc_key, sub$row_id, sep = "\r")
  entry_by_grp <- tapply(sub$pos_entry, grp, function(e) paste(e, collapse = ";"))
  rep_rows <- sub[!duplicated(grp), c("acc_key", "row_id", "y"), drop = FALSE]
  rep_rows$entry <- as.character(
    entry_by_grp[paste(rep_rows$acc_key, rep_rows$row_id, sep = "\r")])

  # Top-N peptides per accession by display_intensity (desc); stable within-group
  # head() via split (preserves the -y order).
  rep_rows <- rep_rows[order(rep_rows$acc_key, -rep_rows$y), , drop = FALSE]
  keep_idx <- unlist(lapply(
    split(seq_len(nrow(rep_rows)), rep_rows$acc_key),
    function(ix) ix[seq_len(min(n, length(ix)))]), use.names = FALSE)
  top <- rep_rows[sort(keep_idx), , drop = FALSE]
  if (nrow(top) == 0L) return(list(highlight = highlight, labels = empty$labels))

  # Collapse per peptide across accessions: split to individual positions,
  # de-duplicate, and ;-join (handles repeat-motif AND shared-peptide).
  agg <- tapply(top$entry, top$row_id, function(e) {
    parts <- unlist(strsplit(e, ";", fixed = TRUE))
    paste(unique(parts), collapse = ";")
  })
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
    # Pad/truncate genes to accs length BEFORE dropping empty accessions so gene
    # i stays paired with accession i; then apply the SAME keep mask to both. If
    # genes were filtered only positionally after `accs` shrank, a leading/interior
    # empty accession token would slide every gene onto the wrong accession.
    genes <- if (length(genes) >= length(accs)) genes[seq_along(accs)] else
      c(genes, rep("", length(accs)))[seq_along(accs)]
    keep  <- !is.na(accs) & nzchar(accs)
    accs  <- accs[keep]
    genes <- genes[keep]
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
# the plotly view: grey cloud, magenta marker overlay (+ teal trypsin overlay
# when on) with a right-hand legend, repelled top-N labels via
# ggrepel::geom_label_repel. `title`/`subtitle` feed ggplot2::labs() directly
# (export passes the sample name as subtitle). @noRd
pelsa_splot_build_ggplot <- function(prep,
                                     title = "Intensity rank (S-plot)",
                                     subtitle = NULL) {
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

  # Repelled labels: white box + colored outline/text. max.overlaps is a CAP
  # (not Inf): in crowded clusters ggrepel drops the least-room labels (with a
  # warning) instead of stacking overlapping boxes; the point stays drawn.
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

  g +
    ggplot2::scale_color_manual(
      name = NULL, values = SERIES_COLORS, breaks = legend_breaks) +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(size = 3))) +
    ggplot2::labs(x = "Intensity rank", y = prep$y_title,
                  title = title, subtitle = subtitle) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold"),   # bold plot title
      axis.title       = ggplot2::element_text(face = "bold"),   # bold axis titles
      panel.grid.major = ggplot2::element_blank(),               # no grid lines
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "right",                                # legend on right
      # --- Compact legend: pull it in tight to the panel + shrink internals ---
      legend.box.spacing = ggplot2::unit(4, "pt"),   # gap panel <-> legend
      legend.margin      = ggplot2::margin(0, 0, 0, 0),
      legend.key.size    = ggplot2::unit(12, "pt"),  # swatch box size
      legend.spacing.y   = ggplot2::unit(2, "pt"),   # gap between entries
      legend.text        = ggplot2::element_text(margin = ggplot2::margin(l = 2)))
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
    g <- pelsa_splot_build_ggplot(prep, subtitle = s)
    tryCatch(
      pelsa_save_figure(g, out, paste0("intensity_rank_", pelsa_safe_name(s)),
                        width = 8, height = 5),
      error = function(e) warning(sprintf(
        "pelsa_splot_export_for: failed to write sample '%s': %s",
        s, conditionMessage(e)), call. = FALSE))
  }
  invisible(out)
}
