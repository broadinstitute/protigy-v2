################################################################################
# Module: PELSA Section 3 - volcano SELECTION/INTERACTION pure helpers.
#
# The single-selection model's pure logic: resolve a click to a peptide, compute
# the gold recolor arrays for the proxy restyle, the Find-accession match mask,
# and the pinned-panel metadata rows. No Shiny; unit-tested against the seeded
# synthetic generator's closed-form ground truth.
################################################################################

# ---- 7E: resolve a plotly_click event to a volcano-df peptide ---------------

# Map a plotly_click event (a one-row data.frame from event_data() carrying at
# least numeric `x` and `y`, the clicked point's logFC / -log10(P.Value)) back to
# the volcano-df row it came from, returning that peptide's identity.
#
# WHY coordinate-matching (not customdata/key): ggplotly() does NOT reliably
# round-trip a `key`/`customdata` aesthetic through plotly::toWebGL across the
# multiple geom traces (background / marker / labels) the volcano draws, and the
# event's curveNumber/pointNumber then index INTO the wrong trace after WebGL
# trace-merging. The point's (x, y) IS stable: plotly returns the exact
# (logFC, logP) of the clicked marker, so we match the nearest df row by squared
# Euclidean distance in (logFC, logP). Ties / multiple peptides at the same
# coordinate resolve to the FIRST df row (deterministic).
#
# Representative accession when a peptide maps to several: the volcano df's
# `winning_accession` (the 2I feature-annotation winner - the leading/
# representative accession for that peptide). Falls back to the first
# ;-separated token of PG.ProteinAccessions when winning_accession is NA/empty.
#
# @param event     a one-row data.frame/list with numeric `x` and `y` (an
#   event_data("plotly_click") row), or NULL.
# @param volcano_df a 3A frame (logFC, logP, id, winning_accession,
#   PG.ProteinAccessions, label).
# @return list(row = <1-based df row index>, peptide_seq = <id>,
#   accession = <representative accession>, label = <df label>), or NULL when no
#   match (NULL/empty event, empty df, or no finite coordinate).
# @noRd
pelsa_volcano_resolve_click <- function(event, volcano_df) {
  if (is.null(event) || !is.data.frame(volcano_df) || nrow(volcano_df) == 0L) {
    return(NULL)
  }
  ex <- suppressWarnings(as.numeric(event$x)[1L])
  ey <- suppressWarnings(as.numeric(event$y)[1L])
  if (length(ex) == 0L || length(ey) == 0L || is.na(ex) || is.na(ey)) {
    return(NULL)
  }
  if (!all(c("logFC", "logP") %in% colnames(volcano_df))) return(NULL)

  fx <- as.numeric(volcano_df$logFC)
  fy <- as.numeric(volcano_df$logP)
  d2 <- (fx - ex)^2 + (fy - ey)^2
  d2[is.na(d2)] <- Inf
  if (!any(is.finite(d2))) return(NULL)
  row <- which.min(d2)

  acc <- NA_character_
  if ("winning_accession" %in% colnames(volcano_df)) {
    acc <- as.character(volcano_df$winning_accession[row])
  }
  if (is.na(acc) || !nzchar(acc)) {
    pg <- if ("PG.ProteinAccessions" %in% colnames(volcano_df)) {
      as.character(volcano_df$PG.ProteinAccessions[row])
    } else {
      NA_character_
    }
    acc <- if (is.na(pg) || !nzchar(pg)) NA_character_ else
      trimws(strsplit(pg, ";", fixed = TRUE)[[1]][1L])
  }

  pep <- if ("id" %in% colnames(volcano_df)) {
    as.character(volcano_df$id[row])
  } else {
    NA_character_
  }
  lab <- if ("label" %in% colnames(volcano_df)) {
    as.character(volcano_df$label[row])
  } else {
    NA_character_
  }
  list(row = as.integer(row), peptide_seq = pep, accession = acc, label = lab)
}

# ---- 7E: sibling-peptide trace split (for the pinned-protein highlight) ------

# Split a volcano frame into the PINNED protein's peptides (the pinned peptide +
# its sibling peptides - every row whose winning_accession equals the pinned
# accession) and the REST. On selection, the main volcano is NOT rebuilt; instead
# the highlight is applied client-side via a plotlyProxy restyle (single
# mechanism) that sets per-point fill/ring arrays on the background + marker
# traces (see pelsa_volcano_recolor). This mask identifies a protein's peptides
# for callers that need the membership test.
#
# Matching is on `winning_accession` (the representative accession 3A resolves
# per peptide), so a peptide pinned in a multi-protein group lights up its
# siblings under the SAME representative protein.
#
# @param volcano_df a 3A frame carrying winning_accession.
# @param accession  the pinned protein's representative accession (scalar), or
#   NULL/NA -> no siblings (all rows go to `rest`).
# @return list(siblings = <logical mask>, n_siblings = <integer>), the mask
#   length nrow(volcano_df) TRUE where the row belongs to the pinned protein.
# @noRd
pelsa_volcano_sibling_mask <- function(volcano_df, accession) {
  if (!is.data.frame(volcano_df)) {
    stop("pelsa_volcano_sibling_mask: volcano_df must be a data.frame")
  }
  n <- nrow(volcano_df)
  if (n == 0L || is.null(accession) || length(accession) != 1L ||
      is.na(accession) || !nzchar(accession) ||
      !"winning_accession" %in% colnames(volcano_df)) {
    return(list(siblings = rep(FALSE, n), n_siblings = 0L))
  }
  wa <- as.character(volcano_df$winning_accession)
  mask <- !is.na(wa) & wa == accession
  list(siblings = mask, n_siblings = sum(mask))
}

# Compute the per-trace recolor arrays for the volcano proxy restyle under the
# single-selection model. Returns fills + ring color/width for BOTH restyled
# traces (background == pelsa_volcano_marker_split(df)$background row order,
# markers == $markers row order).
#
# selection: NULL, or list(origin="click"|"find", accession, peptide_seq).
# find_mask: NULL, or a logical over df rows (the MULTI-accession find highlight;
#            uniform gold fill, no dark ring). Ignored when selection is non-NULL.
# color_mode: "significance" | "feature" -> the BASE fill column.
# @return list(background=list(color,line.color,line.width),
#              markers=list(color,line.color,line.width)). @noRd
pelsa_volcano_recolor <- function(df, selection, find_mask = NULL,
                                  color_mode = "significance") {
  split <- pelsa_volcano_marker_split(df)
  mk_one <- function(sub) {
    n <- nrow(sub)
    base <- if (identical(color_mode, "feature")) {
      as.character(sub$feature_color)
    } else {
      as.character(sub$sig_color)
    }
    color <- base
    line.color <- rep("rgba(0,0,0,0)", n)
    line.width <- rep(0, n)
    if (n == 0L) return(list(color = color, line.color = line.color,
                             line.width = line.width))
    ids <- as.character(sub$id)
    wacc <- as.character(sub$winning_accession)

    sel_seq <- if (!is.null(selection)) selection$peptide_seq else NA_character_
    sel_acc <- if (!is.null(selection)) selection$accession   else NA_character_

    if (!is.na(sel_acc) && nzchar(sel_acc)) {
      sib <- !is.na(wacc) & wacc == sel_acc & (is.na(sel_seq) | ids != sel_seq)
      line.color[sib] <- .PELSA_GOLD
      line.width[sib] <- .PELSA_GOLD_RING_W
    }
    if (!is.na(sel_seq) && nzchar(sel_seq)) {
      hit <- ids == sel_seq
      color[hit] <- .PELSA_GOLD
      line.color[hit] <- .PELSA_SEL_DARK_RING
      line.width[hit] <- .PELSA_SEL_DARK_RING_W
    }
    if (!is.null(find_mask) && is.null(selection)) {
      fm_sub <- find_mask[match(ids, as.character(df$id))]
      fm_sub[is.na(fm_sub)] <- FALSE
      color[fm_sub] <- .PELSA_GOLD
    }
    list(color = color, line.color = line.color, line.width = line.width)
  }
  list(background = mk_one(split$background), markers = mk_one(split$markers))
}

# Resolve the background / marker trace JS indices (0-based) of a built volcano
# plotly by the `meta` tag the build stamps (pelsa_volcano_build_plot). Returns
# list(background=<int|NA>, markers=<int|NA>). @noRd
.pelsa_volcano_trace_index <- function(p) {
  metas <- vapply(p$x$data, function(tr) {
    m <- tr$meta
    if (is.null(m) || length(m) != 1L) NA_character_ else as.character(m)
  }, character(1))
  bg <- which(metas == "pelsa_bg")
  mk <- which(metas == "pelsa_mk")
  list(background = if (length(bg)) bg[1L] - 1L else NA_integer_,
       markers    = if (length(mk)) mk[1L] - 1L else NA_integer_)
}

# Strip a trailing UniProt isoform suffix ("-2") to the base accession. @noRd
.pelsa_iso_base <- function(x) sub("-[0-9]+$", "", as.character(x))

# Match a typed accession against the volcano df. A peptide matches when its
# winning_accession OR any PG.ProteinAccessions token equals the input, OR shares
# its isoform base. Case-insensitive, trimmed.
# @return list(mask=<logical over df rows>, accessions=<distinct matched
#   winning_accession>, count=<# matched rows>). @noRd
pelsa_volcano_find_mask <- function(df, accession) {
  n <- if (is.data.frame(df)) nrow(df) else 0L
  empty <- list(mask = rep(FALSE, n), accessions = character(0), count = 0L)
  if (n == 0L) return(empty)
  q <- toupper(trimws(as.character(accession)[1L] %||% ""))
  if (is.na(q) || !nzchar(q)) return(empty)
  qbase <- .pelsa_iso_base(q)

  wacc <- toupper(as.character(df$winning_accession %||% rep(NA, n)))
  wbase <- .pelsa_iso_base(wacc)
  pg <- toupper(as.character(df$PG.ProteinAccessions %||% rep(NA, n)))

  hit <- (!is.na(wacc) & (wacc == q | wbase == qbase))
  pg_hit <- vapply(seq_len(n), function(i) {
    if (is.na(pg[i]) || !nzchar(pg[i])) return(FALSE)
    toks <- trimws(strsplit(pg[i], ";", fixed = TRUE)[[1]])
    any(toks == q | .pelsa_iso_base(toks) == qbase)
  }, logical(1))
  mask <- hit | pg_hit
  mask[is.na(mask)] <- FALSE
  accs <- unique(as.character(df$winning_accession)[mask])
  list(mask = mask, accessions = accs[!is.na(accs) & nzchar(accs)],
       count = sum(mask))
}

# Build the pinned-panel metadata as a 2-column (label, value) data.frame from a
# volcano-df row. The Peptide label is the winning-accession label
# "<winning_gene>_aa<pep_start>" (gene->accession fallback when gene is empty).
# n_peptides is the count the caller computed (distinct peptides PLOTTED for this
# accession in the active contrast). @noRd
pelsa_pin_metadata_rows <- function(volcano_df, row, n_peptides) {
  r <- volcano_df[row, , drop = FALSE]
  acc_fb <- if (!is.na(r$winning_accession) && nzchar(r$winning_accession))
    r$winning_accession else as.character(r$PG.ProteinAccessions)[1L]
  gene <- if (!is.na(r$winning_gene) && nzchar(r$winning_gene))
    r$winning_gene else as.character(r$PG.Genes)[1L]
  gene_disp <- if (is.na(gene) || !nzchar(gene)) "NA" else gene
  label_stem <- if (gene_disp == "NA") acc_fb else gene_disp
  pep_label <- paste0(label_stem, "_aa", r$pep_start)
  data.frame(
    label = c("Peptide", "Accession", "Gene",
              "Quantified peptides (this contrast)", "Sequence", "Position",
              "adj.P", "logFC"),
    value = c(pep_label, acc_fb, gene_disp, as.character(as.integer(n_peptides)),
              as.character(r$id),
              paste0(r$pep_start, "-", r$pep_end),
              sprintf("%.2g", r$adj.P.Val), sprintf("%.2g", r$logFC)),
    stringsAsFactors = FALSE
  )
}
