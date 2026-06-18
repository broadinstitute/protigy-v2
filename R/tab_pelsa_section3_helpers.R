################################################################################
# Module: PELSA Section 3 (Volcano) - pure, testable plot-assembly helpers.
#
# The Section-3 module server (R/tab_pelsa_section3.R) is intentionally thin:
# every piece of logic that can be tested closed-form (contrast-key building,
# the display-label <-> stat-column-suffix mapping, the stat_df <-> cache join,
# the label-mode row selection, the color-mode column pick, and the marker /
# background trace split) lives here so it unit-tests with NO Shiny session.
#
# The PELSA volcano does NOT compute differential statistics (Decision A): the
# Statistics tab supplies stat_results()[[ome]] carrying the peptide rdesc
# columns PLUS contrast-suffixed stat columns
#   logFC.<g1>_over_<g2> / adj.P.Val.<g1>_over_<g2> / P.Value.<g1>_over_<g2>
# (see R/tab_stat_setup_helpers.R). The display label is "<g1> / <g2>".
#
# These helpers feed the 3A pelsa_build_volcano_df() builder (which needs
# pep_start / pep_end on its stat_df and joins to the cache's matched frame) and
# the 3B pelsa_thin_background() thinner.
################################################################################

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

# Default per-contrast label mode and top-N. Default is "none" - a clean plot
# out of the box; the user opts into labels via the sidebar radio.
.PELSA_VOLCANO_DEFAULT_LABEL_MODE <- "none"
.PELSA_VOLCANO_DEFAULT_TOP_N      <- 3L

# The gold used to highlight a selected/pinned peptide (legend entry, Woods
# cross-highlight). Distinct from the magenta marker fill.
.PELSA_VOLCANO_GOLD <- .PELSA_GOLD

# ---- contrast key + label/suffix mapping ------------------------------------

# Resolve which Woods peptide a plotly_click selected, by coordinate.
#
# Candidates are the peptides whose [pep_start, pep_end] span contains the click
# x; if the click x is NULL/NA or in no span, ALL peptides are candidates. Among
# the candidates, pick the one whose logFC is nearest the click y. A NULL OR NA
# click y falls back to each candidate's own logFC (distance 0), so the first
# candidate is chosen -- never letting an NA y collapse which.min() to
# integer(0) (the bug this guards).
#
# @param pep   data.frame with pep_start, pep_end, logFC (the Woods peptide set).
# @param ev_x  click x coordinate (numeric scalar) or NULL.
# @param ev_y  click y coordinate (numeric scalar), or NULL/NA.
# @return integer row index into `pep` (length 1), or NULL when pep is empty.
# @noRd
.pelsa_woods_click_index <- function(pep, ev_x, ev_y) {
  n <- nrow(pep)
  if (is.null(n) || n == 0L) return(NULL)
  in_span <- if (is.null(ev_x) || length(ev_x) != 1L || is.na(ev_x)) {
    rep(FALSE, n)
  } else {
    pep$pep_start <= ev_x & ev_x <= pep$pep_end
  }
  cand <- which(in_span)
  if (!length(cand)) cand <- seq_len(n)
  # NA or NULL y -> use each candidate's own logFC (distance 0 everywhere).
  y_ref <- if (is.null(ev_y) || length(ev_y) != 1L || is.na(ev_y)) {
    pep$logFC[cand]
  } else {
    ev_y
  }
  j <- cand[which.min(abs(pep$logFC[cand] - y_ref))]
  # which.min returns integer(0) when every candidate distance is NA (e.g. all
  # candidate logFC are NA); honor the "length-1 or NULL" contract so the caller
  # never indexes pep with integer(0).
  if (!length(j)) return(NULL)
  j
}

# Build the registry key "<ome>::<contrast>" for a per-contrast registry slot.
#
# `contrast` is the STAT-COLUMN SUFFIX (e.g. "A_over_B"), not the display label,
# so a registry slot maps 1:1 to the columns 3A reads. NULL/empty contrast
# yields NULL (no key - the caller gates on this).
#
# @param ome      the dataset/ome name.
# @param contrast the stat-column suffix, or NULL.
# @return "<ome>::<contrast>" or NULL.
# @noRd
pelsa_volcano_contrast_key <- function(ome, contrast) {
  if (is.null(ome) || length(ome) != 1L || is.na(ome) || !nzchar(ome)) {
    return(NULL)
  }
  if (is.null(contrast) || length(contrast) != 1L || is.na(contrast) ||
      !nzchar(contrast)) {
    return(NULL)
  }
  paste0(ome, "::", contrast)
}

# Convert a Statistics-tab contrast DISPLAY label ("<g1> / <g2>") to the
# stat-column SUFFIX ("<g1>_over_<g2>") used in stat_results column names and as
# the `contrast` argument to pelsa_build_volcano_df().
#
# @param label a contrast display label, or vector thereof.
# @return the corresponding stat-column suffix(es).
# @noRd
pelsa_volcano_label_to_suffix <- function(label) {
  label <- as.character(label)
  gsub(" / ", "_over_", label, fixed = TRUE)
}

# Build the contrast selector choices for an ome from the Statistics-tab params.
#
# Returns a NAMED character vector: names are the display labels ("A / B")
# shown in the selectInput, values are the stat-column suffixes ("A_over_B")
# threaded through to 3A. Only the Two-sample Moderated T-test produces a
# volcano with contrasts; one-sample/F/None yield character(0) (the caller
# shows the appropriate notice). Order follows stat_params order (the
# Statistics-tab contrast list), so the PELSA selector mirrors that tab.
#
# @param stat_params the Statistics-tab stat_params list (keyed by ome).
# @param ome         the active dataset/ome.
# @return named character vector label -> suffix (possibly empty).
# @noRd
pelsa_volcano_contrast_choices <- function(stat_params, ome) {
  if (is.null(stat_params) || is.null(ome)) return(character(0))
  sp <- stat_params[[ome]]
  if (is.null(sp)) return(character(0))
  test <- sp$test
  if (is.null(test) || length(test) != 1L ||
      test != "Two-sample Moderated T-test") {
    return(character(0))
  }
  labels <- sp$contrasts %||% character(0)
  labels <- as.character(labels)
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (length(labels) == 0L) return(character(0))
  suffixes <- pelsa_volcano_label_to_suffix(labels)
  stats::setNames(suffixes, labels)
}

# Does stat_results()[[ome]] carry the three contrast-suffixed stat columns?
# A cheap gate the module uses before calling 3A (3A itself errors loudly).
#
# @param stat_df  stat_results()[[ome]] (a data.frame) or NULL.
# @param contrast the stat-column suffix.
# @return TRUE iff all three logFC./adj.P.Val./P.Value. columns are present.
# @noRd
pelsa_volcano_has_contrast <- function(stat_df, contrast) {
  if (!is.data.frame(stat_df)) return(FALSE)
  if (is.null(contrast) || length(contrast) != 1L || is.na(contrast) ||
      !nzchar(contrast)) {
    return(FALSE)
  }
  need <- c(paste0("logFC.", contrast),
            paste0("adj.P.Val.", contrast),
            paste0("P.Value.", contrast))
  all(need %in% colnames(stat_df))
}

# ---- stat_df assembly (attach the representative pep_start/pep_end span) -----

# Attach a representative pep_start / pep_end span (and PG.Genes) to the
# Statistics-tab per-peptide frame so it satisfies the 3A pelsa_build_volcano_df
# contract.
#
# stat_results()[[ome]] is built from the GCT rdesc joined to the limma stat
# columns; it carries PEP.StrippedSequence / PG.ProteinAccessions (and usually
# PG.Genes) but NOT pep_start / pep_end (those are synthesized in the cache's
# matched frame during explode/mapping). This joins the LEADING (smallest
# pep_start) occurrence per peptide from the matched cache, keyed by
# PEP.StrippedSequence, so 3A's tooltip span + feature annotation have a
# representative coordinate. Peptides absent from the matched cache get NA span
# (3A's feature annotation tolerates NA -> "none").
#
# ID-COLUMN FALLBACK: peptide-result datasets key on PEP.StrippedSequence, so a
# PELSA dataset that used the stripped sequence AS its id column (rid) has no
# PEP.StrippedSequence in stat_results() - stat.testing carries that rid in the
# `id` column instead. When PEP.StrippedSequence is absent we synthesize it from
# `id`, exactly as the analysis pipeline does for the matched cache (both come
# from the same rid), so the join key lines up end-to-end. We only error when
# neither column is present (a genuinely malformed stat frame).
#
# Pure: a function of its two data.frame args; no Shiny.
#
# @param stat_df       stat_results()[[ome]] (per-peptide, contrast-suffixed).
# @param matched_cache the cache's $matched frame (peptide x accession x occ).
# @return stat_df with pep_start / pep_end columns added (PG.Genes ensured).
# @noRd
pelsa_volcano_stat_df <- function(stat_df, matched_cache) {
  if (!is.data.frame(stat_df)) {
    stop("pelsa_volcano_stat_df: stat_df must be a data.frame")
  }
  if (!is.data.frame(matched_cache)) {
    stop("pelsa_volcano_stat_df: matched_cache must be a data.frame")
  }
  if (!"PEP.StrippedSequence" %in% colnames(stat_df)) {
    # Fall back to the id column (the rid) the matched cache also keyed on.
    if ("id" %in% colnames(stat_df)) {
      stat_df <- .pelsa_ensure_stripped_sequence(
        stat_df, id_values = stat_df[["id"]])
    }
    if (!"PEP.StrippedSequence" %in% colnames(stat_df)) {
      stop("pelsa_volcano_stat_df: stat_df must have PEP.StrippedSequence ",
           "(or an 'id' column to derive it from)")
    }
  }
  out <- stat_df
  if (!"PG.Genes" %in% colnames(out)) out$PG.Genes <- NA_character_

  n <- nrow(out)
  # Representative span: leading occurrence (smallest pep_start) per peptide.
  has_span <- all(c("PEP.StrippedSequence", "pep_start", "pep_end") %in%
                    colnames(matched_cache))
  if (!has_span || nrow(matched_cache) == 0L) {
    out$pep_start <- rep(NA_integer_, n)
    out$pep_end   <- rep(NA_integer_, n)
    return(out)
  }

  m <- data.frame(
    seq       = as.character(matched_cache[["PEP.StrippedSequence"]]),
    pep_start = as.integer(matched_cache[["pep_start"]]),
    pep_end   = as.integer(matched_cache[["pep_end"]]),
    stringsAsFactors = FALSE
  )
  # Order so the leading (smallest start) occurrence per peptide is first, then
  # take the first row per sequence.
  m <- m[order(m$seq, m$pep_start, na.last = TRUE), , drop = FALSE]
  first <- !duplicated(m$seq)
  rep_span <- m[first, , drop = FALSE]

  idx <- match(as.character(out$PEP.StrippedSequence), rep_span$seq)
  out$pep_start <- rep_span$pep_start[idx]
  out$pep_end   <- rep_span$pep_end[idx]
  out
}

# ---- color-mode column pick -------------------------------------------------

# Pick the per-point color vector for the chosen color mode.
#
# ONE source of truth for the single color toggle:
#   "significance" (default, two-sided): the 3A sig_color column
#       (up = darkred, down = #1f4e9c blue, ns = gray).
#   "feature":      the 3A feature_color column (the 9-bucket UniProt class).
#
# @param volcano_df a 3A pelsa_build_volcano_df() frame.
# @param mode       "significance" | "feature".
# @return character vector of hex/named colors, length nrow(volcano_df).
# @noRd
pelsa_volcano_color_column <- function(volcano_df, mode = "significance") {
  if (!is.data.frame(volcano_df)) {
    stop("pelsa_volcano_color_column: volcano_df must be a data.frame")
  }
  mode <- mode %||% "significance"
  if (length(mode) != 1L || is.na(mode) ||
      !mode %in% c("significance", "feature")) {
    stop("pelsa_volcano_color_column: mode must be 'significance' or 'feature'")
  }
  col <- if (mode == "feature") "feature_color" else "sig_color"
  if (!col %in% colnames(volcano_df)) {
    stop("pelsa_volcano_color_column: volcano_df missing column '", col, "'")
  }
  as.character(volcano_df[[col]])
}

# ---- marker / background trace split ----------------------------------------

# Split a volcano frame into the marker rows (drawn magenta, on top, ALWAYS) and
# the non-marker background rows. The split is run on the FULL frame the plot
# consumes (every point - the volcano applies no background thinning).
#
# @param volcano_df a 3A frame carrying logical is_marker.
# @return list(markers = <rows where is_marker>, background = <the rest>),
#         each a data.frame with reset rownames.
# @noRd
pelsa_volcano_marker_split <- function(volcano_df) {
  if (!is.data.frame(volcano_df)) {
    stop("pelsa_volcano_marker_split: volcano_df must be a data.frame")
  }
  if (!"is_marker" %in% colnames(volcano_df)) {
    stop("pelsa_volcano_marker_split: volcano_df missing is_marker")
  }
  is_m <- volcano_df$is_marker
  is_m[is.na(is_m)] <- FALSE
  markers <- volcano_df[is_m, , drop = FALSE]
  background <- volcano_df[!is_m, , drop = FALSE]
  rownames(markers) <- NULL
  rownames(background) <- NULL
  list(markers = markers, background = background)
}

# ---- label-mode row selection -----------------------------------------------

# Select which rows of a volcano frame get an on-plot text label, for a given
# label mode. Labels are FIXED to the 3A `label` column (the ;-joined
# <gene>_aa<pos>); only WHICH rows are labeled varies.
#
# Modes:
#   "none"            no labels (integer(0)).
#   "all_markers"     every marker-protein peptide (is_marker == TRUE).
#   "all_significant" every significant peptide (Significant == TRUE).
#   "best_per_marker" one peptide per marker PROTEIN (winning_accession): the
#                     smallest adj.P.Val within each marker protein.
#   "top_n"           the N peptides with the smallest adj.P.Val per PROTEIN
#                     (winning_accession), across ALL proteins (default N=3).
#
# Returns the 1-based row indices to label (sorted, unique). Ties in adj.P.Val
# break by row order (stable). NA adj.P.Val sorts last.
#
# @param volcano_df a 3A frame (label, is_marker, Significant, adj.P.Val,
#                   winning_accession).
# @param mode       one of the five modes above.
# @param n_top      N for "top_n" (default 3, coerced to >= 1).
# @return integer vector of row indices to label.
# @noRd
.PELSA_VOLCANO_LABEL_MODES <- c("none", "all_markers", "all_significant",
                                "best_per_marker", "top_n")

pelsa_volcano_label_rows <- function(volcano_df, mode = "top_n", n_top = 3L) {
  if (!is.data.frame(volcano_df)) {
    stop("pelsa_volcano_label_rows: volcano_df must be a data.frame")
  }
  mode <- mode %||% "top_n"
  if (length(mode) != 1L || is.na(mode) ||
      !mode %in% .PELSA_VOLCANO_LABEL_MODES) {
    stop("pelsa_volcano_label_rows: mode must be one of ",
         paste(sprintf("'%s'", .PELSA_VOLCANO_LABEL_MODES), collapse = ", "))
  }
  n <- nrow(volcano_df)
  if (n == 0L || mode == "none") return(integer(0))

  is_m <- volcano_df$is_marker %||% rep(FALSE, n)
  is_m[is.na(is_m)] <- FALSE

  if (mode == "all_markers") {
    return(which(is_m))
  }

  if (mode == "all_significant") {
    sig <- volcano_df$Significant %||% rep(FALSE, n)
    sig[is.na(sig)] <- FALSE
    return(which(sig))
  }

  adjp <- as.numeric(volcano_df$adj.P.Val %||% rep(NA_real_, n))
  acc  <- as.character(volcano_df$winning_accession %||% rep(NA_character_, n))

  if (mode == "best_per_marker") {
    marker_idx <- which(is_m)
    if (length(marker_idx) == 0L) return(integer(0))
    # Group marker rows by protein, keep the smallest-adjp row per protein.
    return(.pelsa_top_per_group(marker_idx, acc[marker_idx],
                                adjp[marker_idx], n_top = 1L))
  }

  # mode == "top_n": top-N smallest adj.P.Val per protein across all rows.
  n_top <- max(1L, as.integer(n_top)[1L])
  if (is.na(n_top)) n_top <- .PELSA_VOLCANO_DEFAULT_TOP_N
  .pelsa_top_per_group(seq_len(n), acc, adjp, n_top = n_top)
}

# Keep the n_top rows with the smallest `value` within each group of `key`.
# `idx` are the original row indices these (key, value) entries correspond to.
# Stable: ties / NA values resolve by original index order; NA values sort last.
#
# @return sorted unique original indices kept.
# @noRd
.pelsa_top_per_group <- function(idx, key, value, n_top) {
  if (length(idx) == 0L) return(integer(0))
  # Stable order by (value asc, idx asc); NA value last.
  ord <- order(value, idx, na.last = TRUE)
  idx_o <- idx[ord]
  key_o <- key[ord]
  # Within each group (in this sorted order), rank position.
  rank_in_grp <- stats::ave(seq_along(key_o), key_o, FUN = seq_along)
  kept <- idx_o[rank_in_grp <= n_top]
  sort(unique(kept))
}

# ---- "showing N of M" honesty note ------------------------------------------

# Build the human-readable background-thinning note from a 3B result.
#
# KEPT-BUT-UNWIRED: the volcano no longer thins its background (per user
# decision - toWebGL renders all points), so this note is not shown in the UI.
# Retained (alongside the 3B pelsa_thin_background helper) for callers that may
# still thin, and covered by its own test.
#
# @param thin a pelsa_thin_background() list (n_shown / n_total / ...).
# @return a single string, or NULL when nothing was thinned.
# @noRd
pelsa_volcano_thin_note <- function(thin) {
  if (!is.list(thin) || is.null(thin$n_shown) || is.null(thin$n_total)) {
    return(NULL)
  }
  if (thin$n_shown >= thin$n_total) return(NULL)
  sprintf("Showing %s of %s points (dense non-significant background thinned).",
          format(thin$n_shown, big.mark = ","),
          format(thin$n_total, big.mark = ","))
}

################################################################################
# PASS 2 (7D-7F) pure, testable helpers
################################################################################

# ---- 7F: the 12-column volcano-labels sidecar CSV shaping --------------------

# Shape a 3A volcano data.frame into the EXACT 12-column sidecar CSV the plan
# specifies (one row per plotted dot). Pure: a function of the df + the panel tag.
#
# Columns, in order:
#   panel, peptide_sequence, gene, accession, pep_start, display_label,
#   feature_class_primary, winning_accession, winning_gene, logFC, adj_p, raw_p
#
# Source mapping from the 3A frame:
#   panel                 <- the supplied panel tag ("all_peptide"/"best_peptide")
#   peptide_sequence      <- id
#   gene                  <- PG.Genes
#   accession             <- PG.ProteinAccessions
#   pep_start             <- pep_start
#   display_label         <- label
#   feature_class_primary <- feature_class_primary
#   winning_accession     <- winning_accession
#   winning_gene          <- winning_gene
#   logFC                 <- logFC
#   adj_p                 <- adj.P.Val
#   raw_p                 <- P.Value
#
# @param volcano_df a 3A pelsa_build_volcano_df() frame.
# @param panel      the panel label written into the `panel` column.
# @return a 12-column data.frame (zero-row but full-width on an empty df).
# @noRd
pelsa_volcano_labels_sidecar <- function(volcano_df, panel = "all_peptide") {
  if (!is.data.frame(volcano_df)) {
    stop("pelsa_volcano_labels_sidecar: volcano_df must be a data.frame")
  }
  panel <- as.character(panel)[1L]
  n <- nrow(volcano_df)
  col <- function(name, type = "character") {
    if (name %in% colnames(volcano_df)) return(volcano_df[[name]])
    switch(type,
           character = rep(NA_character_, n),
           integer   = rep(NA_integer_, n),
           numeric   = rep(NA_real_, n))
  }
  out <- data.frame(
    panel                 = rep(panel, n),
    peptide_sequence      = as.character(col("id")),
    gene                  = as.character(col("PG.Genes")),
    accession             = as.character(col("PG.ProteinAccessions")),
    pep_start             = as.integer(col("pep_start", "integer")),
    display_label         = as.character(col("label")),
    feature_class_primary = as.character(col("feature_class_primary")),
    winning_accession     = as.character(col("winning_accession")),
    winning_gene          = as.character(col("winning_gene")),
    logFC                 = as.numeric(col("logFC", "numeric")),
    adj_p                 = as.numeric(col("adj.P.Val", "numeric")),
    raw_p                 = as.numeric(col("P.Value", "numeric")),
    stringsAsFactors      = FALSE,
    check.names           = FALSE
  )
  rownames(out) <- NULL
  out
}

# ---- volcano hover-tip (shared by the base build + the gold overlay) --------

# Build the 6-line volcano hover text for a set of df rows. Factored out of
# pelsa_volcano_build_plot so the gold OVERLAY trace (pelsa_volcano_gold_trace,
# pushed via plotlyProxyInvoke("addTraces")) gets the IDENTICAL hover as the base
# background/marker traces. Pure: a function of its data.frame arg. @noRd
pelsa_volcano_tip <- function(d) {
  if (nrow(d) == 0L) return(character(0))
  no_span <- is.na(d$pep_start) | is.na(d$pep_end)
  pos <- ifelse(no_span, "unknown", paste0(d$pep_start, "-", d$pep_end))
  gene_fb <- ifelse(is.na(d$winning_gene) | !nzchar(d$winning_gene),
                    d$PG.Genes, d$winning_gene)
  acc_fb <- ifelse(is.na(d$winning_accession) | !nzchar(d$winning_accession),
                   d$PG.ProteinAccessions, d$winning_accession)
  stem <- ifelse(is.na(gene_fb) | !nzchar(gene_fb), acc_fb, gene_fb)
  pep_lab <- paste0(stem, "_aa", d$pep_start)
  lfc_chr  <- ifelse(is.na(d$logFC), "NA", sprintf("%.2f", d$logFC))
  adjp_chr <- ifelse(is.na(d$adj.P.Val), "NA", sprintf("%.2g", d$adj.P.Val))
  paste0("Peptide: ", pep_lab, "<br>",
         "Accession: ", acc_fb, "<br>",
         "Gene: ", ifelse(is.na(gene_fb) | !nzchar(gene_fb), "NA", gene_fb), "<br>",
         "Position: ", pos, "<br>",
         "logFC: ", lfc_chr, "<br>",
         "adj.P: ", adjp_chr)
}

# Build the gold-highlight OVERLAY scattergl trace (a plain list, ready for
# plotlyProxyInvoke("addTraces", ...)) for the selection/find highlight: gold
# fill + black outline at marker size, with the standard 6-line hover. Returns
# NULL when nothing is highlighted.
#
# The marker `size` here (7) MUST match the build's gold/marker px
# (pelsa_volcano_build_plot's gold_px == mk_px == 7) so the proxy-pushed overlay
# visually matches the gold the static export build bakes. @noRd
pelsa_volcano_gold_trace <- function(df, selection = NULL, find_mask = NULL) {
  if (!is.data.frame(df) || nrow(df) == 0L) return(NULL)
  m <- pelsa_volcano_highlight_mask(df, selection, find_mask)
  if (!any(m)) return(NULL)
  d <- df[m, , drop = FALSE]
  list(
    type = "scattergl", mode = "markers",
    x = as.numeric(d$logFC), y = as.numeric(d$logP),
    text = pelsa_volcano_tip(d), hoverinfo = "text",
    marker = list(color = .PELSA_GOLD, size = 7,
                  line = list(color = .PELSA_VOLCANO_MARKER_EDGE, width = 0.5)),
    showlegend = FALSE, meta = "pelsa_gold"
  )
}

# Build the dark-gold LABEL overlay trace for the CLICKED peptide only (NOT its
# siblings): a one-point scattergl "text+markers" trace, ready for
# plotlyProxyInvoke("addTraces", ...). The label is "<gene>_aa<pep_start>" built
# with the SAME stem logic as pelsa_volcano_tip (gene -> accession fallback;
# self-curated rows already carry a blanked winning_gene so the accession
# fallback fires). When pep_start is unknown (NA) the label is the stem alone (no
# "_aaNA" suffix). Dark-gold text over a white halo marker so the text reads
# against the gold dot beneath it - the mode is "text+markers" because a
# text-only scattergl trace draws NO marker, so the halo would not render. (A
# true boxed annotation is not available on a scattergl trace; proxy
# relayout(annotations=) is unreliable on this WebGL volcano.) Returns NULL when
# nothing is selected or the clicked row cannot be resolved (e.g. a
# multi-accession Find that sets selection() to NULL). @noRd
pelsa_volcano_clicked_label_trace <- function(df, selection = NULL) {
  if (!is.data.frame(df) || nrow(df) == 0L || is.null(selection)) return(NULL)
  row <- selection$row
  if (is.null(row) || length(row) != 1L || is.na(row)) {
    seq <- selection$peptide_seq
    if (is.null(seq) || length(seq) != 1L || is.na(seq) || !nzchar(seq)) {
      return(NULL)
    }
    row <- match(as.character(seq), as.character(df$id))
  }
  # selection$row is trusted to index THIS df: the caller (apply_gold_overlay)
  # reads the same active_volcano_df() the click resolved against, and the
  # base-rebuild observer re-resolves after any reorder. The Woods path carries
  # row=NA and is re-resolved by peptide_seq above, so it is never stale.
  if (is.na(row) || row < 1L || row > nrow(df)) return(NULL)
  d <- df[row, , drop = FALSE]
  if (is.na(d$logFC) || is.na(d$logP)) return(NULL)

  gene_fb <- ifelse(is.na(d$winning_gene) | !nzchar(d$winning_gene),
                    d$PG.Genes, d$winning_gene)
  acc_fb <- ifelse(is.na(d$winning_accession) | !nzchar(d$winning_accession),
                   d$PG.ProteinAccessions, d$winning_accession)
  stem <- ifelse(is.na(gene_fb) | !nzchar(gene_fb), acc_fb, gene_fb)
  if (is.na(stem) || !nzchar(stem)) return(NULL)
  # No "_aaNA" cruft when the residue position is unknown: stem alone.
  label <- if (is.na(d$pep_start)) stem else paste0(stem, "_aa", d$pep_start)

  list(
    type = "scattergl", mode = "text+markers",
    x = as.numeric(d$logFC), y = as.numeric(d$logP),
    text = label, textposition = "top right",
    textfont = list(color = .PELSA_GOLD_DARK, size = 11, family = "Arial"),
    marker = list(color = "rgba(255,255,255,0.9)", size = 14,
                  line = list(width = 0)),
    hoverinfo = "skip", showlegend = FALSE, meta = "pelsa_gold_label"
  )
}

# ---- shared plot-assembly (BOTH volcano panels reuse this) ------------------

# Assemble the WebGL volcano plotly object from the FULL volcano frame (every
# point - no thinning; toWebGL renders the whole cloud on the GPU). The
# all-peptide AND best-peptide panels call this with the same arguments and a
# distinct `source` id, so the plot code is written ONCE.
#
# Trace order is z-order only (later traces draw ON TOP):
#   1. background (non-marker)  - the dense cloud
#   2. markers    (magenta overlay, on top, ALWAYS)
#   (+ a geom_text label layer + an optional threshold hline)
# The build ALWAYS emits exactly TWO point traces (background + markers); the
# bg + marker traces are meta-tagged ("pelsa_bg"/"pelsa_mk") so the selection
# highlight can be applied client-side via a plotlyProxy "restyle" of their
# fill/ring arrays (see pelsa_volcano_recolor + .pelsa_volcano_trace_index and
# the plotly_click observer in tab_pelsa_section3.R) WITHOUT rebuilding the
# ~100k-point figure. There is exactly ONE interactive highlight mechanism: the
# proxy restyle.
#
# @param df          the FULL volcano frame the plot consumes (every point).
# @param full_df     the same frame, used for the y_cutoff attr + label-row
#   selection over all rows. Defaults to df.
# @param color_mode  "significance" | "feature".
# @param label_mode  a pelsa_volcano_label_rows() mode.
# @param n_top       N for top_n label mode.
# @param source_id   the plotly source id (ns("pelsa_volcano") /
#   ns("pelsa_volcano_best")).
# @param selection   NULL, or a list(origin, accession, peptide_seq) - the
#   active selection whose gold highlight is BAKED into the build.
# @param find_mask   NULL, or a logical over df rows - the multi-accession Find
#   highlight (uniform gold fill), baked into the build.
# @param register_click  TRUE -> event_register the plotly_click on this source.
# @return a built plotly object (native scattergl traces, no ggplotly/toWebGL).
# @noRd
pelsa_volcano_build_plot <- function(df, full_df = df,
                                     color_mode = "significance",
                                     label_mode = "top_n", n_top = 3L,
                                     source_id = "pelsa_volcano",
                                     selection = NULL, find_mask = NULL,
                                     register_click = FALSE) {
  if (!is.data.frame(df)) {
    stop("pelsa_volcano_build_plot: df must be a data.frame")
  }
  color_mode <- color_mode %||% "significance"

  split <- pelsa_volcano_marker_split(df)
  bg     <- split$background
  mk     <- split$markers

  # The selection/find highlight is baked into the build (rebuild-on-select:
  # per-point marker.color restyle is unreliable on WebGL scattergl, so the gold
  # is drawn into the figure itself). See the highlight-overlay geoms below.

  # The 6-line hover is shared with the gold overlay trace via the top-level
  # pelsa_volcano_tip() helper (so base + overlay hovers are identical).
  tip <- pelsa_volcano_tip

  # Highlight mask over the FULL df (selected + same-protein + find-matched). All
  # highlighted points are styled IDENTICALLY: gold fill + black outline, SAME
  # size as their base point (no selected-vs-sibling split, no size bump).
  hl_mask <- pelsa_volcano_highlight_mask(df, selection, find_mask)
  bg_hl <- if (nrow(bg) > 0L)
    pelsa_volcano_highlight_mask(bg, selection, find_mask) else logical(0)
  mk_hl <- if (nrow(mk) > 0L)
    pelsa_volcano_highlight_mask(mk, selection, find_mask) else logical(0)

  # ---- native plot_ly scattergl build (replaces the slow ggplotly path) ------
  # Trace z-order (later traces draw ON TOP):
  #   0. background cloud (sig/feature colors)
  #   1. magenta markers (ALWAYS on top of the cloud)
  #   2+. gold highlight overlays (selection/find), drawn over everything
  # The marker/background traces are meta-tagged so the recolor proxy restyle
  # finds them by index (see .pelsa_volcano_trace_index). With a hand-built
  # figure the trace order is deterministic, so the bg + marker traces are added
  # FIRST and in that order (always index 0 and 1). The scalar `meta` tag is
  # stamped AFTER plotly_build (a trace-level `meta=` arg would be recycled to a
  # per-point vector by plot_ly's data-mapping); stamping it on the built trace
  # keeps it a true scalar that survives Shiny's serialize-time re-build, so no
  # RGB tag-detection loop is needed.
  #
  # Sizes: ggplot point `size` is in mm, plotly `size` is in px; the px values
  # below were tuned against the previous ggplotly render so the cloud/marker/
  # gold dots match visually. The marker:bg ratio (~1.6/1.1) and gold == marker
  # size are preserved.
  bg_px   <- 5
  mk_px   <- 7
  gold_px <- mk_px

  p <- plotly::plot_ly(source = source_id)

  # 0. BACKGROUND cloud (always added so the bg trace exists at index 0; an empty
  #    frame yields an empty trace, which keeps the meta indices stable).
  bg_tip <- tip(bg)
  p <- plotly::add_trace(
    p, type = "scattergl", mode = "markers",
    x = bg$logFC, y = bg$logP,
    marker = list(
      color = pelsa_volcano_color_column(bg, color_mode),
      opacity = .PELSA_VOLCANO_BG_ALPHA, size = bg_px,
      line = list(width = 0)),
    text = bg_tip, hoverinfo = "text",
    showlegend = FALSE)

  # 1. MARKER overlay (magenta, ON TOP, ALWAYS). Non-highlighted markers keep
  #    their magenta fill even under an active selection/find.
  mk_tip <- tip(mk)
  p <- plotly::add_trace(
    p, type = "scattergl", mode = "markers",
    x = mk$logFC, y = mk$logP,
    marker = list(
      color = .PELSA_VOLCANO_MARKER_COLOR, size = mk_px,
      line = list(color = .PELSA_VOLCANO_MARKER_EDGE, width = 0.5)),
    text = mk_tip, hoverinfo = "text",
    showlegend = FALSE)

  # 2. GOLD highlight overlays (gold fill + black outline, marker size), drawn on
  #    top of EVERYTHING. Background-highlighted then marker-highlighted points.
  if (length(bg_hl) > 0L && any(bg_hl)) {
    hb <- bg[bg_hl, , drop = FALSE]
    p <- plotly::add_trace(
      p, type = "scattergl", mode = "markers",
      x = hb$logFC, y = hb$logP,
      marker = list(
        color = .PELSA_GOLD, size = gold_px,
        line = list(color = .PELSA_VOLCANO_MARKER_EDGE, width = 0.5)),
      text = tip(hb), hoverinfo = "text",
      showlegend = FALSE)
  }
  if (length(mk_hl) > 0L && any(mk_hl)) {
    hm <- mk[mk_hl, , drop = FALSE]
    p <- plotly::add_trace(
      p, type = "scattergl", mode = "markers",
      x = hm$logFC, y = hm$logP,
      marker = list(
        color = .PELSA_GOLD, size = gold_px,
        line = list(color = .PELSA_VOLCANO_MARKER_EDGE, width = 0.5)),
      text = tip(hm), hoverinfo = "text",
      showlegend = FALSE)
  }

  # Threshold line: a horizontal dashed grey40 line across the x-range, drawn as
  # a layout shape (NOT a trace) so it never perturbs the bg/marker trace indices.
  shapes <- list()
  y_cut <- attr(full_df, "y_cutoff")
  if (!is.null(y_cut) && is.finite(y_cut)) {
    shapes <- list(list(
      type = "line", xref = "paper", yref = "y",
      x0 = 0, x1 = 1, y0 = y_cut, y1 = y_cut,
      line = list(dash = "dash", color = "grey40", width = 1)))
  }

  # Labels are NOT drawn as a ggplot geom_text (that renders ON the point, hard
  # to read, and ggrepel does not survive ggplotly+toWebGL). Instead we collect
  # the labeled rows here and add them as native plotly boxed annotations AFTER
  # the build (white opaque-ish bg + a border colored to the labeled point), so
  # they survive toWebGL and read as clear callouts. See add_annotations below.
  lab_idx <- tryCatch(
    pelsa_volcano_label_rows(full_df, mode = label_mode, n_top = n_top),
    error = function(e) integer(0)
  )
  lab_df <- NULL
  if (length(lab_idx) > 0L) {
    lab_df <- full_df[lab_idx, , drop = FALSE]
    lab_df <- lab_df[!is.na(lab_df$label) & nzchar(lab_df$label), , drop = FALSE]
    if (nrow(lab_df) == 0L) lab_df <- NULL
  }

  # theme_bw look (white panel, light-grey gridlines, no zero-lines) + axis
  # titles. The threshold-line shape (if any) goes in here too. Trace `meta`
  # tags are set DIRECTLY above, so no post-build tag-detection loop is needed.
  p <- plotly::layout(
    p,
    xaxis = list(title = "logFC", zeroline = FALSE, showgrid = TRUE,
                 gridcolor = "grey92"),
    yaxis = list(title = "-log10(P.Value)", zeroline = FALSE, showgrid = TRUE,
                 gridcolor = "grey92"),
    plot_bgcolor = "white", paper_bgcolor = "white",
    shapes = shapes, showlegend = FALSE)

  # Build once now so the trace list is materialized, then stamp the SCALAR meta
  # tags on the (deterministic) bg/marker traces - index 0 = background,
  # index 1 = markers. A scalar set post-build survives a downstream re-build
  # (verified), so .pelsa_volcano_trace_index resolves them on both the returned
  # object AND plotly_build(p).
  p <- plotly::plotly_build(p)
  if (length(p$x$data) >= 1L) p$x$data[[1L]]$meta <- "pelsa_bg"
  if (length(p$x$data) >= 2L) p$x$data[[2L]]$meta <- "pelsa_mk"

  # Boxed labels (white opaque-ish bg, border = labeled point's own color),
  # offset from the point + overlap-suppressed (Statistics-tab scheme).
  if (!is.null(lab_df)) {
    p <- .pelsa_volcano_label_annotations(p, lab_df, color_mode,
                                          full_df = full_df)
  }
  if (isTRUE(register_click)) {
    p <- plotly::event_register(p, "plotly_click")
  }
  p
}

# Add boxed labels to a built volcano plotly as native annotations (so they
# survive toWebGL, which a ggplot geom_text/ggrepel layer would not). Mirrors the
# Statistics > Volcano interactive-label scheme (add_volcano_labels): each label
# is OFFSET up-and-right of its point (xshift/yshift, so the box never covers the
# point), a white slightly-transparent box with a 1px border colored to that
# point's OWN color (sig_color/feature_color), and a greedy proximity suppressor
# drops labels that would pile on top of an already-placed one (in normalized
# [0,1] coordinate space). The default best_per_marker / "none" modes keep the
# starting count low; the suppressor handles the rest.
#
# @param p          a built plotly (post-toWebGL) volcano.
# @param lab_df     the labeled rows (logFC, logP, label, + color columns).
# @param color_mode "significance" | "feature" (drives the border color).
# @param full_df    the full volcano df (for the normalization x/y ranges).
# @param min_dist   normalized-space proximity threshold to suppress overlaps.
# @return p with annotations added.
# @noRd
.pelsa_volcano_label_annotations <- function(p, lab_df, color_mode,
                                             full_df = lab_df, min_dist = 0.045) {
  anns <- pelsa_volcano_label_annotation_list(lab_df, color_mode, full_df,
                                              min_dist)
  if (length(anns) == 0L) return(p)
  plotly::layout(p, annotations = anns)
}

# Compute the boxed-label annotation LIST for a volcano (greedy overlap-
# suppressed, Statistics-tab scheme). Returns a list of plotly annotation specs
# (possibly empty) - PURE, no plot object. This is the authoritative annotation
# computation used both by the build wrapper above (baked into the figure) and
# by the module's relayout fast-path (applied via plotlyProxyInvoke without a
# rebuild). Each spec is offset up-and-right of its point (xshift/yshift, box
# never covers the point), a white slightly-transparent box with a 1px border
# colored to the labeled point's OWN color, and a greedy proximity suppressor
# drops labels that would pile on an already-placed one (normalized [0,1] space).
#
# @param lab_df     the labeled rows (logFC, logP, label, + color columns).
# @param color_mode "significance" | "feature" (drives the border color).
# @param full_df    the full volcano df (for the normalization x/y ranges).
# @param min_dist   normalized-space proximity threshold to suppress overlaps.
# @return a list of plotly annotation specs (empty list() when nothing kept).
# @noRd
pelsa_volcano_label_annotation_list <- function(lab_df, color_mode,
                                                full_df = lab_df,
                                                min_dist = 0.045) {
  if (is.null(lab_df) || nrow(lab_df) == 0L) return(list())

  # Normalize to [0,1] using the full plot's ranges (so "close" means close
  # on-screen, not in raw logFC/logP units).
  xr <- range(full_df$logFC, na.rm = TRUE)
  yr <- range(full_df$logP,  na.rm = TRUE)
  xs <- diff(xr); ys <- diff(yr)
  if (!is.finite(xs) || xs == 0) xs <- 1
  if (!is.finite(ys) || ys == 0) ys <- 1

  # Greedy placement: most-significant first (smallest adj.P.Val), drop any label
  # within min_dist of an already-placed one. Mirrors add_volcano_labels.
  adjp <- as.numeric(lab_df$adj.P.Val %||% rep(NA_real_, nrow(lab_df)))
  ord  <- order(adjp, na.last = TRUE)
  border_all <- pelsa_volcano_color_column(lab_df, color_mode)

  placed <- list(); keep <- integer(0)
  for (i in ord) {
    nx <- (lab_df$logFC[i] - xr[1]) / xs
    ny <- (lab_df$logP[i]  - yr[1]) / ys
    too_close <- FALSE
    for (pl in placed) {
      if (sqrt((nx - pl$nx)^2 + (ny - pl$ny)^2) < min_dist) {
        too_close <- TRUE; break
      }
    }
    if (!too_close) {
      placed <- c(placed, list(list(nx = nx, ny = ny)))
      keep <- c(keep, i)
    }
  }
  if (length(keep) == 0L) return(list())
  kept   <- lab_df[keep, , drop = FALSE]
  border <- border_all[keep]

  lapply(seq_len(nrow(kept)), function(i) {
    list(
      x = kept$logFC[i], y = kept$logP[i], text = kept$label[i],
      xref = "x", yref = "y",
      showarrow = FALSE,                 # offset, not a leader line (Stats-tab)
      xanchor = "left", yanchor = "bottom",
      xshift = 6, yshift = 4,            # float up-and-right of the point
      font = list(size = 10, color = "#222222", family = "Arial"),
      bgcolor = "rgba(255,255,255,0.85)",
      bordercolor = border[i], borderwidth = 1, borderpad = 2,
      captureevents = FALSE
    )
  })
}

# Compute the current volcano annotation LIST from the active df + label
# settings (the module relayout fast-path uses this). Resolves the labeled rows
# for `label_mode`/`n_top`, filters to rows with a non-empty `label`, then
# delegates to pelsa_volcano_label_annotation_list. Returns an EMPTY list() when
# the mode yields no labels (e.g. "none") - so an empty relayout clears ALL
# annotations on the client (the "remove stale labels" path). PURE + testable.
#
# @param df         the active volcano df.
# @param label_mode a pelsa_volcano_label_rows() mode.
# @param n_top      N for the top_n label mode.
# @param color_mode "significance" | "feature" (drives the border color).
# @return a list of plotly annotation specs (empty list() for no labels).
# @noRd
pelsa_volcano_current_annotations <- function(df, label_mode, n_top,
                                              color_mode) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(list())
  lab_idx <- tryCatch(
    pelsa_volcano_label_rows(df, mode = label_mode, n_top = n_top),
    error = function(e) integer(0))
  if (length(lab_idx) == 0L) return(list())
  lab_df <- df[lab_idx, , drop = FALSE]
  lab_df <- lab_df[!is.na(lab_df$label) & nzchar(lab_df$label), , drop = FALSE]
  if (nrow(lab_df) == 0L) return(list())
  pelsa_volcano_label_annotation_list(lab_df, color_mode, full_df = df)
}

# ---- 7F: the static export ggplot + the empty matched-cache frame -----------

# A canonical empty matched-cache frame (the columns 3A's all-peptide join
# reads), used when the active dataset has no matched rows so 3A still runs and
# yields an unlabeled (label = NA) frame rather than erroring. @noRd
pelsa_volcano_empty_matched <- function() {
  data.frame(
    PEP.StrippedSequence = character(0),
    accession            = character(0),
    gene                 = character(0),
    pep_start            = integer(0),
    pep_end              = integer(0),
    stringsAsFactors     = FALSE,
    check.names          = FALSE
  )
}

# Assemble the per-protein intensity LINE ggplot from 3C line data (the pinned
# panel's plot). One line per (peptide_seq, pep_occurrence_idx), colored by the
# end-of-line aa_label; marker proteins facet Significant/Non-significant (>1
# panel value), a non-marker single panel. Pure ggplot - the caller wraps it in
# ggplotly.
#
# The PINNED peptide (the one the user clicked) is highlighted: its line/points
# are drawn in GOLD and its legend entry is bolded + suffixed " (selected)", so
# it is easy to tell the clicked peptide apart from the other peptides mapped to
# the same protein. The facet strip labels are bold + sit ABOVE the panel (so
# the band never overlaps the lines).
#
# @param ld a pelsa_intensity_line_data() frame (condition factor, mean_log2,
#   peptide_seq, pep_occurrence_idx, aa_label, panel).
# @param pinned_label the pinned peptide's aa_label (e.g. "aa462") to highlight,
#   or NULL for no highlight.
# @return a ggplot object.
# @noRd
pelsa_intensity_line_ggplot <- function(ld, pinned_label = NULL) {
  # Clean per-point hover tooltip (built from the RAW columns before the pinned
  # remap mangles aa_label): aa_label, position start->end, sequence, condition,
  # mean intensity. pep_end may be NA (older caches) -> show only the start.
  pos_txt <- ifelse(is.na(ld$pep_end %||% NA),
                    as.character(ld$pep_start),
                    paste0(ld$pep_start, " -> ", ld$pep_end))
  ld$.tip <- paste0(
    ld$aa_label, "<br>",
    "Position: ", pos_txt, "<br>",
    "Sequence: ", ld$peptide_seq, "<br>",
    "Condition: ", as.character(ld$condition), "<br>",
    "Mean log2 intensity: ", sprintf("%.2f", ld$mean_log2)
  )

  # Order aa_labels by residue position so the legend reads ascending.
  pos <- suppressWarnings(as.integer(sub("^aa", "", ld$aa_label)))
  raw_lvl <- unique(ld$aa_label[order(pos, ld$aa_label)])

  # Relabel the pinned key in the DATA + the factor levels (so ggplotly carries
  # the bold "(selected)" text into the trace name - ggplotly uses the factor
  # level as the legend/trace name, not scale_*'s `labels=` arg). Bold via plotly
  # HTML (<b>); harmless plain text in a static ggplot.
  pinned_disp <- if (!is.null(pinned_label) && nzchar(pinned_label)) {
    paste0("<b>", pinned_label, " (selected)</b>")
  } else NA_character_
  # Guard the no-pin case: with pinned_label NULL, `x == pinned_label` is
  # length-0 and collapses ifelse() to a 0-row result (breaks the column
  # assignment). Return x unchanged when there is nothing to remap.
  remap <- function(x) {
    if (is.null(pinned_label) || is.na(pinned_disp)) return(x)
    ifelse(x == pinned_label, pinned_disp, x)
  }
  ld$aa_label <- remap(ld$aa_label)
  lvl <- remap(raw_lvl)
  ld$aa_label <- factor(ld$aa_label, levels = lvl)

  # Per-key colors: the pinned peptide gold, the rest from the default hue
  # palette.
  is_pinned_lvl <- !is.na(pinned_disp) & lvl == pinned_disp
  others <- lvl[!is_pinned_lvl]
  hues <- scales::hue_pal()(max(length(others), 1L))
  pal <- stats::setNames(rep(NA_character_, length(lvl)), lvl)
  pal[others] <- hues[seq_along(others)]
  if (any(is_pinned_lvl)) pal[lvl[is_pinned_lvl]] <- .PELSA_VOLCANO_GOLD

  gg <- ggplot2::ggplot(
    ld,
    ggplot2::aes(x = .data$condition, y = .data$mean_log2,
                 group = interaction(.data$peptide_seq,
                                     .data$pep_occurrence_idx),
                 color = .data$aa_label, text = .data$.tip)
  ) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(na.rm = TRUE, size = 1.4) +
    ggplot2::scale_color_manual(values = pal, drop = FALSE)
  # The legend is removed (the hover tooltip identifies each line), so mark the
  # SELECTED peptide's line with black-outlined points (gold fill + black ring) so
  # the user can still tell which line is the clicked one.
  if (any(is_pinned_lvl)) {
    sel_rows <- ld[ld$aa_label %in% lvl[is_pinned_lvl], , drop = FALSE]
    if (nrow(sel_rows) > 0L) {
      # `text` is a plotly tooltip aes, not a ggplot one; geom_point() emits a
      # DEFERRED "Ignoring unknown aesthetics: text" warning at construction
      # that escapes suppressWarnings at the ggplotly/build site, so muffle it
      # here where the layer is actually built.
      gg <- gg + suppressWarnings(ggplot2::geom_point(
        data = sel_rows, na.rm = TRUE, shape = 21, size = 2.2, stroke = 0.6,
        fill = .PELSA_VOLCANO_GOLD, color = "black",
        ggplot2::aes(x = .data$condition, y = .data$mean_log2,
                     group = interaction(.data$peptide_seq,
                                         .data$pep_occurrence_idx),
                     text = .data$.tip),
        inherit.aes = FALSE, show.legend = FALSE))
    }
  }
  # Marker proteins: facet Significant/Non-significant; non-marker -> single.
  # Extra TOP headroom (mult upper = 0.22) so the facet strip sits in blank space
  # above the data instead of overlapping the lines (ggplotly renders facet strips
  # as overlaid annotations; with scales="free_y" the panel can otherwise extend
  # right under the strip). panel.spacing keeps the two panels apart.
  if (length(unique(ld$panel)) > 1L) {
    gg <- gg +
      ggplot2::facet_wrap(~ .data$panel, ncol = 1, scales = "free_y") +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0.05, 0.22)))
  }
  gg +
    ggplot2::labs(x = NULL, y = "mean log2 intensity", color = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      # Legend removed: the floating hover tooltip identifies each peptide line.
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      strip.text = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_rect(fill = "grey92", color = NA),
      panel.spacing = ggplot2::unit(1.2, "lines")
    )
}

# Build the pinned intensity line PLOTLY (the render path).
#
# When a marker protein has BOTH significance groups, ggplot faceting through
# ggplotly mispositions the facet strip so it overlaps the data. To avoid that
# entirely, we render the two groups as a vertical plotly::subplot of two
# single-panel ggplots (each gets a bold title annotation in clear space, no
# strip). The single-group case is a plain ggplotly. Tooltip = the .tip column.
#
# @param ld           a pelsa_intensity_line_data() frame.
# @param pinned_label the pinned peptide's aa_label to highlight (or NULL).
# @return a plotly object.
# @noRd
pelsa_intensity_line_plot <- function(ld, pinned_label = NULL) {
  panels <- unique(as.character(ld$panel))
  if (length(panels) <= 1L) {
    # showlegend = FALSE: ggplotly does not always honor legend.position="none";
    # the floating hover tooltip identifies each peptide line.
    # plotly_build() is forced INSIDE suppressWarnings so the deferred ggplot
    # build (which emits "Ignoring unknown aesthetics: text" for the plotly
    # tooltip aes) is muffled here, not later in renderPlotly's print path.
    return(suppressWarnings(plotly::plotly_build(plotly::layout(
      plotly::ggplotly(
        pelsa_intensity_line_ggplot(ld, pinned_label = pinned_label),
        tooltip = "text"),
      showlegend = FALSE))))
  }
  # Stable order: Significant on top, Non-significant below.
  ord <- c("Significant", "Non-significant")
  panels <- c(intersect(ord, panels), setdiff(panels, ord))

  parts <- lapply(panels, function(pn) {
    sub <- ld[as.character(ld$panel) == pn, , drop = FALSE]
    gg  <- pelsa_intensity_line_ggplot(sub, pinned_label = pinned_label) +
      ggplot2::ggtitle(pn) +
      ggplot2::labs(y = NULL) +           # one shared y-title added below
      ggplot2::theme(plot.title = ggplot2::element_text(
        face = "bold", size = 11, hjust = 0.5))
    # Only the bottom panel keeps the x tick labels (shared axis).
    # Force the build inside suppressWarnings so the deferred ggplot build
    # (which warns "Ignoring unknown aesthetics: text" for the tooltip aes)
    # is muffled here rather than later in renderPlotly's print path.
    suppressWarnings(plotly::plotly_build(plotly::ggplotly(gg, tooltip = "text")))
  })
  # titleY = FALSE so plotly does NOT render the per-panel y-axis titles (they
  # were stripped via labs(y = NULL) but titleY = TRUE would re-add them and they
  # overlap). We add exactly ONE shared, vertically-centered y-title annotation.
  p <- plotly::subplot(parts, nrows = length(parts), shareX = TRUE,
                       titleY = FALSE, margin = 0.06)
  p <- plotly::layout(
    p,
    showlegend = FALSE,       # tooltip identifies each line; no legend needed
    margin = list(l = 70),    # room so the single y-title clears the tick labels
    annotations = list(list(
      text = "mean log2 intensity", x = -0.12, y = 0.5,
      xref = "paper", yref = "paper", textangle = -90,
      showarrow = FALSE, font = list(size = 12))))
  suppressWarnings(plotly::plotly_build(p))
}

# Build the STATIC export intensity-line plot (ggplot + ggrepel). Mirrors the
# notebook layout: centered bold title "GENE (ACC)", centered subtitle
# "Mapped with N peptide(s)", two shared-y facets "Significant peptides (n)" |
# "Non-significant peptides (n)", one line per peptide-occurrence with end-of-
# line "aa<pos>" labels (the static analogue of the in-app hover tooltip).
#
# @param ld   a pelsa_intensity_line_data() frame (condition factor, mean_log2,
#   peptide_seq, pep_occurrence_idx, aa_label, panel).
# @param gene/accession  title tokens.
# @param log_base  intensity transform applied at setup (2 or 10) -> y label.
# @return a ggplot.
# @noRd
pelsa_intensity_export_ggplot <- function(ld, gene, accession, log_base = 2) {
  conds <- levels(ld$condition)
  if (is.null(conds)) {
    conds <- unique(as.character(ld$condition))
    ld$condition <- factor(as.character(ld$condition), levels = conds)
  }
  ld$grp <- interaction(ld$peptide_seq, ld$pep_occurrence_idx, drop = TRUE)

  counts <- tapply(ld$grp, ld$panel, function(x) length(unique(x)))
  panel_lab <- function(p) sprintf("%s peptides (%d)", p, counts[[p]])
  lvl <- intersect(c("Significant", "Non-significant"), names(counts))
  ld$panel_f <- factor(vapply(as.character(ld$panel), panel_lab, character(1)),
                       levels = vapply(lvl, panel_lab, character(1)))

  # one label per line, anchored at the last condition that line reaches.
  lab_rows <- do.call(rbind, lapply(split(ld, ld$grp), function(d) {
    d <- d[order(match(as.character(d$condition), conds)), , drop = FALSE]
    d[nrow(d), , drop = FALSE]
  }))

  n_total <- length(unique(ld$grp))
  sub_txt <- if (n_total == 1L) "Mapped with 1 peptide"
             else sprintf("Mapped with %d peptides", n_total)
  y_lab <- sprintf("Average log%d(intensity)", as.integer(log_base))

  ggplot2::ggplot(ld, ggplot2::aes(x = .data$condition, y = .data$mean_log2,
                                   group = .data$grp, color = .data$grp)) +
    ggplot2::geom_line(linewidth = 0.5, alpha = 0.9, na.rm = TRUE) +
    ggplot2::geom_point(size = 1.4, na.rm = TRUE) +
    ggrepel::geom_text_repel(
      data = lab_rows,
      ggplot2::aes(label = .data$aa_label, color = .data$grp),
      direction = "y", hjust = 0, nudge_x = 0.12, size = 2.6,
      segment.size = 0.25, segment.color = "grey70", min.segment.length = 0,
      box.padding = 0.1, max.overlaps = Inf, na.rm = TRUE,
      xlim = c(length(conds) + 0.02, NA)) +
    ggplot2::facet_wrap(~ .data$panel_f, nrow = 1, scales = "fixed") +
    ggplot2::scale_color_hue(guide = "none") +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0.3, 0.7))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(title = sprintf("%s (%s)", gene, accession),
                  subtitle = sub_txt, x = "Condition", y = y_lab) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "grey25"),
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
      strip.text  = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_rect(fill = "grey92", color = NA),
      panel.spacing = ggplot2::unit(1.4, "lines"),
      panel.grid.minor = ggplot2::element_blank())
}

# Re-derive a volcano df for export (all_peptide / best_peptide), from plain
# inputs (no Shiny). Returns NULL when stats/cache/contrast are missing so the
# export caller no-ops gracefully. Mirrors the on-screen df build.
#
# @param stat_raw  stat_results()[[ome]], or NULL.
# @param matched   the cache $matched frame, or NULL.
# @param feat_df   the species feature table, or NULL (-> "none" coloring).
# @param markers   marker accessions.
# @param contrast  the contrast suffix, or NULL.
# @param panel     "all_peptide" | "best_peptide".
# @param sig_cutoff the adj.P significance threshold (drives Significant /
#                  sig_direction and the empirical y_cutoff dashed line).
# @param is_self_curated TRUE for a self-curated species: forces accession labels
#                  + blanks the gene, so the exported figure matches the on-screen
#                  volcano (the export is a SEPARATE re-derive of the same df).
# @return a 3A volcano df, or NULL.
# @noRd
pelsa_volcano_export_df <- function(stat_raw, matched, feat_df, markers,
                                    contrast, panel,
                                    sig_cutoff = .PELSA_EXPORT_SIG_CUTOFF,
                                    is_self_curated = FALSE) {
  if (!is.data.frame(stat_raw) || nrow(stat_raw) == 0L) return(NULL)
  if (is.null(contrast) ||
      !pelsa_volcano_has_contrast(stat_raw, contrast)) return(NULL)
  matched <- if (is.data.frame(matched)) matched else data.frame()
  fdf <- feat_df %||% data.frame(accession = character(0), start = integer(0),
                                 end = integer(0), feature_class = character(0))
  stat_df <- pelsa_volcano_stat_df(stat_raw, matched)
  tryCatch(
    pelsa_build_volcano_df(
      stat_df = stat_df,
      matched_cache = if (nrow(matched) > 0L) matched else
        pelsa_volcano_empty_matched(),
      feat_df = fdf, markers = markers, contrast = contrast,
      opts = list(panel = panel, sig_cutoff = sig_cutoff),
      is_self_curated = is_self_curated
    ),
    error = function(e) NULL
  )
}

# Combine the 3C per-protein intensity line data for ALL plotted proteins into
# one tidy frame (the plotted_intensities.csv body). Pure: a function of its
# inputs; no Shiny. Returns NULL when any required input is missing/empty or no
# protein qualifies, so the export caller can no-op gracefully.
#
# @param stat_raw  stat_results()[[ome]] (per-peptide, contrast-suffixed).
# @param matched   the cache $matched frame (peptide x accession x occurrence).
# @param markers   marker accessions (Setup).
# @param contrast  the contrast suffix.
# @param pm        the processed/log2 GCT matrix, or NULL.
# @param cmap      sample -> condition map (named char), or NULL.
# @param corder    condition order (factor levels), or NULL/empty.
# @return tidy long data.frame (rbind of pelsa_intensity_line_data over the
#   pelsa_intensity_proteins set), or NULL.
# @noRd
pelsa_plotted_intensities_df <- function(stat_raw, matched, markers, contrast,
                                         pm, cmap, corder) {
  if (!is.data.frame(stat_raw) || nrow(stat_raw) == 0L) return(NULL)
  if (!is.data.frame(matched) || nrow(matched) == 0L) return(NULL)
  if (is.null(contrast) || is.null(pm) || is.null(cmap) ||
      length(corder) == 0L) {
    return(NULL)
  }
  stat_df <- pelsa_volcano_stat_df(stat_raw, matched)
  prot <- pelsa_intensity_proteins(stat_df, matched, markers, contrast,
                                   sig_cutoff = 0.05)
  if (nrow(prot) == 0L) return(NULL)
  rows <- lapply(seq_len(nrow(prot)), function(i) {
    tryCatch(
      pelsa_intensity_line_data(
        accession = prot$accession[i], stat_df = stat_df,
        matched_cache = matched, processed_mat = pm,
        condition_map = cmap, condition_order = corder,
        contrast = contrast, sig_cutoff = 0.05,
        is_marker = prot$is_marker[i]
      ),
      error = function(e) NULL
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) return(NULL)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Significance-direction -> human legend label (fixed display order).
.PELSA_EXPORT_SIG_LABELS <- c(down = "Downregulated",
                              ns   = "Non-significant",
                              up   = "Upregulated")

# Build the per-point legend category + the manual color scale for a color mode.
# significance: the 3 fixed direction buckets; feature: the 9 UniProt classes
# (always all listed, mirroring the Woods feature legend). Returns the factor
# category column for the background rows + a named values vector for the scale.
# @noRd
.pelsa_export_color_spec <- function(bg, color_mode) {
  if (identical(color_mode, "feature")) {
    keys   <- names(PELSA_FEATURE_COLORS)
    labels <- unname(.PELSA_FEATURE_LABELS[keys])
    values <- stats::setNames(unname(PELSA_FEATURE_COLORS[keys]), labels)
    raw    <- as.character(bg$feature_class_primary)
    cat    <- factor(unname(.PELSA_FEATURE_LABELS[raw]), levels = labels)
    list(category = cat, values = values, method = "feature coloring")
  } else {
    labels <- unname(.PELSA_EXPORT_SIG_LABELS[c("down", "ns", "up")])
    values <- stats::setNames(
      c(.PELSA_SIG_COLOR_DOWN, .PELSA_SIG_COLOR_NS, .PELSA_SIG_COLOR_UP), labels)
    raw    <- as.character(bg$sig_direction)
    cat    <- factor(unname(.PELSA_EXPORT_SIG_LABELS[raw]), levels = labels)
    list(category = cat, values = values, method = "significance coloring")
  }
}

# Build the static export ggplot (mirrors pelsa_volcano_build_plot's geom layout
# but returns a plain ggplot for the PDF device - no plotly / WebGL / browser).
# Color/fill are mapped INSIDE aes() so the figure carries a legend: the chosen
# color mode's categories (significance buckets or UniProt feature classes) plus
# a separate magenta "Marker" entry. A title (the contrast) and subtitle
# (<volcano type> | <coloring method>) are added when supplied.
# @param contrast       the contrast suffix, used for the title (NULL -> none).
# @param volcano_label  e.g. "All-peptide volcano" -> the subtitle prefix.
# @param sig_cutoff     the adj.P significance threshold; drives the dashed-line
#                       annotation text so it always matches the cutoff the df
#                       was built with (single source of truth, no drift).
# @noRd
.pelsa_export_ggplot <- function(df, full_df, color_mode = "significance",
                                 label_mode = "none", n_top = 3L,
                                 contrast = NULL, volcano_label = NULL,
                                 sig_cutoff = .PELSA_EXPORT_SIG_CUTOFF) {
  color_mode <- color_mode %||% "significance"
  split <- pelsa_volcano_marker_split(df)
  bg <- split$background
  mk <- split$markers
  spec <- .pelsa_export_color_spec(bg, color_mode)

  gg <- ggplot2::ggplot()
  if (nrow(bg) > 0L) {
    bg$legend_cat <- spec$category
    gg <- gg + ggplot2::geom_point(
      data = bg, ggplot2::aes(x = .data$logFC, y = .data$logP,
                              color = .data$legend_cat),
      alpha = .PELSA_VOLCANO_BG_ALPHA, size = 1)
  }
  y_cut <- attr(full_df, "y_cutoff")
  if (!is.null(y_cut) && is.finite(y_cut)) {
    gg <- gg + ggplot2::geom_hline(yintercept = y_cut, linetype = "dashed",
                                   color = "grey40")
    # cutoff annotation: small + bold, flush to the right panel edge, just below
    # the dashed line. Label derives from sig_cutoff so it stays consistent with
    # the threshold the df was built with.
    gg <- gg + ggplot2::annotate(
      "text", x = Inf, y = y_cut,
      label = paste0("adj.P < ", format(sig_cutoff, scientific = FALSE,
                                        trim = TRUE)),
      hjust = 1.15, vjust = 1.5, size = 2, fontface = "bold",
      color = "grey30")
  }
  if (nrow(mk) > 0L) {
    gg <- gg + ggplot2::geom_point(
      data = mk, ggplot2::aes(x = .data$logFC, y = .data$logP, fill = "Marker"),
      shape = 21, size = 2.4, stroke = 0.5, color = .PELSA_VOLCANO_MARKER_EDGE)
  }
  # Bake peptide labels per the in-app label mode (the on-screen labels are
  # plotly annotations; the static export draws them as repelled boxed labels:
  # white box, black outline + text, black segment; force=20 to spread them).
  if (!identical(label_mode, "none") && "label" %in% colnames(df)) {
    idx <- tryCatch(
      pelsa_volcano_label_rows(df, mode = label_mode, n_top = n_top),
      error = function(e) integer(0))
    if (length(idx) > 0L) {
      lab <- df[idx, , drop = FALSE]
      lab <- lab[!is.na(lab$label) & nzchar(lab$label), , drop = FALSE]
      if (nrow(lab) > 0L) {
        gg <- gg + ggrepel::geom_label_repel(
          data = lab,
          ggplot2::aes(x = .data$logFC, y = .data$logP, label = .data$label),
          size = 2.6, force = 20, max.overlaps = Inf,
          fill = "white", color = "black",
          label.size = 0.3, label.padding = 0.18,
          min.segment.length = 0, segment.size = 0.3, segment.color = "black")
      }
    }
  }

  title_txt <- if (is.null(contrast)) NULL else
    gsub("_over_", " vs ", contrast, fixed = TRUE)
  subtitle_txt <- if (is.null(volcano_label)) spec$method else
    paste0(volcano_label, " | ", spec$method)

  gg +
    ggplot2::scale_color_manual(name = NULL, values = spec$values,
                                drop = FALSE) +
    ggplot2::scale_fill_manual(name = NULL,
                               values = c("Marker" = .PELSA_VOLCANO_MARKER_COLOR)) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        order = 1, override.aes = list(size = 2, alpha = 1)),
      fill  = ggplot2::guide_legend(
        order = 2,
        override.aes = list(shape = 21, size = 2,
                            color = .PELSA_VOLCANO_MARKER_EDGE))) +
    ggplot2::labs(x = "logFC", y = "-log10(P.Value)",
                  title = title_txt, subtitle = subtitle_txt) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title    = ggplot2::element_text(face = "bold", size = 12,
                                            hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 10, color = "grey30",
                                            hjust = 0.5),
      axis.title = ggplot2::element_text(size = 9, face = "bold"),
      axis.text  = ggplot2::element_text(size = 6),
      legend.position = "right",
      legend.title  = ggplot2::element_blank(),
      legend.text   = ggplot2::element_text(size = 6),
      legend.key    = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(8, "pt"),
      legend.spacing.y = ggplot2::unit(2, "pt"),
      legend.margin = ggplot2::margin(2, 4, 2, 4),
      legend.box.spacing = ggplot2::unit(4, "pt"),
      legend.box.background = ggplot2::element_rect(color = "black", fill = NA,
                                                    linewidth = 0.4),
      legend.box.margin = ggplot2::margin(2, 2, 2, 2))
}
