################################################################################
# Module: PELSA Section 3 (Volcano) — pure, testable plot-assembly helpers.
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

# Default per-contrast label mode and top-N.
.PELSA_VOLCANO_DEFAULT_LABEL_MODE <- "top_n"
.PELSA_VOLCANO_DEFAULT_TOP_N      <- 3L

# ---- contrast key + label/suffix mapping ------------------------------------

# Build the registry key "<ome>::<contrast>" for a per-contrast registry slot.
#
# `contrast` is the STAT-COLUMN SUFFIX (e.g. "A_over_B"), not the display label,
# so a registry slot maps 1:1 to the columns 3A reads. NULL/empty contrast
# yields NULL (no key — the caller gates on this).
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
    stop("pelsa_volcano_stat_df: stat_df must have PEP.StrippedSequence")
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
# the non-marker background rows. Markers are never thinned, so the split is run
# on the (already thinned) frame the plot consumes.
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
#   "all_markers"     every marker-protein peptide (is_marker == TRUE).
#   "best_per_marker" one peptide per marker PROTEIN (winning_accession): the
#                     smallest adj.P.Val within each marker protein.
#   "top_n"           the N peptides with the smallest adj.P.Val per PROTEIN
#                     (winning_accession), across ALL proteins (default N=3).
#
# Returns the 1-based row indices to label (sorted, unique). Ties in adj.P.Val
# break by row order (stable). NA adj.P.Val sorts last.
#
# @param volcano_df a 3A frame (label, is_marker, adj.P.Val, winning_accession).
# @param mode       one of the three modes above.
# @param n_top      N for "top_n" (default 3, coerced to >= 1).
# @return integer vector of row indices to label.
# @noRd
pelsa_volcano_label_rows <- function(volcano_df, mode = "top_n", n_top = 3L) {
  if (!is.data.frame(volcano_df)) {
    stop("pelsa_volcano_label_rows: volcano_df must be a data.frame")
  }
  mode <- mode %||% "top_n"
  if (length(mode) != 1L || is.na(mode) ||
      !mode %in% c("all_markers", "best_per_marker", "top_n")) {
    stop("pelsa_volcano_label_rows: mode must be 'all_markers', ",
         "'best_per_marker', or 'top_n'")
  }
  n <- nrow(volcano_df)
  if (n == 0L) return(integer(0))

  is_m <- volcano_df$is_marker %||% rep(FALSE, n)
  is_m[is.na(is_m)] <- FALSE

  if (mode == "all_markers") {
    return(which(is_m))
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
