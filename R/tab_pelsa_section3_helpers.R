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
# is about ALL peptides, not just markers). The sibling-fade alpha (pin redraw)
# is separate and stays low.
.PELSA_VOLCANO_MARKER_SIZE  <- 1.6
.PELSA_VOLCANO_BG_SIZE      <- 1.1
.PELSA_VOLCANO_BG_ALPHA     <- 0.8
.PELSA_VOLCANO_FADE_ALPHA   <- 0.12   # background cloud when a pin dims siblings

# Default per-contrast label mode and top-N. Default is "best peptide per marker"
# (one label per marker protein - readable out of the box, vs top_n's clutter).
.PELSA_VOLCANO_DEFAULT_LABEL_MODE <- "best_per_marker"
.PELSA_VOLCANO_DEFAULT_TOP_N      <- 3L

# The gold used to highlight a selected/pinned peptide (legend entry, Woods
# cross-highlight). Distinct from the magenta marker fill.
.PELSA_VOLCANO_GOLD <- "#D4AF37"

# ---- contrast key + label/suffix mapping ------------------------------------

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
# accession) and the REST. On pin, the main volcano is NOT rebuilt; instead the
# FADE is applied client-side via a plotlyProxy restyle (single mechanism) that
# sets a per-point marker-opacity vector on the background trace - full opacity
# for the pinned protein's peptides, dimmed for the rest. This mask drives that
# opacity vector (see pelsa_volcano_pin_opacity). It is also reused by the
# static PDF export path's build.
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

# Default + dimmed background marker opacities. The default matches the build's
# bg_alpha so the unpinned restyle restores the exact base look.
.PELSA_VOLCANO_BG_ALPHA      <- 0.6
.PELSA_VOLCANO_BG_ALPHA_DIM  <- 0.12

# Build the per-point marker-opacity vector for the BACKGROUND trace of the main
# volcano, for a plotlyProxy "restyle" fade (the perf fix). The main volcano is
# built ONCE (with sibling_acc = NULL) so its background trace contains EVERY
# non-marker point in pelsa_volcano_marker_split(df)$background row order; this
# restyle sets that trace's marker.opacity WITHOUT rebuilding the ~100k-point
# figure (a small message, not a ~15MB redraw).
#
# When `accession` is NULL/NA (unpin / contrast switch) -> every background point
# returns to the default opacity (the base look). When set -> the pinned
# protein's peptides get full opacity (1) and the rest are dimmed.
#
# The vector is aligned to the background-trace point order: it is computed over
# `pelsa_volcano_marker_split(df)$background`, the SAME split the build applies,
# so element j of this vector targets background point j of the rendered trace.
#
# @param df        the FULL volcano frame the base plot was built from.
# @param accession the pinned protein's representative accession, or NULL/NA.
# @return list(opacity = <numeric vector, length = #background points>,
#   n_siblings = <integer>); opacity is the base default everywhere when no pin.
# @noRd
pelsa_volcano_pin_opacity <- function(df, accession) {
  if (!is.data.frame(df)) {
    stop("pelsa_volcano_pin_opacity: df must be a data.frame")
  }
  bg <- pelsa_volcano_marker_split(df)$background
  nb <- nrow(bg)
  if (nb == 0L) return(list(opacity = numeric(0), n_siblings = 0L))

  no_pin <- is.null(accession) || length(accession) != 1L || is.na(accession) ||
    !nzchar(accession)
  if (no_pin) {
    return(list(opacity = rep(.PELSA_VOLCANO_BG_ALPHA, nb), n_siblings = 0L))
  }

  sib <- pelsa_volcano_sibling_mask(bg, accession)$siblings
  opacity <- rep(.PELSA_VOLCANO_BG_ALPHA_DIM, nb)
  opacity[sib] <- 1
  list(opacity = opacity, n_siblings = sum(sib))
}

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

# ---- shared plot-assembly (BOTH volcano panels reuse this) ------------------

# Assemble the WebGL volcano plotly object from the FULL volcano frame (every
# point - no thinning; toWebGL renders the whole cloud on the GPU). The
# all-peptide AND best-peptide panels call this with the same arguments and a
# distinct `source` id, so the plot code is written ONCE.
#
# Trace order is z-order only (later traces draw ON TOP):
#   1. background (non-marker, non-sibling)  - the dense cloud
#   2. siblings   (the pinned protein's peptides) - full opacity, drawn on top
#   3. markers    (magenta overlay, on top, ALWAYS)
#   (+ a geom_text label layer + an optional threshold hline)
# IMPORTANT (perf): the MAIN volcano is built with sibling_acc = NULL, so the
# fade is NOT done by rebuilding here - it is a client-side plotlyProxy "restyle"
# of the background trace's marker.opacity (see pelsa_volcano_pin_opacity and the
# plotly_click observer in tab_pelsa_section3.R). The sibling_acc != NULL path
# (and bg_alpha dim) is retained ONLY for callers that DO want a static rebuild
# (e.g. tests / a one-shot non-interactive render); the interactive volcano does
# not use it. There is exactly ONE interactive fade mechanism: the proxy restyle.
#
# @param df          the FULL volcano frame the plot consumes (every point).
# @param full_df     the same frame, used for the y_cutoff attr + label-row
#   selection over all rows. Defaults to df.
# @param color_mode  "significance" | "feature".
# @param label_mode  a pelsa_volcano_label_rows() mode.
# @param n_top       N for top_n label mode.
# @param source_id   the plotly source id (ns("pelsa_volcano") /
#   ns("pelsa_volcano_best")).
# @param sibling_acc the pinned protein's representative accession, or NULL (no
#   sibling highlight). Used to carve the sibling trace out of the background.
# @param register_click  TRUE -> event_register the plotly_click on this source.
# @return a plotly object (toWebGL'd).
# @noRd
# Drop the `hoveron` attribute from every trace of a (pre-build) plotly object.
# ggplotly sets hoveron = "points" on geom_point traces; scattergl (toWebGL)
# rejects it and warns on EVERY plotly_build, including Shiny's serialize-time
# rebuild that a build-site suppressWarnings cannot wrap. Stripping it once
# silences the benign warning at its source. @noRd
.pelsa_strip_hoveron <- function(p) {
  if (is.list(p$x) && !is.null(p$x$data) && length(p$x$data) > 0L) {
    p$x$data <- lapply(p$x$data, function(tr) {
      tr$hoveron <- NULL
      tr
    })
  }
  p
}

pelsa_volcano_build_plot <- function(df, full_df = df,
                                     color_mode = "significance",
                                     label_mode = "top_n", n_top = 3L,
                                     source_id = "pelsa_volcano",
                                     sibling_acc = NULL,
                                     register_click = FALSE) {
  if (!is.data.frame(df)) {
    stop("pelsa_volcano_build_plot: df must be a data.frame")
  }
  color_mode <- color_mode %||% "significance"

  split <- pelsa_volcano_marker_split(df)
  bg     <- split$background
  mk     <- split$markers

  # Carve the pinned protein's siblings out of the BACKGROUND (markers stay on
  # top regardless). The sibling trace is full-opacity; the rest is the cloud.
  sib_mask <- pelsa_volcano_sibling_mask(bg, sibling_acc)$siblings
  sib <- bg[sib_mask, , drop = FALSE]
  bg  <- bg[!sib_mask, , drop = FALSE]

  tip <- function(d) {
    if (nrow(d) == 0L) return(character(0))
    no_span <- is.na(d$pep_start) | is.na(d$pep_end)
    pos <- ifelse(no_span, "unknown", paste0(d$pep_start, "-", d$pep_end))
    len_chr <- ifelse(no_span, "", as.character(d$pep_end - d$pep_start + 1L))
    len_line <- ifelse(no_span, "", paste0("<br>Length: ", len_chr))
    # Element-wise fallback (NOT %||% on the whole vector): a per-ROW NA/empty
    # winning_accession/gene must fall back to that row's PG.* value, otherwise a
    # single NA row would render the literal "NA" in its tooltip.
    acc_fb <- ifelse(is.na(d$winning_accession) | !nzchar(d$winning_accession),
                     d$PG.ProteinAccessions, d$winning_accession)
    gene_fb <- ifelse(is.na(d$winning_gene) | !nzchar(d$winning_gene),
                      d$PG.Genes, d$winning_gene)
    # Effect-size lines: logFC + adj.P.Val for the active contrast (already on the
    # 3A df). Formatted compactly; NA shows as "NA".
    lfc_chr  <- ifelse(is.na(d$logFC), "NA", sprintf("%.2f", d$logFC))
    adjp_chr <- ifelse(is.na(d$adj.P.Val), "NA", sprintf("%.2g", d$adj.P.Val))
    paste0(
      "Accession: ", acc_fb, "<br>",
      "Gene: ", gene_fb, "<br>",
      "Position: ", pos, len_line, "<br>",
      "logFC: ", lfc_chr, "<br>",
      "adj.P: ", adjp_chr
    )
  }

  gg <- ggplot2::ggplot()
  # Background cloud (dimmed on the pin redraw when sibling_acc is set). The
  # non-faded cloud is fairly opaque (real sig/feature colors readable) - the
  # volcano is about ALL peptides, not only markers.
  bg_alpha <- if (!is.null(sibling_acc) && !is.na(sibling_acc) &&
                  nzchar(sibling_acc)) .PELSA_VOLCANO_FADE_ALPHA
              else .PELSA_VOLCANO_BG_ALPHA
  if (nrow(bg) > 0L) {
    bg$.tip <- tip(bg)
    gg <- gg + ggplot2::geom_point(
      data = bg,
      ggplot2::aes(x = .data$logFC, y = .data$logP, text = .data$.tip),
      color = pelsa_volcano_color_column(bg, color_mode),
      alpha = bg_alpha, size = .PELSA_VOLCANO_BG_SIZE
    )
  }
  # Pinned protein's sibling peptides (full opacity, drawn on top of the cloud).
  if (nrow(sib) > 0L) {
    sib$.tip <- tip(sib)
    gg <- gg + ggplot2::geom_point(
      data = sib,
      ggplot2::aes(x = .data$logFC, y = .data$logP, text = .data$.tip),
      color = pelsa_volcano_color_column(sib, color_mode),
      alpha = 1, size = .PELSA_VOLCANO_MARKER_SIZE + 0.4
    )
  }
  # Marker overlay (magenta, ON TOP, ALWAYS - drawn last). Only SLIGHTLY larger
  # than the cloud so it does not visually dominate the other peptides.
  if (nrow(mk) > 0L) {
    mk$.tip <- tip(mk)
    gg <- gg + ggplot2::geom_point(
      data = mk,
      ggplot2::aes(x = .data$logFC, y = .data$logP, text = .data$.tip),
      fill = .PELSA_VOLCANO_MARKER_COLOR,
      color = .PELSA_VOLCANO_MARKER_EDGE,
      shape = 21, size = .PELSA_VOLCANO_MARKER_SIZE, stroke = 0.4
    )
  }

  y_cut <- attr(full_df, "y_cutoff")
  if (!is.null(y_cut) && is.finite(y_cut)) {
    gg <- gg + ggplot2::geom_hline(yintercept = y_cut, linetype = "dashed",
                                   color = "grey40")
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

  gg <- gg + ggplot2::labs(x = "logFC", y = "-log10(P.Value)") +
    ggplot2::theme_bw()

  # suppressWarnings on BOTH ggplotly + toWebGL: ggplotly leaks the benign
  # "'scattergl' objects don't have these attributes: 'hoveron'" warning (one per
  # geom_point trace) that otherwise spams test output.
  p <- suppressWarnings(plotly::ggplotly(gg, source = source_id, tooltip = "text"))
  # ggplotly sets `hoveron = "points"` on each geom_point trace; scattergl (what
  # toWebGL produces) does not support `hoveron`, so EVERY downstream
  # plotly_build (incl. Shiny's serialize-time rebuild, which suppressWarnings
  # here can't reach) re-emits the warning. Strip it once now so the rebuild is
  # clean -- the attribute is benign and unused for scattergl.
  p <- .pelsa_strip_hoveron(p)
  p <- suppressWarnings(plotly::toWebGL(p))
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
  if (is.null(lab_df) || nrow(lab_df) == 0L) return(p)

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
  if (length(keep) == 0L) return(p)
  kept   <- lab_df[keep, , drop = FALSE]
  border <- border_all[keep]

  anns <- lapply(seq_len(nrow(kept)), function(i) {
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
  plotly::layout(p, annotations = anns)
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
  remap <- function(x) ifelse(!is.na(pinned_disp) & x == pinned_label,
                              pinned_disp, x)
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
  # Marker proteins: facet Significant/Non-significant; non-marker -> single.
  if (length(unique(ld$panel)) > 1L) {
    gg <- gg + ggplot2::facet_wrap(~ .data$panel, ncol = 1, scales = "free_y")
  }
  gg +
    ggplot2::labs(x = NULL, y = "mean log2 intensity", color = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "right",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      strip.text = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_rect(fill = "grey92", color = NA),
      panel.spacing = ggplot2::unit(0.8, "lines")
    )
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
# @return a 3A volcano df, or NULL.
# @noRd
pelsa_volcano_export_df <- function(stat_raw, matched, feat_df, markers,
                                    contrast, panel) {
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
      opts = list(panel = panel, sig_cutoff = 0.05)
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

# Build the static export ggplot (mirrors pelsa_volcano_build_plot's geom layout
# but returns a plain ggplot for the PDF device - no plotly / WebGL / browser).
# @noRd
.pelsa_export_ggplot <- function(df, full_df, color_mode = "significance") {
  split <- pelsa_volcano_marker_split(df)
  bg <- split$background
  mk <- split$markers
  gg <- ggplot2::ggplot()
  if (nrow(bg) > 0L) {
    gg <- gg + ggplot2::geom_point(
      data = bg, ggplot2::aes(x = .data$logFC, y = .data$logP),
      color = pelsa_volcano_color_column(bg, color_mode), alpha = 0.6, size = 1)
  }
  if (nrow(mk) > 0L) {
    gg <- gg + ggplot2::geom_point(
      data = mk, ggplot2::aes(x = .data$logFC, y = .data$logP),
      fill = .PELSA_VOLCANO_MARKER_COLOR, color = .PELSA_VOLCANO_MARKER_EDGE,
      shape = 21, size = 2.4, stroke = 0.5)
  }
  y_cut <- attr(full_df, "y_cutoff")
  if (!is.null(y_cut) && is.finite(y_cut)) {
    gg <- gg + ggplot2::geom_hline(yintercept = y_cut, linetype = "dashed",
                                   color = "grey40")
  }
  gg + ggplot2::labs(x = "logFC", y = "-log10(P.Value)") + ggplot2::theme_bw()
}
