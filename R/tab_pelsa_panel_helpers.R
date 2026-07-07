################################################################################
# Module: PELSA Volcano - per-protein coverage + UniProt-feature + Woods panel
# (pure data + plot helpers).
#
# When the user pins a peptide on the volcano, the fixed panel shows a 3-track
# view for that peptide's protein, sharing one residue-position x-axis (1..len):
#
#   1. COVERAGE ruler  - full-length backbone; residues covered by >=1 peptide
#                        highlighted gold.
#   2. FEATURE track   - UniProt features as colored segments (PELSA_FEATURE_COLORS),
#                        lane-packed so overlapping features stack.
#   3. WOODS plot      - each peptide a horizontal segment from pep_start..pep_end
#                        at y = logFC (current contrast); significance is encoded
#                        by the -log10(adj.P) color gradient of that segment.
#
# Interval math uses IRanges (reduce = covered union; disjointBins = feature lane
# packing). The peptide<->feature tooltip join uses data.table::foverlaps (already
# a PELSA dependency), avoiding the S4Vectors hit-accessor import surface.
#
# All helpers are PURE (functions of their data.frame args; no Shiny, no network)
# so they unit-test in isolation; the module server (tab_pelsa_section3.R) wires
# them to the pinned reactive + the analysis cache.
#
# Public helpers (all @noRd):
#   pelsa_woods_peptide_data(accession, matched, stat_df, contrast, sig_cutoff)
#       -> per-peptide df: peptide_seq, pep_start, pep_end, logFC, adj.P.Val, sig
#   pelsa_coverage_intervals(starts, ends)        -> merged covered [start,end] df
#   pelsa_widen_point_features(features, prot_len) -> features + display_start/
#       display_end/was_widened (single-AA features widened +-N for display)
#   pelsa_feature_lanes(features, prot_len=Inf)   -> features + lane/display_start/
#       display_end/was_widened (lane-packed on the widened span)
#   pelsa_woods_overlap_annotations(starts, ends, features) -> chr per peptide
#   pelsa_coverage_track_ggplot / pelsa_feature_track_ggplot /
#   pelsa_woods_track_ggplot / pelsa_woods_panel  -> the plots
#
# Sibling files (split out to stay under the 800-line file cap):
#   tab_pelsa_woods_export_helpers.R    - static export Woods ggplot + click index
#   tab_pelsa_intensity_line_helpers.R  - per-protein intensity-line data builder
################################################################################

# The gold used for the coverage-track highlight.
# (Shared intent with .PELSA_VOLCANO_GOLD in tab_pelsa_constants.R.)
.PELSA_WOODS_GOLD <- .PELSA_GOLD

.PELSA_WOODS_NEGLOG_CAP <- 5  # clamp -log10(adj.P) so tiny p-values don't flatten

# Single-residue ("point") UniProt features (start == end, e.g. a modified
# residue) are widened by this many aa on each side for DISPLAY ONLY, so they
# render as a visible span instead of a near-invisible zero-width rect/segment.
.PELSA_WOODS_POINT_PAD <- 3L

# Human-readable labels for the PELSA_FEATURE_COLORS class keys (the feature-track
# legend). Keyed by the palette names so a complete reference can be rendered. @noRd
.PELSA_FEATURE_LABELS <- c(
  active_or_binding_site     = "active / binding site",
  catalytic_domain           = "catalytic domain",
  folded_domain              = "folded domain",
  region_or_motif            = "region / motif",
  transmembrane_or_signal    = "transmembrane / signal",
  repeat_or_coiled_coil      = "repeat / coiled coil",
  low_complexity_or_disorder = "low complexity / disorder",
  other                      = "other",
  none                       = "none / unannotated"
)

# Build the static UniProt-feature color legend UI: one swatch + label per class
# in PELSA_FEATURE_COLORS, shown ALWAYS (even classes absent from the current
# protein) so the user has a complete reference to the Woods feature-track colors.
# A pure tagList builder (no Shiny reactivity). @noRd
.pelsa_feature_legend_ui <- function() {
  classes <- names(PELSA_FEATURE_COLORS)
  items <- lapply(classes, function(cl) {
    lab <- .PELSA_FEATURE_LABELS[[cl]]
    if (is.null(lab)) lab <- cl
    shiny::tags$li(
      shiny::tags$span(style = sprintf("color:%s;", PELSA_FEATURE_COLORS[[cl]]),
                       "\u25cf"),
      paste0(" ", lab))
  })
  shiny::tags$ul(class = "pelsa-feature-key",
                 style = "list-style:none; padding-left:0; margin:0;", items)
}

# ---- Helper 0: per-accession export index (perf) -----------------------------

# Build a reusable per-accession index of `matched` so the export loop can look
# up one protein's rows in O(1) instead of re-scanning the whole frame on every
# (protein x contrast) iteration. `stat_key` is precomputed once (a pure function
# of stat_df, so identical for every call within one export). NA / blank
# accessions are dropped here (mirroring the `!is.na & nzchar` guard the other
# feat/annotation consumers use), so this index never depends on an upstream
# invariant to stay equivalent to the linear-scan path. @noRd
pelsa_woods_build_index <- function(matched, stat_df) {
  by_acc <- list()
  if (is.data.frame(matched) && nrow(matched) > 0L &&
      "accession" %in% colnames(matched)) {
    acc <- as.character(matched[["accession"]])
    valid <- !is.na(acc) & nzchar(acc)
    if (any(valid)) {
      by_acc <- split(matched[valid, , drop = FALSE], acc[valid])
    }
  }
  stat_key <- if (is.data.frame(stat_df) &&
                  "PEP.StrippedSequence" %in% colnames(stat_df)) {
    as.character(stat_df[["PEP.StrippedSequence"]])
  } else {
    character(0)
  }
  list(by_acc = by_acc, stat_key = stat_key)
}

# ---- Helper 1: per-peptide Woods data ----------------------------------------

# Build the per-peptide Woods frame for ONE protein (accession).
#
# Joins the matched-cache peptide spans for `accession` to the Statistics frame's
# contrast columns (logFC.<contrast>, adj.P.Val.<contrast>) by PEP.StrippedSequence,
# yielding one row per matched peptide-occurrence with its span + effect size.
# `sig` flags adj.P.Val < sig_cutoff (NA adj.P -> not significant). Peptides with
# an NA span are dropped (cannot place on the residue axis).
#
# @param accession  the pinned protein accession.
# @param matched    the cache $matched frame (PEP.StrippedSequence, accession,
#                   pep_start, pep_end, pep_occurrence_idx).
# @param stat_df    a pelsa_volcano_stat_df() frame (PEP.StrippedSequence + the
#                   contrast-suffixed logFC/adj.P.Val columns).
# @param contrast   the contrast suffix (e.g. "A_over_B").
# @param sig_cutoff adj.P.Val significance threshold. Defaults to the shared
#   .PELSA_EXPORT_SIG_CUTOFF; live module callers thread the user-set
#   isolate(sig_cutoff_r()) (Statistics > Summary), matching the volcano.
# @param .index    optional pelsa_woods_build_index(matched, stat_df) result,
#   reused across a (protein x contrast) export loop instead of re-scanning
#   `matched`/`stat_df` on every call. MUST be built from the exact same
#   `matched`/`stat_df` passed to this call, or the two paths silently diverge.
#   NULL (default) falls back to the original linear-scan behavior.
# @return data.frame(peptide_seq, pep_start, pep_end, logFC, adj.P.Val, sig),
#         sorted by pep_start; 0-row frame (same columns) when nothing matches.
# @noRd
pelsa_woods_peptide_data <- function(accession, matched, stat_df, contrast,
                                     sig_cutoff = .PELSA_EXPORT_SIG_CUTOFF,
                                     sig_stat = "adj.p.val",
                                     .index = NULL) {
  empty <- data.frame(
    peptide_seq = character(0), pep_start = integer(0), pep_end = integer(0),
    logFC = numeric(0), adj.P.Val = numeric(0), P.Value = numeric(0),
    sig = logical(0),
    stringsAsFactors = FALSE
  )
  if (!is.data.frame(matched) || nrow(matched) == 0L) return(empty)
  if (!is.data.frame(stat_df) || nrow(stat_df) == 0L) return(empty)
  need_m <- c("PEP.StrippedSequence", "accession", "pep_start", "pep_end")
  if (!all(need_m %in% colnames(matched))) return(empty)
  if (!"PEP.StrippedSequence" %in% colnames(stat_df)) return(empty)

  lfc_col  <- paste0("logFC.", contrast)
  adjp_col <- paste0("adj.P.Val.", contrast)
  pval_col <- paste0("P.Value.", contrast)
  # The significance flag uses the SHARED stat choice: raw P.Value for
  # "nom.p.val", else adj.P.Val. The reported adj.P.Val column is unchanged.
  sig_col  <- if (identical(sig_stat, "nom.p.val")) pval_col else adjp_col
  if (!all(c(lfc_col, adjp_col, sig_col) %in% colnames(stat_df))) return(empty)

  m <- if (!is.null(.index)) {
    .index$by_acc[[accession]] %||% matched[0L, , drop = FALSE]
  } else {
    matched[as.character(matched$accession) == accession, , drop = FALSE]
  }
  m <- m[!is.na(m$pep_start) & !is.na(m$pep_end), , drop = FALSE]
  if (nrow(m) == 0L) return(empty)

  key_s <- if (!is.null(.index)) .index$stat_key else
    as.character(stat_df[["PEP.StrippedSequence"]])
  idx   <- match(as.character(m[["PEP.StrippedSequence"]]), key_s)
  logfc <- as.numeric(stat_df[[lfc_col]])[idx]
  adjp  <- as.numeric(stat_df[[adjp_col]])[idx]
  sigp  <- as.numeric(stat_df[[sig_col]])[idx]
  # Carry the raw P.Value (when present) so downstream Woods coloring can follow
  # the shared sig_stat; NA when the stat frame lacks the nominal-p column.
  pval  <- if (pval_col %in% colnames(stat_df))
    as.numeric(stat_df[[pval_col]])[idx] else rep(NA_real_, length(idx))

  out <- data.frame(
    peptide_seq = as.character(m[["PEP.StrippedSequence"]]),
    pep_start   = as.integer(m$pep_start),
    pep_end     = as.integer(m$pep_end),
    logFC       = logfc,
    adj.P.Val   = adjp,
    P.Value     = pval,
    sig         = !is.na(sigp) & sigp < sig_cutoff,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$pep_start, out$pep_end), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# ---- Helper 2: covered-residue union (IRanges) -------------------------------

# Merge peptide [start,end] intervals into the covered-residue union.
#
# Uses IRanges::reduce (merges overlapping AND directly-adjacent ranges). Returns
# a data.frame of disjoint [start,end] covered blocks (sorted). Empty input -> a
# 0-row frame. NA / inverted (start>end) intervals are dropped.
#
# @param starts integer peptide start positions.
# @param ends   integer peptide end positions (same length as starts).
# @return data.frame(start, end) of merged covered intervals.
# @noRd
pelsa_coverage_intervals <- function(starts, ends) {
  empty <- data.frame(start = integer(0), end = integer(0))
  starts <- suppressWarnings(as.integer(starts))
  ends   <- suppressWarnings(as.integer(ends))
  ok <- !is.na(starts) & !is.na(ends) & ends >= starts
  starts <- starts[ok]; ends <- ends[ok]
  if (length(starts) == 0L) return(empty)
  ir <- IRanges::reduce(IRanges::IRanges(start = starts, end = ends))
  data.frame(start = as.integer(IRanges::start(ir)),
             end   = as.integer(IRanges::end(ir)))
}

# ---- Helper 3: feature lane packing (IRanges) --------------------------------

# Assign each UniProt feature to a non-overlapping LANE (row) for stacked
# rendering, via IRanges::disjointBins (greedy min-lane packing). Features with
# NA/inverted spans are dropped. Single-AA ("point") features are widened via
# pelsa_widen_point_features() before lane-packing, so lanes are computed over
# the WIDENED display coordinates (display_start/display_end), not the true
# start/end -- two point features that only overlap after widening still land
# in separate lanes. Returns the input rows (kept ones) with added integer
# `lane` (1-based), `display_start`, `display_end`, `was_widened`.
#
# @param features data.frame with at least `start`, `end` (+ feature_class etc.).
# @param prot_len protein length used to clamp widened point features at the
#   C-terminus; Inf (default) disables clamping for callers with no known
#   protein length, matching pre-widening behavior.
# @return features (valid rows) + `lane`/`display_start`/`display_end`/
#   `was_widened`; a 0-row copy when nothing is valid.
# @noRd
pelsa_feature_lanes <- function(features, prot_len = Inf) {
  if (!is.data.frame(features) || nrow(features) == 0L ||
      !all(c("start", "end") %in% colnames(features))) {
    out <- if (is.data.frame(features)) features[0L, , drop = FALSE] else
      data.frame(start = integer(0), end = integer(0))
    out$lane <- integer(0)
    out$display_start <- integer(0)
    out$display_end   <- integer(0)
    out$was_widened    <- logical(0)
    return(out)
  }
  s <- suppressWarnings(as.integer(features$start))
  e <- suppressWarnings(as.integer(features$end))
  ok <- !is.na(s) & !is.na(e) & e >= s
  f <- features[ok, , drop = FALSE]
  if (nrow(f) == 0L) {
    f$lane <- integer(0)
    f$display_start <- integer(0)
    f$display_end   <- integer(0)
    f$was_widened    <- logical(0)
    return(f)
  }
  # prot_len = Inf (default) -> pmin(..., Inf) is a no-op ceiling, matching
  # the pre-widening behavior for any caller that hasn't passed a real length.
  clamp_len <- if (is.finite(prot_len)) as.integer(prot_len) else .Machine$integer.max
  f <- pelsa_widen_point_features(f, prot_len = clamp_len)
  ir <- IRanges::IRanges(start = f$display_start, end = f$display_end)
  f$lane <- as.integer(IRanges::disjointBins(ir))
  rownames(f) <- NULL
  f
}

# ---- Helper 3b: single-AA ("point") feature widening for DISPLAY -------------

# Add `display_start`/`display_end` (+ `was_widened`) to a features frame: for
# rows where start == end (a single-residue annotation, e.g. a modified
# residue at 214-214), widen by .PELSA_WOODS_POINT_PAD aa on each side,
# clamped to [1, prot_len], so the feature renders as a visible span instead
# of a zero-width rect. Multi-residue rows (start != end) pass through with
# display_start/display_end == start/end. NA-coord rows pass through with NA
# display_* and was_widened FALSE (nothing to widen). The TRUE start/end
# columns are never modified -- overlap joins (pelsa_woods_overlap_annotations,
# pelsa_feature_overlap_peptides) and any other consumer of `features` must
# keep reading `start`/`end`, not the display_* columns.
#
# @param features data.frame with at least `start`, `end`.
# @param prot_len  protein length (clamp ceiling); coerced to a single integer.
# @return `features` with `display_start`, `display_end` (integer) and
#   `was_widened` (logical) appended.
# @noRd
pelsa_widen_point_features <- function(features, prot_len) {
  if (!is.data.frame(features)) {
    stop("pelsa_widen_point_features: features must be a data.frame",
         call. = FALSE)
  }
  if (length(prot_len) != 1L ||
      !is.finite(suppressWarnings(as.numeric(prot_len)))) {
    stop("pelsa_widen_point_features: prot_len must be a single finite number",
         call. = FALSE)
  }
  plen <- max(1L, as.integer(prot_len))
  n <- nrow(features)
  if (n == 0L) {
    features$display_start <- integer(0)
    features$display_end   <- integer(0)
    features$was_widened   <- logical(0)
    return(features)
  }
  s <- suppressWarnings(as.integer(features$start))
  e <- suppressWarnings(as.integer(features$end))
  is_point <- !is.na(s) & !is.na(e) & s == e

  disp_s <- s
  disp_e <- e
  if (any(is_point)) {
    lo <- pmax(1L, s[is_point] - .PELSA_WOODS_POINT_PAD)
    hi <- pmin(plen, e[is_point] + .PELSA_WOODS_POINT_PAD)
    disp_s[is_point] <- lo
    disp_e[is_point] <- hi
  }

  features$display_start <- disp_s
  features$display_end   <- disp_e
  features$was_widened   <- is_point
  features
}

# ---- Helper 4: peptide <-> feature overlap annotations (data.table) ----------

# For each peptide span, the DISTINCT overlapping UniProt feature names (for the
# Woods tooltip). Uses data.table::foverlaps (closed intervals). Collapsed by
# feature name (no per-occurrence start-end) and de-duplicated, so the tooltip
# reads "catalytic_domain;region_or_motif" rather than listing the same feature
# class once per overlapping region. Returns "" when a peptide overlaps nothing.
#
# @param starts   integer peptide starts.
# @param ends     integer peptide ends.
# @param features data.frame(start, end, feature_class[, feature_type]).
# @return character vector length == length(starts).
# @noRd
pelsa_woods_overlap_annotations <- function(starts, ends, features) {
  n <- length(starts)
  out <- rep("", n)
  if (n == 0L) return(out)
  if (!is.data.frame(features) || nrow(features) == 0L ||
      !all(c("start", "end", "feature_class") %in% colnames(features))) {
    return(out)
  }
  pep <- data.table::data.table(
    .pid = seq_len(n),
    start = suppressWarnings(as.integer(starts)),
    end   = suppressWarnings(as.integer(ends))
  )
  pep <- pep[!is.na(pep$start) & !is.na(pep$end)]
  if (nrow(pep) == 0L) return(out)
  fe <- data.table::data.table(
    start = suppressWarnings(as.integer(features$start)),
    end   = suppressWarnings(as.integer(features$end)),
    feature_class = as.character(features$feature_class)
  )
  fe <- fe[!is.na(fe$start) & !is.na(fe$end) & fe$end >= fe$start]
  if (nrow(fe) == 0L) return(out)

  data.table::setkey(fe, start, end)
  ov <- data.table::foverlaps(pep, fe, type = "any", nomatch = NULL)
  if (nrow(ov) == 0L) return(out)
  # Collapse by DISTINCT feature name per peptide (keep first-seen order).
  agg <- tapply(ov$feature_class, ov$.pid,
                function(x) paste(unique(x), collapse = ";"))
  out[as.integer(names(agg))] <- as.character(agg)
  out
}

# For each feature span, the DISTINCT overlapping peptide aa-labels ("aa12;aa45"),
# sorted by position; "none" when a feature overlaps no peptide. data.table
# foverlaps. @noRd
pelsa_feature_overlap_peptides <- function(feat_starts, feat_ends,
                                           pep_starts, pep_ends) {
  nf <- length(feat_starts)
  out <- rep("none", nf)
  if (nf == 0L) return(out)
  if (length(pep_starts) == 0L) return(out)
  fe <- data.table::data.table(
    .fid = seq_len(nf),
    start = suppressWarnings(as.integer(feat_starts)),
    end   = suppressWarnings(as.integer(feat_ends)))
  fe <- fe[!is.na(fe$start) & !is.na(fe$end)]
  if (nrow(fe) == 0L) return(out)
  pep <- data.table::data.table(
    start = suppressWarnings(as.integer(pep_starts)),
    end   = suppressWarnings(as.integer(pep_ends)))
  pep <- pep[!is.na(pep$start) & !is.na(pep$end) & pep$end >= pep$start]
  if (nrow(pep) == 0L) return(out)
  data.table::setkey(pep, start, end)
  ov <- data.table::foverlaps(fe, pep, type = "any", nomatch = NULL)
  if (nrow(ov) == 0L) return(out)
  agg <- tapply(ov$start, ov$.fid, function(s) {
    paste(paste0("aa", sort(unique(as.integer(s)))), collapse = ";")
  })
  out[as.integer(names(agg))] <- as.character(agg)
  out
}

# ---- Helper 5: plot builders -------------------------------------------------

# Coverage ruler track: grey backbone + gold covered intervals, residue ticks.
# @param intervals  pelsa_coverage_intervals() output (start, end).
# @param prot_len   protein length (x-axis extent).
# @return a ggplot.
# @noRd
pelsa_coverage_track_ggplot <- function(intervals, prot_len) {
  prot_len <- max(1L, as.integer(prot_len))
  # Guard the upper-tick sequence: seq(10L, prot_len, by=...) errors with "wrong
  # sign in 'by' argument" when prot_len < 10 (from > to with positive by). Short
  # proteins simply get the single tick at residue 1.
  brks <- unique(c(1L, if (prot_len >= 10L) {
    seq(10L, prot_len, by = max(10L, round(prot_len / 10)))
  } else integer(0)))
  gg <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = 1, xmax = prot_len, ymin = 0, ymax = 1),
      fill = "grey88")
  if (is.data.frame(intervals) && nrow(intervals) > 0L) {
    intervals$.tip <- sprintf("covered %d-%d", intervals$start, intervals$end)
    gg <- gg + ggplot2::geom_rect(
      data = intervals,
      ggplot2::aes(xmin = .data$start, xmax = .data$end, ymin = 0, ymax = 1,
                   text = .data$.tip),
      fill = .PELSA_WOODS_GOLD)
  }
  gg +
    ggplot2::scale_x_continuous(limits = c(1, prot_len), expand = c(0, 0),
                                breaks = brks) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = "Coverage") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(color = "grey60",
                                                        fill = NA, linewidth = 0.4))
}

# UniProt feature track: colored lane-packed segments.
# @param features_lanes pelsa_feature_lanes() output (start, end, feature_class, lane).
# @param prot_len       protein length.
# @param palette        feature_class -> color (default PELSA_FEATURE_COLORS).
# @return a ggplot.
# @noRd
pelsa_feature_track_ggplot <- function(features_lanes, prot_len,
                                       palette = PELSA_FEATURE_COLORS) {
  prot_len <- max(1L, as.integer(prot_len))
  if (!is.data.frame(features_lanes) || nrow(features_lanes) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = prot_len / 2, y = 0.5,
                          label = "no UniProt features", size = 3,
                          color = "grey50") +
        ggplot2::scale_x_continuous(limits = c(1, prot_len), expand = c(0, 0)) +
        ggplot2::labs(x = NULL, y = "Feature") +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_text(angle = 0,
                                                            vjust = 0.5),
                       panel.grid = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(
                         color = "grey60", fill = NA, linewidth = 0.4))
    )
  }
  f <- features_lanes
  # DISPLAY span: prefer the widened display_start/display_end (produced by
  # pelsa_feature_lanes()); fall back to the true start/end for any caller
  # that supplies a features_lanes-shaped frame without going through that
  # widener (e.g. a hand-built fixture).
  disp_s <- if (!is.null(f$display_start)) f$display_start else f$start
  disp_e <- if (!is.null(f$display_end)) f$display_end else f$end
  was_widened <- if (!is.null(f$was_widened)) f$was_widened else
    rep(FALSE, nrow(f))
  # Tooltip NAME = the real UniProt feature_type (e.g. "Active site", "Domain",
  # "Transmembrane") - NOT the 9-bucket feature_class (which read as a generic
  # "region_or_motif" etc). For generic types ("Region", "Site"), append the
  # free-text `description` (e.g. "Disordered") which carries the specifics.
  ftype <- if ("feature_type" %in% colnames(f)) as.character(f$feature_type) else
    as.character(f$feature_class)
  desc <- if ("description" %in% colnames(f)) as.character(f$description) else
    rep(NA_character_, nrow(f))
  name_line <- ifelse(!is.na(desc) & nzchar(desc) &
                        tolower(desc) != tolower(ftype),
                      paste0(ftype, ": ", desc), ftype)
  ov <- if (".overlap_peps" %in% colnames(f)) f$.overlap_peps else "none"
  # The TRUE start-end is always the primary coordinate in the tooltip; a
  # single-AA feature (start == end) additionally notes the widened DISPLAY
  # span it was drawn at, so the user isn't misled into thinking 211-217 is
  # the real annotation.
  widen_note <- ifelse(was_widened,
                       sprintf(" (shown widened %d-%d for visibility)",
                               disp_s, disp_e), "")
  f$.tip <- sprintf("%s\n%d-%d%s\nOverlapping peptides: %s",
                    name_line, f$start, f$end, widen_note, ov)
  f$.disp_start <- disp_s
  f$.disp_end   <- disp_e
  # No per-plot fill legend: the sidebar carries a complete static UniProt feature
  # color key (every class, present or not), so a second dynamic legend here is
  # redundant and crowds the track.
  ggplot2::ggplot(f) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = .data$.disp_start, xmax = .data$.disp_end,
                   ymin = .data$lane - 0.4, ymax = .data$lane + 0.4,
                   fill = .data$feature_class, text = .data$.tip)) +
    ggplot2::scale_fill_manual(values = palette, drop = TRUE, name = NULL,
                               guide = "none") +
    ggplot2::scale_x_continuous(limits = c(1, prot_len), expand = c(0, 0)) +
    ggplot2::scale_y_reverse(expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::labs(x = NULL, y = "Feature") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(color = "grey60",
                                                        fill = NA, linewidth = 0.4),
                   legend.position = "none")
}

# Woods plot: each peptide a horizontal segment (start..end) at y = logFC;
# significance is encoded by the segment's -log10(adj.P) color gradient.
# @param peptides pelsa_woods_peptide_data() output, optionally with a `.tip`.
# @param prot_len protein length.
# @return a ggplot.
# @noRd
pelsa_woods_track_ggplot <- function(peptides, prot_len,
                                     sig_stat = "adj.p.val") {
  prot_len <- max(1L, as.integer(prot_len))
  if (!is.data.frame(peptides) || nrow(peptides) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = prot_len / 2, y = 0,
                          label = "no mapped peptides", size = 3,
                          color = "grey50") +
        ggplot2::scale_x_continuous(limits = c(1, prot_len), expand = c(0, 0)) +
        ggplot2::labs(x = "Residue position", y = "logFC") +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(angle = 0,
                                                            vjust = 0.5),
                       panel.border = ggplot2::element_rect(
                         color = "grey60", fill = NA, linewidth = 0.4))
    )
  }
  pk <- peptides
  # Color gradient + tooltip encode the SHARED significance statistic: raw
  # P.Value for "nom.p.val" (when present), else adj.P.Val. Matches the export
  # Woods figure (pelsa_woods_export_ggplot) so on-screen and exported agree.
  use_nom <- identical(sig_stat, "nom.p.val") && "P.Value" %in% colnames(pk)
  pcol <- if (use_nom) as.numeric(pk$P.Value) else pk$adj.P.Val
  p_label <- if (use_nom) "nom.P" else "adj.P"
  if (is.null(pk$.tip)) {
    pk$.tip <- sprintf("%s\naa %d-%d (len %d)\nlogFC: %.2f\n%s: %.2g",
                       pk$peptide_seq, pk$pep_start, pk$pep_end,
                       pk$pep_end - pk$pep_start + 1L, pk$logFC, p_label, pcol)
  }
  pk$y <- pk$logFC
  pk$neglogp <- pmin(-log10(pmax(pcol, .Machine$double.xmin)),
                     .PELSA_WOODS_NEGLOG_CAP)
  pk$neglogp[is.na(pcol)] <- 0
  gg <- ggplot2::ggplot(pk, ggplot2::aes(text = .data$.tip)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "grey70")
  gg +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$pep_start, xend = .data$pep_end,
                   y = .data$y, yend = .data$y, color = .data$neglogp),
      linewidth = 1.8, lineend = "round", alpha = 0.95) +
    ggplot2::scale_color_gradient(low = "grey92", high = "#B2182B",
      limits = c(0, .PELSA_WOODS_NEGLOG_CAP),
      name = sprintf("-log10(%s)", p_label)) +
    ggplot2::scale_x_continuous(limits = c(1, prot_len), expand = c(0, 0)) +
    ggplot2::labs(x = "Residue position", y = "logFC") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5),
                   panel.border = ggplot2::element_rect(color = "grey60",
                                                        fill = NA, linewidth = 0.4))
}

# Assemble the 3 tracks into one shared-x plotly (coverage / features / Woods,
# top -> bottom). Woods dominant. Registers plotly_click on `source_id` so the
# module can cross-highlight the volcano. NOT toWebGL (segment/rect hover
# fidelity; a single protein's peptides/features are few).
#
# @param peptides       pelsa_woods_peptide_data() output (with .tip).
# @param features_lanes pelsa_feature_lanes() output.
# @param intervals      pelsa_coverage_intervals() output.
# @param prot_len       protein length.
# @param source_id      plotly source id for click events.
# @return a plotly subplot.
# @noRd
pelsa_woods_panel <- function(peptides, features_lanes, intervals, prot_len,
                              source_id = "pelsa_woods",
                              sig_stat = "adj.p.val") {
  g_cov   <- pelsa_coverage_track_ggplot(intervals, prot_len)
  g_feat  <- pelsa_feature_track_ggplot(features_lanes, prot_len)
  g_woods <- pelsa_woods_track_ggplot(peptides, prot_len, sig_stat = sig_stat)

  # Build each track WITHOUT a per-panel source (subplot errors if more than one
  # child carries a source); set the source on the assembled subplot instead.
  pc <- suppressWarnings(plotly::ggplotly(g_cov, tooltip = "text"))
  pf <- suppressWarnings(plotly::ggplotly(g_feat, tooltip = "text"))
  pw <- suppressWarnings(plotly::ggplotly(g_woods, tooltip = "text"))
  p <- plotly::subplot(pc, pf, pw, nrows = 3, shareX = TRUE, titleY = TRUE,
                       heights = c(0.08, 0.32, 0.60), margin = 0.02)
  p$x$source <- source_id
  plotly::event_register(p, "plotly_click")
}

