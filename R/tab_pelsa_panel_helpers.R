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
#   pelsa_feature_lanes(features)                 -> features + integer `lane`
#   pelsa_woods_overlap_annotations(starts, ends, features) -> chr per peptide
#   pelsa_coverage_track_ggplot / pelsa_feature_track_ggplot /
#   pelsa_woods_track_ggplot / pelsa_woods_panel  -> the plots
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
# NA/inverted spans are dropped. Returns the input rows (kept ones) with an
# added integer `lane` (1-based).
#
# @param features data.frame with at least `start`, `end` (+ feature_class etc.).
# @return features (valid rows) + `lane`; a 0-row copy when nothing is valid.
# @noRd
pelsa_feature_lanes <- function(features) {
  if (!is.data.frame(features) || nrow(features) == 0L ||
      !all(c("start", "end") %in% colnames(features))) {
    out <- if (is.data.frame(features)) features[0L, , drop = FALSE] else
      data.frame(start = integer(0), end = integer(0))
    out$lane <- integer(0)
    return(out)
  }
  s <- suppressWarnings(as.integer(features$start))
  e <- suppressWarnings(as.integer(features$end))
  ok <- !is.na(s) & !is.na(e) & e >= s
  f <- features[ok, , drop = FALSE]
  if (nrow(f) == 0L) { f$lane <- integer(0); return(f) }
  ir <- IRanges::IRanges(start = s[ok], end = e[ok])
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
  f$.tip <- sprintf("%s\n%d-%d\nOverlapping peptides: %s",
                    name_line, f$start, f$end, ov)
  # No per-plot fill legend: the sidebar carries a complete static UniProt feature
  # color key (every class, present or not), so a second dynamic legend here is
  # redundant and crowds the track.
  ggplot2::ggplot(f) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = .data$start, xmax = .data$end,
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

# ---- Helper 6: STATIC export Woods plot (single-panel ggplot) ----------------

# Greedy lane packing fallback (base R; mirrors IRanges::disjointBins) so the
# export builder works on plain feature frames without the S4 import. @noRd
.pelsa_pack_lanes <- function(starts, ends) {
  n <- length(starts)
  if (n == 0L) return(integer(0))
  ord <- order(starts, ends)
  lane_end <- numeric(0)
  lanes <- integer(n)
  for (i in ord) {
    placed <- FALSE
    for (l in seq_along(lane_end)) {
      if (starts[i] > lane_end[l]) {
        lanes[i] <- l; lane_end[l] <- ends[i]; placed <- TRUE; break
      }
    }
    if (!placed) { lane_end <- c(lane_end, ends[i]); lanes[i] <- length(lane_end) }
  }
  lanes
}

# Build the STATIC export Woods plot: a single-panel ggplot (NOT the interactive
# 3-track subplot). Peptide segments sit at y = logFC over [pep_start, pep_end],
# colored by -log10(adj.P) (grey80 -> #B2182B, capped); significant peptides
# (adj.P < sig_cutoff) get a small "*" at the segment center. ALL UniProt
# features for the protein render as a lane-packed, box-outlined band below the
# data, and the feature legend always shows all PELSA_FEATURE_COLORS classes.
#
# @param peptides  pelsa_woods_peptide_data() output (peptide_seq, pep_start,
#                  pep_end, logFC, adj.P.Val, sig).
# @param features  raw UniProt feature rows for the accession (start, end,
#                  feature_class). Any subset of classes; all are plotted.
# @param prot_len  protein length (x extent).
# @param gene/accession  title tokens.
# @param contrast  contrast label for the subtitle.
# @param sig_cutoff adj.P significance threshold (default .PELSA_EXPORT_SIG_CUTOFF).
# @return a ggplot.
# @noRd
pelsa_woods_export_ggplot <- function(peptides, features, prot_len, gene,
                                      accession, contrast,
                                      sig_cutoff = .PELSA_EXPORT_SIG_CUTOFF,
                                      sig_stat = "adj.p.val") {
  prot_len <- max(1L, as.integer(prot_len))
  # Human-readable contrast for the subtitle, matching the volcano export
  # (pelsa_volcano_export_df): "DMSO_over_VEH" -> "DMSO vs VEH".
  contrast_disp <- gsub("_over_", " vs ", contrast, fixed = TRUE)
  pk <- if (is.data.frame(peptides)) peptides else data.frame()
  if (nrow(pk) > 0L) {
    pk$.lfc  <- as.numeric(pk$logFC)
    pk$.adjp <- as.numeric(pk$adj.P.Val)
    pk <- pk[is.finite(pk$.lfc), , drop = FALSE]
  }
  title <- if (nzchar(gene)) sprintf("%s (%s), %d aa", gene, accession, prot_len)
           else sprintf("%s, %d aa", accession, prot_len)
  base_theme <- ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", hjust = 0),
      plot.subtitle = ggplot2::element_text(face = "italic", hjust = 0,
                                            color = "grey25"),
      plot.caption  = ggplot2::element_text(hjust = 0, size = 8, color = "grey30"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.key.height = ggplot2::unit(0.8, "lines"),
      legend.title  = ggplot2::element_text(size = 9),
      legend.text   = ggplot2::element_text(size = 7.5),
      legend.background = ggplot2::element_rect(color = "black", fill = NA,
                                               linewidth = 0.3),
      legend.margin = ggplot2::margin(4, 6, 4, 6),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  if (nrow(pk) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = prot_len / 2, y = 0,
                          label = "no mapped peptides", color = "grey50") +
        ggplot2::scale_x_continuous(
          limits = c(1, prot_len),
          # always keep the protein end as a break so short proteins (< 20 aa,
          # where seq(0, len, 20) is just {0} and 0 sits below the axis) are not
          # left with a bare, label-less x-axis.
          breaks = unique(c(seq(0L, prot_len, by = 20L), prot_len))) +
        ggplot2::labs(title = title,
                      subtitle = sprintf("Wood's plot: %s", contrast_disp),
                      x = "Residue position", y = "log2FC") +
        base_theme)
  }

  # Color gradient encodes the SHARED significance statistic: -log10 of the raw
  # P.Value for "nom.p.val" (when present), else -log10(adj.P.Val). Falls back to
  # adj.P.Val when the raw-p column is absent so older frames still color.
  use_nom <- identical(sig_stat, "nom.p.val") && "P.Value" %in% colnames(pk)
  pcol <- if (use_nom) as.numeric(pk$P.Value) else pk$.adjp
  p_label <- if (use_nom) "nom.P" else "adj.P"  # legend + caption stat label
  pk$neglogp <- pmin(-log10(pmax(pcol, .Machine$double.xmin)),
                     .PELSA_WOODS_NEGLOG_CAP)
  pk$neglogp[is.na(pcol)] <- 0
  # The "*" significance markers follow the SHARED stat choice. Prefer the
  # authoritative `sig` column computed upstream by pelsa_woods_peptide_data()
  # (sig_stat-aware); fall back to recomputing on adj.P.Val if it is absent.
  pk$is_sig <- if ("sig" %in% colnames(pk)) {
    isTRUE_vec <- pk$sig %in% TRUE
    isTRUE_vec
  } else {
    !is.na(pk$.adjp) & pk$.adjp < sig_cutoff
  }

  # ALL features, lane-packed.
  feats <- if (is.data.frame(features)) features else data.frame()
  has_feats <- nrow(feats) > 0L &&
    all(c("start", "end", "feature_class") %in% colnames(feats))
  if (has_feats) {
    feats <- feats[, c("start", "end", "feature_class")]
    feats$start <- as.integer(feats$start)
    feats$end   <- as.integer(feats$end)
    feats <- feats[is.finite(feats$start) & is.finite(feats$end) &
                     feats$end >= feats$start, , drop = FALSE]
  }
  has_feats <- is.data.frame(feats) && nrow(feats) > 0L
  if (has_feats) {
    feats$feature_class <- factor(as.character(feats$feature_class),
                                  levels = names(PELSA_FEATURE_COLORS))
    feats$lane <- .pelsa_pack_lanes(feats$start, feats$end)
    n_lane <- max(1L, suppressWarnings(max(feats$lane, na.rm = TRUE)))
  } else {
    n_lane <- 1L
  }

  y_lo <- min(pk$.lfc, 0, na.rm = TRUE)
  y_hi <- max(pk$.lfc, 0, na.rm = TRUE)
  yr   <- max(y_hi - y_lo, 1e-6)
  band_gap <- yr * 0.10
  lane_h   <- yr * 0.06
  pad      <- lane_h * 0.30
  feat_top <- y_lo - band_gap
  content_top <- feat_top - pad
  if (has_feats) {
    feats$ymax <- content_top - (feats$lane - 1L) * lane_h
    feats$ymin <- feats$ymax - lane_h * 0.82
    feat_bot <- min(feats$ymin) - pad
  } else {
    feat_bot <- feat_top - lane_h - 2 * pad
  }

  intervals <- pelsa_coverage_intervals(pk$pep_start, pk$pep_end)
  cov_aa  <- if (nrow(intervals) > 0L)
    sum(intervals$end - intervals$start + 1L) else 0L
  cov_pct <- 100 * cov_aa / prot_len
  cov_txt <- sprintf("Coverage: %.1f%% (%d aa, %d pep)", cov_pct, cov_aa, nrow(pk))

  dummy <- data.frame(
    feature_class = factor(names(PELSA_FEATURE_COLORS),
                           levels = names(PELSA_FEATURE_COLORS)),
    stringsAsFactors = FALSE)

  gg <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "grey70") +
    ggplot2::geom_rect(
      data = dummy,
      ggplot2::aes(xmin = 1, xmax = 1, ymin = feat_bot, ymax = feat_bot,
                   fill = .data$feature_class), na.rm = TRUE) +
    ggplot2::annotate("rect", xmin = 1, xmax = prot_len, ymin = feat_bot,
                      ymax = feat_top, fill = NA, color = "grey40",
                      linewidth = 0.4)
  if (has_feats) {
    gg <- gg + ggplot2::geom_rect(
      data = feats,
      ggplot2::aes(xmin = .data$start, xmax = .data$end, ymin = .data$ymin,
                   ymax = .data$ymax, fill = .data$feature_class))
  }
  gg <- gg +
    ggplot2::geom_segment(
      data = pk,
      ggplot2::aes(x = .data$pep_start, xend = .data$pep_end, y = .data$.lfc,
                   yend = .data$.lfc, color = .data$neglogp),
      linewidth = 2.4, lineend = "butt") +
    ggplot2::geom_point(
      data = pk[pk$is_sig, , drop = FALSE],
      ggplot2::aes(x = (.data$pep_start + .data$pep_end) / 2, y = .data$.lfc),
      shape = 8, size = 1.1, stroke = 0.4, color = "black") +
    ggplot2::annotate("text", x = 1, y = Inf, label = cov_txt, hjust = 0,
                      vjust = 1.6, color = "grey45", fontface = "bold", size = 3.4) +
    ggplot2::scale_fill_manual(
      values = PELSA_FEATURE_COLORS, limits = names(PELSA_FEATURE_COLORS),
      labels = .PELSA_FEATURE_LABELS, drop = FALSE, name = "UniProt feature",
      guide = ggplot2::guide_legend(order = 1)) +
    ggplot2::scale_color_gradient(
      low = "grey80", high = "#B2182B", limits = c(0, .PELSA_WOODS_NEGLOG_CAP),
      name = sprintf("-log10(%s)", p_label),
      guide = ggplot2::guide_colourbar(order = 2)) +
    ggplot2::scale_x_continuous(
      limits = c(1, prot_len), expand = ggplot2::expansion(mult = 0.01),
      # always keep the protein end as a break (see empty-data path above).
      breaks = unique(c(seq(0L, prot_len, by = 20L), prot_len))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.04, 0.16))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = title, subtitle = sprintf("Wood's plot: %s", contrast_disp),
      caption = sprintf("*Significant peptides (%s < %s)",
                        p_label, format(sig_cutoff)),
      x = "Residue position", y = "log2FC") +
    base_theme
  gg
}
################################################################################
# Module: PELSA per-protein intensity-line DATA builder (Task 3C) - pure, no
# Shiny, no plotting.
#
# Produces the DATA the Phase-7 pinned left-click panel renders as a per-protein
# intensity LINE plot (the notebook's `plotted_intensities.csv`,
# marker_protein_intensity_line_plot cells 34/35). NO plotly here. Two helpers:
#
#   pelsa_intensity_proteins(stat_df, matched_cache, markers, contrast, ...)
#     -> WHICH accessions get an intensity-line figure: the UNION of
#          (a) the marker accessions (isoform-base matched via 2J), AND
#          (b) accessions with >=1 SIGNIFICANT peptide (adj.P.Val.<contrast> <
#              sig_cutoff in the chosen contrast).
#        Each returned accession is tagged `is_marker` (a marker-AND-significant
#        accession is still a marker). VECTORIZED group-by over the exploded
#        peptide<->accession join (no per-protein loop).
#
#   pelsa_intensity_line_data(accession, stat_df, matched_cache, processed_mat,
#                             condition_map, condition_order, contrast,
#                             sig_cutoff, is_marker)
#     -> tidy long line data for ONE protein. One line per peptide-OCCURRENCE
#        (a matched_cache row for this accession). For a MARKER protein, BOTH
#        significant AND non-significant occurrences are included, tagged by
#        `panel` in {"Significant","Non-significant"} (Phase-7's two-panel facet:
#        left = significantly-regulated peptides, right = its other peptides -
#        dropping an empty side is Phase-7's concern; here we only TAG). For a
#        NON-marker significant protein, ONLY its significant occurrences (panel
#        all "Significant").
#
# y = MEAN PROCESSED-GCT log2 intensity, AS-IS: no delinearize, no z-score, no
# re-normalize. For each (occurrence, condition) we average the processed_mat
# values across that condition's replicate sample columns (na.rm). Contrast this
# with CV (tab_pelsa_analysis_helpers.R), which uses RAW (non-normalized) intensities -
# a deliberate difference.
#
# x = condition as a FACTOR with levels = the user-confirmed condition_order
# (Phase-5 Setup). Conditions in condition_order that have NO sample columns are
# dropped from the data (no rows) but RETAINED as factor levels so the x-axis
# order is stable.
#
# end-of-line LABEL = "aa<pos>" where pos = the FASTA-derived `pep_start` from
# matched_cache (2B) - NOT PEP.PeptidePosition. One label per line (per
# occurrence); a peptide occurring twice yields two distinct lines with distinct
# pep_start / aa_label.
#
# ROW ALIGNMENT (peptide <-> processed_mat row), documented:
#   matched_cache carries `.row_id`, the 1-based index into the ORIGINAL peptide
#   frame synthesized by 2A's pelsa_explode_accessions(). The processed/log2 GCT
#   matrix rows align to that SAME peptide order, so a peptide's intensities are
#   processed_mat[.row_id, sample_cols]. We use `.row_id` as the primary,
#   collision-proof key WHEN it is present on matched_cache AND
#   nrow(processed_mat) covers the referenced ids. Otherwise we fall back to
#   rownames(processed_mat) keyed on PEP.StrippedSequence (when processed_mat is
#   rownamed by peptide id). Exactly one of these keys is chosen and documented
#   at call time.
#
# Reuses verified Phase-2 helpers (reimplements none): 2J pelsa_match_markers /
# pelsa_isoform_base for marker tagging. The significance grouping operates over
# the matched_cache's already-exploded (peptide x accession) rows (2A output).
#
# Hardening: pelsa_intensity_proteins is fully vectorized (a single grouped
# any() over accession; no O(n_peptides) R loop). pelsa_intensity_line_data
# operates on ONE protein's (small) peptide set, so a modest loop over the
# handful of CONDITIONS is acceptable; there is no per-peptide R loop. Boundary
# validation fails fast. Keep free of Shiny reactivity (unit-testable).
################################################################################

# Resolve the contrast's significance p-column name and verify it exists. Which
# column depends on the SHARED sig_stat choice: "nom.p.val" -> P.Value.<contrast>
# (raw p), otherwise adj.P.Val.<contrast>. Fails fast with a loud, column-naming
# error. (Kept named *_adjp_col for call-site stability; it now resolves either p
# column.)
# @noRd
.pelsa_intensity_adjp_col <- function(stat_df, contrast,
                                      sig_stat = "adj.p.val") {
  if (length(contrast) != 1L || is.na(contrast) || !nzchar(contrast)) {
    stop("PELSA intensity: contrast must be a single non-empty string",
         call. = FALSE)
  }
  prefix <- if (identical(sig_stat, "nom.p.val")) "P.Value." else "adj.P.Val."
  col <- paste0(prefix, contrast)
  if (!col %in% colnames(stat_df)) {
    stop("PELSA intensity: stat_df missing required stat column: ", col,
         call. = FALSE)
  }
  col
}

# Decide WHICH accessions get an intensity-line figure.
#
# Returns a data.frame with columns `accession` (character) and `is_marker`
# (logical), one row per accession in the union of:
#   - markers (isoform-base symmetric match via 2J), AND
#   - accessions with >=1 SIGNIFICANT peptide for the chosen contrast
#     (adj.P.Val.<contrast> < sig_cutoff for ANY peptide mapped to that
#      accession via the exploded matched_cache).
# A marker that is also significant is still a marker (is_marker TRUE).
#
# Significance per accession is a grouped any() over the matched_cache rows
# (each row is a (peptide, accession, occurrence)); the peptide's adj.P.Val is
# joined from stat_df by the peptide key (`.row_id` when present on both, else
# PEP.StrippedSequence). FULLY VECTORIZED: no per-protein R loop.
#
# @param stat_df        per-peptide frame carrying adj.P.Val.<contrast> and a
#   peptide key (`.row_id` preferred, else PEP.StrippedSequence).
# @param matched_cache  the 2B $matched frame (peptide x accession x occurrence)
#   carrying `accession` plus the same peptide key.
# @param markers        character vector of marker accessions (isoforms ok).
# @param contrast       contrast key; selects adj.P.Val.<contrast>.
# @param sig_cutoff     significance threshold on adj.P.Val. Defaults to the
#   shared .PELSA_EXPORT_SIG_CUTOFF; live module callers thread the user-set
#   isolate(sig_cutoff_r()) (Statistics > Summary), matching the volcano.
# @return data.frame(accession, is_marker), zero-row when the union is empty.
# @noRd
pelsa_intensity_proteins <- function(stat_df, matched_cache, markers,
                                     contrast,
                                     sig_cutoff = .PELSA_EXPORT_SIG_CUTOFF,
                                     sig_stat = "adj.p.val") {
  # ---- Boundary validation (fail fast) ------------------------------------
  if (!is.data.frame(stat_df)) {
    stop("pelsa_intensity_proteins: stat_df must be a data.frame", call. = FALSE)
  }
  if (!is.data.frame(matched_cache)) {
    stop("pelsa_intensity_proteins: matched_cache must be a data.frame",
         call. = FALSE)
  }
  if (!is.character(markers)) {
    stop("pelsa_intensity_proteins: markers must be a character vector",
         call. = FALSE)
  }
  if (!"accession" %in% colnames(matched_cache)) {
    stop("pelsa_intensity_proteins: matched_cache must have an 'accession' column",
         call. = FALSE)
  }
  adjp_col <- .pelsa_intensity_adjp_col(stat_df, contrast, sig_stat)

  # ---- Join the peptide's significance p-value onto each matched row -------
  use_row_id <- ".row_id" %in% colnames(stat_df) &&
    ".row_id" %in% colnames(matched_cache)
  if (use_row_id) {
    key_m <- matched_cache[[".row_id"]]
    key_s <- stat_df[[".row_id"]]
  } else {
    if (!"PEP.StrippedSequence" %in% colnames(stat_df) ||
        !"PEP.StrippedSequence" %in% colnames(matched_cache)) {
      stop("pelsa_intensity_proteins: need `.row_id` on both frames or ",
           "PEP.StrippedSequence on both for the peptide join", call. = FALSE)
    }
    key_m <- as.character(matched_cache[["PEP.StrippedSequence"]])
    key_s <- as.character(stat_df[["PEP.StrippedSequence"]])
  }

  acc <- as.character(matched_cache[["accession"]])
  adjp <- as.numeric(stat_df[[adjp_col]])[match(key_m, key_s)]
  is_sig_row <- !is.na(adjp) & adjp < sig_cutoff

  # Grouped any() over accession -> accessions with >=1 significant peptide.
  # tapply over a factor of accessions is a single vectorized group op.
  sig_accs <- character(0)
  if (length(acc) > 0L) {
    fac <- factor(acc)
    agg <- tapply(is_sig_row, fac, FUN = any)
    sig_accs <- levels(fac)[!is.na(agg) & agg]
  }

  # ---- Marker accessions present in the data (2J isoform-symmetric) --------
  # Restrict marker accessions to those actually appearing in matched_cache so
  # we never emit a marker with no peptide to plot. Match by isoform-base key.
  uniq_acc <- unique(acc)
  marker_accs <- character(0)
  if (length(markers) > 0L && length(uniq_acc) > 0L) {
    is_marker_acc <- pelsa_match_markers(uniq_acc, markers)
    marker_accs <- uniq_acc[is_marker_acc]
  }

  # ---- Union + is_marker tag ----------------------------------------------
  union_accs <- union(marker_accs, sig_accs)
  if (length(union_accs) == 0L) {
    return(data.frame(accession = character(0), is_marker = logical(0),
                      stringsAsFactors = FALSE))
  }
  out <- data.frame(
    accession = union_accs,
    is_marker = union_accs %in% marker_accs,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

# Resolve a condition map to a per-column character vector aligned to
# colnames(processed_mat). Accepts a named vector (names must COVER all columns)
# or a positionally-aligned vector. Fails fast on length / coverage mismatch.
# @noRd
.pelsa_intensity_condition_map <- function(condition_map, processed_mat) {
  cn <- colnames(processed_mat)
  if (is.null(cn)) {
    stop("pelsa_intensity_line_data: processed_mat must have column names",
         call. = FALSE)
  }
  nm <- names(condition_map)
  if (!is.null(nm)) {
    if (!all(cn %in% nm)) {
      missing <- setdiff(cn, nm)
      stop("pelsa_intensity_line_data: condition_map must cover all ",
           "processed_mat columns; missing: ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
    return(as.character(condition_map[cn])) # reorder to column order
  }
  if (length(condition_map) != length(cn)) {
    stop("pelsa_intensity_line_data: positional condition_map length (",
         length(condition_map), ") must match processed_mat columns (",
         length(cn), ")", call. = FALSE)
  }
  as.character(condition_map)
}

# Resolve each matched occurrence's processed_mat row index. Primary key is
# `.row_id` (1-based into the original peptide frame == processed_mat rows);
# fallback is rownames(processed_mat) keyed on PEP.StrippedSequence. Returns an
# integer row index per matched row (NA where the peptide row is absent).
# @noRd
.pelsa_intensity_row_index <- function(matched_sub, processed_mat) {
  n_proc <- nrow(processed_mat)
  has_row_id <- ".row_id" %in% colnames(matched_sub)
  if (has_row_id) {
    rid <- as.integer(matched_sub[[".row_id"]])
    # `.row_id` is a valid primary key only if every referenced id is in range.
    if (all(!is.na(rid) & rid >= 1L & rid <= n_proc)) {
      return(rid)
    }
  }
  # Fallback: rownames(processed_mat) keyed on PEP.StrippedSequence.
  rn <- rownames(processed_mat)
  if (!is.null(rn) && "PEP.StrippedSequence" %in% colnames(matched_sub)) {
    return(match(as.character(matched_sub[["PEP.StrippedSequence"]]), rn))
  }
  stop("pelsa_intensity_line_data: cannot align matched_cache rows to ",
       "processed_mat - provide `.row_id` (1-based row index) on matched_cache ",
       "or peptide-id rownames on processed_mat", call. = FALSE)
}

# Build the tidy line-data for ONE protein.
#
# @param accession        single accession to build line data for.
# @param stat_df          per-peptide frame with adj.P.Val.<contrast> + peptide
#   key (`.row_id` preferred, else PEP.StrippedSequence).
# @param matched_cache    the 2B $matched frame (peptide x accession x
#   occurrence) carrying accession / pep_start / pep_occurrence_idx /
#   PEP.StrippedSequence and (preferably) `.row_id`.
# @param processed_mat    the PROCESSED/log2 GCT matrix (peptides x samples);
#   rows align to matched_cache `.row_id` (or are rownamed by peptide id).
# @param condition_map    named char (names = colnames(processed_mat)) sample ->
#   condition, OR a positionally-aligned char vector.
# @param condition_order  ordered character vector of conditions (factor levels
#   for the x-axis); conditions with no samples are dropped from rows but kept
#   as levels.
# @param contrast         contrast key; selects adj.P.Val.<contrast>.
# @param sig_cutoff       significance threshold on adj.P.Val. Defaults to the
#   shared .PELSA_EXPORT_SIG_CUTOFF; live module callers thread the user-set
#   isolate(sig_cutoff_r()) (Statistics > Summary), matching the volcano.
# @param is_marker        TRUE -> include BOTH significant + non-significant
#   occurrences (panel-tagged); FALSE -> only significant occurrences.
# @param .index    optional pelsa_intensity_build_index(matched_cache) result,
#   reused across a per-protein export loop instead of re-scanning
#   matched_cache on every call. MUST be built from the exact same
#   matched_cache passed to this call, or the two paths silently diverge.
#   NULL (default) falls back to the original linear-scan behavior.
# @return tidy long data.frame, one row per (occurrence, condition-with-samples),
#   columns: accession, peptide_seq, pep_start, pep_end, pep_occurrence_idx, aa_label,
#   panel ("Significant"/"Non-significant"), condition (factor = condition_order),
#   mean_log2, n_rep_nonNA.
# @noRd
# Per-accession index of matched_cache for the intensity export loop: look up one
# protein's occurrences in O(1) instead of re-scanning matched_cache on every
# protein iteration. NA / blank accessions are dropped here (mirroring the other
# accession consumers), so the indexed path is equivalent to the linear scan
# without relying on an upstream non-NA invariant. @noRd
pelsa_intensity_build_index <- function(matched_cache) {
  by_acc <- list()
  if (is.data.frame(matched_cache) && nrow(matched_cache) > 0L &&
      "accession" %in% colnames(matched_cache)) {
    acc <- as.character(matched_cache[["accession"]])
    valid <- !is.na(acc) & nzchar(acc)
    if (any(valid)) {
      by_acc <- split(matched_cache[valid, , drop = FALSE], acc[valid])
    }
  }
  list(by_acc = by_acc)
}

pelsa_intensity_line_data <- function(accession, stat_df, matched_cache,
                                      processed_mat, condition_map,
                                      condition_order, contrast,
                                      sig_cutoff = .PELSA_EXPORT_SIG_CUTOFF,
                                      is_marker = FALSE,
                                      show_all = FALSE,
                                      sig_stat = "adj.p.val",
                                      .index = NULL) {
  # ---- Boundary validation (fail fast) ------------------------------------
  if (length(accession) != 1L || is.na(accession) || !nzchar(accession)) {
    stop("pelsa_intensity_line_data: accession must be a single non-empty string",
         call. = FALSE)
  }
  if (!is.data.frame(stat_df)) {
    stop("pelsa_intensity_line_data: stat_df must be a data.frame", call. = FALSE)
  }
  if (!is.data.frame(matched_cache)) {
    stop("pelsa_intensity_line_data: matched_cache must be a data.frame",
         call. = FALSE)
  }
  if (is.data.frame(processed_mat)) processed_mat <- as.matrix(processed_mat)
  if (!is.matrix(processed_mat) || !is.numeric(processed_mat)) {
    stop("pelsa_intensity_line_data: processed_mat must be a numeric matrix",
         call. = FALSE)
  }
  matched_required <- c("accession", "pep_start", "pep_occurrence_idx")
  if (!all(matched_required %in% colnames(matched_cache))) {
    stop("pelsa_intensity_line_data: matched_cache must have ",
         paste(matched_required, collapse = "/"), call. = FALSE)
  }
  if (!"PEP.StrippedSequence" %in% colnames(matched_cache)) {
    stop("pelsa_intensity_line_data: matched_cache must have PEP.StrippedSequence",
         call. = FALSE)
  }
  if (length(condition_order) == 0L) {
    stop("pelsa_intensity_line_data: condition_order must be non-empty",
         call. = FALSE)
  }
  adjp_col <- .pelsa_intensity_adjp_col(stat_df, contrast, sig_stat)
  cond <- .pelsa_intensity_condition_map(condition_map, processed_mat)

  # ---- Subset matched_cache to this accession (the occurrences == lines) ---
  if (!is.null(.index)) {
    m <- .index$by_acc[[accession]]
    if (is.null(m) || nrow(m) == 0L) {
      stop("pelsa_intensity_line_data: accession '", accession,
           "' not found in matched_cache", call. = FALSE)
    }
  } else {
    sel <- as.character(matched_cache[["accession"]]) == accession
    sel[is.na(sel)] <- FALSE
    if (!any(sel)) {
      stop("pelsa_intensity_line_data: accession '", accession,
           "' not found in matched_cache", call. = FALSE)
    }
    m <- matched_cache[sel, , drop = FALSE]
  }

  # ---- Per-occurrence significance (from stat_df by peptide key) -----------
  use_row_id <- ".row_id" %in% colnames(stat_df) &&
    ".row_id" %in% colnames(matched_cache)
  if (use_row_id) {
    key_m <- m[[".row_id"]]
    key_s <- stat_df[[".row_id"]]
  } else {
    if (!"PEP.StrippedSequence" %in% colnames(stat_df)) {
      stop("pelsa_intensity_line_data: need `.row_id` on both frames or ",
           "PEP.StrippedSequence on stat_df for the peptide join", call. = FALSE)
    }
    # matched_cache$PEP.StrippedSequence already validated present above (the
    # matched-column boundary check ~line 284), so m[[...]] is a guarded access.
    key_m <- as.character(m[["PEP.StrippedSequence"]])
    key_s <- as.character(stat_df[["PEP.StrippedSequence"]])
  }
  adjp <- as.numeric(stat_df[[adjp_col]])[match(key_m, key_s)]
  occ_sig <- !is.na(adjp) & adjp < sig_cutoff

  # ---- Choose occurrences to plot -----------------------------------------
  # show_all -> EVERY peptide mapping to this protein (the pinned-panel view:
  # the user wants the full peptide set for the clicked protein, not just the
  # significant ones). Otherwise the legacy rule: marker -> all (panel-tagged),
  # non-marker -> significant only.
  keep <- if (isTRUE(show_all) || isTRUE(is_marker)) {
    rep(TRUE, nrow(m))
  } else {
    occ_sig
  }
  if (!any(keep)) {
    return(.pelsa_intensity_empty(condition_order))
  }
  m <- m[keep, , drop = FALSE]
  # Display-friendly panel labels (capitalized; "other" -> "Non-significant").
  panel <- ifelse(occ_sig[keep], "Significant", "Non-significant")

  # ---- Resolve each occurrence's processed_mat row -------------------------
  row_idx <- .pelsa_intensity_row_index(m, processed_mat)

  # ---- Per-condition mean log2 (AS-IS) over replicate columns --------------
  # ONE protein's peptide set is small; loop over the FEW conditions only (never
  # over peptide rows). condition_order conditions with no sample columns are
  # dropped from the rows but kept as factor levels.
  conditions_present <- intersect(condition_order, unique(cond))
  # No condition in condition_order matches the data's conditions: return the
  # full-contract empty frame (do.call(rbind, list()) below would be NULL, and
  # out$condition <- factor(...) on NULL would coerce `out` into a malformed
  # bare list that drops the contracted columns).
  if (length(conditions_present) == 0L) {
    return(.pelsa_intensity_empty(condition_order))
  }
  n_occ <- nrow(m)
  parts <- vector("list", length(conditions_present))
  for (i in seq_along(conditions_present)) {
    cnd <- conditions_present[i]
    cols <- which(cond == cnd)
    block <- processed_mat[row_idx, cols, drop = FALSE] # n_occ x reps

    n_nonNA <- rowSums(!is.na(block))                   # vectorized over occs
    means <- matrixStats::rowMeans2(block, na.rm = TRUE)
    means[n_nonNA == 0L] <- NA_real_                    # all-NA -> NA mean

    pep_end_vec <- if ("pep_end" %in% colnames(m)) {
      as.integer(m[["pep_end"]])
    } else {
      rep(NA_integer_, n_occ)
    }
    parts[[i]] <- data.frame(
      accession          = rep(accession, n_occ),
      peptide_seq        = as.character(m[["PEP.StrippedSequence"]]),
      pep_start          = as.integer(m[["pep_start"]]),
      pep_end            = pep_end_vec,
      pep_occurrence_idx = as.integer(m[["pep_occurrence_idx"]]),
      aa_label           = paste0("aa", as.integer(m[["pep_start"]])),
      panel              = panel,
      condition          = rep(cnd, n_occ),
      mean_log2          = as.numeric(means),
      n_rep_nonNA        = as.integer(n_nonNA),
      stringsAsFactors   = FALSE
    )
  }

  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  # x-axis factor: levels = the full requested condition_order (stable order).
  out$condition <- factor(out$condition, levels = condition_order)
  out
}

# Zero-row line-data frame with the full column contract and the condition
# factor levels preserved (used when a protein has no qualifying occurrence).
# @noRd
.pelsa_intensity_empty <- function(condition_order) {
  data.frame(
    accession          = character(0),
    peptide_seq        = character(0),
    pep_start          = integer(0),
    pep_end            = integer(0),
    pep_occurrence_idx = integer(0),
    aa_label           = character(0),
    panel              = character(0),
    condition          = factor(character(0), levels = condition_order),
    mean_log2          = numeric(0),
    n_rep_nonNA        = integer(0),
    stringsAsFactors   = FALSE
  )
}

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
