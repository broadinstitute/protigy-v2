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
# (Shared intent with .PELSA_VOLCANO_GOLD in tab_pelsa_section3_helpers.R.)
.PELSA_WOODS_GOLD <- .PELSA_GOLD

.PELSA_WOODS_NEGLOG_CAP <- 5  # clamp -log10(adj.P) so tiny p-values don't flatten

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
# @return data.frame(peptide_seq, pep_start, pep_end, logFC, adj.P.Val, sig),
#         sorted by pep_start; 0-row frame (same columns) when nothing matches.
# @noRd
pelsa_woods_peptide_data <- function(accession, matched, stat_df, contrast,
                                     sig_cutoff = .PELSA_EXPORT_SIG_CUTOFF) {
  empty <- data.frame(
    peptide_seq = character(0), pep_start = integer(0), pep_end = integer(0),
    logFC = numeric(0), adj.P.Val = numeric(0), sig = logical(0),
    stringsAsFactors = FALSE
  )
  if (!is.data.frame(matched) || nrow(matched) == 0L) return(empty)
  if (!is.data.frame(stat_df) || nrow(stat_df) == 0L) return(empty)
  need_m <- c("PEP.StrippedSequence", "accession", "pep_start", "pep_end")
  if (!all(need_m %in% colnames(matched))) return(empty)
  if (!"PEP.StrippedSequence" %in% colnames(stat_df)) return(empty)

  lfc_col  <- paste0("logFC.", contrast)
  adjp_col <- paste0("adj.P.Val.", contrast)
  if (!all(c(lfc_col, adjp_col) %in% colnames(stat_df))) return(empty)

  m <- matched[as.character(matched$accession) == accession, , drop = FALSE]
  m <- m[!is.na(m$pep_start) & !is.na(m$pep_end), , drop = FALSE]
  if (nrow(m) == 0L) return(empty)

  key_s <- as.character(stat_df[["PEP.StrippedSequence"]])
  idx   <- match(as.character(m[["PEP.StrippedSequence"]]), key_s)
  logfc <- as.numeric(stat_df[[lfc_col]])[idx]
  adjp  <- as.numeric(stat_df[[adjp_col]])[idx]

  out <- data.frame(
    peptide_seq = as.character(m[["PEP.StrippedSequence"]]),
    pep_start   = as.integer(m$pep_start),
    pep_end     = as.integer(m$pep_end),
    logFC       = logfc,
    adj.P.Val   = adjp,
    sig         = !is.na(adjp) & adjp < sig_cutoff,
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
pelsa_woods_track_ggplot <- function(peptides, prot_len) {
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
  if (is.null(pk$.tip)) {
    pk$.tip <- sprintf("%s\naa %d-%d (len %d)\nlogFC: %.2f\nadj.P: %.2g",
                       pk$peptide_seq, pk$pep_start, pk$pep_end,
                       pk$pep_end - pk$pep_start + 1L, pk$logFC, pk$adj.P.Val)
  }
  pk$y <- pk$logFC
  pk$neglogp <- pmin(-log10(pmax(pk$adj.P.Val, .Machine$double.xmin)),
                     .PELSA_WOODS_NEGLOG_CAP)
  pk$neglogp[is.na(pk$adj.P.Val)] <- 0
  gg <- ggplot2::ggplot(pk, ggplot2::aes(text = .data$.tip)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "grey70")
  gg +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$pep_start, xend = .data$pep_end,
                   y = .data$y, yend = .data$y, color = .data$neglogp),
      linewidth = 1.8, lineend = "round", alpha = 0.95) +
    ggplot2::scale_color_gradient(low = "grey92", high = "#B2182B",
      limits = c(0, .PELSA_WOODS_NEGLOG_CAP), name = "-log10(adj.P)") +
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
                              source_id = "pelsa_woods") {
  g_cov   <- pelsa_coverage_track_ggplot(intervals, prot_len)
  g_feat  <- pelsa_feature_track_ggplot(features_lanes, prot_len)
  g_woods <- pelsa_woods_track_ggplot(peptides, prot_len)

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
                                      sig_cutoff = .PELSA_EXPORT_SIG_CUTOFF) {
  prot_len <- max(1L, as.integer(prot_len))
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
      legend.margin = ggplot2::margin(4, 6, 4, 6))
  if (nrow(pk) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = prot_len / 2, y = 0,
                          label = "no mapped peptides", color = "grey50") +
        ggplot2::scale_x_continuous(limits = c(1, prot_len),
                                    breaks = seq(0L, prot_len, by = 10L)) +
        ggplot2::labs(title = title, subtitle = sprintf("Wood's plot: %s", contrast),
                      x = "Residue position", y = "log2FC") +
        base_theme)
  }

  pk$neglogp <- pmin(-log10(pmax(pk$.adjp, .Machine$double.xmin)),
                     .PELSA_WOODS_NEGLOG_CAP)
  pk$neglogp[is.na(pk$.adjp)] <- 0
  pk$is_sig <- !is.na(pk$.adjp) & pk$.adjp < sig_cutoff

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
      name = "-log10(adj.P)", guide = ggplot2::guide_colourbar(order = 2)) +
    ggplot2::scale_x_continuous(limits = c(1, prot_len),
                                expand = ggplot2::expansion(mult = 0.01),
                                breaks = seq(0L, prot_len, by = 10L)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.04, 0.16))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = title, subtitle = sprintf("Wood's plot: %s", contrast),
      caption = sprintf("*Significant peptides (adj.P < %s)", format(sig_cutoff)),
      x = "Residue position", y = "log2FC") +
    base_theme
  gg
}
