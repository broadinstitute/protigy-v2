################################################################################
# Module: PELSA volcano PLOT-ASSEMBLY (native plotly) helpers - pure, no Shiny.
#
# Split out of R/tab_pelsa_volcano_helpers.R (which grew past the repo's
# 800-line file cap) to keep the plot-building concern separate from the
# volcano data-frame builder / background thinning / label & color-mode
# selection helpers that remain there.
#
# Builds the native plotly (scattergl/scatter) volcano figure itself: the
# shared hover-tip text, the gold selection/find overlay trace, the clicked-
# point trace, the main pelsa_volcano_build_plot() assembly, and its label
# annotations. See R/tab_pelsa_volcano_helpers.R's header for the WebGL bake-in
# rationale (relayout/restyle do not reliably re-render on scattergl).
################################################################################

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
  name_fb <- if ("winning_protein_name" %in% colnames(d))
    d$winning_protein_name else rep(NA_character_, nrow(d))
  stem <- pelsa_resolve_label_stem(gene_fb, name_fb, acc_fb)
  pep_lab <- ifelse(is.na(d$pep_start), as.character(stem),
                    paste0(stem, "_aa", d$pep_start))
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
pelsa_volcano_gold_trace <- function(df, selection = NULL, find_mask = NULL,
                                     use_webgl = TRUE) {
  if (!is.data.frame(df) || nrow(df) == 0L) return(NULL)
  m <- pelsa_volcano_highlight_mask(df, selection, find_mask)
  if (!any(m)) return(NULL)
  d <- df[m, , drop = FALSE]
  # as.list() forces x/y/text to serialize as JSON ARRAYS even for a SINGLE
  # highlighted point (one peptide with no siblings). plotlyProxyInvoke goes
  # through jsonlite, which collapses a length-1 vector to a scalar; a scattergl
  # trace then can't index it and the gold dot vanishes. (Same guard as
  # pelsa_volcano_clicked_point_trace.)
  list(
    type = if (isTRUE(use_webgl)) "scattergl" else "scatter", mode = "markers",
    x = as.list(as.numeric(d$logFC)), y = as.list(as.numeric(d$logP)),
    text = as.list(pelsa_volcano_tip(d)), hoverinfo = "text",
    marker = list(color = .PELSA_GOLD, size = 7,
                  line = list(color = .PELSA_VOLCANO_MARKER_EDGE, width = 0.5)),
    showlegend = FALSE, meta = "pelsa_gold"
  )
}

# Build the CLICKED-POINT emphasis overlay trace for the clicked peptide only
# (NOT its siblings): a one-point scattergl "markers" trace, ready for
# plotlyProxyInvoke("addTraces", ...). It carries the SAME gold fill
# (.PELSA_GOLD) as the gold highlight of its siblings, but a LARGER dot
# (.PELSA_CLICK_PT_SIZE) with a THICKER black outline (.PELSA_CLICK_PT_RING_W)
# so the clicked peptide stands out from the same-protein gold dots beneath it.
# Drawn on top of the gold overlay at the SAME (logFC, logP), so it reads as one
# emphasized gold point. Carries the standard 6-line hover (pelsa_volcano_tip).
#
# The clicked row is resolved by selection$row (a volcano click) with a
# peptide_seq fallback (a Woods click carries row=NA). Returns NULL when nothing
# is selected, the row cannot be resolved (e.g. a multi-accession Find sets
# selection() to NULL), or the row has NA coordinates. @noRd
pelsa_volcano_clicked_point_trace <- function(df, selection = NULL,
                                              use_webgl = TRUE) {
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

  # x/y/text are wrapped in list() so a SINGLE point serializes to a JSON ARRAY
  # ([5.68]) rather than a scalar (5.68). plotlyProxyInvoke("addTraces", ...)
  # goes through jsonlite, which collapses a length-1 vector to a scalar; a
  # scattergl trace then reads x[0] as undefined -> NaN pixel -> the point never
  # paints. Forcing arrays keeps the one-point overlay renderable. (The gold
  # overlay escaped this only because it usually has >=2 points; see
  # pelsa_volcano_gold_trace for the same guard.)
  list(
    type = if (isTRUE(use_webgl)) "scattergl" else "scatter", mode = "markers",
    x = list(as.numeric(d$logFC)), y = list(as.numeric(d$logP)),
    text = list(pelsa_volcano_tip(d)), hoverinfo = "text",
    marker = list(color = .PELSA_GOLD, size = .PELSA_CLICK_PT_SIZE,
                  line = list(color = .PELSA_VOLCANO_MARKER_EDGE,
                              width = .PELSA_CLICK_PT_RING_W)),
    showlegend = FALSE, meta = "pelsa_gold_click"
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
# The build ALWAYS emits exactly TWO point traces (background + markers), which
# are meta-tagged ("pelsa_bg"/"pelsa_mk"). The PRODUCTION selection highlight is
# a GOLD OVERLAY: a separate scattergl trace (plus an optional label trace) is
# pushed/removed via plotlyProxyInvoke addTraces/deleteTraces (apply_gold_overlay
# in tab_pelsa_section3.R), so a click/find never rebuilds the ~100k-point base
# figure. (The pelsa_volcano_recolor / .pelsa_volcano_trace_index proxy-restyle
# path is an earlier approach kept only for unit tests -- it is NOT wired into
# the module; per CLAUDE.md, per-point marker.color restyle does not render
# reliably on WebGL scattergl, which is why the addTraces overlay is used.)
#
# @param df          the FULL volcano frame the plot consumes (every point).
# @param full_df     the same frame, used for the y_cutoff attr + label-row
#   selection over all rows. Defaults to df.
# @param color_mode  "significance" | "feature".
# @param label_mode  a character vector of pelsa_volcano_label_rows() modes
#   (possibly empty).
# @param n_top_adjp         N for the "down" bucket of "top_n_adjp"; the "up"
#   bucket keeps ceiling(N / 2) (default 3).
# @param n_top_markers      N for the "down" bucket of "top_n_markers"; the
#   "up" bucket keeps ceiling(N / 2) (default 3).
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
                                     label_mode = character(0),
                                     n_top_adjp = 3L,
                                     n_top_markers = 3L,
                                     source_id = "pelsa_volcano",
                                     selection = NULL, find_mask = NULL,
                                     register_click = FALSE,
                                     use_webgl = TRUE) {
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
  # The marker/background traces are meta-tagged so the test-only recolor helper
  # can find them by index (see .pelsa_volcano_trace_index; the PRODUCTION
  # highlight is the addTraces gold overlay, which does not read these tags).
  # With a hand-built figure the trace order is deterministic, so the bg +
  # marker traces are added FIRST and in that order (always index 0 and 1). The
  # scalar `meta` tag is
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

  # WebGL vs SVG render backend. scattergl draws the cloud on the GPU (fast, but
  # paints blank if the client browser has no WebGL context); scatter is the SVG
  # fallback. The caller resolves this from the client probe (see webgl_capability
  # + app_UI). Default TRUE preserves the WebGL path for every capable client.
  trace_type <- if (isTRUE(use_webgl)) "scattergl" else "scatter"

  p <- plotly::plot_ly(source = source_id)

  # 0. BACKGROUND cloud (always added so the bg trace exists at index 0; an empty
  #    frame yields an empty trace, which keeps the meta indices stable).
  bg_tip <- tip(bg)
  p <- plotly::add_trace(
    p, type = trace_type, mode = "markers",
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
    p, type = trace_type, mode = "markers",
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
      p, type = trace_type, mode = "markers",
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
      p, type = trace_type, mode = "markers",
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
    pelsa_volcano_label_rows(full_df, mode = label_mode,
                             n_top_adjp = n_top_adjp,
                             n_top_markers = n_top_markers),
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
      dist <- sqrt((nx - pl$nx)^2 + (ny - pl$ny)^2)
      if (!is.na(dist) && dist < min_dist) {
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
