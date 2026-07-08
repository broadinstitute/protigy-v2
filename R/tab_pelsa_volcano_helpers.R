################################################################################
# Module: PELSA volcano background thinning (Task 3B) - pure, no Shiny.
#
# The PELSA volcano can carry 100k+ points. plotly's toWebGL renders them, but
# the dense, uninformative non-significant cloud is the part that costs the most
# to draw and adds nothing the user can act on. This helper thins ONLY that
# background - never points the user might want to click.
#
# Thinnable set (a point is thinnable IFF ALL of):
#   1. NOT significant  (Significant == FALSE), AND
#   2. abs(logFC) <= logfc_thresh, AND
#   3. NOT a marker-protein peptide (is_marker == FALSE).
# EVERYTHING ELSE IS RETAINED, NEVER THINNED:
#   - every significant peptide (Significant == TRUE),
#   - every peptide with abs(logFC) > logfc_thresh (a sizeable effect is worth a
#     click even when non-significant),
#   - every marker-protein peptide (is_marker == TRUE).
#
# Density-PROPORTIONAL (not uniform): 2-D bin the thinnable points over
# (logFC, logP) into an n_bins x n_bins grid spanning the thinnable points' own
# range, then within EACH non-empty bin keep ceiling(keep_frac * n_bin_points)
# points sampled WITHOUT replacement. A fixed fraction per bin means dense bins
# stay dense and sparse bins stay sparse - the cloud's shape/spread is preserved.
# Contrast with uniform "keep every Nth row", which flattens relative density.
#
# Cost: the only per-group work is over BINS (<= n_bins^2 <= 2500), via split();
# there is no per-point loop, so ~100k thinnable points thin in well under a
# second.
#
# Consumes the Task 3A pelsa_build_volcano_df() output columns: Significant
# (logical), logFC (numeric), logP (numeric, -log10 P.Value), is_marker
# (logical). Phase 7 calls this before plotly + toWebGL and surfaces the returned
# counts as a "showing N of M background points" honesty note.
################################################################################

# Density-proportional thinning of the PELSA volcano background cloud.
#
# Thins ONLY the non-significant, small-effect, non-marker background, keeping a
# fixed FRACTION of points per 2-D (logFC, logP) bin so relative density is
# preserved. All non-thinnable rows pass through untouched.
#
# @param volcano_df data.frame with columns Significant (logical), logFC
#   (numeric), logP (numeric), is_marker (logical). Typically the 3A
#   pelsa_build_volcano_df() output.
# @param keep_frac fraction of each bin to keep, in (0, 1]. keep_frac >= 1 is a
#   no-op (everything kept, no thinning). Default 0.3. Because each non-empty bin
#   keeps ceiling(keep_frac * n) >= 1, a tiny keep_frac (e.g. 0.001) still keeps
#   at least one point per non-empty bin, so the global n_thinnable_kept can
#   exceed keep_frac * n_thinnable - this is intended, it preserves sparse
#   structure rather than erasing whole regions.
# @param logfc_thresh abs(logFC) threshold; points with abs(logFC) above it are
#   ALWAYS retained. Default 0.5.
# @param n_bins number of bins per axis (grid is n_bins x n_bins). Default 50.
# @param seed if non-NULL, set.seed(seed) for reproducible within-bin sampling;
#   if NULL the RNG is left untouched (the caller may seed for determinism).
# @return list(df, n_shown, n_total, n_thinnable, n_thinnable_kept) where df is
#   the retained rows (all non-thinnable rows + the kept thinnable sample) in
#   ORIGINAL row order, and n_shown = n_total - (n_thinnable - n_thinnable_kept).
#   A thinnable row whose logFC/logP coordinate is NA OR non-finite (Inf/-Inf,
#   e.g. logP from a P.Value of 0) cannot be binned and is RETAINED untouched.
# @noRd
pelsa_thin_background <- function(volcano_df, keep_frac = 0.3,
                                  logfc_thresh = 0.5, n_bins = 50,
                                  seed = NULL) {
  # ---- boundary validation (fail fast) --------------------------------------
  if (!is.data.frame(volcano_df)) {
    stop("pelsa_thin_background: volcano_df must be a data.frame")
  }
  required <- c("Significant", "logFC", "logP", "is_marker")
  missing_cols <- setdiff(required, names(volcano_df))
  if (length(missing_cols) > 0) {
    stop("pelsa_thin_background: volcano_df missing required column(s): ",
         paste(missing_cols, collapse = ", "))
  }
  if (length(keep_frac) != 1L || is.na(keep_frac) || !is.numeric(keep_frac) ||
      keep_frac <= 0) {
    stop("pelsa_thin_background: keep_frac must be a single number in (0, 1]")
  }
  if (length(logfc_thresh) != 1L || is.na(logfc_thresh) ||
      !is.numeric(logfc_thresh) || logfc_thresh < 0) {
    stop("pelsa_thin_background: logfc_thresh must be a single non-negative number")
  }
  if (length(n_bins) != 1L || is.na(n_bins) || !is.numeric(n_bins) ||
      n_bins < 1) {
    stop("pelsa_thin_background: n_bins must be a single positive integer")
  }
  n_bins <- as.integer(n_bins)

  n_total <- nrow(volcano_df)

  # ---- identify the thinnable set (the 3 ANDs) ------------------------------
  thinnable <- !volcano_df$Significant &
    !is.na(volcano_df$logFC) & abs(volcano_df$logFC) <= logfc_thresh &
    !volcano_df$is_marker
  # NA in Significant/is_marker must not silently become thinnable.
  thinnable[is.na(thinnable)] <- FALSE

  n_thinnable <- sum(thinnable)

  # No-op fast paths: nothing thinnable, or keep_frac keeps everything anyway.
  if (n_thinnable == 0L || keep_frac >= 1) {
    return(list(
      df               = volcano_df,
      n_shown          = n_total,
      n_total          = n_total,
      n_thinnable      = n_thinnable,
      n_thinnable_kept = n_thinnable
    ))
  }

  if (!is.null(seed)) set.seed(seed)

  # Row indices of the thinnable cloud, then split off rows whose coords cannot
  # be binned - NA OR non-finite (Inf/-Inf, e.g. logP = -log10(0) when an
  # upstream permutation/underflow P.Value is 0). is.finite() is FALSE for both
  # NA and +/-Inf, so it covers both cases. Those rows are RETAINED untouched.
  thin_idx <- which(thinnable)
  fc <- volcano_df$logFC[thin_idx]
  lp <- volcano_df$logP[thin_idx]
  binnable_mask <- is.finite(fc) & is.finite(lp)
  binnable_idx <- thin_idx[binnable_mask]
  na_coord_idx <- thin_idx[!binnable_mask]  # retained untouched (NA/Inf coords)

  kept_thin_idx <- na_coord_idx  # start with the un-binnable thinnable rows

  if (length(binnable_idx) > 0L) {
    fc_b <- fc[binnable_mask]
    lp_b <- lp[binnable_mask]

    # 2-D bin over the thinnable points' OWN range. findInterval() with n_bins-1
    # interior breakpoints yields bin ids in 0..(n_bins-1); a degenerate
    # (zero-width) range collapses to a single bin, which is fine.
    bin_axis <- function(x, n_bins) {
      rng <- range(x)
      if (rng[1L] == rng[2L]) return(rep.int(0L, length(x)))
      breaks <- seq(rng[1L], rng[2L], length.out = n_bins + 1L)
      # interior breaks only; rightmost point lands in the last bin.
      findInterval(x, breaks[-c(1L, length(breaks))], rightmost.closed = TRUE)
    }
    bx <- bin_axis(fc_b, n_bins)
    by <- bin_axis(lp_b, n_bins)
    bin_id <- bx * n_bins + by  # unique per (bx, by) cell

    # Per-BIN sampling via split() - split() itself is O(n) but cheap; the only
    # GROUP work is over bins (<= n_bins^2 <= 2500), never a per-point loop.
    by_bin <- split(binnable_idx, bin_id)
    kept_per_bin <- lapply(by_bin, function(rows) {
      n <- length(rows)
      k <- ceiling(keep_frac * n)  # singleton bins keep >= 1, never vanish
      if (k >= n) return(rows)
      rows[sample.int(n, k)]
    })
    kept_thin_idx <- c(kept_thin_idx, unlist(kept_per_bin, use.names = FALSE))
  }

  n_thinnable_kept <- length(kept_thin_idx)

  # Retained rows = all non-thinnable rows + the kept thinnable rows, restored to
  # ORIGINAL row order so downstream tooltips/ordering are stable.
  keep_idx <- sort(c(which(!thinnable), kept_thin_idx))
  out_df <- volcano_df[keep_idx, , drop = FALSE]
  rownames(out_df) <- NULL

  list(
    df               = out_df,
    n_shown          = nrow(out_df),
    n_total          = n_total,
    n_thinnable      = n_thinnable,
    n_thinnable_kept = n_thinnable_kept
  )
}
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
# set of label modes. Labels are FIXED to the 3A `label` column (the ;-joined
# <gene>_aa<pos>); only WHICH rows are labeled varies.
#
# Modes (a CHARACTER VECTOR - zero or more of the four below; the checkbox
# group in the PELSA sidebar allows selecting any combination):
#   "all_markers"        every marker-protein peptide (is_marker == TRUE).
#   "all_significant"    every significant peptide (Significant == TRUE).
#   "top_n_adjp"         the n_top_adjp smallest adj.P.Val peptides in the
#                        "down" logFC-sign bucket (logFC < 0), plus
#                        ceiling(n_top_adjp / 2) smallest adj.P.Val peptides
#                        in the "up" bucket (logFC >= 0); union of both
#                        buckets. Ranks ALL peptides regardless of
#                        significance. Ties in adj.P.Val (common at high test
#                        counts, where BH-adjustment collapses many raw
#                        p-values to a shared plateau) are broken by the
#                        smallest raw P.Value, or by the largest |logFC| when
#                        P.Value is unavailable.
#   "top_n_markers"      per MARKER (grouped by winning_accession): the
#                        n_top_markers smallest adj.P.Val MARKER peptides
#                        (is_marker == TRUE) in that marker's "down" logFC-sign
#                        bucket (logFC < 0), plus ceiling(n_top_markers / 2)
#                        smallest adj.P.Val MARKER peptides in that marker's
#                        "up" bucket (logFC >= 0). The union across all markers
#                        is returned, so N is applied independently per marker
#                        (one marker cannot crowd out another). Ranks ALL marker
#                        peptides regardless of significance. Same adj.P.Val
#                        tiebreak as "top_n_adjp" (raw P.Value, then |logFC|).
#                        Marker rows with a missing/blank winning_accession are
#                        pooled into one catch-all group.
#
# Returns the UNION of matching rows across every mode in the vector, as
# 1-based row indices (sorted, unique). An empty/NULL `mode` returns
# integer(0) (no labels). If a top-N bucket has fewer than N eligible rows,
# all of them are kept (no padding, no error).
#
# @param volcano_df        a 3A frame (label, is_marker, Significant,
#                          sig_direction, adj.P.Val, logFC).
# @param mode               a character vector; each element one of the four
#                           modes above. NULL or character(0) means no labels.
# @param n_top_adjp         N for the "down" bucket of "top_n_adjp"; the "up"
#                           bucket keeps ceiling(N / 2) (default 3, coerced
#                           to >= 1).
# @param n_top_markers      N for the "down" bucket of "top_n_markers", applied
#                           PER marker; each marker's "up" bucket keeps
#                           ceiling(N / 2) (default 3, coerced to >= 1).
# @return integer vector of row indices to label.
# @noRd
pelsa_volcano_label_rows <- function(volcano_df, mode = character(0),
                                     n_top_adjp = 3L,
                                     n_top_markers = 3L) {
  if (!is.data.frame(volcano_df)) {
    stop("pelsa_volcano_label_rows: volcano_df must be a data.frame")
  }
  mode <- mode %||% character(0)
  mode <- as.character(mode)
  if (length(mode) == 0L) return(integer(0))
  if (anyNA(mode) || !all(mode %in% .PELSA_VOLCANO_LABEL_MODES)) {
    stop("pelsa_volcano_label_rows: mode must be one of ",
         paste(sprintf("'%s'", .PELSA_VOLCANO_LABEL_MODES), collapse = ", "))
  }
  n <- nrow(volcano_df)
  if (n == 0L) return(integer(0))

  is_m <- volcano_df$is_marker %||% rep(FALSE, n)
  is_m[is.na(is_m)] <- FALSE
  sig <- volcano_df$Significant %||% rep(FALSE, n)
  sig[is.na(sig)] <- FALSE

  idx <- integer(0)
  if ("all_markers" %in% mode)     idx <- c(idx, which(is_m))
  if ("all_significant" %in% mode) idx <- c(idx, which(sig))

  if ("top_n_adjp" %in% mode) {
    logfc <- as.numeric(volcano_df$logFC %||% rep(NA_real_, n))
    adjp  <- as.numeric(volcano_df$adj.P.Val %||% rep(NA_real_, n))
    # Tiebreak for the massive plateaus BH-adjustment routinely produces at
    # this many tests: prefer the raw (pre-correction) P.Value -- smaller is
    # more significant -- when available. When P.Value is absent (should not
    # happen in practice; the shared frame-builders always set it, but this
    # guards any future/edge-case caller), fall back to the largest |logFC|
    # (negated so ascending order() still picks it first).
    rawp <- volcano_df$P.Value
    tb   <- if (!is.null(rawp)) as.numeric(rawp) else -abs(logfc)
    direction <- ifelse(is.na(logfc), "ns", ifelse(logfc < 0, "down", "up"))
    # PELSA weights downregulated peptides more heavily: the down bucket
    # keeps the full requested N, the up bucket keeps only half (rounded up).
    n_down <- max(1L, as.integer(n_top_adjp)[1L])
    n_up   <- ceiling(n_down / 2)
    idx <- c(idx, .pelsa_top_n_by_direction(seq_len(n), direction, adjp,
                                            n_top_down = n_down,
                                            n_top_up = n_up,
                                            tiebreak_value = tb))
  }

  if ("top_n_markers" %in% mode) {
    marker_idx <- which(is_m)
    if (length(marker_idx) > 0L) {
      logfc <- as.numeric(volcano_df$logFC %||% rep(NA_real_, n))
      adjp  <- as.numeric(volcano_df$adj.P.Val %||% rep(NA_real_, n))
      rawp  <- volcano_df$P.Value
      tb    <- if (!is.null(rawp)) as.numeric(rawp) else -abs(logfc)
      n_down_mk <- max(1L, as.integer(n_top_markers)[1L])
      n_up_mk   <- ceiling(n_down_mk / 2)
      # PER-MARKER: group marker rows by their winning_accession and run the
      # per-direction top-N once within each marker so no marker can crowd out
      # another. A missing (NA) or blank winner would silently drop a marker
      # from the grouping; fold all such rows into one catch-all group instead
      # (marker rows normally carry a reconciled non-empty winner, so this only
      # guards an edge case). Group keys use the row's winning_accession value.
      winner <- as.character(volcano_df$winning_accession %||%
                               rep(NA_character_, n))
      grp_key <- winner[marker_idx]
      grp_key[is.na(grp_key) | !nzchar(grp_key)] <- "__pelsa_na_marker__"
      for (g in unique(grp_key)) {
        g_rows <- marker_idx[grp_key == g]
        g_dir  <- ifelse(is.na(logfc[g_rows]), "ns",
                         ifelse(logfc[g_rows] < 0, "down", "up"))
        idx <- c(idx, .pelsa_top_n_by_direction(g_rows, g_dir, adjp[g_rows],
                                                n_top_down = n_down_mk,
                                                n_top_up = n_up_mk,
                                                tiebreak_value = tb[g_rows]))
      }
    }
  }

  sort(unique(idx))
}

# Keep the top N rows with the smallest `value` within each of the "up"/"down"
# buckets of `direction` (any other direction value, e.g. "ns", is excluded
# from both buckets). `n_top_down`/`n_top_up` are independent per-bucket
# limits (PELSA weights downregulated peptides more heavily by default, so
# callers commonly pass a smaller n_top_up than n_top_down -- see
# pelsa_volcano_label_rows). `idx` are the original row indices these
# (direction, value) entries correspond to. Stable: ties / NA values resolve
# by original index order; NA values sort last. If a bucket has fewer than
# its n_top eligible rows, all of them are kept (no padding, no error).
#
# @param n_top_down number of rows to keep from the "down" bucket.
# @param n_top_up   number of rows to keep from the "up" bucket.
# @param tiebreak_value optional secondary sort key (same length as `idx`/
#   `direction`/`value`), used ascending to break ties in `value` before
#   falling back to original index order. Pass a NEGATED value if "largest
#   wins" is desired (e.g. -abs(logFC)).
# @return sorted unique original indices kept (union of both buckets).
# @noRd
.pelsa_top_n_by_direction <- function(idx, direction, value,
                                      n_top_down, n_top_up,
                                      tiebreak_value = NULL) {
  clamp_n <- function(n, default = 5L) {
    n <- max(1L, as.integer(n)[1L])
    if (is.na(n)) default else n
  }
  n_top_down <- clamp_n(n_top_down)
  n_top_up   <- clamp_n(n_top_up)
  keep_bucket <- function(want, n_top) {
    bucket <- which(direction == want)
    if (length(bucket) == 0L) return(integer(0))
    bucket_idx <- idx[bucket]
    bucket_val <- value[bucket]
    ord <- if (!is.null(tiebreak_value)) {
      order(bucket_val, tiebreak_value[bucket], bucket_idx, na.last = TRUE)
    } else {
      order(bucket_val, bucket_idx, na.last = TRUE)
    }
    head(bucket_idx[ord], n_top)
  }
  sort(unique(c(keep_bucket("up", n_top_up), keep_bucket("down", n_top_down))))
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

