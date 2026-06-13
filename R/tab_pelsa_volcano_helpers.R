################################################################################
# Module: PELSA volcano data-frame builder (Task 3A) — pure, no Shiny.
#
# Produces ONE tidy data.frame that the Phase-7 plotly volcano renders. It does
# NOT plot and does NOT compute statistics — the Statistics tab supplies
# logFC.<contrast> / adj.P.Val.<contrast> / P.Value.<contrast> upstream (PELSA
# never recomputes BH).
#
# This builder COMPOSES the verified Phase-2 helpers (it reimplements none of
# them):
#   2C pelsa_build_multilabel()    -> the ;-joined <gene>_aa<pos> point label
#   2I pelsa_annotate_features()   -> feature_class_primary + winning_acc/gene,
#                                     PELSA_FEATURE_COLORS -> feature_color
#   2J pelsa_match_markers()       -> is_marker (isoform-symmetric, by accession)
#   2G pelsa_best_peptide_rollup() -> the best-peptide panel's one-dot-per-peptide
#
# Panels (opts$panel):
#   "all_peptide" (default): NO explode — exactly ONE dot per SOURCE peptide
#     row. The dot's color resolves across all ;-accession tokens (feature
#     annotation runs on the ;-frame), and its label is the multilabel across
#     all of the peptide's (gene, pep_start, accession) mappings from the
#     matched cache.
#   "best_peptide": one dot per distinct best-peptide via the 2G rollup over the
#     exploded+stat frame, then the same sig/feature/marker columns attached.
#
# Two-sided significance coloring (Decision #4): a significant peptide is colored
# whether it goes UP (darkred) or DOWN (a blue, #1f4e9c) — down is NOT a filter.
# Non-significant -> "gray". (The app's volcano marker overlay hex #FF00FF is a
# Phase-7 plotting concern, not encoded here.)
#
# JOIN: the matched cache (2B) carries one row per (peptide, accession,
# occurrence) plus the stable `.row_id` synthesized by 2B's explode (the 1-based
# source-peptide-row index). The all-peptide panel keys peptide<->matched_cache
# on `.row_id` when both carry it (the robust, sequence-collision-proof key);
# otherwise it falls back to PEP.StrippedSequence. One output row per peptide.
#
# Hardening: vectorized. The all-peptide multilabel builds each per-mapping
# entry string ("<gene-or-accession>_aa<pos>") in ONE pass over the flat matched
# column vectors, then collapses per peptide with a data.table group paste —
# the pelsa_build_multilabel R closure is NOT invoked per peptide (that 80k-call
# group-op was the profiled hot spot; the inline form is byte-identical and ~10x
# faster). annotation/marker matching are already vectorized on the ;-frame.
# pelsa_build_multilabel remains the canonical single-label builder for small
# paths (e.g. the 2G best-peptide rollup).
################################################################################

# Two-sided significance colors (Decision #4). Down uses a disciplined blue
# (#1f4e9c) distinct from the up red so both significant directions read.
.PELSA_SIG_COLOR_UP   <- "darkred"
.PELSA_SIG_COLOR_DOWN <- "#1f4e9c"
.PELSA_SIG_COLOR_NS   <- "gray"

# Resolve the three contrast stat columns from `contrast`, or accept already-
# renamed logFC/adj.P.Val/P.Value when `contrast` is NULL.
#
# @return named list logfc/adjp/pval of column names present in stat_df
# @noRd
.pelsa_volcano_stat_cols <- function(stat_df, contrast) {
  if (is.null(contrast)) {
    cols <- c(logfc = "logFC", adjp = "adj.P.Val", pval = "P.Value")
  } else {
    if (length(contrast) != 1L || is.na(contrast) || !nzchar(contrast)) {
      stop("pelsa_build_volcano_df: contrast must be NULL or a single ",
           "non-empty string")
    }
    cols <- c(
      logfc = paste0("logFC.", contrast),
      adjp  = paste0("adj.P.Val.", contrast),
      pval  = paste0("P.Value.", contrast)
    )
  }
  missing_cols <- cols[!cols %in% colnames(stat_df)]
  if (length(missing_cols) > 0L) {
    stop("pelsa_build_volcano_df: stat_df missing required stat column(s): ",
         paste(unname(missing_cols), collapse = ", "))
  }
  as.list(cols)
}

# Build the ;-joined multilabel for every source peptide, FULLY VECTORIZED.
#
# Mirrors pelsa_build_multilabel()'s per-label logic but applied to the WHOLE
# matched cache at once (no per-peptide closure call): each mapping's entry is
# "<gene>_aa<pos>" with the gene -> accession fallback when the gene is
# empty/NA, built in one vectorized pass; entries are then collapsed per peptide
# with a data.table group paste over unique() (which preserves first-occurrence
# order). Within-group order is fixed by a stable setorder on (pep_start,
# accession) BEFORE the group paste, so the label is identical to
# pelsa_build_multilabel()'s !duplicated()-on-sorted output.
#
# @param matched   the 2B $matched cache (peptide x accession x occurrence)
# @param key_col   the join key column name present in matched ("..key")
# @return data.frame(key, label) one row per distinct key
# @noRd
.pelsa_volcano_labels <- function(matched, key_col) {
  dt <- data.table::data.table(
    .key      = matched[[key_col]],
    gene      = as.character(matched[["gene"]]),
    pep_start = matched[["pep_start"]],
    accession = as.character(matched[["accession"]])
  )
  # Deterministic within-group order: by pep_start then accession. setorder is a
  # stable sort, so equal-start entries keep input order.
  data.table::setorder(dt, .key, pep_start, accession, na.last = TRUE)

  # Per-mapping entry string, VECTORIZED over the whole column (no per-peptide
  # call). fifelse: empty/NA gene -> accession fallback, then "<id>_aa<pos>".
  # This is byte-identical to pelsa_build_multilabel()'s per-entry construction.
  lid <- data.table::fifelse(
    is.na(dt$gene) | !nzchar(trimws(dt$gene)), dt$accession, dt$gene
  )
  dt[, entry := paste0(lid, "_aa", pep_start)]

  # Collapse per peptide: unique() preserves first-occurrence (sorted) order,
  # matching pelsa_build_multilabel()'s !duplicated() dedup before the ;-join.
  lab <- dt[, list(label = paste(unique(entry), collapse = ";")), by = ".key"]
  out <- as.data.frame(lab, stringsAsFactors = FALSE)
  names(out)[names(out) == ".key"] <- key_col
  out
}

# Attach the two-sided significance columns (Significant / sig_direction /
# sig_color) to a frame already carrying logFC / adj.P.Val. Vectorized.
#
# @noRd
.pelsa_attach_significance <- function(df, sig_cutoff) {
  sig <- !is.na(df$adj.P.Val) & df$adj.P.Val < sig_cutoff
  up <- sig & !is.na(df$logFC) & df$logFC > 0
  down <- sig & !is.na(df$logFC) & df$logFC < 0
  sig_direction <- rep("ns", nrow(df))
  sig_direction[up] <- "up"
  sig_direction[down] <- "down"
  sig_color <- rep(.PELSA_SIG_COLOR_NS, nrow(df))
  sig_color[up] <- .PELSA_SIG_COLOR_UP
  sig_color[down] <- .PELSA_SIG_COLOR_DOWN
  df$Significant <- sig
  df$sig_direction <- sig_direction
  df$sig_color <- sig_color
  df
}

# Empirical raw-p threshold (the dashed line): -log10 of the largest raw P.Value
# among peptides passing adj.P.Val < sig_cutoff; Inf if none pass. Mirrors
# build_volcano_df()'s adj.p.val branch.
#
# @noRd
.pelsa_volcano_y_cutoff <- function(adjp, pval, sig_cutoff) {
  passing <- which(!is.na(adjp) & adjp < sig_cutoff)
  if (length(passing) == 0L) return(Inf)
  -log10(max(pval[passing], na.rm = TRUE))
}

# Build the PELSA volcano data.frame (one row per peptide for all_peptide).
#
# @param stat_df       per-peptide frame with the chosen contrast's stat columns
#   (logFC.<contrast> / adj.P.Val.<contrast> / P.Value.<contrast>, OR renamed
#   logFC / adj.P.Val / P.Value when `contrast` is NULL) plus the peptide
#   identity columns PEP.StrippedSequence / PG.ProteinAccessions /
#   (;-aligned) PG.Genes and the representative pep_start / pep_end span.
# @param matched_cache the 2B $matched frame (peptide x accession x occurrence),
#   joined to stat_df by `.row_id` (preferred) or PEP.StrippedSequence.
# @param feat_df       UniProt feature table (2I/2H) for feature-class coloring.
# @param markers       character vector of marker accessions (Setup; isoforms ok)
# @param contrast      the contrast key, or NULL for already-renamed columns.
# @param opts          list: sig_cutoff (0.05), panel ("all_peptide" |
#   "best_peptide"), logfc_cap (optional display clamp; NULL = no clamp).
# @return data.frame, one row per peptide (all_peptide) / per best-peptide
#   (best_peptide), with attr(.,"y_cutoff").
# @noRd
pelsa_build_volcano_df <- function(stat_df, matched_cache, feat_df, markers,
                                   contrast, opts = list()) {
  # ---- Boundary validation (fail fast) ------------------------------------
  if (!is.data.frame(stat_df)) {
    stop("pelsa_build_volcano_df: stat_df must be a data.frame")
  }
  if (!is.data.frame(matched_cache)) {
    stop("pelsa_build_volcano_df: matched_cache must be a data.frame")
  }
  if (!is.data.frame(feat_df)) {
    stop("pelsa_build_volcano_df: feat_df must be a data.frame")
  }
  if (!is.character(markers)) {
    stop("pelsa_build_volcano_df: markers must be a character vector")
  }

  sig_cutoff <- opts$sig_cutoff %||% 0.05
  panel <- opts$panel %||% "all_peptide"
  logfc_cap <- opts$logfc_cap
  if (!panel %in% c("all_peptide", "best_peptide")) {
    stop("pelsa_build_volcano_df: opts$panel must be 'all_peptide' or ",
         "'best_peptide'")
  }

  stat_cols <- .pelsa_volcano_stat_cols(stat_df, contrast)

  if (!"PEP.StrippedSequence" %in% colnames(stat_df)) {
    stop("pelsa_build_volcano_df: stat_df must have PEP.StrippedSequence")
  }
  if (!"PG.ProteinAccessions" %in% colnames(stat_df)) {
    stop("pelsa_build_volcano_df: stat_df must have PG.ProteinAccessions")
  }
  if (!all(c("pep_start", "pep_end") %in% colnames(stat_df))) {
    stop("pelsa_build_volcano_df: stat_df must have pep_start and pep_end")
  }
  matched_required <- c("accession", "gene", "pep_start")
  if (!all(matched_required %in% colnames(matched_cache))) {
    stop("pelsa_build_volcano_df: matched_cache must have ",
         paste(matched_required, collapse = "/"))
  }

  if (panel == "best_peptide") {
    return(.pelsa_build_volcano_best(stat_df, matched_cache, feat_df, markers,
                                     contrast, stat_cols, sig_cutoff,
                                     logfc_cap))
  }
  .pelsa_build_volcano_all(stat_df, matched_cache, feat_df, markers,
                           stat_cols, sig_cutoff, logfc_cap)
}

# ---- all-peptide panel (one dot per source peptide, no explode) -------------

# @noRd
.pelsa_build_volcano_all <- function(stat_df, matched_cache, feat_df, markers,
                                     stat_cols, sig_cutoff, logfc_cap) {
  n <- nrow(stat_df)

  # Determine the peptide<->matched join key: `.row_id` when present on BOTH
  # (collision-proof), else PEP.StrippedSequence.
  use_row_id <- ".row_id" %in% colnames(stat_df) &&
    ".row_id" %in% colnames(matched_cache)
  key_col <- if (use_row_id) ".row_id" else "PEP.StrippedSequence"

  # Project the stat frame to the standardized volcano columns. One row in =
  # one dot out (no explode).
  df <- data.frame(
    id        = as.character(stat_df[["PEP.StrippedSequence"]]),
    logFC     = as.numeric(stat_df[[stat_cols$logfc]]),
    adj.P.Val = as.numeric(stat_df[[stat_cols$adjp]]),
    P.Value   = as.numeric(stat_df[[stat_cols$pval]]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  df$logP <- -log10(df$P.Value)
  df$PG.ProteinAccessions <- as.character(stat_df[["PG.ProteinAccessions"]])
  df$PG.Genes <- if ("PG.Genes" %in% colnames(stat_df)) {
    as.character(stat_df[["PG.Genes"]])
  } else {
    rep(NA_character_, n)
  }
  df$pep_start <- as.integer(stat_df[["pep_start"]])
  df$pep_end <- as.integer(stat_df[["pep_end"]])

  # ---- 2I feature annotation on the ;-frame (multi-protein resolution) -----
  # pelsa_annotate_features reads PG.ProteinAccessions + pep_start/pep_end; the
  # dot stays singular (one row per peptide) while color resolves across tokens.
  annot_in <- data.frame(
    PG.ProteinAccessions = df$PG.ProteinAccessions,
    PG.Genes             = df$PG.Genes,
    pep_start            = df$pep_start,
    pep_end              = df$pep_end,
    stringsAsFactors     = FALSE,
    check.names          = FALSE
  )
  annotated <- pelsa_annotate_features(annot_in, feat_df)
  df$feature_class_primary <- annotated$feature_class_primary
  df$feature_color <- unname(
    PELSA_FEATURE_COLORS[df$feature_class_primary]
  )
  df$winning_accession <- annotated$winning_accession
  df$winning_gene <- annotated$winning_gene

  # ---- 2J marker flag over the ;-accession tokens --------------------------
  df$is_marker <- pelsa_match_markers(df$PG.ProteinAccessions, markers)

  # ---- 2C multilabel: grouped pass over the matched cache ------------------
  df[[key_col]] <- if (use_row_id) {
    stat_df[[".row_id"]]
  } else {
    df$id
  }
  labels <- .pelsa_volcano_labels(matched_cache, key_col)
  # Left-join the label by key, preserving stat_df row order. match() gives the
  # first label per key (one label per distinct key by construction).
  df$label <- labels$label[match(df[[key_col]], labels[[key_col]])]
  df[[key_col]] <- NULL

  # ---- Two-sided significance + display clamp ------------------------------
  df <- .pelsa_attach_significance(df, sig_cutoff)
  if (!is.null(logfc_cap)) {
    df$logFC <- pmax(pmin(df$logFC, logfc_cap), -logfc_cap)
  }

  y_cutoff <- .pelsa_volcano_y_cutoff(df$adj.P.Val, df$P.Value, sig_cutoff)

  df <- df[, .pelsa_volcano_out_cols(), drop = FALSE]
  rownames(df) <- NULL
  attr(df, "y_cutoff") <- y_cutoff
  df
}

# ---- best-peptide panel (one dot per distinct best-peptide via 2G) ----------

# Resolve the UNAMBIGUOUS per-dot identity columns for a best-peptide rollup
# result (H2 fix). The rollup already returns, per distinct best-peptide, its
# OWN peptide-level (adj_p, logFC) coordinate plus the `won_accessions` it won.
# A best-peptide dot's protein/gene/span/feature/marker MUST come from the
# peptide's WON accession (the rollup's winner) -- NOT from an arbitrary first
# `stat_df` row matched on the stripped sequence, which (when a stripped
# sequence is shared across protein groups, common in DIA) can disagree with
# the rollup's winner and produce a dot whose label, color, and y-height belong
# to different proteins.
#
# Representative won accession: the FIRST token of `won_accessions` (the
# rollup's deterministic sort already orders them, e.g. by accession), so the
# dot's accession/gene/span are reproducible. The raw P.Value for the y-axis is
# pulled from the exploded stat frame `m` matched on the (peptide_seq,
# accession) PAIR -- which is unique (one row per peptide x accession x
# occurrence; the leading occurrence is taken) -- NOT on the stripped sequence
# alone. This keeps logP consistent with the same won accession.
#
# @param rolled a pelsa_best_peptide_rollup() frame (peptide_seq, adj_p, logFC,
#   label, won_accessions, n_won).
# @param m      the exploded (peptide x accession x occurrence) stat frame the
#   rollup consumed, carrying PEP.StrippedSequence / accession / gene /
#   pep_start / pep_end / P.Value.
# @return data.frame, one row per rolled best-peptide, with columns
#   won_accession, won_gene, pep_start, pep_end, P.Value -- all from the SAME
#   won accession (mutually consistent).
# @noRd
.pelsa_best_back_map <- function(rolled, m) {
  n <- nrow(rolled)
  # Representative won accession = first ;-token of won_accessions.
  won_acc <- vapply(
    strsplit(as.character(rolled$won_accessions), ";", fixed = TRUE),
    function(x) if (length(x) == 0L) NA_character_ else trimws(x[[1L]]),
    character(1)
  )

  # Build a (peptide_seq, accession) -> first-occurrence lookup over m. The pair
  # is unique per occurrence; take the LEADING (smallest pep_start) occurrence so
  # a peptide mapping to the same accession at several positions is deterministic.
  mm <- data.frame(
    seq       = as.character(m[["PEP.StrippedSequence"]]),
    acc       = as.character(m[["accession"]]),
    gene      = as.character(m[["gene"]]),
    pep_start = as.integer(m[["pep_start"]]),
    pep_end   = if ("pep_end" %in% colnames(m)) as.integer(m[["pep_end"]]) else
      rep(NA_integer_, nrow(m)),
    P.Value   = if ("P.Value" %in% colnames(m)) as.numeric(m[["P.Value"]]) else
      rep(NA_real_, nrow(m)),
    stringsAsFactors = FALSE
  )
  mm <- mm[order(mm$seq, mm$acc, mm$pep_start, na.last = TRUE), , drop = FALSE]
  pair <- paste0(mm$seq, "\r", mm$acc)
  mm <- mm[!duplicated(pair), , drop = FALSE]

  lookup_key <- paste0(mm$seq, "\r", mm$acc)
  qry_key <- paste0(as.character(rolled$peptide_seq), "\r", won_acc)
  idx <- match(qry_key, lookup_key)

  data.frame(
    won_accession = won_acc,
    won_gene      = mm$gene[idx],
    pep_start     = mm$pep_start[idx],
    pep_end       = mm$pep_end[idx],
    P.Value       = mm$P.Value[idx],
    stringsAsFactors = FALSE
  )
}

# @noRd
.pelsa_build_volcano_best <- function(stat_df, matched_cache, feat_df, markers,
                                      contrast, stat_cols, sig_cutoff,
                                      logfc_cap) {
  # Build the exploded+stat frame the 2G rollup consumes: one row per
  # (peptide, accession), carrying the contrast's adj.P.Val / logFC under the
  # rollup's default names plus accession / gene / pep_start. The matched cache
  # IS already exploded (one row per peptide x accession x occurrence) and
  # carries pep_start; we attach the stat triplet by the peptide join key.
  use_row_id <- ".row_id" %in% colnames(stat_df) &&
    ".row_id" %in% colnames(matched_cache)
  key_col <- if (use_row_id) ".row_id" else "PEP.StrippedSequence"

  m <- matched_cache
  key_vals <- m[[key_col]]
  stat_key <- if (use_row_id) stat_df[[".row_id"]] else
    as.character(stat_df[["PEP.StrippedSequence"]])
  idx <- match(if (use_row_id) key_vals else as.character(key_vals), stat_key)
  m$adj.P.Val <- as.numeric(stat_df[[stat_cols$adjp]])[idx]
  m$logFC <- as.numeric(stat_df[[stat_cols$logfc]])[idx]
  m$P.Value <- as.numeric(stat_df[[stat_cols$pval]])[idx]
  if (!"PEP.StrippedSequence" %in% colnames(m)) {
    m$PEP.StrippedSequence <- as.character(stat_df[["PEP.StrippedSequence"]])[idx]
  }

  rolled <- pelsa_best_peptide_rollup(m)

  # H2 FIX: derive the dot's protein/gene/span + raw P.Value from the rollup's
  # WON accession (consistent with the label + coordinate), NOT from an arbitrary
  # first stat_df row matched on the stripped sequence (which mis-positions /
  # mis-labels a dot when a stripped sequence is shared across protein groups).
  back <- .pelsa_best_back_map(rolled, m)

  df <- data.frame(
    id        = rolled$peptide_seq,
    logFC     = rolled$logFC,    # peptide's OWN coordinate (unambiguous)
    adj.P.Val = rolled$adj_p,    # peptide's OWN coordinate (unambiguous)
    P.Value   = back$P.Value,    # raw-p of the SAME (peptide, won accession)
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  df$logP <- -log10(df$P.Value)
  # The dot's protein/gene/span are the WON accession's (single accession, not a
  # ;-list), so the label/color/coordinate all describe the same protein.
  df$PG.ProteinAccessions <- back$won_accession
  df$PG.Genes <- back$won_gene
  df$pep_start <- as.integer(back$pep_start)
  df$pep_end <- as.integer(back$pep_end)

  # 2I feature annotation on the WON accession (protein-panel shape: one
  # accession per row), so feature_color + winning_accession describe the won
  # protein -- consistent with the labeled accession.
  annot_in <- data.frame(
    accession            = back$won_accession,
    gene                 = back$won_gene,
    pep_start            = df$pep_start,
    pep_end              = df$pep_end,
    stringsAsFactors     = FALSE,
    check.names          = FALSE
  )
  annotated <- pelsa_annotate_features(annot_in, feat_df)
  df$feature_class_primary <- annotated$feature_class_primary
  df$feature_color <- unname(PELSA_FEATURE_COLORS[df$feature_class_primary])
  df$winning_accession <- annotated$winning_accession
  df$winning_gene <- annotated$winning_gene

  # 2J marker flag on the WON accession (consistent with the dot's protein).
  df$is_marker <- pelsa_match_markers(back$won_accession, markers)

  # The 2G rollup already built the ;-joined multilabel over won accessions.
  df$label <- rolled$label

  df <- .pelsa_attach_significance(df, sig_cutoff)
  if (!is.null(logfc_cap)) {
    df$logFC <- pmax(pmin(df$logFC, logfc_cap), -logfc_cap)
  }

  # y_cutoff: empirical raw-p at adj.P.Val == sig_cutoff over the best-peptide
  # dots (the dashed line is computed on what is plotted).
  y_cutoff <- .pelsa_volcano_y_cutoff(df$adj.P.Val, df$P.Value, sig_cutoff)

  df <- df[, .pelsa_volcano_out_cols(), drop = FALSE]
  rownames(df) <- NULL
  attr(df, "y_cutoff") <- y_cutoff
  df
}

# Canonical output column order (shared by both panels). Names overlapping
# build_volcano_df (id/logFC/logP/adj.P.Val/P.Value/Significant) match so
# Phase-7 plotting can reuse the tab_stat_plot machinery.
# @noRd
.pelsa_volcano_out_cols <- function() {
  c("id", "logFC", "adj.P.Val", "P.Value", "logP", "Significant",
    "sig_direction", "sig_color", "feature_class_primary", "feature_color",
    "winning_accession", "winning_gene", "label", "is_marker",
    "PG.ProteinAccessions", "PG.Genes", "pep_start", "pep_end")
}
