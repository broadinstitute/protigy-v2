################################################################################
# Module: PELSA volcano data-frame builder (Task 3A) - pure, no Shiny.
#
# Produces ONE tidy data.frame that the Phase-7 plotly volcano renders. It does
# NOT plot and does NOT compute statistics - the Statistics tab supplies
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
#   "all_peptide" (default): NO explode - exactly ONE dot per SOURCE peptide
#     row. The dot's color resolves across all ;-accession tokens (feature
#     annotation runs on the ;-frame), and its label is the multilabel across
#     all of the peptide's (gene, pep_start, accession) mappings from the
#     matched cache.
#   "best_peptide": one dot per distinct best-peptide via the 2G rollup over the
#     exploded+stat frame, then the same sig/feature/marker columns attached.
#
# Two-sided significance coloring (Decision #4): a significant peptide is colored
# whether it goes UP (darkred) or DOWN (a blue, #1f4e9c) - down is NOT a filter.
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
# column vectors, then collapses per peptide with a data.table group paste -
# the pelsa_build_multilabel R closure is NOT invoked per peptide (that 80k-call
# group-op was the profiled hot spot; the inline form is byte-identical and ~10x
# faster). annotation/marker matching are already vectorized on the ;-frame.
# pelsa_build_multilabel remains the canonical single-label builder for small
# paths (e.g. the 2G best-peptide rollup).
#
# This file also carries the Pass-2 (7D-7F) contrast-key / label-mode /
# color-mode selection helpers that feed the builder above. The plot-assembly,
# intensity-line/export, and selection/interaction concerns that used to live
# in this same file have been split out (this file had grown past the repo's
# 800-line cap) into the sibling files:
#   R/tab_pelsa_volcano_plot_helpers.R        - native plotly build + overlays
#   R/tab_pelsa_intensity_helpers.R           - intensity-line + ggplot export
#   R/tab_pelsa_volcano_interaction_helpers.R - click/selection/highlight logic
################################################################################

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
# NOTE: this all_peptide label spans ALL of a key's accession mappings; the
# best_peptide panel (pelsa_best_peptide_rollup) labels only the WON accessions
# (a subset) but in the SAME (pep_start, accession) order, so the entries that
# appear in both panels share an identical ordering.
#
# @param matched   the 2B $matched cache (peptide x accession x occurrence)
# @param key_col   the join key column name present in matched ("..key")
# @return data.frame(key, label) one row per distinct key
# @noRd
.pelsa_volcano_labels <- function(matched, key_col, is_self_curated = FALSE) {
  dt <- data.table::data.table(
    .key         = matched[[key_col]],
    gene         = as.character(matched[["gene"]]),
    protein_name = if ("protein_name" %in% colnames(matched))
      as.character(matched[["protein_name"]]) else
      rep(NA_character_, nrow(matched)),
    pep_start    = matched[["pep_start"]],
    accession    = as.character(matched[["accession"]])
  )
  # Deterministic within-group order: by pep_start then accession. setorder is a
  # stable sort, so equal-start entries keep input order.
  data.table::setorder(dt, .key, pep_start, accession, na.last = TRUE)

  # Per-mapping stem, VECTORIZED over the whole column (no per-peptide call):
  # gene -> protein_name -> accession, byte-identical to
  # pelsa_build_multilabel()'s stem via the shared resolver. Self-curated species
  # have no UniProt gene/name: force the accession label.
  lid <- pelsa_resolve_label_stem(dt$gene, dt$protein_name, dt$accession,
                                  is_self_curated)
  dt[, entry := paste0(lid, "_aa", pep_start)]

  # Collapse per peptide: unique() preserves first-occurrence (sorted) order,
  # matching pelsa_build_multilabel()'s !duplicated() dedup before the ;-join.
  lab <- dt[, list(label = paste(unique(entry), collapse = ";")), by = ".key"]
  out <- as.data.frame(lab, stringsAsFactors = FALSE)
  names(out)[names(out) == ".key"] <- key_col
  out
}

# Repoint each row's winning_accession to a MATCHED accession when the
# annotation-derived winner is absent from that peptide's matched-cache rows.
#
# The annotation winner is consistent with the matched cache for UniProt (the
# feature overlap can only fire on a FASTA-mapped accession) and for self-curated
# rows whose leading PG.ProteinAccessions token mapped. The ONLY rows that need
# repair are self-curated peptides whose leading token did not map but a
# secondary token did: their winner points at the unmapped leading token while
# the matched cache holds only the secondary one. We replace such winners with
# the peptide's REPRESENTATIVE matched accession -- the first matched accession by
# (pep_start, accession), the same order .pelsa_volcano_labels() uses, so the
# pinned label's leading entry and the resolved accession agree.
#
# @param winner   character vector of current winning_accession (length nrow df).
# @param keys     the per-row join key values (df[[key_col]]).
# @param matched  the matched cache (2B $matched).
# @param key_col  the join key column present on BOTH df and matched.
# @return character vector (length(winner)) with non-matched winners repaired;
#   rows with no matched entry for their key are left unchanged.
# @noRd
.pelsa_volcano_reconcile_winner <- function(winner, keys, matched, key_col) {
  if (length(winner) == 0L) return(winner)
  mk <- as.character(matched[[key_col]])
  ma <- as.character(matched[["accession"]])
  mps <- matched[["pep_start"]]

  # Sort matched rows by (key, pep_start, accession) so the FIRST row per key is
  # that key's REPRESENTATIVE matched accession -- the same order
  # .pelsa_volcano_labels() uses, so the repointed winner equals the pinned
  # label's leading entry.
  ord <- order(mk, mps, ma, na.last = TRUE)
  mk <- mk[ord]; ma <- ma[ord]
  first <- !duplicated(mk)
  rep_acc <- ma[first]
  names(rep_acc) <- mk[first]

  # FULLY VECTORIZED (this builder runs on ~80k-row frames; a per-row loop with
  # name-based list lookup is O(n^2) and was the profiled hot spot this file was
  # rewritten to avoid -- see header). Membership is a paired-string set test:
  # a winner is OK iff (key, winner) is one of the cache's (key, accession)
  # pairs. The "\r" separator is collision-safe (accessions/keys never contain
  # it) and mirrors .pelsa_best_back_map()'s pair-key idiom.
  k <- as.character(keys)
  have_pair <- paste0(mk, "\r", ma)
  want_pair <- paste0(k, "\r", winner)
  is_member <- !is.na(winner) & nzchar(winner) & want_pair %in% have_pair
  rep_for_k <- rep_acc[k]                   # NA when the key has no matched rows
  needs_fix <- !is_member & !is.na(rep_for_k)

  out <- winner
  out[needs_fix] <- rep_for_k[needs_fix]
  out
}

# Re-derive each peptide's tooltip span (pep_start/pep_end) from the WINNING
# accession's matched-cache occurrence, so the volcano tooltip + pinned metadata
# table report the SAME coordinate the pinned intensity/Woods panels use.
#
# The span on stat_df is a REPRESENTATIVE one: pelsa_volcano_stat_df picks the
# leading (smallest pep_start) occurrence per peptide across ALL of its accession
# mappings, keyed by stripped sequence alone. When a peptide's winning accession
# is NOT the one with the smallest residue position (e.g. AEIITVSDGR maps to
# A2A4J8@170, A8XY17@114, Q9CQ80@162 and Q9CQ80 wins), that representative span
# points at the wrong protein's coordinate. The intensity + Woods panels look the
# peptide up by (key, winning_accession) in the matched cache, so they show the
# winning accession's span; this aligns the volcano-side span to match.
#
# Vectorized pair-key lookup (key, winning_accession) -> the matched occurrence,
# taking the LEADING (smallest pep_start) occurrence when a peptide hits the same
# accession at several positions -- the same tie-break the intensity panel's
# .pelsa_best_back_map / representative-span logic uses. Peptides whose winning
# accession has no matched row (should not happen after winner reconciliation)
# keep their existing span.
#
# @param keys     the per-row join key values (df[[key_col]]).
# @param winner   character vector of winning_accession (length nrow df).
# @param matched  the matched cache (2B $matched), carrying key_col + accession +
#   pep_start (+ pep_end).
# @param key_col  the join key column present on BOTH df and matched.
# @return list(pep_start, pep_end), each length(winner); NA where no match.
# @noRd
.pelsa_volcano_winner_span <- function(keys, winner, matched, key_col) {
  n <- length(winner)
  na_span <- list(pep_start = rep(NA_integer_, n), pep_end = rep(NA_integer_, n))
  if (n == 0L) return(na_span)

  mk  <- as.character(matched[[key_col]])
  ma  <- as.character(matched[["accession"]])
  mps <- as.integer(matched[["pep_start"]])
  mpe <- if ("pep_end" %in% colnames(matched)) {
    as.integer(matched[["pep_end"]])
  } else {
    rep(NA_integer_, length(mk))
  }

  # Leading (smallest pep_start) occurrence per (key, accession) pair: order by
  # (key, accession, pep_start) then keep the first row of each pair.
  ord <- order(mk, ma, mps, na.last = TRUE)
  mk <- mk[ord]; ma <- ma[ord]; mps <- mps[ord]; mpe <- mpe[ord]
  pair <- paste0(mk, "\r", ma)
  first <- !duplicated(pair)
  lookup_key <- pair[first]
  lk_start <- mps[first]
  lk_end   <- mpe[first]

  # Build the query pair-key. Force NA/blank winners to a sentinel that cannot
  # match any real pair: an empty winner would otherwise paste to "<key>\r" and
  # collide with a matched row carrying an empty accession. NA propagates through
  # match() to NA (-> representative span retained), so map blank -> NA here too.
  w <- as.character(winner)
  w[is.na(w) | !nzchar(w)] <- NA_character_
  qry <- ifelse(is.na(w), NA_character_, paste0(as.character(keys), "\r", w))
  idx <- match(qry, lookup_key)
  list(pep_start = lk_start[idx], pep_end = lk_end[idx])
}

# Attach the two-sided significance columns (Significant / sig_direction /
# sig_color) to a frame already carrying logFC / adj.P.Val / logP. Vectorized.
#
# @param y_cutoff  the empirical raw-p threshold (-log10 scale) from
#   .pelsa_volcano_y_cutoff(), used only in "adj.p.val" mode so the boundary
#   peptide is classified identically to the Statistics tab's
#   `df$logP > y_cutoff` rule (a strict adj.P.Val < sig_cutoff comparison can
#   disagree with that empirical, ties-inclusive threshold at the boundary).
# @noRd
.pelsa_attach_significance <- function(df, sig_cutoff, sig_stat = "adj.p.val",
                                       y_cutoff = NULL) {
  # Honor the user-selected significance statistic (shared with the Statistics
  # tab): "nom.p.val" classifies on the raw P.Value directly; "adj.p.val"
  # mirrors tab_stat_plot_helpers.R's `df$logP > y_cutoff` empirical rule so
  # both tabs agree at the boundary peptide.
  sig <- if (identical(sig_stat, "nom.p.val")) {
    !is.na(df$P.Value) & df$P.Value < sig_cutoff
  } else {
    !is.na(df$logP) & df$logP > y_cutoff
  }
  # Use >= 0 for "up" so a significant peptide with logFC exactly 0 still gets
  # bucketed/colored instead of falling through to "ns"/gray.
  up <- sig & !is.na(df$logFC) & df$logFC >= 0
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
.pelsa_volcano_y_cutoff <- function(adjp, pval, sig_cutoff,
                                    sig_stat = "adj.p.val") {
  # For "nom.p.val" the dashed line is simply -log10(cutoff) on the raw-p axis
  # (mirrors tab_stat_plot_helpers.R). For "adj.p.val" it is the empirical raw-p
  # of the largest P.Value among peptides passing the adj.P.Val filter.
  if (identical(sig_stat, "nom.p.val")) return(-log10(sig_cutoff))
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
                                   contrast, opts = list(),
                                   is_self_curated = FALSE) {
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

  sig_cutoff <- opts$sig_cutoff %||% .PELSA_EXPORT_SIG_CUTOFF
  sig_stat <- opts$sig_stat %||% "adj.p.val"
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
                                     contrast, stat_cols, sig_cutoff, sig_stat,
                                     logfc_cap, is_self_curated))
  }
  .pelsa_build_volcano_all(stat_df, matched_cache, feat_df, markers,
                           stat_cols, sig_cutoff, sig_stat, logfc_cap,
                           is_self_curated)
}

# ---- all-peptide panel (one dot per source peptide, no explode) -------------

# @noRd
.pelsa_build_volcano_all <- function(stat_df, matched_cache, feat_df, markers,
                                     stat_cols, sig_cutoff,
                                     sig_stat = "adj.p.val", logfc_cap,
                                     is_self_curated = FALSE) {
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
  # Self-curated species have no UniProt gene: blank the winning gene so the
  # fixed tooltip's gene field renders empty (consistent with the forced
  # accession label below).
  df$winning_gene <- if (isTRUE(is_self_curated)) {
    rep("", nrow(df))
  } else {
    annotated$winning_gene
  }
  # winning_protein_name: the matched cache's protein_name for this row's
  # winning accession (representative first occurrence). NA when the cache lacks
  # the column or the accession has no matched row. Self-curated blanks it.
  if (isTRUE(is_self_curated) || !"protein_name" %in% colnames(matched_cache)) {
    df$winning_protein_name <- rep(if (isTRUE(is_self_curated)) "" else
      NA_character_, nrow(df))
  } else {
    macc <- as.character(matched_cache[["accession"]])
    mname <- as.character(matched_cache[["protein_name"]])
    first <- !duplicated(macc)
    name_by_acc <- stats::setNames(mname[first], macc[first])
    df$winning_protein_name <- unname(name_by_acc[as.character(df$winning_accession)])
  }

  # ---- 2J marker flag over the ;-accession tokens --------------------------
  df$is_marker <- pelsa_match_markers(df$PG.ProteinAccessions, markers)

  # ---- 2C multilabel: grouped pass over the matched cache ------------------
  df[[key_col]] <- if (use_row_id) {
    stat_df[[".row_id"]]
  } else {
    df$id
  }
  labels <- .pelsa_volcano_labels(matched_cache, key_col, is_self_curated)
  # Left-join the label by key, preserving stat_df row order. match() gives the
  # first label per key (one label per distinct key by construction).
  df$label <- labels$label[match(df[[key_col]], labels[[key_col]])]

  # ---- winning_accession reconciliation (pin/intensity consistency) --------
  # pelsa_annotate_features set winning_accession from the FEATURE overlap winner
  # (UniProt) or, with no overlap (always, for a self-curated species), the
  # LEADING PG.ProteinAccessions token. That leading token may not FASTA-map when
  # a SECONDARY token is the one that maps -- the peptide then lives in the
  # matched cache under the secondary accession only. The pinned intensity/Woods
  # panels look winning_accession up in that cache, so a non-matched winner makes
  # them silently blank. Repoint any winner ABSENT from this peptide's matched
  # accessions to a representative MATCHED one (first by pep_start, accession --
  # the same order .pelsa_volcano_labels uses). Rows whose winner already maps
  # (every UniProt row, and self-curated rows whose leading token mapped) are
  # untouched. We do NOT also repoint winning_gene / feature_class_primary: only
  # self-curated rows are ever repointed in practice (UniProt winners always map),
  # and for those winning_gene is already blanked + the label is accession-based
  # across all tokens, so the repointed accession cannot disagree with them.
  df$winning_accession <- .pelsa_volcano_reconcile_winner(
    df$winning_accession, df[[key_col]], matched_cache, key_col)

  # ---- span follows the winning accession (tooltip/metadata consistency) ----
  # The span carried on stat_df is the representative (smallest pep_start across
  # ALL of the peptide's accession mappings) one. Re-derive pep_start/pep_end
  # from the WINNING accession's matched occurrence so the volcano tooltip + the
  # pinned metadata table report the same coordinate as the pinned intensity +
  # Woods panels (which key on winning_accession). Peptides whose winner has no
  # matched row keep the representative span (no worse than before).
  win_span <- .pelsa_volcano_winner_span(
    df[[key_col]], df$winning_accession, matched_cache, key_col)
  has_win_span <- !is.na(win_span$pep_start)
  df$pep_start[has_win_span] <- win_span$pep_start[has_win_span]
  df$pep_end[has_win_span] <- win_span$pep_end[has_win_span]

  df[[key_col]] <- NULL

  # ---- Two-sided significance + display clamp ------------------------------
  # y_cutoff must be computed BEFORE attach_significance: in "adj.p.val" mode
  # the two-sided sig flag is derived from this same empirical threshold (see
  # .pelsa_attach_significance) so it agrees with the Statistics tab at the
  # boundary peptide.
  y_cutoff <- .pelsa_volcano_y_cutoff(df$adj.P.Val, df$P.Value, sig_cutoff,
                                      sig_stat)
  df <- .pelsa_attach_significance(df, sig_cutoff, sig_stat, y_cutoff)
  if (!is.null(logfc_cap)) {
    df$logFC <- pmax(pmin(df$logFC, logfc_cap), -logfc_cap)
  }

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
    seq          = as.character(m[["PEP.StrippedSequence"]]),
    acc          = as.character(m[["accession"]]),
    gene         = as.character(m[["gene"]]),
    protein_name = if ("protein_name" %in% colnames(m))
      as.character(m[["protein_name"]]) else rep(NA_character_, nrow(m)),
    pep_start    = as.integer(m[["pep_start"]]),
    pep_end      = if ("pep_end" %in% colnames(m)) as.integer(m[["pep_end"]]) else
      rep(NA_integer_, nrow(m)),
    P.Value      = if ("P.Value" %in% colnames(m)) as.numeric(m[["P.Value"]]) else
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
    won_accession    = won_acc,
    won_gene         = mm$gene[idx],
    won_protein_name = mm$protein_name[idx],
    pep_start        = mm$pep_start[idx],
    pep_end          = mm$pep_end[idx],
    P.Value          = mm$P.Value[idx],
    stringsAsFactors = FALSE
  )
}

# @noRd
.pelsa_build_volcano_best <- function(stat_df, matched_cache, feat_df, markers,
                                      contrast, stat_cols, sig_cutoff,
                                      sig_stat = "adj.p.val",
                                      logfc_cap, is_self_curated = FALSE) {
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

  rolled <- pelsa_best_peptide_rollup(m, is_self_curated = is_self_curated)

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
  # Self-curated: blank the gene so the tooltip's gene field renders empty.
  df$winning_gene <- if (isTRUE(is_self_curated)) {
    rep("", nrow(df))
  } else {
    annotated$winning_gene
  }
  # Self-curated: no protein name either, blank it so the tip/pin fall to acc.
  df$winning_protein_name <- if (isTRUE(is_self_curated)) {
    rep("", nrow(df))
  } else {
    back$won_protein_name
  }

  # 2J marker flag on the WON accession (consistent with the dot's protein).
  df$is_marker <- pelsa_match_markers(back$won_accession, markers)

  # The 2G rollup already built the ;-joined multilabel over won accessions.
  df$label <- rolled$label

  # y_cutoff: empirical raw-p at adj.P.Val == sig_cutoff over the best-peptide
  # dots (the dashed line is computed on what is plotted); -log10(cutoff) when
  # the user selected the nominal-p statistic. Computed BEFORE
  # attach_significance since the "adj.p.val" sig flag is derived from this
  # same threshold (see .pelsa_attach_significance) so it agrees with the
  # Statistics tab at the boundary peptide.
  y_cutoff <- .pelsa_volcano_y_cutoff(df$adj.P.Val, df$P.Value, sig_cutoff,
                                      sig_stat)
  df <- .pelsa_attach_significance(df, sig_cutoff, sig_stat, y_cutoff)
  if (!is.null(logfc_cap)) {
    df$logFC <- pmax(pmin(df$logFC, logfc_cap), -logfc_cap)
  }

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
    "winning_accession", "winning_gene", "winning_protein_name", "label",
    "is_marker", "PG.ProteinAccessions", "PG.Genes", "pep_start", "pep_end")
}
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
#   "top_n_markers"      the n_top_markers smallest adj.P.Val MARKER peptides
#                        (is_marker == TRUE) in the "down" logFC-sign bucket
#                        (logFC < 0), plus ceiling(n_top_markers / 2) smallest
#                        adj.P.Val MARKER peptides in the "up" bucket
#                        (logFC >= 0); union of both buckets. Ranks ALL marker
#                        peptides regardless of significance. Same adj.P.Val
#                        tiebreak as "top_n_adjp" (raw P.Value, then |logFC|).
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
# @param n_top_markers      N for the "down" bucket of "top_n_markers"; the
#                           "up" bucket keeps ceiling(N / 2) (default 3,
#                           coerced to >= 1).
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
      m_dir <- ifelse(is.na(logfc[marker_idx]), "ns",
                      ifelse(logfc[marker_idx] < 0, "down", "up"))
      n_down_mk <- max(1L, as.integer(n_top_markers)[1L])
      n_up_mk   <- ceiling(n_down_mk / 2)
      idx <- c(idx, .pelsa_top_n_by_direction(marker_idx, m_dir,
                                              adjp[marker_idx],
                                              n_top_down = n_down_mk,
                                              n_top_up = n_up_mk,
                                              tiebreak_value = tb[marker_idx]))
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

