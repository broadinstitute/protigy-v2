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
  # Self-curated species have no UniProt genes: force the accession label even
  # when the input report carried a gene token.
  lid <- if (isTRUE(is_self_curated)) {
    dt$accession
  } else {
    data.table::fifelse(
      is.na(dt$gene) | !nzchar(trimws(dt$gene)), dt$accession, dt$gene
    )
  }
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
                                     logfc_cap, is_self_curated))
  }
  .pelsa_build_volcano_all(stat_df, matched_cache, feat_df, markers,
                           stat_cols, sig_cutoff, logfc_cap, is_self_curated)
}

# ---- all-peptide panel (one dot per source peptide, no explode) -------------

# @noRd
.pelsa_build_volcano_all <- function(stat_df, matched_cache, feat_df, markers,
                                     stat_cols, sig_cutoff, logfc_cap,
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
  # as.list() forces x/y/text to serialize as JSON ARRAYS even for a SINGLE
  # highlighted point (one peptide with no siblings). plotlyProxyInvoke goes
  # through jsonlite, which collapses a length-1 vector to a scalar; a scattergl
  # trace then can't index it and the gold dot vanishes. (Same guard as
  # pelsa_volcano_clicked_point_trace.)
  list(
    type = "scattergl", mode = "markers",
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
pelsa_volcano_clicked_point_trace <- function(df, selection = NULL) {
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
    type = "scattergl", mode = "markers",
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
    # NO ggtitle here: plotly::subplot collapses a per-plot ggtitle into a SINGLE
    # overall layout$title (it keeps only the LAST plot's title, so the top
    # panel's title is silently dropped and the bottom panel's renders as one
    # centered overall title). We add the per-panel titles as paper-referenced
    # subplot annotations below instead.
    gg  <- pelsa_intensity_line_ggplot(sub, pinned_label = pinned_label) +
      ggplot2::labs(y = NULL)             # one shared y-title added below
    # Only the bottom panel keeps the x tick labels (shared axis).
    # Force the build inside suppressWarnings so the deferred ggplot build
    # (which warns "Ignoring unknown aesthetics: text" for the tooltip aes)
    # is muffled here rather than later in renderPlotly's print path.
    suppressWarnings(plotly::plotly_build(plotly::ggplotly(gg, tooltip = "text")))
  })
  # titleY = FALSE so plotly does NOT render the per-panel y-axis titles (they
  # were stripped via labs(y = NULL) but titleY = TRUE would re-add them and they
  # overlap). We add exactly ONE shared, vertically-centered y-title annotation.
  margin <- 0.06
  p <- plotly::subplot(parts, nrows = length(parts), shareX = TRUE,
                       titleY = FALSE, margin = margin)

  # Per-panel TITLE at the TOP of each panel (paper coords). subplot stacks the
  # panels top-to-bottom with `margin` between them; panel i (1-based from the
  # top) spans [top_i - h, top_i] where h is the per-panel height. The title sits
  # just above each panel's top edge. Full, unambiguous wording (the short
  # "Significant"/"Non-significant" was ambiguous about WHAT contrast).
  n_panel <- length(parts)
  h <- (1 - (n_panel - 1) * margin) / n_panel
  title_for <- function(pn) {
    if (identical(pn, "Significant")) "Significant in selected contrast"
    else if (identical(pn, "Non-significant")) "Non-significant in selected contrast"
    else pn
  }
  # The top panel's top edge is at 1, so its title (yanchor = "bottom") sits
  # flush against the panel. Adding a +0.02 offset to lower panels pushed their
  # titles farther from the panel than the top one, so the gap looked uneven.
  # Anchor every title at its own panel's top edge for matching spacing. Bold
  # via <b></b> (plotly annotation font has no `face`; text supports HTML).
  panel_titles <- lapply(seq_len(n_panel), function(i) {
    top_i <- 1 - (i - 1) * (h + margin)
    list(
      text = paste0("<b>", title_for(panels[i]), "</b>"),
      x = 0.5, y = min(top_i, 1),
      xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom",
      showarrow = FALSE, font = list(size = 13, color = "rgba(0,0,0,1)"))
  })
  y_title <- list(
    text = "mean log2 intensity", x = -0.12, y = 0.5,
    xref = "paper", yref = "paper", textangle = -90,
    showarrow = FALSE, font = list(size = 12))

  p <- plotly::layout(
    p,
    title = list(text = ""),  # no overall title (per-panel titles cover this)
    showlegend = FALSE,       # tooltip identifies each line; no legend needed
    margin = list(l = 70, t = 40),  # room for the y-title + the top panel title
    annotations = c(list(y_title), panel_titles))
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
      panel.grid.minor = ggplot2::element_blank(),
      # Reserve a wider left gutter so a long rotated leftmost condition label
      # (e.g. "AY9944_U18666A_DMSO") is not clipped off the panel edge.
      plot.margin = ggplot2::margin(t = 5.5, r = 5.5, b = 5.5, l = 40))
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
# @param sig_cutoff significance threshold on adj.P.Val. Defaults to the export
#   constant; callers in the module thread the SHARED cutoff (isolate(sig_cutoff_r()))
#   so this export matches the on-screen volcano/intensity views rather than a
#   hardcoded 0.05.
# @return tidy long data.frame (rbind of pelsa_intensity_line_data over the
#   pelsa_intensity_proteins set), or NULL.
# @noRd
pelsa_plotted_intensities_df <- function(stat_raw, matched, markers, contrast,
                                         pm, cmap, corder,
                                         sig_cutoff = .PELSA_EXPORT_SIG_CUTOFF) {
  if (!is.data.frame(stat_raw) || nrow(stat_raw) == 0L) return(NULL)
  if (!is.data.frame(matched) || nrow(matched) == 0L) return(NULL)
  if (is.null(contrast) || is.null(pm) || is.null(cmap) ||
      length(corder) == 0L) {
    return(NULL)
  }
  stat_df <- pelsa_volcano_stat_df(stat_raw, matched)
  prot <- pelsa_intensity_proteins(stat_df, matched, markers, contrast,
                                   sig_cutoff = sig_cutoff)
  if (nrow(prot) == 0L) return(NULL)
  rows <- lapply(seq_len(nrow(prot)), function(i) {
    tryCatch(
      pelsa_intensity_line_data(
        accession = prot$accession[i], stat_df = stat_df,
        matched_cache = matched, processed_mat = pm,
        condition_map = cmap, condition_order = corder,
        contrast = contrast, sig_cutoff = sig_cutoff,
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
      shape = 21, size = 1, stroke = 0.5, color = .PELSA_VOLCANO_MARKER_EDGE)
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
################################################################################
# Module: PELSA Section 3 - volcano SELECTION/INTERACTION pure helpers.
#
# The single-selection model's pure logic: resolve a click to a peptide, compute
# the gold recolor arrays for the proxy restyle, the Find-accession match mask,
# and the pinned-panel metadata rows. No Shiny; unit-tested against the seeded
# synthetic generator's closed-form ground truth.
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
# accession) and the REST. On selection, the main volcano is NOT rebuilt; instead
# the highlight is applied client-side via a plotlyProxy restyle (single
# mechanism) that sets per-point fill/ring arrays on the background + marker
# traces (see pelsa_volcano_recolor). This mask identifies a protein's peptides
# for callers that need the membership test.
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

# Logical mask (over df rows) of the points to gold-highlight: the selected
# peptide, every same-protein peptide (winning_accession == selection$accession),
# and every find-matched peptide. ALL highlighted points are styled identically
# (gold fill + black outline) - there is no selected-vs-sibling visual split.
#
# selection: NULL, or list(accession, peptide_seq, ...).
# find_mask: NULL, or a logical over df rows (the typed-accession match set).
# @return a logical vector length nrow(df). @noRd
pelsa_volcano_highlight_mask <- function(df, selection = NULL, find_mask = NULL) {
  n <- if (is.data.frame(df)) nrow(df) else 0L
  mask <- rep(FALSE, n)
  if (n == 0L) return(mask)
  if (!is.null(selection)) {
    acc <- selection$accession
    seq <- selection$peptide_seq
    wacc <- as.character(df$winning_accession)
    if (!is.null(acc) && !is.na(acc) && nzchar(acc)) {
      mask <- mask | (!is.na(wacc) & wacc == acc)
    }
    if (!is.null(seq) && !is.na(seq) && nzchar(seq)) {
      mask <- mask | (as.character(df$id) == seq)
    }
  }
  if (!is.null(find_mask)) {
    fm <- as.logical(find_mask); fm[is.na(fm)] <- FALSE
    if (length(fm) == n) mask <- mask | fm
  }
  mask
}

# Compute the per-trace recolor arrays for the volcano proxy restyle under the
# single-selection model. Returns fills + ring color/width for BOTH restyled
# traces (background == pelsa_volcano_marker_split(df)$background row order,
# markers == $markers row order).
#
# selection: NULL, or list(origin="click"|"find", accession, peptide_seq).
# find_mask: NULL, or a logical over df rows (the MULTI-accession find highlight;
#            uniform gold fill, no dark ring). Ignored when selection is non-NULL.
# color_mode: "significance" | "feature" -> the BASE fill column.
# @return list(background=list(color,line.color,line.width),
#              markers=list(color,line.color,line.width)). @noRd
pelsa_volcano_recolor <- function(df, selection, find_mask = NULL,
                                  color_mode = "significance") {
  split <- pelsa_volcano_marker_split(df)
  # Partition find_mask by the SAME is_marker split (NOT a positional match on
  # df$id, which mis-assigns when a stripped sequence repeats across protein
  # groups). pelsa_volcano_marker_split puts !is_marker rows (in order) in
  # background and is_marker rows (in order) in markers; slice find_mask the same
  # way so each sub-frame's find slice is exactly row-aligned.
  fm <- if (is.null(find_mask) || !is.null(selection)) NULL else {
    im <- df$is_marker; im[is.na(im)] <- FALSE
    list(background = find_mask[!im], markers = find_mask[im])
  }
  mk_one <- function(sub, fm_sub = NULL) {
    n <- nrow(sub)
    base <- if (identical(color_mode, "feature")) {
      as.character(sub$feature_color)
    } else {
      as.character(sub$sig_color)
    }
    color <- base
    line.color <- rep("rgba(0,0,0,0)", n)
    line.width <- rep(0, n)
    if (n == 0L) return(list(color = color, line.color = line.color,
                             line.width = line.width))
    ids <- as.character(sub$id)
    wacc <- as.character(sub$winning_accession)

    sel_seq <- if (!is.null(selection)) selection$peptide_seq else NA_character_
    sel_acc <- if (!is.null(selection)) selection$accession   else NA_character_

    if (!is.na(sel_acc) && nzchar(sel_acc)) {
      sib <- !is.na(wacc) & wacc == sel_acc & (is.na(sel_seq) | ids != sel_seq)
      line.color[sib] <- .PELSA_GOLD
      line.width[sib] <- .PELSA_GOLD_RING_W
    }
    if (!is.na(sel_seq) && nzchar(sel_seq)) {
      hit <- ids == sel_seq
      color[hit] <- .PELSA_GOLD
      line.color[hit] <- .PELSA_SEL_DARK_RING
      line.width[hit] <- .PELSA_SEL_DARK_RING_W
    }
    if (!is.null(fm_sub)) {
      fm_sub[is.na(fm_sub)] <- FALSE
      color[fm_sub] <- .PELSA_GOLD
    }
    list(color = color, line.color = line.color, line.width = line.width)
  }
  list(background = mk_one(split$background,
                           if (is.null(fm)) NULL else fm$background),
       markers     = mk_one(split$markers,
                           if (is.null(fm)) NULL else fm$markers))
}

# Resolve the background / marker trace JS indices (0-based) of a built volcano
# plotly by the `meta` tag the build stamps (pelsa_volcano_build_plot). Returns
# list(background=<int|NA>, markers=<int|NA>). @noRd
.pelsa_volcano_trace_index <- function(p) {
  metas <- vapply(p$x$data, function(tr) {
    m <- tr$meta
    if (is.null(m) || length(m) != 1L) NA_character_ else as.character(m)
  }, character(1))
  bg <- which(metas == "pelsa_bg")
  mk <- which(metas == "pelsa_mk")
  list(background = if (length(bg)) bg[1L] - 1L else NA_integer_,
       markers    = if (length(mk)) mk[1L] - 1L else NA_integer_)
}

# Strip a trailing UniProt isoform suffix ("-2") to the base accession. @noRd
.pelsa_iso_base <- function(x) sub("-[0-9]+$", "", as.character(x))

# Match a typed accession against the volcano df. A peptide matches when its
# winning_accession OR any PG.ProteinAccessions token equals the input, OR shares
# its isoform base. Case-insensitive, trimmed.
# @return list(mask=<logical over df rows>, accessions=<distinct matched
#   winning_accession>, count=<# matched rows>). @noRd
pelsa_volcano_find_mask <- function(df, accession) {
  n <- if (is.data.frame(df)) nrow(df) else 0L
  empty <- list(mask = rep(FALSE, n), accessions = character(0), count = 0L)
  if (n == 0L) return(empty)
  q <- toupper(trimws(as.character(accession)[1L] %||% ""))
  if (is.na(q) || !nzchar(q)) return(empty)
  qbase <- .pelsa_iso_base(q)

  wacc <- toupper(as.character(df$winning_accession %||% rep(NA, n)))
  wbase <- .pelsa_iso_base(wacc)
  pg <- toupper(as.character(df$PG.ProteinAccessions %||% rep(NA, n)))

  hit <- (!is.na(wacc) & (wacc == q | wbase == qbase))
  pg_hit <- vapply(seq_len(n), function(i) {
    if (is.na(pg[i]) || !nzchar(pg[i])) return(FALSE)
    toks <- trimws(strsplit(pg[i], ";", fixed = TRUE)[[1]])
    any(toks == q | .pelsa_iso_base(toks) == qbase)
  }, logical(1))
  mask <- hit | pg_hit
  mask[is.na(mask)] <- FALSE
  accs <- unique(as.character(df$winning_accession)[mask])
  list(mask = mask, accessions = accs[!is.na(accs) & nzchar(accs)],
       count = sum(mask))
}

# Build the pinned-panel metadata as a 2-column (label, value) data.frame from a
# volcano-df row. The Peptide label is the winning-accession label
# "<winning_gene>_aa<pep_start>" (gene->accession fallback when gene is empty).
# n_peptides is the count the caller computed (distinct peptides PLOTTED for this
# accession in the active contrast). coverage_frac is the parent accession's
# fractional sequence coverage in [0,1] (covered residues / FASTA length), or NA
# when the protein length is unresolved - rendered as "Sequence coverage" right
# under Accession. @noRd
pelsa_pin_metadata_rows <- function(volcano_df, row, n_peptides,
                                    coverage_frac = NA_real_) {
  r <- volcano_df[row, , drop = FALSE]
  acc_fb <- if (!is.na(r$winning_accession) && nzchar(r$winning_accession))
    r$winning_accession else as.character(r$PG.ProteinAccessions)[1L]
  gene <- if (!is.na(r$winning_gene) && nzchar(r$winning_gene))
    r$winning_gene else as.character(r$PG.Genes)[1L]
  gene_disp <- if (is.na(gene) || !nzchar(gene)) "NA" else gene
  label_stem <- if (gene_disp == "NA") acc_fb else gene_disp
  pep_label <- paste0(label_stem, "_aa", r$pep_start)
  cov_disp <- if (length(coverage_frac) != 1L || is.na(coverage_frac))
    "NA" else sprintf("%.1f%%", 100 * coverage_frac)
  data.frame(
    label = c("Peptide", "Accession", "Sequence coverage", "Gene",
              "Quantified peptides (this contrast)", "Sequence", "Position",
              "adj.P", "logFC"),
    value = c(pep_label, acc_fb, cov_disp, gene_disp,
              as.character(as.integer(n_peptides)),
              as.character(r$id),
              paste0(r$pep_start, "-", r$pep_end),
              sprintf("%.2g", r$adj.P.Val), sprintf("%.2g", r$logFC)),
    stringsAsFactors = FALSE
  )
}
