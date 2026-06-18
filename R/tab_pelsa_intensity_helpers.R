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
# with CV (tab_pelsa_cv_helpers.R), which uses RAW sum-normalized intensities -
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

# Resolve the contrast's adj.P.Val column name (adj.P.Val.<contrast>) and verify
# it exists. Fails fast with a loud, column-naming error.
# @noRd
.pelsa_intensity_adjp_col <- function(stat_df, contrast) {
  if (length(contrast) != 1L || is.na(contrast) || !nzchar(contrast)) {
    stop("PELSA intensity: contrast must be a single non-empty string",
         call. = FALSE)
  }
  col <- paste0("adj.P.Val.", contrast)
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
# @param sig_cutoff     significance threshold on adj.P.Val (default 0.05).
# @return data.frame(accession, is_marker), zero-row when the union is empty.
# @noRd
pelsa_intensity_proteins <- function(stat_df, matched_cache, markers,
                                     contrast, sig_cutoff = 0.05) {
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
  adjp_col <- .pelsa_intensity_adjp_col(stat_df, contrast)

  # ---- Join the peptide's adj.P.Val onto each matched row (vectorized) -----
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
# @param sig_cutoff       significance threshold on adj.P.Val (default 0.05).
# @param is_marker        TRUE -> include BOTH significant + non-significant
#   occurrences (panel-tagged); FALSE -> only significant occurrences.
# @return tidy long data.frame, one row per (occurrence, condition-with-samples),
#   columns: accession, peptide_seq, pep_start, pep_end, pep_occurrence_idx, aa_label,
#   panel ("Significant"/"Non-significant"), condition (factor = condition_order),
#   mean_log2, n_rep_nonNA.
# @noRd
pelsa_intensity_line_data <- function(accession, stat_df, matched_cache,
                                      processed_mat, condition_map,
                                      condition_order, contrast,
                                      sig_cutoff = 0.05, is_marker = FALSE,
                                      show_all = FALSE) {
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
  adjp_col <- .pelsa_intensity_adjp_col(stat_df, contrast)
  cond <- .pelsa_intensity_condition_map(condition_map, processed_mat)

  # ---- Subset matched_cache to this accession (the occurrences == lines) ---
  sel <- as.character(matched_cache[["accession"]]) == accession
  sel[is.na(sel)] <- FALSE
  if (!any(sel)) {
    stop("pelsa_intensity_line_data: accession '", accession,
         "' not found in matched_cache", call. = FALSE)
  }
  m <- matched_cache[sel, , drop = FALSE]

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
