################################################################################
# Module: PELSA within-condition CV helpers
#
# Pure (non-reactive) helpers for the single CV definition used everywhere CV
# appears in PELSA (per-condition KDE in Summary, per-sample companion CV).
# CV is ALWAYS computed on RAW (un-log-transformed, LINEAR), NON-normalized
# intensities, then sd/mean*100. These helpers take whatever matrix they are
# given and assume it is ALREADY LINEAR (the math here does NOT log/delinearize
# and does NOT normalize).
#
# IMPORTANT CALLER CONTRACT: Protigy's `GCTs_original` is the LOG-TRANSFORMED
# matrix (post perform_log_transformation), NOT raw linear. CV is NOT invariant
# under log. So the SOLE caller (pelsa_run_analysis_one in
# tab_pelsa_analysis_run_helpers.R) must DELINEARIZE that matrix by the dataset's
# declared log base (pelsa_delinearize: log2 => 2^x, log10 => 10^x, None/NA =>
# already-linear pass-through) BEFORE handing it to these helpers. Passing a
# log-space matrix in here yields a quantitatively WRONG CV.
#
# VECTORIZED ONLY. PELSA matrices are 100k+ rows. All per-row work uses
# matrixStats (rowMeans2 / rowSds) and rowSums over per-condition column blocks.
# The ONLY R-level loop allowed is over the handful of CONDITIONS
# (O(n_conditions)). There is NO apply()/loop over peptide ROWS anywhere --
# apply(x, 1, sd) is the documented ~54x performance trap and is never used.
#
# KDE / density curve rendering is NOT this helper's concern (Phase 6 renders the
# curve); these helpers only build the tidy CV table. Keep free of Shiny
# reactivity so they remain unit-testable.
################################################################################

# Compute per-peptide-row CV within each condition on the RAW (non-normalized) matrix.
#
# Pipeline: for each condition compute, per peptide row over that condition's
# RAW (non-normalized) replicate columns (NA ignored): cv_pct = sd / mean * 100,
# where sd is the SAMPLE sd (ddof = 1, matrixStats::rowSds default) and mean is
# rowMeans2(na.rm = TRUE). The caller delinearizes the matrix first; CV is
# computed on the non-normalized linear intensities (no sum-normalization).
#
# cv_status per (row, condition):
#   "insufficient_replicates"  if n_nonNA < min_nonNA  (cv_pct = NA)
#   "non_finite"               if n_nonNA >= min_nonNA but cv_pct is not finite
#                              (mean is 0 / NaN / Inf, or sd / result not finite)
#                              (cv_pct = NA)
#   "ok"                       otherwise (cv_pct finite)
# When status != "ok", cv_pct is set to NA.
#
# VECTORIZED with matrixStats: for each condition's column block use
# rowMeans2() / rowSds() (na.rm = TRUE) and rowSums(!is.na(block)) for n_nonNA.
# The only loop is over the handful of CONDITIONS. NEVER apply(x, 1, sd).
#
# CONTRACT: callers must supply NON-NEGATIVE raw linear intensities. A negative
# mean is not guarded -- it would yield a finite negative cv_pct flagged "ok" --
# so non-negativity is a precondition, not a runtime check.
#
# @param raw_mat        numeric matrix of RAW intensities (or data.frame block).
# @param condition_map  named/positional condition vector (see compute helpers).
# @param min_nonNA      minimum non-NA replicates for a finite CV (>= 1L).
# @return tidy long data.frame: columns row_id (1-based peptide row index into
#         raw_mat), condition, cv_pct, n_nonNA, cv_status; one row per
#         (peptide, condition).
# @noRd
pelsa_within_condition_cv <- function(raw_mat, condition_map, min_nonNA = 3L) {
  if (is.data.frame(raw_mat)) raw_mat <- as.matrix(raw_mat)

  stopifnot(
    "raw_mat must be a matrix" = is.matrix(raw_mat),
    "raw_mat must be numeric" = is.numeric(raw_mat),
    "min_nonNA must be a single value >= 1" =
      length(min_nonNA) == 1L && !is.na(min_nonNA) && min_nonNA >= 1L
  )
  min_nonNA <- as.integer(min_nonNA)

  cond <- .pelsa_resolve_condition_map(condition_map, raw_mat)

  n_row <- nrow(raw_mat)
  conditions <- unique(cond)
  row_ids <- seq_len(n_row)

  # Accumulate one per-condition block of length n_row, then rbind once.
  parts <- vector("list", length(conditions))
  for (i in seq_along(conditions)) {
    cnd <- conditions[i]
    cols <- which(cond == cnd)
    block <- raw_mat[, cols, drop = FALSE]

    n_nonNA <- rowSums(!is.na(block)) # vectorized, no per-row loop
    means <- matrixStats::rowMeans2(block, na.rm = TRUE)
    sds <- matrixStats::rowSds(block, na.rm = TRUE)
    cv_pct <- sds / means * 100

    status <- rep("ok", n_row)
    insufficient <- n_nonNA < min_nonNA
    status[insufficient] <- "insufficient_replicates"
    nonfinite <- !insufficient & !is.finite(cv_pct)
    status[nonfinite] <- "non_finite"
    cv_pct[status != "ok"] <- NA_real_

    parts[[i]] <- data.frame(
      row_id = row_ids,
      condition = rep(cnd, n_row),
      cv_pct = cv_pct,
      n_nonNA = as.integer(n_nonNA),
      cv_status = status,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}

# Validate and resolve a condition map to a per-column character vector aligned
# to colnames(mat). Accepts a named vector (names must match columns) or a
# positionally-aligned vector. Fails fast on length/name mismatch.
#
# @param condition_map  named or positional character vector
# @param mat            the matrix whose columns it describes
# @return character vector of length ncol(mat), ordered to match the columns
# @noRd
.pelsa_resolve_condition_map <- function(condition_map, mat) {
  n_col <- ncol(mat)
  stopifnot(
    "condition_map length must match number of columns" =
      length(condition_map) == n_col
  )
  cn <- colnames(mat)
  nm <- names(condition_map)
  if (!is.null(nm) && !is.null(cn)) {
    stopifnot(
      "condition_map names must match matrix column names" =
        setequal(nm, cn)
    )
    condition_map <- condition_map[cn] # reorder to column order
  }
  as.character(unname(condition_map))
}
################################################################################
# Module: PELSA per-sample quantified-peptide depth helpers
#
# Pure (non-reactive) helpers feeding the Summary per-sample DEPTH bar plot (one
# bar per sample = number of quantified peptides) plus a companion table
# (mean / median / CV of those per-sample COUNTS).
#
# SOURCE matrix = the PROCESSED GCT (log2) matrix (GCTs_and_params()$GCTs[[ome]]),
# NOT the raw uploaded intensities. This DIFFERS from the within-condition CV
# helper above in this file (R/tab_pelsa_analysis_metrics_helpers.R), which operates on RAW linear intensities.
#
# QUANTIFIED MASK -- the canonical pelsa_quantified_mask (finite AND non-zero,
# `is.finite(x) & x != 0`). This DEPARTS from the notebook's literal `> 0`:
# the notebook ran that mask on RAW LINEAR intensities (where `> 0` correctly
# means "detected"), but Protigy applies it to the PROCESSED matrix, which is
# log-transformed and median-normalized. On a log/median-centered matrix a real
# low-abundance peptide has a NEGATIVE value and median-centering pushes ~half
# of all finite values <= 0, so `> 0` silently dropped up to ~50% of genuine
# measurements. `!= 0` keeps negatives (real, low-abundance) while still
# excluding NA (upstream maps raw 0 -> NA before logging) and exact-zero; on a
# LINEAR matrix (never negative) it is identical to the old `> 0`.
#
# CV DISTINCTION: pelsa_depth_summary()'s cv_pct is the PLAIN linear CV of the
# per-sample COUNTS (sample sd / mean * 100, ddof = 1) -- it is the CV of the
# COUNT VECTOR, NOT the CV of intensities. This is consistent with the single CV
# definition used across PELSA (sd / mean * 100); it just happens to be applied
# to the integer depth counts here.
#
# VECTORIZED ONLY. PELSA matrices are 100k+ rows. The per-sample count is a
# single colSums() over a logical matrix -- there is NO per-row/per-column
# apply() loop. Summary stats are plain base-R aggregates over the count vector.
#
# ORDERING is the CALLER's job: bars/table are ordered by the user's sample_order
# in Phase 6. These helpers return counts keyed by column name in matrix-column
# order and do not re-order. Keep free of Shiny reactivity for unit-testability.
################################################################################

# Presence/absence mask for a PELSA intensity matrix: TRUE where a value is a
# GENUINE measurement. A value is "quantified" iff it is finite AND non-zero.
#
# This is the single, canonical definition of "quantified" shared by per-sample
# depth (pelsa_peptides_per_sample), fully-quantified counts, and per-condition
# membership, so the three never drift apart.
#
# Why `!= 0` and not the notebook's `> 0`:
#   - LINEAR matrix (log_transformation = "None"): raw intensities are never
#     negative; a literal 0 means "not detected" -> excluded. So `!= 0` is
#     IDENTICAL to the old `> 0` here -- no behavior change on linear data.
#   - LOG-transformed / normalized matrix: a real low-abundance peptide has a
#     NEGATIVE log value (raw intensity < 1), and median-centering pushes ~half
#     of all finite values <= 0 BY CONSTRUCTION. The old `> 0` silently dropped
#     those genuine measurements (under-counting depth / fully-quantified /
#     membership by up to ~50%). `!= 0` keeps them. Upstream maps raw 0 -> NA
#     before logging, so NA already encodes absence; the only value `!= 0`
#     excludes on log data is an exact log == 0 (raw intensity exactly 1), a
#     negligible measure-zero edge.
# NA / NaN / Inf / -Inf -> FALSE (not finite). 0 -> FALSE (absent).
# @noRd
pelsa_quantified_mask <- function(mat) {
  is.finite(mat) & mat != 0
}

# Count, per sample column, how many peptides are "quantified" in a PROCESSED
# matrix, using the canonical pelsa_quantified_mask (finite & non-zero).
#
# Vectorized: colSums() over the logical mask matrix -- one pass, no apply loop.
#
# @param processed_mat numeric matrix (rows = peptides, cols = samples) of
#                       PROCESSED values, OR a data.frame coerced to matrix
#                       (documented). Must have UNIQUE column (sample) names
#                       (duplicates make the named-integer return ambiguous for
#                       downstream counts[name] selection).
# @return NAMED integer vector; names = sample (column) names in column order,
#         values = number of quantified peptides per sample.
# @noRd
pelsa_peptides_per_sample <- function(processed_mat) {
  # Coerce a data.frame to a numeric matrix (documented).
  if (is.data.frame(processed_mat)) processed_mat <- as.matrix(processed_mat)

  stopifnot(
    "processed_mat must be a matrix" = is.matrix(processed_mat),
    "processed_mat must be numeric" = is.numeric(processed_mat),
    "processed_mat must have column (sample) names" =
      !is.null(colnames(processed_mat)),
    "processed_mat must have unique column (sample) names" =
      !anyDuplicated(colnames(processed_mat))
  )

  counts <- colSums(pelsa_quantified_mask(processed_mat))
  storage.mode(counts) <- "integer"
  counts
}

# Companion summary statistics over the per-sample quantified-count vector.
#
# cv_pct is the PLAIN linear CV of the COUNTS (sample sd ddof=1 / mean * 100) --
# the CV of the count vector, NOT of intensities (see banner). Edge cases:
#   - empty vector       -> all stats NA (mean/median/cv).
#   - single sample      -> cv_pct NA (sample sd of one value is undefined/NA).
#   - mean count == 0    -> cv_pct NA (avoid Inf/NaN; non-finite -> NA).
#   - NA elements        -> propagate to NA mean_n/median_n/cv_pct (no na.rm).
#                           The producer pelsa_peptides_per_sample() never
#                           yields NA (colSums over a logical mask is always a
#                           finite count), so an NA here signals a CALLER BUG;
#                           we propagate rather than silently masking it.
#
# @param n_quantified     named integer vector from pelsa_peptides_per_sample()
#                          (or any numeric vector).
# @param total_n_peptides optional total peptide count carried through (the
#                          notebook sets total_n_peptides = nrow(data_df), i.e.
#                          ALL GCT rows). Defaults to NA_integer_ when not given.
# @return one-row data.frame with columns mean_n, median_n, cv_pct,
#         total_n_peptides (export-friendly).
# @noRd
pelsa_depth_summary <- function(n_quantified, total_n_peptides = NULL) {
  stopifnot(
    "n_quantified must be numeric" =
      is.numeric(n_quantified) || length(n_quantified) == 0L,
    "total_n_peptides must be NULL or a single value" =
      is.null(total_n_peptides) || length(total_n_peptides) == 1L
  )

  # Coerce supplied total to integer so the output column type is stable
  # whether the caller passes 500 or 500L.
  total <- if (is.null(total_n_peptides)) {
    NA_integer_
  } else {
    as.integer(total_n_peptides)
  }

  if (length(n_quantified) == 0L) {
    return(data.frame(
      mean_n = NA_real_,
      median_n = NA_real_,
      cv_pct = NA_real_,
      total_n_peptides = total,
      stringsAsFactors = FALSE
    ))
  }

  mean_n <- mean(n_quantified)
  median_n <- stats::median(n_quantified)
  # Sample sd (ddof = 1); NA for a single element. Guard non-finite cv -> NA.
  sd_n <- stats::sd(n_quantified)
  cv_pct <- sd_n / mean_n * 100
  if (!is.finite(cv_pct)) cv_pct <- NA_real_

  data.frame(
    mean_n = mean_n,
    median_n = median_n,
    cv_pct = cv_pct,
    total_n_peptides = total,
    stringsAsFactors = FALSE
  )
}
################################################################################
# Module: PELSA peptide helpers
#
# Pure (non-reactive) per-peptide computations:
#   pelsa_missed_cleavages() - tryptic missed-cleavage count (notebook parity)
#   pelsa_peptide_length()   - residue count
#   pelsa_build_multilabel() - canonical ;-joined gene_aa<pos> label builder
#
# Missed-cleavage MUST match the analysis notebook exactly. The notebook rule is
#   core = peptide[:-1]; len(re.findall(r'[KR](?!P)', core))
# i.e. drop the C-terminal residue, then count internal K/R NOT immediately
# followed by P (the Keil trypsin rule: K-P and R-P are not cleaved). This count
# feeds the Summary missed-cleavage distribution and is parity-critical, so the
# implementation mirrors the regex on substr(seq, 1, nchar(seq) - 1L).
#
# pelsa_build_multilabel() is the single source of truth for volcano-point
# labels, reused by the best-peptide rollup (2G) and the volcano-df builders
# (3A) for both all-peptide and best-peptide panels.
#
# All functions are vectorized where the per-peptide contract allows (PELSA
# frames are 100k+ rows). They take no Shiny reactivity so they stay
# unit-testable.
################################################################################

# Count tryptic missed cleavages for each peptide sequence.
#
# Matches the notebook's exact rule: drop the C-terminal residue, then count
# internal K or R that is NOT immediately followed by P. stringr uses ICU regex,
# so the negative lookahead `[KR](?!P)` is supported. Peptides shorter than 2
# residues have no internal positions and yield 0. NA sequences yield NA.
#
# @param seq character vector of stripped peptide sequences
# @return integer vector of missed-cleavage counts, same length as `seq`
# @noRd
pelsa_missed_cleavages <- function(seq) {
  n <- length(seq)
  if (n == 0L) return(integer(0))

  # Coerce factor columns (e.g. from a TSV/join read) so nchar() never errors.
  seq <- as.character(seq)

  # Drop the C-terminal residue; sequences shorter than 2 have an empty core.
  len <- nchar(seq)
  core <- substr(seq, 1L, len - 1L)
  core[!is.na(len) & len < 2L] <- ""

  # Count internal K/R not immediately followed by P (Keil rule, K-P excluded).
  counts <- stringr::str_count(core, "[KR](?!P)")

  # Preserve NA sequences as NA; str_count already returns NA for NA input,
  # this guard makes the contract explicit and robust to backend differences.
  counts[is.na(seq)] <- NA_integer_
  as.integer(counts)
}

# Peptide residue count.
#
# Centralizes peptide-length logic so callers do not scatter nchar() calls.
# NA sequences yield NA.
#
# @param seq character vector of stripped peptide sequences
# @return integer vector of residue counts, same length as `seq`
# @noRd
pelsa_peptide_length <- function(seq) {
  # Coerce factor columns (e.g. from a TSV/join read) so nchar() never errors.
  as.integer(nchar(as.character(seq)))
}

# Per-sample missed-cleavage rate: for each sample, the fraction of peptides
# quantified (finite & non-zero) in that sample with >= 1 missed cleavage.
#
# @param proc_mat        peptides x samples numeric matrix (colnames = samples).
# @param peptide_metrics the cache peptide_metrics frame, row-aligned to
#                        proc_mat (same row order/count).
# @return data.frame(sample, rate, n_quantified), one row per proc_mat column
#         in column order. rate is NA when n_quantified == 0 for that sample.
#         Empty frame (0 rows, documented columns) when proc_mat has 0 columns.
# @noRd
pelsa_missed_cleavage_rate_by_sample <- function(proc_mat, peptide_metrics) {
  empty <- data.frame(sample = character(0), rate = numeric(0),
                      n_quantified = integer(0), stringsAsFactors = FALSE)
  if (is.data.frame(proc_mat)) proc_mat <- as.matrix(proc_mat)
  if (!is.matrix(proc_mat) || ncol(proc_mat) == 0L) return(empty)

  has_mc <- as.integer(suppressWarnings(
    as.integer(peptide_metrics$missed_cleavages))) >= 1L
  mask <- pelsa_quantified_mask(proc_mat)

  samples <- colnames(proc_mat)
  n_quant <- as.integer(colSums(mask))
  n_with_mc <- as.integer(colSums(mask & has_mc))
  rate <- ifelse(n_quant == 0L, NA_real_, n_with_mc / n_quant)

  data.frame(sample = samples, rate = rate, n_quantified = n_quant,
            stringsAsFactors = FALSE)
}

# Per-sample mean peptide length: for each sample, the mean residue length of
# peptides quantified (finite & non-zero) in that sample.
#
# @param proc_mat        peptides x samples numeric matrix (colnames = samples).
# @param peptide_metrics the cache peptide_metrics frame, row-aligned to
#                        proc_mat.
# @return data.frame(sample, mean_length, n_quantified). mean_length is NA
#         when n_quantified == 0. Empty frame when proc_mat has 0 columns.
# @noRd
pelsa_length_by_sample <- function(proc_mat, peptide_metrics) {
  empty <- data.frame(sample = character(0), mean_length = numeric(0),
                      n_quantified = integer(0), stringsAsFactors = FALSE)
  if (is.data.frame(proc_mat)) proc_mat <- as.matrix(proc_mat)
  if (!is.matrix(proc_mat) || ncol(proc_mat) == 0L) return(empty)

  lens <- suppressWarnings(as.numeric(peptide_metrics$peptide_length))
  mask <- pelsa_quantified_mask(proc_mat)

  samples <- colnames(proc_mat)
  n_quant <- as.integer(colSums(mask))
  mean_length <- vapply(seq_len(ncol(mask)), function(j) {
    v <- lens[mask[, j]]
    v <- v[is.finite(v)]
    if (length(v) == 0L) NA_real_ else mean(v)
  }, numeric(1))

  data.frame(sample = samples, mean_length = mean_length,
            n_quantified = n_quant, stringsAsFactors = FALSE)
}

# Resolve the label STEM (the text before "_aa<pos>") for a set of peptide
# mappings. Fallback order: gene -> protein_name -> accession. A stem is
# "missing" when it is NA or blank/whitespace after trimming (readr renders a
# blank report cell as NA, so both must count as missing). Self-curated species
# have no UniProt gene and no protein name, so they force the accession stem.
#
# Vectorized: every argument is a character vector (or a scalar recycled by the
# fifelse cascade). `protein_name` may be NULL when the caller's report has no
# PG.ProteinNames column, in which case the middle tier is skipped and the
# fallback degrades to the legacy gene -> accession behavior.
#
# @param gene            character vector of gene symbols (may be NA/"")
# @param protein_name    character vector of protein names (may be NA/""), or
#                        NULL when unavailable
# @param accession       character vector of accessions (the final fallback)
# @param is_self_curated single logical; TRUE forces the accession stem
# @return character vector of label stems, same length as `accession`
# @noRd
pelsa_resolve_label_stem <- function(gene, protein_name, accession,
                                     is_self_curated = FALSE) {
  accession <- as.character(accession)
  if (isTRUE(is_self_curated)) return(accession)

  gene <- as.character(gene)
  if (is.null(protein_name)) {
    protein_name <- rep(NA_character_, length(accession))
  } else {
    protein_name <- as.character(protein_name)
  }

  missing_gene <- is.na(gene) | !nzchar(trimws(gene))
  missing_name <- is.na(protein_name) | !nzchar(trimws(protein_name))

  # gene -> protein_name -> accession, applied as a two-step cascade so a blank
  # gene with a real protein name lands on the name, and both blank lands on the
  # accession.
  stem <- ifelse(missing_gene,
                 ifelse(missing_name, accession, protein_name),
                 gene)
  as.character(stem)
}

# Build one ;-joined multi-label string for a single peptide/dot.
#
# Given the per-mapping (gene, position, accession) vectors for ONE peptide
# (all the same length; these are the distinct accession x occurrence mappings
# in PG.ProteinAccessions token order), produce a single label string:
#   - each entry is "<gene>_aa<pos>", falling back to protein-name then
#     accession when the gene is empty/NA;
#   - fully-identical entries collapse to one;
#   - distinct entries (e.g. same gene at different positions) are kept;
#   - no cap; entries are joined with ";" in first-occurrence input order.
# Vectorization across many peptides is the caller's job (2G/3A): this builds
# exactly one label. Empty input returns NA_character_.
#
# An NA position renders literally as "<id>_aaNA": this is NOT guarded, by
# design, since callers pass FASTA-resolved positions (positions are expected to
# be resolved before this builder is reached).
#
# @param genes         character vector of gene symbols (may contain "" or NA)
# @param positions     integer or character vector of residue positions
# @param accessions    character vector of protein accessions (final fallback)
# @param is_self_curated single logical; TRUE forces accession stem
# @param protein_names character vector of PG.ProteinNames tokens (may be
#                      NA/""), or NULL when unavailable
# @return character scalar label for the peptide
# @noRd
pelsa_build_multilabel <- function(genes, positions, accessions,
                                   is_self_curated = FALSE,
                                   protein_names = NULL) {
  if (length(genes) == 0L) return(NA_character_)

  # Fail fast on length mismatch: a silent scalar recycle would emit a
  # plausible-but-wrong volcano label, which is worse than a loud failure.
  stopifnot(
    length(genes) == length(positions),
    length(genes) == length(accessions)
  )
  if (!is.null(protein_names)) {
    stopifnot(length(genes) == length(protein_names))
  }

  # Stem fallback gene -> protein_name -> accession (single source of truth).
  # Self-curated species have no UniProt gene/name: the resolver forces the
  # accession stem.
  label_id <- pelsa_resolve_label_stem(genes, protein_names, accessions,
                                       is_self_curated)

  entries <- paste0(label_id, "_aa", as.character(positions))

  # Collapse fully-identical entries, preserving first-occurrence order.
  entries <- entries[!duplicated(entries)]
  paste(entries, collapse = ";")
}
################################################################################
# Module: PELSA best-peptide-per-protein rollup helpers
#
# Pure (non-reactive) two-step rollup of an EXPLODED stat frame (one row per
# peptide x accession, already carrying per-contrast adj.P.Val / logFC for the
# contrast the caller selected) into one dot per distinct best-peptide.
#
#   Step 1 - per-accession best peptide (the notebook's _rollup_to_proteins):
#     STABLE sort on [adj.P.Val, logFC, peptide_seq, accession], then keep the
#     FIRST row per accession. The last two keys are a DETERMINISTIC total-order
#     tiebreak so the chosen "best" peptide is fully reproducible even on exact
#     (adj.P.Val, logFC) ties. data.table::setorder is a stable sort; the
#     four-key total order makes the head(1)-per-accession pick deterministic.
#
#   Step 2 - regroup winners by peptide (Protigy refinement):
#     A peptide has a SINGLE (adj.P.Val, logFC) coordinate, so a peptide that
#     won multiple accessions must be ONE dot, not several overlapping ones.
#     Group the step-1 winners by peptide_seq and emit one row per distinct
#     best-peptide, carrying a ;-joined multi-label (one <gene>_aa<pos> per won
#     accession) built via pelsa_build_multilabel() (the single source of truth
#     for labels - reused, not reimplemented).
#
# NA handling: rows with NA adj.P.Val sort LAST (na.last = TRUE) so a peptide
# with a real p-value is preferred over an NA one. An accession whose ONLY
# peptide has all-NA stats still keeps that peptide (its adj_p / logFC are NA).
#
# The down-only (logFC < 0) PELSA signature is applied via the most-negative-
# logFC tiebreak ONLY (ascending logFC), NOT as a filter - all peptides remain
# eligible. adj.P.Val / logFC are computed upstream (Statistics tab) and passed
# in by the caller; this module does not compute stats.
#
# Vectorized: setorder + per-accession .SD[1L], then a per-peptide group-by.
# pelsa_build_multilabel() is called ONCE PER DISTINCT BEST-PEPTIDE (the small
# winners set), never per exploded row. No per-row apply over the full frame.
################################################################################

# Roll an exploded per-(peptide, accession) stat frame up to one dot per
# distinct best-peptide.
#
# @param exploded_stat_df data.frame; one row per peptide x accession, carrying
#   the selected contrast's adj.P.Val / logFC plus accession / gene / pep_start.
# @param adjp_col   column name of the (selected-contrast) adjusted p-value
# @param logfc_col  column name of the (selected-contrast) log fold change
# @param pep_col    column name of the stripped peptide sequence
# @param acc_col    column name of the protein accession
# @param gene_col   column name of the gene symbol
# @param pos_col    column name of the FASTA-resolved peptide start position
# @return data.frame, one row per distinct best-peptide, with columns:
#   peptide_seq, adj_p, logFC, label, won_accessions, n_won
# @noRd
pelsa_best_peptide_rollup <- function(exploded_stat_df,
                                      adjp_col  = "adj.P.Val",
                                      logfc_col = "logFC",
                                      pep_col   = "PEP.StrippedSequence",
                                      acc_col   = "accession",
                                      gene_col  = "gene",
                                      pos_col   = "pep_start",
                                      is_self_curated = FALSE) {
  # ---- Boundary validation (fail fast) ------------------------------------
  if (!is.data.frame(exploded_stat_df)) {
    stop("pelsa_best_peptide_rollup: exploded_stat_df must be a data.frame")
  }
  required <- c(adjp_col, logfc_col, pep_col, acc_col, gene_col, pos_col)
  missing_cols <- setdiff(required, colnames(exploded_stat_df))
  if (length(missing_cols) > 0L) {
    stop("pelsa_best_peptide_rollup: missing required column(s): ",
         paste(missing_cols, collapse = ", "))
  }

  out_cols <- c("peptide_seq", "adj_p", "logFC", "label",
                "won_accessions", "n_won")
  empty_out <- data.frame(
    peptide_seq    = character(0),
    adj_p          = numeric(0),
    logFC          = numeric(0),
    label          = character(0),
    won_accessions = character(0),
    n_won          = integer(0),
    stringsAsFactors = FALSE
  )
  if (nrow(exploded_stat_df) == 0L) return(empty_out)

  # ---- Project to a minimal, stably-named data.table ----------------------
  dt <- data.table::data.table(
    peptide_seq  = as.character(exploded_stat_df[[pep_col]]),
    accession    = as.character(exploded_stat_df[[acc_col]]),
    gene         = as.character(exploded_stat_df[[gene_col]]),
    protein_name = if ("protein_name" %in% colnames(exploded_stat_df))
      as.character(exploded_stat_df[["protein_name"]]) else
      rep(NA_character_, nrow(exploded_stat_df)),
    pep_start    = exploded_stat_df[[pos_col]],
    adj_p        = as.numeric(exploded_stat_df[[adjp_col]]),
    logFC        = as.numeric(exploded_stat_df[[logfc_col]])
  )

  # ---- Step 1: stable total-order sort + first row per accession ----------
  # setorder is a STABLE sort; na.last = TRUE pushes NA adj_p / logFC to the
  # end so a real-stat peptide outranks an NA one. The peptide_seq / accession
  # keys give a deterministic total order so exact (adj_p, logFC) ties resolve
  # to the same row on every run.
  data.table::setorder(dt, adj_p, logFC, peptide_seq, accession,
                        na.last = TRUE)
  winners <- dt[, .SD[1L], by = "accession"]

  # ---- Step 2: regroup winners by peptide -> one dot per best-peptide -----
  # Per distinct best-peptide: its single (adj_p, logFC) coordinate, a ;-joined
  # multi-label over the accessions it won, the ;-joined won accessions, and the
  # win count. pelsa_build_multilabel runs once per peptide group (small set).
  out_dt <- winners[, {
    # The LABEL's ;-joined entries are ordered by (pep_start, accession) so the
    # best_peptide multi-label matches the all_peptide panel's entry order
    # (.pelsa_volcano_labels sorts by (pep_start, accession)); otherwise the two
    # panels could show the same peptide's ;-joined tokens in a different order.
    # NOTE: this reorders ONLY the display label. won_accessions stays in the
    # winners' stats-priority row order because its FIRST token is the
    # representative won accession (.pelsa_best_back_map drives the dot's
    # protein/gene/span/P.Value/is_marker from won_accessions[1]).
    lab_ord <- order(pep_start, accession)
    list(
      # adj_p[1L] / logFC[1L] rely on the per-peptide-single-coordinate
      # invariant: a peptide has ONE (adj_p, logFC), so any winner row for it
      # carries that same coordinate -- the first row is representative.
      adj_p          = adj_p[1L],
      logFC          = logFC[1L],
      label          = pelsa_build_multilabel(gene[lab_ord], pep_start[lab_ord],
                                               accession[lab_ord],
                                               is_self_curated,
                                               protein_names = protein_name[lab_ord]),
      won_accessions = paste(accession, collapse = ";"),
      n_won          = .N
    )
  }, by = "peptide_seq"]

  # Stable, deterministic row order for reproducible output.
  data.table::setorder(out_dt, adj_p, logFC, peptide_seq, na.last = TRUE)

  out <- as.data.frame(out_dt, stringsAsFactors = FALSE)
  out$n_won <- as.integer(out$n_won)
  out[, out_cols, drop = FALSE]
}
################################################################################
# PELSA marker matching + parsing (Task 2J) - the pure, testable matching/
# parsing core. The org.db / UniProt accession<->gene resolution UI (canonical
# / reviewed flags) lives in Phase 5; this file only owns the deterministic
# helpers it will call.
#
# Public helpers:
#   pelsa_isoform_base(accession)   strip UniProt isoform suffix "-<n>" (vectorized)
#   pelsa_parse_markers(raw)        marker paste-box string -> token char vector
#   pelsa_match_markers(acc, mk)    per-peptide logical: ANY token == ANY marker
#
# MATCHING RULE (pelsa_match_markers):
#   A peptide is a marker hit if ANY of its ;-delimited accession tokens,
#   normalized to isoform-BASE + lowercased, equals ANY marker's isoform-base +
#   lowercased. The rule is by ACCESSION (not gene), ANY-TOKEN, CASE-INSENSITIVE,
#   and SYMMETRIC in the isoform suffix: marker "P12345" matches a peptide on
#   "P12345-2", and marker "P12345-2" matches a peptide on "P12345" (both
#   normalize to base "p12345").
#
# pelsa_isoform_base consolidates the suffix-strip that the coverage and fasta
# helpers (R/tab_pelsa_analysis_helpers.R) and
# tab_pelsa_annotation_helpers.R each inlined
# (sub("-[0-9]+$", "", x)); those files are intentionally left unchanged - this
# is the shared entry point for future use (Phase 5/7).
################################################################################

# Strip the UniProt isoform suffix from accession(s): "P12345-2" -> "P12345".
# Vectorized over a character vector; NA -> NA. Only a TRAILING "-<digits>" is
# removed (so "A0A-B1" is unchanged; "Q9-10" -> "Q9", intended for the UniProt
# isoform-suffix convention).
# @noRd
pelsa_isoform_base <- function(accession) {
  if (is.null(accession) || length(accession) == 0L) {
    return(character(0))
  }
  if (!is.character(accession)) {
    stop("pelsa_isoform_base(): `accession` must be a character vector.",
         call. = FALSE)
  }
  sub("-[0-9]+$", "", accession)
}

# Parse a marker paste-box string into a character vector of tokens. Thin named
# wrapper over parse_protein_search_input (splits on space/comma/semicolon/
# newline, trims, drops empties). Returns character(0) on empty / NULL.
# @noRd
pelsa_parse_markers <- function(raw) {
  parse_protein_search_input(raw)
}

# Normalize a character vector of accessions to the matching key: isoform-base,
# trimmed, lowercased, with empties dropped.
# @noRd
pelsa_marker_key <- function(x) {
  x <- pelsa_isoform_base(trimws(x))
  x <- tolower(x)
  x[!is.na(x) & nzchar(x)]
}

# Per-peptide logical: does ANY of the peptide's accession tokens match ANY
# marker, by isoform-base + case-insensitive accession (see header for the full
# SYMMETRIC rule)?
#
# @param accession_tokens_list  EITHER a character vector of ";"-delimited
#   accession strings (one per peptide, e.g. "P12345;Q99999") OR an already-split
#   list of character vectors (one per peptide). The ";"-string form is primary
#   and is split internally on ";".
# @param marker_accessions      character vector of marker accessions as entered
#   (may themselves be isoform forms like "P12345-2").
# @return logical vector, one per peptide (TRUE = marker hit). Vectorized: no
#   per-peptide R loop.
#
# Edges: empty `marker_accessions` -> all FALSE; a peptide with NA/empty
# accessions -> FALSE; tokens are whitespace-trimmed.
# @noRd
pelsa_match_markers <- function(accession_tokens_list, marker_accessions) {
  # ---- Boundary validation (fail fast) ------------------------------------
  if (!is.character(marker_accessions)) {
    stop("pelsa_match_markers(): `marker_accessions` must be a character vector.",
         call. = FALSE)
  }
  is_list_form <- is.list(accession_tokens_list)
  if (!is_list_form && !is.character(accession_tokens_list)) {
    stop("pelsa_match_markers(): `accession_tokens_list` must be a character ",
         "vector of ;-delimited accessions or a list of character vectors.",
         call. = FALSE)
  }

  n <- length(accession_tokens_list)

  # ---- Normalize marker set once ------------------------------------------
  marker_keys <- unique(pelsa_marker_key(marker_accessions))
  if (n == 0L || length(marker_keys) == 0L) {
    return(rep(FALSE, n))
  }

  # ---- Build a (row_id, token) grid, then vectorize membership ------------
  if (is_list_form) {
    tokens_by_row <- accession_tokens_list
  } else {
    tokens_by_row <- strsplit(accession_tokens_list, ";", fixed = TRUE)
    # strsplit() on NA yields NA_character_; keep length 1 so row_id aligns.
  }

  lengths_per_row <- lengths(tokens_by_row)
  # Rows with zero tokens (e.g. "" -> character(0)) contribute nothing but must
  # still occupy a slot in the result; tapply handles absent row_ids via levels.
  row_id <- rep.int(seq_len(n), lengths_per_row)
  flat_tokens <- unlist(tokens_by_row, use.names = FALSE)

  if (length(flat_tokens) == 0L) {
    return(rep(FALSE, n))
  }

  flat_keys <- pelsa_isoform_base(trimws(flat_tokens))
  flat_keys <- tolower(flat_keys)
  is_hit <- !is.na(flat_keys) & nzchar(flat_keys) & flat_keys %in% marker_keys

  # any() per row across all its tokens, restoring all n rows in order.
  agg <- tapply(is_hit, factor(row_id, levels = seq_len(n)), FUN = any)
  out <- as.logical(agg)
  out[is.na(out)] <- FALSE
  out
}
