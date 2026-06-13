################################################################################
# Module: PELSA within-condition CV helpers
#
# Pure (non-reactive) helpers for the single CV definition used everywhere CV
# appears in PELSA (per-condition KDE in Summary, per-sample companion CV).
# CV is ALWAYS computed on RAW (un-log-transformed, linear) intensities that
# have first been SUM-NORMALIZED per condition, then sd/mean*100. This mirrors
# the PELSA notebook pipeline (normalization.py::sum_normalize then CV). The
# caller (Phase 6) supplies the ORIGINAL raw uploaded intensities (GCTs_original),
# NOT Protigy's processed/log2 matrix; these helpers just take whatever raw
# matrix they are given.
#
# VECTORIZED ONLY. PELSA matrices are 100k+ rows. All per-row work uses
# matrixStats (rowMeans2 / rowSds) and rowSums over per-condition column blocks.
# The ONLY R-level loop allowed is over the handful of CONDITIONS
# (O(n_conditions)). There is NO apply()/loop over peptide ROWS anywhere --
# apply(x, 1, sd) is the documented ~54x performance trap and is never used.
#
# KDE / density curve rendering is NOT this helper's concern (Phase 6 renders the
# curve); these helpers only build the sum-normalized matrix and the tidy CV
# table. Keep free of Shiny reactivity so they remain unit-testable.
################################################################################

# Sum-normalize a RAW (linear, un-logged) intensity matrix on a PER-CONDITION
# COMPLETE-CASE basis, scale = "mean".
#
# EXACT FORMULA IMPLEMENTED (the closed-form parity tests pin this):
#   For each condition c (a group of replicate sample columns):
#     1. complete-case feature set CC(c) = the peptide ROWS that are non-NA
#        across ALL of condition c's replicate columns.
#     2. For each column j in c, colSum_cc[j] = sum over rows in CC(c) of the
#        raw value in column j.
#     3. target = mean over condition c's columns of colSum_cc  (scale="mean").
#     4. factor_j = target / colSum_cc[j].
#     5. normalized_col_j = raw_col_j * factor_j   (applied to ALL rows,
#        including rows not in CC(c)). NA positions stay NA.
#   Columns of the SAME condition therefore share a common complete-case total
#   equal to the condition's mean raw complete-case column sum.
#
# Only the `scale = "mean"` rescale is implemented (the PELSA default); the
# argument exists to document/lock that choice. A condition column whose
# complete-case sum is 0 yields a non-finite factor; that column is left scaled
# by 1 (no rescale) rather than producing Inf/NaN intensities -- such degenerate
# columns surface downstream as non_finite CV, not silently corrupted values.
#
# CONTRACT: callers must supply NON-NEGATIVE raw linear intensities. Negative
# values are not guarded; a condition whose complete-case column sum is negative
# would produce a negative rescale factor (sign-flipped intensities). PELSA
# intensities are abundances (>= 0), so this is a precondition, not a guard.
#
# @param mat            numeric matrix (rows = peptides, cols = samples), OR a
#                       data.frame intensity block (coerced to matrix).
# @param condition_map  named character vector (names = colnames(mat)) OR a
#                       character vector aligned positionally to the columns.
# @param scale          rescale target; only "mean" is supported.
# @return numeric matrix, same shape/dimnames as mat; NA positions preserved.
# @noRd
pelsa_sum_normalize <- function(mat, condition_map, scale = "mean") {
  # Coerce a data.frame intensity block to a numeric matrix (documented).
  if (is.data.frame(mat)) mat <- as.matrix(mat)

  stopifnot(
    "mat must be a matrix" = is.matrix(mat),
    "mat must be numeric" = is.numeric(mat),
    "scale must be 'mean'" = identical(scale, "mean")
  )

  cond <- .pelsa_resolve_condition_map(condition_map, mat)

  out <- mat
  conditions <- unique(cond)
  # Loop over the FEW conditions only -- never over peptide rows.
  for (cnd in conditions) {
    cols <- which(cond == cnd)
    block <- mat[, cols, drop = FALSE]

    # Complete-case rows: non-NA across ALL of this condition's columns.
    cc <- rowSums(is.na(block)) == 0L
    if (!any(cc)) next # no complete-case features -> leave block unscaled

    col_sums_cc <- colSums(block[cc, , drop = FALSE])
    target <- mean(col_sums_cc) # scale = "mean"
    factors <- target / col_sums_cc
    # Guard degenerate columns (cc sum 0 -> Inf/NaN factor): do not rescale.
    factors[!is.finite(factors)] <- 1

    # Apply per-column factors to the WHOLE block (vectorized; NA stays NA).
    out[, cols] <- sweep(block, 2L, factors, FUN = "*")
  }

  out
}

# Compute per-peptide-row CV within each condition on the SUM-NORMALIZED matrix.
#
# Pipeline: sum-normalize the raw matrix (pelsa_sum_normalize), then for each
# condition compute, per peptide row over that condition's normalized replicate
# columns (NA ignored): cv_pct = sd / mean * 100, where sd is the SAMPLE sd
# (ddof = 1, matrixStats::rowSds default) and mean is rowMeans2(na.rm = TRUE).
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
# CONTRACT: callers must supply NON-NEGATIVE raw linear intensities (see
# pelsa_sum_normalize). A negative normalized mean is not guarded -- it would
# yield a finite negative cv_pct flagged "ok" -- so non-negativity is a
# precondition, not a runtime check.
#
# @param raw_mat        numeric matrix of RAW intensities (or data.frame block).
# @param condition_map  named/positional condition vector (see sum_normalize).
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
  norm <- pelsa_sum_normalize(raw_mat, condition_map)

  n_row <- nrow(raw_mat)
  conditions <- unique(cond)
  row_ids <- seq_len(n_row)

  # Accumulate one per-condition block of length n_row, then rbind once.
  parts <- vector("list", length(conditions))
  for (i in seq_along(conditions)) {
    cnd <- conditions[i]
    cols <- which(cond == cnd)
    block <- norm[, cols, drop = FALSE]

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
