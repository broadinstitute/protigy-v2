################################################################################
# Module: LM_Setup Helpers
#
# Backend computation logic for the Linear Model module.
# Builds design matrices, runs limma lmFit -> eBayes, extracts per-coefficient
# results via topTable.
################################################################################

#' Resolve the per-ome intensity flag.
#'
#' Each ome's `intensity` flag controls whether `eBayes(trend=TRUE)` is used
#' on that ome's fit. The flag is set by the sidebar upload module per-ome and
#' can arrive as logical, integer (0/1), or string ("yes"/"true"/"1"/"t").
#' Anything missing/unknown coerces to FALSE.
#'
#' @param parameters Named list of per-ome parameter lists (the value of
#'   `GCTs_and_params()$parameters`).
#' @param ome Character scalar, the ome key to look up.
#' @return logical scalar.
pick_intensity_for_ome <- function(parameters, ome) {
  if (is.null(parameters) || !is.list(parameters)) return(FALSE)
  v <- parameters[[ome]]$intensity
  if (is.null(v)) return(FALSE)
  if (is.character(v)) {
    return(tolower(v) %in% c("yes", "true", "1", "t"))
  }
  tmp <- suppressWarnings(as.logical(v))
  if (length(tmp) != 1 || is.na(tmp)) return(FALSE)
  tmp
}


#' Canonical control-token list for default-reference-level heuristic.
#'
#' Order matters: tokens earlier in the list win over later tokens when more
#' than one match exists. Lowercased so callers can compare against
#' `tolower(level)`.
#' @keywords internal
.lm_control_tokens <- c(
  "control", "ctrl", "vehicle", "wt", "wildtype",
  "baseline", "untreated", "placebo", "none", "healthy"
)


#' Choose a defensible default reference level for a factor variable.
#'
#' Replaces the alphabetical-first default (R's `factor()` behaviour) with a
#' rule chain that more often matches the scientist's mental model:
#'
#' 1. If any observed level (case-insensitive) matches a control token from
#'    `.lm_control_tokens`, pick that level. First match wins, by token-list
#'    order.
#' 2. Otherwise, pick the most-frequent (modal) level.
#' 3. If two or more levels tie for modal, pick the alphabetical first.
#'
#' Empty / NULL / all-NA input returns `NA_character_`.
#'
#' @param values Character vector or factor of observed levels (one entry per
#'   sample). NA and empty strings are dropped.
#' @return A list with:
#' - `level`: chosen level (character) or `NA_character_` for empty input.
#' - `reason`: one of `"control_token"`, `"modal"`, `"tie_alphabetical"`,
#'     `"single"`, `"empty"`.
#' - `matched_token`: present only when `reason == "control_token"`; the
#'     lowercase token from `.lm_control_tokens` that matched.
#' - `n`: present only when `reason == "modal"`; the count for the modal level.
#' @export
pick_default_reference_level <- function(values) {
  if (is.null(values) || length(values) == 0) {
    return(list(level = NA_character_, reason = "empty"))
  }
  vals <- as.character(values)
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals) == 0) {
    return(list(level = NA_character_, reason = "empty"))
  }

  lv <- unique(vals)
  if (length(lv) == 1) {
    return(list(level = lv, reason = "single"))
  }

  # 1. Control-token match (case-insensitive). Iterate tokens in priority order
  #    so the earliest matching token wins regardless of the input ordering.
  lv_lower <- tolower(lv)
  for (tok in .lm_control_tokens) {
    hit_idx <- which(lv_lower == tok)
    if (length(hit_idx) > 0) {
      return(list(
        level = lv[hit_idx[1]],
        reason = "control_token",
        matched_token = tok
      ))
    }
  }

  # 2/3. Modal level, with alphabetical tie-break.
  counts <- table(vals)
  max_count <- max(counts)
  modal_levels <- sort(names(counts)[counts == max_count])
  if (length(modal_levels) > 1) {
    return(list(
      level = modal_levels[1],
      reason = "tie_alphabetical"
    ))
  }
  list(
    level = modal_levels[1],
    reason = "modal",
    n = as.integer(max_count)
  )
}


#' Format the default-reference-level result as a short user-facing annotation.
#'
#' Used to display *why* the system picked a particular default so the user
#' can decide whether to override it.
#'
#' @param result Output of [pick_default_reference_level()].
#' @return Character scalar. Empty string for `single` / `empty` (no useful
#'   annotation in those cases - the picker can stay un-annotated).
#' @export
format_reference_level_annotation <- function(result) {
  if (is.null(result) || is.null(result$reason)) return("")
  switch(
    result$reason,
    control_token = sprintf("(matched \"%s\")", result$matched_token),
    modal         = sprintf("(modal, n=%d)", result$n),
    tie_alphabetical = "(modal tie; alphabetical fallback)",
    single = "",
    empty  = "",
    ""
  )
}


#' Summarize how many samples will be dropped by `complete.cases` filtering.
#'
#' Given the working `cdesc` and the variables that participate in the design
#' (model variables plus the optional blocking variable), report how many
#' samples will survive the `complete.cases` filter and attribute the drops to
#' the columns that introduced NAs.
#'
#' Returns NULL when there is nothing meaningful to report (empty data, no
#' inspectable variables).
#'
#' @param cdesc Data frame of sample metadata (rows = samples).
#' @param model_vars Character vector of column names that feed
#'   `complete.cases()`. Duplicates are deduped; unknown columns are silently
#'   ignored so callers don't have to pre-filter the formula's `all.vars()`
#'   output against `colnames(cdesc)`.
#' @return Either NULL or a list with:
#' - `n_total`: integer, `nrow(cdesc)`.
#' - `n_used`: integer, surviving rowcount.
#' - `n_dropped`: integer, `n_total - n_used`.
#' - `dropped_columns`: character vector of column names whose NAs caused
#'     drops, ordered by descending NA count (alphabetical tie-break). Empty
#'     when nothing was dropped.
#' - `message`: one-line human-readable caption suitable for display above
#'     the design-matrix preview.
#' @export
summarize_sample_drops <- function(cdesc, model_vars) {
  if (is.null(cdesc) || !is.data.frame(cdesc) || nrow(cdesc) == 0) {
    return(NULL)
  }
  vars <- unique(as.character(model_vars))
  vars <- vars[vars %in% colnames(cdesc)]
  if (length(vars) == 0) return(NULL)

  sub <- cdesc[, vars, drop = FALSE]
  mask <- stats::complete.cases(sub)
  n_total <- as.integer(nrow(cdesc))
  n_used <- as.integer(sum(mask))
  n_dropped <- n_total - n_used

  na_counts <- vapply(vars, function(v) sum(is.na(cdesc[[v]])), integer(1))
  offenders <- names(na_counts)[na_counts > 0]
  if (length(offenders) > 0) {
    # Order: descending NA count, alphabetical tie-break.
    ord <- order(-na_counts[offenders], offenders)
    offenders <- offenders[ord]
  } else {
    offenders <- character(0)
  }

  message <- if (n_dropped == 0L) {
    sprintf("Using all %d samples.", n_total)
  } else if (length(offenders) == 1L) {
    sprintf("Using %d of %d samples (%d dropped: missing '%s').",
            n_used, n_total, n_dropped, offenders)
  } else {
    sprintf("Using %d of %d samples (%d dropped: missing '%s').",
            n_used, n_total, n_dropped,
            paste(offenders, collapse = "', '"))
  }

  list(
    n_total = n_total,
    n_used = n_used,
    n_dropped = as.integer(n_dropped),
    dropped_columns = offenders,
    message = message
  )
}


#' Build a formula string from user selections
#'
#' @param variables Character vector of selected variable names
#' @param include_intercept Logical, whether to include intercept
#' @param interactions List of character vectors, each a pair of variable names
#' @return A formula string (e.g., "~ Treatment + Age + Treatment:Age")
build_formula_string <- function(variables, include_intercept = TRUE, interactions = list()) {
  if (length(variables) == 0) return("")

  terms <- variables

  # Add interaction terms
  if (length(interactions) > 0) {
    for (inter in interactions) {
      if (length(inter) == 2 && all(inter %in% variables)) {
        terms <- c(terms, paste(inter[1], inter[2], sep = ":"))
      }
    }
  }

  rhs <- paste(terms, collapse = " + ")

  if (include_intercept) {
    formula_str <- paste("~", rhs)
  } else {
    formula_str <- paste("~ 0 +", rhs)
  }

  formula_str
}


#' Build the LM design matrix: single source of truth for preview and fit
#'
#' Constructs the design matrix exactly as [lm.regression()] does, so the
#' on-screen design preview (in `tab_lm_setup`) and the actual fit can never
#' disagree. It performs, in order: formula normalization (empty formula plus a
#' blocking variable becomes intercept-only `~ 1`), per-variable coercion and
#' reference releveling, complete-case sample dropping over the formula
#' variables *and* the blocking variable, `droplevels` with a single-level
#' guard, repeated-measures detection, and a rank-deficiency check.
#'
#' Diagnostics are returned as DATA, never signalled: `error` is a message
#' string when no usable design can be built (or `NULL` on success), and
#' `warnings` is a character vector of non-fatal notices (rank deficiency,
#' missing/degenerate blocking). The preview renders these; [lm.regression()]
#' re-raises them via `stop()`/`warning()`.
#'
#' @param cdesc Sample metadata `data.frame` (a GCT `cdesc`); row names are
#'   sample ids.
#' @param formula_string Model formula string; `""`/`NA`/`NULL` is allowed only
#'   when `blocking_var` is set (repeated measures without groups).
#' @param variable_types Named list mapping variable names to `"factor"` or
#'   `"continuous"`.
#' @param reference_levels Named list mapping factor variables to a chosen
#'   reference level; unknown variables/levels are ignored.
#' @param blocking_var Optional blocking variable name (a column in `cdesc`).
#' @return A list with elements: `design` (numeric matrix or `NULL` on error),
#'   `cdesc_clean` (filtered metadata or `NULL`), `n_used`, `n_total`,
#'   `dropped` (count), `repeated_measures_only` (logical),
#'   `warnings` (character vector), and `error` (message string or `NULL`).
build_lm_design <- function(cdesc,
                            formula_string,
                            variable_types = list(),
                            reference_levels = list(),
                            blocking_var = NULL) {

  warnings <- character(0)
  n_total <- nrow(cdesc)

  fail <- function(msg) {
    list(design = NULL, cdesc_clean = NULL, n_used = 0L, n_total = n_total,
         dropped = n_total, repeated_measures_only = FALSE,
         warnings = warnings, error = msg)
  }

  # Empty formula is valid ONLY with a blocking variable (repeated measures
  # without groups): the design is intercept-only. Otherwise it is an error.
  if (is.null(formula_string) || is.na(formula_string) || !nzchar(formula_string)) {
    if (!is.null(blocking_var)) {
      formula_string <- "~ 1"
    } else {
      return(fail("No predictor variables provided."))
    }
  }

  formula_obj <- tryCatch(
    stats::as.formula(formula_string),
    error = function(e) NULL
  )
  if (is.null(formula_obj)) {
    return(fail(paste0("Invalid formula string '", formula_string, "'.")))
  }
  model_vars <- all.vars(formula_obj)

  # A blocking variable must not also be a fixed effect.
  if (!is.null(blocking_var) && blocking_var %in% model_vars) {
    return(fail(paste0(
      "Blocking variable '", blocking_var, "' cannot also appear in the model ",
      "formula. Blocking variables model within-subject correlation as a random ",
      "effect; they should not be included as fixed effects."
    )))
  }

  # Coerce columns per variable_types, releveling factors to the chosen reference.
  cdesc_work <- cdesc
  for (var_name in names(variable_types)) {
    if (var_name %in% colnames(cdesc_work)) {
      if (variable_types[[var_name]] == "factor") {
        f <- factor(cdesc_work[[var_name]])
        ref <- reference_levels[[var_name]]
        if (!is.null(ref) && nzchar(as.character(ref))) {
          if (as.character(ref) %in% levels(f)) {
            f <- stats::relevel(f, ref = as.character(ref))
          } else {
            warnings <- c(warnings, paste0(
              "Ignoring reference level '", ref, "' for variable '", var_name,
              "': not present among observed levels (",
              paste(levels(f), collapse = ", "), ")."
            ))
          }
        }
        cdesc_work[[var_name]] <- f
      } else if (variable_types[[var_name]] == "continuous") {
        cdesc_work[[var_name]] <- as.numeric(as.character(cdesc_work[[var_name]]))
      }
    }
  }

  # Complete-case columns: formula vars PLUS the blocking var (if present).
  if (!is.null(blocking_var) && blocking_var %in% colnames(cdesc_work)) {
    model_vars_with_block <- unique(c(model_vars, blocking_var))
  } else {
    if (!is.null(blocking_var)) {
      warnings <- c(warnings, paste0(
        "Blocking variable '", blocking_var, "' not found in sample metadata. ",
        "Proceeding without blocking - results may be statistically incorrect."
      ))
    }
    model_vars_with_block <- model_vars
  }

  vars_for_complete <- if (length(model_vars) == 0 && !is.null(blocking_var) &&
                             blocking_var %in% colnames(cdesc_work)) {
    blocking_var
  } else {
    model_vars_with_block
  }

  complete_mask <- stats::complete.cases(
    cdesc_work[, vars_for_complete, drop = FALSE]
  )
  cdesc_clean <- cdesc_work[complete_mask, , drop = FALSE]
  n_used <- nrow(cdesc_clean)

  # Drop unused factor levels; a factor collapsing to one level is fatal.
  for (var_name in names(variable_types)) {
    if (variable_types[[var_name]] == "factor" &&
        var_name %in% colnames(cdesc_clean)) {
      cdesc_clean[[var_name]] <- droplevels(cdesc_clean[[var_name]])
      if (nlevels(cdesc_clean[[var_name]]) < 2) {
        return(fail(paste0(
          "Variable '", var_name, "' has only one level after filtering NAs. ",
          "Remove it from the model or choose a different dataset."
        )))
      }
    }
  }

  # Repeated-measures-without-groups: blocking var set, no formula predictors.
  repeated_measures_only <- length(model_vars) == 0 &&
    !is.null(blocking_var) &&
    blocking_var %in% colnames(cdesc_clean)

  design <- tryCatch(
    if (repeated_measures_only) {
      stats::model.matrix(~ 1, data = cdesc_clean)
    } else {
      stats::model.matrix(formula_obj, data = cdesc_clean)
    },
    error = function(e) NULL
  )
  if (is.null(design)) {
    return(fail("Could not build design matrix. Check variable types."))
  }

  # Rank-deficiency preflight: warn before limma produces silent NA coefficients.
  design_rank <- qr(design)$rank
  if (design_rank < ncol(design)) {
    warnings <- c(warnings, paste0(
      "Design matrix is rank-deficient (rank ", design_rank, " < ", ncol(design),
      " columns). Some coefficients will be NA. Consider removing redundant ",
      "variables or interactions."
    ))
  }

  list(
    design = design,
    cdesc_clean = cdesc_clean,
    n_used = n_used,
    n_total = n_total,
    dropped = n_total - n_used,
    repeated_measures_only = repeated_measures_only,
    warnings = warnings,
    error = NULL
  )
}


#' Run limma linear model regression on a GCT object
#'
#' @param gct A GCT object
#' @param formula_string Character string of the model formula
#' @param variable_types Named list mapping variable names to "factor" or "continuous"
#' @param blocking_var Optional blocking variable name (column in cdesc)
#' @param contrasts_list Optional list of contrast strings for makeContrasts
#' @param intensity Logical, whether data is intensity-based (enables eBayes trend)
#' @param reference_levels Named list mapping factor variables to the user's chosen
#'   reference level (string). Unknown variable names or unknown levels are ignored
#'   with a warning so the user is not silently surprised.
#' @return A data.frame with rdesc columns, per-coefficient stats, optional
#'   per-factor F-test columns (for variables with >1 non-intercept coefficient),
#'   and the original normalized sample values.
lm.regression <- function(gct,
                          formula_string,
                          variable_types = list(),
                          blocking_var = NULL,
                          contrasts_list = NULL,
                          intensity = FALSE,
                          reference_levels = list()) {

  # Ensure intensity is logical
  if (is.null(intensity)) {
    intensity <- FALSE
  } else if (is.character(intensity)) {
    intensity <- tolower(intensity) %in% c("yes", "true", "1", "t")
  } else {
    tmp <- as.logical(intensity)
    intensity <- if (is.na(tmp)) FALSE else tmp
  }

  mat <- gct@mat
  rdesc <- gct@rdesc
  cdesc <- gct@cdesc

  # Ensure id column exists in rdesc
  if (!"id" %in% colnames(rdesc)) {
    rdesc$id <- rownames(rdesc)
  }

  # Build the design matrix via the SHARED builder so the fit and the on-screen
  # preview cannot diverge. build_lm_design() performs formula normalization
  # (empty formula + blocking -> intercept-only), variable coercion + reference
  # releveling, complete-case dropping over formula vars PLUS the blocking var,
  # droplevels with a single-level guard, repeated-measures detection, and the
  # rank-deficiency check. It reports diagnostics as data; re-raise them here so
  # lm.regression keeps its original stop()/warning() contract.
  built <- build_lm_design(
    cdesc = cdesc,
    formula_string = formula_string,
    variable_types = variable_types,
    reference_levels = reference_levels,
    blocking_var = blocking_var
  )
  if (!is.null(built$error)) {
    stop(built$error)
  }
  for (w in built$warnings) warning(w)

  design <- built$design
  cdesc_clean <- built$cdesc_clean
  repeated_measures_only <- built$repeated_measures_only
  mat_clean <- mat[, rownames(cdesc_clean), drop = FALSE]

  block <- NULL
  correlation <- NULL

  if (repeated_measures_only) {
    # Repeated-measures without groups: blocking var is the subject ID.
    # duplicateCorrelation estimates within-subject correlation; lmFit accounts
    # for it via block + correlation. (The intercept-only design and the >=2
    # blocking-level guarantee both come from build_lm_design.)
    sampleRepeats <- droplevels(factor(cdesc_clean[[blocking_var]]))
    if (nlevels(sampleRepeats) < 2) {
      stop("Blocking variable '", blocking_var, "' has fewer than 2 levels after filtering; ",
           "cannot estimate within-subject correlation.")
    }
    dupcor <- limma::duplicateCorrelation(mat_clean, design, block = sampleRepeats)
    correlation <- dupcor$consensus.correlation
    block <- sampleRepeats
    fit <- limma::lmFit(mat_clean, design, block = block, correlation = correlation)
  } else {
    # Normal path: the shared design is already built and rank-checked.
    if (!is.null(blocking_var) && blocking_var %in% colnames(cdesc_clean)) {
      block <- droplevels(factor(cdesc_clean[[blocking_var]]))
      n_block_levels <- nlevels(block)
      if (n_block_levels < 2) {
        warning("Blocking variable '", blocking_var, "' has <2 levels after filtering; ",
                "ignoring blocking.")
        block <- NULL
      } else if (n_block_levels == length(block)) {
        warning("Blocking variable '", blocking_var, "' has all-unique values; ",
                "duplicateCorrelation will estimate ~0 and blocking will be a no-op.")
        dupcor <- limma::duplicateCorrelation(mat_clean, design, block = block)
        correlation <- dupcor$consensus.correlation
      } else {
        dupcor <- limma::duplicateCorrelation(mat_clean, design, block = block)
        correlation <- dupcor$consensus.correlation
      }
    }
    fit <- limma::lmFit(mat_clean, design, block = block, correlation = correlation)
  }

  # Snapshot the original design coefficient names BEFORE any contrast
  # re-parameterisation. We use these to (a) attribute design columns to their
  # source factor variable, and (b) decide which variables warrant a per-factor
  # F-test under reviewer option (c): emit an F-test only when the variable
  # contributes >1 non-intercept coefficient (multi-level factor or interaction).
  pre_contrast_coefs <- colnames(fit$coefficients)

  # Apply contrasts if provided.
  # makeContrasts() rejects any levels that aren't syntactically valid R names
  # (interaction terms contain ":", which is illegal). Work around this by:
  #   1. renaming both the design columns AND the fit's coefficient columns to
  #      make.names()-safe versions,
  #   2. applying make.names() to each whitespace-delimited token in the
  #      user-supplied contrast strings so they reference the renamed columns.
  # Then call makeContrasts on the safe-named design and contrasts.fit on the
  # renamed fit. The coefficient output downstream is keyed on contrast NAMES
  # (e.g., "C1"), not on the original level names, so nothing else breaks.
  if (!is.null(contrasts_list) && length(contrasts_list) > 0) {
    safe_levels <- make.names(colnames(fit$coefficients))
    fit_safe <- fit
    colnames(fit_safe$coefficients) <- safe_levels
    if (!is.null(fit_safe$stdev.unscaled)) {
      colnames(fit_safe$stdev.unscaled) <- safe_levels
    }
    design_safe <- design
    colnames(design_safe) <- safe_levels

    # Rename tokens inside contrast strings so they match the safe level names.
    rename_contrast_string <- function(s) {
      # Split on spaces or arithmetic operators, keeping delimiters so we can
      # reassemble identically (except renamed tokens).
      parts <- strsplit(s, "(?=[-+*/() ])|(?<=[-+*/() ])", perl = TRUE)[[1]]
      renamed <- vapply(parts, function(tok) {
        if (nzchar(tok) && !grepl("^[-+*/() ]+$", tok) &&
            suppressWarnings(is.na(as.numeric(tok)))) {
          make.names(tok)
        } else {
          tok
        }
      }, character(1))
      paste(renamed, collapse = "")
    }
    contrasts_safe <- vapply(contrasts_list, rename_contrast_string, character(1),
                              USE.NAMES = FALSE)

    contrast_matrix <- limma::makeContrasts(
      contrasts = contrasts_safe,
      levels = design_safe
    )
    # Preserve the contrast names (C1, C2, ...) from the user-supplied list.
    if (!is.null(names(contrasts_list))) {
      colnames(contrast_matrix) <- names(contrasts_list)
    }
    fit <- limma::contrasts.fit(fit_safe, contrast_matrix)
  }

  # eBayes with trend and robust fallbacks.
  # `robust = TRUE` (Phipson 2016) Winsorizes outlier variances; it needs enough
  # residual degrees of freedom. On near-saturated designs (n ~ p) the prior-df
  # estimation can fail. Fall back to `robust = FALSE` in that case rather than
  # crashing the whole fit.
  do_ebayes <- function(fit, trend, robust) {
    tryCatch(
      limma::eBayes(fit, trend = trend, robust = robust),
      error = function(e) {
        if (isTRUE(robust)) {
          warning(
            "eBayes with robust=TRUE failed (", conditionMessage(e),
            "). Falling back to robust=FALSE. This usually occurs on near-",
            "saturated designs (few residual degrees of freedom)."
          )
          limma::eBayes(fit, trend = trend, robust = FALSE)
        } else {
          stop(e)
        }
      }
    )
  }

  if (intensity) {
    fit <- tryCatch(
      do_ebayes(fit, trend = TRUE, robust = TRUE),
      error = function(e) {
        warning(
          "eBayes with trend=TRUE failed. Falling back to trend=FALSE. ",
          "This usually occurs when the distribution of detected features ",
          "is not uniform across samples."
        )
        do_ebayes(fit, trend = FALSE, robust = TRUE)
      }
    )
  } else {
    fit <- do_ebayes(fit, trend = FALSE, robust = TRUE)
  }

  # Determine which coefficients to extract.
  # Exclude "(Intercept)" when no contrasts are specified - it tests only the
  # grand mean vs. zero, which is not a differential comparison of interest.
  coef_names <- colnames(fit$coefficients)
  if (is.null(contrasts_list) || length(contrasts_list) == 0) {
    coef_names <- setdiff(coef_names, "(Intercept)")
  }
  # If filtering left nothing (e.g. intercept-only repeated-measures model),
  # fall back to all coefficients so at least the grand-mean is reported.
  if (length(coef_names) == 0) {
    coef_names <- colnames(fit$coefficients)
  }

  # Extract results per coefficient via topTable
  lm_model_list <- lapply(coef_names, function(coef_name) {
    lm_model <- limma::topTable(
      fit,
      coef = coef_name,
      adjust.method = "BH",
      number = Inf,
      sort.by = "none"
    )
    lm_model <- lm_model[, c("logFC", "P.Value", "adj.P.Val"), drop = FALSE]
    lm_model$id <- rownames(lm_model)
    p_val_clamped <- pmax(lm_model$P.Value, .Machine$double.xmin)
    # Note: when logFC == 0, sign(0) == 0 so logSignP will be 0 regardless of
    # the p-value. Features with exact zero fold-change will appear as
    # logSignP == 0, which does not imply non-significance.
    lm_model$logSignP <- -sign(lm_model$logFC) * log10(p_val_clamped)

    # Reorder to put id first
    lm_model <- lm_model[, c("id", "logFC", "P.Value", "adj.P.Val", "logSignP"), drop = FALSE]

    # Rename columns with coefficient suffix
    safe_coef <- make.names(coef_name)
    colnames(lm_model)[-1] <- paste(colnames(lm_model)[-1], safe_coef, sep = ".")

    lm_model
  })

  # Combine all coefficient results
  combined <- Reduce(
    function(x, y) dplyr::full_join(x, y, by = "id"),
    lm_model_list
  )

  # Per-factor F-tests (reviewer option (c)):
  # Only meaningful in the no-contrast path (where design coefficients are the
  # named columns of `pre_contrast_coefs`). In the contrast path, coefficients
  # are user-defined linear combinations and per-factor attribution is undefined.
  #
  # Trigger rule: emit a per-factor F-test for each variable in
  # `variable_types` (or for the special "<varA>:<varB>" interaction key) when
  # the number of associated non-intercept design coefficients is >= 2.
  if (is.null(contrasts_list) || length(contrasts_list) == 0) {
    factor_F_blocks <- build_factor_F_blocks(
      fit = fit,
      pre_contrast_coefs = pre_contrast_coefs,
      variable_types = variable_types
    )
    for (blk in factor_F_blocks) {
      combined <- dplyr::full_join(combined, blk, by = "id")
    }
  }

  # Join rdesc metadata
  rdesc_df <- as.data.frame(rdesc)
  if (!"id" %in% colnames(rdesc_df)) {
    rdesc_df$id <- rownames(rdesc_df)
  }
  combined <- dplyr::right_join(rdesc_df, combined, by = "id")

  # Join normalized values from mat
  normalized_df <- as.data.frame(mat)
  normalized_df$id <- rownames(normalized_df)
  combined <- dplyr::left_join(combined, normalized_df, by = "id")

  combined
}


# ---- internal helpers --------------------------------------------------------

# Attribute each non-intercept design coefficient to its source variable
# (or interaction). For a coefficient like `groupMUT:timeT3` we record it under
# the synthetic key `"group:time"`; for plain `groupMUT` under `"group"`.
attribute_design_coefs <- function(pre_contrast_coefs, variable_types) {
  coef_names <- setdiff(pre_contrast_coefs, "(Intercept)")
  var_names <- names(variable_types)
  if (length(var_names) == 0 || length(coef_names) == 0) return(list())

  # Longest-prefix match (so `treatmentXxx` matches `treatment`, not partial).
  match_var <- function(token, vars) {
    # Find vars that the token starts with; pick the longest.
    starts <- vars[startsWith(token, vars)]
    if (length(starts) == 0) return(NA_character_)
    starts[which.max(nchar(starts))]
  }

  groups <- list()
  for (coef in coef_names) {
    parts <- strsplit(coef, ":", fixed = TRUE)[[1]]
    matched <- vapply(parts, match_var, character(1), vars = var_names)
    if (any(is.na(matched))) {
      # Unrecognised coef (e.g. continuous variable name itself) - skip from
      # per-factor F-tests; per-coef t-test is already emitted.
      next
    }
    key <- paste(sort(unique(matched)), collapse = ":")
    groups[[key]] <- c(groups[[key]], coef)
  }
  groups
}


# Build one block per "factor" key whose coefficient group has >= 2 elements.
# Each block is a data frame with columns id, F.<key>, P.Value.<key>, adj.P.Val.<key>.
# `make.names()` is applied to the key for column suffixes (e.g. "group:time" -> "group.time").
build_factor_F_blocks <- function(fit, pre_contrast_coefs, variable_types) {
  groups <- attribute_design_coefs(pre_contrast_coefs, variable_types)
  blocks <- list()
  for (key in names(groups)) {
    coefs <- groups[[key]]
    if (length(coefs) < 2) next   # option (c): skip 1-coef variables
    # Confirm all named coefs are present in the fit object.
    coefs <- intersect(coefs, colnames(fit$coefficients))
    if (length(coefs) < 2) next
    tt <- limma::topTable(
      fit,
      coef = coefs,
      number = Inf,
      sort.by = "none",
      adjust.method = "BH"
    )
    # vector-coef topTable returns per-coef logFC columns we ignore (no single
    # signed effect for an F-test), plus AveExpr, F, P.Value, adj.P.Val.
    suffix <- make.names(key)
    blk <- data.frame(
      id = rownames(tt),
      F = tt$F,
      P.Value = tt$P.Value,
      adj.P.Val = tt$adj.P.Val,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    colnames(blk)[-1] <- paste(colnames(blk)[-1], suffix, sep = ".")
    blocks[[key]] <- blk
  }
  blocks
}
