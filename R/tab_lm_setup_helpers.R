################################################################################
# Module: LM_Setup Helpers
#
# Backend computation logic for the Linear Model module.
# Builds design matrices, runs limma lmFit -> eBayes, extracts per-coefficient
# results via topTable.
################################################################################

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


#' Run limma linear model regression on a GCT object
#'
#' @param gct A GCT object
#' @param formula_string Character string of the model formula
#' @param variable_types Named list mapping variable names to "factor" or "continuous"
#' @param blocking_var Optional blocking variable name (column in cdesc)
#' @param contrasts_list Optional list of contrast strings for makeContrasts
#' @param intensity Logical, whether data is intensity-based (enables eBayes trend)
#' @return A data.frame with rdesc columns, per-coefficient stats, and normalized values
lm.regression <- function(gct,
                          formula_string,
                          variable_types = list(),
                          blocking_var = NULL,
                          contrasts_list = NULL,
                          intensity = FALSE) {

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

  # Allow empty formula when blocking_var is set (repeated measures without groups)
  if (is.null(formula_string) || is.na(formula_string) || !nzchar(formula_string)) {
    if (!is.null(blocking_var)) {
      formula_string <- "~ 1"  # intercept-only placeholder
    } else {
      stop("No predictor variables provided.")
    }
  }

  # Parse the formula to extract variable names
  formula_obj <- tryCatch(
    as.formula(formula_string),
    error = function(e) stop("Invalid formula string '", formula_string, "': ", e$message)
  )
  model_vars <- all.vars(formula_obj)

  # Validate: blocking variable cannot also be a fixed effect.
  # duplicateCorrelation treats the block as a random effect; a variable should
  # be either a fixed-effect predictor OR a blocking random effect, not both.
  if (!is.null(blocking_var) && blocking_var %in% model_vars) {
    stop(
      "Blocking variable '", blocking_var, "' cannot also appear in the model formula. ",
      "Blocking variables model within-subject correlation as a random effect; ",
      "they should not be included as fixed effects."
    )
  }

  # Coerce cdesc columns per variable_types
  cdesc_work <- cdesc
  for (var_name in names(variable_types)) {
    if (var_name %in% colnames(cdesc_work)) {
      if (variable_types[[var_name]] == "factor") {
        cdesc_work[[var_name]] <- factor(cdesc_work[[var_name]])
      } else if (variable_types[[var_name]] == "continuous") {
        cdesc_work[[var_name]] <- as.numeric(as.character(cdesc_work[[var_name]]))
      }
    }
  }

  # Include blocking variable for complete.cases only if it exists in cdesc
  if (!is.null(blocking_var) && blocking_var %in% colnames(cdesc_work)) {
    model_vars_with_block <- unique(c(model_vars, blocking_var))
  } else {
    if (!is.null(blocking_var)) {
      warning(
        "Blocking variable '", blocking_var, "' not found in sample metadata. ",
        "Proceeding without blocking — results may be statistically incorrect."
      )
    }
    model_vars_with_block <- model_vars
  }

  # When formula has no vars, complete.cases uses only blocking_var
  vars_for_complete <- if (length(model_vars) == 0 && !is.null(blocking_var) &&
                            blocking_var %in% colnames(cdesc_work)) {
    blocking_var
  } else {
    model_vars_with_block
  }

  # Remove samples with NA in any model variable (including blocking var)
  complete_mask <- complete.cases(cdesc_work[, vars_for_complete, drop = FALSE])
  cdesc_clean <- cdesc_work[complete_mask, , drop = FALSE]
  mat_clean <- mat[, rownames(cdesc_clean), drop = FALSE]

  # Drop unused factor levels that may remain after complete-case filtering,
  # and validate that each factor still has at least two levels.
  for (var_name in names(variable_types)) {
    if (variable_types[[var_name]] == "factor" &&
        var_name %in% colnames(cdesc_clean)) {
      cdesc_clean[[var_name]] <- droplevels(cdesc_clean[[var_name]])
      if (nlevels(cdesc_clean[[var_name]]) < 2) {
        stop(
          "Variable '", var_name, "' has only one level after filtering NAs. ",
          "Remove it from the model or choose a different dataset."
        )
      }
    }
  }

  # Detect repeated-measures-without-groups mode:
  # blocking var is set but no predictors in formula
  repeated_measures_only <- length(model_vars) == 0 &&
    !is.null(blocking_var) &&
    blocking_var %in% colnames(cdesc_clean)

  block <- NULL
  correlation <- NULL

  if (repeated_measures_only) {
    # Repeated-measures without groups: blocking var is the subject ID.
    # Use intercept-only design; duplicateCorrelation estimates within-subject
    # correlation; lmFit accounts for it via block + correlation.
    sampleRepeats <- droplevels(factor(cdesc_clean[[blocking_var]]))
    if (nlevels(sampleRepeats) < 2) {
      stop("Blocking variable '", blocking_var, "' has fewer than 2 levels after filtering; ",
           "cannot estimate within-subject correlation.")
    }
    design <- model.matrix(~ 1, data = cdesc_clean)
    dupcor <- limma::duplicateCorrelation(mat_clean, design, block = sampleRepeats)
    correlation <- dupcor$consensus.correlation
    block <- sampleRepeats
    fit <- limma::lmFit(mat_clean, design, block = block, correlation = correlation)
  } else {
    # Normal path: use formula_string design matrix
    design <- model.matrix(formula_obj, data = cdesc_clean)

    # Rank-deficiency preflight: warn before limma produces silent NA coefficients.
    design_rank <- qr(design)$rank
    if (design_rank < ncol(design)) {
      warning(
        "Design matrix is rank-deficient (rank ", design_rank, " < ", ncol(design),
        " columns). Some coefficients will be NA. Consider removing redundant ",
        "variables or interactions."
      )
    }

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

  # eBayes with trend fallback
  if (intensity) {
    fit <- tryCatch(
      {
        limma::eBayes(fit, trend = TRUE, robust = TRUE)
      },
      error = function(e) {
        warning(
          "eBayes with trend=TRUE failed. Falling back to trend=FALSE. ",
          "This usually occurs when the distribution of detected features ",
          "is not uniform across samples."
        )
        limma::eBayes(fit, trend = FALSE, robust = TRUE)
      }
    )
  } else {
    fit <- limma::eBayes(fit, robust = TRUE)
  }

  # Determine which coefficients to extract.
  # Exclude "(Intercept)" when no contrasts are specified — it tests only the
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
