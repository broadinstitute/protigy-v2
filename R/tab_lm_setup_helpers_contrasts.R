################################################################################
# Module: LM_Setup Contrast Builder Helpers
#
# Pure helpers for the redesigned contrast builder:
#   - id/label/expr three-field model for contrasts
#   - Auto-generate readable labels from numerator/denominator design coefs
#   - Strip common variable prefix so labels look like "Drug-Vehicle" rather
#     than "treatmentDrug-treatmentVehicle" when a shared prefix is detected.
#   - Direction sentence to explain the sign of log2FC
#
# Naming conventions (see plan):
#   "-"  between the two sides of a single-variable contrast (num minus den)
#   ":"  between primary and secondary variable levels in an interaction cell
#   "( )" wrap sub-contrasts when the secondary variable is differenced out
#   no whitespace anywhere in labels (safe for CSV/TSV parsing)
################################################################################


#' Strip a shared prefix between two design-coefficient tokens.
#'
#' Design columns from treatment contrasts look like "treatmentDrug",
#' "treatmentVehicle". When both numerator and denominator share the same
#' leading variable name, strip it so the label reads as "Drug-Vehicle".
#'
#' @param num Character scalar, numerator design-column name.
#' @param den Character scalar, denominator design-column name.
#' @return A length-2 character vector c(num_clean, den_clean).
strip_shared_prefix <- function(num, den) {
  if (!nzchar(num) || !nzchar(den)) return(c(num, den))
  # Find the longest common prefix consisting of letters/dots that both strings
  # share AND is followed by at least one character in each.
  n <- min(nchar(num), nchar(den))
  if (n == 0) return(c(num, den))
  common <- 0L
  for (i in seq_len(n)) {
    if (substr(num, i, i) == substr(den, i, i)) {
      common <- i
    } else {
      break
    }
  }
  if (common == 0L) return(c(num, den))
  # Only strip if both remainders are non-empty (otherwise prefix == whole
  # string, meaning the two tokens are identical or one is a prefix of the
  # other — don't strip in those cases).
  if (common >= nchar(num) || common >= nchar(den)) return(c(num, den))
  # Also guard: prefix must look like a variable name (letters, digits, dots,
  # underscores). Otherwise don't strip.
  prefix <- substr(num, 1L, common)
  if (!grepl("^[A-Za-z][A-Za-z0-9._]*$", prefix)) return(c(num, den))
  c(substr(num, common + 1L, nchar(num)),
    substr(den, common + 1L, nchar(den)))
}


#' Auto-generate a readable label from a numerator and denominator design coef.
#'
#' Rules:
#'   - strip shared prefix (e.g. "treatmentDrug"/"treatmentVehicle" -> "Drug"/"Vehicle")
#'   - join with "-" (per user naming convention)
#'   - strip any whitespace
#'
#' @param num Character scalar, numerator design-column name.
#' @param den Character scalar, denominator design-column name.
#' @return Character scalar label. Empty string if either input is empty.
make_simple_label <- function(num, den) {
  if (is.null(num) || is.null(den)) return("")
  if (!nzchar(num) || !nzchar(den)) return("")
  stripped <- strip_shared_prefix(num, den)
  lbl <- paste0(stripped[1], "-", stripped[2])
  gsub("\\s+", "", lbl)
}


#' Build a simple contrast expression string (num - den) for limma::makeContrasts.
#'
#' @param num Character scalar, numerator design-column name.
#' @param den Character scalar, denominator design-column name.
#' @return Character scalar, e.g. "treatmentDrug - treatmentVehicle". Empty if
#'   either input is empty.
build_simple_expr <- function(num, den) {
  if (is.null(num) || is.null(den)) return("")
  if (!nzchar(num) || !nzchar(den)) return("")
  paste(num, "-", den)
}


#' Direction sentence for a simple contrast.
#'
#' @param label Character scalar, the auto-generated label.
#' @param num Character scalar, numerator design-column name (original).
#' @param den Character scalar, denominator design-column name (original).
#' @return Character scalar, e.g. "Positive log2FC = higher in Drug than in Vehicle".
direction_sentence_simple <- function(label, num, den) {
  if (!nzchar(label)) return("")
  stripped <- strip_shared_prefix(num, den)
  paste0("Positive log2FC = higher in ", stripped[1], " than in ", stripped[2])
}


#' Validate that an advanced contrast expression references design coefs.
#'
#' Returns a list with `ok` (logical) and `message` (character). Splits the
#' expression on operators ( + - * / ( ) ) and checks that every token is
#' either a numeric literal, whitespace, an operator, or a known design coef
#' (after `make.names()` normalization, matching the backend path).
#'
#' @param expr Character scalar, user-supplied contrast expression.
#' @param design_coefs Character vector of design-matrix column names.
#' @return list(ok = logical, message = character, unknown = character)
validate_advanced_expr <- function(expr, design_coefs) {
  if (is.null(expr) || !nzchar(trimws(expr))) {
    return(list(ok = FALSE, message = "(empty expression)", unknown = character(0)))
  }
  if (length(design_coefs) == 0) {
    return(list(ok = FALSE, message = "Cannot validate: design matrix unavailable",
                unknown = character(0)))
  }
  # Tokenize: split on operators and whitespace, keep non-empty tokens
  tokens <- strsplit(expr, "[-+*/()\\s]+", perl = TRUE)[[1]]
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) == 0) {
    return(list(ok = FALSE, message = "No tokens parsed from expression",
                unknown = character(0)))
  }
  # Filter out numeric literals (weights like "0.5")
  is_num <- !is.na(suppressWarnings(as.numeric(tokens)))
  name_tokens <- tokens[!is_num]
  if (length(name_tokens) == 0) {
    return(list(ok = FALSE, message = "No design-coef tokens found",
                unknown = character(0)))
  }
  # The backend applies make.names() to both design and contrast tokens. We
  # mirror that normalization for validation.
  safe_coefs <- make.names(design_coefs)
  safe_tokens <- make.names(name_tokens)
  unknown <- name_tokens[!safe_tokens %in% safe_coefs]
  if (length(unknown) > 0) {
    return(list(
      ok = FALSE,
      message = paste0("Unknown coefficient(s): ", paste(unique(unknown), collapse = ", ")),
      unknown = unknown
    ))
  }
  list(ok = TRUE, message = paste0("valid (", length(name_tokens), " token(s))"),
       unknown = character(0))
}


#' Sanitize a user-provided label: strip whitespace, enforce non-empty.
#'
#' @param s Character scalar.
#' @return Character scalar with whitespace stripped.
sanitize_label <- function(s) {
  if (is.null(s)) return("")
  gsub("\\s+", "", as.character(s))
}


#' Enumerate all pairwise Simple contrast rows for a factor variable.
#'
#' Given the levels of a factor of interest and the design-column prefix used
#' for that factor (i.e. the variable name from the user's formula), returns a
#' list of row drafts ready to be appended to `contrast_rows`. Each draft is in
#' the same shape as a Simple-mode card: `id`, `type="simple"`, `num`, `den`,
#' `advanced_expr=""`, auto-label, `label_user_edited=FALSE`.
#'
#' The pairs are generated by `utils::combn(levels, 2)`: each unordered pair
#' (L_i, L_j) with i<j becomes a single contrast `L_i - L_j`. Users can swap
#' a card if they want the reversed direction.
#'
#' Design-coefficient names follow R's `model.matrix` convention: for an
#' intercept-included formula the coefficient corresponding to level `L` of
#' variable `V` is `paste0(V, L)`. We use that here without consulting the
#' design matrix; the caller is responsible for confirming the variable is in
#' the active design before calling this function.
#'
#' @param factor_levels Character vector of factor levels (length >= 2).
#' @param variable_name Character scalar, the formula variable name (e.g.
#'   "treatment").
#' @param include_intercept Logical; if FALSE the design uses a cell-means
#'   parameterisation (`~ 0 + V`), in which case the design column names are
#'   the bare level names (no `V` prefix). Defaults to TRUE.
#' @return A list of row drafts (each itself a list). Empty list if fewer than
#'   two levels.
enumerate_pairwise_simple_rows <- function(factor_levels,
                                            variable_name,
                                            include_intercept = TRUE) {
  if (is.null(factor_levels)) return(list())
  factor_levels <- as.character(factor_levels)
  factor_levels <- factor_levels[nzchar(factor_levels)]
  if (length(factor_levels) < 2) return(list())
  if (is.null(variable_name) || !nzchar(variable_name)) return(list())

  prefix <- if (isTRUE(include_intercept)) variable_name else ""
  pairs <- utils::combn(factor_levels, 2, simplify = FALSE)
  lapply(pairs, function(pair) {
    num <- paste0(prefix, pair[[1]])
    den <- paste0(prefix, pair[[2]])
    list(
      id = new_contrast_row_id(),
      type = "simple",
      num = num,
      den = den,
      advanced_expr = "",
      label = make_simple_label(num, den),
      label_user_edited = FALSE
    )
  })
}


#' Generate a fresh row id for contrast state.
#'
#' Uses the current time plus a random integer to avoid collisions across
#' quickly-successive clicks. Not globally unique — only unique within a
#' single Shiny session.
#' @return Character scalar id.
new_contrast_row_id <- function() {
  paste0("r", as.integer(Sys.time()), "_", sample.int(1e6, 1))
}
