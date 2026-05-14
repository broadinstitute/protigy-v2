################################################################################
# tests/lm-sandbox/compare/assert_equivalent.R
#
# Tolerance-based comparison helpers + a self-check entry point.
#
# As a library: source this file from a testthat test and call
#   assert_per_coef_equivalent(actual, golden, tol = 1e-6)
# As a script: re-runs each `manual/run_*.R` and confirms the produced output
# matches the saved golden.
################################################################################

#' Compare two named lists of topTable-style frames for numerical equivalence.
#'
#' @param actual List of data frames keyed by coefficient name.
#' @param golden Same structure.
#' @param tol Absolute tolerance.
#' @return TRUE if equivalent; otherwise stops with a descriptive message.
assert_per_coef_equivalent <- function(actual, golden, tol = 1e-6) {
  if (!setequal(names(actual), names(golden))) {
    stop("coefficient name mismatch: actual=[", paste(names(actual), collapse = ","),
         "] golden=[", paste(names(golden), collapse = ","), "]")
  }
  for (nm in names(golden)) {
    a <- actual[[nm]]
    g <- golden[[nm]]
    common_cols <- intersect(colnames(a), colnames(g))
    common_cols <- intersect(common_cols, c("logFC", "AveExpr", "t", "P.Value",
                                            "adj.P.Val", "B", "F"))
    if (length(common_cols) == 0) {
      stop("no common numeric columns for coef '", nm, "'")
    }
    if (nrow(a) != nrow(g)) {
      stop("row count mismatch for coef '", nm, "': actual=", nrow(a), " golden=", nrow(g))
    }
    # Align by rownames if both present
    if (!is.null(rownames(a)) && !is.null(rownames(g)) &&
        !identical(rownames(a), rownames(g))) {
      a <- a[rownames(g), , drop = FALSE]
    }
    for (col in common_cols) {
      diffs <- abs(a[[col]] - g[[col]])
      max_diff <- max(diffs, na.rm = TRUE)
      if (!is.finite(max_diff) || max_diff > tol) {
        stop("coef '", nm, "' column '", col, "' max abs diff=", max_diff,
             " exceeds tol=", tol)
      }
    }
  }
  invisible(TRUE)
}


#' Compare a scalar correlation with relative tolerance.
assert_scalar_equivalent <- function(actual, golden, rel_tol = 1e-3) {
  denom <- max(abs(golden), .Machine$double.eps)
  if (abs(actual - golden) / denom > rel_tol) {
    stop("scalar mismatch: actual=", actual, " golden=", golden,
         " rel_diff=", abs(actual - golden) / denom, " tol=", rel_tol)
  }
  invisible(TRUE)
}


# ---- Self-check entry point --------------------------------------------------
#
# Run each manual/run_*.R script in a temp directory (writing to its own
# golden path) and assert the regenerated output matches the committed golden.
# This catches accidental modifications to the manual scripts.

self_check <- function() {
  scripts <- c("run_type1.R", "run_type2.R", "run_type3.R",
               "run_continuous.R", "run_intensity_trend.R")
  goldens <- c("type1_rm_with_groups", "type2_rm_only", "type3_contrasts",
               "continuous_covariate", "intensity_trend")
  for (i in seq_along(scripts)) {
    script <- file.path("tests", "lm-sandbox", "manual", scripts[i])
    golden_path <- file.path("tests", "lm-sandbox", "golden", paste0(goldens[i], ".rds"))
    if (!file.exists(golden_path)) {
      stop("missing golden ", golden_path, " — run the manual script first.")
    }
    # Just confirm the script can re-source without error; the script overwrites
    # the golden each time. To make this idempotent, we make a backup, re-run,
    # and compare.
    tmp <- tempfile(fileext = ".rds")
    file.copy(golden_path, tmp, overwrite = TRUE)
    source(script, chdir = FALSE)
    new <- readRDS(golden_path)
    saved <- readRDS(tmp)
    file.copy(tmp, golden_path, overwrite = TRUE)  # restore committed copy
    unlink(tmp)
    # Compare per_coef sections (others vary in structure across fixtures).
    if (!is.null(new$per_coef) && !is.null(saved$per_coef)) {
      assert_per_coef_equivalent(new$per_coef, saved$per_coef, tol = 1e-12)
    }
    if (!is.null(new$consensus) && !is.null(saved$consensus)) {
      assert_scalar_equivalent(new$consensus, saved$consensus, rel_tol = 1e-12)
    }
    cat("  OK  ", basename(golden_path), "\n")
  }
  cat("self_check: all goldens regenerated identically.\n")
  invisible(TRUE)
}

if (!interactive() && sys.nframe() == 0L) {
  self_check()
}
