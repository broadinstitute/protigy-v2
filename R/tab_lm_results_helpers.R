################################################################################
# Module: LM_Results Helpers
#
# Helper functions for the Linear Model Results module.
################################################################################

#' Get p-values for a specific coefficient from LM results
#'
#' @param ome Character, the ome name
#' @param lm_results Named list of LM result data frames, one entry per ome
#' @param coefficient Character, the coefficient name to extract
#' @param pval_type Character, either "adj.P.Val" (default) or "P.Value"
#' @return Numeric vector of p-values (NA removed)
get_lm_pvals <- function(ome, lm_results, coefficient, pval_type = c("adj.P.Val", "P.Value")) {
  pval_type <- match.arg(pval_type)

  df <- lm_results[[ome]]
  safe_coef <- make.names(coefficient)

  col_pattern <- paste0("^", gsub("\\.", "\\\\.", pval_type), "\\.", gsub("\\.", "\\\\.", safe_coef), "$")
  col_name <- grep(col_pattern, colnames(df), value = TRUE, perl = TRUE)[1]

  if (is.na(col_name)) {
    return(numeric(0))
  }

  pvals <- as.numeric(df[[col_name]])
  pvals[!is.na(pvals)]
}


#' Suggest an advisory alpha level via KS test on nominal p-value tail
#'
#' Advisory heuristic: nominal p-values are uniformly distributed under the null,
#' so for each candidate alpha in seq(0.01, 0.10, by=0.01), we test whether the
#' tail (p > alpha) is uniform on (alpha, 1) via a KS test. The lowest alpha
#' whose tail is not rejected is returned. Note: the returned value is advisory
#' only - it is NOT a principled FDR estimator. For a statistically rigorous
#' null-proportion estimate, see `qvalue::pi0est`.
#'
#' @param pvals Numeric vector of NOMINAL p-values (NAs already removed)
#' @return A named list: $alpha (numeric or NA), $message (character)
suggest_alpha_level <- function(pvals) {
  if (length(pvals) == 0) {
    return(list(alpha = NA, message = "No p-values available."))
  }
  levels <- seq(0.01, 0.10, by = 0.01)
  for (a in levels) {
    tail <- pvals[pvals > a]
    if (length(tail) < 2) next
    result <- suppressWarnings(ks.test(tail, "punif", min = a, max = 1))
    if (result$p.value > 0.05) {
      return(list(
        alpha = a,
        message = paste0("Suggested \u03b1 (advisory): ", a,
                         " (KS test p = ", signif(result$p.value, 3),
                         " on nominal p-value tail)")
      ))
    }
  }
  list(alpha = NA,
       message = "Alpha-level analysis inconclusive. Inspect the p-value histogram manually.")
}


#' Build a GCT object from an LM results frame, ready for SSGSEA.
#'
#' The `@mat` slot holds the `logSignP.<coef>` columns (one column per
#' coefficient); `@rdesc` carries everything else (annotations, raw sample
#' values, and the per-coef logFC / P.Value / adj.P.Val statistics). This
#' mirrors `R/tab_stat_summary.R:700-707` so downstream SSGSEA tooling can
#' consume LM outputs the same way it consumes stat-module outputs.
#'
#' @param df Data frame with at least an `id` column and one or more
#'   `logSignP.<coef>` columns.
#' @return A `cmapR::GCT` object, or `NULL` if no `logSignP` columns exist.
build_lm_stat_gct <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  sign_cols <- grep("^logSignP\\.", colnames(df), value = TRUE)
  if (length(sign_cols) == 0) return(NULL)
  if (!"id" %in% colnames(df)) {
    df$id <- as.character(seq_len(nrow(df)))
  }
  ids <- as.character(df$id)
  mat <- as.matrix(df[, sign_cols, drop = FALSE])
  rownames(mat) <- ids
  rdesc <- df[, setdiff(colnames(df), sign_cols), drop = FALSE]
  rownames(rdesc) <- ids
  methods::new("GCT", mat = mat, rdesc = data.frame(rdesc), rid = ids)
}


#' Write an LM-results GCT for SSGSEA consumption.
#'
#' @param df LM results frame.
#' @param dir_name Output directory.
#' @param ome Ome name; embedded in the filename.
#' @return Invisible path on success, NULL on no-op.
write_lm_stat_gct <- function(df, dir_name, ome) {
  gct <- build_lm_stat_gct(df)
  if (is.null(gct)) return(invisible(NULL))
  out <- file.path(dir_name, paste0("lm_stat_results_for_ssGSEA_", ome, ".gct"))
  cmapR::write_gct(gct, out, appenddim = FALSE)
  invisible(out)
}


#' Plot p-value histogram for a specific coefficient
#'
#' @param pvals Numeric vector of p-values
#' @param title Character, plot title
#' @param xlabel Character, x-axis label
#' @param lm_results Named list of LM result data frames, one entry per ome
#' @param lm_params Named list of LM parameters, one entry per ome
#' @param ome Character, the ome name
#' @param coefficient Character, the coefficient name
#' @param pval_type Character, either "adj.P.Val" or "P.Value"
#' @return A ggplot object
plot_lm_pval_histogram <- function(pvals, title, xlabel, lm_results, lm_params, ome, coefficient, pval_type) {
  df <- lm_results[[ome]]
  safe_coef <- make.names(coefficient)

  # Get both adj and nominal p-value columns for cutoff calculation
  adjP_pattern <- paste0("^adj\\.P\\.Val\\.", gsub("\\.", "\\\\.", safe_coef), "$")
  pval_pattern <- paste0("^P\\.Value\\.", gsub("\\.", "\\\\.", safe_coef), "$")

  adjP_col <- grep(adjP_pattern, colnames(df), value = TRUE, perl = TRUE)[1]
  pval_col <- grep(pval_pattern, colnames(df), value = TRUE, perl = TRUE)[1]

  if (is.na(adjP_col) || is.na(pval_col)) {
    return(ggplot() + labs(title = "No data available"))
  }

  df$adj.P.Val <- as.numeric(df[[adjP_col]])
  df$P.Value <- as.numeric(df[[pval_col]])

  stat_choice <- lm_params[[ome]]$stat
  cutoff_val <- lm_params[[ome]]$cutoff

  if (is.null(stat_choice) || is.na(stat_choice)) stat_choice <- "adj.p.val"
  if (is.null(cutoff_val) || is.na(cutoff_val)) cutoff_val <- 0.05

  # Calculate cutoff line position (same logic as stat module)
  if (pval_type == "P.Value") {
    if (stat_choice == "nom.p.val") {
      x_cutoff <- cutoff_val
    } else {
      passing.id <- which(df$adj.P.Val < cutoff_val)
      if (length(passing.id) > 0 && !all(is.na(df$P.Value[passing.id]))) {
        x_cutoff <- max(df$P.Value[passing.id], na.rm = TRUE)
      } else {
        x_cutoff <- cutoff_val
      }
    }
  } else {
    if (stat_choice == "adj.p.val") {
      x_cutoff <- cutoff_val
    } else {
      passing.id <- which(df$P.Value < cutoff_val)
      if (length(passing.id) > 0 && !all(is.na(df$adj.P.Val[passing.id]))) {
        x_cutoff <- min(df$adj.P.Val[passing.id], na.rm = TRUE)
      } else {
        x_cutoff <- cutoff_val
      }
    }
  }

  ggplot(data.frame(pval = pvals), aes(x = .data$pval)) +
    geom_histogram(breaks = seq(0, 1, by = 0.01), fill = "#d3d3d3", color = "#404040", linewidth = 0.2) +
    geom_vline(xintercept = x_cutoff, color = "red", linetype = "solid", linewidth = 0.2) +
    labs(title = title, x = xlabel, y = "Number of Features") +
    xlim(0, 1) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "#f5f5f5", linewidth = 0.3),
      panel.grid.minor = element_line(color = "#fafafa", linewidth = 0.2),
      plot.title = element_text(size = 10)
    )
}
