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


#' Plot p-value histogram for a specific coefficient
#'
#' @param pvals Numeric vector of p-values
#' @param title Character, plot title
#' @param xlabel Character, x-axis label
#' @param lm_results List of LM results
#' @param lm_params List of LM parameters
#' @param ome Character, the ome name
#' @param coefficient Character, the coefficient name
#' @param pval_type Character, either "adj.P.Val" or "P.Value"
#' Suggest optimal alpha level via KS test on adjusted p-value tail
#'
#' Replicates the alpha-level analysis from LinearModelApp_v2.
#' For each candidate alpha in seq(0.01, 0.10, by=0.01), tests whether
#' the tail of adj p-values (p > alpha) is uniform using a KS test against
#' Uniform(alpha, 1) — the correct reference for the conditional tail.
#' Returns the lowest alpha where ks.test p-value > 2e-16.
#'
#' @param adj_pvals Numeric vector of adjusted p-values (NAs already removed)
#' @return A named list: $alpha (numeric or NA), $message (character)
suggest_alpha_level <- function(adj_pvals) {
  if (length(adj_pvals) == 0) {
    return(list(alpha = NA, message = "No adjusted p-values available."))
  }
  levels <- seq(0.01, 0.10, by = 0.01)
  for (a in levels) {
    tail <- adj_pvals[adj_pvals > a]
    if (length(tail) < 2) next
    result <- suppressWarnings(ks.test(tail, "punif", min = a, max = 1))
    if (result$p.value > 2e-16) {
      return(list(
        alpha = a,
        message = paste0("Recommended alpha: ", a,
                         " (KS test p = ", signif(result$p.value, 3), ")")
      ))
    }
  }
  list(alpha = NA,
       message = "Alpha-level analysis inconclusive. Check p-value distribution manually.")
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
