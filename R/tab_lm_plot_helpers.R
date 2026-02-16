################################################################################
# Module: LM_Plot Helpers
#
# Volcano plot function for the Linear Model module.
################################################################################

#' Create a volcano plot for a specific coefficient from LM results
#'
#' @param ome Character, the ome name
#' @param coefficient Character, the coefficient name
#' @param df Data.frame of LM results for this ome
#' @param lm_params List of LM parameters (from reactiveVal)
#' @param sig.col Color for significant points (default "darkred")
#' @param bg.col Color for non-significant points (default "gray")
#' @param gene_symbol_col Column name for gene symbols (default "geneSymbol")
#' @return A ggplot object
plotLmVolcano <- function(ome,
                          coefficient,
                          df,
                          lm_params,
                          sig.col = "darkred",
                          bg.col = "gray",
                          gene_symbol_col = "geneSymbol") {

  safe_coef <- make.names(coefficient)

  # Find columns by pattern
  logfc_pattern <- paste0("^logFC\\.", gsub("\\.", "\\\\.", safe_coef), "$")
  adjP_pattern <- paste0("^adj\\.P\\.Val\\.", gsub("\\.", "\\\\.", safe_coef), "$")
  pval_pattern <- paste0("^P\\.Value\\.", gsub("\\.", "\\\\.", safe_coef), "$")

  logFC_col <- grep(logfc_pattern, colnames(df), value = TRUE, perl = TRUE)[1]
  adjP_col <- grep(adjP_pattern, colnames(df), value = TRUE, perl = TRUE)[1]
  pval_col <- grep(pval_pattern, colnames(df), value = TRUE, perl = TRUE)[1]

  id_col <- "id"

  # Fall back to rownames if id column doesn't exist
  if (!(id_col %in% colnames(df))) {
    df$id <- rownames(df)
  }

  # Gene symbol column — use case-insensitive literal match via tolower()
  geneSymbol_col <- tryCatch({
    matches <- colnames(df)[tolower(colnames(df)) == tolower(gene_symbol_col)]
    if (length(matches) > 0) matches[1] else NA_character_
  }, error = function(e) {
    NULL
  })

  # Check required columns
  if (is.na(logFC_col) || is.na(adjP_col) || is.na(pval_col)) {
    return(
      ggplot() +
        labs(title = paste("Missing columns for coefficient:", coefficient)) +
        theme_minimal()
    )
  }

  # Filter out NA rows
  df <- df[!is.na(df[[pval_col]]), ]

  # Add plotting columns
  df$id <- df[[id_col]]
  df$logFC <- as.numeric(df[[logFC_col]])
  df$adj.P.Val <- as.numeric(df[[adjP_col]])
  df$P.Value <- as.numeric(df[[pval_col]])
  df$logP <- -log10(pmax(df$P.Value, .Machine$double.xmin))

  # Gene symbol
  if (!is.null(geneSymbol_col) && !is.na(geneSymbol_col)) {
    df$geneSymbol <- df[[geneSymbol_col]]
  } else {
    df$geneSymbol <- df$id
  }

  # Significance threshold
  if (is.null(lm_params[[ome]])) {
    return(ggplot() + labs(title = paste("No parameters found for ome:", ome)) + theme_minimal())
  }

  sig_cutoff <- lm_params[[ome]]$cutoff
  sig_stat <- lm_params[[ome]]$stat

  if (is.null(sig_cutoff) || is.na(sig_cutoff) || !is.numeric(sig_cutoff) ||
      sig_cutoff <= 0 || sig_cutoff >= 1) {
    sig_cutoff <- 0.05
  }
  if (is.null(sig_stat) || is.na(sig_stat) || !nzchar(sig_stat)) {
    sig_stat <- "adj.p.val"
  }

  if (sig_stat == "adj.p.val") {
    passing.id <- which(df$adj.P.Val < sig_cutoff)
    if (length(passing.id) > 0) {
      y_cutoff <- -log10(max(df$P.Value[passing.id], na.rm = TRUE))
    } else {
      y_cutoff <- NA_real_
    }
  } else {
    y_cutoff <- -log10(sig_cutoff)
  }

  df$Significant <- factor(
    if (!is.na(y_cutoff)) df$logP >= y_cutoff else FALSE,
    levels = c(FALSE, TRUE)
  )

  # Plot
  volcano <- ggplot(df, aes(x = .data$logFC, y = .data$logP,
                        text = paste("ID:", .data$id, "<br>Gene Symbol:", .data$geneSymbol))) +
    geom_point(aes(color = .data$Significant), size = 1) +
    scale_color_manual(values = c("TRUE" = sig.col, "FALSE" = bg.col)) +
    labs(
      title = paste("Volcano plot for", ome, ":", coefficient,
                     "(cutoff:", sig_cutoff, ")"),
      x = "log2 Fold Change / Effect Size",
      y = "-log10 Nom. p-value"
    ) +
    theme_minimal()

  if (!is.na(y_cutoff)) {
    volcano <- volcano +
      geom_hline(yintercept = y_cutoff, color = "black", linetype = "solid", linewidth = 0.5)
  }

  return(volcano)
}
