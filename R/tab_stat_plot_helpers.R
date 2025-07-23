################################################################################
# Module: Stat_Plot
#
# Allow users to see the Volcano plot of their results
################################################################################

plotVolcano <- function(ome, volcano_groups, volcano_contrasts, df, sig.col='darkred', bg.col='gray'){
  cat('\n-- plotVolcano --\n')
  
  req(stat_param())
  req(stat_results())
  
  ##LOG FC COLUMN##
  if (stat_param()[[ome]]$test == "One-sample Moderated T-test") {
    keyword <- volcano_groups
    logfc_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "logFC.", ")")
  } else if (stat_param()[[ome]]$test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(volcano_contrasts, " / "))
    contrast_name <- paste0(groups[1], "_vs_", groups[2])
    logfc_pattern <- paste0("logFC.*", contrast_name)
  } 
  
  logFC_col <- grep(logfc_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  ##LOG P VALUE COLUMN##
  if (stat_param()[[ome]]$test == "One-sample Moderated T-test") {
    keyword <- volcano_groups
    logP_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "Log.P.Value.", ")")
  } else if (stat_param()[[ome]]$test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(volcano_contrasts, " / "))
    contrast_name <- paste0(groups[1], "_vs_", groups[2])
    logP_pattern  <- paste0("Log\\.P\\.Value.*", contrast_name)
  } 
  
  logP_col <- grep(logP_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  ##ADJ P VALUE COLUMN##
  if (stat_param()[[ome]]$test == "One-sample Moderated T-test") {
    keyword <- volcano_groups
    adjP_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "adj.P.Val.", ")")
  } else if (stat_param()[[ome]]$test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(volcano_contrasts, " / "))
    contrast_name <- paste0(groups[1], "_vs_", groups[2])
    adjP_pattern  <- paste0("adj\\.P\\.Val.*", contrast_name)
  } 
  
  adjP_col <- grep(adjP_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  ##P VAL COLUMN##
  if (stat_param()[[ome]]$test == "One-sample Moderated T-test") {
    keyword <- volcano_groups
    pval_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "P.value.", ")")
  } else if (stat_param()[[ome]]$test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(volcano_contrasts, " / "))
    contrast_name <- paste0(groups[1], "_vs_", groups[2])
    pval_pattern  <- paste0("P\\.value.*", contrast_name)
  }

  pval_col <- grep(pval_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]

  ##ID COLUMN##
  id_col <- grep("id", colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  ##GENE SYMBOL COLUMN##
  geneSymbol_col <- grep("geneSymbol", colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  ## Check columns exist
  required_cols <- c(logFC_col, logP_col, adjP_col, id_col)
  if(!all(required_cols %in% colnames(df))) {
    stop("Some required columns are missing in the result data.")
  }
  df <- df[complete.cases(df[, required_cols]), ]
  
  ## Add columns for plotting
  df$id <- df[[id_col]]
  df$geneSymbol <- df[[geneSymbol_col]]
  df$logFC <- df[[logFC_col]]
  df$adj.P.Val <- as.numeric(df[[adjP_col]])
  df$logP <- df[[logP_col]]
  df$P.Value <- as.numeric(df[[pval_col]])
  
  ## Define significance based on chosen stat and cutoff
  sig_cutoff <- stat_param()[[ome]]$cutoff
  sig_stat <- stat_param()[[ome]]$stat
  
  # Always use nominal p-values for Y-axis
  stat <- df$logP
  
  # Compute threshold for dashed line
  if(sig_stat == "adj.p.val") {
    passing.id <- which(df$adj.P.Val < sig_cutoff)
    if(length(passing.id) > 0){
      # Set y-axis threshold based on largest nominal p that still passes adj.p filter
      y_cutoff <- -log10(max(df$P.Value[passing.id], na.rm = TRUE))
    } else {
      y_cutoff <- Inf
    }
  } else {
    y_cutoff <- -log10(sig_cutoff)
  }
  
  df$Significant <- df$logP > y_cutoff

  ## Plot
  volcano <- ggplot(df, aes(x = logFC, y = stat, 
                       text = paste("ID:", id, "<br>Gene Symbol:", geneSymbol))) +
    geom_point(aes(color = Significant)) +
    scale_color_manual(values = c(`TRUE` = sig.col, `FALSE` = bg.col)) +
    geom_hline(yintercept = y_cutoff, color = "black", linetype = "dashed", size = 1) +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
    labs(title = paste("Volcano plot for",ome), x = "log2 Fold Change", y = "-log10 Nom. p-value") +
    theme_minimal()
  

  if (stat_param()[[ome]]$test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(volcano_contrasts, " / "))
    top_left <- groups[1]
    top_right <- groups[2]
    x_range <- range(df$logFC, na.rm = TRUE)
    y_range <- range(stat, na.rm = TRUE)
    
    volcano <- volcano +
      annotate("text", x = x_range[1], y = y_range[2], label = top_left, hjust = -0.1, vjust = 3.1, size = 5, fontface = "bold", color = "red") +
      annotate("text", x = x_range[2], y = y_range[2], label = top_right, hjust = 1.1, vjust = 3.1, size = 5, fontface = "bold", color = "red")
  }
  
  cat('\n-- plotVolcano finished--\n')
  return(volcano)
  
}