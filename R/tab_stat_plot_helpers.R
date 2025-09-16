################################################################################
# Module: Stat_Plot
#
# Allow users to see the Volcano plot of their results
################################################################################

# #Input parameters- 
# ome- ome that plot is run on
# volcano_groups- current group selected in the plot sidebar
# volcano_contrasts- current contrast selected in the plot sidebar
# df- stat_results of selected ome
# sig.col- color of significant points
# bg.col- color of non significant points

plotVolcano <- function(ome, volcano_groups, volcano_contrasts, df, stat_params, stat_results, sig.col='darkred', bg.col='gray',gene_symbol_col = "geneSymbol"){
  
  cat('\n-- plotVolcano --\n')
  
  req(stat_params())
  req(stat_results())
  
  ##LOG FC COLUMN##
  if (stat_params()[[ome]]$test == "One-sample Moderated T-test") {
    keyword <- volcano_groups
    logfc_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "logFC.", ")")
  } else if (stat_params()[[ome]]$test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(volcano_contrasts, " / "))
    contrast_name <- paste0(groups[1], "_vs_", groups[2])
    logfc_pattern <- paste0("logFC.*", contrast_name)
  } 
  
  logFC_col <- grep(logfc_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  
  ##LOG P VALUE COLUMN##
  if (stat_params()[[ome]]$test == "One-sample Moderated T-test") {
    keyword <- volcano_groups
    logP_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "Log.P.Value.", ")")
  } else if (stat_params()[[ome]]$test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(volcano_contrasts, " / "))
    contrast_name <- paste0(groups[1], "_vs_", groups[2])
    logP_pattern  <- paste0("Log\\.P\\.Value.*", contrast_name)
  } 
  
  logP_col <- grep(logP_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  
  ##ADJ P VALUE COLUMN##
  if (stat_params()[[ome]]$test == "One-sample Moderated T-test") {
    keyword <- volcano_groups
    adjP_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "adj.P.Val.", ")")
  } else if (stat_params()[[ome]]$test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(volcano_contrasts, " / "))
    contrast_name <- paste0(groups[1], "_vs_", groups[2])
    adjP_pattern  <- paste0("adj\\.P\\.Val.*", contrast_name)
  } 
  
  adjP_col <- grep(adjP_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  ##P VAL COLUMN##
  if (stat_params()[[ome]]$test == "One-sample Moderated T-test") {
    keyword <- volcano_groups
    pval_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "P.value.", ")")
  } else if (stat_params()[[ome]]$test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(volcano_contrasts, " / "))
    contrast_name <- paste0(groups[1], "_vs_", groups[2])
    pval_pattern  <- paste0("P\\.value.*", contrast_name)
  }

  pval_col <- grep(pval_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]

  ##ID COLUMN##
  id_col <- grep("id", colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  ##GENE SYMBOL COLUMN##
  #make a parameter so this could be user-specified
  geneSymbol_col <- tryCatch({
    grep(gene_symbol_col, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  }, error = function(e) {
    NULL
  })
  
  ## Check columns exist
  required_cols <- c(logFC_col, logP_col, adjP_col, id_col)
  if(!all(required_cols %in% colnames(df))) {
    stop("Some required columns are missing in the result data.")
  }
  
  
  # Only filter out rows where logP is NA (essential for volcano plot)
  # Keep rows with NA in other columns as they might be legitimately untested
  df <- df[!is.na(df[[logP_col]]), ]
  
  ## Add columns for plotting
  df$id <- df[[id_col]]
  df$logFC <- df[[logFC_col]]
  df$adj.P.Val <- as.numeric(df[[adjP_col]])
  df$logP <- df[[logP_col]]
  df$P.Value <- as.numeric(df[[pval_col]])
  
  # Handle geneSymbol column - create it if it exists, otherwise use ID
  if (!is.null(geneSymbol_col) && !is.na(geneSymbol_col)) {
    df$geneSymbol <- df[[geneSymbol_col]]
  } else {
    # If no geneSymbol column, use ID as fallback
    df$geneSymbol <- df$id
  }
  
  ## Define significance based on chosen stat and cutoff
  sig_cutoff <- stat_params()[[ome]]$cutoff
  sig_stat <- stat_params()[[ome]]$stat
  
  # Always use nominal p-values for Y-axis
  # stat <- df$logP
  
  # Compute threshold for dashed line
  if(sig_stat == "adj.p.val") {
    passing.id <- which(df$adj.P.Val < sig_cutoff)
    if(length(passing.id) > 0){
      # Set y-axis threshold based on maximum nominal p-value among features that pass adj.p filter
      # This corresponds to the logic used in the original ProTIGY volcano plot
      y_cutoff <- -log10(max(df$P.Value[passing.id], na.rm = TRUE))
    } else {
      y_cutoff <- Inf
    }
  } else {
    y_cutoff <- -log10(sig_cutoff)
  }
  
  df$Significant <- df$logP > y_cutoff

  if (stat_params()[[ome]]$test == "Two-sample Moderated T-test"){
    group_contrast<- volcano_contrasts
  } else if (stat_params()[[ome]]$test == "One-sample Moderated T-test") {
    group_contrast<- volcano_groups
  }
  ## Plot
  volcano <- ggplot(df, aes(x = .data$logFC, y = .data$logP, 
                       text = paste("ID:", .data$id, "<br>Gene Symbol:", .data$geneSymbol))) +
    geom_point(aes(color = .data$Significant), size = 1) +
    scale_color_manual(values = c('TRUE' = sig.col, 'FALSE' = bg.col)) +
    geom_hline(yintercept = y_cutoff, color = "black", linetype = "solid", size = 0.5) +
    labs(title = paste("Volcano plot for",ome, ": ",group_contrast, "(cutoff:", stat_params()[[ome]]$cutoff, ")"), x = "log2 Fold Change", y = "-log10 Nom. p-value") +
    theme_minimal()
  

  if (stat_params()[[ome]]$test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(volcano_contrasts, " / "))
    # The log fold change is now calculated as group1 - group2 (where group1 is the first group in contrast)
    # So positive logFC means higher expression in group1, negative logFC means higher expression in group2
    group1 <- groups[1]  # First group in contrast (right side of volcano plot for positive logFC)
    group2 <- groups[2]  # Second group in contrast (left side of volcano plot for negative logFC)
    x_range <- range(df$logFC, na.rm = TRUE)
    y_range <- range(df$logP, na.rm = TRUE)
    
    volcano <- volcano +
      annotate("text", x = x_range[1], y = y_range[2], label = group2, hjust = -0.1, vjust = 3.1, size = 5, fontface = "bold", color = "red") +
      annotate("text", x = x_range[2], y = y_range[2], label = group1, hjust = 1.1, vjust = 3.1, size = 5, fontface = "bold", color = "red")
  }
  
  return(volcano)
  
}