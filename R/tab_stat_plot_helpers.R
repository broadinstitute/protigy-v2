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

plotVolcano <- function(ome, volcano_groups, volcano_contrasts, df, stat_params, stat_results,
                        sig.col = 'darkred', bg.col = 'gray', gene_symbol_col = "geneSymbol",
                        label_proteins = character(0), label_mode = character(0)) {
  
  cat('\n-- plotVolcano --\n')
  
  req(stat_params())
  req(stat_results())
  
  ##LOG FC COLUMN##
  if (stat_params()[[ome]]$test == "One-sample Moderated T-test") {
    keyword <- volcano_groups
    logfc_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "logFC.", ")")
  } else if (stat_params()[[ome]]$test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(volcano_contrasts, " / "))
    contrast_name <- paste0(groups[1], "_over_", groups[2])
    logfc_pattern <- paste0("logFC.*", contrast_name)
  } 
  
  logFC_col <- grep(logfc_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  
  ##LOG P VALUE COLUMN##
  if (stat_params()[[ome]]$test == "One-sample Moderated T-test") {
    keyword <- volcano_groups
    logP_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "Log.P.Value.", ")")
  } else if (stat_params()[[ome]]$test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(volcano_contrasts, " / "))
    contrast_name <- paste0(groups[1], "_over_", groups[2])
    logP_pattern  <- paste0("Log\\.P\\.Value.*", contrast_name)
  } 
  
  logP_col <- grep(logP_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  
  ##ADJ P VALUE COLUMN##
  if (stat_params()[[ome]]$test == "One-sample Moderated T-test") {
    keyword <- volcano_groups
    adjP_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "adj.P.Val.", ")")
  } else if (stat_params()[[ome]]$test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(volcano_contrasts, " / "))
    contrast_name <- paste0(groups[1], "_over_", groups[2])
    adjP_pattern  <- paste0("adj\\.P\\.Val.*", contrast_name)
  } 
  
  adjP_col <- grep(adjP_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  ##P VAL COLUMN##
  if (stat_params()[[ome]]$test == "One-sample Moderated T-test") {
    keyword <- volcano_groups
    pval_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "P.value.", ")")
  } else if (stat_params()[[ome]]$test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(volcano_contrasts, " / "))
    contrast_name <- paste0(groups[1], "_over_", groups[2])
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
    geom_hline(yintercept = y_cutoff, color = "black", linetype = "solid", linewidth = 0.5) +
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
  
  # Add ggrepel labels for PDF export (mirrors add_volcano_labels color logic)
  if (length(label_mode) > 0) {
    label_df_gg <- data.frame(
      id = character(0), logFC = numeric(0), logP = numeric(0),
      label_txt = character(0), label_col = character(0),
      stringsAsFactors = FALSE
    )

    if ("significant" %in% label_mode) {
      sig_rows <- df[!is.na(df$Significant) & df$Significant == TRUE, ]
      if (nrow(sig_rows) > 0) {
        label_df_gg <- rbind(label_df_gg, data.frame(
          id = sig_rows$id, logFC = sig_rows$logFC, logP = sig_rows$logP,
          label_txt = sig_rows$geneSymbol, label_col = "#FF00FF",
          stringsAsFactors = FALSE
        ))
      }
    }

    if ("poi" %in% label_mode && length(label_proteins) > 0) {
      poi_rows <- df[df$id %in% label_proteins, ]
      if (nrow(poi_rows) > 0) {
        label_df_gg <- label_df_gg[!label_df_gg$id %in% poi_rows$id, ]
        label_df_gg <- rbind(label_df_gg, data.frame(
          id = poi_rows$id, logFC = poi_rows$logFC, logP = poi_rows$logP,
          label_txt = poi_rows$geneSymbol, label_col = "#28a745",
          stringsAsFactors = FALSE
        ))
      }
    }

    if (nrow(label_df_gg) > 0) {
      volcano <- volcano +
        geom_point(
          data        = label_df_gg,
          aes(x = .data$logFC, y = .data$logP),
          inherit.aes = FALSE,
          color       = label_df_gg$label_col,
          size        = 2,
          show.legend = FALSE
        ) +
        ggrepel::geom_text_repel(
          data          = label_df_gg,
          aes(x = .data$logFC, y = .data$logP, label = .data$label_txt),
          inherit.aes   = FALSE,
          color         = label_df_gg$label_col,
          size          = 3,
          max.overlaps  = 20,
          box.padding   = 0.35,
          point.padding = 0.3,
          segment.color = "grey50",
          show.legend   = FALSE,
          bg.color      = "white",
          bg.r          = 0.15
        )
    }
  }

  return(volcano)

}


################################################################################
# Protein Search & Labeling Helpers
################################################################################

# Resolve the stat-result column names needed for a given test/group/contrast.
# Returns a named list: logfc_col, logp_col, adjp_col, pval_col, id_col, gs_col
# df: stat_results()[[ome]]
# test: one of "One-sample Moderated T-test" / "Two-sample Moderated T-test"
# volcano_groups: group name for one-sample test (or NULL)
# volcano_contrasts: contrast string "A / B" for two-sample test (or NULL)
get_volcano_cols <- function(df, test, volcano_groups, volcano_contrasts) {
  if (test == "One-sample Moderated T-test") {
    keyword    <- volcano_groups
    logfc_col  <- grep(paste0("(?i)(?=.*", keyword, ")(?=.*logFC\\.)"),       colnames(df), value = TRUE, perl = TRUE)[1]
    logp_col   <- grep(paste0("(?i)(?=.*", keyword, ")(?=.*Log\\.P\\.Value\\.)"), colnames(df), value = TRUE, perl = TRUE)[1]
    adjp_col   <- grep(paste0("(?i)(?=.*", keyword, ")(?=.*adj\\.P\\.Val\\.)"),   colnames(df), value = TRUE, perl = TRUE)[1]
    pval_col   <- grep(paste0("(?i)(?=.*", keyword, ")(?=.*P\\.value\\.)"),        colnames(df), value = TRUE, perl = TRUE)[1]
  } else {
    groups        <- unlist(strsplit(as.character(volcano_contrasts), " / "))
    contrast_name <- paste0(groups[1], "_over_", groups[2])
    logfc_col  <- grep(paste0("logFC.*", contrast_name),          colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
    logp_col   <- grep(paste0("Log\\.P\\.Value.*", contrast_name), colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
    adjp_col   <- grep(paste0("adj\\.P\\.Val.*", contrast_name),   colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
    pval_col   <- grep(paste0("P\\.value.*", contrast_name),       colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  }

  id_col <- grep("^id$", colnames(df), value = TRUE, ignore.case = TRUE)[1]
  gs_col <- tryCatch(
    grep("geneSymbol", colnames(df), value = TRUE, ignore.case = TRUE)[1],
    error = function(e) NA_character_
  )

  list(logfc = logfc_col, logp = logp_col, adjp = adjp_col,
       pval = pval_col, id = id_col, gs = gs_col)
}


# Build the standardized plotting data frame from raw stat_results for a given
# test/group/contrast. Returns df with columns: id, logFC, logP, adj.P.Val,
# P.Value, geneSymbol, Significant.
build_volcano_df <- function(df_raw, cols, sig_cutoff, sig_stat) {
  # Validate required columns exist
  required <- c(cols$logfc, cols$logp, cols$adjp, cols$pval, cols$id)
  missing  <- required[is.na(required) | !required %in% colnames(df_raw)]
  if (length(missing) > 0) {
    stop("Missing required volcano columns: ", paste(missing, collapse = ", "))
  }

  df <- df_raw[!is.na(df_raw[[cols$logp]]), ]
  df$id        <- as.character(df[[cols$id]])
  df$logFC     <- df[[cols$logfc]]
  df$logP      <- df[[cols$logp]]
  df$adj.P.Val <- as.numeric(df[[cols$adjp]])
  df$P.Value   <- as.numeric(df[[cols$pval]])
  df$geneSymbol <- if (!is.na(cols$gs) && cols$gs %in% colnames(df_raw)) df_raw[!is.na(df_raw[[cols$logp]]), cols$gs] else df$id

  # Compute significance threshold
  if (sig_stat == "adj.p.val") {
    passing_id <- which(df$adj.P.Val < sig_cutoff)
    y_cutoff <- if (length(passing_id) > 0) -log10(max(df$P.Value[passing_id], na.rm = TRUE)) else Inf
  } else {
    y_cutoff <- -log10(sig_cutoff)
  }
  df$Significant <- df$logP > y_cutoff

  attr(df, "y_cutoff") <- y_cutoff
  df
}

# Returns the feature ID closest to a plotly click event point.
# click: list with $x (logFC) and $y (logP) from event_data("plotly_click")
# df: data frame with columns logFC, logP, id
# tol: maximum Euclidean distance threshold (in data coordinates) to match
get_clicked_feature_id <- function(click, df, tol = 0.01) {
  if (nrow(df) == 0 || is.null(click$x) || is.null(click$y)) {
    return(NA_character_)
  }

  dx   <- abs(df$logFC - click$x)
  dy   <- abs(df$logP  - click$y)
  dist <- sqrt(dx^2 + dy^2)

  idx <- which.min(dist)

  # Reject if beyond tolerance (Euclidean distance threshold = tol * sqrt(2))
  if (dist[idx] > tol * sqrt(2)) {
    return(NA_character_)
  }

  as.character(df$id[idx])
}


# Tokenize a raw search string into a character vector of protein IDs.
# Accepts space, comma, semicolon, and newline as delimiters. Drops empty tokens.
parse_protein_search_input <- function(raw) {
  if (is.null(raw) || nchar(trimws(raw)) == 0) {
    return(character(0))
  }
  tokens <- unlist(strsplit(raw, "[,;\n\r\\s]+", perl = TRUE))
  tokens <- trimws(tokens)
  tokens[nchar(tokens) > 0]
}


# Add color-coded protein labels as Plotly annotations.
#
# p               - plotly object (output of ggplotly)
# df              - data frame with columns: id, logFC, logP, Significant, geneSymbol
# poi             - character vector of feature IDs to label as POI (green)
# label_mode      - character vector; "poi" and/or "significant"
# y_cutoff        - significance y threshold (used to identify Significant points)
# hidden_count_rv - reactiveVal or mock_rv; updated with count of hidden labels
# min_dist        - minimum normalized distance between labels (0 to 1 scale)
add_volcano_labels <- function(p, df, poi, label_mode, y_cutoff,
                                hidden_count_rv, min_dist = 0.04) {

  show_poi <- "poi" %in% label_mode
  show_sig <- "significant" %in% label_mode

  if (!show_poi && !show_sig) {
    hidden_count_rv(0L)
    return(p)
  }

  # Build label data frame
  label_df <- data.frame(
    id        = character(0),
    logFC     = numeric(0),
    logP      = numeric(0),
    label_col = character(0),
    label_txt = character(0),
    stringsAsFactors = FALSE
  )

  if (show_sig) {
    sig_rows <- df[!is.na(df$Significant) & df$Significant == TRUE, ]
    if (nrow(sig_rows) > 0) {
      label_df <- rbind(label_df, data.frame(
        id        = sig_rows$id,
        logFC     = sig_rows$logFC,
        logP      = sig_rows$logP,
        label_col = "#FF00FF",
        label_txt = if (!is.null(sig_rows$geneSymbol)) sig_rows$geneSymbol else sig_rows$id,
        stringsAsFactors = FALSE
      ))
    }
  }

  if (show_poi && length(poi) > 0) {
    poi_rows <- df[df$id %in% poi, ]
    if (nrow(poi_rows) > 0) {
      # POI takes priority — remove any existing sig entries for the same IDs
      label_df <- label_df[!label_df$id %in% poi_rows$id, ]
      label_df <- rbind(label_df, data.frame(
        id        = poi_rows$id,
        logFC     = poi_rows$logFC,
        logP      = poi_rows$logP,
        label_col = "#28a745",
        label_txt = if (!is.null(poi_rows$geneSymbol)) poi_rows$geneSymbol else poi_rows$id,
        stringsAsFactors = FALSE
      ))
    }
  }

  if (nrow(label_df) == 0) {
    hidden_count_rv(0L)
    return(p)
  }

  # Sort by logP descending (most significant first = highest priority to keep)
  label_df <- label_df[order(-label_df$logP), ]

  # Normalize coordinates for overlap detection
  x_range <- range(df$logFC, na.rm = TRUE)
  y_range <- range(df$logP,  na.rm = TRUE)
  x_span  <- diff(x_range)
  y_span  <- diff(y_range)
  if (x_span == 0) x_span <- 1
  if (y_span == 0) y_span <- 1

  placed   <- list()
  hidden   <- 0L
  keep_idx <- integer(0)

  for (i in seq_len(nrow(label_df))) {
    nx <- (label_df$logFC[i] - x_range[1]) / x_span
    ny <- (label_df$logP[i]  - y_range[1]) / y_span

    too_close <- FALSE
    for (pl in placed) {
      if (sqrt((nx - pl$nx)^2 + (ny - pl$ny)^2) < min_dist) {
        too_close <- TRUE
        break
      }
    }

    if (too_close) {
      hidden <- hidden + 1L
    } else {
      placed   <- c(placed, list(list(nx = nx, ny = ny)))
      keep_idx <- c(keep_idx, i)
    }
  }

  hidden_count_rv(hidden)

  label_df_kept <- label_df[keep_idx, , drop = FALSE]
  if (nrow(label_df_kept) == 0) return(p)

  # Colored marker overlay for labeled points
  p <- plotly::add_trace(
    p,
    data       = label_df_kept,
    x          = ~logFC,
    y          = ~logP,
    type       = "scatter",
    mode       = "markers",
    marker     = list(color = label_df_kept$label_col, size = 9,
                      line = list(color = "white", width = 1.5)),
    showlegend = FALSE,
    hoverinfo  = "skip",
    inherit    = FALSE
  )

  # White-background text annotations (no arrow)
  annotations <- lapply(seq_len(nrow(label_df_kept)), function(i) {
    list(
      x           = label_df_kept$logFC[i],
      y           = label_df_kept$logP[i],
      text        = label_df_kept$label_txt[i],
      font        = list(color = label_df_kept$label_col[i], size = 11,
                         family = "Arial"),
      showarrow   = FALSE,
      xanchor     = "left",
      yanchor     = "bottom",
      xshift      = 6,
      yshift      = 4,
      bgcolor     = "rgba(255,255,255,0.85)",
      bordercolor = label_df_kept$label_col[i],
      borderwidth = 1,
      borderpad   = 2
    )
  })

  plotly::layout(p, annotations = annotations)
}