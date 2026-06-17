################################################################################
# Module: QC_PCA
#
# Produce PCA plots
################################################################################

## Calculate PCA from GCT object
## This function extracts and processes the data matrix, then calculates PCA
## Returns the PCA object and the processed data matrix
calculate_PCA <- function(gct) {
  # Get matrix
  mat <- gct@mat
  
  # Store original column names to preserve hyphens, etc.
  original_colnames <- colnames(mat)
  
  # Convert to data.frame and drop NA rows (features), then transpose
  mat_df <- data.frame(mat, check.names = FALSE)
  # Explicitly restore original column names to preserve hyphens, etc.
  colnames(mat_df) <- original_colnames
  
  # Drop rows (features) with any NA, then transpose
  # After transpose, rows are samples (from original columns)
  mat_df <- mat_df %>% drop_na()
  data.norm <- t(mat_df)
  
  # Ensure rownames (samples) match original column names
  rownames(data.norm) <- original_colnames
  
  # Filter out zero-variance features (columns after transpose)
  data.norm <- data.norm[,apply(data.norm, 2, var, na.rm=TRUE) != 0]
  
  # Check if we have any data left after filtering
  if (ncol(data.norm) == 0) {
    stop("No features remain after filtering (all features have zero variance or are all NA). Cannot perform PCA.")
  }
  if (nrow(data.norm) == 0) {
    stop("No samples remain after filtering. Cannot perform PCA.")
  }
  
  # Calculate PCA
  my_pca <- prcomp(data.norm, center=TRUE, scale=TRUE)
  
  return(list(
    pca = my_pca,
    data_norm = data.norm,
    original_colnames = original_colnames,
    n_features = ncol(data.norm),
    n_features_total = nrow(mat)
  ))
}

## plot PCA
create_PCA_plot <- function (gct, col_of_interest, ome, custom_color_map = NULL, comp.x=1, comp.y=2, 
                            second_col_of_interest = NULL, var1_display = "color", var2_display = "shape",
                            fill_shapes = FALSE, pca_result = NULL) {
  # Check for valid PC inputs
  if (is.null(comp.x) || is.null(comp.y) || length(comp.x) == 0 || length(comp.y) == 0) {
    stop("PC1 and PC2 must be valid and non-empty.")
  }
  #print error message if PC1=PC2
  if(comp.x==comp.y){
    stop("PC1 and PC2 are equal. Please try again with different values for PC1 and PC2.")
  }
  
  # validate dual variable inputs
  if (!is.null(second_col_of_interest)) {
    if (var1_display == var2_display) {
      stop("Both variables cannot use the same display method. Please select different display options for each variable.")
    }
    if (second_col_of_interest == col_of_interest) {
      stop("Second variable must be different from the first variable.")
    }
  }
  
  # Use pre-calculated PCA if provided, otherwise calculate it
  if (!is.null(pca_result)) {
    my_pca <- pca_result$pca
    original_colnames <- pca_result$original_colnames
    n_features <- if (!is.null(pca_result$n_features)) {
      pca_result$n_features
    } else {
      ncol(pca_result$data_norm)
    }
    n_features_total <- if (!is.null(pca_result$n_features_total)) {
      pca_result$n_features_total
    } else {
      nrow(gct@mat)
    }
  } else {
    # Calculate PCA if not provided
    pca_result <- calculate_PCA(gct)
    my_pca <- pca_result$pca
    original_colnames <- pca_result$original_colnames
    n_features <- pca_result$n_features
    n_features_total <- pca_result$n_features_total
  }
  
  # Get matrix for annotations (use original, unsorted for annotation extraction)
  mat <- gct@mat
  group <- gct@cdesc[[col_of_interest]]
  
  # Check if this column is continuous based on color mapping
  # If continuous, keep as numeric; if discrete, convert to factor
  if (!is.null(custom_color_map) && !custom_color_map$is_discrete) {
    # Continuous column - keep as numeric (don't convert to factor)
    if (is.character(group) || is.factor(group)) {
      group <- suppressWarnings(as.numeric(as.character(group)))
    }
    annot <- data.frame('sample'=colnames(mat),"annot"=group, stringsAsFactors = FALSE)
  } else {
    # Discrete column - convert to character then factor
    group <- as.character(group)
    annot <- data.frame('sample'=colnames(mat),"annot"=group, stringsAsFactors = FALSE)
    #replace NA with characters so that colors map appropriately
    annot$annot[is.na(annot$annot)]="NA"
    # Force alphabetical legend ordering for discrete annotations.
    annot_levels <- sort(unique(annot$annot), na.last = TRUE)
    annot$annot <- factor(annot$annot, levels = annot_levels)
  }
  
  # add second annotation if provided
  if (!is.null(second_col_of_interest)) {
    second_group <- as.character(gct@cdesc[[second_col_of_interest]])
    annot$second_annot <- second_group
    #replace NA with characters so that colors map appropriately
    annot$second_annot[is.na(annot$second_annot)] <- "NA"
    # Force alphabetical legend ordering for second annotation as well.
    second_levels <- sort(unique(annot$second_annot), na.last = TRUE)
    annot$second_annot <- factor(annot$second_annot, levels = second_levels)
    colnames(annot)[3] <- second_col_of_interest
  }
  
  # Sort annotations by the primary annotation for visualization (doesn't affect PCA)
  annot <- annot[order(annot$annot),]
  colnames(annot)[2] = col_of_interest
  
  # get variance explained
  vars <- my_pca$sdev^2
  prop_vars <- vars / sum(vars)
  
  # Use manual ggplot for PCA points with enhanced tooltip
  pca_df <- as.data.frame(my_pca$x)
  # Ensure sample names match original column names (preserve hyphens, etc.)
  pca_df$sample <- rownames(pca_df)
  
  # Check if merge will result in empty data frame
  if (nrow(pca_df) == 0) {
    stop("PCA resulted in empty data frame. This may indicate a problem with the input data.")
  }
  
  # Filter annot to only include samples that are in pca_df (after filtering)
  # This ensures the merge will work correctly
  annot <- annot[annot$sample %in% pca_df$sample, , drop = FALSE]
  
  # Check if annot is empty after filtering
  if (nrow(annot) == 0) {
    stop("No matching samples found between PCA results and annotations. This may indicate a sample name mismatch.")
  }
  
  pca_df <- merge(pca_df, annot, by = "sample")
  
  # Check if merge resulted in empty data frame (shouldn't happen now, but safety check)
  if (nrow(pca_df) == 0) {
    stop("Merge resulted in empty data frame. This should not happen - please report this error.")
  }
  
  # get color definition
  #NOTE: need to add NA as a color or else it doesn't show up properly in the legend
  if (is.null(custom_color_map)) {
    color_definition <- NULL
  } else if (custom_color_map$is_discrete) {
    colors <- c(unlist(custom_color_map$colors),"gray50")
    names(colors) <- c(custom_color_map$vals,NA)
    color_definition <- scale_colour_manual(values = colors)
  } else {
    # Re-extract group from merged pca_df to ensure it matches
    group <- as.character(pca_df[[col_of_interest]])
    group <- as.numeric(group)
    
    # Get colors from custom_color_map or use defaults
    if (is.null(custom_color_map$colors) || length(custom_color_map$colors) == 0) {
      # Use default continuous colors
      low_col <- "blue"
      mid_col <- "white"
      high_col <- "red"
      na_col <- "gray50"
    } else {
      low_col <- custom_color_map$colors[which(custom_color_map$vals == "low")]
      mid_col <- custom_color_map$colors[which(custom_color_map$vals == "mid")]
      high_col <- custom_color_map$colors[which(custom_color_map$vals == "high")]
      na_col <- custom_color_map$colors[which(custom_color_map$vals == "na_color")]
      
      # Fallback to defaults if any are missing
      if (length(low_col) == 0) low_col <- "blue"
      if (length(mid_col) == 0) mid_col <- "white"
      if (length(high_col) == 0) high_col <- "red"
      if (length(na_col) == 0) na_col <- "gray50"
    }
    
    color_definition <- scale_colour_gradient2(
      low = low_col,
      mid = mid_col,
      high = high_col,
      midpoint = mean(c(min(group, na.rm = TRUE), max(group, na.rm = TRUE))),
      na.value = na_col
    )
  }
  
  # Compose tooltip text
  tooltip_text <- paste0(
    "Sample: ", pca_df$sample,
    "<br>PC", comp.x, ": ", signif(pca_df[[paste0("PC", comp.x)]], 4),
    "<br>PC", comp.y, ": ", signif(pca_df[[paste0("PC", comp.y)]], 4),
    "<br>", col_of_interest, ": ", pca_df[[col_of_interest]]
  )
  
  # Add second variable to tooltip if present
  if (!is.null(second_col_of_interest)) {
    tooltip_text <- paste0(tooltip_text, "<br>", second_col_of_interest, ": ", pca_df[[second_col_of_interest]])
  }
  pca_df$tooltip <- tooltip_text
  
  # Create aesthetic mappings based on display preferences
  if (is.null(second_col_of_interest)) {
    # Single variable - use existing logic
    plot_aes <- aes(
      x = !!rlang::sym(paste0("PC", comp.x)),
      y = !!rlang::sym(paste0("PC", comp.y)),
      color = !!rlang::sym(col_of_interest),
      text = !!rlang::sym("tooltip")
    )
    plot_title <- paste0("PCA plot by ", col_of_interest, ": ", ome)
  } else {
    # Two variables - create appropriate aesthetic mapping
    if (var1_display == "color" && var2_display == "shape") {
      plot_aes <- aes(
        x = !!rlang::sym(paste0("PC", comp.x)),
        y = !!rlang::sym(paste0("PC", comp.y)),
        color = !!rlang::sym(col_of_interest),
        shape = !!rlang::sym(second_col_of_interest),
        text = !!rlang::sym("tooltip")
      )
    } else if (var1_display == "shape" && var2_display == "color") {
      plot_aes <- aes(
        x = !!rlang::sym(paste0("PC", comp.x)),
        y = !!rlang::sym(paste0("PC", comp.y)),
        color = !!rlang::sym(second_col_of_interest),
        shape = !!rlang::sym(col_of_interest),
        text = !!rlang::sym("tooltip")
      )
    } else {
      # Default fallback
      plot_aes <- aes(
        x = !!rlang::sym(paste0("PC", comp.x)),
        y = !!rlang::sym(paste0("PC", comp.y)),
        color = !!rlang::sym(col_of_interest),
        text = !!rlang::sym("tooltip")
      )
    }
    plot_title <- paste0("PCA plot by ", col_of_interest, " and ", second_col_of_interest, ": ", ome)
  }
  
  g <- ggplot(pca_df, plot_aes) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "longdash", color = "darkgrey") +
    geom_vline(xintercept = 0, linetype = "longdash", color = "darkgrey") +
    theme_bw() +
    theme(text = element_text(size = 12)) +
    labs(
      title = plot_title,
      subtitle = pca_feature_count_subtitle(pca_result),
      x = paste0("PC", comp.x, " (", round(prop_vars[comp.x] * 100, 1), "%)"),
      y = paste0("PC", comp.y, " (", round(prop_vars[comp.y] * 100, 1), "%)")
    )
  
  # Apply color definition
  if (!is.null(color_definition)) {
    g <- g + color_definition
  }
  
  # Add shape scale - use open shapes by default, filled shapes when toggle is selected
  if (!is.null(second_col_of_interest)) {
    # Get unique values for shape variable
    shape_var <- if (var1_display == "shape") col_of_interest else second_col_of_interest
    unique_shapes <- sort(unique(pca_df[[shape_var]]))
    n_shapes <- length(unique_shapes)
    
    # Define shapes - by default use open shapes (hollow), or filled shapes if toggle is selected
    # Logic: shapes 3, 4, 8, 11 count as filled (have symbols inside: plus, cross, asterisk, star)
    # All other shapes (0, 1, 2, 5, 6, 7, 9, 10, 12, 13, 14) count as hollow
    # Order: circle first, then square, triangle, diamond, triangle down, then the rest
    if (fill_shapes) {
      # Filled shapes: circle first, then other distinct filled shapes
      # 16 (circle), 15 (square), 17 (triangle), 18 (diamond), then symbol shapes (3, 4, 8, 11),
      # then 19 (circle small), 20 (bullet) as last resort
      available_shapes <- c(16, 15, 17, 18, 3, 4, 8, 11, 19, 20)
    } else {
      # Open shapes (hollow) - default: circle first, then other hollow shapes
      # Order: 1 (circle), 0 (square), 2 (triangle), 5 (diamond), 6 (triangle down),
      #        then 7, 9, 10, 12, 13, 14 (other hollow shapes)
      available_shapes <- c(1, 0, 2, 5, 6, 7, 9, 10, 12, 13, 14)
    }
    
    if (n_shapes <= length(available_shapes)) {
      shape_values <- available_shapes[1:n_shapes]
      names(shape_values) <- unique_shapes
      g <- g + scale_shape_manual(values = shape_values)
    } else {
      # If we still need more shapes, we can cycle through them
      shape_values <- rep(available_shapes, ceiling(n_shapes / length(available_shapes)))[1:n_shapes]
      names(shape_values) <- unique_shapes
      g <- g + scale_shape_manual(values = shape_values)
    }
  }
  
  g
}

## Convert ggplot to plotly while preserving ggplot2 subtitle
## ggplotly() only transfers labs(title); subtitles are dropped in the browser.
ggplotly_with_gg_subtitle <- function(gg, ...) {
  p <- ggplotly(gg, ...)
  subtitle <- gg$labels$subtitle
  if (!is.null(subtitle) && nzchar(subtitle)) {
    title <- gg$labels$title
    if (is.null(title)) {
      title <- ""
    }
    p$x$layout$title$text <- paste0(
      title,
      "<br><span style=\"font-size:12px;color:rgba(0,0,0,0.75)\">",
      subtitle,
      "</span>"
    )
  }
  p
}

## Feature loadings table (features x PCs) from a cached PCA result
##' @param pca_result Cached output from `calculate_PCA()`.
##' @param gct Optional processed GCT used to attach `id` and `geneSymbol` from `@rdesc`.
##' @param for_export If TRUE, return rows sorted by `rank` (1 = highest). Includes
##'   `cumulative_loading_PC1_k` where `k` is min(10, number of PCs), then `id`,
##'   `geneSymbol`, and all raw PC loadings. Ignores `max_pcs`.
##' @param max_pcs Maximum number of PC columns to return; `NULL` keeps all PCs.
get_pca_loadings_df <- function(pca_result, gct = NULL, for_export = FALSE, max_pcs = 10L) {
  loadings <- pca_result$pca$rotation
  df <- as.data.frame(loadings, check.names = FALSE)
  df$feature <- rownames(loadings)

  df$id <- df$feature
  df$geneSymbol <- NA_character_

  if (!is.null(gct)) {
    rdesc <- gct@rdesc
    rdesc_rn <- rownames(rdesc)
    if (is.null(rdesc_rn)) {
      rdesc_rn <- gct@rid
    }
    idx <- match(df$feature, rdesc_rn)
    if ("id" %in% names(rdesc)) {
      mapped_id <- rdesc[idx, "id", drop = TRUE]
      df$id <- ifelse(is.na(mapped_id), df$feature, unname(mapped_id))
    }
    if ("geneSymbol" %in% names(rdesc)) {
      mapped_gs <- rdesc[idx, "geneSymbol", drop = TRUE]
      df$geneSymbol <- unname(mapped_gs)
    }
  }

  pc_cols_all <- grep("^PC", names(df), value = TRUE)
  pc_cols <- pc_cols_all
  if (!is.null(max_pcs)) {
    pc_cols <- head(pc_cols, as.integer(max_pcs))
  }
  if (for_export) {
    rank_through <- pca_rank_pcs_used(length(pc_cols_all))
    cum_col <- pca_cumulative_loading_column_name(rank_through)
    ranked <- pca_rank_features_by_pc_loading(df, through_pc = rank_through)
    df <- df[match(ranked$feature, df$feature), , drop = FALSE]
    df$rank <- seq_len(nrow(df))
    df[[cum_col]] <- ranked[[cum_col]]
    return(df[, c("rank", cum_col, "id", "geneSymbol", pc_cols_all), drop = FALSE])
  }
  df[, c("id", "geneSymbol", "feature", pc_cols), drop = FALSE]
}

## Subtitle text for PCA plots showing features used vs total
pca_feature_count_subtitle <- function(pca_result) {
  n_features <- pca_result$n_features
  n_features_total <- pca_result$n_features_total
  paste0(
    format(n_features, big.mark = ","), "/",
    format(n_features_total, big.mark = ","),
    " features used"
  )
}

## loading^2 for each feature x PC
pca_loading_sq_matrix <- function(loadings_df) {
  pc_cols <- grep("^PC", names(loadings_df), value = TRUE)
  mat <- as.matrix(loadings_df[, pc_cols, drop = FALSE])
  storage.mode(mat) <- "double"
  mat^2
}

## Number of leading PCs used for ranking / plot (at most `max_pcs`, capped by available PCs).
pca_rank_pcs_used <- function(n_pc_all, max_pcs = 10L) {
  min(as.integer(max_pcs), as.integer(n_pc_all))
}

## Export column name for cumulative loading through PC1..k.
pca_cumulative_loading_column_name <- function(through_pc) {
  paste0("cumulative_loading_PC1_", as.integer(through_pc))
}

## Fraction of total squared loading captured through the first `through_pc` PCs (0-1).
pca_cumulative_loading_fraction <- function(loadings_df, through_pc = 10L) {
  sq_mat <- pca_loading_sq_matrix(loadings_df)
  if (ncol(sq_mat) == 0L) {
    return(numeric(nrow(sq_mat)))
  }
  k <- min(as.integer(through_pc), ncol(sq_mat))
  sq_all <- rowSums(sq_mat)
  sq_through <- rowSums(sq_mat[, seq_len(k), drop = FALSE])
  ifelse(sq_all == 0, 0, sq_through / sq_all)
}

## Rank features by PC1-`through_pc` importance (same metric as the cumulative plot at PC k).
## Primary: cumulative squared-loading fraction through PC1..k; ties: max |loading| on those PCs, then id.
pca_rank_features_by_pc_loading <- function(loadings_df, through_pc = 10L) {
  pc_cols_all <- grep("^PC", names(loadings_df), value = TRUE)
  if (length(pc_cols_all) == 0L) {
    out <- data.frame(feature = character(), stringsAsFactors = FALSE)
    out[[pca_cumulative_loading_column_name(0L)]] <- numeric()
    return(out)
  }
  k <- pca_rank_pcs_used(length(pc_cols_all), max_pcs = through_pc)
  cum_col <- pca_cumulative_loading_column_name(k)
  cum_frac <- pca_cumulative_loading_fraction(loadings_df, through_pc = k)
  pc_rank_cols <- head(pc_cols_all, k)
  max_abs <- apply(
    abs(as.matrix(loadings_df[, pc_rank_cols, drop = FALSE])),
    1,
    max
  )
  feature <- if ("feature" %in% names(loadings_df)) {
    loadings_df$feature
  } else {
    loadings_df$id
  }
  tie_id <- if ("id" %in% names(loadings_df)) loadings_df$id else feature
  ord <- order(-cum_frac, -max_abs, tie_id)
  out <- data.frame(feature = feature[ord], stringsAsFactors = FALSE)
  out[[cum_col]] <- cum_frac[ord]
  out
}

## Plot legend / label text: `geneSymbol` when present, otherwise `id`.
pca_feature_display_label <- function(id, geneSymbol) {
  if (length(geneSymbol) == 1L && !is.na(geneSymbol) && nzchar(geneSymbol)) {
    unname(geneSymbol)
  } else {
    unname(id)
  }
}

## Top-N feature names using `pca_rank_features_by_pc_loading()`.
##' @param max_pcs Number of leading PCs used for ranking (default 10).
top_pca_loading_features <- function(loadings_df, topn, max_pcs = 10L) {
  if (is.null(max_pcs)) {
    max_pcs <- length(grep("^PC", names(loadings_df), value = TRUE))
  }
  ranked <- pca_rank_features_by_pc_loading(loadings_df, through_pc = max_pcs)
  head(ranked$feature, min(as.integer(topn), nrow(ranked)))
}

## Cumulative squared-loading plot for the top 10 features (PC1-10 on the x-axis).
## Top 10 and legend order match export rank (cumulative squared loading through PC1-10).
create_PCA_loadings_cumulative <- function(pca_result, ome = "", gct = NULL) {
  topn <- 10L
  loadings_df <- get_pca_loadings_df(pca_result, gct = gct, max_pcs = NULL)

  n_pc_all <- ncol(pca_result$pca$rotation)
  n_pc_plot <- pca_rank_pcs_used(n_pc_all)
  sq_all_mat <- pca_loading_sq_matrix(loadings_df)

  highlight_features <- top_pca_loading_features(loadings_df, topn, max_pcs = n_pc_plot)

  rows <- lapply(seq_along(highlight_features), function(i) {
    f <- highlight_features[i]
    meta <- loadings_df[loadings_df$feature == f, , drop = FALSE][1L, , drop = FALSE]
    has_gs <- "geneSymbol" %in% names(meta) &&
      !is.na(meta$geneSymbol) &&
      nzchar(meta$geneSymbol)
    sq_all <- as.numeric(sq_all_mat[f, , drop = TRUE])
    total_sq <- sum(sq_all)
    sq_plot <- sq_all[seq_len(n_pc_plot)]
    pct_pc <- if (total_sq == 0) rep(0, n_pc_plot) else 100 * sq_plot / total_sq
    cumulative <- if (total_sq == 0) {
      rep(0, n_pc_plot)
    } else {
      cumsum(sq_plot) / total_sq
    }
    data.frame(
      feature = f,
      id = meta$id,
      geneSymbol = if (has_gs) meta$geneSymbol else NA_character_,
      display_label = pca_feature_display_label(meta$id, meta$geneSymbol),
      PC = seq_len(n_pc_plot),
      pct_pc = pct_pc,
      cumulative = cumulative,
      stringsAsFactors = FALSE
    )
  })
  plot_df <- dplyr::bind_rows(rows)
  display_levels <- plot_df$display_label[match(highlight_features, plot_df$feature)]
  feature_ranks <- seq_along(display_levels)
  legend_labels <- paste0(sprintf("%02d", feature_ranks), ": ", display_levels)
  plot_df$legend_label <- legend_labels[match(plot_df$feature, highlight_features)]
  plot_df$legend_label <- factor(plot_df$legend_label, levels = legend_labels)

  plot_df$tooltip <- paste0(
    "Rank: ", match(plot_df$feature, highlight_features),
    "<br>id: ", plot_df$id,
    ifelse(
      !is.na(plot_df$geneSymbol) & nzchar(plot_df$geneSymbol),
      paste0("<br>geneSymbol: ", plot_df$geneSymbol),
      ""
    ),
    "<br>PC: PC", plot_df$PC,
    "<br>Individual loading: ", round(plot_df$pct_pc, 1), "%",
    "<br>Cumulative loading: ", round(plot_df$cumulative * 100, 1), "%"
  )

  title <- if (nzchar(ome)) {
    paste0("Top 10 features - cumulative loading: ", ome)
  } else {
    "Top 10 features - cumulative loading"
  }

  n_features <- length(display_levels)
  if (n_features <= 9L) {
    line_colors <- get_preset_palette("Paul Tol Muted", n_features)
  } else {
    line_colors <- c(get_preset_palette("Paul Tol Muted", 9L), "#BBBBBB")
    line_colors <- line_colors[seq_len(n_features)]
  }
  names(line_colors) <- legend_labels

  ggplot(plot_df, aes(
    x = .data$PC,
    y = .data$cumulative,
    group = .data$feature,
    color = .data$legend_label,
    text = .data$tooltip
  )) +
    geom_line(linewidth = 0.9, alpha = 0.65) +
    geom_point(size = 2.5, alpha = 0.75) +
    scale_color_manual(
      values = line_colors,
      breaks = legend_labels,
      labels = legend_labels
    ) +
    scale_x_continuous(breaks = seq_len(n_pc_plot), labels = paste0("PC", seq_len(n_pc_plot))) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
    theme_bw() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
      title = title,
      x = NULL,
      y = "Cumulative squared loading",
      color = "Rank"
    )
}

## calculate PCA regression
pca_variance_explained <- function (pca,cdesc,components=c(1:10)){
  # Determine maximum available PCs
  max_available_PCs <- ncol(pca$x)
  
  # Adjust components to use available PCs only
  components <- components[components <= max_available_PCs]
  
  # Check if we have any valid components
  if (length(components) == 0) {
    stop("No valid principal components available. Cannot perform PCA regression.")
  }
  
  # Obtain the principal component coordinates
  p <- data.frame (pca$x[,components])
  
  # Intitialize the result data frame
  data <- data.frame (dims=character(),
                      pct.exp=numeric(),
                      experimental.factor = character() )
  
  # Loop through all component-metadata combinations
  for (i in colnames(cdesc)) {
    for (j in colnames(p)) {
      # Check if the current metadata vector is valid
      if ( (sum(is.na(cdesc[,i])) == length(cdesc[,i])) |
           (length(levels(as.factor(cdesc[,i]))) < 2) ) {
        next
      }
      
      # Fit a linear model between the principal component and metadata variable
      fit <- lm (p[,j] ~ cdesc[,i])
      af <- anova (fit)
      afss <- af$"Sum Sq"
      
      dimensions <- as.numeric (gsub("PC","",j))
      data <- rbind (data,
                     data.frame (dims = dimensions,
                                 pct.exp = afss[1]/sum(afss)*100,
                                 experimental.factor=i))
    }
  }
  var.explained <- pca$sdev^2
  pct.var <- var.explained * 100 / sum(var.explained)

  dims_sorted <- sort(unique(data$dims))
  pc_levels <- paste0(
    "PC", dims_sorted,
    " (", round(pct.var[dims_sorted], 1), "%)"
  )
  data$pc_label <- factor(
    paste0("PC", data$dims, " (", round(pct.var[data$dims], 1), "%)"),
    levels = pc_levels
  )
  data$tooltip <- paste0(
    "PC: ", data$pc_label,
    "<br>% variance explained: ", round(data$pct.exp, 1), "%"
  )

  # calculate % sum total (over pca components in p) of variance attributable to each experimental factor
  expt.var <- data %>% group_by(.data$experimental.factor) %>%
    summarize(sum.total.var.pct = sum(.data$pct.exp / 100 * pct.var[.data$dims] / 100 * 100))

  g <- ggplot(
    data = data,
    aes(
      x = .data$pc_label,
      y = .data$pct.exp,
      group = .data$experimental.factor,
      color = .data$experimental.factor,
      text = .data$tooltip
    )
  ) +
    geom_line() +
    geom_point() +
    labs(
      x = "Component (% total variance explained)",
      y = "% variance explained within component"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return (list (plot=g, table=expt.var))
}

##plot PCA regression
create_PCA_reg <- function(gct, col_of_interest, ome, custom_color_map = NULL, components.max=10, pca_result = NULL){
  
  #get data and annotations
  cdesc <- gct@cdesc
  
  # Use pre-calculated PCA if provided, otherwise calculate it
  if (!is.null(pca_result)) {
    my_pca <- pca_result$pca
  } else {
    # Calculate PCA if not provided
    pca_result <- calculate_PCA(gct)
    my_pca <- pca_result$pca
  }
  
  # Determine maximum available PCs and adjust components.max accordingly
  max_available_PCs <- ncol(my_pca$x)
  components.max <- min(components.max, max_available_PCs)
  
  #perform batch effect check and plot PCA regression
  pca.var <- pca_variance_explained (my_pca, cdesc[col_of_interest], components=1:components.max)
  g <- pca.var$plot +
    ggtitle(glue(
      "Cumulative Variance Explained by {col_of_interest} for {ome}: ",
      "{round(pca.var$table$sum.total.var.pct, digits=2)}"
    )) +
    theme(legend.position = "none")
  
  # Return ggplot object (Shiny's renderPlot() will handle printing automatically)
  return(g)
}

