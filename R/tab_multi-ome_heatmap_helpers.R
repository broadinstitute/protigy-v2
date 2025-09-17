# Note: preprocess_gcts_multiome_heatmap function removed
# The multi-ome heatmap now uses the merged GCT created during the main setup process
# This eliminates duplicate GCT merging and ensures consistency across modules

## make complex heatmap
## calls global variables merged_rdesc, merged_mat, sample_anno
myComplexHeatmap <- function(
    params, GENEMAX, merged_rdesc, merged_mat, sample_anno, custom_colors, selected_annotations = NULL) {
  
  # extract parameters
  genes.char <- params$genes.char
  zscore <- params$zscore
  min.val <- params$min.val
  max.val <- params$max.val
  sort.after <- params$sort.after
  show.sample.label <- params$show.sample.label
  ome.order <- params$ome.order
  max.levels <- if (is.null(params$max.levels)) 20 else params$max.levels  # Same as autodetect_continuous_nfactor_cutoff in setup
  max_features_per_gene <- if (is.null(params$max_features_per_gene)) 5 else params$max_features_per_gene
  cluster_columns <- if (is.null(params$cluster_columns)) TRUE else as.logical(params$cluster_columns)
  
  # extract genes
  genes.all <- extractGenes(genes.char, select(merged_rdesc, .data$geneSymbol), GENEMAX)
  genes.vec <- genes.all$genes.vec
  
  # Check if any genes were found
  if (length(genes.vec) == 0) {
    stop("No genes found in the dataset. Please check your gene list and try again.")
  }
  
  # extract rows for that gene
  genes.Table <- as.data.frame(merged_mat)[which(merged_rdesc$geneSymbol %in% genes.vec), ]
  row.anno <- merged_rdesc[which(merged_rdesc$geneSymbol %in% genes.vec), ]
  
  # generate heatmap table
  # first 3 columns are: geneSymbol, ome, row_label
  # the rest of the columns are labeled by each sample, contain abundance data
  # NOTE: if you ever edit this so that more/less then 3 columns are 
  # description-type information, check to other lines where genes.Table is 
  # indexed to make sure unexpected errors don't occur
  genes.Table <- getHMTable(genes.Table, row.anno, params)
  
  # Filter features per gene per dataset by standard deviation if requested
  if (max_features_per_gene < Inf && nrow(genes.Table) > 0) {
    # Calculate standard deviation for each feature (using the abundance data columns)
    abundance_cols <- 4:ncol(genes.Table)  # Skip first 3 columns (geneSymbol, ome, row_label)
    feature_sd <- apply(genes.Table[, abundance_cols], 1, function(x) sd(x, na.rm = TRUE))
    
    # For each gene, filter per dataset (ome)
    filtered_indices <- c()
    for (gene in genes.vec) {
      gene_indices <- which(genes.Table$geneSymbol == gene)
      
      # Get unique datasets (omes) for this gene
      gene_omes <- unique(genes.Table$ome[gene_indices])
      
      for (ome in gene_omes) {
        # Find features for this gene in this dataset
        gene_ome_indices <- gene_indices[which(genes.Table$ome[gene_indices] == ome)]
        
        if (length(gene_ome_indices) > max_features_per_gene) {
          # Get top N features by standard deviation for this gene in this dataset
          gene_ome_sd <- feature_sd[gene_ome_indices]
          top_indices <- gene_ome_indices[order(gene_ome_sd, decreasing = TRUE)[1:max_features_per_gene]]
          filtered_indices <- c(filtered_indices, top_indices)
        } else {
          # Keep all features for this gene in this dataset if <= max_features_per_gene
          filtered_indices <- c(filtered_indices, gene_ome_indices)
        }
      }
    }
    
    # Apply filtering
    genes.Table <- genes.Table[filtered_indices, , drop = FALSE]
  } 
  genes.Table <- genes.Table[ ,c(1:3, order(names(genes.Table)[-(1:3)])+3)]
  
  # Re-order the omes based on ome.order if provided
  if (!is.null(ome.order) && length(ome.order) > 0) {
    # Ensure ome.order only contains datasets that are actually present in genes.Table
    available_omes <- unique(genes.Table$ome)
    final_order <- ome.order[ome.order %in% available_omes]
    
    # Re-order the omes based on final order
    genes.Table$ome <- factor(genes.Table$ome, final_order)
  }
  
  # make matrix to feed into heatmap
  # z-score rows if requested
  if (zscore == "row") {
    genes.Matrix <- t(apply(genes.Table[,-(1:3)], 1,
                            function(x)(x-mean(x, na.rm=T))/sd(x, na.rm=T)))
  } else {
    genes.Matrix <- as.matrix(genes.Table[,-(1:3)])
  }
  rownames(genes.Matrix) <- make.unique(genes.Table$row_label)
  
  
  ## make heatmap for annotations
  anno.fig <- sample_anno[order(sample_anno$Sample.ID), , drop = FALSE]
  rownames(anno.fig) <- anno.fig$Sample.ID
  
  # Filter annotations to only include samples that are in the matrix
  matrix_samples <- colnames(genes.Matrix)
  anno.fig <- anno.fig[rownames(anno.fig) %in% matrix_samples, , drop = FALSE]
  
  # Filter to only selected annotations (if provided)
  if (!is.null(selected_annotations) && length(selected_annotations) > 0) {
    # Keep Sample.ID and selected annotations
    keep_cols <- c("Sample.ID", selected_annotations)
    keep_cols <- keep_cols[keep_cols %in% colnames(anno.fig)]
    anno.fig <- anno.fig[, keep_cols, drop = FALSE]
  }
  
  # Remove Sample.ID column for heatmap display
  anno.fig <- anno.fig[, -(which(names(anno.fig) == "Sample.ID")), drop = FALSE]

  
  # get custom color palette and convert to ComplexHeatmap format
  anno.fig.color <- custom_colors[names(anno.fig)]
  
  # Convert color structure to ComplexHeatmap format
  anno.fig.color <- mapply(function(color_obj, annot_name) {
    if (color_obj$is_discrete) {
      # Convert discrete colors to named vector
      names(color_obj$colors) <- color_obj$vals
      return(color_obj$colors)
    } else {
      # Convert continuous colors to function
      annot_values <- suppressWarnings(as.numeric(sample_anno[[annot_name]]))
      
      # If conversion failed (all NAs), treat as discrete
      if (all(is.na(annot_values))) {
        unique_vals <- unique(sample_anno[[annot_name]][!is.na(sample_anno[[annot_name]])])
        colors_vector <- rep(color_obj$colors[1], length(unique_vals))
        names(colors_vector) <- unique_vals
        return(colors_vector)
      }
      
      min_val <- min(annot_values, na.rm = TRUE)
      max_val <- max(annot_values, na.rm = TRUE)
      
      # Handle case where all values are identical
      if (min_val == max_val) {
        range_val <- abs(min_val) * 0.1 + 0.1
        breaks <- c(min_val - range_val, min_val, min_val + range_val)
      } else {
        breaks <- c(min_val, mean(annot_values, na.rm = TRUE), max_val)
      }
      
      # For continuous colors, use the low/mid/high colors from the color object
      if ("low" %in% color_obj$vals && "mid" %in% color_obj$vals && "high" %in% color_obj$vals) {
        low_idx <- which(color_obj$vals == "low")
        mid_idx <- which(color_obj$vals == "mid")
        high_idx <- which(color_obj$vals == "high")
        colors_for_ramp <- c(color_obj$colors[low_idx], color_obj$colors[mid_idx], color_obj$colors[high_idx])
      } else {
        # Fallback: use first 3 colors or repeat as needed
        if (length(color_obj$colors) >= 3) {
          colors_for_ramp <- color_obj$colors[1:3]
        } else {
          colors_for_ramp <- c(color_obj$colors, rep(color_obj$colors[length(color_obj$colors)], 3 - length(color_obj$colors)))
        }
      }
      
      color_function <- circlize::colorRamp2(
        breaks,
        colors_for_ramp
      )
      return(color_function)
    }
  }, anno.fig.color, names(anno.fig.color), SIMPLIFY = FALSE)
  
  # filter for only annotations that are continuous or less than max.levels 
  if (length(anno.fig.color) > 0) {
    # Check if sapply returns any results
    anno_checks <- sapply(anno.fig.color, function(anno_colors) {
      is.function(anno_colors) | length(anno_colors) <= max.levels
    })
    
    # Only proceed if we have valid results
    if (length(anno_checks) > 0 && is.logical(anno_checks) && any(!is.na(anno_checks))) {
      anno.keep <- names(which(anno_checks))
      anno.fig.color <- anno.fig.color[anno.keep]
      anno.fig <- anno.fig[, anno.keep, drop = FALSE]
    } else {
      # No valid annotations
      anno.fig.color <- list()
      anno.fig <- anno.fig[, character(0), drop = FALSE]
    }
  } else {
    # No annotations available
    anno.fig.color <- list()
    anno.fig <- anno.fig[, character(0), drop = FALSE]
  }
  
  if (dim(anno.fig)[2] == 0) {
    HM.anno <- NULL
    column.to.sort <- NULL
    final.Table <- genes.Table
    
  } else {
    # # convert numeric columns in anno.fig to numeric type
    # numeric_cols <- sapply(anno.fig, function(col) 
    #   all(suppressWarnings(!is.na(as.numeric(as.character(col[!is.na(col)]))))))
    # anno.fig[, numeric_cols] <- sapply(anno.fig[, numeric_cols], as.numeric)
    
    # combine annotation table with genes.Table for final output
    anno.fig.new <- data.frame(matrix(ncol=ncol(genes.Table),
                                      nrow=ncol(anno.fig)))
    names(anno.fig.new) <- names(genes.Table)
    rownames(anno.fig.new) <- names(anno.fig)
    anno.fig.new[, -(1:4)] <- t(anno.fig)
    final.Table <- rbind(anno.fig.new, genes.Table)
    
    HM.anno <- HeatmapAnnotation(df = anno.fig,
                                 col = anno.fig.color,
                                 show_legend = T,
                                 show_annotation_name = T,
                                 annotation_name_side = "left",
                                 na_col = "grey",
                                 annotation_legend_param = list(
                                   direction = 'horizontal'),
                                 height = unit(0.5, 'cm') * nrow(anno.fig))
    column.to.sort <- anno.fig[, sort.after]
  }
  
  # define color map for heatmap
  # Handle case where min.val equals max.val (e.g., all zeros)
  if (min.val == max.val) {
    if (min.val == 0) {
      # If all values are zero, create a simple color map
      col_fun_ratios <- colorRamp2(c(-0.1, 0, 0.1), 
                                   c("blue", "white", "red"))
    } else {
      # If all values are the same but not zero, create a range around that value
      range_val <- abs(min.val) * 0.1 + 0.1  # Small range around the value
      col_fun_ratios <- colorRamp2(c(min.val - range_val, min.val, min.val + range_val), 
                                   c("blue", "white", "red"))
    }
  } else {
    col_fun_ratios <- colorRamp2(c(min.val, 0, max.val), 
                                 c("blue", "white", "red"))
  }
  
  
  # Determine column clustering and ordering
  if (isTRUE(cluster_columns) && !is.null(column.to.sort)) {
    # Check if data is suitable for clustering (no all-identical values, no NA/NaN/Inf)
    data_for_clustering <- genes.Matrix
    has_variation <- any(apply(data_for_clustering, 2, function(x) {
      !all(is.na(x)) && length(unique(x[!is.na(x)])) > 1
    }))
    
    if (has_variation && !any(is.infinite(data_for_clustering))) {
      # Cluster columns within each group
      cluster_columns_param <- TRUE
      column_order_param <- NULL  # ComplexHeatmap will cluster within groups defined by column_split
    } else {
      # Data not suitable for clustering - disable clustering
      cluster_columns_param <- FALSE
      column_order_param <- NULL
      
      # Print warning message
      if (!has_variation) {
        warning("Column clustering disabled: Data contains no variation (all identical values)")
      } else if (any(is.infinite(data_for_clustering))) {
        warning("Column clustering disabled: Data contains infinite values")
      }
    }
  } else {
    # No clustering - use original order
    cluster_columns_param <- FALSE
    column_order_param <- NULL
  }
  
  # make final heatmap for genes
  HM <- tryCatch({
    Heatmap(genes.Matrix,
            col = col_fun_ratios,
            column_title = "Sample",
            row_title_rot = 0,
            row_order = order(genes.Table$ome, genes.Table$row_label),
            cluster_rows = F,
            cluster_columns = cluster_columns_param,
            column_order = column_order_param,
            row_split = genes.Table$geneSymbol,
            column_split = column.to.sort, 
            top_annotation = HM.anno,
            show_row_names = T,
            show_column_names = show.sample.label,
            name = 'relative abundance',
            height = unit(0.5, 'cm') * nrow(genes.Matrix),
            column_names_side = "top",
            column_names_rot = 45,
            column_names_gp = gpar(fontsize = 10),
            row_names_gp = gpar(fontsize = 8),
            column_gap = if (is.numeric(column.to.sort)) {unit(0, "mm")} else {unit(1, "mm")},
            heatmap_legend_param = list(direction='horizontal',
                                        max_width = 350,
                                        legend_width = unit(4, 'cm')))
  }, error = function(e) {
    warning("Heatmap creation failed, attempting without clustering: ", e$message)
    # Try again without clustering but preserve column grouping
    Heatmap(genes.Matrix,
            col = col_fun_ratios,
            column_title = "Sample",
            row_title_rot = 0,
            row_order = order(genes.Table$ome, genes.Table$row_label),
            cluster_rows = F,
            cluster_columns = FALSE,
            column_order = column_order_param,
            row_split = genes.Table$geneSymbol,
            column_split = column.to.sort, 
            top_annotation = HM.anno,
            show_row_names = T,
            show_column_names = show.sample.label,
            name = 'relative abundance',
            height = unit(0.5, 'cm') * nrow(genes.Matrix),
            column_names_side = "top",
            column_names_rot = 45,
            column_names_gp = gpar(fontsize = 10),
            row_names_gp = gpar(fontsize = 8),
            column_gap = if (is.numeric(column.to.sort)) {unit(0, "mm")} else {unit(1, "mm")},
            heatmap_legend_param = list(direction='horizontal',
                                        max_width = 350,
                                        legend_width = unit(4, 'cm')))
  })
  
  return(list(HM=HM, Table=final.Table, cluster_columns=cluster_columns_param))
}

## function to extract gene names from a string input
extractGenes <- function(genes.char, table, GENEMAX){
  
  if(is.null(genes.char))
    return(NULL)
  
  if( length(genes.char) == 0 ){
    return(NULL)
  }
  ## extract genes
  genes.vec= unlist(strsplit(genes.char, ','))
  if(length(genes.vec)==1)
    genes.vec=unlist(strsplit(genes.char, ' '))
  if(length(genes.vec)==1)
    genes.vec=unlist(strsplit(genes.char, ';'))
  
  ## unique gene names
  genes.vec <- unique(genes.vec)
  
  ## limit to 'GENEMAX' genes
  if(length(genes.vec) > GENEMAX){
    warning(paste('more than', GENEMAX,'gene ids submitted! Showing results for the first 20 genes in the list.\n'))
    genes.vec <- genes.vec[1:GENEMAX]
  }
  
  ## remove spaces
  genes.vec <- sapply(genes.vec, function(x) gsub(' ', '', x))
  
  ## exclude genes that are not in the dataset
  genes.notInTable <- setdiff(genes.vec, table$geneSymbol)
  if (length(genes.notInTable) > 0) {
    warning("Some genes were not found in the dataset. These are being excluded.\n")
    genes.vec <- setdiff(genes.vec, genes.notInTable)
  }
  
  return(list(genes.vec=genes.vec, genes.notInTable=genes.notInTable))
}

## function to get the heatmap table
getHMTable <- function(genes.Table, row.anno, params) {
  # Get ome information - check if DataType column exists, if not use protigy.ome
  if ("DataType" %in% names(row.anno)) {
    data_type <- row.anno$DataType
  } else if ("protigy.ome" %in% names(row.anno)) {
    data_type <- row.anno$protigy.ome
  } else {
    data_type <- rep("Unknown", nrow(row.anno))
  }

  # Use the original feature IDs as row labels (these are the actual GCT row IDs)
  # The row.anno comes from merged_rdesc, so rownames(row.anno) contains the original feature IDs
  row_labels <- rownames(row.anno)

  # get omes
  ome <- factor(data_type)

  # combine into one genes.Table
  # Only 3 columns now: geneSymbol, ome, row_label
  genes.Table <- cbind(data.frame(geneSymbol = row.anno$geneSymbol,
                                  ome = ome,
                                  row_label = row_labels),
                       genes.Table)
  rownames(genes.Table) <- row_labels

  return (genes.Table)
}

## function to dynamically determine the height (in px) of the heatmap
## depending on the number of genes
dynamicHeightHM <- function(n.entries){
  height <- 0.3*(n.entries+12) + 3  ## height in inch
  height <- height * 48             ## inch  to pixel
  
  return(height)
}

## DEPRECATED: function to convert global custom colors to the ComplexHeatmap structure
## This function is no longer used - colors are now used directly from globals$colors$multi_ome
multiome_heatmap_custom_colors <- function(custom_colors, sample_anno) {
  mapply(
    custom_colors, names(custom_colors),
    SIMPLIFY = FALSE, USE.NAMES = TRUE,
    FUN = function(annot_colors, annot_name) {
      
      # discrete colors
      if (annot_colors$is_discrete) {
        colors_vector <- annot_colors$colors
        names(colors_vector) <- annot_colors$val
        return(colors_vector)
        
      # continuous colors
      } else {
        # Check if annotation values can be converted to numeric
        annot_values_raw <- sample_anno[[annot_name]]
        annot_values <- suppressWarnings(as.numeric(annot_values_raw))
        
        # If conversion failed (all NAs), treat as discrete
        if (all(is.na(annot_values))) {
          # Convert to discrete colors with better color assignment
          unique_vals <- unique(annot_values_raw[!is.na(annot_values_raw)])
          if (length(unique_vals) <= length(annot_colors$colors)) {
            colors_vector <- annot_colors$colors[1:length(unique_vals)]
          } else {
            # If more unique values than colors, cycle through colors
            colors_vector <- rep(annot_colors$colors, length.out = length(unique_vals))
          }
          names(colors_vector) <- unique_vals
          return(colors_vector)
        }
        
        min_val <- min(annot_values, na.rm = TRUE)
        max_val <- max(annot_values, na.rm = TRUE)
        
        # Handle case where all values are identical
        if (min_val == max_val) {
          # Create a small range around the value to avoid colorRamp2 error
          range_val <- abs(min_val) * 0.1 + 0.1
          breaks <- c(min_val - range_val, min_val, min_val + range_val)
        } else {
          breaks <- c(min_val, mean(annot_values, na.rm = TRUE), max_val)
        }
        
        color_function <- circlize::colorRamp2(
          breaks,
          c(annot_colors$colors[which(annot_colors$vals == "low")],
            annot_colors$colors[which(annot_colors$vals == "mid")],
            annot_colors$colors[which(annot_colors$vals == "high")])
        )
        return(color_function)
      }
    })
}

draw_multiome_HM <- function(HM) {
  draw(HM, 
       annotation_legend_side = 'bottom', 
       show_heatmap_legend = T, 
       heatmap_legend_side='bottom')
}

