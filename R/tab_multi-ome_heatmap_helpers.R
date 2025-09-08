## preprocess and merge GCT files (simplified version without VM logic)
preprocess_gcts_multiome_heatmap <- function(GCTs, setup_inputs) {
  
  message("\nPreprocessing GCTs for multi-ome data viewer heatmap")
  
  # extract setup inputs
  labels <- setup_inputs$labels
  datatypes <- setup_inputs$datatypes
  geneSymbol_columns <- setup_inputs$geneSymbol_columns
  
  # show progress in progress bar
  if (shiny::isRunning()) incProgress()
  
  ## add data label and ID to gct
  gcts_processed <- lapply(1:length(GCTs), function(i) {
    # get gct object, data label, and geneSymbol column name
    gct <- GCTs[[i]]
    label <- labels[i]
    datatype <- datatypes[i]
    geneSymbol_column <- geneSymbol_columns[i]
    
    # get default gene symbol column for SpectrumMill
    if (datatype == 'SpectrumMill') {
      geneSymbol_column <- 'geneSymbol'
    }
    
    # condense rdesc to only necessary information
    rdesc <- gct@rdesc
    rdesc_new <- data.frame(ID = paste0(gct@rid, '.', i),
                            geneSymbol = rdesc[[geneSymbol_column]],
                            DataType = rep(label, dim(rdesc)[1]))
    rownames(rdesc_new) <- rdesc_new$ID
    
    # make sure Sample.ID is in cdesc
    cdesc <- gct@cdesc
    cdesc$Sample.ID <- gct@cid
    
    # change gct mat rownames to match rdesc_new rownames
    gct_mat <- mat(gct)
    rownames(gct_mat) <- paste0(rownames(gct_mat), '.', i)
    
    # create new GCT with updated rdesc
    gct_new <- new('GCT', 
                   mat = gct_mat, 
                   rdesc = rdesc_new,
                   cdesc = cdesc)
    
    # show progress in progress bar
    if (shiny::isRunning()) incProgress()
    
    return(gct_new)
  })
  
  ## merge all gct's into one large one
  merged_gct <- Reduce(function(gct1, gct2) merge_gct(gct1, gct2, dim='row'), 
                       gcts_processed)
  
  # show progress in progress bar
  if (shiny::isRunning()) incProgress()
  
  
  
  return(merged_gct)
}

## make complex heatmap
## calls global variables merged_rdesc, merged_mat, sample_anno
myComplexHeatmap <- function(
    params, GENEMAX, merged_rdesc, merged_mat, sample_anno, custom_colors) {
  
  # extract parameters
  genes.char <- params$genes.char
  zscore <- params$zscore
  min.val <- params$min.val
  max.val <- params$max.val
  sort.after <- params$sort.after
  show.sample.label <- params$show.sample.label
  ome.order <- params$ome.order
  max.levels <- params$max.levels
  
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
  genes.Table <- genes.Table[ ,c(1:3, order(names(genes.Table)[-(1:3)])+3)]
  
  # re-order the omes
  genes.Table$ome <- factor(genes.Table$ome, ome.order)
  
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
  anno.fig <- anno.fig[, -(which(names(anno.fig) == "Sample.ID")), drop = FALSE]

  
  # get custom color palette
  anno.fig.color <- custom_colors[names(anno.fig)]
  
  # filter for only annotations that are continuous or less than max.levels 
  anno.keep <- names(which(sapply(anno.fig.color, function(anno_colors) {
    is.function(anno_colors) | length(anno_colors) <= max.levels
  })))
  anno.fig.color <- anno.fig.color[anno.keep]
  anno.fig <- anno.fig[, anno.keep, drop = FALSE]
  
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
  
  # make final heatmap for genes
  HM <- Heatmap(genes.Matrix,
                col = col_fun_ratios,
                column_title = "Sample",
                row_title_rot = 0,
                row_order = order(genes.Table$ome, genes.Table$row_label),
                cluster_rows = F,
                cluster_columns = F,
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
                column_gap = if (is.numeric(column.to.sort)) {unit(0, "mm")} else {unit(1, "mm")},
                heatmap_legend_param = list(direction='horizontal',
                                            max_width = 350,
                                            legend_width = unit(4, 'cm')))
  
  return(list(HM=HM, Table=final.Table))
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

## function to convert global custom colors to the ComplexHeatmap structure
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
        annot_values <- as.numeric(sample_anno[[annot_name]])
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

