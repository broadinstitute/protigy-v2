################################################################################
# Module: QC_correlation
#
# Produce correlation plots
################################################################################

## Create intra-group correlation boxplot

## Create correlation heatmap
create_corr_heatmap <- function (gct, col_of_interest, ome, custom_color_map = NULL, corr_method="pearson") {
  #browser()
  
  #sort matrix by annotation
  mat <- gct@mat
  group <- as.character(gct@cdesc[[col_of_interest]])
  annot <- data.frame('sample'=colnames(mat),"annot"=group)
  #replace NA with characters so that colors map appropriately
  annot$annot[is.na(annot$annot)]="NA"
  annot$annot <- as.factor(annot$annot)
  mat <- mat[,order(annot$annot)]
  annot <- annot[order(annot$annot),]
  colnames(annot)[2] = col_of_interest
  
  #calculate correlation matrix
  #pearson is default - allows us to change later if we want
  corr <- cor(mat, use="pairwise.complete.obs", method=corr_method)
  
  #get colors if defined
  #otherwise just use default colors
  #NOTE: need to add NA as a color or else it doesn't show up properly in the legend
  if (custom_color_map$is_discrete) {
    colors <- c(unlist(custom_color_map$colors),"gray50")
    names(colors) <- c(custom_color_map$vals,"NA")
  } else{
    colors = NULL
  }
  
  #create heatmap using ComplexHeatmap
  
  # Column annotation
  ha <- HeatmapAnnotation(
    annot = annot[,col_of_interest],
    col = list(annot = colors),
    show_legend = F,
    show_annotation_name = F
  )
  
  #Row annotation
  row_ha <- rowAnnotation(
    annot = annot[,col_of_interest],
    col = list(annot = colors),
    show_annotation_name=F,
    annotation_legend_param = list(annot = list(title=col_of_interest))
  )
  
  #color scale
  col_fun = colorRamp2(c(-1,-0.5,0,0.5,1), 
                            c("#2166ac","#92c5de","white","#fddbc7","#b2182b"))
  
  HM <- Heatmap(corr,
                heatmap_legend_param = list(
                  title = paste0("correlation (",corr_method,")"),
                  legend_direction = "vertical",
                  legend_width = unit(50, "mm"),
                  at = c(-1,-0.5,0,0.5,1),
                  col_fun=col_fun
                ),
                col=col_fun,
                row_title_rot = 0,
                cluster_rows = F,
                cluster_columns = F,
                row_split = annot[,col_of_interest],
                column_split = annot[,col_of_interest], 
                top_annotation = ha,
                left_annotation = row_ha,
                column_names_side = "top",
                column_gap = unit(1, "mm"),
                row_names_side="left",
                row_names_gp=grid::gpar(fontsize = 10),
                column_names_gp=grid::gpar(fontsize = 10),
                column_title=paste0("Correlation heatmap (",corr_method,")"),
                column_title_gp=grid::gpar(fontsize=18),
                row_title=NULL
  )
  
  return(list(HM=HM,Table=corr))
  
}

#draws the heatmap
draw_corr_HM <- function(HM) {
  draw(HM, 
       annotation_legend_side = 'right', 
       show_heatmap_legend = T, 
       heatmap_legend_side='right')
}


## function to dynamically determine the height (in px) of the heatmap
## depending on the number of genes
dynamicHeightHMCorr <- function(n.entries){
  height <- 0.3*(n.entries+12) + 3  ## height in inch
  height <- height * 36             ##3/4 inch  to pixel
  
  return(height)
}

## Create correlation boxplot

create_corr_boxplot <- function (gct, col_of_interest, ome, custom_color_map = NULL,corr_method="pearson",cor.matrix) {
  
  #sort matrix by annotation
  mat <- gct@mat
  group <- as.character(gct@cdesc[[col_of_interest]])
  annot <- data.frame('sample'=colnames(mat),"annot"=group)
  #replace NA with characters so that colors map appropriately
  annot$annot[is.na(annot$annot)]="NA"
  annot$annot <- as.factor(annot$annot)
  mat <- mat[,order(annot$annot)]
  annot <- annot[order(annot$annot),]
  
  #calculate correlation matrix
  #pearson is default - allows us to change later if we want
  corr <- cor(mat, use="pairwise.complete.obs", method=corr_method)
  
  # calculate the correlation for each subgroup
  # First, identify groups with sufficient samples for correlation
  group_counts <- table(annot$annot)
  valid_groups <- names(group_counts)[group_counts > 1]
  single_sample_groups <- names(group_counts)[group_counts == 1]
  
  # Warn user about groups with single samples
  if(length(single_sample_groups) > 0) {
    warning(paste("Groups with only one sample cannot be correlated and will be excluded:",
                  paste(single_sample_groups, collapse = ", ")))
  }
  
  # Only calculate correlations for groups with multiple samples
  if(length(valid_groups) == 0) {
    stop("No groups have more than one sample. Cannot calculate intra-group correlations.")
  }
  
  cor.group <- lapply(valid_groups, function(x){
    cm.grp <- corr[annot$annot==x,annot$annot==x]
    cm.grp <- cm.grp[upper.tri(cm.grp, diag = FALSE)]
    return(cm.grp)
  })
  names(cor.group) <- valid_groups
  plot.data <- stack(cor.group)
  
  # get color definition
  #NOTE: need to add NA as a color or else it doesn't show up properly in the legend
  if (is.null(custom_color_map)) {
    fill_definition <- NULL
  } else if (custom_color_map$is_discrete) {
    colors <- c(unlist(custom_color_map$colors),"gray50")
    names(colors) <- c(custom_color_map$vals,"NA")
    fill_definition <- scale_fill_manual(values = colors)
  } else {
    group <- as.numeric(group)
    fill_definition <- scale_fill_gradient2(
      low = custom_color_map$colors[which(custom_color_map$vals == "low")],
      mid = custom_color_map$colors[which(custom_color_map$vals == "mid")],
      high = custom_color_map$colors[which(custom_color_map$vals == "high")],
      midpoint = mean(min(group), max(group)),
      na.value = custom_color_map$colors[which(custom_color_map$vals == "na_color")]
    )
  }
  
  # make boxplot
  ggplot(data = plot.data,  
              aes(x = .data$ind,  
                  y = .data$values)) +
    geom_boxplot(aes(fill=.data$ind)) + 
    theme_bw() + #change theme
    fill_definition + #color scale
    theme(text= element_text(size=12)) + #change font sizes
    ylab(paste0("Correlation (",corr_method,")")) + #y axis title
    xlab(col_of_interest) + #x axis title
    labs(fill = col_of_interest) + #legend title
    ggtitle(paste0("Intra-group correlations (",corr_method,"): ",ome)) #plot title

}

