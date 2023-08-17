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
  
  #get font size
  font.size <- scale_font_size(dimension=dim(corr)[1], max.size = 8, scale.factor=60)
  
  #if font size is too small, don't show
  if(font.size<8){
    show_names=F
  }else{
    show_names=T
  }
  
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
  
  #Heatmap height (to return)
  a <- unit(3, "mm")
  height <- nrow(corr) * a
  
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
  
  HM <- Heatmap(corr,
                heatmap_legend_param = list(
                  title = paste0("correlation (",corr_method,")"),
                  legend_direction = "vertical",
                  legend_width = unit(50, "mm")
                ),
                row_title_rot = 0,
                cluster_rows = F,
                cluster_columns = F,
                row_split = annot[,col_of_interest],
                column_split = annot[,col_of_interest], 
                top_annotation = ha,
                left_annotation = row_ha,
                show_row_names = show_names,
                show_column_names = show_names,
                column_names_side = "top",
                column_gap = unit(1, "mm"),
                row_names_side="left",
                row_names_gp=grid::gpar(fontsize = font.size),
                column_names_gp=grid::gpar(fontsize = font.size),
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
  height <- height * 24             ##1/2 inch  to pixel
  
  return(height)
}

## Create correlation boxplot

create_corr_boxplot <- function (gct, col_of_interest, ome, custom_color_map = NULL) {
  
  # convert to long format
  mat <- gct@mat
  mat_long <- as.data.frame(mat) %>% pivot_longer(everything(),names_to="sample",values_to="values")
  
  # add annotation information
  group <- as.factor(as.character(gct@cdesc[[col_of_interest]]))
  annot <- data.frame("sample"=colnames(gct@mat),"annot"=group)
  mat_long <- left_join(mat_long,annot,by="sample")
  
  #sort by annotation
  mat_long <- mat_long[order(mat_long$annot),]
  mat_long$sample <- factor(mat_long$sample, levels=unique(mat_long$sample))
  
  # get color definition
  #NOTE: need to add NA as a color or else it doesn't show up properly in the legend
  if (is.null(custom_color_map)) {
    color_defintion <- NULL
  } else if (custom_color_map$is_discrete) {
    colors <- c(unlist(custom_color_map$colors),"gray50")
    names(colors) <- c(custom_color_map$vals,NA)
    color_definition <- scale_colour_manual(values = colors)
  } else {
    group <- as.numeric(group)
    fill_definition <- scale_colour_gradient2(
      low = custom_color_map$colors[which(custom_color_map$vals == "low")],
      mid = custom_color_map$colors[which(custom_color_map$vals == "mid")],
      high = custom_color_map$colors[which(custom_color_map$vals == "high")],
      midpoint = mean(min(group), max(group)),
      na.value = custom_color_map$colors[which(custom_color_map$vals == "na_color")]
    )
  }
  
  # make plot
  
  # if type=norm but no normalization, make an empty ggplot with appropriate title
    g <- ggplot(data = mat_long, 
                aes(x = .data$values,  
                    colour = .data$annot,
                    text = paste0("Sample ID: ", .data$sample))) +
      geom_line(stat="density",alpha=0.7) + #make density plot
      theme_bw() + #change theme
      color_definition + #color scale
      theme(text= element_text(size=12)) + #change font sizes
      ylab("Density") + #y axis title
      xlab("Expression") + #x axis title
      labs(colour = col_of_interest) + #legend title
      ggtitle("Intra-group correlations") #plot title
}

