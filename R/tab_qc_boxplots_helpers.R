################################################################################
# Module: QC_BOXPLOTS
#
# Produce boxplots before and after normalization
################################################################################

## Create boxplot using ggplot on pre-computed summary statistics
## NOTE: This DOES NOT work with plotly due to an ongoing issue with plotly not correctly interpreting geom_boxplot(stat="identity"). If using this, you MUST render as a ggplot and NOT as a ggplotly!

create_boxplot <- function (gct, col_of_interest, ome, custom_color_map = NULL, parameters, type=c("org","norm")) {
  
  # compute box plot statistics
  mat <- apply(gct@mat, 2, function(x) boxplot.stats(x))
  
  # get the statistics
  stats <- as.data.frame(t(sapply(mat,"[[","stats")))
  colnames(stats) <- c("ymin","lower","middle","upper","ymax")
  stats$sample <- rownames(stats)
  
  # add annotation information
  group <- as.factor(as.character(gct@cdesc[[col_of_interest]]))
  annot <- data.frame("sample"=colnames(gct@mat),"annot"=group)
  stats <- left_join(stats,annot,by="sample")
  
  #get the outliers and add annotation information
  outliers <- sapply(mat,"[[","out")
  outliers <- stack(outliers)
  colnames(outliers) <- c("values","sample")
  outliers <- left_join(outliers,annot,by="sample")
  
  #sort by annotation
  stats <- stats[order(stats$annot),]
  outliers <- outliers[order(outliers$annot),]
  stats$sample <- factor(stats$sample, levels=unique(stats$sample))
  outliers$sample <- factor(outliers$sample, levels=unique(outliers$sample))
  
  # get color definition
  #NOTE: geom_tufteboxplot doesn't like colors as lists, but can take a named vector of colors for whatever reason
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
  
  #get font size
  font.size <- scale_font_size(dimension=dim(stats)[1])
  
  # make plot
  
  # if type=norm but no normalization, make an empty ggplot with appropriate title
  if(type=="norm" & parameters$data_normalization=="None"){
    g <- ggplot() + theme_void() +
      ggtitle(paste("No normalization performed for",ome)) +
      theme(text= element_text(size=14))
  } else{
    g <- ggplot(data = stats,    #base boxplot with calculated stats
                aes(x = .data$sample,  
                    ymin = .data$ymin,
                    lower = .data$lower,
                    middle = .data$middle,
                    upper = .data$upper,
                    ymax = .data$ymax),show.legend=F) +
      geom_tufteboxplot(stat="identity",aes(colour=.data$annot),show.legend=F) + #convert to tufte boxplot
      geom_point(size = 1, aes(y = .data$middle, colour = .data$annot))+ #make the median point bigger (and fixes the legend too!)
      geom_point(data=outliers, aes(x=.data$sample, y=.data$values), inherit.aes=FALSE, size=0.1, pch=1, show.legend=F) + #add outliers
      theme_bw() + #change theme
      color_definition + #color scale
      theme(text= element_text(size=14)) + #change font sizes
      ylab("Expression") + #y axis title
      xlab("Sample") + #x axis title
      labs(colour = col_of_interest) + #legend title
      ggtitle(ifelse(type=="org", paste("Boxplot before normalization:", ome), paste("Boxplot after normalization:",ome))) #plot title
    
    # if font size is too small, hide the labels
    if(font.size < 8){
      g <- g + theme(axis.text.x = element_blank())
    }else{
      g <- g + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=font.size))
    }
  }
  
  print(g)
  
}
