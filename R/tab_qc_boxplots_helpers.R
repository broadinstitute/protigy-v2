################################################################################
# Module: QC_BOXPLOTS
#
# Produce boxplots before and after normalization
################################################################################

## Create boxplot using ggplot on pre-computed summary statistics
## NOTE: This DOES NOT work with plotly due to an ongoing issue with plotly not correctly interpreting geom_box(stat="identity). If using this, you MUST render as a ggplot and NOT as a ggplotly!

create_boxplot <- function (gct, col_of_interest, ome, custom_color_map = NULL, type=c("org","norm")) {
  
  #browser()
  
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
  
  #sort by annotation
  stats <- stats[order(stats$annot),]
  stats$sample <- factor(stats$sample, levels=unique(stats$sample))
  
  #get the outliers
  #outliers <- sapply(data,"[[","out")
  #outliers <- Reduce(rbind,outliers)
  
  # get color definition
  if (is.null(custom_color_map)) {
    color_definition <- NULL
  } else if (custom_color_map$is_discrete) {
    colors <- as.list(custom_color_map$colors)
    names(colors) <- custom_color_map$vals
    color_definition <- scale_fill_manual(values = colors)
  } else {
    group <- as.numeric(group)
    color_definition <- scale_fill_gradient2(
      low = custom_color_map$colors[which(custom_color_map$vals == "low")],
      mid = custom_color_map$colors[which(custom_color_map$vals == "mid")],
      high = custom_color_map$colors[which(custom_color_map$vals == "high")],
      midpoint = mean(min(group), max(group)),
      na.value = custom_color_map$colors[which(custom_color_map$vals == "na_color")]
    )
  }
  
  # make plot
  ggplot(data = stats, 
         aes(x = sample,  
             ymin = .data$ymin,
             lower = .data$lower,
             middle = .data$middle,
             upper = .data$upper,
             ymax = .data$ymax,
             fill = .data$annot)) +
    geom_boxplot(stat="identity") +
    theme_bw() +
    color_definition + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          text= element_text(size=14)) +
    ylab("Expression") +
    xlab("Sample") + 
    labs(fill = col_of_interest) +
    ggtitle(if(type=="org"){
      paste("Boxplot before normalization:", ome)
    }else{
      paste("Boxplot after normalization:",ome)
    })
}