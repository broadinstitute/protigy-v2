################################################################################
# Module: QC_profile_plots
#
# Produce profile plots before and after normalization
################################################################################

## Create profile plot using ggplot (geom_density)

create_profile_plot <- function (gct, col_of_interest, ome, custom_color_map = NULL, parameters, type=c("org","norm")) {
  
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
    color_definition <- NULL
  } else if (custom_color_map$is_discrete) {
    colors <- c(unlist(custom_color_map$colors),"gray50")
    names(colors) <- c(custom_color_map$vals,NA)
    color_definition <- scale_colour_manual(values = colors)
  } else {
    group <- as.numeric(group)
    color_definition <- scale_colour_gradient2(
      low = custom_color_map$colors[which(custom_color_map$vals == "low")],
      mid = custom_color_map$colors[which(custom_color_map$vals == "mid")],
      high = custom_color_map$colors[which(custom_color_map$vals == "high")],
      midpoint = mean(min(group), max(group)),
      na.value = custom_color_map$colors[which(custom_color_map$vals == "na_color")]
    )
  }
  
  # make plot
  
  # if type=norm but no normalization, make an empty ggplot with appropriate title
  if(type=="norm" & parameters$data_normalization=="None"){
    g <- ggplot() + theme_void() +
      ggtitle(paste("No normalization was performed for",ome)) +
      theme(text= element_text(size=12))
  } else{
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
      ggtitle(ifelse(type=="org", paste("Profile plot before normalization and filtering:", ome), paste("Profile plot after normalization and filtering:",ome))) #plot title
  }
}
