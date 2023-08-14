################################################################################
# Module: SUMMARY
# 
# Functions to generate plots and named lists of tables to display in summary
# module. 
################################################################################

## PLOTS

summary_quant_features <- function (gct, col_of_interest, ome, custom_color_map = NULL) {
  # get number of non-missing per sample
  sample_id <- colnames(gct@mat)
  non.missing <- as.data.frame(apply(gct@mat, 2, function(x) sum(!is.na(x))))
  names(non.missing) <- "numFeatures"
  non.missing$SampleID <- as.factor(as.character(rownames(non.missing)))
  non.missing$group <- as.factor(as.character(gct@cdesc[[col_of_interest]]))
  
  # reorder based on sampleID
  non.missing$SampleID <- with(non.missing, reorder(SampleID, as.integer(group)))

  # get color definition
  if (is.null(custom_color_map)) {
    color_definition <- NULL
  } else if (custom_color_map$is_discrete) {
    colors <- as.list(custom_color_map$colors)
    names(colors) <- custom_color_map$vals
    color_definition <- scale_fill_manual(values = colors)
  } else {
    non.missing$group <- as.numeric(non.missing$group)
    color_definition <- scale_fill_gradient2(
      low = custom_color_map$colors[which(custom_color_map$vals == "low")],
      mid = custom_color_map$colors[which(custom_color_map$vals == "mid")],
      high = custom_color_map$colors[which(custom_color_map$vals == "high")],
      midpoint = mean(min(non.missing$group), max(non.missing$group)),
      na.value = custom_color_map$colors[which(custom_color_map$vals == "na_color")]
    )
  }
  
  #get font size
  font.size <- scale_font_size(dimension=length(sample_id),max.size=12,scale.factor=55)
  
  # make plot
  g <- ggplot(data = non.missing, 
         aes(x = .data$SampleID, y = .data$numFeatures, fill = .data$group, 
             text = paste0("Sample ID: ", .data$SampleID, 
                           "\nNum. Features: ", .data$numFeatures))) +
    geom_bar(stat = 'identity') +
    theme_bw() +
    color_definition + 
    theme(text= element_text(size=12)) +
    ylab("# Quantified Features") +
    xlab("Sample columns") + 
    labs(fill = col_of_interest)  + 
    ggtitle(paste("Quantified features:", ome))
  
  # if font size is too small, hide the labels
  if(font.size < 8){
    g <- g + theme(axis.text.x = element_blank())
  }else{
    g <- g + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=font.size))
  }
  
}

summary_missing_value_distribution <- function(gct, missing_val_cutoff, ome) {
  
  # make a data frame of the percent missing values
  missing_val_perc <- apply(gct@mat, 1, function(x) sum(is.na(x)) / length(x) * 100)
  missing_val_df <- data.frame(missing = missing_val_perc)
  
  color_key <- c("red")
  names(color_key) <- paste0(missing_val_cutoff, "%")
  
  # generate plot
  gg <- ggplot(missing_val_df, text = "HELLO") +
    stat_ecdf(aes(missing), geom = "step", pad = FALSE, linewidth = 0.5) + 
    geom_vline(
      aes(xintercept = missing_val_cutoff, 
          color = names(color_key)), 
      show.legend = TRUE) +
    geom_segment(aes(x = max(missing), y = 1, xend = 100, yend = 1), 
                 linewidth = 0.5) +
    scale_color_manual(name = "Missing value cutoff", values = color_key) +
    scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    xlab("Percent Missing Allowed") + ylab("Percent Features Retained") +
    ggtitle(paste("Missing Value Distribution:", ome)) +
    theme_bw()+
    theme(text= element_text(size=12))
}

summary_missing_value_distribution_to_ggplotly <- function(gg) {
  ggply <- ggplotly(gg)
  
  # Add custom text
  num_total_features <- dim(gg$data)[1]
  ggply %>% 
    plotly::style(
      traces = 1,
      text = paste0("Missing: ", round(ggply$x$data[[1]]$x, 2), "%",
                    "</br></br>", 
                    "Features retained: ", 
                    round(ggply$x$data[[1]]$y * num_total_features))) %>%
    plotly::style(
      traces = 2,
      text = paste0("Missing value cutoff: ", ggply$x$data[[2]]$x, "%")) %>%
    plotly::style(
      traces = 3,
      text = paste0("Missing: ", 
                    round(ggply$x$data[[3]]$x, 2), "%",
                    "</br></br>",
                    "Features retained: ",
                    round(ggply$x$data[[3]]$y * num_total_features)))
}


## TABLES

# workflow parameters tables
summary_workflow <- function(params) {
  params_to_display <- list(
    "File name" = "gct_file_name",
    "Annotation column" = "annotation_column",
    "Intensity data" = "intensity_data",
    "Log transformation" = "log_transformation",
    "Data normalization" = "data_normalization",
    "Normalized by group" = "group_normalization",
    "Data filter" = "data_filter",
    "Max missing %" = "max_missing"
  )
    
  # include group normalization column
  if (params$group_normalization) {
    params_to_display <- append(
      params_to_display,
      list("Group normalization col." = "group_normalization_column"),
      after = which(params_to_display == "group_normalization"))
  }
  
  # include filtering percentile
  if (params$data_filter == "StdDev") {
    params_to_display <- append(
      params_to_display,
      list("Std. Dev. filter percentile" = "data_filter_sd_pct"),
      after = which(params_to_display == "data_filter"))
  }
  
  df <- t(as.data.frame(params))
  df <- df[as.character(params_to_display), , drop = FALSE]
  rownames(df) <- names(params_to_display)
  
  return(df)
}


# dataset information summary table
summary_dataset <- function(params, gct_original, gct_processed) {
  dataset_summary <- list(
    "Features (original)" = sum(
      apply(gct_original@mat, 1, function(x) sum(!is.na(x)) > 1)),
    "Features (post-filtering)" = sum(
      apply(gct_processed@mat, 1, function(x) sum(!is.na(x)) > 1)),
    "Expression columns" = dim(gct_processed@mat)[2],
    "Groups" = length(unique(gct_processed@cdesc[[params$annotation_column]]))
  )
  
  # check if there are any unquantified features
  unquantified_features <- apply(gct_processed@mat, 1, function(x) all(is.na(x)))
  if (any(unquantified_features)) {
    append(dataset_summary,
           list("Features w/o quantification" = sum(unquantified_features)),
           after = which(names(dataset_summary) == "Features (post-filtering)"))
  }
  
  # compile into a data frame
  df <- t(data.frame(dataset_summary))
  colnames(df) <- "Number"
  rownames(df) <- names(dataset_summary)
  
  return(df)
}

