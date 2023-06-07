################################################################################
# Module: SUMMARY
# 
# Functions to generate plots and named lists of tables to display in summary
# module. 
################################################################################

## PLOTS

summary.quant.features <- function (gct, col_of_interest, ome) {
  # get number of non-missing per sample
  sample_id <- colnames(gct@mat)
  non.missing <- as.data.frame(apply(gct@mat, 2, function(x) sum(!is.na(x))))
  names(non.missing) <- "numFeatures"
  non.missing$SampleID <- as.factor(as.character(rownames(non.missing)))
  non.missing$group <- as.factor(as.character(gct@cdesc[[col_of_interest]]))
  
  non.missing$SampleID <- with(non.missing, reorder(SampleID, as.integer(group)))
  
  ggplot(data = non.missing, aes(x = SampleID, y = numFeatures, fill = group)) +
    geom_bar(stat = 'identity') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ylab("# Quantified Features") +
    xlab("Sample columns") + 
    labs(fill = col_of_interest)  + 
    ggtitle(paste("Quantified features:", ome))
}

summary_missing_value_distribution <- function(gct, missing_val_cutoff, ome) {
  # make a data frame of the percept missing values
  missing_val_perc <- apply(gct@mat, 1, function(x) sum(is.na(x)) / length(x) * 100)
  missing_val_df <- data.frame(missing = missing_val_perc)
  
  color_key <- c("red")
  names(color_key) <- paste0(missing_val_cutoff, "%")
  
  # generate plot
  ggplot(missing_val_df, aes(x = missing)) +
    stat_bin(aes(y = cumsum(after_stat(density))/sum(after_stat(density))*100), 
             fill = "grey70", color = "black", alpha = 0.8, bins = 30) +
    geom_vline(
      aes(xintercept = missing_val_cutoff, 
          color = names(color_key)), 
      size = 1.5, 
      show.legend = TRUE) +
    scale_color_manual(name = "Missing val. cutoff", values = color_key) +
    xlab("% Missing Allowed") + ylab("% Features Kept") +
    ggtitle(paste("Missing Value Distribution:", ome))
}


## TABLES

# workflow tables list
generate_summary_workflows_list <- function(parameters, all_omes) {
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
  
  sapply(all_omes, function(ome) {
    params <- parameters[[ome]]
    
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
    df
  }, simplify = FALSE)
}

# dataset summary list
generate_summary_dataset_list <- function(parameters, all_omes, 
                                          GCTs_processed, GCTs_original) {
  
  sapply(all_omes, function(ome) {
    gct_original <- GCTs_original[[ome]]
    gct_processed <- GCTs_processed[[ome]]
    params <- parameters[[ome]]
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
    df
  }, simplify = FALSE)
  
}