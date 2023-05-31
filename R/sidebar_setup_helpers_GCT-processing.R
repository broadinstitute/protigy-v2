################################################################################
# Module: SETUP SIDEBAR
# The main processGCT()` function and it's helpers
################################################################################

# function to parse, normalize, filter, etc. GCT file(s)
# INPUT: parameters list from setup, list of parsed GCTs
# OUTPUT: named list of processed GCTs, updated parameters
processGCT <- function(GCTs, parameters) {
  
  message("\nProcessing GCTs...")
  
  # TODO: validate GCTs
  
  processing_out <- mapply(
    GCTs, names(GCTs),
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE,
    FUN = function(gct, ome) {
      
      # wrap everything in a try/catch statement
      my_shinyalert_tryCatch(
        text.warning = paste0("<b>Warning in ", ome, ":</b>"),
        append.warning = TRUE,
        text.error = paste0("<b>Error in ", ome, ":</b>"),
        append.error = TRUE,
        return.error = NULL,
        
        expr = {
          cdesc <- gct@cdesc
          rdesc <- gct@rdesc
          data <- gct@mat
          params <- parameters[[ome]]
          
          ## log transformation
          output_list <- perform_log_transformation(data, params$log_transformation)
          data.log.trans <- output_list$data.log.trans
          params$log_transformation <- output_list$updated_method
          
          ## data normalization
          output_list <- perform_data_normalization(
            data = data.log.trans, 
            method = params$data_normalization,
            perform.group.normalization = params$group_normalization,
            group.normalization.column = params$group_normalization_column,
            cdesc = cdesc)
          data.norm <- output_list$data.norm
          params$data_normalization <- output_list$updated_method
          
          
          ## missing value filter
          data.missing.filtered <- perform_missing_filter(data.norm, params$max_missing)
          
          
          ## data filter
          data.filtered <- perform_data_filtering(data.missing.filtered, params$data_filter)
          

          ## re-compine GCT and return
          GCT(cdesc = cdesc, 
              rdesc = rdesc,
              mat = data.filtered)
        }
      )
    })
  
  # have the whole output be NULL if there was an error
  if (any(sapply(processing_out, is.null))) {
    processing_out <- NULL
  }
  
  # return processed GCT files
  message("\nDone with GCT processing!")
  return(processing_out)
}

# perform log transformation
perform_log_transformation <- function(data, method) {
  if (method == "None") {
    data.log.transform <- data
    
    #if there are negative values in the matrix, do not log transform!
  } else if (any(data < 0, na.rm = T)) {
    warning(paste0("Dataset contains negative values! ", 
                   "Analysis will proceed WITHOUT log-transformation. ",
                   "If you wish to log-transform, please re-upload a ",
                   "dataset without negative values."))
    
    # don't do log transformation, update parameters
    method <- "None"
    data.log.transform <- data
    
    # log 2 transformation
  } else if (method == 'log2') {
    data[data == 0] <- NA
    data.log.transform <- log(data, 2)
    
    # log 10 transformation
  } else if (method == 'log10') {
    data[data == 0] <- NA
    data.log.transform <- log(data, 10)
    
  } else {
    stop("Invalid log transformation selection: ", method)
  }
  
  return(list(data.log.transform = data.log.transform,
              updated_method = method))
}

# perform data normalization
perform_data_normalization <- function(data, method, cdesc,
                                       perform.group.normalization,
                                       group.normalization.column) {
  if (method == "None") {
    data.norm <- data
  } else {
    
    if (perform.group.normalization) {
      # get groups vector
      groups.vector <- cdesc[[group.normalization.column]]
      names(groups.vector) <- rownames(cdesc)
      
      # perform group-wise normalization
      data.norm <- normalize.data(data, method, groups.vector)
    } else {
      
      # perform regular normalization
      data.norm <- normalize.data(data, method)
    }

    # if two-component norm fails....
    if(inherits(data.norm, 'try-error')){
      # reset to no normalization
      data.norm <- data
      method <- "None"
      
      # send out a warning
      # the HTML will be rendered as part of a shinyalert
      warning(paste(
        '<b>Warning in normalization for', ome, '</b><br>',
        'The two-component normalization failed to converge on at least one',
        'data column. Please note that this type of normalization expects',
        '<b>log-ratio</b> data that is approximately <b>centered around',
        'zero</b>. Please make sure this is the case by <b>inspecting the',
        'profile plots</b> under the QC tab.'))
    }
  }
  
  return(list(data.norm = data.norm,
              updated_method = method))
}

# maximum missing value filter
perform_missing_filter <- function(data, max_missing) {
  missing_percent <- apply(data, 1, function(x) sum(is.na(x))/length(x))
  data <- data[missing_percent <= max_missing, ]
  return(data)
}

# perform data filtering
perform_data_filtering <- function(data, method) {
  if (method == "None") {
    data.filtered <- data
  } else if (method == "Reproducibility") {
    #data.filtered <- my.reproducibility.filter(data.norm, groups.vector)
    warning("DATA FILTER NOT COMPLETED")
    data.filtered <- data
  } else if (method == "StdDev") {
    warning("DATA FILTER NOT COMPLETED")
    data.filtered <- data
  } else {
    stop("Invalid data filter selected")
  }
  
  return(data.filtered)
}


