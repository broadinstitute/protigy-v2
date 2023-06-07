################################################################################
# Module: SETUP SIDEBAR
# The main processGCT()` function and it's helpers
################################################################################

# function to parse, normalize, filter, etc. GCT file(s)
# INPUT: parameters list from setup, list of parsed GCTs
# OUTPUT: named list of processed GCTs, updated parameters
processGCTs <- function(GCTs, parameters) {
  
  message("\nProcessing GCTs...")
  
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
          
          # also wrap everything in a withProgress
          withProgress(
            min = 0,
            max = 6, # number of preprocessing steps
            message = paste0("Processing ", ome, ":"),
            expr = {
              ## validate GCT
              gct <- validateGCT(gct)
              
              ## extract data and parameters
              cdesc <- gct@cdesc
              rdesc <- gct@rdesc
              data <- gct@mat
              params <- parameters[[ome]]
              
              ## remove unnecesary elements from parameters
              if (!params$group_normalization) {
                params$group_normalization_column <- NULL
              }
              if (params$data_filter != "StdDev") {
                params$data_filter_sd_pct <- NULL
              }
              
              incProgress(1, detail = "log transformation")
              
              ## log transformation
              output_list <- perform_log_transformation(data, params$log_transformation)
              data.log.trans <- output_list$data.log.trans
              params$log_transformation <- output_list$updated_method
              
              incProgress(1, detail = "normalization")
              
              ## data normalization
              output_list <- perform_data_normalization(
                data = data.log.trans, 
                method = params$data_normalization,
                perform.group.normalization = params$group_normalization,
                group.normalization.column = params$group_normalization_column,
                cdesc = cdesc)
              data.norm <- output_list$data.norm
              params$data_normalization <- output_list$updated_method
              
              incProgress(1, detail = "missing value filter")
              
              ## missing value filter
              data.missing.filtered <- perform_missing_filter(data.norm, params$max_missing)
              
              incProgress(1, detail = "standard deviation filter")
              
              ## data filter
              data.filtered <- perform_data_filtering(
                data = data.missing.filtered, 
                method = params$data_filter,
                group.column = params$annotation_column,
                cdesc = cdesc,
                sd.perc = params$data_filter_sd_pct)
              
              incProgress(1, detail = "compiling results")
    
              ## re-compine GCT and return
              processed_GCT <- GCT(cdesc = cdesc, 
                                   rdesc = rdesc,
                                   mat = data.filtered)
              
              return(list(processed_GCT = processed_GCT, params = params))
            }
          )
        }
      )
    })
  
  # have the whole output be NULL if there was an error
  if (any(sapply(processing_out, is.null))) {
    output <- NULL
    
  # otherwise, pull out the GCTs and updated parameters separately
  } else {
    GCTs_processed <- sapply(processing_out, 
                             function(ome) ome$processed_GCT,
                             simplify = FALSE)
    parameters_updated <- sapply(processing_out,
                                 function(ome) ome$params,
                                 simplify = FALSE)
    
    output <- list(
      GCTs = GCTs_processed,
      parameters = parameters_updated
    )
  }
  
  message("\nDone with GCT processing!")
  
  return(output)
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
      
      # warning if there is any level in groups.vector with only one element
      freq_count <- aggregate(groups.vector, list(element = groups.vector), length)[[2]]
      if (any(freq_count == 1)) {
        warning(
          "One or more levels in the group normalization column only contain ",
          "one element. Consider group normalizing by a different column.")
      }
      
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
perform_data_filtering <- function(data, method, group.column, cdesc, sd.perc) {
  if (method == "None") {
    data.filtered <- data
    
  } else if (method == "StdDev") {
    # turn data into the expected format
    data_with_id <- data.frame(data, id = rownames(data))
    
    # get the groups vector
    group.vec <- cdesc[[group.column]]
    names(group.vec) <- rownames(cdesc)
    
    # filter data
    filtering_out <- sd.filter(
      tab = data_with_id, 
      grp.vec = group.vec, 
      id.col = 'id',
      sd.perc = sd.perc)
    
    # get the output
    tab <- filtering_out$table
    data.filtered <- as.matrix(tab[, setdiff(names(tab), 'id')])
    
  } else {
    stop("Invalid data filter selected")
  }
  
  return(data.filtered)
}

# validate GCT is the correct input
validateGCT <- function(gct) {
  mat <- gct@mat
  cdesc <- gct@cdesc
  rdesc <- gct@rdesc

  # check that rdesc matches row names
  if (!setequal(rownames(mat), rownames(rdesc))) {
    stop("GCT data row names not match `rdesc` row names.")
  }
  
  # check that cdesc matches column names
  if (!setequal(colnames(mat), rownames(cdesc))) {
    stop("GCT data column names does not match `cdesc` row names.")
  }
  
  # check for infinities
  if (any(is.infinite(mat))) {
    warning("Data contains infinite entries. Replacing these entries with NA.")
    mat[is.infinite(mat)] <- NA
  }
  
  # check for NaN's
  if (any(is.nan(mat))) {
    warning("Data contains NaN (Not a Number) entries. Replacing these entries with NA.")
    mat[is.nan(mat)] <- NA
  }
  
  # make sure cdesc/rdesc order matches data column/row names
  # warning here if rows/columns are misaligned?
  cdesc <- cdesc[colnames(mat), , drop = FALSE]
  rdesc <- rdesc[rownames(mat), , drop = FALSE]
  
  return(GCT(mat = mat, rdesc = rdesc, cdesc = cdesc))
}
