################################################################################
# Module: SETUP SIDEBAR
# The main processGCT()` function and it's helpers
################################################################################

# Fix gene symbol column formatting
# Replaces semicolons with pipes, converts blank symbols to NA, and cleans up formatting
# Blank geneSymbol values are converted to NA and kept (rows are not removed)
# INPUT: rdesc data frame with geneSymbol column
# OUTPUT: updated rdesc with fixed geneSymbol column, and empty vector (for backward compatibility)
fix_gene_symbols <- function(rdesc) {
  if (!"geneSymbol" %in% names(rdesc)) {
    return(list(rdesc = rdesc, removed_rids = character(0)))
  }
  
  # Store original row names (for backward compatibility - no rows are removed)
  original_rids <- rownames(rdesc)
  
  # Convert geneSymbol to character vector if it's a list or other type
  if (is.list(rdesc$geneSymbol)) {
    rdesc$geneSymbol <- unlist(lapply(rdesc$geneSymbol, function(x) {
      if (is.null(x) || length(x) == 0) return(NA_character_)
      paste(as.character(x), collapse = "|")
    }))
  }
  rdesc$geneSymbol <- as.character(rdesc$geneSymbol)
  
  # Replace semicolons with pipes
  rdesc$geneSymbol <- gsub(";", "|", rdesc$geneSymbol)
  
  # Remove blank gene symbols within strings (e.g., "EGFR| |" -> "EGFR")
  # Split by pipe, remove empty/whitespace-only entries, rejoin with pipe
  rdesc$geneSymbol <- vapply(rdesc$geneSymbol, function(x) {
    if (is.na(x) || x == "") return(NA_character_)
    parts <- strsplit(x, "\\|", fixed = FALSE)[[1]]
    parts <- parts[trimws(parts) != ""]  # Remove blank/whitespace-only parts
    result <- paste(parts, collapse = "|")
    # If result is empty after cleaning, return NA instead of empty string
    if (result == "") return(NA_character_)
    return(result)
  }, character(1))
  
  # Convert any remaining blank gene symbols to NA (keep all rows)
  # Blank gene symbols are valid - convert to NA but don't remove rows
  rdesc$geneSymbol[rdesc$geneSymbol == ""] <- NA_character_
  
  # No rows should be removed - blank geneSymbol values are converted to NA
  removed_rids <- character(0)
  
  # Remove any starting | characters (only for non-NA values)
  if (nrow(rdesc) > 0) {
    non_na_mask <- !is.na(rdesc$geneSymbol)
    if (any(non_na_mask)) {
      start_str <- substring(rdesc$geneSymbol[non_na_mask], 1, 1)
      start_pipe_mask <- start_str == "|"
      if (any(start_pipe_mask)) {
        rdesc$geneSymbol[non_na_mask][start_pipe_mask] <- substring(
          rdesc$geneSymbol[non_na_mask][start_pipe_mask], 2
        )
        # Convert to NA if result is empty
        empty_after_start <- rdesc$geneSymbol[non_na_mask][start_pipe_mask] == ""
        if (any(empty_after_start)) {
          na_indices <- which(non_na_mask)[start_pipe_mask][empty_after_start]
          rdesc$geneSymbol[na_indices] <- NA_character_
        }
      }
      
      # Remove any ending | characters (only for non-NA values)
      non_na_mask <- !is.na(rdesc$geneSymbol)
      if (any(non_na_mask)) {
        gene_values <- rdesc$geneSymbol[non_na_mask]
        end_str <- vapply(gene_values, function(x) {
          if (nchar(x) > 0) substring(x, nchar(x), nchar(x)) else ""
        }, character(1))
        end_pipe_mask <- end_str == "|"
        if (any(end_pipe_mask)) {
          rdesc$geneSymbol[non_na_mask][end_pipe_mask] <- vapply(
            rdesc$geneSymbol[non_na_mask][end_pipe_mask], 
            function(x) {
              if (nchar(x) > 1) substring(x, 1, nchar(x) - 1) else ""
            }, 
            character(1)
          )
          # Convert to NA if result is empty
          empty_after_end <- rdesc$geneSymbol[non_na_mask][end_pipe_mask] == ""
          if (any(empty_after_end)) {
            na_indices <- which(non_na_mask)[end_pipe_mask][empty_after_end]
            rdesc$geneSymbol[na_indices] <- NA_character_
          }
        }
      }
    }
  }
  
  return(list(rdesc = rdesc, removed_rids = removed_rids))
}

# Apply sample-level filtering using cdesc column values.
# Selected values are always kept; all other values are discarded.
apply_sample_filter <- function(data, cdesc, params, ome) {
  if (!isTRUE(params$sample_filter_enabled)) {
    return(list(data = data, cdesc = cdesc))
  }

  filter_column <- params$sample_filter_column
  filter_values <- params$sample_filter_values
  if (is.null(filter_column) || identical(filter_column, "")) {
    stop("Sample filtering is enabled, but no sample filter column was selected.")
  }
  if (!(filter_column %in% names(cdesc))) {
    stop("Sample filter column '", filter_column, "' was not found in cdesc for ", ome, ".")
  }
  if (is.null(filter_values) || length(filter_values) == 0) {
    stop("Sample filtering is enabled, but no filter values were selected for ", ome, ".")
  }

  filter_values <- as.character(filter_values)
  keep_samples <- as.character(cdesc[[filter_column]]) %in% filter_values
  keep_ids <- rownames(cdesc)[keep_samples]
  if (length(keep_ids) == 0) {
    stop("No samples remain after filtering ", ome, " by ", filter_column, ".")
  }

  data <- data[, keep_ids, drop = FALSE]
  cdesc <- cdesc[keep_ids, , drop = FALSE]

  return(list(data = data, cdesc = cdesc))
}

# Apply row-level filtering using rdesc column values.
# Selected values are always kept; all other values are discarded.
apply_row_filter <- function(data, rdesc, params, ome) {
  if (!isTRUE(params$row_filter_enabled)) {
    return(list(data = data, rdesc = rdesc))
  }

  filter_column <- params$row_filter_column
  filter_values <- params$row_filter_values
  if (is.null(filter_column) || identical(filter_column, "")) {
    stop("Row filtering is enabled, but no row filter column was selected.")
  }
  if (!(filter_column %in% names(rdesc))) {
    stop("Row filter column '", filter_column, "' was not found in rdesc for ", ome, ".")
  }
  if (is.null(filter_values) || length(filter_values) == 0) {
    stop("Row filtering is enabled, but no filter values were selected for ", ome, ".")
  }

  filter_values <- as.character(filter_values)
  keep_rows <- as.character(rdesc[[filter_column]]) %in% filter_values
  keep_ids <- rownames(rdesc)[keep_rows]
  if (length(keep_ids) == 0) {
    stop("No rows remain after filtering ", ome, " by ", filter_column, ".")
  }

  data <- data[keep_ids, , drop = FALSE]
  rdesc <- rdesc[keep_ids, , drop = FALSE]

  return(list(data = data, rdesc = rdesc))
}

# function to transform original GCT file so it is comparable to processed GCT file
# INPUT: parameters list from setup, list of parsed GCTs
# OUTPUT: transformed GCTs without filtering or normalization
transformGCTs <- function(GCTs, parameters) {
  
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

              ## row filtering
              row_filter_out <- apply_row_filter(
                data = data,
                rdesc = rdesc,
                params = params,
                ome = ome
              )
              data <- row_filter_out$data
              rdesc <- row_filter_out$rdesc

              ## sample filtering
              sample_filter_out <- apply_sample_filter(
                data = data,
                cdesc = cdesc,
                params = params,
                ome = ome
              )
              data <- sample_filter_out$data
              cdesc <- sample_filter_out$cdesc
              
              ## handle gene symbol column selection
              gene_symbol_col <- params$gene_symbol_column
              
              # If geneSymbol already exists in input, preserve it unless user explicitly selects a different column
              if ("geneSymbol" %in% names(rdesc)) {
                # User selected a different column - preserve original as geneSymbol_original
                if (!is.null(gene_symbol_col) && gene_symbol_col != "None" && 
                    gene_symbol_col != "geneSymbol" && gene_symbol_col %in% names(rdesc)) {
                  warning("Gene symbol column already exists. Original geneSymbol column will be preserved as 'geneSymbol_original'. The selected column will also be preserved in the dataset.")
                  rdesc$geneSymbol_original <- rdesc$geneSymbol
                  rdesc$geneSymbol <- rdesc[[gene_symbol_col]]
                  # Preserve the selected column (don't remove it)
                }
                # If user selected "None" or geneSymbol itself, keep existing geneSymbol
                # (no action needed - geneSymbol already exists)
              } else if (!is.null(gene_symbol_col) && gene_symbol_col != "None" && gene_symbol_col %in% names(rdesc)) {
                # geneSymbol doesn't exist - create it from selected column
                # Preserve the original column (don't remove it)
                rdesc$geneSymbol <- rdesc[[gene_symbol_col]]
              }
              # If geneSymbol doesn't exist and user selected "None" or column doesn't exist, geneSymbol won't be created
              
              ## fix gene symbol formatting (replace semicolons with pipes, clean up)
              if ("geneSymbol" %in% names(rdesc)) {
                fix_result <- fix_gene_symbols(rdesc)
                rdesc <- fix_result$rdesc
                # No rows are removed - blank geneSymbol values are converted to NA
                # removed_rids is kept for backward compatibility but should be empty
              }
              
              incProgress(1, detail = "log transformation")
              
              ## log transformation
              output_list <- perform_log_transformation(data, params$log_transformation)
              data.log.trans <- output_list$data.log.transform
              params$log_transformation <- output_list$updated_method
              
              ## re-combine GCT and return
              transformed_GCT <- GCT(cdesc = cdesc, 
                                   rdesc = rdesc,
                                   mat = data.log.trans,
                                   cid=colnames(data.log.trans),
                                   rid=rownames(data.log.trans))
              
              return(transformed_GCT)
            }
          )
        }
      )
    })
}

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

              ## row filtering
              row_filter_out <- apply_row_filter(
                data = data,
                rdesc = rdesc,
                params = params,
                ome = ome
              )
              data <- row_filter_out$data
              rdesc <- row_filter_out$rdesc

              ## sample filtering
              sample_filter_out <- apply_sample_filter(
                data = data,
                cdesc = cdesc,
                params = params,
                ome = ome
              )
              data <- sample_filter_out$data
              cdesc <- sample_filter_out$cdesc
              
              ## handle gene symbol column selection
              gene_symbol_col <- params$gene_symbol_column
              
              # If geneSymbol already exists in input, preserve it unless user explicitly selects a different column
              if ("geneSymbol" %in% names(rdesc)) {
                # User selected a different column - preserve original as geneSymbol_original
                if (!is.null(gene_symbol_col) && gene_symbol_col != "None" && 
                    gene_symbol_col != "geneSymbol" && gene_symbol_col %in% names(rdesc)) {
                  warning("Gene symbol column already exists. Original geneSymbol column will be preserved as 'geneSymbol_original'. The selected column will also be preserved in the dataset.")
                  rdesc$geneSymbol_original <- rdesc$geneSymbol
                  rdesc$geneSymbol <- rdesc[[gene_symbol_col]]
                  # Preserve the selected column (don't remove it)
                }
                # If user selected "None" or geneSymbol itself, keep existing geneSymbol
                # (no action needed - geneSymbol already exists)
              } else if (!is.null(gene_symbol_col) && gene_symbol_col != "None" && gene_symbol_col %in% names(rdesc)) {
                # geneSymbol doesn't exist - create it from selected column
                # Preserve the original column (don't remove it)
                rdesc$geneSymbol <- rdesc[[gene_symbol_col]]
              }
              # If geneSymbol doesn't exist and user selected "None" or column doesn't exist, geneSymbol won't be created
              
              ## fix gene symbol formatting (replace semicolons with pipes, clean up)
              if ("geneSymbol" %in% names(rdesc)) {
                fix_result <- fix_gene_symbols(rdesc)
                rdesc <- fix_result$rdesc
                # No rows are removed - blank geneSymbol values are converted to NA
                # removed_rids is kept for backward compatibility but should be empty
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
              
              #update cdesc and rdesc if needed
              cdesc <- cdesc[rownames(cdesc)%in%colnames(data.filtered),,drop=F]
              rdesc <- rdesc[rownames(rdesc)%in%rownames(data.filtered),,drop=F]
              
              ## re-combine GCT and return
              processed_GCT <- GCT(cdesc = cdesc, 
                                   rdesc = rdesc,
                                   mat = data.filtered,
                                   cid=colnames(data.filtered),
                                   rid=rownames(data.filtered))
              
              return(list(processed_GCT = processed_GCT, params = params))
            }
          )
        }
      )
    })
  
  # have the whole output be NULL if there was an error
  if (any(sapply(processing_out, is.null))) return(NULL)
  
  # otherwise, continue
  # pull out the GCTs and updated parameters separately
  GCTs_processed <- sapply(processing_out, 
                           function(ome) ome$processed_GCT,
                           simplify = FALSE)
  parameters_updated <- sapply(processing_out,
                               function(ome) ome$params,
                               simplify = FALSE)
  
  # Convert numeric columns that are discrete to strings in all processed GCTs
  # This ensures discrete columns are treated as categorical, not continuous
  for (ome in names(GCTs_processed)) {
    for (col_name in names(GCTs_processed[[ome]]@cdesc)) {
      if (is.numeric(GCTs_processed[[ome]]@cdesc[[col_name]])) {
        if (is.discrete(GCTs_processed[[ome]]@cdesc[[col_name]], nfactor_cutoff = 20)) {
          GCTs_processed[[ome]]@cdesc[[col_name]] <- as.character(GCTs_processed[[ome]]@cdesc[[col_name]])
        }
      }
    }
  }
  
  GCTs_merged <- my_shinyalert_tryCatch(
    merge_processed_gcts(GCTs_processed, parameters_updated),
    text.warning = "<b>Warning in merging GCTs:</b>",
    show.warning = TRUE,
    append.warning = TRUE,
    text.error = "<b>Error in merging GCTs:</b>",
    show.error = TRUE,
    return.error = NULL,
    append.error = TRUE
  )
  
  # have the whole output be NULL if there was an error
  if (is.null(GCTs_merged)) return(NULL)
  
  # Convert numeric columns that are discrete to strings in merged GCT
  # Use cutoff 20 to match processGCTs logic
  for (col_name in names(GCTs_merged@cdesc)) {
    if (is.numeric(GCTs_merged@cdesc[[col_name]])) {
      if (is.discrete(GCTs_merged@cdesc[[col_name]], nfactor_cutoff = 20)) {
        GCTs_merged@cdesc[[col_name]] <- as.character(GCTs_merged@cdesc[[col_name]])
      }
    }
  }
  
  output <- list(
    GCTs = GCTs_processed,
    parameters = parameters_updated,
    GCTs_merged = GCTs_merged
  )
  
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
    
    # Disable two-component normalization for datasets with more than 20 samples (too slow)
    # This is a safety check in case the UI didn't prevent selection (e.g., from old parameters)
    if (method == "2-component" && ncol(data) > 20) {
      warning(
        paste0(
          "Two-component normalization is disabled for datasets with more than 20 samples ",
          "(current dataset has ", ncol(data), " samples) due to performance concerns. ",
          "No normalization will be applied."
        )
      )
      method <- "None"
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
  }
  
  return(list(data.norm = data.norm,
              updated_method = method))
}

# maximum missing value filter
perform_missing_filter <- function(data, max_missing) {
  missing_percent <- apply(data, 1, function(x) sum(is.na(x))/length(x))
  data <- data[missing_percent <= max_missing/100, ]
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
  
  # Check if cdesc is missing, empty, or only has "id" column - if so, create Sample.ID column
  # This handles GCTs that don't have proper cdesc metadata
  if (is.null(cdesc) || nrow(cdesc) == 0 || ncol(cdesc) == 0) {
    # Create new cdesc with Sample.ID column
    sample_ids <- colnames(mat)
    cdesc <- data.frame(
      Sample.ID = sample_ids,
      stringsAsFactors = FALSE
    )
    rownames(cdesc) <- sample_ids
  } else if (ncol(cdesc) == 1 && names(cdesc)[1] == "id") {
    # If cdesc only has exactly one column named "id", recreate with Sample.ID column
    sample_ids <- colnames(mat)
    cdesc <- data.frame(
      Sample.ID = sample_ids,
      stringsAsFactors = FALSE
    )
    rownames(cdesc) <- sample_ids
  } else if (!setequal(colnames(mat), rownames(cdesc))) {
    # cdesc has real metadata but rownames don't match - this is an error
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

# merge processed GCTs
merge_processed_gcts <- function(GCTs_processed, parameters_updated) {
  withProgress(message = "Merging GCTs", expr = {
    
    # add a protigy.ome column to each gct's rdesc using dataset labels from parameters
    GCTs_processed <- mapply(
      GCTs_processed, names(GCTs_processed), parameters_updated,
      SIMPLIFY = FALSE, USE.NAMES = TRUE, 
      FUN = function(gct, filename, params) {
        # Get the dataset label from parameters
        dataset_label <- params$dataset_label
        if (is.null(dataset_label)) {
          # Fallback to filename if no label is set
          dataset_label <- filename
        }
        
        # check if `protigy.ome` is a column in the current gct
        if ("protigy.ome" %in% names(gct@rdesc) & any(gct@rdesc$protigy.ome != dataset_label)) {
          warning("`protigy.ome` column already exists and will be overwritten in ", filename)
        }
        gct@rdesc$protigy.ome <- rep(dataset_label, dim(gct@rdesc)[1])
        return(gct)
      })
    
    incProgress()
    
    # merge GCTs first using cmapR::merge_gct
    GCTs_merged <- Reduce(
      function(gct1, gct2) {
        #before merging, need to make sure the rids are unique
        #first save the old IDs
        gct1@rdesc$old_id = gct1@rid
        gct2@rdesc$old_id = gct2@rid
        
        # Only apply prefix if not already prefixed (avoid duplication)
        # Check if the rid already starts with the ome name
        if (!any(startsWith(gct1@rid, paste0(gct1@rdesc$protigy.ome[1], "_")))) {
          rownames(gct1@mat) = rownames(gct1@rdesc) = gct1@rdesc$id = gct1@rid = paste(gct1@rdesc$protigy.ome,gct1@rid,sep="_")
        }
        if (!any(startsWith(gct2@rid, paste0(gct2@rdesc$protigy.ome[1], "_")))) {
          rownames(gct2@mat) = rownames(gct2@rdesc) = gct2@rdesc$id = gct2@rid = paste(gct2@rdesc$protigy.ome,gct2@rid,sep="_")
        }
        
        #now can merge and rids will always be unique
        merged <- cmapR::merge_gct(gct1, gct2, dim='row')
        incProgress()
        return(merged)
      },
      GCTs_processed)
    rownames(GCTs_merged@cdesc) <- GCTs_merged@cid
    # Keep the merged feature IDs as rdesc rownames (these are the actual feature IDs)
    rownames(GCTs_merged@rdesc) <- GCTs_merged@rid
    
    
    ## Now deal with the cdesc
    # cmapR::merge_gct will override any conflicting annotation columns in cdesc
    # with whatever is in the first GCT. Instead, we want to duplicate conflict
    # columns so no data is lost.
    
    # figure out which columns conflict with other omes
    conflict_columns <- c()
    for (i in seq_along(GCTs_processed)) {
      ome <- names(GCTs_processed)[i]
      gct <- GCTs_processed[[i]]
      
      # subset to only samples in ome
      samples_in_ome <- gct@cid
      merged_cdesc_subset <- GCTs_merged@cdesc[samples_in_ome, , drop = FALSE]
      
      # if there's a column with all NA, replace with values in this ome
      replace_NA_col <- intersect(
        names(which(sapply(merged_cdesc_subset, function(col) all(is.na(col))))),
        names(gct@cdesc)
      )
      if (length(replace_NA_col) > 0) {
        GCTs_merged@cdesc[samples_in_ome, replace_NA_col] <- gct@cdesc[samples_in_ome, replace_NA_col]
        merged_cdesc_subset <- GCTs_merged@cdesc[samples_in_ome, , drop = FALSE]
      }
      
      
      # find columns that have a conflict
      conflict_columns_ome <- names(which(
        sapply(names(gct@cdesc), function(col) {
          TRUE %in% c(
            any(gct@cdesc[[col]] != merged_cdesc_subset[[col]]), # any values are not the same
            any(is.na(gct@cdesc[[col]]) != is.na(merged_cdesc_subset[[col]])) # any NA's are not in the same place
          )
        })
      ))
      
      conflict_columns <- unique(c(conflict_columns, conflict_columns_ome))
    }
    
    incProgress()
    
    # remove conflicting columns and re-name by ome
    for (col in conflict_columns) {
      
      # get the omes that contain this conflict column
      omes_with_col <- names(which(
        sapply(GCTs_processed, function(gct) col %in% names(gct@cdesc))
      ))
      
      # get the new column names, make sure they're unique
      new_col_names <- utils::tail(
        n = length(omes_with_col),
        make.names(c(names(GCTs_merged@cdesc), paste0(col, '.', omes_with_col)),
                   unique = TRUE)
      )
      
      # get the new columns from each ome's GCT's cdesc
      # make sure samples are in the same order as they are in GCTs_merged
      all_samples <- rownames(GCTs_merged@cdesc)
      new_columns <- as.data.frame(sapply(omes_with_col, 
                                          function(ome) GCTs_processed[[ome]]@cdesc[all_samples, col],
                                          simplify = FALSE))
      names(new_columns) <- new_col_names
      
      
      GCTs_merged@cdesc <- GCTs_merged@cdesc %>%
        dplyr::mutate(new_columns, .after = .data[[col]]) %>% 
        dplyr::select(-.data[[col]])
    }
    
    # Add missing columns logic
    # Find columns that exist in some datasets but not in the merged cdesc
    all_unique_columns <- unique(unlist(lapply(GCTs_processed, function(gct) names(gct@cdesc))))
    missing_columns <- setdiff(all_unique_columns, names(GCTs_merged@cdesc))
    
    if (length(missing_columns) > 0) {
      message("Adding missing columns to merged GCT: ", paste(missing_columns, collapse = ", "))
      
      # Add missing columns to the merged cdesc
      for (col in missing_columns) {
        # Find which datasets have this column
        omes_with_col <- names(which(
          sapply(GCTs_processed, function(gct) col %in% names(gct@cdesc))
        ))
        
        # For samples that don't have this column, fill with NA
        # For samples that do have this column, use their values
        all_samples <- rownames(GCTs_merged@cdesc)
        new_column <- rep(NA, length(all_samples))
        names(new_column) <- all_samples
        
        # Fill in values from datasets that have this column
        for (ome in omes_with_col) {
          samples_in_ome <- GCTs_processed[[ome]]@cid
          new_column[samples_in_ome] <- GCTs_processed[[ome]]@cdesc[samples_in_ome, col]
        }
        
        # Add the column to merged cdesc
        GCTs_merged@cdesc[[col]] <- new_column
      }
    }
    
    setProgress(1)
    
  })
  
  return(GCTs_merged)
}
