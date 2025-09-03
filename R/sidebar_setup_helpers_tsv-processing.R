################################################################################
# Module: SETUP SIDEBAR
# Functions for processing TSV files, converting them to CSV format, and generating experimental design for user input 
################################################################################

# Process TSV files with condition setup and experimental design to create GCT objects ####
# INPUT: raw TSV data and corresponding condition setup file from Spectronaut
# OUTPUT: list of GCT objects (same format as existing GCT workflow)

processTSVFiles <- function(dataFiles, conditionSetup, identifierColumn = NULL) {
  # Validate inputs
  if (is.null(dataFiles) || nrow(dataFiles) == 0) {
    stop("No TSV data files provided")
  }
  
  if (is.null(conditionSetup)) {
    stop("Condition setup file is required for TSV processing")
  }
  
  # Validate condition setup file
  # Check for necessary columns
  # Check for unique values 
  validateConditionSetup(conditionSetup)
  
  # Process each TSV file
  processed_data_list <- list()
  mapping_tables <- list()
  identifier_info <- list()
  
  for (i in seq_len(nrow(dataFiles))) {
    file_path <- dataFiles$datapath[i]
    file_name <- dataFiles$name[i]
    
    tryCatch({
      # Read TSV file
      data <- readr::read_delim(file_path)
      
      # Process the TSV data
      result <- processSingleTSVFile(data, conditionSetup, file_name, identifierColumn)
      
      # Store results
      label <- tools::file_path_sans_ext(file_name)
      processed_data_list[[label]] <- result$processed_data
      # mapping_tables[[label]] <- result$mapping_table
      identifier_info[[label]] <- list(
        identifier_column = result$identifier_column,
        identifier_method = result$identifier_method
      )
      
    }, error = function(e) {
      stop("Failed to process TSV file ", file_name, ": ", e$message)
    })
  }
  
  return(list(
    processed_data = processed_data_list,
    # mapping_tables = mapping_tables,
    identifier_info = identifier_info
  ))
}

# Validate condition setup file structure and content ####
validateConditionSetup <- function(conditionSetup) {
  # Check required columns
  run_label_col <- NULL
  available_columns <- colnames(conditionSetup)
  
  # Run Label column for file names (handle both "Run Label" and "Run.Label" formats)
  if ("Run Label" %in% available_columns) {
    run_label_col <- "Run Label"
  } else if ("Run.Label" %in% available_columns) {
    # Fallback to check if R renamed whitespaces to dots
    run_label_col <- "Run.Label"
  } else {
    stop(
      "Condition setup file must have the required `Run Label` column.", 
      "Available columns: ", paste(available_columns, collapse = ", ")
    )
  }
  
  # Condition/Label column for sample condition
  if ("Condition" %in% available_columns) {
    condition_col <- "Condition"
  } else if ("Label" %in% available_columns) {
    condition_col <- "Label"
  } else {
    stop(
      "Condition setup file must have the required `Condition` or `Label` column.", 
      "Available columns: ", paste(available_columns, collapse = ", ")
    )
  }
  
  # Check for unique values
  if (any(duplicated(conditionSetup[[run_label_col]]))) {
    stop("Duplicate values found in '", run_label_col, "' column of condition setup file")
  }

  if (any(duplicated(conditionSetup[[condition_col]]))) {
    stop("Duplicate values found in '", condition_col, "' column of condition setup file")
  }
  
  # Check for empty values
  if (any(is.na(conditionSetup[[run_label_col]]) | conditionSetup[[run_label_col]] == "")) {
    stop("Empty or NA values found in '", run_label_col, "' column")
  }
  
  if (any(is.na(conditionSetup[[condition_col]]) | conditionSetup[[condition_col]] == "")) {
    stop("Empty or NA values found in '", condition_col, "' column")
  }
  
  return(TRUE)
}

# Process a single TSV file with condition setup ####
processSingleTSVFile <- function(data, conditionSetup, file_name, identifierColumn = NULL) {
  
  # Determine column names (handle both space and dot versions)
  run_label_col <- if ("Run Label" %in% colnames(conditionSetup)) "Run Label" else "Run.Label"
  condition_col <- if ("Condition" %in% colnames(conditionSetup)) "Condition" else "Label" # fallback if Condition column is not availble in rare cases
  
  # Detect quantity columns
  quantity_columns <- detectQuantityColumns(data)
  
  # Filter for only non-experimental and quantity columns 
  filtered_data <- filterTsvData(data, quantity_columns)

  # Rename quantity columns based on condition setup
  result <- renameQuantityColumns(filtered_data, conditionSetup, quantity_columns, condition_col, run_label_col)
  
  # Determine identifier column and process protein groups if available
  identifier_result <- determineIdentifierColumns(result$processed_data, identifierColumn)
  final_processed_data <- identifier_result$data
  
  # # Generate mapping table for display
  # mapping_table <- generateMappingTable(data, final_processed_data, file_name)
  
  return(list(
    processed_data = final_processed_data,
    # mapping_table = mapping_table,
    processing_summary = result$summary,
    identifier_column = identifier_result$identifier_column,
    identifier_method = identifier_result$method
  ))
}

# Detect PG.Quantity columns ####
detectQuantityColumns <- function(data) {
  all_columns <- colnames(data)
  
  # Debug: print all columns to help diagnose the issue
  message("\nAll columns in data: ", paste(all_columns, collapse = ", "))
  
  # Find columns ending with .PG.Quantity
  quantity_pattern <- "\\.PG\\.Quantity$"
  quantity_columns <- grep(quantity_pattern, all_columns, value = TRUE)
  
  if (length(quantity_columns) == 0) {
    stop(
      "\nNo quantity columns found. Expected columns containing 'PG.Quantity'. Available columns: ", 
      paste(all_columns, collapse = ", ")
    )
  }
  
  message("\nDetected quantity columns: ", paste(quantity_columns, collapse = ", "))
  return(quantity_columns)
}

# Filter non-experimental and PG.Quantity columns ####
filterTsvData <- function(data, quantity_columns) {
  nonExperimental_columns <- identifyMetadataColumns(data)

  # Combine non-experimental and quantity columns 
  columns_to_keep <- c(nonExperimental_columns, quantity_columns)

  # Debug: show what columns are being kept
  message("\nColumns to keep: ", paste(columns_to_keep, collapse = ", "))

  # Filter the dataframe to keep only these columns 
  filtered_data <- data[, columns_to_keep, drop = FALSE]

  # Debug: check dimensions for the filtered dataframe
  message("\nFiltered data dimensions: ", nrow(filtered_data), " rows x ", ncol(filtered_data), " columns")

  return(filtered_data)
}

# Rename quantity columns using condition setup mapping ####
renameQuantityColumns <- function(data, conditionSetup, quantity_columns, condition_col, run_label_col) {
  
  processed_data <- data
  
  # Step 1: Create mapping from Run Label to Condition
  run_label_map <- setNames(conditionSetup[[condition_col]], conditionSetup[[run_label_col]])
  message("Created Run Label to Condition mapping with ", length(run_label_map), " entries")
  message("Sample mappings: ", paste(names(run_label_map)[1:min(3, length(run_label_map))], 
                                    "->", run_label_map[1:min(3, length(run_label_map))], 
                                    collapse = "; "))
  
  # # Step 2: Identify quantity columns (already done, but confirm)
  # message("Step 2: Found ", length(quantity_columns), " quantity columns ending with '.PG.Quantity'")
  # message("Quantity columns: ", paste(quantity_columns[1:min(3, length(quantity_columns))], collapse = ", "), 
  #         if(length(quantity_columns) > 3) "..." else "")
  
  # Keep track of changes for the summary
  successful_renames <- list() # stores old_name -> new_name
  failed_renames <- c()
  
  # Step 3: Find corresponding Run Label and rename to Condition value
  message("Starting column renaming process...")
  
  for (col_name in quantity_columns) {
    match_found <- FALSE
    message("  Processing quantity column: ", col_name)
    
    # Iterate through the run labels to find a match 
    for (run_label in names(run_label_map)) {
      # Sanitize the run_label from the mapping file in the same way R would
      # sanitized_run_label <- make.names(run_label)
      sanitized_run_label <- run_label
      
      # Check if the sanitized run label is a substring of the column name
      if (grepl(sanitized_run_label, col_name, fixed = TRUE)) {
        new_name <- run_label_map[[run_label]]
        message("    ✓ Match found: '", run_label, "' (sanitized: '", sanitized_run_label, 
                "') -> renaming to '", new_name, "'")
        
        # Check for potential duplicate column names before renaming 
        if (new_name %in% unlist(successful_renames)) {
          warning("Duplicate condition '", new_name, "' detected. This will create duplicate column names.")
        }
        
        # Rename the column
        col_index <- which(colnames(processed_data) == col_name)
        if (length(col_index) > 0) {
          colnames(processed_data)[col_index] <- new_name
          successful_renames[[col_name]] <- new_name
          match_found <- TRUE
          break # Exit inner loop once a match is found
        }
      }
    }
    
    if (!match_found) {
      failed_renames <- c(failed_renames, col_name)
      message("    ✗ No match found for: ", col_name)
    }
  }
  
  # Step 4: Keep metadata columns and renamed quantity columns
  metadata_columns <- identifyMetadataColumns(processed_data)
  message("Identified ", length(metadata_columns), " metadata columns")
  message("Metadata columns: ", paste(metadata_columns[1:min(5, length(metadata_columns))], collapse = ", "),
          if(length(metadata_columns) > 5) "..." else "")
  
  # The columns to keep are the metadata columns & the renamed quantity columns
  renamed_quantity_columns <- unlist(successful_renames)
  final_columns <- c(metadata_columns, renamed_quantity_columns)
  
  # Ensure all columns to keep actually exist in the processed data
  existing_columns <- final_columns[final_columns %in% colnames(processed_data)]
  missing_columns <- final_columns[!final_columns %in% colnames(processed_data)]
  
  if (length(missing_columns) > 0) {
    message("Warning: Some expected columns are missing: ", paste(missing_columns, collapse = ", "))
  }
  
  processed_data <- processed_data[, existing_columns, drop = FALSE]
  message("Final dataset has ", ncol(processed_data), " columns (", 
          length(metadata_columns), " metadata + ", length(renamed_quantity_columns), " renamed quantity)")
  
  # Compile a column renaming summary
  summary <- list(
    total_quantity_columns = length(quantity_columns),
    renamed_columns = length(successful_renames),
    failed_renames = length(failed_renames),
    failed_rename_names = failed_renames,
    renamed_map = successful_renames,
    metadata_columns = metadata_columns
  )
  
  message("Renaming summary: ", length(successful_renames), " successful, ", 
          length(failed_renames), " failed out of ", length(quantity_columns), " total")
  
  return(list(
    processed_data = processed_data,
    summary = summary
  ))
}

# Identify metadata columns (non-quantity columns without Run Label patterns) ####
identifyMetadataColumns <- function(data) {
  all_columns <- colnames(data)

  # Define pattern for non-experimental columns 
  nonExperimental_patterns <- c(
    "PG\\.ProteinGroups$", "PG\\.ProteinAccessions$", "PG\\.Genes$", "PG\\.Organisms$", "PG\\.ProteinDescriptions$", "PG\\.UniProtIds$", "PG\\.ProteinNames$", "PG\\.NrOfStrippedSequencesIdentified", "PG\\.Meta$", "PG\\.BiologicalProcess$", "PG\\.MolecularFunction$"
  )

  # Find non-experimental columns 
  nonExperimental_columns <- c()
  for (pattern in nonExperimental_patterns) {
    matched_cols <- grep(pattern, all_columns, value = TRUE)
    nonExperimental_columns <- c(nonExperimental_columns, matched_cols)
  }

  return(nonExperimental_columns)
}

# Generate mapping table showing before/after column transformations ####
generateMappingTable <- function(original_data, processed_data, file_name) {
  
  original_cols <- colnames(original_data)
  processed_cols <- colnames(processed_data)
  
  # Create mapping table
  mapping_df <- data.frame(
    original_column_name = original_cols,
    renamed_column_name = original_cols,
    stringsAsFactors = FALSE
  )
  
  return(mapping_df)
}

## Determine the best identifier column for the data ####
determineIdentifierColumns <- function(data, identifierColumn = NULL) {
  # If user specified an identifier column, validate and use it
  if (!is.null(identifierColumn) && identifierColumn %in% colnames(data)) {
    return(list(
      identifier_column = identifierColumn, 
      data = data, 
      method = "user_specific"
    ))
  }
  
  # Check for PG.ProteinGroups column and process it by extracting the first protein group before the first semicolon
  if ("PG.ProteinGroups" %in% colnames(data)) {
    processed_data <- extractFirstProteinGroup(data)
    return(list(
      identifier_column = "id", 
      data = processed_data, 
      method = "first_protein_group"
    ))
  }
  
  # If PG.ProteinGroups column does not exist, use the first column as fallback and ask for user input 
  return(list(
    identifier_column = colnames(data)[1], 
    data = data, 
    method = "first_column_fallback"
  ))
}

### Extract first protein group from the identifier column ####
extractFirstProteinGroup <- function(data) {
  if (!"PG.ProteinGroups" %in% colnames(data)) {
    stop("PG.ProteinGroups column not found in data")
  }
  
  # Extract first protein group (everything before first semicolon)
  protein_groups <- data$PG.ProteinGroups
  first_groups <- sapply(protein_groups, function(x) {
    if (is.na(x) || x == "") {
      return(x)
    }
    # Split on semicolon and take first element, trim whitespace
    first_group <- trimws(strsplit(as.character(x), ";")[[1]][1])
    return(first_group)
  }, USE.NAMES = FALSE)
  
  # Add new column to data
  data$id <- first_groups
  return(data)
}

### Validate that identifier column have unique values ####
validateUniqueIdentifiers <- function(data, identifier_column) {
  if (!identifier_column %in% colnames(data)) {
    stop("Identifier column '", identifier_column, "' not found in data")
  }
  
  identifier_values <- data[[identifier_column]]
  
  # Remove NA values for duplicate checking
  non_na_values <- identifier_values[!is.na(identifier_values)]
  
  # Check for duplicates
  if (any(duplicated(non_na_values))) {
    duplicate_values <- non_na_values[duplicated(non_na_values)]
    stop("Duplicate values found in identifier column '", identifier_column, "': ", 
         paste(unique(duplicate_values), collapse = ", "))
  }
  
  # Check for empty strings
  empty_values <- sum(non_na_values == "", na.rm = TRUE)
  if (empty_values > 0) {
    stop("Empty values found in identifier column '", identifier_column, "' (", empty_values, " rows)")
  }
  
  return(TRUE)
}

# Read condition setup file ####
readConditionSetup <- function(file_path) {
  if (is.null(file_path)) {
    stop("Condition setup file path is required")
  }
  
  # Read TSV file normally - the # is actually a column name, not a comment character
  condition_setup <- readr::read_delim(file_path)
  
  # # Trim whitespace from column names to handle any formatting issues
  # colnames(condition_setup) <- trimws(colnames(condition_setup))
  
  # Validate the file structure
  validateConditionSetup(condition_setup)
  
  return(condition_setup)
}

# Convert processed TSV data to format compatible with CSV/Excel workflow ####
convertTSVToCSVFormat <- function(processed_data, identifierColumn = NULL) {
  
  # The processed data is already in the right format for CSV processing
  # Just need to ensure identifier column is properly handled
  
  if (!is.null(identifierColumn)) {
    # Determine identifier information
    identifier_result <- determineIdentifierColumns(processed_data, identifierColumn)
    processed_data <- identifier_result$data
  }
  
  return(processed_data)
}