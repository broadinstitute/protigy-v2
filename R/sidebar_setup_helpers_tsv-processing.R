################################################################################
# Module: SETUP SIDEBAR
# Functions for processing TSV files, converting them to CSV format, and generating experimental design for user input 
################################################################################

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
      data <- readr::read_delim(file_path) |> 
        clean_names()

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
  if ("run_label" %in% available_columns) {
    run_label_col <- "run_label"
  } else {
    stop(
      "Condition setup file must have the required `run_label` column.", 
      "Available columns: ", paste(available_columns, collapse = ", ")
    )
  }
  
  # Condition/Label column for sample condition
  if ("condition" %in% available_columns) {
    condition_col <- "condition"
  } else if ("label" %in% available_columns) {
    condition_col <- "label"
  } else {
    stop(
      "Condition setup file must have the required `condition` or `label` column.", 
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
  # Make sure column names of data and condition setup are cleaned up
  data <- janitor::clean_names(data)
  conditionSetup <- janitor::clean_names(conditionSetup)
  message(colnames(conditionSetup))
  
  # Determine conditions based on condition setup
  run_label_col <- if ("run_label" %in% colnames(conditionSetup)) "run_label" else stop("No Run Label column found in Condition Setup.")
  condition_col <- if ("condition" %in% colnames(conditionSetup)) "condition" else stop("No Condition column found in Condition Setup.")
  
  # Detect metadata (non-experimental data) and quantity columns 
  # Once columns of interest are determined, filter dataframe to include only metadata and quantity columns
  # Rename quantity columns by the run label-condition mapping correspondance 
  # Output vars: metadata_cols, quantity_cols, filtered_df 
  quantity_columns <- filterMetadataQuantityCols(data, conditionSetup, condition_col, run_label_col)
  
  # Determine identifier column and process protein groups if available
  identifier_result <- determineIdentifierColumns(quantity_columns$filtered_df, identifierColumn)
  final_processed_data <- identifier_result$data
  
  # # Generate mapping table for display
  # mapping_table <- generateMappingTable(data, final_processed_data, file_name)
  
  return(list(
    processed_data = final_processed_data,
    # mapping_table = mapping_table,
    # processing_summary = result$summary,
    identifier_column = identifier_result$identifier_column,
    identifier_method = identifier_result$method
  ))
}

# Detect PG.Quantity columns ####
filterMetadataQuantityCols <- function(data, conditionSetup, condition_col, run_label_col) {
  # data <- janitor::clean_names(data) # Sanity check
  all_columns <- colnames(data)
  
  # # Debug: print all columns to help diagnose the issue
  # message("\nAll columns in data: ", paste(all_columns, collapse = ", "))
  
  # Extract conditions and run labels from the specified column in conditionSetup
  run_labels <- clean_like_janitor(conditionSetup[[run_label_col]])
  message("\nAll run labels: ", paste(run_labels, collapse = ", "))
  conditions <- clean_like_janitor(conditionSetup[[condition_col]])
  message("\nAll condition: ", paste(conditions, collapse = ", "))

  # Identify columns whose names contain any run label as a substring
  experimental_cols <- all_columns[
    sapply(all_columns, function(col)
      any(sapply(run_labels, function(lbl) grepl(lbl, col, fixed = TRUE)))
    )
  ]

  # Non-experimental (metadata) columns are all columns that are not included in the experimental column list 
  metadata_cols <- setdiff(all_columns, experimental_cols)

  # From experimental columns, keep only those ending with "PG.Quantity"
  quantity_cols <- experimental_cols[grepl("pg_quantity$", experimental_cols, ignore.case = TRUE)]
  
  if (length(quantity_cols) == 0) {
    stop(
      "\nNo quantity columns found. Available columns: ", 
      paste(experimental_cols, collapse = ", ")
    )
  }
  message("\nDetected quantity columns: ", paste(quantity_cols, collapse = ", "))

  # Map quantity column to its condition
  # Determine the mapping relationship by identifying run label as substring of the column name
  # Replace the entire column name by the corresponding condition 
  renamed_quantity_cols <- vapply(quantity_cols, function(col) {
    match_idx <- sapply(run_labels, function(lbl) grepl(lbl, col))
    if (any(match_idx)) {
      # If multiple matches, first TRUE is taken 
      conditions[which(match_idx)[1]]
    } else {
      col # fallback
    }
  }, character(1))

  # Build filtered and renamed dataframe
  filtered_df <- data[, c(metadata_cols, quantity_cols), drop = FALSE]
  colnames(filtered_df)[match(quantity_cols, colnames(filtered_df))] <- renamed_quantity_cols

  return(list(
    filtered_df = filtered_df, 
    metadata_cols = metadata_cols, 
    quantity_cols = quantity_cols
  ))
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
  condition_setup <- readr::read_delim(file_path) |> 
    clean_names()
  
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

# Clean list values the same way janitor::clean_names() cleans up column names to keep data-condition setup mapping consistent 
# Done by generating a fake dataframe from the vector 
clean_like_janitor <- function(x) {
  vapply(x, janitor::make_clean_names, character(1))
}