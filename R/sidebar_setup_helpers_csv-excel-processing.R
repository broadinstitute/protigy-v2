################################################################################
# Module: SETUP SIDEBAR
# Functions for processing CSV/Excel files and converting them to GCT format
################################################################################

# Process CSV/Excel files with experimental design to create GCT objects
# INPUT: list of data files, experimental design data.frame, identifierColumn (optional), gene mapping parameters (optional)
# OUTPUT: list of GCT objects (same format as existing GCT workflow)
processCSVExcelFiles <- function(dataFiles, experimentalDesign, identifierColumn = NULL, 
                                mappingFilePath = NULL, mappingProteinCol = NULL, mappingGeneCol = NULL) {
  GCTs <- list()
  
  # Process each file
  for (i in seq_len(nrow(dataFiles))) {
    file_path <- dataFiles$datapath[i]
    file_name <- dataFiles$name[i]
    file_ext <- tools::file_ext(tolower(file_name))
    
    tryCatch({
      # Read the data file
      if (file_ext == "csv") {
        data <- readr::read_csv(file_path, stringsAsFactors = FALSE)
      } else if (file_ext %in% c("xlsx", "xls")) {
        data <- readxl::read_excel(file_path)
      } else {
        stop("Unsupported file format: ", file_ext)
      }
      
      # Convert to GCT object
      gct_obj <- convertToGCT(data, experimentalDesign, file_name, identifierColumn, 
                             mappingFilePath, mappingProteinCol, mappingGeneCol)
      
      # Create a simple label from filename (remove extension)
      label <- tools::file_path_sans_ext(file_name)
      GCTs[[label]] <- gct_obj
      
    }, error = function(e) {
      stop("Failed to process file ", file_name, ": ", e$message)
    })
  }
  
  return(GCTs)
}

# Determine the best identifier column for the data
# INPUT: data.frame, optional user-specified identifier column
# OUTPUT: list with identifier_column name and processed data
determineIdentifierColumn <- function(data, identifierColumn = NULL) {
  # If user specified an identifier column, validate and use it
  if (!is.null(identifierColumn) && identifierColumn %in% colnames(data)) {
    return(list(
      identifier_column = identifierColumn,
      data = data,
      method = "user_specified"
    ))
  }
  
  # Check for PG.ProteinGroups column and process it
  if ("PG.ProteinGroups" %in% colnames(data)) {
    processed_data <- extractFirstProteinGroup(data)
    return(list(
      identifier_column = "firstProteinGroup",
      data = processed_data,
      method = "first_protein_group"
    ))
  }
  
  # If PG.ProteinGroups doesn't exist, use first column as fallback
  # This will trigger user selection in the UI
  return(list(
    identifier_column = colnames(data)[1],
    data = data,
    method = "first_column_fallback"
  ))
}

# Extract first protein group from PG.ProteinGroups column
# INPUT: data.frame with PG.ProteinGroups column
# OUTPUT: data.frame with new firstProteinGroup column
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
  data$firstProteinGroup <- first_groups
  return(data)
}

# Validate that identifier column has unique values
# INPUT: data.frame, identifier column name
# OUTPUT: TRUE if valid, throws error if duplicates found
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

# Convert CSV/Excel data to GCT format
convertToGCT <- function(data, experimentalDesign, file_name, identifierColumn = NULL,
                        mappingFilePath = NULL, mappingProteinCol = NULL, mappingGeneCol = NULL) {
  
  # Determine the best identifier column using robust logic
  identifier_result <- determineIdentifierColumn(data, identifierColumn)
  processed_data <- identifier_result$data
  final_identifier_column <- identifier_result$identifier_column
  
  # Validate that the identifier column has unique values
  validateUniqueIdentifiers(processed_data, final_identifier_column)
  
  # Get feature IDs from the determined identifier column
  feature_id_col <- which(colnames(processed_data) == final_identifier_column)
  feature_ids <- processed_data[[feature_id_col]]
  
  # Get all sample IDs from data (all columns except identifier column)
  all_sample_ids <- colnames(processed_data[, -feature_id_col, drop = FALSE])
  
  # Filter to only experimental columns (those with valid metadata)
  experimental_sample_ids <- filterExperimentalColumns(all_sample_ids, experimentalDesign)
  
  # Check if we have any experimental columns
  if (length(experimental_sample_ids) == 0) {
    stop("No experimental columns found with valid metadata for file: ", file_name)
  }
  
  # Extract data matrix using only experimental columns
  experimental_columns <- c(feature_id_col, which(colnames(processed_data) %in% experimental_sample_ids))
  filtered_data <- processed_data[, experimental_columns, drop = FALSE]
  data_matrix <- as.matrix(filtered_data[, -1, drop = FALSE]) # Remove identifier column from matrix
  rownames(data_matrix) <- feature_ids
  
  # Get final sample IDs (should be the experimental ones)
  sample_ids <- colnames(data_matrix)
  
  # Create rdesc (row descriptor) - minimal version
  rdesc <- data.frame(
    id = feature_ids,
    id.description = feature_ids,
    stringsAsFactors = FALSE
  )
  rownames(rdesc) <- feature_ids
  
  # Add gene mapping if mapping file is provided
  if (!is.null(mappingFilePath) && !is.null(mappingProteinCol) && !is.null(mappingGeneCol)) {
    tryCatch({
      # Source the gene mapping functions if not already loaded
      if (!exists("performGeneMapping")) {
        source("R/sidebar_setup_helpers_gene-mapping.R")
      }
      
      # Determine which column contains protein groups for mapping
      working_protein_col <- final_identifier_column
      if (identifier_result$method == "first_protein_group") {
        # If we extracted first protein group, use original PG.ProteinGroups column
        working_protein_col <- "PG.ProteinGroups"
      }
      
      # Read mapping file
      mapping_data <- readMappingFile(mappingFilePath)
      
      # Create temporary data frame with protein groups for mapping
      temp_working_data <- data.frame(
        protein_group = processed_data[[working_protein_col]],
        stringsAsFactors = FALSE
      )
      
      # Perform gene mapping
      mapping_result <- performGeneMapping(
        working_data = temp_working_data,
        mapping_data = mapping_data,
        working_protein_col = "protein_group",
        mapping_protein_col = mappingProteinCol,
        mapping_gene_col = mappingGeneCol
      )
      
      # Add gene symbols to rdesc
      rdesc$gene_symbol <- mapping_result$mapped_data$gene_symbol
      
      # Store mapping statistics for later use
      attr(rdesc, "mapping_stats") <- mapping_result$mapping_stats
      
    }, error = function(e) {
      warning("Gene mapping failed: ", e$message, ". Continuing without gene mapping.")
    })
  }
  
  # Create cdesc (column descriptor) from experimental design
  cdesc <- createCdesc(sample_ids, experimentalDesign, file_name)
  
  # Create GCT object using cmapR
  gct_obj <- cmapR::GCT(
    mat = data_matrix,
    rdesc = rdesc,
    cdesc = cdesc
  )
  
  return(gct_obj)
}

# Filter experimental columns based on experimental design metadata
# INPUT: sample IDs from data file, experimental design
# OUTPUT: vector of sample IDs that have valid experimental metadata (non-NA)
filterExperimentalColumns <- function(sample_ids, experimentalDesign) {
  # Match sample IDs with experimental design
  exp_design_matched <- experimentalDesign[match(sample_ids, experimentalDesign$columnName), ]
  
  # Get metadata columns (all columns except columnName)
  metadata_columns <- setdiff(names(experimentalDesign), "columnName")
  
  if (length(metadata_columns) == 0) {
    # No metadata columns - treat all as non-experimental
    warning("No metadata columns found in experimental design. No experimental columns will be included.")
    return(character(0))
  }
  
  # Find samples that have valid experimental metadata 
  # (at least one metadata column with non-NA, non-empty value)
  valid_samples <- rep(FALSE, length(sample_ids))
  
  for (col in metadata_columns) {
    # Handle cases where exp_design_matched has NA rows (samples not in experimental design)
    if (col %in% colnames(exp_design_matched)) {
      col_values <- exp_design_matched[[col]]
      # Check for valid values: not NA, not empty string, not just whitespace
      col_valid <- !is.na(col_values) & 
                  as.character(col_values) != "" & 
                  trimws(as.character(col_values)) != ""
      
      # Handle NA rows in exp_design_matched (columns not found in experimental design)
      col_valid[is.na(exp_design_matched$columnName)] <- FALSE
      
      valid_samples <- valid_samples | col_valid
    }
  }
  
  # Filter out samples that weren't found in experimental design
  samples_found_in_design <- !is.na(exp_design_matched$columnName)
  valid_samples <- valid_samples & samples_found_in_design
  
  valid_sample_ids <- sample_ids[valid_samples]
  
  # Provide informative feedback
  if (length(valid_sample_ids) == 0) {
    not_found <- sum(!samples_found_in_design)
    no_metadata <- sum(samples_found_in_design & !valid_samples)
    
    error_msg <- "No valid experimental columns found."
    if (not_found > 0) {
      error_msg <- paste(error_msg, paste(not_found, "column(s) not found in experimental design."))
    }
    if (no_metadata > 0) {
      error_msg <- paste(error_msg, paste(no_metadata, "column(s) have no valid metadata (all NA or empty)."))
    }
    
    stop(error_msg, " Please check your experimental design file.")
  }
  
  # Return only the sample IDs that have valid metadata
  return(valid_sample_ids)
}

# Create column descriptor (cdesc) from experimental design
# INPUT: sample IDs, experimental design, file name
# OUTPUT: data.frame for cdesc
createCdesc <- function(sample_ids, experimentalDesign, file_name) {
  # Match sample IDs with experimental design
  exp_design_matched <- experimentalDesign[match(sample_ids, experimentalDesign$columnName), ]
  
  # Check for missing matches (this should not happen with filtered sample_ids)
  if (any(is.na(exp_design_matched$columnName))) {
    missing_samples <- sample_ids[is.na(exp_design_matched$columnName)]
    stop("Sample IDs not found in experimental design: ", paste(missing_samples, collapse = ", "), 
         "\nThis should not happen if filterExperimentalColumns() was used correctly.")
  }
  
  # Get metadata columns (all columns except columnName)
  metadata_columns <- setdiff(names(experimentalDesign), "columnName")
  
  # Create cdesc data.frame starting with Sample.ID
  cdesc <- data.frame(
    Sample.ID = sample_ids,
    stringsAsFactors = FALSE
  )
  
  # Add all metadata columns from experimental design
  for (col in metadata_columns) {
    if (col %in% colnames(exp_design_matched)) {
      # Clean up the metadata values
      col_values <- exp_design_matched[[col]]
      # Convert empty strings and whitespace-only strings to NA for consistency
      col_values[trimws(as.character(col_values)) == ""] <- NA
      cdesc[[col]] <- col_values
    } else {
      # Add column with NA values if not found (shouldn't happen normally)
      cdesc[[col]] <- rep(NA, length(sample_ids))
    }
  }
  
  rownames(cdesc) <- sample_ids
  
  return(cdesc)
}

# Create parameters list for CSV/Excel data (similar to GCT parameters)
# INPUT: file information
# OUTPUT: parameters list compatible with existing workflow
createCSVExcelParameters <- function(dataFiles) {
  # Read default parameters
  default_parameters <- yaml::read_yaml(
    system.file('setup_parameters/setupDefaults.yaml', package = 'Protigy')
  )
  
  parameters <- list()
  
  for (i in seq_len(nrow(dataFiles))) {
    file_name <- dataFiles$name[i]
    file_path <- dataFiles$datapath[i]
    label <- tools::file_path_sans_ext(file_name)
    
    # Create parameters for this file
    parameters[[label]] <- c(
      gct_file_path = file_path,
      gct_file_name = file_name,
      default_parameters
    )
  }
  
  return(parameters)
}

# Process CSV/Excel workflow to create objects compatible with existing GCT workflow
# INPUT: data files, experimental design, identifierColumn (optional), gene mapping parameters (optional)
# OUTPUT: list with GCTs and parameters (same format as GCT workflow)
processCSVExcelWorkflow <- function(dataFiles, experimentalDesign, identifierColumn = NULL,
                                   mappingFilePath = NULL, mappingProteinCol = NULL, mappingGeneCol = NULL) {
  # Validate experimental design
  validateExperimentalDesign(experimentalDesign)
  
  # Process files to create GCT objects
  GCTs <- processCSVExcelFiles(dataFiles, experimentalDesign, identifierColumn,
                              mappingFilePath, mappingProteinCol, mappingGeneCol)
  
  # Create parameters
  parameters <- createCSVExcelParameters(dataFiles)
  
  # Return in same format as GCT workflow
  return(list(
    GCTs = GCTs,
    parameters = parameters
  ))
}