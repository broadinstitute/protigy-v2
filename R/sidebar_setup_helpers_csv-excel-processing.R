################################################################################
# Module: SETUP SIDEBAR
# Functions for processing CSV/Excel files and converting them to GCT format
################################################################################

# Process CSV/Excel files with experimental design to create GCT objects
# INPUT: list of data files, experimental design data.frame, identifierColumn (optional)
# OUTPUT: list of GCT objects (same format as existing GCT workflow)
processCSVExcelFiles <- function(dataFiles, experimentalDesign, identifierColumn = NULL) {
  GCTs <- list()
  
  # Process each file
  for (i in seq_len(nrow(dataFiles))) {
    file_path <- dataFiles$datapath[i]
    file_name <- dataFiles$name[i]
    file_ext <- tools::file_ext(tolower(file_name))
    
    tryCatch({
      # Read the data file
      if (file_ext == "csv") {
        data <- utils::read.csv(file_path, stringsAsFactors = FALSE)
      } else if (file_ext %in% c("xlsx", "xls")) {
        data <- readxl::read_excel(file_path)
      } else {
        stop("Unsupported file format: ", file_ext)
      }
      
      # Convert to GCT object
      gct_obj <- convertToGCT(data, experimentalDesign, file_name, identifierColumn)
      
      # Create a simple label from filename (remove extension)
      label <- tools::file_path_sans_ext(file_name)
      GCTs[[label]] <- gct_obj
      
    }, error = function(e) {
      stop("Failed to process file ", file_name, ": ", e$message)
    })
  }
  
  return(GCTs)
}

# Convert CSV/Excel data to GCT format
# INPUT: data.frame from CSV/Excel, experimental design, file name, identifierColumn (optional)
# OUTPUT: GCT object compatible with existing workflow
convertToGCT <- function(data, experimentalDesign, file_name, identifierColumn = NULL) {
  # Determine which column contains the feature IDs
  if (!is.null(identifierColumn) && identifierColumn %in% colnames(data)) {
    # Use specified identifier column
    feature_id_col <- which(colnames(data) == identifierColumn)
    feature_ids <- data[[feature_id_col]]
    
    # Get all sample IDs from data (all columns except identifier column)
    all_sample_ids <- colnames(data[, -feature_id_col, drop = FALSE])
  } else {
    # Default: use first column as feature IDs
    feature_ids <- data[[1]]
    
    # Get all sample IDs from data (all columns except first)
    all_sample_ids <- colnames(data[, -1, drop = FALSE])
  }
  
  # Filter to only experimental columns (those with valid metadata)
  experimental_sample_ids <- filterExperimentalColumns(all_sample_ids, experimentalDesign)
  
  # Check if we have any experimental columns
  if (length(experimental_sample_ids) == 0) {
    stop("No experimental columns found with valid metadata (non-NA experiment and condition) for file: ", file_name)
  }
  
  # Extract data matrix using only experimental columns
  if (!is.null(identifierColumn) && identifierColumn %in% colnames(data)) {
    # Include identifier column and experimental columns
    experimental_columns <- c(feature_id_col, which(colnames(data) %in% experimental_sample_ids))
    filtered_data <- data[, experimental_columns, drop = FALSE]
    data_matrix <- as.matrix(filtered_data[, -1, drop = FALSE]) # Remove identifier column from matrix
  } else {
    # Default logic: first column is identifier
    experimental_columns <- c(1, which(colnames(data) %in% experimental_sample_ids))
    filtered_data <- data[, experimental_columns, drop = FALSE]
    data_matrix <- as.matrix(filtered_data[, -1, drop = FALSE]) # Remove first column from matrix
  }
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
    return(character(0))
  }
  
  # Find samples that have valid experimental metadata 
  # (at least one metadata column with non-NA, non-empty value)
  valid_samples <- rep(FALSE, length(sample_ids))
  
  for (col in metadata_columns) {
    col_valid <- !is.na(exp_design_matched[[col]]) & exp_design_matched[[col]] != ""
    valid_samples <- valid_samples | col_valid
  }
  
  # Return only the sample IDs that have valid metadata
  return(sample_ids[valid_samples])
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
    stop("Sample IDs not found in experimental design: ", paste(missing_samples, collapse = ", "))
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
    cdesc[[col]] <- exp_design_matched[[col]]
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
# INPUT: data files, experimental design, identifierColumn (optional)
# OUTPUT: list with GCTs and parameters (same format as GCT workflow)
processCSVExcelWorkflow <- function(dataFiles, experimentalDesign, identifierColumn = NULL) {
  # Validate experimental design
  validateExperimentalDesign(experimentalDesign)
  
  # Process files to create GCT objects
  GCTs <- processCSVExcelFiles(dataFiles, experimentalDesign, identifierColumn)
  
  # Create parameters
  parameters <- createCSVExcelParameters(dataFiles)
  
  # Return in same format as GCT workflow
  return(list(
    GCTs = GCTs,
    parameters = parameters
  ))
}