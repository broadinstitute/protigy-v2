################################################################################
# Module: SETUP SIDEBAR
# Functions for experimental design template generation and validation for CSV/Excel imports
################################################################################

# Generate experimental design template from uploaded CSV/Excel files
# INPUT: list of data files (from fileInput), identifierColumn (column name to exclude)
# OUTPUT: data.frame with columnName, experiment, condition columns
generateExperimentalDesignTemplate <- function(dataFiles, identifierColumn = NULL) {
  all_column_names <- c()
  
  # Process each file to extract column names
  for (i in seq_len(nrow(dataFiles))) {
    file_path <- dataFiles$datapath[i]
    file_ext <- tools::file_ext(tolower(dataFiles$name[i]))
    
    tryCatch({
      if (file_ext == "csv") {
        # Read CSV file - just the first row to get column names
        data <- utils::read.csv(file_path, nrows = 1, stringsAsFactors = FALSE)
        column_names <- names(data)
      } else if (file_ext %in% c("xlsx", "xls")) {
        # Read Excel file - just the first row to get column names
        data <- readxl::read_excel(file_path, n_max = 1)
        column_names <- names(data)
      } else {
        stop("Unsupported file format: ", file_ext)
      }
      
      # Exclude the identifier column if specified
      if (!is.null(identifierColumn) && identifierColumn %in% column_names) {
        sample_columns <- column_names[column_names != identifierColumn]
      } else {
        sample_columns <- column_names
      }
      
      all_column_names <- c(all_column_names, sample_columns)
      
    }, error = function(e) {
      warning("Failed to read file ", dataFiles$name[i], ": ", e$message)
    })
  }
  
  # Remove duplicates and create template
  unique_columns <- unique(all_column_names)
  
  if (length(unique_columns) == 0) {
    stop("No valid sample columns found in the uploaded files.")
  }
  
  # Create experimental design template with example metadata columns
  # Users can modify, add, or remove these columns as needed
  template <- data.frame(
    columnName = unique_columns,
    experiment = rep(NA, length(unique_columns)),
    condition = rep(NA, length(unique_columns)),
    treatment = rep(NA, length(unique_columns)),
    timepoint = rep(NA, length(unique_columns)),
    stringsAsFactors = FALSE
  )
  
  return(template)
}

# Validate experimental design template
# INPUT: data.frame with experimental design
# OUTPUT: TRUE if valid, throws error if invalid
validateExperimentalDesign <- function(exp_design) {
  # Check that columnName column exists (this is the only required column)
  if (!"columnName" %in% names(exp_design)) {
    stop("Missing required column: columnName")
  }
  
  # Check column names are valid (these should never be NA)
  if (any(is.na(exp_design$columnName) | exp_design$columnName == "")) {
    stop("columnName column contains missing or empty values.")
  }
  
  # Get metadata columns (all columns except columnName)
  metadata_columns <- setdiff(names(exp_design), "columnName")
  
  if (length(metadata_columns) == 0) {
    stop("No metadata columns found. Please add at least one metadata column (e.g., experiment, condition, treatment, etc.)")
  }
  
  # Check that we have at least some experimental rows
  # A row is considered experimental if it has at least one non-NA, non-empty metadata value
  experimental_rows <- rep(FALSE, nrow(exp_design))
  
  for (col in metadata_columns) {
    col_valid <- !is.na(exp_design[[col]]) & exp_design[[col]] != ""
    experimental_rows <- experimental_rows | col_valid
  }
  
  if (!any(experimental_rows)) {
    stop("No valid experimental columns found. Please ensure at least some rows have metadata values filled (non-NA and non-empty) in at least one metadata column.")
  }
  
  # For each experimental row, check that no metadata columns have mixed NA/empty and filled values
  # (This helps catch data entry errors, but is not strictly required)
  for (i in which(experimental_rows)) {
    row_values <- exp_design[i, metadata_columns, drop = FALSE]
    has_values <- !is.na(row_values) & row_values != ""
    
    # If a row has some metadata but not all, give a warning (not an error)
    if (any(has_values) && !all(has_values)) {
      missing_cols <- metadata_columns[!has_values]
      warning("Row ", i, " (", exp_design$columnName[i], ") has incomplete metadata. Missing values in: ", 
              paste(missing_cols, collapse = ", "), ". This column will be treated as experimental.")
    }
  }
  
  return(TRUE)
}

# Read and validate uploaded experimental design file
# INPUT: file path to experimental design CSV
# OUTPUT: validated data.frame
readExperimentalDesign <- function(file_path) {
  tryCatch({
    # Try reading as CSV first
    exp_design <- utils::read.csv(file_path, stringsAsFactors = FALSE)
    
    # Validate the experimental design
    validateExperimentalDesign(exp_design)
    
    return(exp_design)
    
  }, error = function(e) {
    stop("Failed to read or validate experimental design file: ", e$message)
  })
}

# Create downloadable experimental design template
# INPUT: experimental design template data.frame
# OUTPUT: temporary file path for download
createDownloadableTemplate <- function(template) {
  temp_file <- tempfile(fileext = ".csv")
  
  tryCatch({
    utils::write.csv(template, temp_file, row.names = FALSE)
    return(temp_file)
  }, error = function(e) {
    stop("Failed to create downloadable template: ", e$message)
  })
}