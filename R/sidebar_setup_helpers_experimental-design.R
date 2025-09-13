################################################################################
# Module: SETUP SIDEBAR
# Functions for experimental design template generation and validation for CSV/Excel imports
################################################################################

# Generate experimental design template from uploaded data file
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
      
      # Add all column names for classification
      all_column_names <- c(all_column_names, column_names)
      
    }, error = function(e) {
      warning("Failed to read file ", dataFiles$name[i], ": ", e$message)
    })
  }
  
  # Create experimental design template with all columns
  # Include both experimental and non-experimental columns so user can see and modify all
  template <- data.frame(
    column_name = all_column_names,
    experiment = rep(NA, length(all_column_names)),
    condition = rep(NA, length(all_column_names)),
    replicate = rep(NA, length(all_column_names)),
    stringsAsFactors = FALSE
  )
  
  return(template)
}

# Validate experimental design template
validateExperimentalDesign <- function(exp_design) {
  # Check that columnName column exists (this is the only required column)
  if (!"columnName" %in% names(exp_design)) {
    stop("Missing required column: columnName")
  }
  
  # Check for empty string throughout the dataframe
  if (any(exp_design == "", na.rm = TRUE)) {
    stop("Missing or empty values found in experimental design.")
  }
  
  return(TRUE)
}

# Read and validate uploaded experimental design file
# INPUT: file path to experimental design CSV
# OUTPUT: validated data.frame
readExperimentalDesign <- function(file_path) {
  tryCatch({
    # Try reading as CSV first
    exp_design <- readr::read_csv(file_path)
    
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