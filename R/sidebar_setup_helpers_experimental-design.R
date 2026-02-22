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

#' Validate an experimental design data frame
#'
#' Checks that the \code{columnName} column exists, contains no NA values, and
#' has no duplicate entries. NA values in factor/metadata columns are intentionally
#' permitted — rows where all factor columns are NA represent feature annotation
#' columns (rdesc) rather than sample/data columns.
#'
#' @param exp_design data.frame with at least a \code{columnName} column
#' @return \code{TRUE} invisibly if valid; otherwise throws an error
#' @details The distinction between metadata rows and data rows:
#'   \itemize{
#'     \item \strong{Metadata rows} (rdesc): all factor columns are NA. These become
#'       row descriptor columns in the GCT object, containing feature annotations like
#'       gene symbols, protein descriptions, etc.
#'     \item \strong{Data rows} (sample columns): at least one factor column has a
#'       non-NA value. These become sample columns in the GCT matrix.
#'   }
#'   Only \code{columnName} NA or duplicates are true validation errors.
validateExperimentalDesign <- function(exp_design) {
  if (!"columnName" %in% names(exp_design)) {
    stop("Missing required column: columnName")
  }

  # columnName itself must not contain NA (each row must identify what it is)
  na_colname_rows <- which(is.na(exp_design$columnName))
  if (length(na_colname_rows) > 0) {
    stop("The 'columnName' column has NA values in row(s): ",
         paste(na_colname_rows, collapse = ", "),
         ". Every row must have a columnName value.")
  }

  # Duplicate columnNames are not allowed
  dupes <- exp_design$columnName[duplicated(exp_design$columnName)]
  if (length(dupes) > 0) {
    stop("Duplicate values found in 'columnName': ", paste(unique(dupes), collapse = ", "))
  }

  # Identify and log column types based on NA pattern in factor columns.
  # Metadata columns: all factor columns are NA → rdesc/feature annotation.
  # Data columns: at least one factor column is non-NA → sample/data.
  factor_cols <- setdiff(names(exp_design), "columnName")
  if (length(factor_cols) > 0) {
    is_metadata_row <- apply(exp_design[, factor_cols, drop = FALSE], 1, function(row) all(is.na(row)))
    n_metadata <- sum(is_metadata_row)
    n_data <- sum(!is_metadata_row)
    message("Experimental design: ", n_data, " data/sample column(s), ",
            n_metadata, " metadata/annotation column(s).")
  }

  return(TRUE)
}

# Read and validate uploaded experimental design file
# INPUT: file path to experimental design file (CSV, TSV, SSV, or Excel)
# OUTPUT: validated data.frame
readExperimentalDesign <- function(file_path) {
  tryCatch({
    # Determine file extension
    file_ext <- tools::file_ext(tolower(file_path))
    
    # Read file based on extension
    if (file_ext == "csv") {
      exp_design <- readr::read_csv(file_path)
    } else if (file_ext == "tsv") {
      exp_design <- readr::read_tsv(file_path)
    } else if (file_ext == "ssv") {
      exp_design <- readr::read_delim(file_path, delim = ";")
    } else if (file_ext %in% c("xlsx", "xls")) {
      exp_design <- readxl::read_excel(file_path)
    } else {
      stop("Unsupported file format: ", file_ext, ". Supported formats are CSV, TSV, SSV, and Excel.")
    }
    
    # Replace empty strings with NA so metadata-only rows don't fail validation
    exp_design[exp_design == ""] <- NA

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