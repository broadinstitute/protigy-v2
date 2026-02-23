################################################################################
# Module: SETUP SIDEBAR
# Functions for processing CSV/Excel files and converting them to GCT format
################################################################################

# UI function for CSV/Excel/TSV label assignment (same pattern as GCT workflow)
csvExcelLabelSetupUI <- function(ns, dataFileNames) {
  tagList(
    h4('Assign labels'),
    lapply(dataFileNames, function(file) {
      tagList(
        add_css_attributes(
          textInput(inputId = ns(paste0('CSVExcelLabel_', file)),
                    label = file,
                    placeholder = "Proteome, Phosphoproteome, etc."),
          classes = "small-input"),
        checkboxInput(ns(paste0("is_spectronaut_", file)), "Spectronaut pivot report", value = FALSE)
      )
    })
  )
}

# UI function for CSV/Excel/TSV identifier column selection (per dataset)
csvExcelIdentifierSetupUI <- function(ns, dataFiles, labels, preprocessed_data = NULL) {
  tagList(
    h4('Select ID column'),

    lapply(seq_len(nrow(dataFiles)), function(i) {
      file_name <- dataFiles$name[i]
      file_path <- dataFiles$datapath[i]
      file_ext <- tools::file_ext(tolower(file_name))
      label <- labels[i]

      # Read data and find unique columns for this file
      unique_columns <- tryCatch({
        # Use preprocessed data if available (e.g., Spectronaut files)
        if (!is.null(preprocessed_data) && !is.null(preprocessed_data[[label]])) {
          data <- preprocessed_data[[label]]
        } else {
          # Check if file exists and is readable
          if (!file.exists(file_path)) {
            stop("File does not exist: ", file_path)
          }

          # Read the full data (not just column names)
          if (file_ext == "csv") {
            data <- readr::read_csv(file_path, show_col_types = FALSE)
          } else if (file_ext == "tsv") {
            data <- readr::read_tsv(file_path, show_col_types = FALSE)
          } else if (file_ext == "ssv") {
            data <- readr::read_delim(file_path, delim = ";", show_col_types = FALSE)
          } else if (file_ext %in% c("xlsx", "xls")) {
            data <- readxl::read_excel(file_path)
          } else {
            stop("Unsupported file format: ", file_ext)
          }
        }

        # Find columns with unique values
        getUniqueColumns(data)
      }, error = function(e) {
        message("Error reading data from ", file_name, " (", file_path, "): ", e$message)
        character(0)
      })
      
      if (length(unique_columns) > 0) {
        div(
          h5(paste("Dataset:", label)),
          p(paste("File:", file_name)),
          div(
            class = "small-input",
            selectInput(
              inputId = ns(paste0("identifierColumn_", i)),
              label = "Select identifier column:",
              choices = unique_columns,
              selected = unique_columns[1]
            )
          ),
          hr()
        )
      } else {
        div(
          h5(paste("Dataset:", label)),
          p(paste("File:", file_name)),
          p(paste("No suitable identifier columns found in", file_name, ". All columns are either numeric, contain duplicate values, or are empty. Please check your file and ensure at least one character column has unique values."), style = "color: red;"),
          hr()
        )
      }
    })
  )
}

# UI function for experimental design inline editor + optional CSV upload fallback
csvExcelExpDesignSetupUI <- function(ns) {
  tagList(
    h4("Experimental Design"),

    # Add / remove factor column controls
    div(
      style = "display:flex; gap:6px; align-items:center; flex-wrap:wrap; margin-bottom:10px;",
      textInput(ns("new_factor_name"), label = NULL, placeholder = "Factor name..."),
      actionButton(ns("add_factor_col"), "Add Factor Column", class = "btn btn-default btn-sm"),
      uiOutput(ns("remove_factor_col_ui"))
    ),

    # Editable spreadsheet table
    rhandsontable::rHandsontableOutput(ns("exp_design_table")),

    hr(),

    # Upload fallback section
    tags$p(
      tags$strong("Or upload a file (CSV / TSV / Excel)"),
      style = "font-size:15px; margin-top:14px; margin-bottom:8px;"
    ),
    p("Download a pre-filled template based on your sample names, edit externally, then upload."),
    downloadButton(ns("downloadExpDesignTemplate"), "Download Template",
                   class = "btn btn-primary btn-sm"),
    div(style = "margin-top:10px;",
      fileInput(
        ns("expDesignFile"),
        label = "Upload completed design:",
        accept = c(".csv", ".tsv", ".ssv", ".xlsx", ".xls"),
        placeholder = "No file selected"
      )
    )
  )
}

# Process CSV/Excel/TSV files with per-dataset identifier columns
# INPUT: list of data files, experimental design data.frame, identifierColumns (vector per dataset)
# OUTPUT: list of GCT objects (same format as existing GCT workflow)
processCSVExcelWorkflowWithPerDatasetIdentifiers <- function(dataFiles, experimentalDesign, identifierColumns, labels, preprocessed_data = NULL) {
  GCTs <- list()
  parameters <- list()

  # Process each file with its specific identifier column
  for (i in seq_len(nrow(dataFiles))) {
    file_path <- dataFiles$datapath[i]
    file_name <- dataFiles$name[i]
    file_ext <- tools::file_ext(tolower(file_name))
    identifier_col <- identifierColumns[i]
    label <- labels[i]  # Use user-assigned label

    tryCatch({
      # Use pre-processed data if provided (e.g. from Spectronaut preprocessing)
      if (!is.null(preprocessed_data) && !is.null(preprocessed_data[[label]])) {
        data <- preprocessed_data[[label]]
      } else if (file_ext == "csv") {
        data <- readr::read_csv(file_path, show_col_types = FALSE)
      } else if (file_ext == "tsv") {
        data <- readr::read_tsv(file_path, show_col_types = FALSE)
      } else if (file_ext == "ssv") {
        data <- readr::read_delim(file_path, delim = ";", show_col_types = FALSE)
      } else if (file_ext %in% c("xlsx", "xls")) {
        data <- readxl::read_excel(file_path)
      } else {
        stop("Unsupported file format: ", file_ext)
      }
      
      # Warn if experimental design lists samples not found in data file
      design_samples <- experimentalDesign$columnName[!is.na(experimentalDesign$columnName)]
      data_cols <- setdiff(colnames(data), identifier_col)
      missing_from_data <- setdiff(design_samples, data_cols)
      if (length(missing_from_data) > 0) {
        warning("[", file_name, "] ", length(missing_from_data),
                " sample(s) in experimental design not found in data: ",
                paste(head(missing_from_data, 5), collapse = ", "),
                if (length(missing_from_data) > 5) "..." else "")
      }

      # Convert to GCT object with specific identifier column
      gct_obj <- convertToGCT(data, experimentalDesign, file_name, identifier_col)
      
      # Use user-assigned label
      GCTs[[label]] <- gct_obj
      
      # Create parameters for this dataset (similar to GCT workflow)
      # Read default parameters from YAML
      default_parameters <- yaml::read_yaml(
        system.file('setup_parameters/setupDefaults.yaml', package = 'Protigy')
      )
      
      parameters[[label]] <- c(
        gct_file_path = file_path,
        gct_file_name = file_name,
        default_parameters
      )
      
    }, error = function(e) {
      stop("Failed to process file ", file_name, ": ", e$message)
    })
  }
  
  return(list(GCTs = GCTs, parameters = parameters))
}

# Process CSV/Excel/TSV files with experimental design to create GCT objects
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
        data <- readr::read_csv(file_path, show_col_types = FALSE)
      } else if (file_ext == "tsv") {
        data <- readr::read_tsv(file_path, show_col_types = FALSE)
      } else if (file_ext == "ssv") {
        data <- readr::read_delim(file_path, delim = ";", show_col_types = FALSE)
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

# Check which columns have unique values and are character type (suitable as identifier columns)
# INPUT: data.frame
# OUTPUT: character vector of column names with unique character values
getUniqueColumns <- function(data) {
  unique_columns <- character(0)
  
  for (col_name in colnames(data)) {
    # Get non-NA values
    values <- data[[col_name]]
    non_na_values <- values[!is.na(values)]
    
    # Check if column is character type and has unique values
    if (length(non_na_values) > 0 && 
        is.character(non_na_values) && 
        length(non_na_values) == length(unique(non_na_values))) {
      unique_columns <- c(unique_columns, col_name)
    }
  }
  
  return(unique_columns)
}

# Validate that the user-specified identifier column exists and is valid
# INPUT: data.frame, user-specified identifier column
# OUTPUT: validated identifier column name
validateIdentifierColumn <- function(data, identifierColumn) {
  if (is.null(identifierColumn) || identifierColumn == "") {
    stop("Identifier column must be specified")
  }
  
  if (!identifierColumn %in% colnames(data)) {
    stop("Identifier column '", identifierColumn, "' not found in data")
  }
  
  return(identifierColumn)
}


#' Validate that identifier column has unique values
#'
#' @param data data.frame containing the identifier column
#' @param identifier_column character name of the identifier column to validate
#' @return \code{TRUE} invisibly if valid; otherwise throws an error
#' @details Errors if the identifier column contains any NA values (all feature IDs
#'   must be non-missing). Also errors on duplicate or empty string values.
validateUniqueIdentifiers <- function(data, identifier_column) {
  if (!identifier_column %in% colnames(data)) {
    stop("Identifier column '", identifier_column, "' not found in data")
  }
  
  identifier_values <- data[[identifier_column]]
  
  # Remove NA values for duplicate checking
  non_na_values <- identifier_values[!is.na(identifier_values)]

  # Reject rows where identifier is NA
  na_count <- sum(is.na(identifier_values))
  if (na_count > 0) {
    stop("Identifier column '", identifier_column, "' has ", na_count,
         " NA/missing value(s). All feature IDs must be non-missing.")
  }

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
convertToGCT <- function(data, experimentalDesign, file_name, identifierColumn) {
  
  # Validate that the identifier column exists and is valid
  final_identifier_column <- validateIdentifierColumn(data, identifierColumn)
  
  # Validate that the identifier column has unique values
  validateUniqueIdentifiers(data, final_identifier_column)
  
  # Get feature IDs from the determined identifier column
  feature_id_col <- which(colnames(data) == final_identifier_column)
  if (length(feature_id_col) == 0) {
    stop("Identifier column '", final_identifier_column, "' not found in data after preprocessing. ",
         "The column may have been removed during Spectronaut preprocessing.")
  }
  feature_ids <- data[[feature_id_col]]
  
  # Get all sample IDs from data (all columns except identifier column)
  all_sample_ids <- colnames(data[, -feature_id_col, drop = FALSE])
  
  # Identify columns that should be moved to rdesc vs kept as samples
  column_classification <- classifyColumns(all_sample_ids, experimentalDesign)
  experimental_sample_ids <- column_classification$sample_columns
  rdesc_columns <- column_classification$rdesc_columns

  # Surface diagnostics about column classification
  n_not_found <- column_classification$n_not_in_design
  n_metadata  <- column_classification$n_all_na_meta
  n_samples   <- length(experimental_sample_ids)

  if (n_not_found > 0) {
    message("[", file_name, "] ", n_not_found,
            " column(s) not in experimental design \u2192 moved to feature metadata (rdesc).")
  }
  if (n_metadata > 0) {
    message("[", file_name, "] ", n_metadata,
            " column(s) found in design but all metadata values are NA \u2192 treated as feature metadata (rdesc).")
  }
  message("[", file_name, "] ", n_samples, " sample column(s) identified.")

  # Check if we have any experimental columns
  if (length(experimental_sample_ids) == 0) {
    stop("No experimental columns found with valid metadata for file: ", file_name)
  }
  
  # Extract data matrix using only experimental columns
  experimental_columns <- c(feature_id_col, which(colnames(data) %in% experimental_sample_ids))
  filtered_data <- data[, experimental_columns, drop = FALSE]
  data_matrix <- as.matrix(filtered_data[, -1, drop = FALSE]) # Remove identifier column from matrix
  rownames(data_matrix) <- feature_ids

  # Ensure numeric: Excel/readxl may read some sample columns as character (e.g. mixed types),
  # which would make the matrix character and break median() in normalization.
  if (!is.numeric(data_matrix)) {
    dim_prev <- dim(data_matrix)
    dimnames_prev <- dimnames(data_matrix)
    data_matrix <- matrix(
      as.numeric(data_matrix),
      nrow = dim_prev[1L],
      ncol = dim_prev[2L],
      dimnames = dimnames_prev
    )
  }

  # Get final sample IDs (should be the experimental ones)
  sample_ids <- colnames(data_matrix)
  
  # Create rdesc (row descriptor) - start with identifier column
  rdesc <- data.frame(
    id = feature_ids,
    id.description = feature_ids,
    stringsAsFactors = FALSE
  )
  rownames(rdesc) <- feature_ids

  # Add all non-sample, non-identifier columns to rdesc (metadata/annotation columns)
  all_cols <- colnames(data)
  sample_and_id_cols <- c(all_cols[feature_id_col], experimental_sample_ids)
  candidate_rdesc_cols <- setdiff(all_cols, sample_and_id_cols)

  for (col in candidate_rdesc_cols) {
    rdesc[[col]] <- data[[col]]
  }
  
  
  # Create cdesc (column descriptor) from experimental design
  cdesc <- createCdesc(sample_ids, experimentalDesign, file_name)
  
  # Create GCT object using cmapR
  gct_obj <- cmapR::GCT(
    mat = data_matrix,
    rdesc = rdesc,
    cdesc = cdesc,
    rid = rownames(data_matrix)
  )
  
  return(gct_obj)
}

#' Classify columns as sample columns or row descriptor (rdesc) columns
#'
#' @param sample_ids character vector of column names from the data file (excluding the identifier column)
#' @param experimentalDesign data.frame with at least a \code{columnName} column
#' @return Named list with: \code{sample_columns} (character vector of data/sample columns),
#'   \code{rdesc_columns} (character vector of metadata/annotation columns),
#'   \code{n_not_in_design} (integer count of columns absent from experimental design),
#'   \code{n_all_na_meta} (integer count of columns in design with all-NA factor values).
#' @details A column is classified as a \strong{data/sample column} if its entry in the
#'   experimental design has at least one non-NA, non-empty value in the factor columns.
#'   A column is classified as a \strong{metadata/rdesc column} if: (a) it is not present
#'   in the experimental design at all, or (b) all its factor column values are NA or empty.
#'   NAs in factor columns for rdesc rows are intentional and expected — they represent
#'   feature annotation like protein group IDs or gene symbols, not missing data.
classifyColumns <- function(sample_ids, experimentalDesign) {
  sample_columns <- character(0)
  rdesc_columns <- character(0)
  
  for (col_name in sample_ids) {
    # Check if column exists in experimental design
    exp_design_row <- experimentalDesign[experimentalDesign$columnName == col_name, ]
    
    if (nrow(exp_design_row) == 0) {
      # Case 1: columnName is missing from experimental design file
      # This column should be moved to rdesc
      rdesc_columns <- c(rdesc_columns, col_name)
    } else {
      # Case 2: columnName is present in experimental design
      # Check if all metadata entries are blank/NA
      metadata_columns <- setdiff(names(experimentalDesign), "columnName")
      
      if (length(metadata_columns) == 0) {
        # No metadata columns - treat as sample
        sample_columns <- c(sample_columns, col_name)
      } else {
        # Check if all metadata values are NA/blank
        all_metadata_values <- exp_design_row[, metadata_columns, drop = FALSE]
        all_blank <- all(is.na(all_metadata_values) | 
                        as.character(all_metadata_values) == "" | 
                        trimws(as.character(all_metadata_values)) == "")
        
        if (all_blank) {
          # All metadata entries are blank/NA - move to rdesc
          rdesc_columns <- c(rdesc_columns, col_name)
        } else {
          # Has valid metadata - treat as sample
          sample_columns <- c(sample_columns, col_name)
        }
      }
    }
  }
  
  # Count columns not found in design vs found but with all-NA metadata
  n_not_in_design <- 0L
  n_all_na_meta   <- 0L
  for (col_name in sample_ids) {
    exp_design_row <- experimentalDesign[experimentalDesign$columnName == col_name, ]
    if (nrow(exp_design_row) == 0) {
      n_not_in_design <- n_not_in_design + 1L
    } else if (col_name %in% rdesc_columns) {
      n_all_na_meta <- n_all_na_meta + 1L
    }
  }

  return(list(
    sample_columns  = sample_columns,
    rdesc_columns   = rdesc_columns,
    n_not_in_design = n_not_in_design,
    n_all_na_meta   = n_all_na_meta
  ))
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

#' Read a preview of an uploaded data file (first n_max rows)
#'
#' Thin wrapper around readr/readxl for the file types supported in the
#' CSV/Excel workflow. Returns a data.frame or NULL on error.
#'
#' @param file_path character path to the file
#' @param file_ext character file extension (lowercase, without dot): "csv", "tsv", "xlsx", "xls", "ssv"
#' @param n_max integer max rows to read (default 20)
#' @return data.frame or NULL
read_uploaded_data_preview <- function(file_path, file_ext, n_max = 20) {
  tryCatch({
    data <- if (file_ext == "csv") {
      readr::read_csv(file_path, n_max = n_max, show_col_types = FALSE)
    } else if (file_ext == "tsv") {
      readr::read_tsv(file_path, n_max = n_max, show_col_types = FALSE)
    } else if (file_ext %in% c("xlsx", "xls")) {
      readxl::read_excel(file_path, n_max = n_max)
    } else if (file_ext == "ssv") {
      readr::read_delim(file_path, delim = ";", n_max = n_max, show_col_types = FALSE)
    } else {
      return(NULL)
    }
    as.data.frame(data)
  }, error = function(e) {
    NULL
  })
}