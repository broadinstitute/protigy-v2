################################################################################
# Module: SETUP SIDEBAR
# Functions for processing CSV/Excel files and converting them to GCT format
################################################################################

# UI function for CSV/Excel/TSV label assignment (same pattern as GCT workflow)
csvExcelLabelSetupUI <- function(ns, dataFileNames) {
  tagList(
    h4('Assign labels'),
    lapply(dataFileNames, function(file) {
      add_css_attributes(
        textInput(inputId = ns(paste0('CSVExcelLabel_', file)),
                  label = file,
                  placeholder = "Proteome, Phosphoproteome, etc."),
        classes = "small-input")
    })
  )
}

# UI function for CSV/Excel/TSV identifier column selection (per dataset)
csvExcelIdentifierSetupUI <- function(ns, dataFiles, labels) {
  tagList(
    h4('Select ID column'),
    
    lapply(seq_len(nrow(dataFiles)), function(i) {
      file_name <- dataFiles$name[i]
      file_path <- dataFiles$datapath[i]
      file_ext <- tools::file_ext(tolower(file_name))
      label <- labels[i]
      
      # Read data and find unique columns for this file
      unique_columns <- tryCatch({
        # Check if file exists and is readable
        if (!file.exists(file_path)) {
          stop("File does not exist: ", file_path)
        }
        
        # Read the full data (not just column names)
        if (file_ext == "csv") {
          data <- readr::read_csv(file_path, show_col_types = FALSE)
        } else if (file_ext == "tsv") {
          data <- readr::read_tsv(file_path, show_col_types = FALSE)
        } else if (file_ext %in% c("xlsx", "xls")) {
          data <- readxl::read_excel(file_path)
        } else {
          stop("Unsupported file format: ", file_ext)
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

# UI function for experimental design template download and upload
csvExcelExpDesignSetupUI <- function(ns, dataFiles, labels) {
  tagList(
    h4('Experimental Design'),
    
    # Template download section
    div(
      style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;",
      'Download Template'
    ),
    p('Download a template experimental design file that you can fill in with your sample information.'),
    
    div(
      class = "small-input",
      downloadButton(
        outputId = ns("downloadExpDesignTemplate"),
        label = "Download Template",
        class = "btn btn-primary"
      )
    ),
    
    hr(),
    
    # Experimental design upload section
    div(
      style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;",
      'Upload Experimental Design'
    ),
    p('Upload your completed experimental design file (CSV, TSV, or Excel format).'),
    
    fileInput(
      inputId = ns("expDesignFile"),
      label = "Choose experimental design file:",
      accept = c(".csv", ".tsv", ".xlsx", ".xls"),
      placeholder = "No file selected"
    ),
    
    hr(),
    
    # Process button (will be shown when experimental design is uploaded)
    div(
      id = ns("processButtonContainer"),
      style = "display: none;",
      actionButton(
        inputId = ns("processCSVExcel"),
        label = "Process Files",
        class = "btn btn-primary"
      )
    ),
    
    # JavaScript to show/hide process button based on file upload
    tags$script(HTML(paste0("
      $(document).on('change', '#", ns("expDesignFile"), "', function() {
        if (this.files.length > 0) {
          $('#", ns("processButtonContainer"), "').show();
        } else {
          $('#", ns("processButtonContainer"), "').hide();
        }
      });
    ")))
  )
}

# Process CSV/Excel/TSV files with per-dataset identifier columns
# INPUT: list of data files, experimental design data.frame, identifierColumns (vector per dataset)
# OUTPUT: list of GCT objects (same format as existing GCT workflow)
processCSVExcelWorkflowWithPerDatasetIdentifiers <- function(dataFiles, experimentalDesign, identifierColumns, labels) {
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
      # Read the data file
      if (file_ext == "csv") {
        data <- readr::read_csv(file_path)
      } else if (file_ext == "tsv") {
        data <- readr::read_tsv(file_path)
      } else if (file_ext %in% c("xlsx", "xls")) {
        data <- readxl::read_excel(file_path)
      } else {
        stop("Unsupported file format: ", file_ext)
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
        data <- readr::read_csv(file_path)
      } else if (file_ext == "tsv") {
        data <- readr::read_tsv(file_path)
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
convertToGCT <- function(data, experimentalDesign, file_name, identifierColumn) {
  
  # Validate that the identifier column exists and is valid
  final_identifier_column <- validateIdentifierColumn(data, identifierColumn)
  
  # Validate that the identifier column has unique values
  validateUniqueIdentifiers(data, final_identifier_column)
  
  # Get feature IDs from the determined identifier column
  feature_id_col <- which(colnames(data) == final_identifier_column)
  feature_ids <- data[[feature_id_col]]
  
  # Get all sample IDs from data (all columns except identifier column)
  all_sample_ids <- colnames(data[, -feature_id_col, drop = FALSE])
  
  # Identify columns that should be moved to rdesc vs kept as samples
  column_classification <- classifyColumns(all_sample_ids, experimentalDesign)
  experimental_sample_ids <- column_classification$sample_columns
  rdesc_columns <- column_classification$rdesc_columns
  
  # Check if we have any experimental columns
  if (length(experimental_sample_ids) == 0) {
    stop("No experimental columns found with valid metadata for file: ", file_name)
  }
  
  # Extract data matrix using only experimental columns
  experimental_columns <- c(feature_id_col, which(colnames(data) %in% experimental_sample_ids))
  filtered_data <- data[, experimental_columns, drop = FALSE]
  data_matrix <- as.matrix(filtered_data[, -1, drop = FALSE]) # Remove identifier column from matrix
  rownames(data_matrix) <- feature_ids
  
  # Get final sample IDs (should be the experimental ones)
  sample_ids <- colnames(data_matrix)
  
  # Create rdesc (row descriptor) - start with identifier column
  rdesc <- data.frame(
    id = feature_ids,
    id.description = feature_ids,
    stringsAsFactors = FALSE
  )
  rownames(rdesc) <- feature_ids
  
  # Add rdesc columns (metadata columns that should be in rdesc)
  if (length(rdesc_columns) > 0) {
    rdesc_data <- data[, c(feature_id_col, which(colnames(data) %in% rdesc_columns)), drop = FALSE]
    for (col in rdesc_columns) {
      rdesc[[col]] <- rdesc_data[[col]]
    }
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

# Classify columns as either sample columns or rdesc columns based on experimental design
# INPUT: sample IDs from data file, experimental design
# OUTPUT: list with sample_columns and rdesc_columns vectors
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
  
  return(list(
    sample_columns = sample_columns,
    rdesc_columns = rdesc_columns
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