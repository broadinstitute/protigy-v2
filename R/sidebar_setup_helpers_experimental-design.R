################################################################################
# Module: SETUP SIDEBAR
# Functions for experimental design template generation and validation for CSV/Excel imports
################################################################################

# Classify columns as experimental vs non-experimental
# INPUT: vector of column names, identifier column to exclude
# OUTPUT: list with experimental_columns and non_experimental_columns
classifyColumns <- function(column_names, identifier_column = NULL) {
  # Define patterns for non-experimental columns (case-insensitive)
  non_experimental_patterns <- c(
    "^id$", "^ids$", "^identifier", "^accession", "^acc$",
    "^protein", "^gene", "^symbol", "^name$", "^names$",
    "^description", "^desc", "^annotation", "^annot",
    "^uniprot", "^swiss", "^refseq", "^ensembl",
    "^sequence", "^seq", "^peptide", "^length", "^len$",
    "^mw$", "^molecular", "^mass", "^weight", "^kda$",
    "^coverage", "^cov$", "^score", "^confidence", "^conf$",
    "^fdr$", "^qval", "^q\\.val", "^q_val", "^pval", "^p\\.val", "^p_val",
    "^adj", "^padj", "^bh$", "^bonf", "^holm$",
    "^fold", "^fc$", "^log.*fc", "^ratio", "^abundance",
    "^intensity.*total", "^total.*intensity", "^sum.*intensity",
    "^pg\\.", "^protein.*group", "^group", "^cluster",
    "^ontology", "^go\\.", "^kegg", "^pathway", "^term$",
    "^organism", "^species", "^taxonomy", "^tax$",
    "^chromosome", "^chr$", "^position", "^pos$", "^coord",
    "^strand", "^start$", "^end$", "^genomic"
  )
  
  # Remove identifier column from consideration
  if (!is.null(identifier_column) && identifier_column %in% column_names) {
    column_names <- column_names[column_names != identifier_column]
  }
  
  # Classify columns
  non_experimental_columns <- c()
  experimental_columns <- c()
  
  for (col_name in column_names) {
    # Check if column matches any non-experimental pattern
    is_non_experimental <- any(sapply(non_experimental_patterns, function(pattern) {
      grepl(pattern, col_name, ignore.case = TRUE)
    }))
    
    if (is_non_experimental) {
      non_experimental_columns <- c(non_experimental_columns, col_name)
    } else {
      # Additional heuristics for experimental columns
      # Look for patterns that suggest sample names or experimental conditions
      
      # Patterns that suggest experimental/sample columns
      sample_patterns <- c(
        "sample", "ctrl", "control", "treat", "rep", "replicate", 
        "_[0-9]+$", "_rep", "_ctrl", "_treat", "_[a-z][0-9]$",
        "^[a-z]+[0-9]+$", "^[a-z]+_[a-z0-9]+$", "^[a-z]+-[a-z0-9]+$"
      )
      
      looks_like_sample <- any(sapply(sample_patterns, function(pattern) {
        grepl(pattern, col_name, ignore.case = TRUE)
      })) &&
      nchar(col_name) <= 50 &&  # Reasonable sample name length
      !grepl("\\.(txt|csv|xlsx?|pdf|doc|html)$", col_name, ignore.case = TRUE)
      
      # Additional check: if it looks like it could contain numerical data
      # (not ending with descriptive suffixes)
      not_descriptive <- !grepl("(name|desc|annotation|info|note|comment)$", col_name, ignore.case = TRUE)
      
      if (looks_like_sample || (not_descriptive && !grepl("^(id|identifier|accession)", col_name, ignore.case = TRUE))) {
        experimental_columns <- c(experimental_columns, col_name)
      } else {
        # When in doubt, classify as non-experimental to be conservative
        non_experimental_columns <- c(non_experimental_columns, col_name)
      }
    }
  }
  
  return(list(
    experimental_columns = experimental_columns,
    non_experimental_columns = non_experimental_columns,
    identifier_column = identifier_column
  ))
}

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
      
      # Add all column names for classification
      all_column_names <- c(all_column_names, column_names)
      
    }, error = function(e) {
      warning("Failed to read file ", dataFiles$name[i], ": ", e$message)
    })
  }
  
  # Remove duplicates and classify columns
  unique_columns <- unique(all_column_names)
  
  if (length(unique_columns) == 0) {
    stop("No valid columns found in the uploaded files.")
  }
  
  # Classify columns into experimental vs non-experimental
  classification <- classifyColumns(unique_columns, identifierColumn)
  
  experimental_columns <- classification$experimental_columns
  non_experimental_columns <- classification$non_experimental_columns
  
  if (length(experimental_columns) == 0) {
    # If no experimental columns detected, provide guidance
    warning(paste(
      "No experimental columns were automatically detected.",
      "All columns appear to be metadata/descriptive.",
      "Please verify your data file contains sample columns with numerical data.",
      "Detected non-experimental columns:", paste(non_experimental_columns, collapse = ", ")
    ))
    
    # Create a minimal template with just the first few columns that might be samples
    # Let user override if needed
    potential_samples <- head(non_experimental_columns, min(5, length(non_experimental_columns)))
    experimental_columns <- potential_samples
  }
  
  # Create experimental design template with all columns
  # Include both experimental and non-experimental columns so user can see and modify all
  all_template_columns <- c(experimental_columns, non_experimental_columns)
  template <- data.frame(
    columnName = all_template_columns,
    experiment = rep(NA, length(all_template_columns)),
    condition = rep(NA, length(all_template_columns)),
    treatment = rep(NA, length(all_template_columns)),
    timepoint = rep(NA, length(all_template_columns)),
    stringsAsFactors = FALSE
  )
  
  # Add attributes to provide feedback to user
  attr(template, "classification") <- classification
  attr(template, "excluded_columns") <- non_experimental_columns
  
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
    if (col %in% colnames(exp_design)) {
      col_values <- exp_design[[col]]
      # Check for valid values: not NA, not empty string, not just whitespace
      col_valid <- !is.na(col_values) & 
                  as.character(col_values) != "" & 
                  trimws(as.character(col_values)) != ""
      experimental_rows <- experimental_rows | col_valid
    }
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

# Generate experimental design template from processed TSV data
# INPUT: list of processed TSV data frames, identifierColumn (column name to exclude)
# OUTPUT: data.frame with columnName, experiment, condition columns
generateExperimentalDesignTemplateForTSV <- function(processedTSVData, identifierColumn = NULL) {
  all_column_names <- c()
  
  # Extract column names from all processed TSV data frames
  for (dataset_name in names(processedTSVData)) {
    data <- processedTSVData[[dataset_name]]
    column_names <- names(data)
    
    # Add all column names for classification
    all_column_names <- c(all_column_names, column_names)
  }
  
  # Remove duplicates and classify columns
  unique_columns <- unique(all_column_names)
  
  if (length(unique_columns) == 0) {
    stop("No valid columns found in the processed TSV data.")
  }
  
  # Classify columns into experimental vs non-experimental
  classification <- classifyColumns(unique_columns, identifierColumn)
  
  experimental_columns <- classification$experimental_columns
  non_experimental_columns <- classification$non_experimental_columns
  
  if (length(experimental_columns) == 0) {
    # For TSV files, after processing, the quantity columns should have been renamed to condition names
    # If no experimental columns detected, it means all columns are metadata
    # In this case, we should look for columns that were renamed from quantity columns
    
    # Get metadata columns that are NOT typical protein annotation columns
    metadata_pattern <- "^(id|PG\\.ProteinGroups|PG\\.Genes|PG\\.FastaFiles|PG\\.ProteinNames|PG\\.ProteinDescriptions|PG\\.ProteinAccessions|PG\\.Organisms|PG\\.UniProtIds|PG\\.NrOfStrippedSequencesIdentified|PG\\.Meta|PG\\.BiologicalProcess|PG\\.MolecularFunction)"
    potential_experimental <- non_experimental_columns[!grepl(metadata_pattern, non_experimental_columns, ignore.case = TRUE)]
    
    if (length(potential_experimental) > 0) {
      experimental_columns <- potential_experimental
      message("TSV processing: Using potentially experimental columns: ", paste(experimental_columns, collapse = ", "))
    } else {
      warning(paste(
        "No experimental columns were detected in processed TSV data.",
        "This may indicate an issue with TSV processing or condition setup mapping.",
        "All columns:", paste(unique_columns, collapse = ", ")
      ))
      
      # Create a minimal template with available columns excluding known metadata
      experimental_columns <- head(unique_columns[!grepl("^(id|identifier)", unique_columns, ignore.case = TRUE)], 5)
    }
  }
  
  # Create experimental design template with all columns
  # Include both experimental and non-experimental columns so user can see and modify all
  all_template_columns <- c(non_experimental_columns, experimental_columns)
  template <- data.frame(
    columnName = all_template_columns,
    experiment = rep(NA, length(all_template_columns)),
    condition = rep(NA, length(all_template_columns)),
    treatment = rep(NA, length(all_template_columns)),
    timepoint = rep(NA, length(all_template_columns)),
    stringsAsFactors = FALSE
  )
  
  # Add attributes to provide feedback to user
  attr(template, "classification") <- classification
  attr(template, "excluded_columns") <- non_experimental_columns
  attr(template, "data_source") <- "processed_tsv"
  
  return(template)
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