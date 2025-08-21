################################################################################
# Module: SETUP SIDEBAR
# Functions for processing TSV files, converting them to CSV format, and generating experimental design for user input 
################################################################################

# Process TSV files with condition setup and experimental design to create GCT objects ####
# INPUT: raw TSV data and corresponding condition setup file from Spectronaut
# OUTPUT: list of GCT objects (same format as existing GCT workflow)

processTSVFiles <- function(dataFiles, experimentalDesign, identifierColumn = NULL) {
  
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
  
  # Check for PG.ProteinGroups column and process it
  if ("PG.ProteinGroups" %in% colnames(data)) {
    processed_data <- extractFirstProteinGroup(data)
    return(list(
      identifier_column = "firstProteinGroup", 
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
  data$firstProteinGroup <- first_groups
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

## Convert TSV data to CSV format ####
convertToCSV <- function(data) {
  
}

## Determine, rename, and filter abundance columns using condition setup file ####


# --- END: The processed data file will be used to generate experimental design template --- #