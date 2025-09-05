################################################################################
# Module: SETUP SIDEBAR
# Functions for mapping protein groups to gene symbols using reference files
################################################################################

# Read mapping reference file (CSV/Excel only)
# INPUT: file path to mapping file
# OUTPUT: data.frame with mapping data
readMappingFile <- function(file_path) {
  file_ext <- tools::file_ext(tolower(file_path))
  
  tryCatch({
    if (file_ext == "csv") {
      mapping_data <- utils::read.csv(file_path, stringsAsFactors = FALSE)
    } else if (file_ext %in% c("xlsx", "xls")) {
      mapping_data <- readxl::read_excel(file_path)
    } else {
      stop("Unsupported file format: ", file_ext, ". Only CSV and Excel files are supported.")
    }
    
    # Convert to data.frame and ensure column names are valid
    mapping_data <- as.data.frame(mapping_data)
    
    if (nrow(mapping_data) == 0) {
      stop("Mapping file is empty")
    }
    
    return(mapping_data)
    
  }, error = function(e) {
    stop("Failed to read mapping file: ", e$message)
  })
}

# Validate mapping file structure
# INPUT: data.frame from mapping file, protein group column name, gene symbol column name
# OUTPUT: logical indicating if validation passed
validateMappingFile <- function(mapping_data, protein_group_col, gene_symbol_col) {
  # Check if required columns exist
  if (!protein_group_col %in% colnames(mapping_data)) {
    stop("Protein group column '", protein_group_col, "' not found in mapping file")
  }
  
  if (!gene_symbol_col %in% colnames(mapping_data)) {
    stop("Gene symbol column '", gene_symbol_col, "' not found in mapping file")
  }
  
  # Check for empty values
  protein_groups <- mapping_data[[protein_group_col]]
  gene_symbols <- mapping_data[[gene_symbol_col]]
  
  if (all(is.na(protein_groups) | protein_groups == "")) {
    stop("Protein group column contains no valid values")
  }
  
  if (all(is.na(gene_symbols) | gene_symbols == "")) {
    stop("Gene symbol column contains no valid values")
  }
  
  return(TRUE)
}

# Extract first protein group from PG.ProteinGroups column if it exists
# INPUT: data.frame with protein group identifiers
# OUTPUT: character vector of first protein groups
extractFirstProteinGroup <- function(protein_groups) {
  first_groups <- sapply(protein_groups, function(x) {
    if (is.na(x) || x == "") {
      return(x)
    }
    # Split on semicolon and take first element, trim whitespace
    first_group <- trimws(strsplit(as.character(x), ";")[[1]][1])
    return(first_group)
  }, USE.NAMES = FALSE)
  
  return(first_groups)
}

# Perform gene mapping between working data and mapping reference
# INPUT: 
#   - working_data: data.frame with protein group column
#   - mapping_data: data.frame with protein group to gene symbol mapping
#   - working_protein_col: column name in working_data containing protein groups
#   - mapping_protein_col: column name in mapping_data containing protein groups
#   - mapping_gene_col: column name in mapping_data containing gene symbols
# OUTPUT: list with mapped_data and mapping_stats
performGeneMapping <- function(working_data, mapping_data, working_protein_col, 
                              mapping_protein_col, mapping_gene_col) {
  
  # Validate inputs
  validateMappingFile(mapping_data, mapping_protein_col, mapping_gene_col)
  
  if (!working_protein_col %in% colnames(working_data)) {
    stop("Working protein column '", working_protein_col, "' not found in working data")
  }
  
  # Extract first protein groups from working data if needed
  working_protein_groups <- working_data[[working_protein_col]]
  
  # If the working protein column looks like it contains multiple protein groups
  # (contains semicolons), extract the first one
  # Handle NA values properly in the check
  semicolon_check <- grepl(";", working_protein_groups, fixed = TRUE)
  # Replace NA values in the check with FALSE
  semicolon_check[is.na(semicolon_check)] <- FALSE
  
  if (any(semicolon_check)) {
    working_protein_groups <- extractFirstProteinGroup(working_protein_groups)
  }
  
  # Prepare mapping reference data
  mapping_ref <- mapping_data[, c(mapping_protein_col, mapping_gene_col), drop = FALSE]
  colnames(mapping_ref) <- c("protein_group", "gene_symbol")
  
  # Remove rows with missing values
  mapping_ref <- mapping_ref[!is.na(mapping_ref$protein_group) & 
                            !is.na(mapping_ref$gene_symbol) & 
                            mapping_ref$protein_group != "" & 
                            mapping_ref$gene_symbol != "", ]
  
  # Remove duplicates (keep first occurrence)
  mapping_ref <- mapping_ref[!duplicated(mapping_ref$protein_group), ]
  
  # Create temporary data frame for mapping
  temp_data <- data.frame(
    row_index = seq_len(nrow(working_data)),
    protein_group = working_protein_groups,
    stringsAsFactors = FALSE
  )
  
  # Perform the mapping using merge
  mapped_temp <- merge(temp_data, mapping_ref, by = "protein_group", all.x = TRUE)
  
  # Reorder to match original order
  mapped_temp <- mapped_temp[order(mapped_temp$row_index), ]
  
  # Add gene symbol column to working data
  working_data$gene_symbol <- mapped_temp$gene_symbol
  
  # Calculate mapping statistics
  total_protein_groups <- nrow(working_data)
  mapped_count <- sum(!is.na(mapped_temp$gene_symbol))
  unmapped_count <- total_protein_groups - mapped_count
  
  mapping_stats <- list(
    total = total_protein_groups,
    mapped = mapped_count,
    unmapped = unmapped_count,
    mapping_rate = round(mapped_count / total_protein_groups * 100, 1)
  )
  
  return(list(
    mapped_data = working_data,
    mapping_stats = mapping_stats
  ))
}

# Update GCT object with gene symbols in rdesc
# INPUT: GCT object, gene symbol column data
# OUTPUT: updated GCT object
updateGCTWithGeneSymbols <- function(gct_object, gene_symbols) {
  # Add gene_symbol column to rdesc
  gct_object@rdesc$gene_symbol <- gene_symbols
  
  return(gct_object)
}

# Main function to add gene mapping to GCT workflow
# INPUT: 
#   - gct_object: GCT object to update
#   - mapping_file_path: path to mapping reference file
#   - mapping_protein_col: protein group column in mapping file
#   - mapping_gene_col: gene symbol column in mapping file
#   - working_protein_col: protein group column in GCT rdesc (default: "firstProteinGroup")
# OUTPUT: list with updated GCT object and mapping statistics
addGeneMappingToGCT <- function(gct_object, mapping_file_path, mapping_protein_col, 
                               mapping_gene_col, working_protein_col = "firstProteinGroup") {
  
  # Read mapping file
  mapping_data <- readMappingFile(mapping_file_path)
  
  # Get working data from GCT rdesc
  working_data <- gct_object@rdesc
  
  # Perform gene mapping
  mapping_result <- performGeneMapping(
    working_data = working_data,
    mapping_data = mapping_data,
    working_protein_col = working_protein_col,
    mapping_protein_col = mapping_protein_col,
    mapping_gene_col = mapping_gene_col
  )
  
  # Update GCT object with gene symbols
  updated_gct <- updateGCTWithGeneSymbols(gct_object, mapping_result$mapped_data$gene_symbol)
  
  return(list(
    gct_object = updated_gct,
    mapping_stats = mapping_result$mapping_stats
  ))
}