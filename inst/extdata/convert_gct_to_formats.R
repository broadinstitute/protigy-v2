#!/usr/bin/env Rscript

# Script to convert GCT files to TSV, CSV, and Excel formats for testing
# This creates test files that combine the data matrix with rdesc but exclude cdesc

library(cmapR)
library(readr)
library(writexl)

# Function to convert GCT to combined format (matrix + rdesc, no cdesc)
convert_gct_to_combined <- function(gct_file_path) {
  # Load the GCT file
  gct <- parse_gctx(gct_file_path)
  
  # Get the data matrix
  data_matrix <- gct@mat
  
  # Get row descriptions
  rdesc <- gct@rdesc
  
  # Combine data matrix with rdesc
  # First, add the rdesc columns to the data matrix
  combined_data <- cbind(rdesc, data_matrix)
  
  return(combined_data)
}

# Function to extract cdesc (column descriptions) from GCT files
extract_cdesc_from_gct <- function(gct_file_path) {
  # Load the GCT file
  gct <- parse_gctx(gct_file_path)
  
  # Get column descriptions
  cdesc <- gct@cdesc
  
  # Add a source file column to identify which file each sample came from
  cdesc$source_file <- basename(gct_file_path)
  
  return(cdesc)
}

# Function to create experimental design from combined cdesc
create_experimental_design <- function(all_cdesc, output_dir) {
  # Get all unique column names across all cdesc
  all_cols <- unique(unlist(lapply(all_cdesc, colnames)))
  
  # Find proteome file to use as default for shared columns
  proteome_file <- names(all_cdesc)[grepl("proteome", names(all_cdesc), ignore.case = TRUE)][1]
  if (is.na(proteome_file)) {
    proteome_file <- names(all_cdesc)[1]  # Use first file if no proteome found
  }
  
  # Standardize all cdesc to have the same columns, using proteome as default
  standardized_cdesc <- lapply(names(all_cdesc), function(file_name) {
    cdesc <- all_cdesc[[file_name]]
    
    # Add missing columns with values from proteome file (or NA if not available)
    missing_cols <- setdiff(all_cols, colnames(cdesc))
    for (col in missing_cols) {
      if (col %in% colnames(all_cdesc[[proteome_file]])) {
        # Use proteome values for missing columns, but pad with NA if different length
        proteome_values <- all_cdesc[[proteome_file]][[col]]
        if (length(proteome_values) == nrow(cdesc)) {
          cdesc[[col]] <- proteome_values
        } else {
          # Pad with NA if lengths don't match
          cdesc[[col]] <- rep(NA, nrow(cdesc))
        }
      } else {
        cdesc[[col]] <- NA
      }
    }
    
    # Reorder columns to match all_cols
    cdesc <- cdesc[, all_cols, drop = FALSE]
    return(cdesc)
  })
  
  # Combine all cdesc data
  combined_cdesc <- do.call(rbind, standardized_cdesc)
  
  # Create experimental design with actual cdesc data
  exp_design <- data.frame(
    columnName = rownames(combined_cdesc),
    stringsAsFactors = FALSE
  )
  
  # Add all the cdesc columns (except source_file) to the experimental design
  cdesc_cols <- setdiff(colnames(combined_cdesc), "source_file")
  for (col in cdesc_cols) {
    exp_design[[col]] <- combined_cdesc[[col]]
  }
  
  # Save experimental design
  exp_design_file <- file.path(output_dir, "experimental_design.csv")
  write_csv(exp_design, exp_design_file)
  cat("Created experimental design:", exp_design_file, "\n")
  
  return(exp_design_file)
}

# Function to save data in multiple formats
save_in_multiple_formats <- function(data, base_name, output_dir) {
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Save as TSV
  tsv_file <- file.path(output_dir, paste0(base_name, ".tsv"))
  write_tsv(data, tsv_file)
  cat("Created:", tsv_file, "\n")
  
  # Save as CSV
  csv_file <- file.path(output_dir, paste0(base_name, ".csv"))
  write_csv(data, csv_file)
  cat("Created:", csv_file, "\n")
  
  # Save as Excel
  excel_file <- file.path(output_dir, paste0(base_name, ".xlsx"))
  write_xlsx(data, excel_file)
  cat("Created:", excel_file, "\n")
  
  return(list(tsv = tsv_file, csv = csv_file, excel = excel_file))
}

# Main conversion process
main <- function() {
  # Define input and output directories
  input_dir <- "."  # Current directory (inst/extdata)
  output_dir <- "."  # Current directory (inst/extdata)
  
  # Get list of GCT files
  gct_files <- list.files(input_dir, pattern = "\\.gct$", full.names = TRUE)
  
  cat("Found", length(gct_files), "GCT files to convert:\n")
  for (file in gct_files) {
    cat("  -", basename(file), "\n")
  }
  cat("\n")
  
  # Collect cdesc from all files for experimental design
  all_cdesc <- list()
  
  # Convert each GCT file
  for (gct_file in gct_files) {
    cat("Processing:", basename(gct_file), "\n")
    
    tryCatch({
      # Convert GCT to combined format
      combined_data <- convert_gct_to_combined(gct_file)
      
      # Extract cdesc for experimental design
      cdesc <- extract_cdesc_from_gct(gct_file)
      all_cdesc[[basename(gct_file)]] <- cdesc
      
      # Get base name without extension
      base_name <- tools::file_path_sans_ext(basename(gct_file))
      
      # Save in multiple formats
      files_created <- save_in_multiple_formats(combined_data, base_name, output_dir)
      
      cat("Successfully converted", basename(gct_file), "\n")
      cat("  Rows:", nrow(combined_data), "\n")
      cat("  Columns:", ncol(combined_data), "\n")
      cat("  Files created:\n")
      cat("    -", basename(files_created$tsv), "\n")
      cat("    -", basename(files_created$csv), "\n")
      cat("    -", basename(files_created$excel), "\n")
      cat("\n")
      
    }, error = function(e) {
      cat("Error processing", basename(gct_file), ":", e$message, "\n")
    })
  }
  
  # Create experimental design from combined cdesc
  if (length(all_cdesc) > 0) {
    cat("Creating experimental design from column descriptions...\n")
    exp_design_file <- create_experimental_design(all_cdesc, output_dir)
    cat("Experimental design created:", basename(exp_design_file), "\n")
  }
  
  cat("Conversion complete!\n")
  cat("Test files created in:", output_dir, "\n")
}

# Run the conversion
main()
