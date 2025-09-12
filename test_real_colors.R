#!/usr/bin/env Rscript

# Test script to reproduce multi-ome heatmap error using REAL color structure
# Using files from /Users/nclark/Desktop/CPTAC-ICPC-tumors

library(cmapR)
library(ComplexHeatmap)
library(circlize)

# Load the GCT files
cat("Loading GCT files...\n")
rna_file <- "/Users/nclark/Desktop/CPTAC-ICPC-tumors/rna-data.gct"
proteome_file <- "/Users/nclark/Desktop/CPTAC-ICPC-tumors/proteome-ratio-norm-filt.gct"
phospho_file <- "/Users/nclark/Desktop/CPTAC-ICPC-tumors/phosphoproteome-ratio-norm-filt.gct"

# Parse GCT files
rna_gct <- parse_gctx(rna_file)
proteome_gct <- parse_gctx(proteome_file)
phospho_gct <- parse_gctx(phospho_file)

# Simulate the merged GCT structure that would be created during setup
cat("Creating merged GCT structure...\n")

# Find EGFR in each dataset
rna_egfr_idx <- which(rna_gct@rdesc$geneSymbol == "EGFR")
prot_egfr_idx <- which(proteome_gct@rdesc$geneSymbol == "EGFR")
phos_egfr_idx <- which(phospho_gct@rdesc$geneSymbol == "EGFR")

# Get EGFR data and metadata
rna_egfr_data <- rna_gct@mat[rna_egfr_idx, , drop = FALSE]
prot_egfr_data <- proteome_gct@mat[prot_egfr_idx, , drop = FALSE]
phos_egfr_data <- phospho_gct@mat[phos_egfr_idx, , drop = FALSE]

rna_egfr_rdesc <- rna_gct@rdesc[rna_egfr_idx, , drop = FALSE]
prot_egfr_rdesc <- proteome_gct@rdesc[prot_egfr_idx, , drop = FALSE]
phos_egfr_rdesc <- phospho_gct@rdesc[phos_egfr_idx, , drop = FALSE]

# Find common samples and columns
common_samples <- Reduce(intersect, list(
  colnames(rna_egfr_data),
  colnames(prot_egfr_data), 
  colnames(phos_egfr_data)
))

common_rdesc_cols <- Reduce(intersect, list(
  colnames(rna_egfr_rdesc),
  colnames(prot_egfr_rdesc),
  colnames(phos_egfr_rdesc)
))

# Subset to common samples and columns
rna_egfr_data_common <- rna_egfr_data[, common_samples, drop = FALSE]
prot_egfr_data_common <- prot_egfr_data[, common_samples, drop = FALSE]
phos_egfr_data_common <- phos_egfr_data[, common_samples, drop = FALSE]

rna_egfr_rdesc_common <- rna_egfr_rdesc[, common_rdesc_cols, drop = FALSE]
prot_egfr_rdesc_common <- prot_egfr_rdesc[, common_rdesc_cols, drop = FALSE]
phos_egfr_rdesc_common <- phos_egfr_rdesc[, common_rdesc_cols, drop = FALSE]

# Add DataType column to distinguish datasets
rna_egfr_rdesc_common$DataType <- "RNA"
prot_egfr_rdesc_common$DataType <- "Proteome"
phos_egfr_rdesc_common$DataType <- "Phosphoproteome"

# Create merged structure (simulating what setup would create)
merged_mat <- rbind(rna_egfr_data_common, prot_egfr_data_common, phos_egfr_data_common)
merged_rdesc <- rbind(rna_egfr_rdesc_common, prot_egfr_rdesc_common, phos_egfr_rdesc_common)

# Create sample annotation from RNA cdesc (they should all have the same samples)
sample_anno <- rna_gct@cdesc[rna_gct@cdesc$Sample.ID %in% common_samples, ]

cat("Merged matrix dimensions:", dim(merged_mat), "\n")
cat("Merged rdesc dimensions:", dim(merged_rdesc), "\n")
cat("Sample annotation dimensions:", dim(sample_anno), "\n")

# Now let's call the REAL color customization function
cat("Calling real set_annot_colors function...\n")

# Source the color customization function
source("/Users/nclark/Desktop/proteomics-protigy-revamp/R/tab_customize_helpers_color-mod.r")

# Call set_annot_colors with the actual sample annotation data
real_colors <- set_annot_colors(sample_anno)

cat("Real colors structure:\n")
str(real_colors)

# Check specific annotations
cat("NMF.prot.consensus colors:\n")
if ("NMF.prot.consensus" %in% names(real_colors)) {
  print(real_colors$NMF.prot.consensus)
  cat("Class:", class(real_colors$NMF.prot.consensus), "\n")
  cat("Length:", length(real_colors$NMF.prot.consensus), "\n")
} else {
  cat("NMF.prot.consensus not found in colors\n")
}

cat("Type colors:\n")
if ("Type" %in% names(real_colors)) {
  print(real_colors$Type)
  cat("Class:", class(real_colors$Type), "\n")
  cat("Length:", length(real_colors$Type), "\n")
} else {
  cat("Type not found in colors\n")
}

# Now test the color conversion logic with REAL colors
cat("Testing color conversion with REAL colors...\n")

# Simulate the color structure that would be in globals$colors$multi_ome
# The real colors are just named vectors, but the multi-ome structure wraps them
mock_multi_ome_colors <- list(
  "NMF.prot.consensus" = list(
    is_discrete = TRUE,
    vals = names(real_colors$NMF.prot.consensus),
    colors = as.character(real_colors$NMF.prot.consensus)
  ),
  "Type" = list(
    is_discrete = TRUE,
    vals = names(real_colors$Type),
    colors = as.character(real_colors$Type)
  )
)

cat("Mock multi-ome colors structure:\n")
str(mock_multi_ome_colors)

# Test the color conversion function with REAL data
test_color_conversion_real <- function(custom_colors, sample_anno) {
  mapply(function(color_obj, annot_name) {
    cat("Processing annotation:", annot_name, "\n")
    cat("Color object structure:\n")
    str(color_obj)
    
    if (color_obj$is_discrete) {
      # Convert discrete colors to named vector
      names(color_obj$colors) <- color_obj$vals
      cat("Discrete result:\n")
      print(color_obj$colors)
      return(color_obj$colors)
    } else {
      # Convert continuous colors to function
      annot_values <- suppressWarnings(as.numeric(sample_anno[[annot_name]]))
      
      # If conversion failed (all NAs), treat as discrete
      if (all(is.na(annot_values))) {
        unique_vals <- unique(sample_anno[[annot_name]][!is.na(sample_anno[[annot_name]])])
        colors_vector <- rep(color_obj$colors[1], length(unique_vals))
        names(colors_vector) <- unique_vals
        return(colors_vector)
      }
      
      min_val <- min(annot_values, na.rm = TRUE)
      max_val <- max(annot_values, na.rm = TRUE)
      
      # Handle case where all values are identical
      if (min_val == max_val) {
        range_val <- abs(min_val) * 0.1 + 0.1
        breaks <- c(min_val - range_val, min_val, min_val + range_val)
      } else {
        breaks <- c(min_val, mean(annot_values, na.rm = TRUE), max_val)
      }
      
      # FIXED: Ensure we have exactly 3 colors for the breaks
      if (length(color_obj$colors) >= 3) {
        colors_for_ramp <- color_obj$colors[1:3]
      } else {
        # If we have fewer than 3 colors, repeat the last color
        colors_for_ramp <- c(color_obj$colors, rep(color_obj$colors[length(color_obj$colors)], 3 - length(color_obj$colors)))
      }
      
      cat("Colors for ramp:", paste(colors_for_ramp, collapse=", "), "\n")
      
      color_function <- circlize::colorRamp2(
        breaks,
        colors_for_ramp
      )
      return(color_function)
    }
  }, custom_colors, names(custom_colors), SIMPLIFY = FALSE)
}

# Test with real colors
cat("Testing with NMF.prot.consensus...\n")
test_result <- test_color_conversion_real(mock_multi_ome_colors["NMF.prot.consensus"], sample_anno)

cat("Test completed successfully!\n")
