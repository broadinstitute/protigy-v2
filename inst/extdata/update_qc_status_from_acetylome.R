#!/usr/bin/env Rscript

# Reproducible updater for QC.status harmonization across extdata test files.
# Rule: samples not present in acetylome are marked QC.fail in proteome,
# phosphoproteome, and experimental_design metadata.

read_gct_table <- function(path) {
  lines <- readLines(path, warn = FALSE)
  split_lines <- strsplit(lines, "\t", fixed = TRUE)
  list(lines = lines, split = split_lines)
}

find_row_index <- function(split_lines, row_name, path) {
  idx <- which(vapply(
    split_lines,
    function(x) length(x) > 0 && x[1] == row_name,
    logical(1)
  ))
  if (length(idx) != 1) {
    stop("Could not uniquely find row '", row_name, "' in ", path)
  }
  idx
}

get_samples_from_gct <- function(path) {
  gct <- read_gct_table(path)
  sample_row_idx <- find_row_index(gct$split, "Sample.ID", path)
  sample_row <- gct$split[[sample_row_idx]]
  sample_row[sample_row != "na" & sample_row != "Sample.ID"]
}

set_qc_from_reference <- function(path, reference_samples) {
  gct <- read_gct_table(path)
  sample_row_idx <- find_row_index(gct$split, "Sample.ID", path)
  qc_row_idx <- find_row_index(gct$split, "QC.status", path)

  sample_row <- gct$split[[sample_row_idx]]
  qc_row <- gct$split[[qc_row_idx]]

  sample_cols <- which(sample_row != "na" & sample_row != "Sample.ID")
  non_reference_samples <- setdiff(sample_row[sample_cols], reference_samples)
  target_cols <- which(sample_row %in% non_reference_samples)
  qc_row[target_cols] <- "QC.fail"

  gct$split[[qc_row_idx]] <- qc_row
  new_lines <- vapply(gct$split, function(x) paste(x, collapse = "\t"), character(1))
  writeLines(new_lines, path, useBytes = TRUE)

  sort(non_reference_samples)
}

update_experimental_design <- function(path, sample_ids_to_fail) {
  d <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  d$QC.status[d$Sample.ID %in% sample_ids_to_fail] <- "QC.fail"
  write.csv(d, path, row.names = FALSE, quote = FALSE)
}

main <- function() {
  root <- "."
  acetylome_path <- file.path(root, "mb-acetylome-ratio-norm-NArm.gct")
  proteome_path <- file.path(root, "mb-proteome-ratio-norm-NArm.gct")
  phospho_path <- file.path(root, "mb-phosphoproteome-ratio-norm-NArm.gct")
  design_path <- file.path(root, "experimental_design.csv")

  acetyl_samples <- get_samples_from_gct(acetylome_path)

  non_acetyl_proteome <- set_qc_from_reference(proteome_path, acetyl_samples)
  non_acetyl_phospho <- set_qc_from_reference(phospho_path, acetyl_samples)

  if (!identical(non_acetyl_proteome, non_acetyl_phospho)) {
    stop("Non-acetyl sample sets differ between proteome and phosphoproteome.")
  }

  update_experimental_design(design_path, non_acetyl_proteome)

  cat(
    "Updated non-acetyl samples (", length(non_acetyl_proteome), "): ",
    paste(non_acetyl_proteome, collapse = ", "),
    "\n",
    sep = ""
  )
}

main()
