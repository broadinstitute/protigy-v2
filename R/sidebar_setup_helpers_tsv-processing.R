################################################################################
# Module: SETUP SIDEBAR
# Functions for processing TSV files, converting them to CSV format, and generating experimental design for user input 
################################################################################

# Process CSV/Excel files with experimental design to create GCT objects
# INPUT: raw TSV data and corresponding condition setup file from Spectronaut
# OUTPUT: list of GCT objects (same format as existing GCT workflow)

processCSVExcelFiles <- function(dataFiles, experimentalDesign, identifierColumn = NULL) {

}

## Determine the best identifier column for the data ####
determineIdentifierColumns <- function(data, identifierColumn = NULL) {
  
}

### Extract first protein group from the identifier column ####
extractFirstProteinGroup <- function(data) {
  
}

### Validate that identifier column have unique values ####

## Convert TSV data to CSV format ####

## Determine, rename, and filter abundance columns using condition setup file ####


# --- END: The processed data file will be used to generate experimental design template --- #