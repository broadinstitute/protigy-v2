################################################################################
# Module: SETUP SIDEBAR
# This file contains helper functions for the setup sidebar module
################################################################################

# function to parse, normalize, filter, etc. GCT file(s)
# INPUT: parameters list from setup 
# OUTPUT: list of processed GCTs
processGCT <- function(parameters) {
  
  message("\nProcessing GCTs...")
  
  # parse GCTs
  GCTs <- lapply(parameters, function(p) parse_gctx(p$gct_file_path))
  
  # validate GCTs
  
  # handle intensity data?
  
  # max missing value filter
  
  # data filter
  
  # data normalization
  
  # log transformation
  
  # return processed GCT files
  return(GCTs)
}