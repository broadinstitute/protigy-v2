
# wrapper to make custom colors
make_custom_colors <- function(GCTs, GCTs_merged) {
  # initialize list
  custom_colors <- list()
  
  # start by making custom colors for the merged GCT
  custom_colors$multi_ome <- set_annot_colors(GCTs_merged@cdesc)
  
  # then, loop through each ome
  # pull colors from merged first, then make unique colors if you can't find them
  for (ome in names(GCTs)) {
    annot_columns_in_ome <- names(GCTs[[ome]]@cdesc)
    annot_columns_in_merged <- names(custom_colors$multi_ome)
    
    # get the colors for the columns that are in both 
    annot_columns_in_both <- intersect(annot_columns_in_ome, annot_columns_in_merged)
    common_colors <- custom_colors$multi_ome[annot_columns_in_both]
    
    # extract from merged the colors that are unique to the ome
    annot_columns_only_in_ome <- setdiff(annot_columns_in_ome, annot_columns_in_merged)
    unique_colors <- sapply(
      annot_columns_only_in_ome,
      simplify = FALSE,
      FUN = function(col) {
        # try to pull from merged
        merged_col_name_regexp <- paste0("^", gsub("\\.", "\\\\.", col), '\\.', ome, ".*")
        merged_col_matches <- grep(merged_col_name_regexp, names(GCTs_merged@cdesc), value = TRUE)
        for (merged_col_name in merged_col_matches) {
          col_values_in_ome <- GCTs[[ome]]@cdesc[[col]]
          col_values_in_merged <- GCTs_merged@cdesc[[merged_col_name]]
          
          # check if the values in both match, return if they do
          is_match <- length(setdiff(col_values_in_ome, col_values_in_merged)) == 0
          if (is_match) return(custom_colors$multi_ome[[merged_col_name]])
        }
        
        # if no match was found, make new colors
        # theoretically this shouldn't happen, but just in case
        warning(ome, ": column '", col, "' could not be found in the merged GCT. ",
                "Generating new colors.")
        return(set_annot_colors(GCTs[[ome]]@cdesc[, col, drop = FALSE])[[1]])
      }
    )
    
    custom_colors[[ome]] <- c(common_colors, unique_colors)
  }
  
  message("\nCustom colors generated!")
  
  return(custom_colors)
}

