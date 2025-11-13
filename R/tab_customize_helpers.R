################################################################################
# Helper functions for color customization
################################################################################

#' Sync colors across omes for the same condition value
#' @param custom_colors The custom colors list
#' @param annot_column The annotation column name
#' @param annot_value The specific value within that column
#' @param new_color The new color hex code to apply
#' @return Updated custom_colors list with synced colors
sync_colors_across_omes <- function(custom_colors, annot_column, annot_value, new_color) {
  # Update multi_ome first
  if (annot_column %in% names(custom_colors$multi_ome)) {
    val_idx <- which(custom_colors$multi_ome[[annot_column]]$vals == annot_value)
    if (length(val_idx) > 0) {
      custom_colors$multi_ome[[annot_column]]$colors[val_idx] <- new_color
    }
  }

  # Update all omes that have this annotation column and value
  for (ome in names(custom_colors)) {
    if (ome == "multi_ome") next

    if (annot_column %in% names(custom_colors[[ome]])) {
      val_idx <- which(custom_colors[[ome]][[annot_column]]$vals == annot_value)
      if (length(val_idx) > 0) {
        custom_colors[[ome]][[annot_column]]$colors[val_idx] <- new_color
      }
    }
  }

  return(custom_colors)
}


#' Reset colors to default for a specific annotation column or all columns
#' @param custom_colors Current custom colors
#' @param default_colors The default color scheme
#' @param ome The ome name (or "multi_ome"), or NULL for all
#' @param annot_column The annotation column name, or NULL for all columns
#' @return Updated custom_colors with reset colors
reset_colors_to_default <- function(custom_colors, default_colors, ome = NULL, annot_column = NULL) {

  if (is.null(ome)) {
    # Reset all omes
    return(default_colors)
  } else if (is.null(annot_column)) {
    # Reset all columns in a specific ome
    custom_colors[[ome]] <- default_colors[[ome]]
  } else {
    # Reset a specific column in a specific ome
    if (annot_column %in% names(default_colors[[ome]])) {
      custom_colors[[ome]][[annot_column]] <- default_colors[[ome]][[annot_column]]
    }
  }

  return(custom_colors)
}


#' Export colors to YAML format
#' @param custom_colors The custom colors list
#' @param file_path Path to save the YAML file
#' @return TRUE if successful
#' @importFrom yaml write_yaml
export_colors_to_yaml <- function(custom_colors, file_path) {
  # Create a simplified structure for YAML export
  yaml_structure <- list(
    metadata = list(
      created_date = as.character(Sys.Date()),
      protigy_version = utils::packageVersion("Protigy")
    ),
    colors = list()
  )

  # Convert custom_colors to simple key-value pairs
  for (ome in names(custom_colors)) {
    yaml_structure$colors[[ome]] <- list()

    for (annot_col in names(custom_colors[[ome]])) {
      # Only export discrete colors (skip continuous for now)
      if (custom_colors[[ome]][[annot_col]]$is_discrete) {
        vals <- custom_colors[[ome]][[annot_col]]$vals
        colors <- custom_colors[[ome]][[annot_col]]$colors

        # Create named vector for each annotation column
        yaml_structure$colors[[ome]][[annot_col]] <- stats::setNames(colors, vals)
      }
    }
  }

  # Write to YAML file
  tryCatch({
    yaml::write_yaml(yaml_structure, file_path)
    return(TRUE)
  }, error = function(e) {
    warning("Failed to export colors to YAML: ", e$message)
    return(FALSE)
  })
}


#' Import colors from YAML format
#' @param file_path Path to the YAML file
#' @param custom_colors Current custom colors structure (to preserve structure)
#' @return Updated custom_colors list
#' @importFrom yaml read_yaml
import_colors_from_yaml <- function(file_path, custom_colors) {
  tryCatch({
    yaml_data <- yaml::read_yaml(file_path)

    if (is.null(yaml_data$colors)) {
      warning("No 'colors' section found in YAML file")
      return(custom_colors)
    }

    # Update colors from YAML
    for (ome in names(yaml_data$colors)) {
      if (!(ome %in% names(custom_colors))) {
        warning("Ome '", ome, "' from YAML not found in current data")
        next
      }

      for (annot_col in names(yaml_data$colors[[ome]])) {
        if (!(annot_col %in% names(custom_colors[[ome]]))) {
          warning("Annotation column '", annot_col, "' from YAML not found in ome '", ome, "'")
          next
        }

        # Match values and update colors
        yaml_vals <- names(yaml_data$colors[[ome]][[annot_col]])
        yaml_colors <- unname(unlist(yaml_data$colors[[ome]][[annot_col]]))
        current_vals <- custom_colors[[ome]][[annot_col]]$vals

        for (i in seq_along(yaml_vals)) {
          val_idx <- which(current_vals == yaml_vals[i])
          if (length(val_idx) > 0) {
            custom_colors[[ome]][[annot_col]]$colors[val_idx] <- yaml_colors[i]
          }
        }
      }
    }

    message("Colors imported successfully from YAML")
    return(custom_colors)

  }, error = function(e) {
    warning("Failed to import colors from YAML: ", e$message)
    return(custom_colors)
  })
}


# wrapper to make custom colors
make_custom_colors <- function(GCTs, GCTs_merged) {
  # initialize list
  custom_colors <- list()
  
  # start by making custom colors for the merged GCT
  custom_colors$multi_ome <- set_annot_colors(GCTs_merged@cdesc, autodetect_continuous_nfactor_cutoff = 20)
  
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
        return(set_annot_colors(GCTs[[ome]]@cdesc[, col, drop = FALSE], autodetect_continuous_nfactor_cutoff = 20)[[1]])
      }
    )
    
    custom_colors[[ome]] <- c(common_colors, unique_colors)
  }
  
  message("\nCustom colors generated!")
  
  return(custom_colors)
}

