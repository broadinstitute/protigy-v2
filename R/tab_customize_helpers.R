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

        # Create named list for each annotation column (preserves names in YAML)
        yaml_structure$colors[[ome]][[annot_col]] <- as.list(stats::setNames(colors, vals))
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


#' Import colors from YAML format with smart matching
#'
#' Implements a three-scenario matching algorithm:
#' 1. All conditions match: Apply colors based on condition-color name matching
#' 2. Some conditions match: Apply colors to matches, then unused colors
#'    sequentially (alphabetically) to unmatched conditions
#' 3. No conditions match: Apply colors by order sequentially to conditions
#'
#' @param file_path Path to the YAML file
#' @param custom_colors Current custom colors structure (to preserve structure)
#' @return Updated custom_colors list
#' @importFrom yaml read_yaml
import_colors_from_yaml <- function(file_path, custom_colors) {
  tryCatch({
    yaml_data <- yaml::read_yaml(file_path)

    # Check for both ProTIGY format (colors) and PANOPLY format (groups.colors)
    colors_data <- NULL
    if (!is.null(yaml_data$colors)) {
      colors_data <- yaml_data$colors
    } else if (!is.null(yaml_data$`groups.colors`)) {
      # PANOPLY format: groups.colors with flat structure (applies to all omes)
      colors_data <- list()
      # Apply to multi_ome and all individual omes
      for (ome in names(custom_colors)) {
        colors_data[[ome]] <- yaml_data$`groups.colors`
      }
    } else {
      warning("No 'colors' section found in YAML file")
      return(custom_colors)
    }

    if (is.null(colors_data)) {
      warning("No color data found in YAML file")
      return(custom_colors)
    }

    # Process each ome in current session
    for (ome in names(custom_colors)) {
      # Skip if ome not in YAML
      if (!(ome %in% names(colors_data))) {
        next
      }

      # Collect ALL colors globally across all columns in YAML for this ome
      # Build mapping: condition_name -> color
      yaml_color_map <- list()
      for (annot_col in names(colors_data[[ome]])) {
        yaml_vals <- names(colors_data[[ome]][[annot_col]])

        # Validate that the YAML structure has names (not an unnamed array)
        if (is.null(yaml_vals) || length(yaml_vals) == 0) {
          warning("YAML file has invalid structure for ", ome, "$", annot_col,
                  ": expected named color mapping (e.g., condition: color) but found unnamed array. ",
                  "This may be from an older export version. Please re-export the color palette.")
          next
        }

        yaml_colors <- unname(unlist(colors_data[[ome]][[annot_col]]))
        for (i in seq_along(yaml_vals)) {
          # Store first occurrence (don't override if duplicate names across columns)
          if (!(yaml_vals[i] %in% names(yaml_color_map))) {
            yaml_color_map[[yaml_vals[i]]] <- yaml_colors[i]
          }
        }
      }

      # Process each annotation column in current session
      for (annot_col in names(custom_colors[[ome]])) {
        # Only process discrete colors
        if (!custom_colors[[ome]][[annot_col]]$is_discrete) {
          next
        }

        current_vals <- custom_colors[[ome]][[annot_col]]$vals
        current_colors <- custom_colors[[ome]][[annot_col]]$colors
        new_colors <- current_colors  # Start with original colors

        # Check if this annotation column exists in YAML
        annot_col_in_yaml <- annot_col %in% names(colors_data[[ome]])

        # Track used colors and matched conditions
        used_colors <- character(0)
        matched_condition_indices <- integer(0)

        # Step 1: Match by condition name (globally across YAML columns)
        for (i in seq_along(current_vals)) {
          if (current_vals[i] %in% names(yaml_color_map)) {
            new_colors[i] <- yaml_color_map[[current_vals[i]]]
            used_colors <- c(used_colors, yaml_color_map[[current_vals[i]]])
            matched_condition_indices <- c(matched_condition_indices, i)
          }
        }

        # Step 2: Get unused colors from YAML
        # Only apply unused colors if:
        # 1. The column exists in YAML, OR
        # 2. The column doesn't exist in YAML but has at least one matched condition (global matching)
        all_yaml_colors <- unname(unlist(yaml_color_map))
        unused_colors <- setdiff(all_yaml_colors, used_colors)

        # Step 3: Get unmatched conditions (sorted alphabetically)
        unmatched_indices <- setdiff(seq_along(current_vals), matched_condition_indices)
        
        # Only apply unused colors if column exists in YAML OR if there are matched conditions
        should_apply_unused <- annot_col_in_yaml || length(matched_condition_indices) > 0
        
        if (length(unmatched_indices) > 0 && length(unused_colors) > 0 && should_apply_unused) {
          # Sort unmatched conditions alphabetically
          unmatched_vals <- current_vals[unmatched_indices]
          sorted_order <- order(unmatched_vals)
          sorted_unmatched_indices <- unmatched_indices[sorted_order]

          # Step 4: Apply unused colors sequentially
          num_to_assign <- min(length(sorted_unmatched_indices), length(unused_colors))
          for (i in 1:num_to_assign) {
            new_colors[sorted_unmatched_indices[i]] <- unused_colors[i]
          }
          # Remaining unmatched conditions keep their original colors
        }
        # If column doesn't exist in YAML and has no matches, unmatched conditions keep their original colors

        # Update colors
        custom_colors[[ome]][[annot_col]]$colors <- new_colors
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

