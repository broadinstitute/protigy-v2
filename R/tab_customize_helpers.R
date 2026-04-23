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
#' @return TRUE on success. Errors propagate (no silent failure) so that
#'   `downloadHandler` surfaces them to the user.
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
    ome_entry <- list()

    for (annot_col in names(custom_colors[[ome]])) {
      # Only export discrete colors (skip continuous for now)
      if (isTRUE(custom_colors[[ome]][[annot_col]]$is_discrete)) {
        vals <- as.character(custom_colors[[ome]][[annot_col]]$vals)
        colors <- as.character(custom_colors[[ome]][[annot_col]]$colors)

        # Skip annotation columns with no values (defensive)
        if (length(vals) == 0) next

        # Create named list for each annotation column (preserves names in YAML).
        # Keys are forced to character to prevent YAML coercing condition names
        # like "yes"/"no"/"1" to logicals/numerics on round-trip.
        ome_entry[[annot_col]] <- as.list(stats::setNames(colors, vals))
      }
    }

    # Only include ome if it has at least one discrete annotation column —
    # avoids writing an empty mapping that round-trips as NULL and trips
    # the importer's names() iteration.
    if (length(ome_entry) > 0) {
      yaml_structure$colors[[ome]] <- ome_entry
    }
  }

  # Write to YAML file — let errors propagate so downloadHandler can surface them.
  yaml::write_yaml(yaml_structure, file_path)
  invisible(TRUE)
}


#' Validate a hex color code
#' @param x Character scalar to test
#' @return TRUE if x is a valid 6-digit hex color (e.g. "#A1B2C3"), FALSE otherwise
is_valid_hex_color <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) &&
    grepl("^#[0-9A-Fa-f]{6}$", x)
}


#' Import colors from YAML format with smart matching
#'
#' Implements a three-scenario matching algorithm:
#' 1. All conditions match: Apply colors based on condition-color name matching
#' 2. Some conditions match: Apply colors to matches, then unused colors
#'    sequentially (alphabetically) to unmatched conditions
#' 3. No conditions match: Apply colors by order sequentially to conditions
#'
#' Accepts two YAML shapes:
#'   - ProTIGY: `colors: { ome: { annot_col: { val: "#hex" } } }`
#'   - PANOPLY: `groups.colors: { annot_col: { val: "#hex" } }` (nested — most
#'     common) or `groups.colors: { val: "#hex" }` (flat). Both are applied to
#'     every ome in the current session.
#'
#' Errors (file not readable, malformed YAML, no recognized section) are
#' raised via `stop()` so the caller can surface them to the user.
#'
#' @param file_path Path to the YAML file
#' @param custom_colors Current custom colors structure (to preserve structure)
#' @return Updated custom_colors list
#' @importFrom yaml read_yaml
import_colors_from_yaml <- function(file_path, custom_colors) {
  yaml_data <- yaml::read_yaml(file_path)

  # Check for both ProTIGY format (colors) and PANOPLY format (groups.colors)
  colors_data <- NULL
  if (!is.null(yaml_data$colors)) {
    colors_data <- yaml_data$colors
  } else if (!is.null(yaml_data$`groups.colors`)) {
    gc <- yaml_data$`groups.colors`

    # Detect nested vs. flat shape. Nested:  {annot_col: {val: "#hex"}}
    # Flat: {val: "#hex"}. In nested shape every top-level value is a list;
    # in flat shape every top-level value is a scalar string.
    is_nested <- length(gc) > 0 && all(vapply(gc, is.list, logical(1)))

    colors_data <- list()
    for (ome in names(custom_colors)) {
      if (is_nested) {
        colors_data[[ome]] <- gc
      } else {
        # Flat: wrap under a synthetic annot column so the loop below finds
        # named entries; global cross-column matching will still pick them up.
        colors_data[[ome]] <- list(`__flat__` = gc)
      }
    }
  } else {
    warning("No 'colors' section found in YAML file")
    return(custom_colors)
  }

  if (is.null(colors_data)) {
    warning("No color data found in YAML file")
    return(custom_colors)
  }

  invalid_entries <- character(0)

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
      # Force keys to character — YAML may have parsed "yes"/"1"/"null" as
      # logical/numeric/NULL, which would fail equality checks against
      # current_vals (always strings after processGCTs conversion).
      yaml_vals <- as.character(names(colors_data[[ome]][[annot_col]]))

      # Validate that the YAML structure has names (not an unnamed array)
      if (length(yaml_vals) == 0 || all(yaml_vals == "")) {
        warning("YAML file has invalid structure for ", ome, "$", annot_col,
                ": expected named color mapping (e.g., condition: color) but found unnamed array. ",
                "This may be from an older export version. Please re-export the color palette.")
        next
      }

      yaml_colors <- as.character(unname(unlist(colors_data[[ome]][[annot_col]])))
      for (i in seq_along(yaml_vals)) {
        key_i <- yaml_vals[i]
        color_i <- yaml_colors[i]

        # Validate hex. Invalid colors are recorded and skipped so a single
        # typo doesn't abort the whole import.
        if (!is_valid_hex_color(color_i)) {
          invalid_entries <- c(invalid_entries,
                               paste0(ome, "$", annot_col, "$", key_i, " = ", color_i))
          next
        }

        # Store first occurrence (don't override if duplicate names across columns)
        if (!(key_i %in% names(yaml_color_map))) {
          yaml_color_map[[key_i]] <- color_i
        }
      }
    }

    # Process each annotation column in current session
    for (annot_col in names(custom_colors[[ome]])) {
      # Only process discrete colors
      if (!isTRUE(custom_colors[[ome]][[annot_col]]$is_discrete)) {
        next
      }

      current_vals <- as.character(custom_colors[[ome]][[annot_col]]$vals)
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

  if (length(invalid_entries) > 0) {
    warning("Skipped ", length(invalid_entries), " invalid hex color entr",
            if (length(invalid_entries) == 1) "y" else "ies", " in YAML: ",
            paste(utils::head(invalid_entries, 5), collapse = "; "),
            if (length(invalid_entries) > 5) ", ..." else "")
  }

  message("Colors imported successfully from YAML")
  return(custom_colors)
}


# Helper function to convert numeric columns that are discrete to strings
# This ensures discrete columns are treated as categorical, not continuous
# Use cutoff 20 to match processGCTs logic
convert_discrete_numeric_to_string <- function(cdesc) {
  for (col_name in names(cdesc)) {
    if (is.numeric(cdesc[[col_name]])) {
      # Check if this column should be treated as discrete
      # Use cutoff 20 to match processGCTs logic
      if (is.discrete(cdesc[[col_name]], nfactor_cutoff = 20)) {
        cdesc[[col_name]] <- as.character(cdesc[[col_name]])
      }
    }
  }
  return(cdesc)
}

# wrapper to make custom colors
make_custom_colors <- function(GCTs, GCTs_merged) {
  # Convert numeric columns that are discrete to strings in all GCTs
  # This must happen before colors are set up
  GCTs_merged@cdesc <- convert_discrete_numeric_to_string(GCTs_merged@cdesc)
  for (ome in names(GCTs)) {
    GCTs[[ome]]@cdesc <- convert_discrete_numeric_to_string(GCTs[[ome]]@cdesc)
  }
  
  # initialize list
  custom_colors <- list()
  
  # start by making custom colors for the merged GCT
  # Use cutoff 20 to match processGCTs logic
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
        # try to pull from merged — escape all regex metachars in the column
        # name (not just `.`) so names like `group+plus` or `treatment(type)`
        # don't produce invalid or over-broad patterns.
        col_escaped <- gsub("([][{}()+*?.^$|\\\\])", "\\\\\\1", col, perl = TRUE)
        merged_col_name_regexp <- paste0("^", col_escaped, '\\.', ome, ".*")
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

