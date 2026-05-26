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
    val_idx <- which(as.character(custom_colors$multi_ome[[annot_column]]$vals) ==
                       as.character(annot_value))
    if (length(val_idx) > 0) {
      custom_colors$multi_ome[[annot_column]]$colors[val_idx] <- new_color
    }
  }

  # Update all omes that have this annotation column and value
  for (ome in names(custom_colors)) {
    if (ome == "multi_ome") next

    if (annot_column %in% names(custom_colors[[ome]])) {
      val_idx <- which(as.character(custom_colors[[ome]][[annot_column]]$vals) ==
                         as.character(annot_value))
      if (length(val_idx) > 0) {
        custom_colors[[ome]][[annot_column]]$colors[val_idx] <- new_color
      }
    }
  }

  return(custom_colors)
}


#' Validate a hex color code
#'
#' Accepts 3-, 6-, and 8-digit (with alpha) hex forms. The 8-digit form is
#' considered valid here so importers can normalize it; storage canonical form
#' remains 6-digit (alpha stripped via `normalize_hex_color`).
#'
#' @param x Character scalar to test
#' @return TRUE if x is a valid hex color, FALSE otherwise
is_valid_hex_color <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) &&
    grepl("^#([0-9A-Fa-f]{3}|[0-9A-Fa-f]{6}|[0-9A-Fa-f]{8})$", x)
}


#' Normalize a hex color string to canonical 6-digit upper-case form
#'
#' Expands 3-digit forms (e.g. `#abc` -> `#AABBCC`) and strips alpha from
#' 8-digit forms (e.g. `#AABBCCDD` -> `#AABBCC`). Returns `NA_character_`
#' for invalid input. When alpha was stripped, the returned value carries
#' an `attr(., "had_alpha") <- TRUE` so callers can surface a warning once.
#'
#' @param x A character scalar.
#' @return Canonical 6-digit upper-case hex, or NA_character_ if invalid.
normalize_hex_color <- function(x) {
  if (!is_valid_hex_color(x)) return(NA_character_)
  body <- substring(x, 2)
  had_alpha <- FALSE
  if (nchar(body) == 3) {
    chars <- strsplit(body, "")[[1]]
    body <- paste0(chars[1], chars[1], chars[2], chars[2], chars[3], chars[3])
  } else if (nchar(body) == 8) {
    body <- substring(body, 1, 6)
    had_alpha <- TRUE
  }
  out <- toupper(paste0("#", body))
  if (had_alpha) attr(out, "had_alpha") <- TRUE
  out
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

  # Also emit a continuous_colors section so gradient palettes (low/mid/high,
  # na_color) round-trip intact. Prior behavior silently dropped them.
  yaml_structure$continuous_colors <- list()

  for (ome in names(custom_colors)) {
    ome_entry <- list()
    cont_entry <- list()

    for (annot_col in names(custom_colors[[ome]])) {
      col_info <- custom_colors[[ome]][[annot_col]]

      if (isTRUE(col_info$is_discrete)) {
        vals <- as.character(col_info$vals)
        colors <- as.character(col_info$colors)

        # Skip annotation columns with no values (defensive)
        if (length(vals) == 0) next

        # Create named list for each annotation column (preserves names in YAML).
        # Keys are forced to character to prevent YAML coercing condition names
        # like "yes"/"no"/"1" to logicals/numerics on round-trip.
        ome_entry[[annot_col]] <- as.list(stats::setNames(colors, vals))
      } else {
        # Continuous entries: only the discrete-palette form (vals=low/mid/high
        # plus optional na_color) is YAML-serializable. The function-form
        # (circlize::colorRamp2) used when continuous.return_function=TRUE is
        # an R closure and cannot round-trip -- skip it.
        vals <- col_info$vals
        colors <- col_info$colors
        if (is.null(vals) || is.function(colors) || length(vals) == 0) next
        cont_entry[[annot_col]] <- as.list(stats::setNames(
          as.character(colors), as.character(vals)))
      }
    }

    # Only include ome if it has at least one discrete annotation column --
    # avoids writing an empty mapping that round-trips as NULL and trips
    # the importer's names() iteration.
    if (length(ome_entry) > 0) {
      yaml_structure$colors[[ome]] <- ome_entry
    }
    if (length(cont_entry) > 0) {
      yaml_structure$continuous_colors[[ome]] <- cont_entry
    }
  }

  # Drop empty continuous_colors to keep the YAML tidy.
  if (length(yaml_structure$continuous_colors) == 0) {
    yaml_structure$continuous_colors <- NULL
  }

  # Write to YAML file -- let errors propagate so downloadHandler can surface them.
  yaml::write_yaml(yaml_structure, file_path)
  invisible(TRUE)
}


#' Compute a structural signature of a custom_colors list
#'
#' Returns a string that changes only when the set of omes, annotation
#' columns, or condition values changes -- but not when only color hex
#' values change. Used by the Customize module to detect new datasets
#' (which must refresh stale color state) while leaving user color edits
#' alone.
#'
#' Uses an ASCII unit-separator (\\u001f) and record-separator (\\u001e) as
#' delimiters to avoid accidental collisions with values containing `|`,
#' `;`, or `=` characters.
#'
#' @param colors A custom_colors list (as produced by make_custom_colors)
#' @return A character scalar signature; empty string for NULL/empty input.
colors_structure_signature <- function(colors) {
  if (is.null(colors) || length(colors) == 0) return("")
  US <- ""  # unit separator (within an entry)
  RS <- ""  # record separator (between entries)
  sig <- lapply(colors, function(ome_cols) {
    if (!is.list(ome_cols)) return(character(0))
    vapply(names(ome_cols), function(col) {
      vals <- ome_cols[[col]]$vals
      paste(col, paste(as.character(vals), collapse = US), sep = ":")
    }, character(1))
  })
  paste(names(sig),
        vapply(sig, paste, character(1), collapse = RS),
        sep = "=", collapse = paste0(RS, RS))
}


#' Import colors from YAML -- full structured result
#'
#' Internal workhorse. Returns a structured list describing the import so the
#' Shiny module can surface counts, missing omes, format detected, and
#' normalization warnings to the user. The thin back-compat wrapper
#' `import_colors_from_yaml()` calls `warning()` for each entry in `$warnings`
#' and returns just the colors list.
#'
#' Implements a three-scenario matching algorithm:
#' 1. All conditions match: Apply colors based on condition-color name matching
#' 2. Some conditions match: Apply colors to matches, then unused colors
#'    sequentially (alphabetically) to unmatched conditions
#' 3. No conditions match: Apply colors by order sequentially to conditions
#'
#' Accepts two YAML shapes:
#'   - ProTIGY: `colors: { ome: { annot_col: { val: "#hex" } } }`
#'   - PANOPLY: `groups.colors: { annot_col: { val: "#hex" } }` (nested) or
#'     `groups.colors: { val: "#hex" }` (flat). Both are applied to every ome
#'     in the current session.
#'
#' Errors (file not readable, malformed YAML) are raised via `stop()`.
#'
#' @param file_path Path to the YAML file
#' @param custom_colors Current custom colors structure (to preserve structure)
#' @return A list with:
#'   * `colors`               - updated custom_colors
#'   * `n_columns_updated`    - integer count of column entries actually changed
#'   * `n_omes_in_yaml`       - integer count of omes that matched session
#'   * `invalid_entries`      - character vector of "ome$col$val=#bad"
#'   * `missing_omes`         - omes present in YAML but not in session
#'   * `format`               - "ProTIGY" | "PANOPLY-nested" | "PANOPLY-flat" | "none"
#'   * `warnings`             - character vector of all warning messages
#'   * `skipped_continuous_function_palettes` - ome$col entries that couldn't be restored
#'   * `alpha_stripped_count` - integer; how many hex inputs had alpha stripped
#' @importFrom yaml read_yaml
import_colors_from_yaml_full <- function(file_path, custom_colors) {
  yaml_data <- yaml::read_yaml(file_path)

  warnings <- character(0)
  invalid_entries <- character(0)
  missing_omes <- character(0)
  skipped_continuous_function_palettes <- character(0)
  alpha_stripped_count <- 0L
  n_columns_updated <- 0L
  detected_format <- "none"

  # Detect format and normalize colors_data to ome-keyed shape.
  colors_data <- NULL
  if (!is.null(yaml_data$colors)) {
    colors_data <- yaml_data$colors
    detected_format <- "ProTIGY"
  } else if (!is.null(yaml_data$`groups.colors`)) {
    gc <- yaml_data$`groups.colors`

    # Detect nested vs flat shape. Nested:  {annot_col: {val: "#hex"}}
    # Flat: {val: "#hex"}.
    is_nested <- length(gc) > 0 && all(vapply(gc, is.list, logical(1)))
    detected_format <- if (is_nested) "PANOPLY-nested" else "PANOPLY-flat"

    colors_data <- list()
    for (ome in names(custom_colors)) {
      colors_data[[ome]] <- if (is_nested) gc else list(`__flat__` = gc)
    }
  } else {
    msg <- "No 'colors' section found in YAML file"
    warnings <- c(warnings, msg)
    return(list(
      colors = custom_colors,
      n_columns_updated = 0L,
      n_omes_in_yaml = 0L,
      invalid_entries = character(0),
      missing_omes = character(0),
      format = "none",
      warnings = warnings,
      skipped_continuous_function_palettes = character(0),
      alpha_stripped_count = 0L
    ))
  }

  if (is.null(colors_data)) {
    msg <- "No color data found in YAML file"
    warnings <- c(warnings, msg)
    return(list(
      colors = custom_colors,
      n_columns_updated = 0L,
      n_omes_in_yaml = 0L,
      invalid_entries = character(0),
      missing_omes = character(0),
      format = detected_format,
      warnings = warnings,
      skipped_continuous_function_palettes = character(0),
      alpha_stripped_count = 0L
    ))
  }

  # Track which omes in the YAML didn't match the session (informational).
  missing_omes <- setdiff(names(colors_data), names(custom_colors))

  # Process each ome in current session
  for (ome in names(custom_colors)) {
    if (!(ome %in% names(colors_data))) next

    # Build column-scoped + global lookups for this ome.
    yaml_col_map <- list()
    yaml_flat_map <- list()
    for (annot_col in names(colors_data[[ome]])) {
      yaml_vals <- as.character(names(colors_data[[ome]][[annot_col]]))

      if (length(yaml_vals) == 0 || all(yaml_vals == "")) {
        msg <- sprintf(
          "YAML file has invalid structure for %s$%s: expected named color mapping (e.g., condition: color) but found unnamed array. This may be from an older export version. Please re-export the color palette.",
          ome, annot_col
        )
        warnings <- c(warnings, msg)
        next
      }

      yaml_colors_raw <- as.character(unname(unlist(colors_data[[ome]][[annot_col]])))
      col_entry <- list()
      for (i in seq_along(yaml_vals)) {
        key_i <- yaml_vals[i]
        raw <- yaml_colors_raw[i]
        norm <- normalize_hex_color(raw)

        if (is.na(norm)) {
          invalid_entries <- c(invalid_entries,
                               paste0(ome, "$", annot_col, "$", key_i, " = ", raw))
          next
        }
        if (isTRUE(attr(norm, "had_alpha"))) {
          alpha_stripped_count <- alpha_stripped_count + 1L
          attr(norm, "had_alpha") <- NULL
        }

        col_entry[[key_i]] <- norm
        if (!(key_i %in% names(yaml_flat_map))) {
          yaml_flat_map[[key_i]] <- norm
        }
      }
      yaml_col_map[[annot_col]] <- col_entry
    }

    # Process each annotation column in current session
    for (annot_col in names(custom_colors[[ome]])) {
      if (!isTRUE(custom_colors[[ome]][[annot_col]]$is_discrete)) next

      current_vals <- as.character(custom_colors[[ome]][[annot_col]]$vals)
      current_colors <- custom_colors[[ome]][[annot_col]]$colors
      new_colors <- current_colors

      annot_col_in_yaml <- annot_col %in% names(yaml_col_map)
      col_map <- if (annot_col_in_yaml) yaml_col_map[[annot_col]] else list()

      used_colors <- character(0)
      matched_condition_indices <- integer(0)

      # Step 1: Match by condition name (column-scoped first, then global).
      for (i in seq_along(current_vals)) {
        v <- current_vals[i]
        chosen <- NULL
        if (v %in% names(col_map)) {
          chosen <- col_map[[v]]
        } else if (v %in% names(yaml_flat_map)) {
          chosen <- yaml_flat_map[[v]]
        }
        if (!is.null(chosen)) {
          new_colors[i] <- chosen
          used_colors <- c(used_colors, chosen)
          matched_condition_indices <- c(matched_condition_indices, i)
        }
      }

      # Step 2: Pull unused colors ONLY from this column's YAML entries.
      unused_colors <- if (annot_col_in_yaml) {
        setdiff(unname(unlist(col_map)), used_colors)
      } else {
        character(0)
      }

      # Step 3: Get unmatched conditions (sorted alphabetically)
      unmatched_indices <- setdiff(seq_along(current_vals), matched_condition_indices)

      if (length(unmatched_indices) > 0 && length(unused_colors) > 0) {
        unmatched_vals <- current_vals[unmatched_indices]
        sorted_order <- order(unmatched_vals)
        sorted_unmatched_indices <- unmatched_indices[sorted_order]

        num_to_assign <- min(length(sorted_unmatched_indices), length(unused_colors))
        for (i in seq_len(num_to_assign)) {
          new_colors[sorted_unmatched_indices[i]] <- unused_colors[i]
        }
      }

      # Count how many columns *actually* changed
      if (!identical(as.character(new_colors), as.character(current_colors))) {
        n_columns_updated <- n_columns_updated + 1L
      }

      custom_colors[[ome]][[annot_col]]$colors <- new_colors
    }
  }

  # Restore continuous palettes if a continuous_colors section is present.
  cont_yaml <- yaml_data$continuous_colors
  if (!is.null(cont_yaml) && is.list(cont_yaml)) {
    for (ome in names(custom_colors)) {
      if (!(ome %in% names(cont_yaml)) || !is.list(cont_yaml[[ome]])) next
      for (annot_col in names(custom_colors[[ome]])) {
        col_info <- custom_colors[[ome]][[annot_col]]
        if (isTRUE(col_info$is_discrete)) next
        if (is.function(col_info$colors)) {
          skipped_continuous_function_palettes <- c(
            skipped_continuous_function_palettes,
            paste0(ome, "$", annot_col)
          )
          next
        }
        if (!(annot_col %in% names(cont_yaml[[ome]]))) next

        yaml_entry <- cont_yaml[[ome]][[annot_col]]
        yaml_names <- as.character(names(yaml_entry))
        yaml_vals_raw <- as.character(unname(unlist(yaml_entry)))
        if (length(yaml_names) == 0) next

        normed <- vapply(yaml_vals_raw, normalize_hex_color, character(1))
        keep <- !is.na(normed)
        if (!all(keep)) {
          for (k in which(!keep)) {
            invalid_entries <- c(invalid_entries,
                                 paste0(ome, "$", annot_col, "$", yaml_names[k],
                                        " (continuous) = ", yaml_vals_raw[k]))
          }
          yaml_names <- yaml_names[keep]
          normed <- normed[keep]
          if (length(yaml_names) == 0) next
        }
        # Strip had_alpha attr if any (per element vapply already coerces).
        cur_names <- as.character(col_info$vals)
        new_colors <- as.character(col_info$colors)
        changed <- FALSE
        for (j in seq_along(cur_names)) {
          hit <- which(yaml_names == cur_names[j])
          if (length(hit) == 1) {
            if (!identical(new_colors[j], unname(normed[hit]))) changed <- TRUE
            new_colors[j] <- unname(normed[hit])
          }
        }
        if (changed) n_columns_updated <- n_columns_updated + 1L
        custom_colors[[ome]][[annot_col]]$colors <- new_colors
      }
    }
  }

  if (length(invalid_entries) > 0) {
    msg <- paste0(
      "Skipped ", length(invalid_entries), " invalid hex color entr",
      if (length(invalid_entries) == 1) "y" else "ies", " in YAML: ",
      paste(utils::head(invalid_entries, 5), collapse = "; "),
      if (length(invalid_entries) > 5) ", ..." else ""
    )
    warnings <- c(warnings, msg)
  }
  if (alpha_stripped_count > 0L) {
    warnings <- c(warnings, sprintf(
      "%d hex entr%s carried an alpha channel and were normalized to 6-digit form.",
      alpha_stripped_count,
      if (alpha_stripped_count == 1) "y" else "ies"
    ))
  }

  list(
    colors = custom_colors,
    n_columns_updated = n_columns_updated,
    n_omes_in_yaml = length(intersect(names(colors_data), names(custom_colors))),
    invalid_entries = invalid_entries,
    missing_omes = missing_omes,
    format = detected_format,
    warnings = warnings,
    skipped_continuous_function_palettes = skipped_continuous_function_palettes,
    alpha_stripped_count = alpha_stripped_count
  )
}


#' Import colors from YAML format with smart matching (back-compat wrapper)
#'
#' Thin wrapper over `import_colors_from_yaml_full()`. Emits each warning via
#' `warning()` and returns just the updated colors list. Existing call sites
#' and tests that expect this signature continue to work unchanged. New code
#' (the Shiny module) should use `import_colors_from_yaml_full()` to surface
#' structured counts to the user.
#'
#' @param file_path Path to the YAML file
#' @param custom_colors Current custom colors structure
#' @return Updated custom_colors list
#' @importFrom yaml read_yaml
import_colors_from_yaml <- function(file_path, custom_colors) {
  res <- import_colors_from_yaml_full(file_path, custom_colors)
  for (w in res$warnings) warning(w, call. = FALSE)
  if (res$n_columns_updated > 0 || length(res$warnings) == 0) {
    message("Colors imported successfully from YAML")
  }
  res$colors
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
    unique_colors <- lapply(annot_columns_only_in_ome, function(col) {
      # try to pull from merged -- escape all regex metachars in the column name
      # (not just `.`) so names like `group+plus` or `treatment(type)` don't
      # produce invalid or over-broad patterns.
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
    })
    names(unique_colors) <- annot_columns_only_in_ome

    custom_colors[[ome]] <- c(common_colors, unique_colors)
  }

  message("\nCustom colors generated!")

  return(custom_colors)
}


#' Get a preset color palette by name
#'
#' Returns N colors from a named preset. Supported names:
#'   * "Paul Tol Bright"
#'   * "Paul Tol Vibrant"
#'   * "Paul Tol Muted"
#'   * "Paul Tol Light"
#'   * "ColorBrewer Set2"
#'   * "ColorBrewer Paired"
#'   * "Viridis"
#'
#' Palettes are interpolated when N exceeds the palette's native size.
#'
#' @param name Palette name (one of the above)
#' @param n Number of colors needed
#' @param reverse If TRUE, return colors in reverse order
#' @return Character vector of N hex colors (canonical 6-digit form)
get_preset_palette <- function(name, n, reverse = FALSE) {
  if (!is.numeric(n) || length(n) != 1 || n < 1) {
    stop("`n` must be a positive integer.", call. = FALSE)
  }
  n <- as.integer(n)

  pal <- switch(name,
    "Paul Tol Bright"   = .preset_tol_palette("bright", n),
    "Paul Tol Vibrant"  = .preset_tol_palette("vibrant", n),
    "Paul Tol Muted"    = .preset_tol_palette("muted", n),
    "Paul Tol Light"    = .preset_tol_palette("light", n),
    "ColorBrewer Set2"  = .preset_brewer_palette("Set2", n),
    "ColorBrewer Paired" = .preset_brewer_palette("Paired", n),
    "Viridis"           = .preset_viridis_palette(n),
    stop(sprintf("Unknown preset palette: '%s'", name), call. = FALSE)
  )

  pal <- vapply(pal, normalize_hex_color, character(1))
  if (isTRUE(reverse)) pal <- rev(pal)
  unname(pal)
}


# Internal: max qualitative colors per Paul Tol scheme (via khroma).
.preset_tol_palette_max <- function(which) {
  switch(which,
    bright = 7L,
    vibrant = 7L,
    muted = 9L,
    light = 9L,
    stop("Unknown Tol palette: ", which, call. = FALSE)
  )
}

# Internal: Tol qualitative palette via khroma, falling back to hardcoded
# values if khroma errors. Palettes interpolate when n > max.
.preset_tol_palette <- function(which, n) {
  max_qual <- .preset_tol_palette_max(which)
  fallback <- list(
    bright  = c('#4477AA', '#EE6677', '#228833', '#CCBB44',
                '#66CCEE', '#AA3377', '#BBBBBB'),
    vibrant = c('#0077BB', '#33BBEE', '#009988', '#EE7733',
                '#CC3311', '#EE3377', '#BBBBBB'),
    muted   = c('#CC6677', '#332288', '#DDCC77', '#117733', '#88CCEE',
                '#882255', '#44AA99', '#999933', '#AA4499'),
    light   = c('#77AADD', '#EE8866', '#EEDD88', '#FFAABB', '#99DDFF',
                '#44BB99', '#BBCC33', '#AAAA00', '#DDDDDD')
  )
  base <- tryCatch(
    as.vector(khroma::color(which)(max_qual)),
    error = function(e) fallback[[which]]
  )
  if (length(base) == 0) base <- fallback[[which]]
  if (n <= length(base)) return(base[seq_len(n)])
  grDevices::colorRampPalette(base)(n)
}

# Internal: ColorBrewer palette via RColorBrewer (already in Imports).
.preset_brewer_palette <- function(name, n) {
  max_n <- RColorBrewer::brewer.pal.info[name, "maxcolors"]
  if (is.na(max_n)) max_n <- 8L
  base <- RColorBrewer::brewer.pal(min(max(n, 3L), max_n), name)
  if (n <= length(base)) return(base[seq_len(n)])
  grDevices::colorRampPalette(base)(n)
}

# Internal: Viridis palette. viridisLite is a transitive dependency of
# many imports; if unavailable, fall back to a colorRampPalette with the
# canonical viridis endpoints.
.preset_viridis_palette <- function(n) {
  if (requireNamespace("viridisLite", quietly = TRUE)) {
    return(substring(viridisLite::viridis(n), 1, 7))
  }
  grDevices::colorRampPalette(c("#440154", "#3B528B", "#21908C",
                                "#5DC863", "#FDE725"))(n)
}
