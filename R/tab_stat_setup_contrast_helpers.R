################################################################################
# Helper functions for contrast selection in Statistical Testing
################################################################################

#' Detect likely control group from list of group names
#'
#' @param groups Character vector of group names
#' @return String name of likely control group, or first group alphabetically
#'
#' @details Looks for common control group keywords: "control", "ctrl", "wt",
#' "wildtype", "untreated", "baseline", "reference", "vehicle", "dmso", "pbs"
#' (case-insensitive). If none found, returns first group alphabetically.
detect_control_group <- function(groups) {
  if (length(groups) == 0) return(NULL)

  # Common control group keywords (case-insensitive)
  control_keywords <- c(
    "control", "ctrl", "wt", "wildtype", "wild.type", "wild_type",
    "untreated", "baseline", "reference", "vehicle", "dmso", "pbs",
    "mock", "normal", "healthy", "unlabeled", "without"
  )

  # Convert to lowercase for matching
  groups_lower <- tolower(groups)

  # Check each keyword
  for (keyword in control_keywords) {
    # Exact match
    exact_match <- which(groups_lower == keyword)
    if (length(exact_match) > 0) {
      return(groups[exact_match[1]])
    }

    # Contains match
    contains_match <- which(grepl(keyword, groups_lower, fixed = TRUE))
    if (length(contains_match) > 0) {
      return(groups[contains_match[1]])
    }
  }

  # If no control found, return first alphabetically
  return(groups[order(groups)][1])
}


#' Generate all pairwise contrasts from groups
#'
#' @param groups Character vector of group names
#' @param bidirectional Logical, if TRUE includes both A/B and B/A (default TRUE)
#' @return Character vector of contrasts in "Group1 / Group2" format
generate_all_pairwise <- function(groups, bidirectional = TRUE) {
  if (length(groups) < 2) return(character(0))

  # Generate all pairwise combinations
  pairwise_contrasts <- combn(groups, 2, simplify = FALSE)

  if (bidirectional) {
    # Add reverse pairs
    all_pairs <- c(pairwise_contrasts, lapply(pairwise_contrasts, rev))
  } else {
    all_pairs <- pairwise_contrasts
  }

  # Format as "Group1 / Group2"
  labels <- sapply(all_pairs, function(p) paste(p[1], "/", p[2]))
  return(labels)
}


#' Generate contrasts for all groups vs a reference group
#'
#' @param groups Character vector of group names
#' @param reference String name of reference group
#' @param bidirectional Logical, if TRUE includes both directions (default FALSE)
#' @return Character vector of contrasts in "Group / Reference" format
generate_all_vs_reference <- function(groups, reference, bidirectional = FALSE) {
  if (length(groups) < 2 || !(reference %in% groups)) return(character(0))

  # Get all groups except reference
  other_groups <- setdiff(groups, reference)

  # Generate "Other / Reference" contrasts
  contrasts <- sapply(other_groups, function(g) paste(g, "/", reference))

  if (bidirectional) {
    # Add reverse: "Reference / Other"
    reverse_contrasts <- sapply(other_groups, function(g) paste(reference, "/", g))
    contrasts <- c(contrasts, reverse_contrasts)
  }

  return(contrasts)
}


#' Generate sequential pairwise contrasts
#'
#' @param groups Character vector of group names (order matters)
#' @param bidirectional Logical, if TRUE includes both directions (default FALSE)
#' @return Character vector of sequential contrasts
#'
#' @details Creates contrasts between adjacent groups in the provided order:
#' A/B, B/C, C/D, etc. Useful for ordered experimental designs (e.g., time series,
#' dose response)
generate_sequential_pairs <- function(groups, bidirectional = FALSE) {
  if (length(groups) < 2) return(character(0))

  contrasts <- character(0)
  for (i in 1:(length(groups) - 1)) {
    contrasts <- c(contrasts, paste(groups[i], "/", groups[i + 1]))
    if (bidirectional) {
      contrasts <- c(contrasts, paste(groups[i + 1], "/", groups[i]))
    }
  }

  return(contrasts)
}


#' Parse contrast label to get numerator and denominator groups
#'
#' @param contrast_label String in "Group1 / Group2" format
#' @return List with elements 'numerator' and 'denominator'
parse_contrast_label <- function(contrast_label) {
  parts <- strsplit(contrast_label, " / ", fixed = TRUE)[[1]]
  if (length(parts) != 2) {
    stop("Invalid contrast label format. Expected 'Group1 / Group2'")
  }
  return(list(numerator = parts[1], denominator = parts[2]))
}


#' Convert contrast labels to list format expected by stat.testing
#'
#' @param contrast_labels Character vector of contrasts in "Group1 / Group2" format
#' @return List of character vectors, each with c(numerator, denominator)
contrast_labels_to_list <- function(contrast_labels) {
  lapply(contrast_labels, function(label) {
    parts <- parse_contrast_label(label)
    return(c(parts$numerator, parts$denominator))
  })
}


#' Check if a contrast is valid for given groups
#'
#' @param contrast_label String in "Group1 / Group2" format
#' @param groups Character vector of available group names
#' @return Logical, TRUE if both groups in contrast exist in groups vector
is_valid_contrast <- function(contrast_label, groups) {
  tryCatch({
    parts <- parse_contrast_label(contrast_label)
    return(parts$numerator %in% groups && parts$denominator %in% groups)
  }, error = function(e) {
    return(FALSE)
  })
}


#' Render contrast selection matrix UI
#'
#' @param groups Character vector of group names
#' @param selected_contrasts Character vector of currently selected contrasts
#' @param ns Namespace function from Shiny module
#' @return Shiny HTML tags for the matrix
#'
#' @details Creates an interactive matrix where rows are numerator groups and
#' columns are denominator groups. Users can click cells to toggle contrast selection.
render_contrast_matrix <- function(groups, selected_contrasts, ns) {
  n_groups <- length(groups)

  # Create table header row
  header_row <- tags$tr(
    tags$td(
      class = "contrast-matrix-corner",
      tags$span(class = "corner-numerator", "Numerator"),
      tags$span(class = "corner-denominator", "Denominator")
    ),
    lapply(groups, function(col_group) {
      tags$td(
        class = "contrast-matrix-header col-header",
        title = col_group,  # Tooltip for long names
        col_group
      )
    })
  )

  # Create matrix body rows
  body_rows <- lapply(groups, function(row_group) {
    tags$tr(
      # Row header
      tags$td(
        class = "contrast-matrix-header row-header",
        title = row_group,  # Tooltip for long names
        row_group
      ),
      # Matrix cells
      lapply(groups, function(col_group) {
        contrast_label <- paste(row_group, "/", col_group)
        is_diagonal <- row_group == col_group
        is_selected <- contrast_label %in% selected_contrasts

        cell_class <- paste0(
          "contrast-matrix-cell",
          if (is_diagonal) " disabled" else "",
          if (is_selected && !is_diagonal) " selected" else ""
        )

        # Build onclick handler (use HTML() to prevent escaping)
        onclick_handler <- if (!is_diagonal) {
          sprintf(
            "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
            ns("contrast_matrix_click"),
            gsub("'", "\\\\'", contrast_label)  # Escape single quotes
          )
        } else {
          NULL
        }

        tags$td(
          class = cell_class,
          `data-numerator` = row_group,
          `data-denominator` = col_group,
          `data-contrast` = contrast_label,
          onclick = onclick_handler,
          "" # Empty content - CSS will add checkmark/dash
        )
      })
    )
  })

  # Assemble complete matrix
  div(
    class = "contrast-matrix-container",
    div(
      style = "text-align: center; margin-bottom: 10px; color: #666; font-size: 12px;",
      icon("info-circle"),
      " Click cells to select contrasts. Rows = Numerator, Columns = Denominator"
    ),
    tags$table(
      class = "contrast-matrix-table",
      tags$thead(header_row),
      tags$tbody(body_rows)
    )
  )
}
