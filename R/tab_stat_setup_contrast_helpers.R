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

  # Generate all pairwise combinations as a matrix (2 x n_pairs)
  pairs <- combn(groups, 2)

  if (bidirectional) {
    # Vectorized: forward labels then reverse labels
    labels <- c(
      paste(pairs[1, ], "/", pairs[2, ]),
      paste(pairs[2, ], "/", pairs[1, ])
    )
  } else {
    labels <- paste(pairs[1, ], "/", pairs[2, ])
  }
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
#' @details Creates contrasts between adjacent groups in the provided order.
#' For sequential groups like Time_1, Time_2, Time_3, generates:
#' Time_2/Time_1, Time_3/Time_2 (later/earlier). Useful for ordered experimental 
#' designs (e.g., time series, dose response) where you want to compare later 
#' timepoints/conditions to earlier ones.
generate_sequential_pairs <- function(groups, bidirectional = FALSE) {
  if (length(groups) < 2) return(character(0))

  n <- length(groups)
  # Vectorized: generate all sequential pairs at once
  contrasts <- paste(groups[2:n], "/", groups[1:(n - 1)])

  if (bidirectional) {
    reverse <- paste(groups[1:(n - 1)], "/", groups[2:n])
    contrasts <- c(contrasts, reverse)
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
  # Vectorized split: split all labels at once, then reshape
  parts <- strsplit(contrast_labels, " / ", fixed = TRUE)
  lapply(parts, function(p) {
    if (length(p) != 2) stop("Invalid contrast label format. Expected 'Group1 / Group2'")
    p
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
#' @details Creates an interactive matrix where columns are numerator groups and
#' rows are denominator groups. Users can click cells to toggle contrast selection.
render_contrast_matrix <- function(groups, selected_contrasts, ns) {
  n_groups <- length(groups)

  # Pre-compute the set of selected contrasts for O(1) lookup
  selected_set <- if (length(selected_contrasts) > 0) {
    new.env(hash = TRUE, parent = emptyenv(), size = length(selected_contrasts))
    # Use environment as hash set for fast membership testing
  } else {
    NULL
  }
  if (!is.null(selected_set)) {
    for (sc in selected_contrasts) selected_set[[sc]] <- TRUE
  }

  # Pre-compute the click handler namespace prefix once
  click_ns <- ns("contrast_matrix_click")

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
        title = col_group,
        col_group
      )
    })
  )

  # Create matrix body rows
  body_rows <- lapply(groups, function(row_group) {
    tags$tr(
      tags$td(
        class = "contrast-matrix-header row-header",
        title = row_group,
        row_group
      ),
      lapply(groups, function(col_group) {
        is_diagonal <- row_group == col_group

        if (is_diagonal) {
          tags$td(
            class = "contrast-matrix-cell disabled",
            `data-numerator` = col_group,
            `data-denominator` = row_group,
            ""
          )
        } else {
          contrast_label <- paste(col_group, "/", row_group)
          is_selected <- !is.null(selected_set) &&
            exists(contrast_label, envir = selected_set, inherits = FALSE)

          tags$td(
            class = if (is_selected) "contrast-matrix-cell selected" else "contrast-matrix-cell",
            `data-numerator` = col_group,
            `data-denominator` = row_group,
            `data-contrast` = contrast_label,
            onclick = sprintf(
              "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
              click_ns,
              gsub("'", "\\\\'", contrast_label)
            ),
            ""
          )
        }
      })
    )
  })

  div(
    class = "contrast-matrix-container",
    div(
      style = "text-align: center; margin-bottom: 10px; color: #666; font-size: 12px;",
      icon("info-circle"),
      " Click cells to select contrasts. Columns = Numerator, Rows = Denominator"
    ),
    tags$table(
      class = "contrast-matrix-table",
      tags$thead(header_row),
      tags$tbody(body_rows)
    )
  )
}


#' Generate contrasts for all groups vs multiple reference groups
#'
#' @param groups Character vector of group names
#' @param reference_groups Character vector of reference group names
#' @param bidirectional Logical, if TRUE includes both directions (default FALSE)
#' @return Character vector of contrasts in "Group / Reference" format
#'
#' @details Generates contrasts comparing each non-reference group to each
#' reference group. Useful when multiple control groups are specified.
generate_all_vs_multiple_references <- function(groups, reference_groups, bidirectional = FALSE) {
  if (length(groups) < 2 || length(reference_groups) == 0) return(character(0))

  # Ensure all reference groups are in groups
  reference_groups <- intersect(reference_groups, groups)
  if (length(reference_groups) == 0) return(character(0))

  # Get all groups except references
  other_groups <- setdiff(groups, reference_groups)
  if (length(other_groups) == 0) return(character(0))

  # Vectorized: expand.grid creates all combinations at once
  combos <- expand.grid(other = other_groups, ref = reference_groups,
                        stringsAsFactors = FALSE)
  contrasts <- paste(combos$other, "/", combos$ref)

  if (bidirectional) {
    reverse_contrasts <- paste(combos$ref, "/", combos$other)
    contrasts <- c(contrasts, reverse_contrasts)
  }

  return(contrasts)
}


#' Render group selection matrix UI for one-sample t-test and F-test
#'
#' @param groups Character vector of group names
#' @param selected_groups Character vector of currently selected groups
#' @param ns Namespace function from Shiny module
#' @return Shiny HTML tags for the group selection interface
#'
#' @details Creates a checkbox interface where users can select groups to include in the test.
render_group_selection_matrix <- function(groups, selected_groups, ns) {
  if (length(groups) == 0) {
    return(div(class = "group-selection-empty", "No groups available"))
  }

  # Create checkbox inputs for each group
  group_checkboxes <- lapply(groups, function(group) {
    is_selected <- group %in% selected_groups
    
    checkboxInput(
      inputId = ns(paste0("group_checkbox_", gsub("[^a-zA-Z0-9_]", "_", group))),
      label = group,
      value = is_selected
    )
  })

  div(
    class = "group-selection-container",
    div(
      style = "text-align: center; margin-bottom: 10px; color: #666; font-size: 12px;",
      icon("info-circle"),
      " Select groups to include in the test."
    ),
    div(
      class = "group-selection-buttons",
      style = "margin-bottom: 10px; display: flex; gap: 8px;",
      actionButton(
        ns("group_select_all"),
        "Select All",
        class = "btn-sm btn-default",
        icon = icon("check-square")
      ),
      actionButton(
        ns("group_clear_all"),
        "Clear All",
        class = "btn-sm btn-primary",
        icon = icon("square")
      )
    ),
    div(
      class = "group-selection-checkbox-list",
      group_checkboxes
    )
  )
}


#' Render control group selector UI
#'
#' @param groups Character vector of group names
#' @param selected_controls Character vector of currently selected control groups
#' @param ns Namespace function from Shiny module
#' @return Shiny HTML tags for the control group selector
#'
#' @details Creates a badge/button interface for selecting one or more control
#' groups. Used with "All vs Control" quick select button.
render_control_group_selector <- function(groups, selected_controls, ns) {
  if (length(groups) == 0) {
    return(div(class = "control-group-empty", "No groups available"))
  }

  # Create control group badge buttons
  control_badges <- lapply(groups, function(group) {
    is_selected <- group %in% selected_controls

    tags$span(
      class = paste0(
        "control-group-badge",
        if (is_selected) " selected" else ""
      ),
      `data-group` = group,
      onclick = sprintf(
        "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
        ns("control_group_click"),
        gsub("'", "\\\\'", group)  # Escape single quotes
      ),
      group
    )
  })

  div(
    class = "control-group-selector",
    div(
      style = "margin-bottom: 10px; color: #666; font-size: 12px;",
      icon("hand-pointer"),
      " Click to select control group(s):"
    ),
    div(
      class = "control-group-grid",
      control_badges
    )
  )
}
