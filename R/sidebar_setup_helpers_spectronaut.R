################################################################################
# Module: SETUP SIDEBAR
# Helper functions for Spectronaut pivot report preprocessing
################################################################################

#' Read a Spectronaut condition setup file (TSV or CSV)
#'
#' Validates required columns: "Run Label", "Condition", "Replicate"
#'
#' @param file_path path to .tsv or .csv file
#' @return data.frame
read_spectronaut_condition_setup <- function(file_path) {
  ext <- tolower(tools::file_ext(file_path))
  data <- if (ext == "tsv") {
    readr::read_tsv(file_path, show_col_types = FALSE)
  } else {
    readr::read_csv(file_path, show_col_types = FALSE)
  }
  data <- as.data.frame(data)

  required <- c("Run Label", "Condition", "Replicate")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Condition setup file is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  na_cols <- c("Run Label", "Condition")
  has_na <- sapply(na_cols, function(col) any(is.na(data[[col]])))
  if (any(has_na)) {
    stop("NA values found in required columns: ",
         paste(na_cols[has_na], collapse = ", "))
  }

  data
}

#' Detect quantification suffixes from Spectronaut pivot report columns
#'
#' For each run_label, finds data_columns containing it as a substring,
#' extracts the suffix (substring after the run_label portion).
#' Returns sorted unique non-empty suffixes.
#'
#' @param data_columns character vector of column names from the data file
#' @param run_labels character vector of run labels from condition setup
#' @return character vector of unique suffixes (e.g. c(".PG.IBAQ", ".PG.Quantity"))
detect_quant_suffixes <- function(data_columns, run_labels) {
  suffixes <- character(0)
  for (rl in run_labels) {
    # A valid match requires the run label to appear at the very start of the column
    # name, or after the "[N] " index prefix that Spectronaut sometimes prepends.
    # This prevents "Sample1" from spuriously matching inside "Sample10.PG.Quantity".
    for (col in data_columns) {
      stripped <- col
      # Strip leading "[N] " index prefix (e.g., "[1] Sample1.PG.Quantity")
      if (grepl("^\\[\\d+\\] ", col)) {
        stripped <- sub("^\\[\\d+\\] ", "", col)
      }
      if (startsWith(stripped, rl)) {
        suffix <- substring(stripped, nchar(rl) + 1L)
        # Only accept suffix if it begins with a non-alphanumeric delimiter
        # (e.g. "." in ".PG.Quantity"). This prevents "Sample1" from spuriously
        # matching "Sample10.PG.Quantity" and extracting "0.PG.Quantity".
        if (nchar(suffix) > 0 && grepl("^[^a-zA-Z0-9]", suffix)) {
          suffixes <- c(suffixes, suffix)
        }
      }
    }
  }
  sort(unique(suffixes))
}

#' Apply Spectronaut condition setup to rename/filter data columns
#'
#' For each row in condition_setup, finds the column paste0(run_label, selected_suffix)
#' and renames it. Drops all columns matching any run_label + non-selected suffix.
#'
#' @param data data.frame (the pivot report)
#' @param condition_setup data.frame with columns "Run Label", "Condition", "Replicate"
#' @param selected_suffix character suffix to keep (e.g. ".PG.Quantity")
#' @param merge_condition_replicate logical; if \code{TRUE}, new column name =
#'   \code{paste0(Condition, "_R", Replicate)}. If \code{FALSE} (default), new
#'   column name = \code{Condition} (note: may produce duplicate column names when
#'   multiple runs share the same Condition value).
#' @return data.frame with renamed columns
apply_spectronaut_condition_setup <- function(data, condition_setup, selected_suffix,
                                              merge_condition_replicate = FALSE) {
  run_labels <- condition_setup[["Run Label"]]
  conditions <- condition_setup[["Condition"]]
  replicates <- condition_setup[["Replicate"]]

  # Detect all suffixes present
  all_suffixes <- detect_quant_suffixes(names(data), run_labels)

  # Build a helper: given a run_label + suffix pattern, find matching column names
  # (handles optional "[N] " index prefix that Spectronaut adds)
  find_col <- function(run_label, suffix) {
    pattern <- paste0(run_label, suffix)
    # exact match first
    if (pattern %in% names(data)) return(pattern)
    # fallback: column that ends with run_label+suffix (covers "[N] " prefix)
    matches <- names(data)[endsWith(names(data), pattern)]
    if (length(matches) == 1L) return(matches[1L])
    if (length(matches) > 1L) {
      warning(
        "find_col: multiple columns match pattern '", pattern, "': ",
        paste(matches, collapse = ", "),
        ". Using the first match."
      )
      return(matches[1L])
    }
    character(0)
  }

  # Build rename map for selected suffix
  exp_design <- buildExpDesignFromConditionSetup(condition_setup, merge_condition_replicate)
  col_names_vec <- exp_design$columnName
  rename_map <- list()
  for (i in seq_along(run_labels)) {
    old_name <- find_col(run_labels[i], selected_suffix)
    if (length(old_name) == 1L && nchar(old_name) > 0) {
      rename_map[[old_name]] <- col_names_vec[i]
    }
  }

  # Identify columns to drop (run_label + non-selected suffix)
  cols_to_drop <- character(0)
  for (rl in run_labels) {
    for (sfx in all_suffixes) {
      if (sfx != selected_suffix) {
        col <- find_col(rl, sfx)
        if (length(col) == 1L && nchar(col) > 0) {
          cols_to_drop <- c(cols_to_drop, col)
        }
      }
    }
  }

  # Drop unwanted columns
  data <- data[, setdiff(names(data), cols_to_drop), drop = FALSE]

  # Warn if rename map targets are not unique (would produce duplicate column names)
  target_names <- as.character(rename_map)
  if (anyDuplicated(target_names) > 0) {
    dup_targets <- unique(target_names[duplicated(target_names)])
    warning(
      "apply_spectronaut_condition_setup: rename map contains duplicate target column name(s): ",
      paste(dup_targets, collapse = ", "),
      ". Multiple source columns will be renamed to the same name, producing a broken data.frame. ",
      "Check your condition setup file for duplicate Condition values."
    )
  }

  # Rename selected columns
  for (old_name in names(rename_map)) {
    idx <- which(names(data) == old_name)
    if (length(idx) > 0) {
      names(data)[idx] <- rename_map[[old_name]]
    }
  }

  data
}

# Construct a named warning condition for NA Replicate situations.
# Using a custom class allows callers to intercept this specific warning
# without fragile message-text matching.
.replicateNAWarning <- function(message) {
  structure(
    class = c("replicateNAWarning", "warning", "condition"),
    list(message = message)
  )
}

#' Build experimental design data.frame from condition setup
#'
#' Produces a data.frame with columnName, Condition, Replicate (and Fraction if present)
#' matching the column names produced by apply_spectronaut_condition_setup().
#'
#' @param condition_data data.frame from read_spectronaut_condition_setup()
#' @param merge_condition_replicate logical
#' @return data.frame
buildExpDesignFromConditionSetup <- function(condition_data, merge_condition_replicate = FALSE) {
  run_labels <- condition_data[["Run Label"]]
  conditions <- condition_data[["Condition"]]
  replicates <- condition_data[["Replicate"]]

  if (any(is.na(conditions))) {
    stop("Condition column has NA values. All rows must have a Condition.")
  }

  column_names <- if (isTRUE(merge_condition_replicate)) {
    if (any(is.na(replicates))) {
      if (length(unique(conditions)) == length(conditions)) {
        warning(.replicateNAWarning(
          "Replicate is NA for one or more rows. Conditions are unique; using Condition only as column names."
        ))
        conditions
      } else {
        warning(.replicateNAWarning(
          "Replicate is NA for one or more rows. Conditions are not unique; keeping original Run Label as column names."
        ))
        run_labels
      }
    } else {
      paste0(conditions, "_R", replicates)
    }
  } else {
    conditions
  }

  if (anyDuplicated(column_names) > 0) {
    dup_vals <- unique(column_names[duplicated(column_names)])
    warning(
      "buildExpDesignFromConditionSetup: duplicate columnName value(s) produced: ",
      paste(dup_vals, collapse = ", "),
      ". This typically occurs when merge_condition_replicate = FALSE and multiple ",
      "runs share the same Condition. Consider setting merge_condition_replicate = TRUE ",
      "to generate unique per-replicate column names."
    )
  }

  result <- data.frame(
    columnName = column_names,
    Condition  = conditions,
    Replicate  = replicates,
    stringsAsFactors = FALSE
  )

  if ("Fraction" %in% names(condition_data)) {
    result$Fraction <- condition_data[["Fraction"]]
  }

  result
}

