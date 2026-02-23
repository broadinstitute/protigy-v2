################################################################################
# Module: SETUP SIDEBAR
# Helper functions for Spectronaut pivot report preprocessing
################################################################################

#' Extract protigy_id from a semicolon-delimited protein groups column
#'
#' Splits each value in source_column by separator, takes the first non-empty
#' token after trimws(), stores result as protigy_id column at position 1.
#'
#' @param data data.frame
#' @param source_column character name of column to split
#' @param separator character separator (default ";")
#' @return data.frame with protigy_id column at position 1
extract_protigy_id <- function(data, source_column, separator = ";") {
  stopifnot(is.data.frame(data), source_column %in% names(data))

  values <- data[[source_column]]
  protigy_id <- vapply(values, function(v) {
    if (is.na(v) || nchar(trimws(v)) == 0) return(NA_character_)
    tokens <- strsplit(as.character(v), separator, fixed = TRUE)[[1]]
    tokens <- trimws(tokens)
    tokens <- tokens[nchar(tokens) > 0]
    if (length(tokens) == 0) return(NA_character_)
    tokens[[1]]
  }, character(1), USE.NAMES = FALSE)

  data$protigy_id <- protigy_id
  # Move protigy_id to position 1
  other_cols <- setdiff(names(data), "protigy_id")
  data[, c("protigy_id", other_cols), drop = FALSE]
}

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
    matching <- data_columns[grepl(rl, data_columns, fixed = TRUE)]
    for (col in matching) {
      # Find position of run_label in column name
      pos <- regexpr(rl, col, fixed = TRUE)
      if (pos > 0) {
        suffix <- substring(col, pos + nchar(rl))
        if (nchar(suffix) > 0) {
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
#' @param merge_condition_replicate logical; if TRUE, new name = paste0(Condition, "_R", Replicate)
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
    if (length(matches) > 1L) return(matches[1L])  # take first if ambiguous
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

#' Split a gene symbol column and use the first non-empty value
#'
#' For each value in rdesc[[column]], splits by separator and takes the first
#' non-empty token. Assigns result to rdesc$geneSymbol.
#'
#' @param rdesc data.frame (row descriptor)
#' @param column character name of column to split
#' @param separator character separator (default ";")
#' @return modified rdesc
split_gene_symbol_column <- function(rdesc, column, separator = ";") {
  stopifnot(is.data.frame(rdesc), column %in% names(rdesc))

  values <- rdesc[[column]]
  rdesc$geneSymbol <- vapply(values, function(v) {
    if (is.na(v) || nchar(trimws(as.character(v))) == 0) return(NA_character_)
    tokens <- strsplit(as.character(v), separator, fixed = TRUE)[[1]]
    tokens <- trimws(tokens)
    tokens <- tokens[nchar(tokens) > 0]
    if (length(tokens) == 0) return(NA_character_)
    tokens[[1]]
  }, character(1), USE.NAMES = FALSE)

  rdesc
}

#' UI for Spectronaut preprocessing options
#'
#' @param ns namespace function from session$ns
#' @param data_columns character vector of column names from the uploaded data
#' @return tagList
spectronautSetupUI <- function(ns, data_columns) {
  tagList(
    h4("Spectronaut Preprocessing"),

    checkboxInput(ns("spectronaut_create_id"),
                  "Create protigy_id from first protein (split by separator)",
                  value = FALSE),

    conditionalPanel(
      condition = paste0("input['", ns("spectronaut_create_id"), "']"),
      div(
        class = "small-input",
        selectInput(ns("spectronaut_id_source_column"),
                    "Source column:",
                    choices = data_columns,
                    selected = data_columns[1]),
        textInput(ns("spectronaut_id_separator"),
                  "Separator:",
                  value = ";")
      )
    ),

    checkboxInput(ns("spectronaut_use_condition_setup"),
                  "Use Spectronaut condition setup file",
                  value = FALSE),

    conditionalPanel(
      condition = paste0("input['", ns("spectronaut_use_condition_setup"), "']"),
      div(
        fileInput(ns("spectronautConditionFile"),
                  "Condition setup file (.tsv or .csv):",
                  accept = c(".tsv", ".csv")),
        checkboxInput(ns("spectronaut_merge_condition_replicate"),
                      "Merge Condition and Replicate (e.g. wholeCell_R1)",
                      value = FALSE),
        uiOutput(ns("spectronautSuffixUI"))
      )
    )
  )
}
