################################################################################
# Module: SETUP SIDEBAR
# File contains helper functions that are used for app UI components or 
# server-side validation of user inputs.
################################################################################

################################################################################
# Helper UI functions.
################################################################################

# function for label assignment UI
labelSetupUI <- function(ns, gctFileNames) {
  tagList(
    h4('Assign labels'),
    lapply(gctFileNames, function(file) {
      add_css_attributes(
        textInput(inputId = ns(paste0('Label_', file)),
                  label = file,
                  placeholder = "Proteome or Prot"),
        classes = "small-input")
    })
  )
}

# function containing setup elements for a single GCT file
# NOTE: make sure that the same naming convention is used as in in the 
# setupDefaults.yaml!
gctSetupUI <- function(ns, 
                       label, 
                       parameter_choices, 
                       parameters, 
                       current_place, 
                       max_place,
                       GCTs) {
  # groups column choices pulled from cdesc
  all_cdesc_columns <- names(GCTs[[label]]@cdesc)
  
  # Filter to only discrete columns for analysis annotation (exclude continuous columns)
  # NOTE: We allow ALL discrete columns regardless of number of categories (even if <2)
  # Statistical testing will be disabled in the statistics tab if there are <2 categories
  groups_choices <- all_cdesc_columns[vapply(GCTs[[label]]@cdesc[all_cdesc_columns], function(col) is.discrete(col), logical(1))]
  
  # If no suitable annotation columns remain, use Sample.ID as fallback
  if (length(groups_choices) == 0) {
    groups_choices <- "Sample.ID"
  }
  
  # find which groups are present in all omes
  groups_choices_all_omes <- base::Reduce(base::intersect, 
                                    lapply(GCTs, function(gct) names(gct@cdesc)))
  # Select normalization choices based on current intensity_data parameter
  ind <- paste0(
    "intensity_data_",
    tolower(ifelse(isTRUE(parameters[[label]]$intensity_data == "Yes"), "yes", "no"))
  )
  # Filter out 2-component normalization if dataset has more than 20 samples (too slow)
  norm_choices <- parameter_choices$data_normalization[[ind]]
  n_samples <- ncol(GCTs[[label]]@mat)
  if (n_samples > 20) {
    norm_choices <- norm_choices[norm_choices != "2-component"]
  }
  # If current selection is not in the available choices, fall back to None
  norm_selected <- parameters[[label]]$data_normalization
  if (!norm_selected %in% norm_choices) {
    norm_selected <- "None"
  }

  sample_filter_column_selected <- parameters[[label]]$sample_filter_column
  if (is.null(sample_filter_column_selected)) {
    sample_filter_column_selected <- ""
  }
  sample_filter_values_choices <- character(0)
  if (sample_filter_column_selected %in% names(GCTs[[label]]@cdesc)) {
    sample_filter_values_choices <- sort(
      unique(as.character(GCTs[[label]]@cdesc[[sample_filter_column_selected]]))
    )
    sample_filter_values_choices <- sample_filter_values_choices[!is.na(sample_filter_values_choices)]
  }
  sample_filter_values_selected <- parameters[[label]]$sample_filter_values
  if (is.null(sample_filter_values_selected)) {
    sample_filter_values_selected <- character(0)
  }

  # row-filter column choices pulled from rdesc (discrete columns preferred)
  all_rdesc_columns <- names(GCTs[[label]]@rdesc)
  row_filter_columns_choices <- all_rdesc_columns[vapply(
    GCTs[[label]]@rdesc[all_rdesc_columns],
    function(col) is.discrete(col),
    logical(1)
  )]
  if (length(row_filter_columns_choices) == 0) {
    row_filter_columns_choices <- all_rdesc_columns
  }

  row_filter_column_selected <- parameters[[label]]$row_filter_column
  if (is.null(row_filter_column_selected)) {
    row_filter_column_selected <- ""
  }
  row_filter_values_choices <- character(0)
  if (row_filter_column_selected %in% names(GCTs[[label]]@rdesc)) {
    row_filter_values_choices <- sort(
      unique(as.character(GCTs[[label]]@rdesc[[row_filter_column_selected]]))
    )
    row_filter_values_choices <- row_filter_values_choices[!is.na(row_filter_values_choices)]
  }
  row_filter_values_selected <- parameters[[label]]$row_filter_values
  if (is.null(row_filter_values_selected)) {
    row_filter_values_selected <- character(0)
  }

  id_source_column_selected <- parameters[[label]]$id_source_column
  if (is.null(id_source_column_selected)) {
    id_source_column_selected <- ""
  }
  id_mapping_species_selected <- parameters[[label]]$id_mapping_species
  if (is.null(id_mapping_species_selected)) {
    id_mapping_species_selected <- "Homo sapiens"
  }
  tagList(
    h4('Setup for ',
       strong(span(label, style = "color:#a4dc84")),
       paste0(' (', current_place, '/', max_place, ')')), 
    
    ## groups column selection
    add_css_attributes(
        selectInput(
          ns(paste0(label, '_annotation_column')),
          "Analysis annotation column",
          choices = groups_choices,
          selected = ifelse(
            is.null(parameters[[label]]$annotation_column),
            groups_choices[1],
            parameters[[label]]$annotation_column)),
        classes = "small-input"),
    
    ## gene symbol column selection (selected value = stored parameters only; defaults
    ## are applied once after parse in sidebar_setup.R, not here)
    add_css_attributes(
        selectInput(
          ns(paste0(label, '_gene_symbol_column')),
          "Gene symbol column",
          choices = c("None", names(GCTs[[label]]@rdesc)),
          selected = {
            gsc_raw <- parameters[[label]]$gene_symbol_column
            gsc <- if (is.null(gsc_raw) || (length(gsc_raw) == 1L && is.na(gsc_raw))) {
              "None"
            } else {
              as.character(gsc_raw)[[1L]]
            }
            choices_gs <- c("None", names(GCTs[[label]]@rdesc))
            if (gsc %in% choices_gs) gsc else "None"
          }),
        classes = "small-input"),

    ## Map IDs to gene symbols (when Gene symbol column is None)
    # Use unqualified input names here; `ns = ns` lets conditionalPanel namespace them
    # correctly in JS. Do not call ns() inside the condition string — that double-
    # namespaces and the panel never shows when "None" is selected.
    conditionalPanel(
      condition = paste0("input['", label, "_gene_symbol_column'] == 'None'"),
      tagList(
        add_css_attributes(
          checkboxInput(
            ns(paste0(label, "_convert_ids_to_gene_symbol")),
            label = "Convert IDs to gene symbols",
            value = isTRUE(parameters[[label]]$convert_ids_to_gene_symbol)),
          classes = "small-input"),
        conditionalPanel(
          condition = paste0(
            "input['", label, "_gene_symbol_column'] == 'None' && input['", label, "_convert_ids_to_gene_symbol']"
          ),
          tagList(
            add_css_attributes(
              selectInput(
                ns(paste0(label, "_id_source_column")),
                label = "ID column for mapping",
                choices = c("", names(GCTs[[label]]@rdesc)),
                selected = id_source_column_selected),
              classes = "small-input"),
            add_css_attributes(
              selectInput(
                ns(paste0(label, "_id_mapping_species")),
                label = "Species (for ID mapping)",
                choices = c("Homo sapiens", "Mus musculus"),
                selected = id_mapping_species_selected),
              classes = "small-input")
          ),
          ns = ns
        )
      ),
      ns = ns
    ),
    
    ## intensity data input
    add_css_attributes(
      checkboxInput(
        ns(paste0(label, '_intensity_data')),
        label = 'Intensity data',
        value = parameters[[label]]$intensity_data == "Yes"),
      classes = "small-input",
      styles = "padding-top: 10px"),

    ## log transformation input
    add_css_attributes(
      selectInput(
        ns(paste0(label, '_log_transformation')),
        label = 'Log-transformation',
        choices = parameter_choices$log_transformation,
        selected = parameters[[label]]$log_transformation),
      classes = "small-input"),
    
    ## data normalization input
    add_css_attributes(
      selectInput(
        ns(paste0(label, '_data_normalization')),
        label = 'Data normalization',
        choices = norm_choices,
        selected = norm_selected),
      classes = "small-input"),
    
    ## group-wise normalization
    conditionalPanel(
      condition = paste0("input['", label, "_data_normalization'] != 'None'"),
      add_css_attributes(
        checkboxInput(
          ns(paste0(label, '_group_normalization')),
          label = "Group-wise normalization",
          value = parameters[[label]]$group_normalization),
        classes = "small-input"),
      ns = ns
    ),
    
    ## group-wise normalization column
    conditionalPanel(
      condition = paste0("(input['", label, "_data_normalization'] != 'None')",
                         " && (input['", label, "_group_normalization'])"),
      add_css_attributes(
        selectInput(
          ns(paste0(label, '_group_normalization_column')),
          label = "Column for group normalization",
          choices = groups_choices,
          selected = ifelse(
            is.null(parameters[[label]]$group_normalization_column),
            groups_choices[1],
            parameters[[label]]$group_normalization_column)),
        classes = "small-input"),
      ns = ns
    ),
    
    ## max missing value input
    add_css_attributes(
      numericInput(
        ns(paste0(label, '_max_missing')),
        'Max. % missing values',
        min = parameter_choices$max_missing[[ind]]$min,
        max = parameter_choices$max_missing[[ind]]$max,
        step = parameter_choices$max_missing[[ind]]$step,
        value = min(parameters[[label]]$max_missing, parameter_choices$max_missing[[ind]]$max)),
      classes = "small-input",
      styles = "padding-bottom: 5px"),
    
    ## data filter input 
    add_css_attributes(
      selectInput(
        ns(paste0(label, '_data_filter')),
        label = 'Filter data',
        choices = parameter_choices$data_filter,
        selected = parameters[[label]]$data_filter),
      classes = "small-input"),
    
    ## percentile for standard deviation filter
    conditionalPanel(
      condition = paste0("input['", label, "_data_filter'] == 'StdDev'"),
      add_css_attributes(
        numericInput(
          ns(paste0(label, '_data_filter_sd_pct')),
          label = "Percentile for StdDev",
          min = parameter_choices$data_filter_sd_pct$min,
          max = parameter_choices$data_filter_sd_pct$max,
          value = parameters[[label]]$data_filter_sd_pct),
        classes = "small-input"),
      ns = ns
    ),

    ## sample filtering input
    add_css_attributes(
      checkboxInput(
        ns(paste0(label, '_sample_filter_enabled')),
        label = 'Filter samples (columns)',
        value = isTRUE(parameters[[label]]$sample_filter_enabled)),
      classes = "small-input"),

    ## sample filtering column
    conditionalPanel(
      condition = paste0("input['", label, "_sample_filter_enabled']"),
      add_css_attributes(
        selectInput(
          ns(paste0(label, '_sample_filter_column')),
          label = "Sample filter column",
          choices = c("", groups_choices),
          selected = sample_filter_column_selected),
        classes = "small-input"),
      ns = ns
    ),

    ## sample filtering values
    conditionalPanel(
      condition = paste0("input['", label, "_sample_filter_enabled'] && input['", label, "_sample_filter_column'] != ''"),
      add_css_attributes(
        selectizeInput(
          ns(paste0(label, '_sample_filter_values')),
          label = "Keep samples with selected values",
          choices = sample_filter_values_choices,
          selected = sample_filter_values_selected,
          multiple = TRUE,
          options = list(plugins = list("remove_button"))),
        classes = "small-input"),
      ns = ns
    ),

    ## row filtering input
    add_css_attributes(
      checkboxInput(
        ns(paste0(label, '_row_filter_enabled')),
        label = 'Filter features (rows)',
        value = isTRUE(parameters[[label]]$row_filter_enabled)),
      classes = "small-input"),

    ## row filtering column
    conditionalPanel(
      condition = paste0("input['", label, "_row_filter_enabled']"),
      add_css_attributes(
        selectInput(
          ns(paste0(label, '_row_filter_column')),
          label = "Row filter column",
          choices = c("", row_filter_columns_choices),
          selected = row_filter_column_selected),
        classes = "small-input"),
      ns = ns
    ),

    ## row filtering values
    conditionalPanel(
      condition = paste0("input['", label, "_row_filter_enabled'] && input['", label, "_row_filter_column'] != ''"),
      add_css_attributes(
        selectizeInput(
          ns(paste0(label, '_row_filter_values')),
          label = "Keep rows with selected values",
          choices = row_filter_values_choices,
          selected = row_filter_values_selected,
          multiple = TRUE,
          options = list(plugins = list("remove_button"))),
        classes = "small-input"),
      ns = ns
    ),
    
    ## apply to all checkbox
    if (max_place > 1) {
      # only shows up if the groups column selection is present in all -omes
      # AND if there's either no group normalization or the group normalization
      # column is also present in all omes
      conditionalPanel(
        condition = paste0("['", paste(groups_choices_all_omes, collapse = "', '"), 
                           "'].includes(input['", label, "_annotation_column']) ",
                           "&& (!input['", label, "_group_normalization'] || ",
                           "['", paste(groups_choices_all_omes, collapse = "', '"), 
                           "'].includes(input['", label, "_group_normalization_column']))"),
        add_css_attributes(
          checkboxInput(ns('applyToAll'), 'Apply settings to all datasets'),
          classes = "small-input"),
        ns = ns
      )
      
    }
  )
}

# function for advanced settings UI
advancedSettingsUI <- function(ns, parameters) {
  labels <- names(parameters)
  
  if (length(labels) > 1) {
    tagList(
      fluidRow(column(12, selectInput(ns('default_ome'),
                                      "Default dataset",
                                      choices = labels))),
      hr()
    )
  }
}


################################################################################
# Helper functions for the setup sidebar module
################################################################################

# function to validate file labels
# all_labels is a named vector:
# elements in vector are all user inputted labels
# names are the corresponding file names
validate_labels <- function(all_labels) {
  # check that each label is a valid name
  for (i in seq_along(all_labels)) {
    label = all_labels[i]
    filename = names(all_labels)[i]
    if (make.names(label) != label) {
      stop(paste("Invalid label for", filename))
    }
    if (label == "multi_ome") {
      stop("Invalid label for ", filename, ", 'multi_ome' is a reserved word.")
    }
  }
  
  # check that labels aren't repeated
  if (length(unique(all_labels)) != length(all_labels)) {
    stop("All labels must be unique")
  }
  
  return(TRUE)
}

