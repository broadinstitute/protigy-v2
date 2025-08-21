################################################################################
# Module: SETUP SIDEBAR
# File contains helper functions that are used for app UI components or
# server-side validation of user inputs.
################################################################################

################################################################################
# Helper UI functions.
################################################################################

# function for label assignment UI
labelSetupUI <- function(ns, dataFileNames) {
  tagList(h4("Assign labels"), lapply(dataFileNames, function(file) {
    add_css_attributes(textInput(
      inputId = ns(paste0("Label_", file)),
      label = file,
      value = "prot"
    ),
    classes = "small-input")
  }))
}

# function for CSV/Excel setup UI
csvExcelSetupUI <- function(ns,
                            dataFiles,
                            identifierColumns = NULL,
                            showIdentifierSelector = TRUE,
                            detectedIdentifier = NULL,
                            detectionMethod = NULL) {
  tagList(
    h4("CSV/Excel File Handler"),
    
    # Conditionally show identifier selector only when needed
    if (showIdentifierSelector) {
      tagList(
        h5("Define Identifier Column"),
        p(
          "Your data doesn't contain a 'PG.ProteinGroups' column. Please select which column to use as the unique identifier."
        ),
        add_css_attributes(
          selectInput(
            ns("identifierColumn"),
            "Identifier Column:",
            choices = identifierColumns,
            selected = if (length(identifierColumns) > 0)
              identifierColumns[1]
            else
              NULL
          ),
          style = "
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
          ",
          classes = "small-input identifier-selector-visible"
        )
      )
    } else {
      div(class = "identifier-selector-hidden", h5("Identifier Column Detected!"), if (!is.null(detectedIdentifier)) {
        p("Using identifier column: ", strong(detectedIdentifier))
      } else {
        p(
          "Using automatic identifier detection (first protein group from PG.ProteinGroups)."
        )
      })
    },
    
    # Download experimental design template
    h5("Download Experimental Design Template"),
    p(
      "Click below to download the experimental design template specific to your data."
    ),
    add_css_attributes(
      downloadButton(
        ns("downloadExpDesign"),
        "Download",
        class = "btn btn-primary",
        icon = icon("download")
      ),
      classes = "download-btn-primary"
    ),
    
    # Upload experimental design file
    h5("Upload Experimental Design"),
    add_css_attributes(
      fileInput(
        ns("expDesignFile"),
        NULL,
        accept = ".csv",
        buttonLabel = "Browse",
        placeholder = "No file selected"
      ),
      classes = "small-input shiny-file-input-container"
    ),
    
    # Once experimental design is uploaded, the user can start analysis
    conditionalPanel(
      condition = paste0("output['", ns("expDesignFileUploaded"), "']"),
      add_css_attributes(
        actionButton(
          ns("processCSVExcel"),
          "Process Data & Continue",
          class = "Start Analysis!",
          icon = icon("cogs"),
          style = "width: 80%; align-items: center;"
        )
      )
    ),
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
  groups_choices <- names(GCTs[[label]]@cdesc)
  
  # find which groups are present in all omes
  groups_choices_all_omes <- base::Reduce(base::intersect, lapply(GCTs, function(gct)
    names(gct@cdesc)))
  
  tagList(
    h4(
      "Setup for ",
      strong(span(label, style = "color:#a4dc84")),
      paste0(" (", current_place, "/", max_place, ")")
    ),
    
    ## groups column selection
    add_css_attributes(
      selectInput(
        ns(paste0(label, "_annotation_column")),
        "Analysis annotation column",
        choices = groups_choices,
        selected = ifelse(
          is.null(parameters[[label]]$annotation_column),
          groups_choices[1],
          parameters[[label]]$annotation_column
        )
      ),
      classes = "small-input"
    ),
    
    ## intensity data input
    add_css_attributes(
      selectInput(
        ns(paste0(label, "_intensity_data")),
        "Intensity data",
        choices = parameter_choices$intensity_data,
        selected = parameters[[label]]$intensity_data
      ),
      classes = "small-input"
    ),
    
    ## log transformation input
    add_css_attributes(
      selectInput(
        ns(paste0(label, "_log_transformation")),
        label = "Log-transformation",
        choices = parameter_choices$log_transformation,
        selected = parameters[[label]]$log_transformation
      ),
      classes = "small-input"
    ),
    
    ## data normalization input
    add_css_attributes(
      selectInput(
        ns(paste0(label, "_data_normalization")),
        label = "Data normalization",
        choices = parameter_choices$data_normalization$intensity_data_no,
        selected = parameters[[label]]$data_normalization
      ),
      classes = "small-input"
    ),
    
    ## group-wise normalization
    conditionalPanel(
      condition = paste0("input['", label, "_data_normalization'] != 'None'"),
      add_css_attributes(
        checkboxInput(
          ns(paste0(label, "_group_normalization")),
          label = "Group-wise normalization",
          value = parameters[[label]]$group_normalization
        ),
        classes = "small-input"
      ),
      ns = ns
    ),
    
    ## group-wise normalization column
    conditionalPanel(
      condition = paste0(
        "(input['",
        label,
        "_data_normalization'] != 'None')",
        " && (input['",
        label,
        "_group_normalization'])"
      ),
      add_css_attributes(
        selectInput(
          ns(paste0(label, "_group_normalization_column")),
          label = "Column for group normalization",
          choices = groups_choices,
          selected = ifelse(
            is.null(parameters[[label]]$group_normalization_column),
            groups_choices[1],
            parameters[[label]]$group_normalization_column
          )
        ),
        classes = "small-input"
      ),
      ns = ns
    ),
    
    ## max missing value input
    add_css_attributes(
      numericInput(
        ns(paste0(label, "_max_missing")),
        "Max. % missing values",
        min = parameter_choices$max_missing$intensity_data_no$min,
        max = parameter_choices$max_missing$intensity_data_no$max,
        step = parameter_choices$max_missing$intensity_data_no$step,
        value = parameters[[label]]$max_missing
      ),
      classes = "small-input",
      styles = "padding-bottom: 5px"
    ),
    
    ## data filter input
    add_css_attributes(
      selectInput(
        ns(paste0(label, "_data_filter")),
        label = "Filter data",
        choices = parameter_choices$data_filter,
        selected = parameters[[label]]$data_filter
      ),
      classes = "small-input"
    ),
    
    ## percentile for standard deviation filter
    conditionalPanel(
      condition = paste0("input['", label, "_data_filter'] == 'StdDev'"),
      add_css_attributes(
        numericInput(
          ns(paste0(label, "_data_filter_sd_pct")),
          label = "Percentile for StdDev",
          min = parameter_choices$data_filter_sd_pct$min,
          max = parameter_choices$data_filter_sd_pct$max,
          value = parameters[[label]]$data_filter_sd_pct
        ),
        classes = "small-input"
      ),
      ns = ns
    ),
    
    ## apply to all checkbox
    if (max_place > 1) {
      # only shows up if the groups column selection is present in all -omes
      # AND if there's either no group normalization or the group normalization
      # column is also present in all omes
      conditionalPanel(
        condition = paste0(
          "['",
          paste(groups_choices_all_omes, collapse = "', '"),
          "'].includes(input['",
          label,
          "_annotation_column']) ",
          "&& (!input['",
          label,
          "_group_normalization'] || ",
          "['",
          paste(groups_choices_all_omes, collapse = "', '"),
          "'].includes(input['",
          label,
          "_group_normalization_column']))"
        ),
        add_css_attributes(checkboxInput(
          ns("applyToAll"), "Apply settings to all -omes"
        ), classes = "small-input"),
        ns = ns
      )
    }
  )
}

# function for advanced settings UI
advancedSettingsUI <- function(ns, parameters) {
  labels <- names(parameters)
  
  if (length(labels) > 1) {
    tagList(fluidRow(column(
      12, selectInput(ns("default_ome"), "Default -ome", choices = labels)
    )), hr())
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
    label <- all_labels[i]
    filename <- names(all_labels)[i]
    if (make.names(label) != label) {
      stop(paste("Invalid label for", filename))
    }
    if (label == "multi_ome") {
      stop("Invalid label for ",
           filename,
           ", 'multi_ome' is a reserved word.")
    }
  }
  
  # check that labels aren't repeated
  if (length(unique(all_labels)) != length(all_labels)) {
    stop("All labels must be unique")
  }
  
  return(TRUE)
}

actionButton_icon_right <- function(inputId, label, icon, width = NULL) {
  button <- shiny::actionButton(inputId, label, icon, width)
  button$children[[1]] <- rev(button$children[[1]])
  return(button)
}
