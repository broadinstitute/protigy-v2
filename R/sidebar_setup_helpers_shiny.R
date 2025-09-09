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
  tagList(
    h4("Assign labels"),
    lapply(dataFileNames, function(file) {
      add_css_attributes(
        textInput(
          inputId = ns(paste0("Label_", file)),
          label = file,
          value = "prot"
        ),
        classes = "small-input"
      )
    })
  )
}

# function for TSV setup UI
tsvSetupUI <- function(ns, dataFiles, identifierColumns = NULL) {
  tagList(
    h4("TSV Handler"),

    # Show uploaded files
    div(
      style = "margin-bottom: 0px; padding: 15px;",
      h5("Uploaded TSV Data Files:"),
      lapply(dataFiles$name, function(filename) {
        div(
          style = "margin: 5px 0; display: flex; align-items: center;",
          icon("file", style = "color: #ffffff;
               margin-left: 5px;"),
          span(filename, style = "margin-left: 15px;
               font-weight: 500;
               word-break: break-word;
               overflow-wrap: break-word;
               max-width: 420px;
               display: inline-block;")
        )
      })
    ),

    # Unique identifier column selector
    conditionalPanel(
      condition = "true", # Always show, but could be conditional based on file upload
      div(
        class = "tsv-step",
        style = "margin-bottom: 10px; padding: 15px;",
        h5("Select Unique Identifier Column",
          style = "margin-top: 0; color: #5cb85c; font-weight: bold;"
        ),
        p("Choose the unique identifier column for your TSV data.",
          style = "word-wrap: break-word; overflow-wrap: break-word;"
        ),
        div(
          style = "margin-bottom: 10px;",
          if (!is.null(identifierColumns) && length(identifierColumns) > 0) {
            add_css_attributes(
              selectInput(ns("identifierColumn"),
                "Identifier Column:",
                choices = identifierColumns,
                selected = identifierColumns[1]
              ),
              classes = "small-input"
            )
          } else {
            p("Please upload TSV data files to see available columns.", style = "font-style: italic; color: #666;")
          }
        )
      )
    ),
    hr(),

    # Download Template
    div(
      class = "tsv-step",
      style = "margin-bottom: 10px; padding: 15px;",
      h5("Download Experimental Design Template",
        style = "margin-top: 0; color: #337ab7; font-weight: bold;"
      ),
      div(
        p("Click below to download the experimental design template specific to your TSV data.",
          style = "word-wrap: break-word; overflow-wrap: break-word; margin-bottom: 10px;")
        ),
      div(
        style = "text-align: left;",
        downloadButton(ns("downloadExpDesign"),
          span("Download experimentalDesign.csv", style = "margin-left: 15px;"), 
          class = "btn btn-primary",
          icon = icon("download"),
          style = "width: 100%; 
          display: flex; 
          align-items: center; 
          justify-content: center; 
          text-align: center; 
          white-space: nowrap"
        )
      )
    ),

    # Upload and Process
    div(
      class = "tsv-step",
      style = "margin-bottom: 10px; padding: 15px; border-left: 4px solid #f0ad4e;
                 border: 1px solid #e3e3e3; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
      h5("Upload Completed Template",
        style = "margin-top: 0; color: #f0ad4e; font-weight: bold;"
      ),
      p("Upload your customized experimental design file. For TSV data, this replaces the need for a separate condition setup file.",
        style = "word-wrap: break-word; overflow-wrap: break-word;"
      ),
      div(
        style = "margin-bottom: 0px;",
        fileInput(ns("expDesignFile"),
          "Upload completed experimentalDesign.csv file",
          accept = ".csv"
        )
      ),

      # Process button - only show when experimental design file is uploaded
      conditionalPanel(
        condition = "output.expDesignFileUploaded",
        ns = ns,
        br(),
        div(
          style = "margin-top: 15px; text-align: center;",
          actionButton(ns("processTSVData"),
            "Process TSV Data",
            class = "btn btn-success",
            style = "font-weight: bold; padding: 10px 30px;"
          )
        )
      )
    )
  )
}

# function for CSV/Excel setup UI
csvExcelSetupUI <- function(ns, dataFiles, identifierColumns = NULL) {
  tagList(
    h4("CSV/Excel Handler"),

    # Show uploaded files
    div(
      style = "margin-bottom: 0px; padding: 15px;",
      h5("Uploladed Data Files:"),
      lapply(dataFiles$name, function(filename) {
        div(
          style = "margin: 5px 0; display: flex; align-items: center;",
          icon("file", style = "color: #ffffff;
               margin-left: 5px;"),
          span(filename, style = "margin-left: 15px;
               font-weight: 500;
               word-break: break-word;
               overflow-wrap: break-word;
               max-width: 420px;
               display: inline-block;")
        )
      })
    ),

    # Unique identifier column selector
    conditionalPanel(
      condition = "true", # Always show, but could be conditional based on file upload
      div(
        class = "csv-excel-step",
        style = "margin-bottom: 10px; padding: 15px;",
        h5("Select Unique Identifier Column",
          style = "margin-top: 0; color: #5cb85c; font-weight: bold;"
        ),
        p("Choose the unique identifier column for your data.",
          style = "word-wrap: break-word; overflow-wrap: break-word;"
        ),
        div(
          style = "margin-bottom: 10px;",
          if (!is.null(identifierColumns) && length(identifierColumns) > 0) {
            add_css_attributes(
              selectInput(ns("identifierColumn"),
                "Identifier Column:",
                choices = identifierColumns,
                selected = identifierColumns[1]
              ),
              classes = "small-input"
            )
          } else {
            p("Please upload data files to see available columns.", style = "font-style: italic; color: #666;")
          }
        )
      )
    ),
    hr(),

    # Download Template
    div(
      class = "csv-excel-step",
      style = "margin-bottom: 10px; padding: 15px;",
      h5("Download Experimental Design Template",
        style = "margin-top: 0; color: #337ab7; font-weight: bold;"
      ),
      div(
        p("Click below to download the experimental design template specific to your data.",
          style = "word-wrap: break-word; overflow-wrap: break-word; margin-bottom: 10px;")
        ),
      div(
        style = "text-align: left;",
        downloadButton(ns("downloadExpDesign"),
          span("Download experimentalDesign.csv", style = "margin-left: 15px;"), 
          class = "btn btn-primary",
          icon = icon("download"),
          style = "width: 100%; 
          display: flex; 
          align-items: center; 
          justify-content: center; 
          text-align: center; 
          white-space: nowrap"
        )
      )
    ),

    # Upload and Process
    div(
      class = "csv-excel-step",
      style = "margin-bottom: 10px; padding: 15px; border-left: 4px solid #f0ad4e;
                 border: 1px solid #e3e3e3; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
      h5("Upload Completed Template",
        style = "margin-top: 0; color: #f0ad4e; font-weight: bold;"
      ),
      p("Upload your customized experimental design file. Remember: only columns with metadata will be included in the analysis, and rows with all NA metadata will be excluded.",
        style = "word-wrap: break-word; overflow-wrap: break-word;"
      ),
      div(
        style = "margin-bottom: 0px;",
        fileInput(ns("expDesignFile"),
          "Upload completed experimentalDesign.csv file",
          accept = ".csv",
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        )
      ),

      # Process button - only show when experimental design file is uploaded
      conditionalPanel(
        condition = paste0("output['", ns("expDesignFileUploaded"), "']"),
        div(
          style = "margin-top: 0px;",
          actionButton(ns("processCSVExcel"),
            "Process Data & Continue",
            class = "btn btn-success",
            icon = icon("cogs"),
            style = "width: 100%; word-wrap: break-word; white-space: normal;"
          )
        )
      ),
    )
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
  groups_choices_all_omes <- base::Reduce(
    base::intersect,
    lapply(GCTs, function(gct) names(gct@cdesc))
  )

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
        "(input['", label, "_data_normalization'] != 'None')",
        " && (input['", label, "_group_normalization'])"
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
          "['", paste(groups_choices_all_omes, collapse = "', '"),
          "'].includes(input['", label, "_annotation_column']) ",
          "&& (!input['", label, "_group_normalization'] || ",
          "['", paste(groups_choices_all_omes, collapse = "', '"),
          "'].includes(input['", label, "_group_normalization_column']))"
        ),
        add_css_attributes(
          checkboxInput(ns("applyToAll"), "Apply settings to all -omes"),
          classes = "small-input"
        ),
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
      fluidRow(column(12, selectInput(ns("default_ome"),
        "Default -ome",
        choices = labels
      ))),
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
    label <- all_labels[i]
    filename <- names(all_labels)[i]
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

actionButton_icon_right <- function(inputId, label, icon, width = NULL) {
  button <- shiny::actionButton(inputId, label, icon, width)
  button$children[[1]] <- rev(button$children[[1]])
  return(button)
}
