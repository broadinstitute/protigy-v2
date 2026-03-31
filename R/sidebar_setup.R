################################################################################
# Module: SETUP SIDEBAR
# Main shiny functions (server and UI)
################################################################################

# Whether current setup choices allow copying settings to every dataset.
# Returns list(ok = TRUE) or list(ok = FALSE, msg = <user-facing string>).
gct_setup_apply_to_all_valid <- function(
    annotation_column,
    group_normalization,
    group_normalization_column,
    sample_filter_enabled,
    sample_filter_column,
    row_filter_enabled,
    row_filter_column,
    gene_symbol_column,
    convert_ids_to_gene_symbol,
    id_source_column,
    groups_in_all,
    rdesc_in_all) {
  if (is.null(annotation_column)) {
    annotation_column <- NA_character_
  }
  if (is.null(group_normalization_column)) {
    group_normalization_column <- NA_character_
  }
  if (is.null(sample_filter_column)) {
    sample_filter_column <- ""
  }
  if (is.null(row_filter_column)) {
    row_filter_column <- ""
  }
  if (is.null(gene_symbol_column)) {
    gene_symbol_column <- "None"
  }
  if (is.null(id_source_column)) {
    id_source_column <- ""
  }
  if (length(groups_in_all) == 0L) {
    return(list(
      ok = FALSE,
      msg = paste0(
        "'Apply settings to all datasets' was disabled. ",
        "Choose consistent shared annotation/filter columns across datasets to enable this setting."
      )
    ))
  }
  if (!(annotation_column %in% groups_in_all)) {
    return(list(
      ok = FALSE,
      msg = paste0(
        "'Apply settings to all datasets' was disabled. ",
        "Choose a consistent analysis annotation column that exists in all datasets to enable this setting."
      )
    ))
  }
  if (isTRUE(group_normalization) && !(group_normalization_column %in% groups_in_all)) {
    return(list(
      ok = FALSE,
      msg = paste0(
        "'Apply settings to all datasets' was disabled. ",
        "Choose a consistent group normalization column that exists in all datasets to enable this setting."
      )
    ))
  }
  if (isTRUE(sample_filter_enabled) && nzchar(sample_filter_column) &&
        !(sample_filter_column %in% groups_in_all)) {
    return(list(
      ok = FALSE,
      msg = paste0(
        "'Apply settings to all datasets' was disabled. ",
        "Sample filter column '", sample_filter_column,
        "' is not available in all datasets. Choose a consistent column to enable this setting."
      )
    ))
  }
  if (isTRUE(row_filter_enabled) && nzchar(row_filter_column)) {
    if (length(rdesc_in_all) == 0L || !(row_filter_column %in% rdesc_in_all)) {
      return(list(
        ok = FALSE,
        msg = if (length(rdesc_in_all) == 0L) {
          paste0(
            "'Apply settings to all datasets' was disabled. ",
            "Choose consistent shared row metadata columns across datasets to enable this setting."
          )
        } else {
          paste0(
            "'Apply settings to all datasets' was disabled. ",
            "Row filter column '", row_filter_column,
            "' is not available in all datasets. Choose a consistent column to enable this setting."
          )
        }
      ))
    }
  }
  if (isTRUE(convert_ids_to_gene_symbol)) {
    if (!identical(gene_symbol_column, "None")) {
      return(list(
        ok = FALSE,
        msg = paste0(
          "'Apply settings to all datasets' was disabled. ",
          "ID-to-gene-symbol conversion requires Gene symbol column to be set to None when apply-to-all is used."
        )
      ))
    }
    if (!nzchar(id_source_column)) {
      return(list(
        ok = FALSE,
        msg = paste0(
          "'Apply settings to all datasets' was disabled. ",
          "Choose an ID source column for mapping to gene symbols to enable this setting."
        )
      ))
    }
    if (length(rdesc_in_all) == 0L || !(id_source_column %in% rdesc_in_all)) {
      return(list(
        ok = FALSE,
        msg = if (length(rdesc_in_all) == 0L) {
          paste0(
            "'Apply settings to all datasets' was disabled. ",
            "Choose consistent shared row metadata columns across datasets to enable this setting."
          )
        } else {
          paste0(
            "'Apply settings to all datasets' was disabled. ",
            "ID source column '", id_source_column,
            "' is not available in all datasets. Choose a consistent column to enable this setting."
          )
        }
      ))
    }
  }
  list(ok = TRUE, msg = NA_character_)
}

# UI for the sidebar setup
setupSidebarUI <- function(id = "setupSidebar") {
  # namespace function, wrap inputId's and outputId's with this (e.g. `ns(id)`)
  ns <- NS(id) 
  
  # tagList(
  #   # file input
  #   fileInput(ns("dataFiles"), 
  #             paste("Choose data file(s). Supported formats: GCT, CSV, Excel. All files should include the same samples."),
  #             multiple = TRUE,
  #             accept = c(".gct", ".csv", ".xlsx", ".xls")),
  #   hr(),
  #   
  #   # the main body of the sidebar, contents assigned in setupSidebarServer
  #   uiOutput(ns('sideBarMain')),
  #   
  #   # navigation buttons on the bottom left/right of sidebar
  #   fluidRow(
  #     column(6, uiOutput(ns('leftButton'))),
  #     column(6, uiOutput(ns('rightButton')))
  #   )
  # ) # end tagList
  
  # Fine-tuning formatting of initial file upload
  tags$div(
    style = "margin-bottom: 10px; padding: 15px; width: 100%", 
    tagList(
      # File upload section - conditionally shown
      uiOutput(ns("fileUploadSection")),

      hr(),

      # Step indicator for multi-step CSV/Excel workflow
      uiOutput(ns('csvStepIndicator')),

      # the main body of the sidebar, contents assigned in setupSidebarServer
      uiOutput(ns('sideBarMain')),
      
      # navigation buttons on the bottom left/right of sidebar
      fluidRow(
        column(6, div(style = "text-align: left;",  uiOutput(ns('leftButton')))),
        column(6, div(style = "text-align: right;", uiOutput(ns('rightButton'))))
      )
    )
  )
}

# server for the sidebar setup
setupSidebarServer <- function(id = "setupSidebar", parent) { moduleServer( 
  id,
  
  ## module function
  function (input, output, session) {
    
    # get namespace in case you need to use it in renderUI-like functions
    ns <- session$ns
    
    ### INITIALIZATION ###
    
    # initialize main outputs from this module
    GCTs_and_params <- reactiveVal() # GCT object and corresponding parameters
    globals <- reactiveValues() # global values for plots, displays, etc.
    GCTs_original <- reactiveVal() # the original GCTS (not processed)
    
    # initialize INTERNAL reactive values....only used in this module
    parameters_internal_reactive <- reactiveVal()
    GCTs_unprocessed_internal_reactive <- reactiveVal()
    accumulated_files <- reactiveVal(NULL)  # Store accumulated file uploads

    # initialize reactiveValues with back/next logic for when user navigates
    # through each GCT file to input parameters
    backNextLogic <- reactiveValues(placeChanged = 0)
    
    # initialize reactiveVal to indicate when labels & gcts are validated + submitted
    labelsGO <- reactiveVal(0)
    gctsGO <- reactiveVal(0)
    csvExcel_identifier_columns_reactive <- reactiveVal(NULL)
    sample_filter_input_state <- reactiveValues()
    row_filter_input_state <- reactiveValues()
    # Labels that just received default_parameters; after first parse, gene_symbol_column
    # is set from rdesc (geneSymbol if present, else None) once — never overwrites user edits.
    gene_symbol_defaults_pending_labels <- reactiveVal(character(0))
    exp_design_df                  <- reactiveVal(NULL)
    show_exp_design_panel          <- reactiveVal(FALSE)
    is_spectronaut_reactive        <- reactiveVal(list())
    spectronaut_processed_data     <- reactiveVal(NULL)
    spectronaut_processed_data_pristine <- reactiveVal(NULL)  # immutable copy, never renamed
    preview_data_reactive          <- reactiveVal(NULL)
    preview_all_data               <- reactiveVal(list())  # named list of data frames keyed by filename
    condition_setup_mappings       <- reactiveVal(list())  # per-label mapping from condition setup
    column_rename_map              <- reactiveVal(character(0))  # final old->new column name map
    raw_column_names_per_label     <- reactiveVal(list())  # original raw column names per label, before any renaming
    condition_file_observers_registered <- reactiveVal(list())  # guard against duplicate observers

    # read in default settings and choices from yamls
    default_parameters <- read_yaml(system.file('setup_parameters/setupDefaults.yaml', package = 'Protigy'))
    parameter_choices <- read_yaml(system.file('setup_parameters/setupChoices.yaml', package = 'Protigy'))


    ### FILE UPLOAD SECTION UI ###
    
    # Conditionally show/hide file upload section based on setup completion
    output$fileUploadSection <- renderUI({
      # Hide file upload UI after setup is complete
      if (!is.null(GCTs_and_params())) {
        return(NULL)
      }
      
      tagList(
        h4("Upload your data file(s)"), 
        
        # File input
        fileInput(ns("dataFiles"),
                  paste("GCT, CSV, TSV, SSV, or Excel"),
                  multiple = TRUE,
                  accept = c(".gct", ".csv", ".xlsx", ".xls", ".tsv", ".ssv")),
        
        # Display uploaded files list with remove buttons
        uiOutput(ns("uploadedFilesList"))
      )
    })

    ### UPLOADED FILES LIST UI ###

    # Display list of uploaded files with remove buttons
    output$uploadedFilesList <- renderUI({
      # Hide file upload UI after setup is complete
      if (!is.null(GCTs_and_params())) {
        return(NULL)
      }
      
      # Use validate instead of req for better error handling
      validate(need(accumulated_files(), ""))

      files <- accumulated_files()

      # Additional safety check
      if (is.null(files) || nrow(files) == 0) {
        return(NULL)
      }

      tagList(
        h5(paste0("Uploaded Files (", nrow(files), ")")),
        div(
          style = "max-height: 200px; overflow-y: auto; margin-bottom: 10px; width: 100%;",
          lapply(1:nrow(files), function(i) {
            # Use filename as unique identifier (sanitize for use as ID)
            file_id <- gsub("[^a-zA-Z0-9_]", "_", files$name[i])
            div(
              style = "padding: 8px; margin: 3px 0; background-color: #f8f9fa; border-radius: 3px; display: flex; align-items: flex-start; justify-content: space-between; width: 100%; box-sizing: border-box; min-height: 35px; height: auto;",
              div(
                style = "flex: 1; padding-right: 10px; color: #333; font-size: 13px; word-wrap: break-word; overflow-wrap: break-word; word-break: break-all; line-height: 1.4;",
                files$name[i]
              ),
              actionButton(
                ns(paste0("remove_file_", file_id)),
                label = NULL,
                icon = icon("times"),
                class = "btn-sm btn-primary",
                style = "padding: 2px 8px; font-size: 12px; flex-shrink: 0;",
                `data-filename` = files$name[i]  # Store filename as data attribute
              )
            )
          })
        ),
        actionButton(
          ns("clearAllFiles"),
          "Clear All",
          class = "btn-sm btn-primary",
          icon = icon("trash")
        )
      )
    })


    ### STEP 1: LABEL ASSIGNMENT ###
    
    # once files uploaded, accumulate and display label assignment
    observeEvent(
      eventExpr = input$dataFiles,
      ignoreInit = TRUE,
      handlerExpr = {
        # Get newly uploaded files
        new_files <- input$dataFiles

        # Validate file format consistency
        new_extensions <- tools::file_ext(tolower(new_files$name))

        # Check if adding to existing files
        if (!is.null(accumulated_files())) {
          existing_extensions <- tools::file_ext(tolower(accumulated_files()$name))
          all_extensions <- c(existing_extensions, new_extensions)

          # Validate all files are same type
          if (!(all(all_extensions == "gct") || all(all_extensions %in% c("csv", "xlsx", "xls", "tsv", "ssv")))) {
            showNotification(
              ui = HTML(paste0("<b>File Type Mismatch</b><br>", "All uploaded files must be the same type (all GCT or all CSV/Excel/TSV/SSV). Please remove existing files before uploading a different file type.")),
              type = "error",
              duration = NULL,
              closeButton = TRUE
            )
            return()
          }

          # Check for duplicate filenames
          if (any(new_files$name %in% accumulated_files()$name)) {
            showNotification(
              ui = HTML(paste0("<b>Duplicate Files</b><br>", "Some files have already been uploaded. Duplicate files will be skipped.")),
              type = "warning",
              duration = NULL,
              closeButton = TRUE
            )
            # Filter out duplicates
            new_files <- new_files[!new_files$name %in% accumulated_files()$name, ]
            if (nrow(new_files) == 0) return()
          }

          # Accumulate files
          accumulated_files(rbind(accumulated_files(), new_files))
        } else {
          # First upload - validate file types
          if (!(all(new_extensions == "gct") || all(new_extensions %in% c("csv", "xlsx", "xls", "tsv", "ssv")))) {
            showNotification(
              ui = HTML(paste0("<b>Error</b><br>", "Please upload files of the same type only (GCT, CSV/Excel/TSV/SSV). Mixed file types are not supported.")),
              type = "error",
              duration = NULL,
              closeButton = TRUE
            )
            return()
          }

          # First upload
          accumulated_files(new_files)

          # Reset internal state for new session
          parameters_internal_reactive(NULL)
          GCTs_unprocessed_internal_reactive(NULL)
        }

        # Trigger workflow based on file type (using accumulated files)
        file_extensions <- tools::file_ext(tolower(accumulated_files()$name))

        if (all(file_extensions == "gct")) {
          # All GCT files - use existing workflow
          labelAssignment()
        } else if (all(file_extensions %in% c("csv", "xlsx", "xls", "tsv", "ssv"))) {
          # All CSV/Excel/TSV/SSV files - use same workflow
          csvExcelWorkflow()

          # Automatically switch to CSV/TSV/SSV/Excel Processing help tab
          updateTabsetPanel(session = parent,
                           inputId = "navbar-tabs",
                           selected = "Help-Analysis")

          # Switch to the CSV/TSV/SSV/Excel Processing tab within the help section
          shinyjs::runjs("
            setTimeout(function() {
              $('a[data-value=\"CSV/TSV/SSV/Excel Processing\"]').click();
            }, 100);
          ")
        }
      })


    ### FILE REMOVAL HANDLERS ###

    # Handle individual file removal - Use filename as unique identifier to prevent index issues
    observe({
      # Hide file removal handlers after setup is complete
      if (!is.null(GCTs_and_params())) {
        return(NULL)
      }
      
      # Use validate for cleaner NULL handling
      validate(need(accumulated_files(), ""))

      files <- accumulated_files()

      # Additional safety check
      if (is.null(files) || nrow(files) == 0) {
        return(NULL)
      }

      # Create observers for each remove button using filename as unique identifier
      lapply(1:nrow(files), function(i) {
        # Use filename as unique identifier (sanitize for use as ID)
        file_id <- gsub("[^a-zA-Z0-9_]", "_", files$name[i])
        btn_id <- paste0("remove_file_", file_id)
        filename <- files$name[i]  # Capture filename at observer creation time

        observeEvent(input[[btn_id]], {
          # Wrap in tryCatch to handle any reactive errors
          tryCatch({
            # Isolate to get current file list at button click time
            current_files <- isolate(accumulated_files())

            # Safety check
            if (is.null(current_files)) {
              return(NULL)
            }

            # Find the file by name (not index) to handle cases where files were removed
            file_idx <- which(current_files$name == filename)
            
            if (length(file_idx) == 0) {
              # File already removed, do nothing
              return(NULL)
            }

            removed_name <- current_files$name[file_idx[1]]

            # Remove the file by name
            remaining_files <- current_files[current_files$name != filename, , drop = FALSE]

            if (nrow(remaining_files) == 0) {
              # Show notification FIRST (before state changes)
              showNotification(
                paste("Removed:", removed_name),
                type = "message",
                duration = 3
              )

              # Small delay to ensure notification is displayed
              Sys.sleep(0.1)

              # No files left - reset everything
              accumulated_files(NULL)
              parameters_internal_reactive(NULL)
              GCTs_unprocessed_internal_reactive(NULL)
              output$sideBarMain <- renderUI({ NULL })
              output$rightButton <- renderUI({ NULL })
              output$leftButton <- renderUI({ NULL })
            } else {
              # Update accumulated files first
              accumulated_files(remaining_files)

              # IMPORTANT: Reset parameters to force re-initialization
              parameters_internal_reactive(NULL)
              GCTs_unprocessed_internal_reactive(NULL)

              # Refresh the workflow with remaining files
              file_extensions <- tools::file_ext(tolower(remaining_files$name))
              if (all(file_extensions == "gct")) {
                labelAssignment()
              } else {
                csvExcelWorkflow()
              }

              # Show notification after state is updated
              showNotification(
                paste("Removed:", removed_name),
                type = "message",
                duration = 3
              )
            }
          }, error = function(e) {
            # Silently handle any reactive errors
            message("Error removing file: ", e$message)
          })
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      })
    })

    # Handle clear all files
    observeEvent(input$clearAllFiles, {
      # Only allow clearing files if setup is not complete
      if (!is.null(GCTs_and_params())) {
        showNotification(
          "Cannot clear files after setup is complete. Please go back to setup to modify files.",
          type = "warning",
          duration = 5
        )
        return()
      }
      
      accumulated_files(NULL)
      parameters_internal_reactive(NULL)
      GCTs_unprocessed_internal_reactive(NULL)
      output$sideBarMain <- renderUI({ NULL })
      output$rightButton <- renderUI({ NULL })
      output$leftButton <- renderUI({ NULL })

      showNotification(
        "All files cleared",
        type = "message",
        duration = 3
      )
    })


    # also display label assignment if user navigates back to it
    observeEvent(
      eventExpr = input$backToLabelsButton,
      ignoreInit = TRUE,
      handlerExpr = {
        # Save current setup widgets before leaving (Back from dataset 1 only runs here;
        # Next/Back between datasets already run collectInputs).
        # Do not run after analysis (advanced settings): inputs are not mounted and would
        # NULL-out parameters.
        if (is.null(GCTs_and_params())) {
          collectInputs()
        }
        # Reset GCTs_and_params to allow file upload/removal again
        GCTs_and_params(NULL)
        labelAssignment()
      })
    
    # validate labels once submitted
    observeEvent(input$submitLabelsButton, {
      out <- my_shinyalert_tryCatch({
        all_labels <- sapply(accumulated_files()$name,
                             function(n) input[[paste0('Label_', n)]])
        validate_labels(all_labels)
      }, return.error = FALSE)

      # increment labelsGO if labels are valid
      if (out) labelsGO(labelsGO() + 1)
    })
    
    # move the current tab to the analysis help tab
    observeEvent(labelsGO(), {
      updateTabsetPanel(session = parent, 
                        inputId = "navbar-tabs", 
                        selected = "Help-Analysis")
      
      # Switch to the Dataset Setup tab within the help section
      shinyjs::runjs("
        setTimeout(function() {
          $('a[data-value=\"Dataset Setup\"]').click();
        }, 100);
      ")
    }, ignoreInit = TRUE)
    
    
    ### STEP 2: INPUT GCT PARAMETERS ###
    
    # once labels assignment submitted, set values for back/next logic
    observeEvent(labelsGO(), {
      # current place in the next/back logic
      backNextLogic$place <- 1

      # maximum place (i.e. the total number of data files)
      backNextLogic$maxPlace <- length(accumulated_files()$name)
    }, ignoreInit = TRUE)

    # update GCT parameters with gct file paths and labels once labels are submitted
    observeEvent(labelsGO(), {
      # Check if parameters are already set up (CSV/Excel case) or need label assignment (GCT case)
      existing_params <- parameters_internal_reactive()

      if (!is.null(existing_params) && length(existing_params) > 0) {
        # CSV/Excel case: parameters already have labels and structure, no need to rebuild
        message("Using existing CSV/Excel parameters with labels: ", paste(names(existing_params), collapse = ", "))
        gene_symbol_defaults_pending_labels(character(0))
      } else {
        # GCT case: build parameters from file uploads and user-provided labels
        new_parameters <- list()
        defaults_pending <- character(0)
        apply(accumulated_files(), 1, function(file) {
          file <- as.list(file)
          
          # get the label using the same inputId notation as in labelSetupUI()
          label <- input[[paste0('Label_', file$name)]] 
          
          # figure out which files were already parsed and have saved parameters
          already_parsed_files <- sapply(parameters_internal_reactive(), 
                                         function(l) l$gct_file_path)
          
          # use the old parameters if they exist
          if (file$datapath %in% already_parsed_files) {
            idx <- which(already_parsed_files == file$datapath)
            new_parameters[[label]] <<- parameters_internal_reactive()[[idx]]
            
            # otherwise use the defaults
          } else {
            new_parameters[[label]] <<- c(gct_file_path = file$datapath,
                                          gct_file_name = file$name,
                                          default_parameters)
            defaults_pending <<- c(defaults_pending, label)
          }
        })
        parameters_internal_reactive(new_parameters) # update GCT parameters reactiveVal
        gene_symbol_defaults_pending_labels(defaults_pending)
      }
    }, ignoreInit = TRUE)
    
    # parse the GCTs for setup
    observeEvent(labelsGO(), {
      parameters <- parameters_internal_reactive()
      existing_gcts <- GCTs_unprocessed_internal_reactive()
      
      # Check if GCTs are already parsed/converted (CSV/Excel case) or need parsing (GCT case)
      if (!is.null(existing_gcts) && length(existing_gcts) > 0) {
        # CSV/Excel case: GCTs already converted and stored, just trigger UI update
        message("Using existing CSV/Excel converted GCTs: ", paste(names(existing_gcts), collapse = ", "))
        backNextLogic$placeChanged <- backNextLogic$placeChanged + 1
      } else {
        # GCT case: need to parse GCT files from disk
        parsed_file_paths <- sapply(GCTs_unprocessed_internal_reactive(), function(gct) gct@src)
        GCTs <- my_shinyalert_tryCatch({
          withProgress(
            min = 0, 
            max = length(parameters),
            message = "Parsing GCTs...", 
            expr = {
              lapply(parameters, function(p) {
                # check if the GCT has already been parsed
                if (p$gct_file_path %in% parsed_file_paths) {
                  parsed_label = names(which(parsed_file_paths == p$gct_file_path))
                  stopifnot(length(parsed_label) == 1)
                  incProgress(amount = 1)
                  return(GCTs_unprocessed_internal_reactive()[[parsed_label]])
                  
                  # otherwise, parse the GCT
                } else {
                  gct <- parse_gctx(p$gct_file_path)
                  incProgress(amount = 1)
                  return(gct)
                }
              })
            })
        }, return.error = NULL)
        
        if (!is.null(GCTs)) {
          # update reactiveVal
          GCTs_unprocessed_internal_reactive(GCTs)
          
          pending <- gene_symbol_defaults_pending_labels()
          if (length(pending) > 0) {
            pm <- parameters_internal_reactive()
            for (nm in names(GCTs)) {
              if (nm %in% pending) {
                rdesc_n <- names(GCTs[[nm]]@rdesc)
                pm[[nm]]$gene_symbol_column <- if ("geneSymbol" %in% rdesc_n) "geneSymbol" else "None"
              }
            }
            parameters_internal_reactive(pm)
            gene_symbol_defaults_pending_labels(setdiff(pending, names(GCTs)))
          }
          
          # indicates if place or something about GCT files changed
          backNextLogic$placeChanged <- backNextLogic$placeChanged + 1 
        }
      }
    }, ignoreInit = TRUE, priority = -1)
    
    # display the correct GCT processing page, handling back/next logic
    observeEvent(
      eventExpr = backNextLogic$placeChanged, 
      ignoreInit = TRUE,
      handlerExpr = {
        # get the correct label for this file
        label = names(parameters_internal_reactive())[backNextLogic$place]
        
        # main GCT processing UI
        output$sideBarMain <- renderUI({gctSetupUI(ns = ns,
                                                   label = label,
                                                   parameter_choices = parameter_choices,
                                                   parameters = parameters_internal_reactive(),
                                                   current_place = backNextLogic$place,
                                                   max_place = backNextLogic$maxPlace,
                                                   GCTs = GCTs_unprocessed_internal_reactive())})
        
        # left button (back to labels or just back)
        if (backNextLogic$place == 1) {
          output$leftButton <- renderUI({actionButton(ns("backToLabelsButton"), 
                                                      "Back",
                                                      icon = icon("chevron-left"))})
        } else {
          output$leftButton <- renderUI({actionButton(ns("backButton"), 
                                                      "Back",
                                                      icon = icon("chevron-left"))})
        }
        
        # right button (next or submit gct for processing)
        if (backNextLogic$place == backNextLogic$maxPlace) {
          output$rightButton <- renderUI({
            actionButton(ns("submitGCTButton"),
                         "Submit",
                         class = "btn btn-primary")})
        } else {
          output$rightButton <- renderUI({
            actionButton(ns("nextButton"), "Next >", class = "btn btn-primary")
          })
        }
      })
    
    # update parameter choices when intensity data is toggled
    current_intensity <- reactive({
      label <- names(parameters_internal_reactive())[isolate(backNextLogic$place)]
      checkbox_value <- input[[paste0(label, '_intensity_data')]]
      # Convert checkbox boolean to "Yes"/"No" string
      if (is.logical(checkbox_value)) {
        if (checkbox_value) "Yes" else "No"
      } else {
        checkbox_value  # Fallback for old string values
      }
    })
    observeEvent(current_intensity(), {
      # first, collect all the current inputs
      collectInputs()

      # gather current label and parameters
      label = names(parameters_internal_reactive())[backNextLogic$place]
      parameters = parameters_internal_reactive()[[label]]

      # indicator for intensity data (check out the yaml format)
      ind = paste0("intensity_data_", tolower(current_intensity()))
      
      # update data normalization
      # Filter out 2-component normalization if dataset has more than 20 samples (too slow)
      norm_choices <- parameter_choices$data_normalization[[ind]]
      gct <- GCTs_unprocessed_internal_reactive()[[label]]
      n_samples <- if (!is.null(gct)) ncol(gct@mat) else 0
      if (n_samples > 20) {
        norm_choices <- norm_choices[norm_choices != "2-component"]
      }
      # If current selection is 2-component but it should be disabled, use default
      norm_selected <- ifelse(
        parameters$data_normalization %in% norm_choices,
        parameters$data_normalization,
        default_parameters$data_normalization)
      if (n_samples > 20 && norm_selected == "2-component") {
        norm_selected <- default_parameters$data_normalization
      }
      updateSelectInput(
        inputId = paste0(label, '_data_normalization'),
        choices = norm_choices,
        selected = norm_selected)
      
      # update max missing
      updateNumericInput(
        inputId = paste0(label, '_max_missing'),
        min = parameter_choices$max_missing[[ind]]$min,
        max = parameter_choices$max_missing[[ind]]$max,
        step = parameter_choices$max_missing[[ind]]$step,
        value = min(parameters$max_missing, parameter_choices$max_missing[[ind]]$max))
    }, ignoreInit = TRUE)

    # update sample filter values choices when sample filter column changes
    observe({
      req(parameters_internal_reactive(), GCTs_unprocessed_internal_reactive())
      current_label <- names(parameters_internal_reactive())[backNextLogic$place]
      req(current_label)
      selected_column <- input[[paste0(current_label, "_sample_filter_column")]]
      req(selected_column, selected_column != "")
      gct <- GCTs_unprocessed_internal_reactive()[[current_label]]
      req(gct, selected_column %in% names(gct@cdesc))

      choices <- sort(unique(as.character(gct@cdesc[[selected_column]])))
      choices <- choices[!is.na(choices)]
      selected_values <- isolate(input[[paste0(current_label, "_sample_filter_values")]])
      if (is.null(selected_values)) {
        selected_values <- character(0)
      }
      selected_values <- intersect(selected_values, choices)

      # Avoid re-sending the same input payload repeatedly (prevents UI flicker).
      next_state <- list(
        column = selected_column,
        choices = choices,
        selected = selected_values
      )
      current_state <- isolate(sample_filter_input_state[[current_label]])
      if (identical(current_state, next_state)) {
        return(invisible(NULL))
      }
      sample_filter_input_state[[current_label]] <- next_state

      updateSelectizeInput(
        inputId = paste0(current_label, "_sample_filter_values"),
        choices = choices,
        selected = selected_values,
        server = FALSE
      )
    })

    # update row filter values choices when row filter column changes
    observe({
      req(parameters_internal_reactive(), GCTs_unprocessed_internal_reactive())
      current_label <- names(parameters_internal_reactive())[backNextLogic$place]
      req(current_label)
      selected_column <- input[[paste0(current_label, "_row_filter_column")]]
      req(selected_column, selected_column != "")
      gct <- GCTs_unprocessed_internal_reactive()[[current_label]]
      req(gct, selected_column %in% names(gct@rdesc))

      choices <- sort(unique(as.character(gct@rdesc[[selected_column]])))
      choices <- choices[!is.na(choices)]
      selected_values <- isolate(input[[paste0(current_label, "_row_filter_values")]])
      if (is.null(selected_values)) {
        selected_values <- character(0)
      }
      selected_values <- intersect(selected_values, choices)

      # Avoid re-sending the same input payload repeatedly (prevents UI flicker).
      next_state <- list(
        column = selected_column,
        choices = choices,
        selected = selected_values
      )
      current_state <- isolate(row_filter_input_state[[current_label]])
      if (identical(current_state, next_state)) {
        return(invisible(NULL))
      }
      row_filter_input_state[[current_label]] <- next_state

      updateSelectizeInput(
        inputId = paste0(current_label, "_row_filter_values"),
        choices = choices,
        selected = selected_values,
        server = FALSE
      )
    })
    
    
    # reset applyToAll to FALSE if it is not a valid option
    groups_in_all_omes <- reactive({
      base::Reduce(base::intersect, lapply(GCTs_unprocessed_internal_reactive(), function(gct) names(gct@cdesc)))
    })
    rdesc_in_all_omes <- reactive({
      base::Reduce(base::intersect, lapply(GCTs_unprocessed_internal_reactive(), function(gct) names(gct@rdesc)))
    })
    observe({
      req(parameters_internal_reactive(), GCTs_unprocessed_internal_reactive())
      # Do not use req() on intersect vectors: length-0 intersect is valid and must still invalidate apply-to-all.
      groups_in_all <- isolate(groups_in_all_omes())
      rdesc_in_all <- isolate(rdesc_in_all_omes())
      
      # get relevant inputs
      current_label <- names(parameters_internal_reactive())[backNextLogic$place]
      current_annotation_column <- input[[paste0(current_label, "_annotation_column")]]
      current_group_norm_column <- input[[paste0(current_label, "_group_normalization_column")]]
      current_group_norm_selection <- input[[paste0(current_label, "_group_normalization")]]
      current_sample_filter_enabled <- input[[paste0(current_label, "_sample_filter_enabled")]]
      current_sample_filter_column <- input[[paste0(current_label, "_sample_filter_column")]]
      current_row_filter_enabled <- input[[paste0(current_label, "_row_filter_enabled")]]
      current_row_filter_column <- input[[paste0(current_label, "_row_filter_column")]]
      current_gene_symbol_column <- input[[paste0(current_label, "_gene_symbol_column")]]
      current_convert_ids <- input[[paste0(current_label, "_convert_ids_to_gene_symbol")]]
      current_id_source_column <- input[[paste0(current_label, "_id_source_column")]]
      if (is.null(current_sample_filter_column)) {
        current_sample_filter_column <- ""
      }
      if (is.null(current_row_filter_column)) {
        current_row_filter_column <- ""
      }
      if (is.null(current_id_source_column)) {
        current_id_source_column <- ""
      }
      
      valid <- gct_setup_apply_to_all_valid(
        annotation_column = current_annotation_column,
        group_normalization = current_group_norm_selection,
        group_normalization_column = current_group_norm_column,
        sample_filter_enabled = current_sample_filter_enabled,
        sample_filter_column = current_sample_filter_column,
        row_filter_enabled = current_row_filter_enabled,
        row_filter_column = current_row_filter_column,
        gene_symbol_column = current_gene_symbol_column,
        convert_ids_to_gene_symbol = current_convert_ids,
        id_source_column = current_id_source_column,
        groups_in_all = groups_in_all,
        rdesc_in_all = rdesc_in_all
      )
      
      # update applyToAll to FALSE if necessary
      if (!isTRUE(valid$ok)) {
        was_on <- isTRUE(isolate(input$applyToAll))
        updateCheckboxInput(session = session, inputId = "applyToAll", value = FALSE)
        if (was_on) {
          showNotification(valid$msg, type = "warning", duration = 8, session = session)
        }
      }
    })
    
    
    
    # change next/back buttons if applyToAll == TRUE
    observeEvent(input$applyToAll, {
      if (isTRUE(input$applyToAll)) {
        current_label <- names(parameters_internal_reactive())[backNextLogic$place]
        groups_in_all <- isolate(groups_in_all_omes())
        rdesc_in_all <- isolate(rdesc_in_all_omes())
        sc <- input[[paste0(current_label, "_sample_filter_column")]]
        rc <- input[[paste0(current_label, "_row_filter_column")]]
        if (is.null(sc)) sc <- ""
        if (is.null(rc)) rc <- ""
        idsc <- input[[paste0(current_label, "_id_source_column")]]
        if (is.null(idsc)) idsc <- ""
        valid <- gct_setup_apply_to_all_valid(
          annotation_column = input[[paste0(current_label, "_annotation_column")]],
          group_normalization = input[[paste0(current_label, "_group_normalization")]],
          group_normalization_column = input[[paste0(current_label, "_group_normalization_column")]],
          sample_filter_enabled = input[[paste0(current_label, "_sample_filter_enabled")]],
          sample_filter_column = sc,
          row_filter_enabled = input[[paste0(current_label, "_row_filter_enabled")]],
          row_filter_column = rc,
          gene_symbol_column = input[[paste0(current_label, "_gene_symbol_column")]],
          convert_ids_to_gene_symbol = input[[paste0(current_label, "_convert_ids_to_gene_symbol")]],
          id_source_column = idsc,
          groups_in_all = groups_in_all,
          rdesc_in_all = rdesc_in_all
        )
        if (!isTRUE(valid$ok)) {
          showNotification(valid$msg, type = "warning", duration = 8, session = session)
          updateCheckboxInput(session = session, inputId = "applyToAll", value = FALSE)
          return()
        }
      }
      
      # change next button to submit
      if (input$applyToAll | backNextLogic$place == backNextLogic$maxPlace) {
        output$rightButton <- renderUI({actionButton(ns("submitGCTButton"), 
                                                     "Submit", 
                                                     class = "btn btn-primary")})
      } else {
        output$rightButton <- renderUI({
          actionButton(ns("nextButton"), "Next >", class = "btn btn-primary")
        })
      }
      
      # change back button to "back to labels"
      if (input$applyToAll | backNextLogic$place == 1) {
        output$leftButton <- renderUI({actionButton(ns("backToLabelsButton"), 
                                                    "Back",
                                                    icon = icon("chevron-left"))})
      } else {
        output$leftButton <- renderUI({actionButton(ns("backButton"), 
                                                    "Back",
                                                    icon = icon("chevron-left"))})
      }
    }) 
    
    # logic for when next button is clicked
    observeEvent(input$nextButton, {
      if (backNextLogic$place < backNextLogic$maxPlace) {
        backNextLogic$place <- backNextLogic$place + 1
        backNextLogic$placeChanged <- backNextLogic$placeChanged + 1
      }
    })
    
    # logic for when back button is clicked
    observeEvent(input$backButton, {
      if (backNextLogic$place > 1) {
        backNextLogic$place <- backNextLogic$place - 1
        backNextLogic$placeChanged <- backNextLogic$placeChanged + 1
      }
    })
    
    # collect user options once next button is hit
    observeEvent(
      eventExpr = input$nextButton, 
      ignoreInit = TRUE,
      priority = 1, # this code is executed before other observeEvent with priority = 0 (default)
      handlerExpr = collectInputs())
    
    # collect user options once back button is hit
    observeEvent(
      eventExpr = input$backButton, 
      ignoreInit = TRUE,
      priority = 1, # this code is executed before other observeEvent with priority = 0 (default)
      handlerExpr = collectInputs())
    
    # collect user options once submit GCT button is hit
    observeEvent(
      eventExpr = input$submitGCTButton, 
      ignoreInit = TRUE,
      priority = 1, # this code is executed before other observeEvent with priority = 0 (default)
      handlerExpr = collectInputs())
    
    
    ### STEP 3: GCT PROCESSING ###
    
    # process GCTs 
    observeEvent(input$submitGCTButton, {
      shinyjs::runjs("$('#data-preview-content').hide(); $('#toggleDataPreview').text('Show');")
      parameters <- parameters_internal_reactive()
      GCTs_up <- GCTs_unprocessed_internal_reactive()
      # Work on deep clones so setup/upload GCTs are not mutated (no geneSymbol_original on upload).
      GCTs_work <- stats::setNames(
        lapply(names(GCTs_up), function(nm) deep_clone_gct(GCTs_up[[nm]])),
        names(GCTs_up)
      )
      
      # call processGCTs function in a tryCatch
      processing_output <- processGCTs(GCTs = GCTs_work, parameters = parameters)
      
      # Use updated parameters (e.g. ID conversion turned off after failed mapping)
      parameters_after_process <- parameters
      if (!is.null(processing_output)) {
        parameters_after_process <- processing_output$parameters
      }
      transformation_output <- transformGCTs(GCTs = GCTs_work, parameters = parameters_after_process)
      # Original GCTs for export/QC: log-transformed mat but row metadata as uploaded (no mapping backups)
      if (!is.null(transformation_output)) {
        for (nm in names(transformation_output)) {
          te <- transformation_output[[nm]]
          if (!is.null(te) && nm %in% names(GCTs_up)) {
            transformation_output[[nm]] <- repackage_transformed_gct_with_upload_rdesc(te, GCTs_up[[nm]])
          }
        }
      }
      
      if (!is.null(processing_output)) {
        # set GCTs_and_params reactiveVal
        GCTs_and_params(processing_output)
        
        # Notify if ID conversion was auto-disabled for any dataset (details in warnings)
        for (ome in names(processing_output$parameters)) {
          if (ome %in% names(parameters) &&
              isTRUE(parameters[[ome]]$convert_ids_to_gene_symbol) &&
              !isTRUE(processing_output$parameters[[ome]]$convert_ids_to_gene_symbol)) {
            showNotification(
              paste0(
                "Dataset ", ome, ": \"Convert IDs to gene symbols\" was turned off because ",
                "no gene symbols could be resolved. Check the R console for message() details."
              ),
              type = "warning",
              duration = 8,
              session = session
            )
          }
        }
        
        # Keep sidebar setup parameters in sync with processing (conversion flags, etc.)
        merged_params <- parameters_internal_reactive()
        if (!is.null(merged_params)) {
          for (ome in names(processing_output$parameters)) {
            merged_params[[ome]] <- processing_output$parameters[[ome]]
            # User's setup choice must survive processing (same list is mutated in pipeline)
            if (ome %in% names(parameters)) {
              merged_params[[ome]]$gene_symbol_column <- parameters[[ome]]$gene_symbol_column
            }
          }
          parameters_internal_reactive(merged_params)
        }
        
        # save the original GCTs for output
        # these have been log transformed if selected
        GCTs_original(transformation_output)
        
        # increment gctsGO reactiveVal to show that processing is done
        gctsGO(gctsGO() + 1)
      }
    })
    
    # set new global variables
    observeEvent(gctsGO(), {
      req(GCTs_and_params())

      parameters <- GCTs_and_params()$parameters
      all_omes <- names(parameters)

      globals$omes <- all_omes
      globals$default_ome <- all_omes[1]
      globals$default_annotations <- sapply(
        all_omes,
        function(ome) parameters[[ome]]$annotation_column,
        simplify = FALSE
      )

      # Initialize colors with colorblind-safe palette
      GCTs <- GCTs_and_params()$GCTs
      GCTs_merged <- GCTs_and_params()$GCTs_merged
      req(GCTs, GCTs_merged)
      globals$colors <- make_custom_colors(GCTs, GCTs_merged)
    }, ignoreInit = TRUE)

    # move the current tab to the summary tab
    observeEvent(gctsGO(), {
      updateTabsetPanel(session = parent, inputId = "navbar-tabs", selected = "Summary")
    }, ignoreInit = TRUE)
    
    
    ### STEP 4: ADVANCED SETTINGS ###
    
    # once GCT setup submitted, go to advanced settings
    observeEvent(gctsGO(), {
      labels = names(GCTs_and_params()$parameters)
      output$sideBarMain <- renderUI({
        tagList(
          advancedSettingsUI(ns = ns, parameters = GCTs_and_params()$parameters),
        )})
      output$leftButton <- renderUI({actionButton(ns("backToLabelsButton"), 
                                                  "Back to setup",
                                                  icon = icon("chevron-left"))})
      output$rightButton <- NULL
    }, ignoreInit = TRUE)
    
    # add default -ome to globals
    observeEvent(
      input$default_ome, 
      globals$default_ome <- input$default_ome,
      ignoreInit = TRUE)
    
    
    
    
    ### LOCAL HELPER FUNCTIONS ###
    # these functions interact with the session's input/output and are used in 
    # multiple observeEvent() calls, so it's easier to have them defined as
    # as local helper functions
    
    # collect user inputs, has to be used in separate observeEvent() calls
    collectInputs <- function() {
      # get the current label
      all_labels <- names(parameters_internal_reactive())
      current_label <- all_labels[backNextLogic$place]

      # select labels for assignment
      applyToAll <- ifelse(is.null(input$applyToAll), FALSE, input$applyToAll)
      if (applyToAll) {
        sc <- input[[paste0(current_label, "_sample_filter_column")]]
        rc <- input[[paste0(current_label, "_row_filter_column")]]
        if (is.null(sc)) sc <- ""
        if (is.null(rc)) rc <- ""
        idsc <- input[[paste0(current_label, "_id_source_column")]]
        if (is.null(idsc)) idsc <- ""
        valid <- gct_setup_apply_to_all_valid(
          annotation_column = input[[paste0(current_label, "_annotation_column")]],
          group_normalization = input[[paste0(current_label, "_group_normalization")]],
          group_normalization_column = input[[paste0(current_label, "_group_normalization_column")]],
          sample_filter_enabled = input[[paste0(current_label, "_sample_filter_enabled")]],
          sample_filter_column = sc,
          row_filter_enabled = input[[paste0(current_label, "_row_filter_enabled")]],
          row_filter_column = rc,
          gene_symbol_column = input[[paste0(current_label, "_gene_symbol_column")]],
          convert_ids_to_gene_symbol = input[[paste0(current_label, "_convert_ids_to_gene_symbol")]],
          id_source_column = idsc,
          groups_in_all = isolate(groups_in_all_omes()),
          rdesc_in_all = isolate(rdesc_in_all_omes())
        )
        if (!isTRUE(valid$ok)) {
          showNotification(valid$msg, type = "warning", duration = 8, session = session)
          updateCheckboxInput(session = session, inputId = "applyToAll", value = FALSE)
          applyToAll <- FALSE
        }
      }
      if (applyToAll) {
        assignment_labels = all_labels # all labels
      } else {
        assignment_labels <- current_label # just the current label
      }

      # get the current parameters
      new_parameters <- parameters_internal_reactive()

      # get the list of all parameters names to update
      parameter_names <- c(names(default_parameters),
                           'annotation_column',
                           'group_normalization_column',
                           'gene_symbol_column',
                           'sample_filter_enabled',
                           'sample_filter_column',
                           'sample_filter_values',
                           'row_filter_enabled',
                           'row_filter_column',
                           'row_filter_values',
                           'gene_symbol_split',
                           'gene_symbol_separator')

      # assign new user selections
      # NOTE: there are fields in `new_parameters` that aren't updated here,
      # which means you can't easily forgo the for loop for an apply equivalent
      for (label in assignment_labels) {
        for (param in parameter_names) {
          input_value <- input[[paste0(current_label, '_', param)]]

          # Keep stored values when inputs are absent (first paint, UI rebuild) or when
          # the field has no widget (e.g. id_mapping_* stats from processing). Otherwise
          # NULL overwrites break gene symbol / ID mapping persistence.
          if (is.null(input_value) && param %in% c(
                "gene_symbol_column",
                "convert_ids_to_gene_symbol",
                "id_source_column",
                "id_mapping_species",
                "id_mapping_keytype",
                "id_mapping_n_total",
                "id_mapping_n_unmapped")) {
            next
          }

          # Convert intensity_data checkbox boolean to "Yes"/"No" string
          if (param == "intensity_data" && is.logical(input_value)) {
            input_value <- if (input_value) "Yes" else "No"
          }
          if (param == "sample_filter_column" && is.null(input_value)) {
            input_value <- ""
          }
          if (param == "sample_filter_values" && is.null(input_value)) {
            input_value <- character(0)
          }
          if (param == "row_filter_column" && is.null(input_value)) {
            input_value <- ""
          }
          if (param == "row_filter_values" && is.null(input_value)) {
            input_value <- character(0)
          }
          if (param == "id_source_column" && is.null(input_value)) {
            input_value <- ""
          }

          new_parameters[[label]][[param]] <- input_value
        }
      }

      # assign reactiveVal
      parameters_internal_reactive(new_parameters)
    }
    
    # label assignment, has to be used in separate observeEvent() calls
    labelAssignment <- function() {
      output$sideBarMain <- renderUI({labelSetupUI(ns = ns,
                                                   gctFileNames = accumulated_files()$name)})
      output$rightButton <- renderUI({actionButton(ns("submitLabelsButton"), 
                                                   "Submit",
                                                   class = "btn btn-primary")})
      output$leftButton <- NULL
      
      # update with saved labels if they exist
      lapply(names(parameters_internal_reactive()), function(label) {
        filename <- parameters_internal_reactive()[[label]]$gct_file_name
        updateTextInput(inputId = paste0('Label_', filename), value = label)
      })
    }
    

    # CSV/Excel/TSV workflow function - starts with label assignment (same as GCT workflow)
    csvExcelWorkflow <- function() {
      csvExcelLabelAssignment()
    }
    
    # Label assignment for CSV/Excel/TSV files (same pattern as GCT workflow)
    csvExcelLabelAssignment <- function() {
      setCsvStepIndicator(1, 4, "Assign Labels & Preprocessing")
      output$sideBarMain <- renderUI({csvExcelLabelSetupUI(ns = ns,
                                                           dataFileNames = accumulated_files()$name)})
      output$rightButton <- renderUI({actionButton(ns("submitCSVExcelLabelsButton"),
                                                   "Next",
                                                   class = "btn btn-primary")})
      output$leftButton <- NULL

      # Update with saved labels if they exist (same as GCT workflow)
      lapply(names(parameters_internal_reactive()), function(label) {
        filename <- parameters_internal_reactive()[[label]]$gct_file_name
        updateTextInput(inputId = paste0('CSVExcelLabel_', filename), value = label)
      })

      # Populate source column dropdowns and register per-file condition setup observers.
      # Observers are registered once per session (guard against duplicate registration
      # on back-navigation, Bug 6).
      lapply(seq_len(nrow(accumulated_files())), function(i) {
        local({
          f <- accumulated_files()$name[i]
          file_id <- gsub("[^a-zA-Z0-9_]", "_", f)
          file_path <- accumulated_files()$datapath[i]
          file_ext <- tools::file_ext(tolower(f))

          # Populate source column dropdown
          cols <- tryCatch({
            preview <- read_uploaded_data_preview(file_path, file_ext, n_max = 1)
            if (!is.null(preview)) names(preview) else character(0)
          }, error = function(e) character(0))

          if (length(cols) > 0) {
            updateSelectInput(session, paste0("id_source_column_", file_id), choices = cols, selected = cols[1])
          }

          # Helper: update the data preview for this file using an optionally processed data frame
          update_file_preview <- function(df) {
            all_data <- preview_all_data()
            all_data[[f]] <- df
            preview_all_data(all_data)
            # Update live preview if this file is currently shown
            currently_shown <- input$previewFileSelect
            if (is.null(currently_shown) || currently_shown == f) {
              preview_data_reactive(df)
            }
          }

          # Only register per-file observers once to prevent duplicate firings
          observer_key <- paste0("conditionObserver_registered_", file_id)
          if (!isTRUE(condition_file_observers_registered()[[observer_key]])) {
            registered <- condition_file_observers_registered()
            registered[[observer_key]] <- TRUE
            condition_file_observers_registered(registered)

            # --- Split identifier live preview ---
            # Reactive that computes the split preview for this file
            split_preview_data <- reactive({
              do_split <- isTRUE(input[[paste0("delimit_id_", file_id)]])
              raw <- tryCatch(
                read_uploaded_data_preview(file_path, file_ext, n_max = 20),
                error = function(e) NULL
              )
              if (is.null(raw) || !do_split) return(raw)
              src_col <- input[[paste0("id_source_column_", file_id)]]
              sep_val <- input[[paste0("id_separator_", file_id)]]
              if (is.null(src_col) || !src_col %in% names(raw)) return(raw)
              sep_val <- if (!is.null(sep_val) && nchar(trimws(sep_val)) > 0) sep_val else ";"
              tryCatch(extract_protigy_id(raw, src_col, sep_val), error = function(e) raw)
            })

            observeEvent(
              list(
                input[[paste0("delimit_id_", file_id)]],
                input[[paste0("id_source_column_", file_id)]],
                input[[paste0("id_separator_", file_id)]]
              ),
              {
                df <- split_preview_data()
                if (!is.null(df)) update_file_preview(df)
              },
              ignoreInit = TRUE
            )

            # --- Condition setup file upload ---
            input_id <- paste0("conditionSetupFile_", file_id)
            observeEvent(input[[input_id]], {
              req(input[[input_id]])
              tryCatch({
                cond_data <- read_spectronaut_condition_setup(input[[input_id]]$datapath)

                # Store condition data keyed by file name (label resolved at submit)
                current_mappings <- condition_setup_mappings()
                current_mappings[[f]] <- cond_data
                condition_setup_mappings(current_mappings)

                # Detect suffixes from this file's columns
                all_cols <- tryCatch({
                  preview <- read_uploaded_data_preview(file_path, file_ext, n_max = 1)
                  if (!is.null(preview)) names(preview) else character(0)
                }, error = function(e) character(0))

                run_labels <- cond_data[["Run Label"]]
                suffixes <- detect_quant_suffixes(all_cols, run_labels)

                output[[paste0("conditionSuffixUI_", file_id)]] <- renderUI({
                  if (length(suffixes) == 0) {
                    p("No quantification suffixes detected. Check that run labels match column names.",
                      style = "color: orange;")
                  } else {
                    selectInput(
                      ns(paste0("conditionSuffix_", file_id)),
                      "Quantification metric:",
                      choices = suffixes,
                      selected = suffixes[1]
                    )
                  }
                })

                # Live preview: apply condition setup rename to a small preview
                if (length(suffixes) > 0) {
                  tryCatch({
                    raw <- read_uploaded_data_preview(file_path, file_ext, n_max = 20)
                    merge_cr <- isTRUE(input[[paste0("merge_cond_rep_", file_id)]])
                    renamed <- withCallingHandlers(
                      apply_spectronaut_condition_setup(raw, cond_data, suffixes[1], merge_cr),
                      warning = function(w) invokeRestart("muffleWarning")
                    )
                    update_file_preview(renamed)
                  }, error = function(e) NULL)
                }
              }, error = function(e) {
                showNotification(
                  HTML(paste0("<b>Error reading condition setup file:</b><br>", e$message)),
                  type = "error", duration = NULL, closeButton = TRUE
                )
              })
            }, ignoreInit = TRUE)

            # --- Re-apply condition preview when suffix or merge changes ---
            observeEvent(
              list(
                input[[paste0("conditionSuffix_", file_id)]],
                input[[paste0("merge_cond_rep_", file_id)]]
              ),
              {
                cond_data <- condition_setup_mappings()[[f]]
                sel_suffix <- input[[paste0("conditionSuffix_", file_id)]]
                if (is.null(cond_data) || is.null(sel_suffix)) return()
                tryCatch({
                  raw <- read_uploaded_data_preview(file_path, file_ext, n_max = 20)
                  merge_cr <- isTRUE(input[[paste0("merge_cond_rep_", file_id)]])
                  renamed <- withCallingHandlers(
                    apply_spectronaut_condition_setup(raw, cond_data, sel_suffix, merge_cr),
                    warning = function(w) invokeRestart("muffleWarning")
                  )
                  update_file_preview(renamed)
                }, error = function(e) NULL)
              },
              ignoreInit = TRUE
            )
          }
        })
      })
    }

    # Handle CSV/Excel/TSV label submission
    observeEvent(input$submitCSVExcelLabelsButton, {
      # Collect labels from input fields
      labels <- sapply(accumulated_files()$name, function(file) {
        input[[paste0('CSVExcelLabel_', file)]]
      })
      
      # Validate labels
      if (any(labels == "")) {
        showNotification(
          ui = HTML(paste0("<b>Error</b><br>", "Please provide labels for all files.")),
          type = "error",
          duration = NULL,
          closeButton = TRUE
        )
        return()
      }
      
      # Check for duplicate labels
      if (length(unique(labels)) != length(labels)) {
        showNotification(
          ui = HTML(paste0("<b>Error</b><br>", "Please provide unique labels for each file.")),
          type = "error",
          duration = NULL,
          closeButton = TRUE
        )
        return()
      }
      
      # Store labels in parameters_internal_reactive (same as GCT workflow)
      for (i in seq_along(labels)) {
        filename <- accumulated_files()$name[i]
        label <- labels[i]
        parameters_internal_reactive(c(
          parameters_internal_reactive(),
          setNames(list(list(gct_file_name = filename)), label)
        ))
      }

      # Collect per-file flags: delimit_id and use_condition_setup
      deliminate_flags <- setNames(
        sapply(accumulated_files()$name, function(f) {
          file_id <- gsub("[^a-zA-Z0-9_]", "_", f)
          isTRUE(input[[paste0("delimit_id_", file_id)]])
        }),
        accumulated_files()$name
      )
      condition_flags <- setNames(
        sapply(accumulated_files()$name, function(f) isTRUE(input[[paste0("use_condition_setup_", f)]])),
        accumulated_files()$name
      )

      # Mark is_spectronaut for any file with either feature enabled
      spectronaut_flags <- setNames(
        deliminate_flags | condition_flags,
        accumulated_files()$name
      )
      is_spectronaut_reactive(spectronaut_flags)

      # Inline preprocessing: apply extract_protigy_id and/or condition setup per file
      n_to_process <- sum(deliminate_flags | condition_flags)
      if (n_to_process > 0) {
        shinyjs::disable("submitCSVExcelLabelsButton")
        on.exit(shinyjs::enable("submitCSVExcelLabelsButton"), add = TRUE)
      }
      processed <- list()
      new_condition_mappings <- list()
      raw_col_names <- list()
      preprocess_error <- FALSE
      withProgress(message = "Preprocessing files...", value = 0, {
      for (i in seq_along(labels)) {
        f <- accumulated_files()$name[i]
        file_id <- gsub("[^a-zA-Z0-9_]", "_", f)
        lbl <- labels[i]
        file_path <- accumulated_files()$datapath[i]
        file_ext <- tools::file_ext(tolower(f))

        do_deliminate <- isTRUE(deliminate_flags[f])
        do_condition <- isTRUE(condition_flags[f])

        if (!do_deliminate && !do_condition) next

        data <- tryCatch(
          read_uploaded_data_preview(file_path, file_ext, n_max = Inf),
          error = function(e) {
            showNotification(
              HTML(paste0("<b>Error reading ", f, ":</b><br>", e$message)),
              type = "error", duration = NULL, closeButton = TRUE
            )
            NULL
          }
        )
        if (is.null(data)) next

        if (do_deliminate) {
          src_col <- input[[paste0("id_source_column_", file_id)]]
          sep_val <- input[[paste0("id_separator_", file_id)]]
          if (!is.null(src_col) && src_col %in% names(data)) {
            sep_val <- if (!is.null(sep_val) && nchar(trimws(sep_val)) > 0) sep_val else ";"
            data <- extract_protigy_id(data, src_col, sep_val)
          }
        }

        # Capture column names before Spectronaut renaming (but after split-id which adds protigy_id)
        pre_condition_names <- names(data)

        if (do_condition) {
          # Condition data is stored by file name (f) in condition_setup_mappings
          cond_data <- condition_setup_mappings()[[f]]
          sel_suffix <- input[[paste0("conditionSuffix_", file_id)]]
          merge_cr <- isTRUE(input[[paste0("merge_cond_rep_", file_id)]])
          if (is.null(cond_data)) {
            showNotification(
              HTML(paste0("<b>Error: Missing condition setup for ", lbl, "</b><br>",
                          "Please upload a condition setup file before proceeding.")),
              type = "error", duration = NULL, closeButton = TRUE
            )
            preprocess_error <- TRUE
            break
          }
          if (is.null(sel_suffix)) {
            showNotification(
              HTML(paste0("<b>Error: No quantification metric selected for ", lbl, "</b><br>",
                          "The suffix dropdown may still be loading. Please wait and try again.")),
              type = "error", duration = NULL, closeButton = TRUE
            )
            preprocess_error <- TRUE
            break
          }
          if (!is.null(cond_data) && !is.null(sel_suffix)) {
            data <- tryCatch(
              withCallingHandlers(
                apply_spectronaut_condition_setup(data, cond_data, sel_suffix, merge_cr),
                warning = function(w) {
                  if (inherits(w, "replicateNAWarning")) {
                    showNotification(
                      HTML(paste0("<b>Warning: Replicate column issue</b><br>", conditionMessage(w))),
                      type = "warning", duration = NULL, closeButton = TRUE
                    )
                    invokeRestart("muffleWarning")
                  }
                }
              ),
              error = function(e) {
                showNotification(
                  HTML(paste0("<b>Error applying condition setup for ", lbl, ":</b><br>", e$message)),
                  type = "error", duration = NULL, closeButton = TRUE
                )
                data
              }
            )
            # Store the condition->column mapping for sample_annotation auto-population
            # Include original run labels so sample_annotation shows traceable names
            exp_design_from_condition <- tryCatch(
              buildExpDesignFromConditionSetup(cond_data, merge_cr),
              error = function(e) NULL
            )
            if (!is.null(exp_design_from_condition)) {
              exp_design_from_condition$original_run_label <- cond_data[["Run Label"]]
              new_condition_mappings[[lbl]] <- exp_design_from_condition
            }
          }
        }

        # Build mapping: post-processing column name -> original raw column name.
        # Columns that survived processing are matched by position against pre-condition names.
        # Columns dropped by Spectronaut won't appear; columns unchanged keep their raw name.
        post_names <- names(data)
        # For columns that existed before condition rename, build the map.
        # apply_spectronaut_condition_setup renames in-place and drops columns,
        # but does NOT reorder. Surviving columns are a subset in original order,
        # so we can reconstruct the map by tracking which raw columns survived.
        # Columns not renamed (non-run columns) have the same name in both.
        # Columns renamed have different names but same positional slot.
        survived_raw <- pre_condition_names[pre_condition_names %in% pre_condition_names]
        # Build by matching: for each column in post_names, find its raw predecessor.
        # Since apply_spectronaut_condition_setup only renames+drops (no reorder),
        # the surviving columns maintain order. We can match by finding which raw columns
        # are still present (same name) and which were renamed (different name, same position).
        raw_map <- character(0)
        if (do_condition) {
          # Columns that weren't renamed: same name in both pre and post
          unchanged <- intersect(pre_condition_names, post_names)
          raw_map <- setNames(unchanged, unchanged)
          # Columns that were renamed: in pre but not in post by name
          # The rename_map from condition setup maps old_raw_col -> new_col
          # Reconstruct it from the condition setup data
          if (!is.null(cond_data) && !is.null(sel_suffix)) {
            exp_d <- tryCatch(buildExpDesignFromConditionSetup(cond_data, merge_cr),
                              error = function(e) NULL)
            if (!is.null(exp_d)) {
              run_labels <- cond_data[["Run Label"]]
              for (j in seq_along(run_labels)) {
                # Find the raw column name that matches this run label + suffix
                pattern <- paste0(run_labels[j], sel_suffix)
                raw_col <- pre_condition_names[pre_condition_names == pattern]
                if (length(raw_col) == 0) {
                  raw_col <- pre_condition_names[endsWith(pre_condition_names, pattern)]
                }
                if (length(raw_col) >= 1 && exp_d$columnName[j] %in% post_names) {
                  raw_map[exp_d$columnName[j]] <- raw_col[1]
                }
              }
            }
          }
        } else {
          # No condition rename — raw names are the post names
          raw_map <- setNames(post_names, post_names)
        }
        raw_col_names[[lbl]] <- raw_map

        processed[[lbl]] <- data
        setProgress(i / length(labels), detail = paste("Processed", lbl))
      }
      }) # end withProgress

      # Abort if any preprocessing step failed — do not mutate downstream state
      if (preprocess_error) return()

      pristine <- if (length(processed) > 0) processed else NULL
      spectronaut_processed_data(pristine)
      spectronaut_processed_data_pristine(pristine)
      condition_setup_mappings(new_condition_mappings)
      raw_column_names_per_label(raw_col_names)
      # Clear stored identifier columns — preprocessing may have changed available columns (Bug 8)
      csvExcel_identifier_columns_reactive(NULL)

      # Update data preview to reflect preprocessed data (keyed by label → filename)
      if (length(processed) > 0) {
        all_previews <- preview_all_data()
        for (i in seq_along(labels)) {
          lbl <- labels[i]
          fname <- accumulated_files()$name[i]
          if (!is.null(processed[[lbl]])) {
            # Show first 20 rows in preview
            all_previews[[fname]] <- head(processed[[lbl]], 20)
          }
        }
        preview_all_data(all_previews)
        # Refresh the currently displayed preview
        currently_shown <- input$previewFileSelect
        first_file <- accumulated_files()$name[1]
        show_file <- if (!is.null(currently_shown) && currently_shown %in% names(all_previews)) {
          currently_shown
        } else {
          first_file
        }
        preview_data_reactive(all_previews[[show_file]])
      }

      csvExcelIdentifierSelection(labels)
    })

    # Identifier column selection step
    csvExcelIdentifierSelection <- function(labels) {
      setCsvStepIndicator(2, 4, "Select Identifier Column")
      output$sideBarMain <- renderUI({csvExcelIdentifierSetupUI(ns = ns,
                                                               dataFiles = accumulated_files(),
                                                               labels = labels,
                                                               preprocessed_data = spectronaut_processed_data())})
      output$rightButton <- renderUI({actionButton(ns("submitCSVExcelIdentifiersButton"), 
                                                   "Next",
                                                   class = "btn btn-primary")})
      output$leftButton <- renderUI({actionButton(ns("backToCSVExcelLabelsButton"), 
                                                  "Back",
                                                  icon = icon("chevron-left"),
                                                  class = "btn btn-default")})
      
      # Update with saved identifier columns if they exist
      stored_identifiers <- csvExcel_identifier_columns_reactive()
      if (!is.null(stored_identifiers)) {
        for (i in seq_along(stored_identifiers)) {
          updateSelectInput(inputId = paste0("identifierColumn_", i), 
                           selected = stored_identifiers[i])
        }
      }
    }
    
    # Handle identifier column submission
    observeEvent(input$submitCSVExcelIdentifiersButton, {
      # Collect identifier columns for each file
      # Use lapply (not sapply) to preserve NULLs when a selectInput was never rendered
      identifier_columns <- lapply(seq_len(nrow(accumulated_files())), function(i) {
        input[[paste0("identifierColumn_", i)]]
      })

      # Validate identifier columns
      if (any(sapply(identifier_columns, is.null)) || any(identifier_columns == "")) {
        showNotification(
          ui = HTML(paste0("<b>Error</b><br>", "Please select identifier columns for all datasets.")),
          type = "error",
          duration = NULL,
          closeButton = TRUE
        )
        return()
      }

      # Convert to character vector now that NULLs are confirmed absent
      identifier_columns <- unlist(identifier_columns)

      # Store identifier columns for later retrieval
      csvExcel_identifier_columns_reactive(identifier_columns)

      labels <- sapply(accumulated_files()$name, function(file) {
        input[[paste0('CSVExcelLabel_', file)]]
      })

      csvExcelExpDesignSetup(identifier_columns, labels)
    })

    # Experimental design setup step
    csvExcelExpDesignSetup <- function(identifier_columns, labels) {
      # Build sample names: prefer preprocessed data (Spectronaut), else raw file
      # Include all columns (including identifier) so the full data shape is visible
      preprocessed <- spectronaut_processed_data()
      all_samples <- c()
      for (i in seq_len(nrow(accumulated_files()))) {
        lbl       <- labels[i]
        file_ext  <- tools::file_ext(tolower(accumulated_files()$name[i]))
        file_path <- accumulated_files()$datapath[i]
        if (!is.null(preprocessed) && !is.null(preprocessed[[lbl]])) {
          all_samples <- c(all_samples, names(preprocessed[[lbl]]))
        } else if (file_ext == "csv") {
          data <- readr::read_csv(file_path, n_max = 1, show_col_types = FALSE)
          all_samples <- c(all_samples, names(data))
        } else if (file_ext == "tsv") {
          data <- readr::read_tsv(file_path, n_max = 1, show_col_types = FALSE)
          all_samples <- c(all_samples, names(data))
        } else if (file_ext %in% c("xlsx", "xls")) {
          data <- readxl::read_excel(file_path, n_max = 1)
          all_samples <- c(all_samples, names(data))
        }
      }
      # Warn if the same column name appears in multiple files — unique() would silently
      # merge them into one exp design row (Bug 10)
      if (length(all_samples) > 0 && anyDuplicated(all_samples) > 0) {
        dup_names <- unique(all_samples[duplicated(all_samples)])
        dup_display <- if (length(dup_names) <= 5) {
          paste(dup_names, collapse = ", ")
        } else {
          paste0(paste(head(dup_names, 5), collapse = ", "),
                 ", \u2026 and ", length(dup_names) - 5, " more")
        }
        showNotification(
          HTML(paste0(
            "<b>Warning: Duplicate column names across files</b><br>",
            length(dup_names), " column name(s) appear in more than one uploaded file: ",
            dup_display,
            ". They will share a single row in the experimental design. ",
            "If these represent different samples, rename the columns in the source files."
          )),
          type = "warning", duration = NULL, closeButton = TRUE
        )
      }
      sample_names <- if (length(all_samples) > 0) unique(all_samples) else template_sample_names()

      # Build initial experimental design data frame
      # If condition setup mappings exist, auto-populate sample_annotation
      cond_mappings <- condition_setup_mappings()
      has_condition_mappings <- length(cond_mappings) > 0

      # Merge all condition mappings into a lookup: renamed_column -> original_run_label
      # After condition setup, data columns are already renamed (columnName = new names).
      # sample_annotation should show the original run labels for traceability.
      annotation_lookup <- character(0)
      if (has_condition_mappings) {
        for (lbl in names(cond_mappings)) {
          mapping_df <- cond_mappings[[lbl]]
          if (is.data.frame(mapping_df) && "columnName" %in% names(mapping_df) &&
              "original_run_label" %in% names(mapping_df)) {
            new_names <- setNames(mapping_df$original_run_label, mapping_df$columnName)
            annotation_lookup <- c(annotation_lookup, new_names)
          }
        }
      }

      base_df <- data.frame(
        columnName = sample_names,
        stringsAsFactors = FALSE
      )

      # Insert sample_annotation as 2nd column if condition mappings exist
      if (has_condition_mappings) {
        base_df$sample_annotation <- annotation_lookup[sample_names]
      }

      base_df$Condition <- rep(NA_character_, length(sample_names))
      base_df$Replicate <- rep(NA_character_, length(sample_names))

      exp_design_df(base_df)

      show_exp_design_panel(TRUE)
      setCsvStepIndicator(3, 4, "Experimental Design")
      output$sideBarMain <- renderUI({
        df <- exp_design_df()
        if (!is.null(df) && ncol(df) > 1) {
          actionButton(ns("processCSVExcel"), "Process Files",
                       class = "btn btn-primary btn-block",
                       style = "width: 80%;")
        } else {
          p(style = "color:#888; font-size:0.9em;",
            "Fill in the Experimental Design table, then click Process Files.")
        }
      })
      output$rightButton <- NULL
      output$leftButton <- renderUI({actionButton(ns("backToCSVExcelIdentifiersButton"),
                                                  "Back",
                                                  icon = icon("chevron-left"),
                                                  class = "btn btn-default")})

      # Auto-check rename checkbox and show guidance when condition mappings were provided (UX 4)
      if (has_condition_mappings) {
        updateCheckboxInput(session, "rename_to_sample_annotation", value = TRUE)
        showNotification(
          HTML(paste0(
            "<b>Condition setup detected</b><br>",
            "Your condition setup file provided sample names. ",
            "The <em>sample_annotation</em> column shows original run labels for traceability. ",
            "\"Use friendly sample names\" has been enabled automatically."
          )),
          type = "message", duration = 8, closeButton = TRUE
        )
      }
    }

    # Helper: render step indicator for CSV/Excel workflow (UX 6)
    setCsvStepIndicator <- function(step, total = 4, label = "") {
      output$csvStepIndicator <- renderUI({
        tags$p(
          style = paste0(
            "color:#888; font-size:0.82em; margin:0 0 4px 0; ",
            "border-bottom:1px solid #ddd; padding-bottom:4px;"
          ),
          paste0("Step ", step, " of ", total, ": ", label)
        )
      })
    }
    clearCsvStepIndicator <- function() {
      output$csvStepIndicator <- renderUI(NULL)
    }

    # Data preview table output
    output$dataPreviewTable <- DT::renderDT({
      req(preview_data_reactive())
      DT::datatable(
        preview_data_reactive(),
        options = list(scrollX = TRUE, pageLength = 20, dom = 'tip'),
        rownames = FALSE
      )
    })
    output$showDataPreview <- reactive({ !is.null(preview_data_reactive()) })
    outputOptions(output, "showDataPreview", suspendWhenHidden = FALSE)

    output$showExpDesignPanel <- reactive({ isTRUE(show_exp_design_panel()) })
    outputOptions(output, "showExpDesignPanel", suspendWhenHidden = FALSE)

    output$expDesignPanelContent <- renderUI({
      req(show_exp_design_panel())
      csvExcelExpDesignSetupUI(ns = ns)
    })

    # Update preview when files are first uploaded — load all files for multi-file selector
    observeEvent(accumulated_files(), {
      req(accumulated_files())
      all_previews <- list()
      for (i in seq_len(nrow(accumulated_files()))) {
        f <- accumulated_files()[i, ]
        file_ext <- tools::file_ext(tolower(f$name))
        all_previews[[f$name]] <- tryCatch(
          read_uploaded_data_preview(f$datapath, file_ext),
          error = function(e) NULL
        )
      }
      preview_all_data(all_previews)
      # Show first file by default
      first_name <- accumulated_files()$name[1]
      preview_data_reactive(all_previews[[first_name]])
    }, ignoreNULL = TRUE)

    # Render file selector for data preview (only when >1 file uploaded)
    output$previewFileSelector <- renderUI({
      files <- accumulated_files()
      if (is.null(files) || nrow(files) <= 1) return(NULL)
      selectInput(ns("previewFileSelect"), label = "Preview file:",
                  choices = files$name, selected = files$name[1],
                  width = "100%")
    })

    # Switch preview when user selects a different file
    observeEvent(input$previewFileSelect, {
      req(input$previewFileSelect)
      all_data <- preview_all_data()
      sel <- input$previewFileSelect
      if (sel %in% names(all_data)) {
        preview_data_reactive(all_data[[sel]])
      }
    })

    # Handle back navigation
    observeEvent(input$backToCSVExcelLabelsButton, {
      csvExcelLabelAssignment()
    })

    observeEvent(input$backToCSVExcelIdentifiersButton, {
      show_exp_design_panel(FALSE)
      labels <- sapply(accumulated_files()$name, function(file) {
        input[[paste0('CSVExcelLabel_', file)]]
      })
      csvExcelIdentifierSelection(labels)
    })

    ### INLINE EXPERIMENTAL DESIGN TABLE ###

    # Render editable rhandsontable (supports clipboard paste and fill-down)
    output$exp_design_table <- rhandsontable::renderRHandsontable({
      req(exp_design_df())
      df <- exp_design_df()

      max_chars  <- max(nchar(as.character(df[[1]])), na.rm = TRUE)
      col1_width <- max(120, min(300, max_chars * 8 + 20))

      # Build custom context menu: standard edit items + "Remove this column" on header right-click
      ns_prefix <- ns("")
      ht <- rhandsontable::rhandsontable(
        df,
        rowHeaders  = NULL,
        useTypes    = FALSE,
        stretchH    = "last",
        width       = "100%",
        colWidths   = c(col1_width, rep(100, ncol(df) - 1))
      )
      # Set contextMenu directly on the widget to avoid hot_table's broken && check
      # (hot_table does `if (!is.null(contextMenu) && contextMenu)` which fails for lists)
      ht$x$contextMenu <- list(
        items = list(
          undo = list(name = "Undo"),
          redo = list(name = "Redo"),
          sep1 = list(name = "---------"),
          copy = list(name = "Copy"),
          cut  = list(name = "Cut"),
          sep2 = list(name = "---------"),
          remove_col = list(
            name = "Remove this column",
            disabled = htmlwidgets::JS(paste0(
              "function() {",
              "  var sel = this.getSelectedRangeLast();",
              "  if (!sel) return true;",
              "  var col = sel.from.col;",
              "  var colName = this.getColHeader(col);",
              "  return colName === 'columnName';",
              "}"
            )),
            callback = htmlwidgets::JS(paste0(
              "function(key, selection) {",
              "  var col = selection[0].start.col;",
              "  var colName = this.getColHeader(col);",
              "  if (colName !== 'columnName') {",
              "    Shiny.setInputValue('", ns_prefix, "remove_col_by_name', ",
              "      {name: colName, ts: Date.now()});",
              "  }",
              "}"
            ))
          )
        )
      )

      # Make first column (columnName) read-only with gray background
      # Suppress "column types were previously not defined" warning — benign when useTypes = FALSE
      suppressWarnings(
        ht <- rhandsontable::hot_col(ht, col = 1, readOnly = TRUE,
                                     renderer = "function(instance, td, row, col, prop, value, cellProperties) {
                                       Handsontable.renderers.TextRenderer.apply(this, arguments);
                                       td.style.background = '#f5f5f5';
                                       td.style.color = '#666';
                                       return td;
                                     }")
      )
      ht
    })

    # Sync table edits back to exp_design_df (entire table sent on every change)
    observeEvent(input$exp_design_table, {
      req(input$exp_design_table)
      new_df <- rhandsontable::hot_to_r(input$exp_design_table)
      # Protect columnName from user edits (read-only in widget, belt-and-suspenders).
      # Only apply if row counts match — a mismatch means the widget is stale from a
      # prior render cycle (e.g. after navigating Back and re-entering this step) and
      # the update should be ignored to avoid a replacement-length crash.
      current <- isolate(exp_design_df())
      if (!is.null(current) && nrow(new_df) == nrow(current)) {
        new_df[[1]] <- current[[1]]
        exp_design_df(new_df)
      }
    }, ignoreInit = TRUE)

    # Live preview: whenever exp_design_df changes and rename is active, rebuild the
    # preview with up-to-date column names derived from the current sample_annotation values.
    observeEvent(exp_design_df(), {
      df <- exp_design_df()
      if (is.null(df)) return()
      if (!isTRUE(input$rename_to_sample_annotation)) return()
      if (!"sample_annotation" %in% names(df)) return()

      # Build rename map: columnName -> sample_annotation (non-empty rows only)
      rename_rows <- df[!is.na(df$sample_annotation) &
                          trimws(as.character(df$sample_annotation)) != "", ]
      if (nrow(rename_rows) == 0) return()

      new_names_vec <- as.character(rename_rows$sample_annotation)
      old_names_vec <- as.character(rename_rows$columnName)
      if (anyDuplicated(new_names_vec) > 0) return()  # skip update on invalid state

      rename_map <- setNames(new_names_vec, old_names_vec)

      # Apply rename from pristine processed data so edits never compound
      pristine <- spectronaut_processed_data_pristine()
      if (is.null(pristine)) return()

      updated <- lapply(pristine, function(d) {
        col_names <- names(d)
        for (old_nm in names(rename_map)) {
          idx <- which(col_names == old_nm)
          if (length(idx) > 0) col_names[idx] <- rename_map[[old_nm]]
        }
        names(d) <- col_names
        d
      })

      # Update the live preview (first 20 rows of each file)
      all_data <- preview_all_data()
      fnames <- accumulated_files()$name
      labels <- sapply(fnames, function(f) input[[paste0("CSVExcelLabel_", f)]])
      for (i in seq_along(fnames)) {
        lbl <- labels[i]
        if (!is.null(updated[[lbl]])) {
          all_data[[fnames[i]]] <- head(updated[[lbl]], 20)
        }
      }
      preview_all_data(all_data)
      sel_file <- input$previewFileSelect
      if (!is.null(sel_file) && sel_file %in% names(all_data)) {
        preview_data_reactive(all_data[[sel_file]])
      } else if (length(all_data) > 0) {
        preview_data_reactive(all_data[[1]])
      }
    }, ignoreInit = TRUE)

    # Add factor column
    observeEvent(input$add_factor_col, {
      req(nchar(trimws(input$new_factor_name)) > 0)
      col_name <- trimws(input$new_factor_name)
      df <- exp_design_df()
      if (!is.null(df)) {
        if (col_name %in% names(df)) {
          showNotification(
            paste0("A column named \"", col_name, "\" already exists."),
            type = "warning", duration = 4, closeButton = TRUE
          )
        } else {
          df[[col_name]] <- NA_character_
          exp_design_df(df)
          updateTextInput(session, "new_factor_name", value = "")
        }
      }
    })

    # Remove column via right-click context menu on column header
    observeEvent(input$remove_col_by_name, {
      col_name <- input$remove_col_by_name$name
      df <- exp_design_df()
      if (!is.null(df) && col_name %in% names(df) && col_name != "columnName") {
        df[[col_name]] <- NULL
        exp_design_df(df)
      }
    })

    # CSV upload populates the inline table and optionally renames columns
    observeEvent(input$expDesignFile, {
      req(input$expDesignFile)
      tryCatch({
        # Warn user if inline edits exist and will be overwritten (UX 7)
        existing_df <- exp_design_df()
        has_user_edits <- !is.null(existing_df) && (
          any(!is.na(existing_df$Condition) & trimws(as.character(existing_df$Condition)) != "") ||
          any(!is.na(existing_df$Replicate) & trimws(as.character(existing_df$Replicate)) != "")
        )
        if (has_user_edits) {
          showNotification(
            HTML("<b>Note:</b> Your inline table edits have been replaced by the uploaded file."),
            type = "warning", duration = 5, closeButton = TRUE
          )
        }

        uploaded <- as.data.frame(readExperimentalDesign(input$expDesignFile$datapath))
        exp_design_df(uploaded)

        # Apply column renaming if checkbox is checked and sample_annotation column has values
        should_rename <- isTRUE(input$rename_to_sample_annotation)
        has_annotation <- "sample_annotation" %in% names(uploaded)
        if (should_rename && has_annotation) {
          # Build rename map: columnName -> sample_annotation where annotation is non-NA/non-empty
          rename_rows <- uploaded[!is.na(uploaded$sample_annotation) &
                                    trimws(as.character(uploaded$sample_annotation)) != "", ]
          if (nrow(rename_rows) > 0) {
            new_names_vec <- as.character(rename_rows$sample_annotation)
            old_names_vec <- as.character(rename_rows$columnName)

            # Validate: new names must be unique (Bug 3 - duplicate annotation values)
            duplicate_new_names <- new_names_vec[duplicated(new_names_vec)]
            if (length(duplicate_new_names) > 0) {
              showNotification(
                HTML(paste0("<b>Error: Duplicate sample_annotation values</b><br>",
                            "The following names appear more than once in sample_annotation: ",
                            paste(unique(duplicate_new_names), collapse = ", "),
                            ". Each sample must have a unique name.")),
                type = "error", duration = NULL, closeButton = TRUE
              )
              return()
            }

            new_rename_map <- setNames(new_names_vec, old_names_vec)
            column_rename_map(new_rename_map)

            # Always apply rename from the PRISTINE copy to prevent double-renaming (Bug 2)
            pristine <- spectronaut_processed_data_pristine()
            if (!is.null(pristine)) {
              updated_processed <- lapply(pristine, function(df) {
                new_col_names <- names(df)
                for (old_name in names(new_rename_map)) {
                  idx <- which(new_col_names == old_name)
                  if (length(idx) > 0) new_col_names[idx] <- new_rename_map[[old_name]]
                }
                names(df) <- new_col_names
                df
              })
              spectronaut_processed_data(updated_processed)
            }

            # Update exp_design_df columnName column to use the new names
            updated_df <- uploaded
            updated_df$columnName <- ifelse(
              !is.na(updated_df$sample_annotation) &
                trimws(as.character(updated_df$sample_annotation)) != "",
              as.character(updated_df$sample_annotation),
              as.character(updated_df$columnName)
            )
            exp_design_df(updated_df)

            # Update all preview data frames to reflect renamed columns
            all_data <- preview_all_data()
            for (fname in names(all_data)) {
              pdata <- all_data[[fname]]
              if (!is.null(pdata)) {
                preview_col_names <- names(pdata)
                for (old_name in names(new_rename_map)) {
                  idx <- which(preview_col_names == old_name)
                  if (length(idx) > 0) preview_col_names[idx] <- new_rename_map[[old_name]]
                }
                names(pdata) <- preview_col_names
                all_data[[fname]] <- pdata
              }
            }
            preview_all_data(all_data)
            # Update the currently displayed preview
            sel_file <- input$previewFileSelect
            if (!is.null(sel_file) && sel_file %in% names(all_data)) {
              preview_data_reactive(all_data[[sel_file]])
            } else if (length(all_data) > 0) {
              preview_data_reactive(all_data[[1]])
            }
          }
        } else {
          # If rename is not active, clear any previous rename map and restore pristine data
          column_rename_map(character(0))
          pristine <- spectronaut_processed_data_pristine()
          if (!is.null(pristine)) spectronaut_processed_data(pristine)
        }
      }, error = function(e) {
        showNotification(paste("Upload error:", e$message), type = "error")
      })
    })

    # Observer: toggle sample_annotation column when rename checkbox changes
    observeEvent(input$rename_to_sample_annotation, {
      df <- exp_design_df()
      if (is.null(df)) return()
      if (isTRUE(input$rename_to_sample_annotation)) {
        # Add sample_annotation as 2nd column if not already present
        if (!"sample_annotation" %in% names(df)) {
          # Try to auto-populate from condition setup mappings
          # Maps renamed column name -> original run label for traceability
          cond_mappings <- condition_setup_mappings()
          annotation_vals <- rep(NA_character_, nrow(df))
          if (length(cond_mappings) > 0) {
            annotation_lookup <- character(0)
            for (lbl in names(cond_mappings)) {
              mapping_df <- cond_mappings[[lbl]]
              if (is.data.frame(mapping_df) && "columnName" %in% names(mapping_df) &&
                  "original_run_label" %in% names(mapping_df)) {
                annotation_lookup <- c(annotation_lookup,
                                       setNames(mapping_df$original_run_label, mapping_df$columnName))
              }
            }
            annotation_vals <- annotation_lookup[df$columnName]
          }
          # Insert sample_annotation as 2nd column
          other_cols <- setdiff(names(df), "columnName")
          df <- cbind(
            df["columnName"],
            data.frame(sample_annotation = annotation_vals, stringsAsFactors = FALSE),
            df[other_cols]
          )
          exp_design_df(df)
        }
      } else {
        # Remove sample_annotation column if present, restore pristine processed data
        if ("sample_annotation" %in% names(df)) {
          df$sample_annotation <- NULL
          exp_design_df(df)
        }
        column_rename_map(character(0))
        pristine <- spectronaut_processed_data_pristine()
        if (!is.null(pristine)) spectronaut_processed_data(pristine)
      }
    }, ignoreInit = TRUE)

    # Create a reactive to store sample names for template generation
    template_sample_names <- reactive({
      req(accumulated_files())

        tryCatch({
        all_samples <- c()
        for (i in seq_len(nrow(accumulated_files()))) {
          file_path <- accumulated_files()$datapath[i]
          file_ext <- tools::file_ext(tolower(accumulated_files()$name[i]))
          
          if (file_ext == "csv") {
            data <- readr::read_csv(file_path, n_max = 1, show_col_types = FALSE)
            samples <- names(data)
          } else if (file_ext == "tsv") {
            data <- readr::read_tsv(file_path, n_max = 1, show_col_types = FALSE)
            samples <- names(data)
          } else if (file_ext %in% c("xlsx", "xls")) {
            data <- readxl::read_excel(file_path, n_max = 1)
            samples <- names(data)
            } else {
            samples <- c()
          }
          
          all_samples <- c(all_samples, samples)
        }
        
        # Get unique sample names
        unique(all_samples)
        }, error = function(e) {
        c("Sample1", "Sample2", "Sample3") # Fallback template
      })
    })
    
    # Download handler for experimental design template
    # Uses the current inline table state so any added columns and filled values are included
    output$downloadExpDesignTemplate <- downloadHandler(
      filename = "experimental_design_template.csv",
      content = function(file) {
        df <- exp_design_df()
        if (is.null(df)) {
          # Fallback: plain columnName template
          sample_names <- template_sample_names()
          df <- data.frame(
            columnName = sample_names,
            Condition  = rep("", length(sample_names)),
            Replicate  = rep("", length(sample_names)),
            stringsAsFactors = FALSE
          )
        }
        # Ensure sample_annotation column is present as 2nd column when rename is enabled
        if (isTRUE(input$rename_to_sample_annotation) && !"sample_annotation" %in% names(df)) {
          other_cols <- setdiff(names(df), "columnName")
          df <- cbind(
            df["columnName"],
            data.frame(sample_annotation = rep("", nrow(df)), stringsAsFactors = FALSE),
            df[other_cols]
          )
        }
        # Replace NAs with empty strings for a cleaner CSV template
        df[is.na(df)] <- ""
        readr::write_csv(df, file)
      }
    )
    
    
    

    # Process CSV/Excel data when experimental design is ready
    observeEvent(input$processCSVExcel, {
      shinyjs::runjs("$('#data-preview-content').hide(); $('#toggleDataPreview').text('Show');")
      req(exp_design_df())

      my_shinyalert_tryCatch(
        text.error = "<b>CSV/Excel Processing Error:</b>",
        append.error = TRUE,
        show.error = TRUE,
        return.error = NULL,
        expr = {
          # Process CSV/Excel files with progress indication
          withProgress(message = "Processing CSV/Excel files...", {
            setProgress(0.2, detail = "Reading experimental design")

            # Use the inline table state as the experimental design
            # Normalize blank/NA-string cells so metadata-only rows are correctly
            # excluded from sample classification. rhandsontable round-trips R NA
            # as the string "NA" (useTypes = FALSE), so we must handle both.
            exp_design <- exp_design_df()
            exp_design[exp_design == ""]   <- NA
            exp_design[exp_design == "NA"] <- NA

            # Guard: columnName column must not contain NA after normalization
            if ("columnName" %in% names(exp_design) && any(is.na(exp_design$columnName))) {
              stop("The experimental design has empty/missing values in the 'columnName' column. ",
                   "Every row must have a valid column name.")
            }

            setProgress(0.3, detail = "Applying sample annotation renames")

            # Apply sample_annotation rename at process time so inline table edits take effect.
            # Build rename map from exp_design: columnName -> sample_annotation (where both exist).
            should_rename <- isTRUE(input$rename_to_sample_annotation) &&
              "sample_annotation" %in% names(exp_design)
            if (should_rename) {
              rename_rows <- exp_design[!is.na(exp_design$sample_annotation) &
                                          trimws(as.character(exp_design$sample_annotation)) != "", ]
              if (nrow(rename_rows) > 0) {
                new_names_vec <- as.character(rename_rows$sample_annotation)
                old_names_vec <- as.character(rename_rows$columnName)

                # Validate: new names must be unique
                dup_new <- new_names_vec[duplicated(new_names_vec)]
                if (length(dup_new) > 0) {
                  stop("Duplicate sample_annotation values: ",
                       paste(unique(dup_new), collapse = ", "),
                       ". Each sample must have a unique name.")
                }

                new_rename_map <- setNames(new_names_vec, old_names_vec)
                column_rename_map(new_rename_map)

                # Rename processed data from PRISTINE to prevent double-renaming
                pristine <- spectronaut_processed_data_pristine()
                if (!is.null(pristine)) {
                  updated_processed <- lapply(pristine, function(df) {
                    col_names <- names(df)
                    for (old_nm in names(new_rename_map)) {
                      idx <- which(col_names == old_nm)
                      if (length(idx) > 0) col_names[idx] <- new_rename_map[[old_nm]]
                    }
                    names(df) <- col_names
                    df
                  })
                  spectronaut_processed_data(updated_processed)
                }

                # Update exp_design columnName to the new (friendly) names
                exp_design$columnName <- ifelse(
                  !is.na(exp_design$sample_annotation) &
                    trimws(as.character(exp_design$sample_annotation)) != "",
                  as.character(exp_design$sample_annotation),
                  as.character(exp_design$columnName)
                )

                # Update data preview to reflect renamed columns
                all_data <- preview_all_data()
                for (fname in names(all_data)) {
                  pdata <- all_data[[fname]]
                  if (!is.null(pdata)) {
                    pcols <- names(pdata)
                    for (old_nm in names(new_rename_map)) {
                      idx <- which(pcols == old_nm)
                      if (length(idx) > 0) pcols[idx] <- new_rename_map[[old_nm]]
                    }
                    names(pdata) <- pcols
                    all_data[[fname]] <- pdata
                  }
                }
                preview_all_data(all_data)
                sel_file <- input$previewFileSelect
                if (!is.null(sel_file) && sel_file %in% names(all_data)) {
                  preview_data_reactive(all_data[[sel_file]])
                } else if (length(all_data) > 0) {
                  preview_data_reactive(all_data[[1]])
                }
              }
            }

            setProgress(0.5, detail = "Converting data to analysis format")

            # Process CSV/Excel files with per-dataset identifier columns
            identifier_columns <- csvExcel_identifier_columns_reactive()

            # Validate identifier columns
            if (is.null(identifier_columns) || any(identifier_columns == "")) {
              stop("Please select identifier columns for all datasets.")
            }

            # Get labels from input fields
            labels <- sapply(accumulated_files()$name, function(file) {
              input[[paste0('CSVExcelLabel_', file)]]
            })

            # Build original_cid_map: final cid -> raw column name from the very original file.
            # Compose: (sample_annotation rename) ∘ (Spectronaut rename) → raw name.
            raw_names <- raw_column_names_per_label()  # per-label: named vec (post-spectronaut -> raw)
            sa_rename <- column_rename_map()             # named vec: old (condition name) -> new (friendly)
            original_cid_map <- character(0)
            if (length(raw_names) > 0) {
              for (lbl in names(raw_names)) {
                spectronaut_to_raw <- raw_names[[lbl]]  # named: condition_name -> raw_name
                for (cond_name in names(spectronaut_to_raw)) {
                  raw_name <- spectronaut_to_raw[[cond_name]]
                  # Check if this condition_name was further renamed by sample_annotation
                  if (cond_name %in% names(sa_rename)) {
                    final_name <- sa_rename[[cond_name]]
                  } else {
                    final_name <- cond_name
                  }
                  original_cid_map[final_name] <- raw_name
                }
              }
            }

            csv_excel_result <- processCSVExcelWorkflowWithPerDatasetIdentifiers(
              accumulated_files(), exp_design, identifier_columns, labels,
              preprocessed_data = spectronaut_processed_data(),
              original_cid_map = original_cid_map
            )

            # Surface sample mismatch warnings as in-app notifications
            for (w in csv_excel_result$warnings) {
              showNotification(HTML(w), type = "warning", duration = 15, closeButton = TRUE)
            }

            setProgress(0.8, detail = "Setting up analysis parameters")
            
            # Store converted GCT objects and parameters for later processing (same as GCT workflow)
            GCTs_unprocessed_internal_reactive(csv_excel_result$GCTs)
            parameters_internal_reactive(csv_excel_result$parameters)

            # Mark as spectronaut in each label's parameters
            if (isTRUE(any(is_spectronaut_reactive()))) {
              params <- parameters_internal_reactive()
              for (lbl in names(params)) {
                params[[lbl]]$is_spectronaut <- TRUE
              }
              parameters_internal_reactive(params)
            }
            
            # Set up back/next navigation logic for parameter setup
            backNextLogic$place <- 1
            backNextLogic$maxPlace <- length(csv_excel_result$GCTs)
            backNextLogic$placeChanged <- backNextLogic$placeChanged + 1
            
            setProgress(1.0, detail = "Ready for parameter setup")
          })
          
          # Show success message
          showNotification(
            ui = HTML(paste0("<b>Files Converted!</b><br>", "Successfully converted ", length(csv_excel_result$GCTs),
                        " CSV/Excel file(s) to analysis format. Now configure analysis parameters...")),
            type = "message",
            duration = NULL,
            closeButton = TRUE
          )
          
          # Automatically switch to Dataset Setup help tab
          updateTabsetPanel(session = parent, 
                           inputId = "navbar-tabs", 
                           selected = "Help-Analysis")
          
          # Switch to the Dataset Setup tab within the help section
          shinyjs::runjs("
            setTimeout(function() {
              $('a[data-value=\"Dataset Setup\"]').click();
            }, 100);
          ")
          
          # Trigger the standard parameter setup workflow (same as GCT files)
          show_exp_design_panel(FALSE)
          clearCsvStepIndicator()
          labelsGO(labelsGO() + 1)
        }
      )
    })
    
    # return GCTs and parameters together in one list
    return(list(GCTs_and_params = GCTs_and_params,
                globals = globals,
                GCTs_original = GCTs_original,
                column_rename_map = column_rename_map))
    
  }) # end moduleServer
} # end setupSidebarServer