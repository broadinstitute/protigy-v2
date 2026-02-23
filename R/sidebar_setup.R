################################################################################
# Module: SETUP SIDEBAR
# Main shiny functions (server and UI)
################################################################################

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
    exp_design_df                  <- reactiveVal(NULL)
    show_exp_design_panel          <- reactiveVal(FALSE)
    is_spectronaut_reactive        <- reactiveVal(list())
    spectronaut_condition_data     <- reactiveVal(NULL)
    spectronaut_processed_data     <- reactiveVal(NULL)
    preview_data_reactive          <- reactiveVal(NULL)
    spectronaut_parse_place        <- reactiveVal(1)

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
      } else {
        # GCT case: build parameters from file uploads and user-provided labels
        new_parameters <- list()
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
          }
        })
        parameters_internal_reactive(new_parameters) # update GCT parameters reactiveVal
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
                                                   GCTs = GCTs_unprocessed_internal_reactive(),
                                                   is_spectronaut = isTRUE(any(is_spectronaut_reactive())))})
        
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
          output$rightButton <- renderUI({actionButton_icon_right(
            ns("nextButton"), "Next", icon = icon("chevron-right"))})
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
    })
    
    
    # reset applyToAll to FALSE if it is not a valid option
    groups_in_all_omes <- reactive({
      base::Reduce(base::intersect, lapply(GCTs_unprocessed_internal_reactive(), function(gct) names(gct@cdesc)))
    })
    observe({
      req(parameters_internal_reactive(), groups_in_all_omes())
      
      # get relevant inputs
      current_label <- names(parameters_internal_reactive())[backNextLogic$place]
      current_annotation_column <- input[[paste0(current_label, "_annotation_column")]]
      current_group_norm_column <- input[[paste0(current_label, "_group_normalization_column")]]
      current_group_norm_selection <- input[[paste0(current_label, "_group_normalization")]]
      
      # get the groups/columns that are present in all omes
      groups_in_all <- isolate(groups_in_all_omes())
      
      # condition for when to update applyToAll to false
      # NOTE: if something changes here, also check out the `gctSetupUI()`
      # function to determine when applyToAll actually shows up in the UI
      condition <- !(current_annotation_column %in% groups_in_all) |
        (current_group_norm_selection & !(current_group_norm_column %in% groups_in_all))
      
      # update applyToAll to FALSE if necessary
      if (TRUE %in% condition) {
        updateCheckboxInput(inputId = "applyToAll", value = FALSE)
      }
    })
    
    
    
    # change next/back buttons if applyToAll == TRUE
    observeEvent(input$applyToAll, {
      
      # change next button to submit
      if (input$applyToAll | backNextLogic$place == backNextLogic$maxPlace) {
        output$rightButton <- renderUI({actionButton(ns("submitGCTButton"), 
                                                     "Submit", 
                                                     class = "btn btn-primary")})
      } else {
        output$rightButton <- renderUI({actionButton_icon_right(
          ns("nextButton"), "Next", icon = icon("chevron-right"))})
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
      GCTs <- GCTs_unprocessed_internal_reactive()
      
      # call processGCTs function in a tryCatch
      processing_output <- processGCTs(GCTs = GCTs, parameters = parameters)
      
      # also transform the original GCTs
      transformation_output <- transformGCTs(GCTs = GCTs, parameters = parameters)
      
      if (!is.null(processing_output)) {
        # set GCTs_and_params reactiveVal
        GCTs_and_params(processing_output) 
        
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
                           'spectronaut_gene_symbol_split',
                           'spectronaut_gene_symbol_separator')

      # assign new user selections
      # NOTE: there are fields in `new_parameters` that aren't updated here,
      # which means you can't easily forgo the for loop for an apply equivalent
      for (label in assignment_labels) {
        for (param in parameter_names) {
          input_value <- input[[paste0(current_label, '_', param)]]

          # Convert intensity_data checkbox boolean to "Yes"/"No" string
          if (param == "intensity_data" && is.logical(input_value)) {
            input_value <- if (input_value) "Yes" else "No"
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

      # Collect per-file Spectronaut flags
      spectronaut_flags <- setNames(
        sapply(accumulated_files()$name, function(f) isTRUE(input[[paste0("is_spectronaut_", f)]])),
        accumulated_files()$name
      )
      is_spectronaut_reactive(spectronaut_flags)

      if (any(spectronaut_flags)) {
        csvExcelSpectronautParseStep(labels, spectronaut_flags, place = 1)
      } else {
        csvExcelIdentifierSelection(labels)
      }
    })

    # Identifier column selection step
    csvExcelIdentifierSelection <- function(labels) {
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
      # Exclude identifier columns so only actual sample/metadata columns appear
      preprocessed <- spectronaut_processed_data()
      all_samples <- c()
      for (i in seq_len(nrow(accumulated_files()))) {
        lbl       <- labels[i]
        id_col    <- identifier_columns[i]
        file_ext  <- tools::file_ext(tolower(accumulated_files()$name[i]))
        file_path <- accumulated_files()$datapath[i]
        if (!is.null(preprocessed) && !is.null(preprocessed[[lbl]])) {
          all_samples <- c(all_samples, setdiff(names(preprocessed[[lbl]]), id_col))
        } else if (file_ext == "csv") {
          data <- readr::read_csv(file_path, n_max = 1, show_col_types = FALSE)
          all_samples <- c(all_samples, setdiff(names(data), id_col))
        } else if (file_ext == "tsv") {
          data <- readr::read_tsv(file_path, n_max = 1, show_col_types = FALSE)
          all_samples <- c(all_samples, setdiff(names(data), id_col))
        } else if (file_ext %in% c("xlsx", "xls")) {
          data <- readxl::read_excel(file_path, n_max = 1)
          all_samples <- c(all_samples, setdiff(names(data), id_col))
        }
      }
      sample_names <- if (length(all_samples) > 0) unique(all_samples) else template_sample_names()

      exp_design_df(data.frame(
        columnName = sample_names,
        Condition  = rep(NA_character_, length(sample_names)),
        Replicate  = rep(NA_character_, length(sample_names)),
        stringsAsFactors = FALSE
      ))

      show_exp_design_panel(TRUE)
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
    }

    # Spectronaut per-file parse step
    csvExcelSpectronautParseStep <- function(labels, spectronaut_flags, place) {
      spectronaut_indices <- which(spectronaut_flags)
      current_file_idx <- spectronaut_indices[place]
      current_file <- accumulated_files()[current_file_idx, ]
      current_label <- labels[current_file_idx]
      file_ext <- tools::file_ext(tolower(current_file$name))

      data_columns <- tryCatch({
        preview <- read_uploaded_data_preview(current_file$datapath, file_ext, n_max = 1)
        if (!is.null(preview)) names(preview) else character(0)
      }, error = function(e) character(0))

      output$sideBarMain <- renderUI({
        tagList(
          h4(paste0("Parse Spectronaut Report: ", current_label,
                    " (", place, " of ", length(spectronaut_indices), ")")),
          spectronautSetupUI(ns = ns, data_columns = data_columns)
        )
      })

      if (place == 1) {
        output$leftButton <- renderUI({
          actionButton(ns("backToCSVExcelLabelsButton"), "Back",
                       icon = icon("chevron-left"), class = "btn btn-default")
        })
      } else {
        output$leftButton <- renderUI({
          actionButton(ns("backSpectronautParseButton"), "Back",
                       icon = icon("chevron-left"), class = "btn btn-default")
        })
      }

      if (place == length(spectronaut_indices)) {
        output$rightButton <- renderUI({
          actionButton(ns("submitSpectronautParseButton"), "Next",
                       class = "btn btn-primary")
        })
      } else {
        output$rightButton <- renderUI({
          actionButton_icon_right(ns("submitSpectronautParseButton"), "Next File",
                                  icon = icon("chevron-right"))
        })
      }

      spectronaut_parse_place(place)
    }

    # Handle condition setup file upload
    observeEvent(input$spectronautConditionFile, {
      req(input$spectronautConditionFile)
      tryCatch({
        cond_data <- read_spectronaut_condition_setup(input$spectronautConditionFile$datapath)
        spectronaut_condition_data(cond_data)

        # Detect suffixes from first data file
        first_file <- accumulated_files()[1, ]
        file_ext <- tools::file_ext(tolower(first_file$name))
        all_cols <- tryCatch({
          preview <- read_uploaded_data_preview(first_file$datapath, file_ext, n_max = 1)
          if (!is.null(preview)) names(preview) else character(0)
        }, error = function(e) character(0))

        run_labels <- cond_data[["Run Label"]]
        suffixes <- detect_quant_suffixes(all_cols, run_labels)

        output$spectronautSuffixUI <- renderUI({
          if (length(suffixes) == 0) {
            p("No quantification suffixes detected. Check that run labels match column names.",
              style = "color: orange;")
          } else {
            selectInput(ns("spectronaut_quant_suffix"),
                        "Quantification metric:",
                        choices = suffixes,
                        selected = suffixes[1])
          }
        })
      }, error = function(e) {
        showNotification(
          ui = HTML(paste0("<b>Error reading condition setup file:</b><br>", e$message)),
          type = "error", duration = NULL, closeButton = TRUE
        )
      })
    })

    # Handle Spectronaut parse step submission
    observeEvent(input$submitSpectronautParseButton, {
      flags <- is_spectronaut_reactive()
      spectronaut_indices <- which(flags)
      place <- spectronaut_parse_place()
      current_file_idx <- spectronaut_indices[place]
      current_file <- accumulated_files()[current_file_idx, ]
      file_ext <- tools::file_ext(tolower(current_file$name))

      labels <- sapply(accumulated_files()$name, function(f) input[[paste0('CSVExcelLabel_', f)]])
      current_label <- labels[current_file_idx]

      processed <- tryCatch({
        full_data <- read_uploaded_data_preview(current_file$datapath, file_ext, n_max = Inf)
        # Apply protigy_id extraction first (if requested)
        if (isTRUE(input$spectronaut_create_id) &&
            !is.null(input$spectronaut_id_source_column) &&
            input$spectronaut_id_source_column %in% names(full_data)) {
          sep <- if (!is.null(input$spectronaut_id_separator) &&
                     nchar(trimws(input$spectronaut_id_separator)) > 0)
            input$spectronaut_id_separator else ";"
          full_data <- extract_protigy_id(full_data, input$spectronaut_id_source_column, sep)
        }
        if (!is.null(spectronaut_condition_data())) {
          withCallingHandlers(
            apply_spectronaut_condition_setup(full_data, spectronaut_condition_data(),
                                              input$spectronaut_quant_suffix,
                                              isTRUE(input$spectronaut_merge_condition_replicate)),
            warning = function(w) {
              # Intercepts replicateNAWarning emitted by buildExpDesignFromConditionSetup
              # via apply_spectronaut_condition_setup. Coupled to the custom condition
              # class defined in sidebar_setup_helpers_spectronaut.R.
              if (inherits(w, "replicateNAWarning")) {
                showNotification(
                  ui = HTML(paste0("<b>Warning: Replicate column issue</b><br>", conditionMessage(w))),
                  type = "warning", duration = NULL, closeButton = TRUE
                )
                invokeRestart("muffleWarning")
              }
            }
          )
        } else {
          full_data
        }
      }, error = function(e) {
        showNotification(HTML(paste0("<b>Error preprocessing:</b><br>", e$message)),
                         type = "error", duration = NULL, closeButton = TRUE)
        NULL
      })

      if (is.null(processed)) return()

      current_data <- spectronaut_processed_data()
      if (is.null(current_data)) current_data <- list()
      current_data[[current_label]] <- processed
      spectronaut_processed_data(current_data)

      if (place < length(spectronaut_indices)) {
        csvExcelSpectronautParseStep(labels, flags, place + 1)
      } else {
        csvExcelIdentifierSelection(labels)
      }
    })

    # Handle back navigation within Spectronaut parse steps
    observeEvent(input$backSpectronautParseButton, {
      flags <- is_spectronaut_reactive()
      place <- spectronaut_parse_place()
      labels <- sapply(accumulated_files()$name, function(f) input[[paste0('CSVExcelLabel_', f)]])
      if (place > 1) {
        csvExcelSpectronautParseStep(labels, flags, place - 1)
      } else {
        csvExcelLabelAssignment()
      }
    })

    # Live preview: update when create_id options change
    observeEvent(
      list(input$spectronaut_create_id, input$spectronaut_id_separator,
           input$spectronaut_id_source_column),
      {
        req(accumulated_files(), isTRUE(input$spectronaut_create_id))
        first_file <- accumulated_files()[1, ]
        file_ext <- tools::file_ext(tolower(first_file$name))
        data <- read_uploaded_data_preview(first_file$datapath, file_ext)
        if (!is.null(data) &&
            !is.null(input$spectronaut_id_source_column) &&
            input$spectronaut_id_source_column %in% names(data)) {
          sep <- if (!is.null(input$spectronaut_id_separator) &&
                     nchar(trimws(input$spectronaut_id_separator)) > 0)
            input$spectronaut_id_separator else ";"
          data <- extract_protigy_id(data, input$spectronaut_id_source_column, sep)
        }
        preview_data_reactive(data)
      },
      ignoreInit = TRUE
    )

    # Live preview: update when condition setup options change
    observeEvent(
      list(input$spectronaut_quant_suffix, input$spectronaut_merge_condition_replicate),
      {
        req(accumulated_files(), !is.null(spectronaut_condition_data()),
            !is.null(input$spectronaut_quant_suffix))
        first_file <- accumulated_files()[1, ]
        file_ext <- tools::file_ext(tolower(first_file$name))
        data <- read_uploaded_data_preview(first_file$datapath, file_ext)
        if (!is.null(data)) {
          data <- apply_spectronaut_condition_setup(
            data,
            spectronaut_condition_data(),
            input$spectronaut_quant_suffix,
            isTRUE(input$spectronaut_merge_condition_replicate)
          )
          preview_data_reactive(data)
        }
      },
      ignoreInit = TRUE
    )

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

    # Update preview when files are first uploaded
    observeEvent(accumulated_files(), {
      req(accumulated_files())
      first_file <- accumulated_files()[1, ]
      file_ext <- tools::file_ext(tolower(first_file$name))
      data <- read_uploaded_data_preview(first_file$datapath, file_ext)
      preview_data_reactive(data)
    }, ignoreNULL = TRUE)

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

      ht <- rhandsontable::rhandsontable(
        df,
        rowHeaders  = NULL,
        useTypes    = FALSE,
        stretchH    = "last",
        contextMenu = TRUE,
        width       = "100%",
        colWidths   = c(col1_width, rep(100, ncol(df) - 1))
      )

      # Make first column (columnName) read-only with gray background
      ht <- rhandsontable::hot_col(ht, col = 1, readOnly = TRUE,
                                   renderer = "function(instance, td, row, col, prop, value, cellProperties) {
                                     Handsontable.renderers.TextRenderer.apply(this, arguments);
                                     td.style.background = '#f5f5f5';
                                     td.style.color = '#666';
                                     return td;
                                   }")
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

    # Add factor column
    observeEvent(input$add_factor_col, {
      req(nchar(trimws(input$new_factor_name)) > 0)
      col_name <- trimws(input$new_factor_name)
      df <- exp_design_df()
      if (!is.null(df) && !col_name %in% names(df)) {
        df[[col_name]] <- NA_character_
        exp_design_df(df)
      }
      updateTextInput(session, "new_factor_name", value = "")
    })

    # Render remove-column selector (only shows user-added columns)
    output$remove_factor_col_ui <- renderUI({
      df <- exp_design_df()
      removable <- setdiff(names(df), c("columnName", "Condition", "Replicate"))
      if (length(removable) == 0) return(NULL)
      div(
        style = "display:flex; gap:4px; align-items:center;",
        selectInput(ns("remove_factor_col_select"), label = NULL,
                    choices = removable, width = "120px"),
        actionButton(ns("remove_factor_col"), "Remove", class = "btn btn-danger btn-sm")
      )
    })

    # Remove factor column via Remove button
    observeEvent(input$remove_factor_col, {
      col_name <- input$remove_factor_col_select
      df <- exp_design_df()
      if (!is.null(df) && col_name %in% names(df) && col_name != "columnName") {
        df[[col_name]] <- NULL
        exp_design_df(df)
      }
    })

    # CSV upload populates the inline table
    observeEvent(input$expDesignFile, {
      req(input$expDesignFile)
      tryCatch({
        uploaded <- readExperimentalDesign(input$expDesignFile$datapath)
        exp_design_df(as.data.frame(uploaded))
      }, error = function(e) {
        showNotification(paste("Upload error:", e$message), type = "error")
      })
    })

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

            csv_excel_result <- processCSVExcelWorkflowWithPerDatasetIdentifiers(
              accumulated_files(), exp_design, identifier_columns, labels,
              preprocessed_data = spectronaut_processed_data()
            )
            
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
          labelsGO(labelsGO() + 1)
        }
      )
    })
    
    # return GCTs and parameters together in one list
    return(list(GCTs_and_params = GCTs_and_params,
                globals = globals,
                GCTs_original = GCTs_original))
    
  }) # end moduleServer
} # end setupSidebarServer