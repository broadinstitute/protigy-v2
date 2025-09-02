################################################################################
# Module: SETUP SIDEBAR
# Main shiny functions (server and UI)
################################################################################

# UI for the sidebar setup
setupSidebarUI <- function(id = "setupSidebar") {
  # namespace function, wrap inputId's and outputId's with this (e.g. `ns(id)`)
  ns <- NS(id) 
  
  tagList(
    div(
      h4("Upload data file(s)"), 
      style = "margin-top: 20px;"
    ),
    
    add_css_attributes(
      fileInput(
        ns("dataFiles"),
        "All files must include the same samples (Supports GCT/GCTX, CSV, Excel, TSV)", 
        multiple = TRUE, 
        accept = c(".gct", ".gctx", ".csv", ".xlsx", ".xls", ".tsv")
      ), 
      classes = "small-input shiny-file-input-container"
    ), 
    
    hr(), 

    # the main body of the sidebar, contents assigned in setupSidebarServer
    uiOutput(ns('sideBarMain')),

    # navigation buttons on the bottom left/right of sidebar
    fluidRow(
      column(6, uiOutput(ns('leftButton'))),
      column(6, uiOutput(ns('rightButton')))
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
    
    # initialize reactiveValues with back/next logic for when user navigates
    # through each GCT file to input parameters
    backNextLogic <- reactiveValues(placeChanged = 0)
    
    # initialize reactiveVal to indicate when labels & gcts are validated + submitted
    labelsGO <- reactiveVal(0)
    gctsGO <- reactiveVal(0)
    
    # read in default settings and choices from yamls
    default_parameters <- read_yaml(system.file('setup_parameters/setupDefaults.yaml', package = 'Protigy'))
    parameter_choices <- read_yaml(system.file('setup_parameters/setupChoices.yaml', package = 'Protigy'))
    
    
    ### Label Assignment ###
    # More robust handling of different file formats 
    # Once files uploaded, display label assignment
    observeEvent(
      eventExpr = input$dataFiles, 
      ignoreInit = TRUE,
      handlerExpr = {
        parameters_internal_reactive(NULL) # reset internal parameters
        GCTs_unprocessed_internal_reactive(NULL) # reset internal GCTs
        
        # Check if files are GCT, CSV/Excel, or TSV format
        file_extensions <- tools::file_ext(tolower(input$dataFiles$name))
        
        if (all(file_extensions == "gct")) {
          # All GCT files - use existing workflow
          labelAssignment()
        } else if (all(file_extensions %in% c("csv", "xlsx", "xls"))) {
          # All CSV/Excel files - use new workflow
          csvExcelWorkflow()
        } else if (all(file_extensions == "tsv")) {
          # All TSV files - use TSV workflow
          tsvWorkflow()
        } else {
          # Mixed file types - show error
          shinyalert::shinyalert(
            title = "Error",
            text = "Please upload files of the same type only: GCT files, CSV/Excel files, or TSV files. Mixed file types are not supported.",
            type = "error"
          )
        }
      })
    
    # also display label assignment if user navigates back to it
    observeEvent(
      eventExpr = input$backToLabelsButton,
      ignoreInit = TRUE,
      handlerExpr = labelAssignment())
    
    # validate labels once submitted
    observeEvent(input$submitLabelsButton, {
      out <- my_shinyalert_tryCatch({
        all_labels <- sapply(input$dataFiles$name, 
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
    }, ignoreInit = TRUE)
    
    
    ### STEP 2: INPUT GCT PARAMETERS ###
    
    # once labels assignment submitted, set values for back/next logic
    observeEvent(labelsGO(), {
      # current place in the next/back logic
      backNextLogic$place <- 1 
      
      # maximum place (i.e. the total number of data files)
      backNextLogic$maxPlace <- length(input$dataFiles$name) 
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
        apply(input$dataFiles, 1, function(file) {
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
          output$rightButton <- renderUI({actionButton_icon_right(
            ns("nextButton"), "Next", icon = icon("chevron-right"))})
        }
      })
    
    # update parameter choices when intensity data is toggled
    current_intensity <- reactive({
      label <- names(parameters_internal_reactive())[isolate(backNextLogic$place)]
      input[[paste0(label, '_intensity_data')]]})
    observeEvent(current_intensity(), {
      # first, collect all the current inputs
      collectInputs()
      
      # gather current label and parameters
      label = names(parameters_internal_reactive())[backNextLogic$place]
      parameters = parameters_internal_reactive()[[label]]
      
      # indicator for intensity data (check out the yaml format)
      ind = paste0("intensity_data_", tolower(current_intensity()))
      
      # update data normalization
      updateSelectInput(
        inputId = paste0(label, '_data_normalization'),
        choices = parameter_choices$data_normalization[[ind]],
        selected = ifelse(
          parameters$data_normalization %in% parameter_choices$data_normalization[[ind]],
          parameters$data_normalization,
          default_parameters$data_normalization))
      
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
      parameters <- GCTs_and_params()$parameters
      all_omes <- names(parameters)
      
      globals$omes <- all_omes
      globals$default_ome <- all_omes[1]
      globals$default_annotations <- sapply(
        all_omes, 
        function(ome) parameters[[ome]]$annotation_column,
        simplify = FALSE
      )
    })
    
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
                           'group_normalization_column')
      
      # assign new user selections
      # NOTE: there are fields in `new_parameters` that aren't updated here, 
      # which means you can't easily forgo the for loop for an apply equivalent
      for (label in assignment_labels) {
        for (param in parameter_names) {
          new_parameters[[label]][[param]] <- input[[paste0(current_label, '_', param)]]
        }
      }
      
      # assign reactiveVal
      parameters_internal_reactive(new_parameters) 
    }
    
    # label assignment, has to be used in separate observeEvent() calls
    labelAssignment <- function() {
      output$sideBarMain <- renderUI({labelSetupUI(ns = ns, 
                                                   dataFileNames = input$dataFiles$name)})
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
    
    # CSV/Excel workflow function
    csvExcelWorkflow <- function() {
      # Check if automatic identifier detection is possible
      identifier_info <- tryCatch({
        needs_user_selection <- FALSE
        all_columns <- c()
        detected_identifier <- NULL
        detection_method <- NULL
        
        for (i in seq_len(nrow(input$dataFiles))) {
          file_path <- input$dataFiles$datapath[i]
          file_ext <- tools::file_ext(tolower(input$dataFiles$name[i]))
          
          if (file_ext == "csv") {
            data <- utils::read.csv(file_path, nrows = 5, stringsAsFactors = FALSE)
          } else if (file_ext %in% c("xlsx", "xls")) {
            data <- readxl::read_excel(file_path, n_max = 5)
          } else {
            next
          }
          
          columns <- names(data)
          all_columns <- c(all_columns, columns)
          
          # Determine what identifier will be used
          if ("PG.ProteinGroups" %in% columns) {
            detected_identifier <- "firstProteinGroup (first protein group in PG.ProteinGroups)"
            detection_method <- "first_protein_group"
          } else {
            needs_user_selection <- TRUE
          }
        }
        
        list(
          columns = unique(all_columns),
          needs_user_selection = needs_user_selection,
          detected_identifier = detected_identifier,
          detection_method = detection_method
        )
      }, error = function(e) {
        list(columns = c(), needs_user_selection = TRUE, detected_identifier = NULL, detection_method = NULL)
      })
      
      output$sideBarMain <- renderUI({
        csvExcelSetupUI(ns = ns, 
                       dataFiles = input$dataFiles,
                       identifierColumns = identifier_info$columns,
                       showIdentifierSelector = identifier_info$needs_user_selection,
                       detectedIdentifier = identifier_info$detected_identifier,
                       detectionMethod = identifier_info$detection_method)
      })
      output$rightButton <- NULL
      output$leftButton <- NULL
    }
    
    # TSV workflow function
    tsvWorkflow <- function() {
      output$sideBarMain <- renderUI({
        tsvSetupUI(ns = ns, dataFiles = input$dataFiles)
      })
      output$rightButton <- NULL
      output$leftButton <- NULL
    }
    
    # Reactive values for TSV processing
    conditionSetupData <- reactiveVal(NULL)
    processedTSVData <- reactiveVal(NULL)
    tsvMappingTables <- reactiveVal(NULL)
    tsvIdentifierInfo <- reactiveVal(NULL)
    
    # Handle condition setup file upload for TSV workflow
    observeEvent(input$conditionSetupFile, {
      req(input$conditionSetupFile)
      
      tryCatch({
        # Read and validate condition setup file
        condition_setup <- readConditionSetup(input$conditionSetupFile$datapath)
        conditionSetupData(condition_setup)
        
        # Process TSV files with condition setup
        withProgress(message = "Processing TSV files...", {
          setProgress(0.3, detail = "Analyzing TSV structure")
          
          tsv_result <- processTSVFiles(
            dataFiles = input$dataFiles,
            conditionSetup = condition_setup
          )
          
          setProgress(0.7, detail = "Generating mapping tables")
          
          # Store results
          processedTSVData(tsv_result$processed_data)
          tsvMappingTables(tsv_result$mapping_tables)
          tsvIdentifierInfo(tsv_result$identifier_info)
          
          # Update identifier column choices based on processed data
          if (length(tsv_result$processed_data) > 0) {
            first_dataset <- tsv_result$processed_data[[1]]
            metadata_cols <- identifyMetadataColumns(first_dataset)
            
            # Get identifier information from the first dataset
            first_label <- names(tsv_result$identifier_info)[1]
            identifier_info <- tsv_result$identifier_info[[first_label]]
            default_identifier <- identifier_info$identifier_column
            
            # Set default to "id" if it exists, otherwise use detected identifier
            final_default <- if ("id" %in% metadata_cols) "id" else default_identifier
            
            updateSelectInput(session, "identifierColumn", choices = metadata_cols, selected = final_default)
          }
          
          setProgress(1.0, detail = "Processing complete")
        })
        
        # Show success message
        shinyalert::shinyalert(
          title = "TSV Processing Complete!",
          text = "Successfully processed TSV files with condition setup. Review the mapping results below.",
          type = "success",
          timer = 3000,
          showConfirmButton = FALSE
        )
        
      }, error = function(e) {
        error_msg <- paste("Failed to process TSV files:", e$message)
        shinyalert::shinyalert(
          title = "TSV Processing Error",
          text = error_msg,
          type = "error"
        )
      })
    })
    
    # Reactive output to control condition setup file uploaded visibility
    output$conditionSetupUploaded <- reactive({
      return(!is.null(input$conditionSetupFile))
    })
    outputOptions(output, "conditionSetupUploaded", suspendWhenHidden = FALSE)
    
    # Generate processing summary output
    output$processingSummaryOutput <- renderUI({
      req(processedTSVData())
      
      # Calculate summary statistics across all processed files
      total_files <- length(processedTSVData())
      
      # For now, show basic summary - could be enhanced with detailed stats
      tagList(
        p(strong("Files processed: "), total_files),
        p(strong("Status: "), span("Ready for experimental design setup", style = "color: green;"))
      )
    })
    
    # Render identifier column selector for TSV workflow
    output$identifierColumnSelector <- renderUI({
      req(tsvIdentifierInfo(), processedTSVData())
      
      # Get identifier information from the first dataset
      first_label <- names(tsvIdentifierInfo())[1]
      identifier_info <- tsvIdentifierInfo()[[first_label]]
      detected_identifier <- identifier_info$identifier_column
      identifier_method <- identifier_info$identifier_method
      
      # Get all available columns for selection
      first_dataset <- processedTSVData()[[1]]
      metadata_cols <- identifyMetadataColumns(first_dataset)
      
      # Determine if we should show selector or detection message
      show_selector <- identifier_method == "first_column_fallback"
      
      if (!show_selector) {
        # Show detection message
        tagList(
          div(
            class = "identifier-selector-hidden",
            h5("Identifier Column Detected!"),
            if (identifier_method == "first_protein_group") {
              p("Using processed protein identifiers: ", strong("id"), 
                " (extracted first protein group from PG.ProteinGroups)")
            } else {
              p("Using identifier column: ", strong(detected_identifier))
            }
          )
        )
      } else {
        # Show selector
        tagList(
          h5("Define Identifier Column"),
          p("Select the column to use as the unique identifier for rows:"),
          add_css_attributes(
            selectInput(
              ns("identifierColumn"),
              "Identifier Column:",
              choices = metadata_cols,
              selected = detected_identifier
            ),
            classes = "small-input identifier-selector-visible"
          )
        )
      }
    })
    
    # Process TSV data when experimental design is uploaded
    observeEvent(input$processTSV, {
      req(input$expDesignFile, processedTSVData())
      
      tryCatch({
        # Process TSV files with experimental design
        withProgress(message = "Converting TSV to analysis format...", {
          setProgress(0.2, detail = "Reading experimental design")
          
          # Read experimental design
          exp_design <- readExperimentalDesign(input$expDesignFile$datapath)
          
          setProgress(0.5, detail = "Converting data to GCT format")
          
          # Convert processed TSV data to GCT format using CSV/Excel workflow
          # Get identifier column - use detected default if input not available
          identifier_col <- input$identifierColumn
          if (is.null(identifier_col) && !is.null(tsvIdentifierInfo())) {
            first_label <- names(tsvIdentifierInfo())[1]
            identifier_info <- tsvIdentifierInfo()[[first_label]]
            identifier_col <- identifier_info$identifier_column
          }
          
          # Get mapping parameters if gene mapping is enabled
          mapping_file_path <- NULL
          mapping_protein_col <- NULL
          mapping_gene_col <- NULL
          
          if (!is.null(input$geneSymbolMapping) && input$geneSymbolMapping && 
              !is.null(input$uploadMappingFile) && !is.null(mappingStats())) {
            mapping_file_path <- input$uploadMappingFile$datapath
            mapping_protein_col <- input$mappingProteinColumn
            mapping_gene_col <- input$mappingGeneColumn
          }
          
          # Create a mock dataFiles structure for the processed TSV data
          processed_data_list <- processedTSVData()
          mock_data_files <- data.frame(
            name = paste0(names(processed_data_list), ".csv"),
            datapath = sapply(names(processed_data_list), function(name) {
              temp_file <- tempfile(fileext = ".csv")
              utils::write.csv(processed_data_list[[name]], temp_file, row.names = FALSE)
              return(temp_file)
            }),
            stringsAsFactors = FALSE
          )
          
          # Use CSV processing workflow for the converted data
          tsv_result <- processCSVExcelWorkflow(
            dataFiles = mock_data_files,
            experimentalDesign = exp_design,
            identifierColumn = identifier_col,
            mappingFilePath = mapping_file_path,
            mappingProteinCol = mapping_protein_col,
            mappingGeneCol = mapping_gene_col
          )
          
          setProgress(0.8, detail = "Setting up analysis parameters")
          
          # Store converted GCT objects and parameters (same as CSV/Excel workflow)
          GCTs_unprocessed_internal_reactive(tsv_result$GCTs)
          
          # Modify parameters if user wants to use gene symbols as identifier
          if (!is.null(input$useGeneSymbolAsIdentifier) && 
              input$useGeneSymbolAsIdentifier == "gene_symbol" && 
              !is.null(mapping_file_path)) {
            
            # Update parameters to use gene_symbol as annotation column
            modified_parameters <- tsv_result$parameters
            for (ome_name in names(modified_parameters)) {
              modified_parameters[[ome_name]]$annotation_column <- "gene_symbol"
            }
            parameters_internal_reactive(modified_parameters)
          } else {
            parameters_internal_reactive(tsv_result$parameters)
          }
          
          # Set up back/next navigation logic for parameter setup
          backNextLogic$place <- 1
          backNextLogic$maxPlace <- length(tsv_result$GCTs)
          backNextLogic$placeChanged <- backNextLogic$placeChanged + 1
          
          setProgress(1.0, detail = "Ready for parameter setup")
        })
        
        # Show success message
        shinyalert::shinyalert(
          title = "TSV Files Converted!",
          text = paste("Successfully converted", length(processedTSVData()), 
                      "TSV file(s) to analysis format. Now configure analysis parameters..."),
          type = "success",
          timer = 3000,
          showConfirmButton = FALSE
        )
        
        # Trigger the standard parameter setup workflow (same as GCT files)
        labelsGO(labelsGO() + 1)
        
      }, error = function(e) {
        error_msg <- paste("Failed to convert TSV data:", e$message)
        shinyalert::shinyalert(
          title = "TSV Conversion Error",
          text = error_msg,
          type = "error"
        )
      })
    })
    
    # Reactive values for gene mapping
    mappingFileData <- reactiveVal(NULL)
    mappingStats <- reactiveVal(NULL)
    
    # Reactive output to control process button visibility
    output$expDesignFileUploaded <- reactive({
      return(!is.null(input$expDesignFile))
    })
    outputOptions(output, "expDesignFileUploaded", suspendWhenHidden = FALSE)
    
    # Reactive output to control mapping file uploaded visibility
    output$mappingFileUploaded <- reactive({
      return(!is.null(input$uploadMappingFile))
    })
    outputOptions(output, "mappingFileUploaded", suspendWhenHidden = FALSE)
    
    # Reactive output to control mapping completed visibility
    output$mappingCompleted <- reactive({
      return(!is.null(mappingStats()))
    })
    outputOptions(output, "mappingCompleted", suspendWhenHidden = FALSE)
    
    # Download handler for experimental design template
    output$downloadExpDesign <- downloadHandler(
      filename = "experimentalDesign.csv",
      content = function(file) {
        tryCatch({
          # Check if we're in TSV workflow (TSV files have been processed)
          if (!is.null(processedTSVData()) && length(processedTSVData()) > 0) {
            # TSV workflow: use processed TSV data
            identifier_col <- input$identifierColumn
            if (is.null(identifier_col) && !is.null(tsvIdentifierInfo())) {
              # Use detected identifier if input not available
              first_label <- names(tsvIdentifierInfo())[1]
              identifier_info <- tsvIdentifierInfo()[[first_label]]
              identifier_col <- identifier_info$identifier_column
            }
            
            template <- generateExperimentalDesignTemplateForTSV(
              processedTSVData = processedTSVData(), 
              identifierColumn = identifier_col
            )
          } else {
            # CSV/Excel workflow: use original files
            identifier_col <- input$identifierColumn
            template <- generateExperimentalDesignTemplate(
              dataFiles = input$dataFiles, 
              identifierColumn = identifier_col
            )
          }
          
          # Write to file
          utils::write.csv(template, file, row.names = FALSE)
          
        }, error = function(e) {
          shinyalert::shinyalert(
            title = "Error",
            text = paste("Failed to generate experimental design template:", e$message),
            type = "error"
          )
        })
      }
    )
    
    # Download handler for TSV mapping table
    output$downloadMapping <- downloadHandler(
      filename = function() {
        paste0("tsv_column_mapping_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(tsvMappingTables())
        
        tryCatch({
          # Combine all mapping tables if multiple files were processed
          mapping_tables <- tsvMappingTables()
          
          if (length(mapping_tables) == 1) {
            # Single file case
            combined_mapping <- mapping_tables[[1]]
            combined_mapping$File <- names(mapping_tables)[1]
          } else {
            # Multiple files case - combine with file names
            combined_mapping <- do.call(rbind, lapply(names(mapping_tables), function(file_name) {
              mapping_table <- mapping_tables[[file_name]]
              mapping_table$File <- file_name
              return(mapping_table)
            }))
          }
          
          # Reorder columns to put File first if it exists
          if ("File" %in% colnames(combined_mapping)) {
            col_order <- c("File", setdiff(colnames(combined_mapping), "File"))
            combined_mapping <- combined_mapping[, col_order, drop = FALSE]
          }
          
          # Write to CSV file
          utils::write.csv(combined_mapping, file, row.names = FALSE)
          
        }, error = function(e) {
          shinyalert::shinyalert(
            title = "Download Error",
            text = paste("Failed to download mapping table:", e$message),
            type = "error"
          )
        })
      }
    )
    
    # Handle mapping file upload
    observeEvent(input$uploadMappingFile, {
      req(input$uploadMappingFile)
      
      tryCatch({
        # Read mapping file to get column names
        if (!exists("readMappingFile")) {
          source("R/sidebar_setup_helpers_gene-mapping.R")
        }
        mapping_data <- readMappingFile(input$uploadMappingFile$datapath)
        mappingFileData(mapping_data)
        
        # Update column choices
        column_choices <- colnames(mapping_data)
        updateSelectInput(session, "mappingProteinColumn", choices = column_choices)
        updateSelectInput(session, "mappingGeneColumn", choices = column_choices)
        
      }, error = function(e) {
        shinyalert::shinyalert(
          title = "Error",
          text = paste("Failed to read mapping file:", e$message),
          type = "error"
        )
        mappingFileData(NULL)
      })
    })
    
    # Handle gene mapping process
    observeEvent(input$performMapping, {
      req(mappingFileData(), input$mappingProteinColumn, input$mappingGeneColumn)
      
      tryCatch({
        # Check if data files are uploaded
        if (is.null(input$dataFiles)) {
          shinyalert::shinyalert(
            title = "No Data Files",
            text = "Please upload data files before performing gene mapping.",
            type = "warning"
          )
          return()
        }
        
        # Perform test mapping on first data file to get statistics
        file_path <- input$dataFiles$datapath[1]
        file_ext <- tools::file_ext(tolower(input$dataFiles$name[1]))
        
        if (file_ext == "csv") {
          test_data <- utils::read.csv(file_path, stringsAsFactors = FALSE)
        } else if (file_ext %in% c("xlsx", "xls")) {
          test_data <- readxl::read_excel(file_path)
        }
        
        # Determine protein column in working data
        if ("PG.ProteinGroups" %in% colnames(test_data)) {
          working_protein_col <- "PG.ProteinGroups"
        } else {
          working_protein_col <- input$identifierColumn
        }
        
        # Perform mapping to get statistics
        mapping_result <- performGeneMapping(
          working_data = test_data,
          mapping_data = mappingFileData(),
          working_protein_col = working_protein_col,
          mapping_protein_col = input$mappingProteinColumn,
          mapping_gene_col = input$mappingGeneColumn
        )
        
        mappingStats(mapping_result$mapping_stats)
        
        shinyalert::shinyalert(
          title = "Gene Mapping Completed!",
          text = paste0("Successfully mapped ", mapping_result$mapping_stats$mapped, " out of ", 
                       mapping_result$mapping_stats$total, " protein groups (", 
                       mapping_result$mapping_stats$mapping_rate, "% success rate)."),
          type = "success",
          timer = 3000,
          showConfirmButton = FALSE
        )
        
      }, error = function(e) {
        shinyalert::shinyalert(
          title = "Mapping Error",
          text = paste("Failed to perform gene mapping:", e$message),
          type = "error"
        )
      })
    })
    
    # Display mapping statistics
    output$mappingStatsOutput <- renderUI({
      req(mappingStats())
      stats <- mappingStats()
      
      tagList(
        p(paste("Total protein groups:", stats$total)),
        p(paste("Successfully mapped:", stats$mapped)),
        p(paste("Not mapped:", stats$unmapped)),
        p(paste("Mapping success rate:", paste0(stats$mapping_rate, "%")))
      )
    })
    
    # Process CSV/Excel data when experimental design is uploaded
    observeEvent(input$processCSVExcel, {
      req(input$expDesignFile)
      
      tryCatch({
        # Process CSV/Excel files with progress indication
        withProgress(message = "Processing CSV/Excel files...", {
          setProgress(0.2, detail = "Reading experimental design")
          
          # Read experimental design
          exp_design <- readExperimentalDesign(input$expDesignFile$datapath)
          
          setProgress(0.5, detail = "Converting data to analysis format")
          
          # Process CSV/Excel files with selected identifier column and gene mapping
          identifier_col <- input$identifierColumn
          
          # Get mapping parameters if gene mapping is enabled
          mapping_file_path <- NULL
          mapping_protein_col <- NULL
          mapping_gene_col <- NULL
          
          if (!is.null(input$geneSymbolMapping) && input$geneSymbolMapping && 
              !is.null(input$uploadMappingFile) && !is.null(mappingStats())) {
            mapping_file_path <- input$uploadMappingFile$datapath
            mapping_protein_col <- input$mappingProteinColumn
            mapping_gene_col <- input$mappingGeneColumn
          }
          
          csv_excel_result <- processCSVExcelWorkflow(
            dataFiles = input$dataFiles, 
            experimentalDesign = exp_design, 
            identifierColumn = identifier_col,
            mappingFilePath = mapping_file_path,
            mappingProteinCol = mapping_protein_col,
            mappingGeneCol = mapping_gene_col
          )
          
          setProgress(0.8, detail = "Setting up analysis parameters")
          
          # Store converted GCT objects and parameters for later processing (same as GCT workflow)
          GCTs_unprocessed_internal_reactive(csv_excel_result$GCTs)
          
          # Modify parameters if user wants to use gene symbols as identifier
          if (!is.null(input$useGeneSymbolAsIdentifier) && 
              input$useGeneSymbolAsIdentifier == "gene_symbol" && 
              !is.null(mapping_file_path)) {
            
            # Update parameters to use gene_symbol as annotation column
            modified_parameters <- csv_excel_result$parameters
            for (ome_name in names(modified_parameters)) {
              modified_parameters[[ome_name]]$annotation_column <- "gene_symbol"
            }
            parameters_internal_reactive(modified_parameters)
          } else {
            parameters_internal_reactive(csv_excel_result$parameters)
          }
          
          # Set up back/next navigation logic for parameter setup
          backNextLogic$place <- 1
          backNextLogic$maxPlace <- length(csv_excel_result$GCTs)
          backNextLogic$placeChanged <- backNextLogic$placeChanged + 1
          
          setProgress(1.0, detail = "Ready for parameter setup")
        })
        
        # Show success message
        shinyalert::shinyalert(
          title = "Files Converted!",
          text = paste("Successfully converted", length(csv_excel_result$GCTs), 
                      "CSV/Excel file(s) to analysis format. Now configure analysis parameters..."),
          type = "success",
          timer = 3000,
          showConfirmButton = FALSE
        )
        
        # Trigger the standard parameter setup workflow (same as GCT files)
        labelsGO(labelsGO() + 1)
        
      }, error = function(e) {
        # Show detailed error message
        error_msg <- paste("Failed to process CSV/Excel data:", e$message)
        
        # Log error for debugging
        message("CSV/Excel Processing Error: ", error_msg)
        
        shinyalert::shinyalert(
          title = "Processing Error",
          text = error_msg,
          type = "error",
          html = TRUE
        )
      })
    })
    
    # return GCTs and parameters together in one list
    return(list(GCTs_and_params = GCTs_and_params,
                globals = globals,
                GCTs_original = GCTs_original))
    
  }) # end moduleServer
} # end setupSidebarServer