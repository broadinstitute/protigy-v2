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
      h4("Upload your data file(s)"), 

      # File input
      fileInput(ns("dataFiles"), 
                paste("GCT, CSV, TSV, or Excel"),
                multiple = TRUE,
                accept = c(".gct", ".csv", ".xlsx", ".xls", ".tsv")),
      hr(),
      
      # the main body of the sidebar, contents assigned in setupSidebarServer
      uiOutput(ns('sideBarMain')),
      
      # navigation buttons on the bottom left/right of sidebar
      fluidRow(
        column(6, uiOutput(ns('leftButton'))),
        column(6, uiOutput(ns('rightButton')))
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
    
    # initialize reactiveValues with back/next logic for when user navigates
    # through each GCT file to input parameters
    backNextLogic <- reactiveValues(placeChanged = 0)
    
    # initialize reactiveVal to indicate when labels & gcts are validated + submitted
    labelsGO <- reactiveVal(0)
    gctsGO <- reactiveVal(0)
    csvExcel_identifier_columns_reactive <- reactiveVal(NULL)
    
    # read in default settings and choices from yamls
    default_parameters <- read_yaml(system.file('setup_parameters/setupDefaults.yaml', package = 'Protigy'))
    parameter_choices <- read_yaml(system.file('setup_parameters/setupChoices.yaml', package = 'Protigy'))
    
    
    ### STEP 1: LABEL ASSIGNMENT ###
    
    # once files uploaded, display label assignment
    observeEvent(
      eventExpr = input$dataFiles, 
      ignoreInit = TRUE,
      handlerExpr = {
        parameters_internal_reactive(NULL) # reset internal parameters
        GCTs_unprocessed_internal_reactive(NULL) # reset internal GCTs
        
        # Check if files are GCT format or CSV/Excel format
        file_extensions <- tools::file_ext(tolower(input$dataFiles$name))
        
        if (all(file_extensions == "gct")) {
          # All GCT files - use existing workflow
          labelAssignment()
        } else if (all(file_extensions %in% c("csv", "xlsx", "xls", "tsv"))) {
          # All CSV/Excel/TSV files - use same workflow
          csvExcelWorkflow()
          
          # Automatically switch to CSV/TSV/Excel Processing help tab
          updateTabsetPanel(session = parent, 
                           inputId = "navbar-tabs", 
                           selected = "Help-Analysis")
          
          # Switch to the CSV/TSV/Excel Processing tab within the help section
          shinyjs::runjs("
            setTimeout(function() {
              $('a[data-value=\"CSV/TSV/Excel Processing\"]').click();
            }, 100);
          ")
        } else {
          # Mixed file types - show error
          shinyalert::shinyalert(
            title = "Error",
            text = "Please upload files of the same type only (GCT, CSV/Excel/TSV). Mixed file types are not supported.",
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
                           'group_normalization_column',
                           'gene_symbol_column')
      
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
                                                   gctFileNames = input$dataFiles$name)})
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
                                                           dataFileNames = input$dataFiles$name)})
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
      labels <- sapply(input$dataFiles$name, function(file) {
        input[[paste0('CSVExcelLabel_', file)]]
      })
      
      # Validate labels
      if (any(labels == "")) {
        shinyalert::shinyalert(
          title = "Error",
          text = "Please provide labels for all files.",
          type = "error"
        )
        return()
      }
      
      # Check for duplicate labels
      if (length(unique(labels)) != length(labels)) {
        shinyalert::shinyalert(
          title = "Error", 
          text = "Please provide unique labels for each file.",
          type = "error"
        )
        return()
      }
      
      # Store labels in parameters_internal_reactive (same as GCT workflow)
      for (i in seq_along(labels)) {
        filename <- input$dataFiles$name[i]
        label <- labels[i]
        parameters_internal_reactive(c(
          parameters_internal_reactive(),
          setNames(list(list(gct_file_name = filename)), label)
        ))
      }
      
      # Move to identifier column selection
      csvExcelIdentifierSelection(labels)
    })
    
    # Identifier column selection step
    csvExcelIdentifierSelection <- function(labels) {
      output$sideBarMain <- renderUI({csvExcelIdentifierSetupUI(ns = ns, 
                                                               dataFiles = input$dataFiles,
                                                               labels = labels)})
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
      identifier_columns <- sapply(seq_len(nrow(input$dataFiles)), function(i) {
        input[[paste0("identifierColumn_", i)]]
      })
      
      # Validate identifier columns
      if (any(is.null(identifier_columns)) || any(identifier_columns == "")) {
        shinyalert::shinyalert(
          title = "Error",
          text = "Please select identifier columns for all datasets.",
          type = "error"
        )
        return()
      }
      
      # Store identifier columns for later retrieval
      csvExcel_identifier_columns_reactive(identifier_columns)
      
      # Move to experimental design setup
      csvExcelExpDesignSetup(identifier_columns)
    })
    
    # Experimental design setup step
    csvExcelExpDesignSetup <- function(identifier_columns) {
      
      output$sideBarMain <- renderUI({csvExcelExpDesignSetupUI(ns = ns, 
                                                      dataFiles = input$dataFiles,
                                                              labels = sapply(input$dataFiles$name, function(file) {
                                                                input[[paste0('CSVExcelLabel_', file)]]
                                                              }))})
      output$rightButton <- NULL
      output$leftButton <- renderUI({actionButton(ns("backToCSVExcelIdentifiersButton"), 
                                                  "Back",
                                                  icon = icon("chevron-left"),
                                                  class = "btn btn-default")})
    }
    
    # Handle back navigation
    observeEvent(input$backToCSVExcelLabelsButton, {
      csvExcelLabelAssignment()
    })
    
    observeEvent(input$backToCSVExcelIdentifiersButton, {
      labels <- sapply(input$dataFiles$name, function(file) {
        input[[paste0('CSVExcelLabel_', file)]]
      })
      csvExcelIdentifierSelection(labels)
    })
    
    # Create a reactive to store sample names for template generation
    template_sample_names <- reactive({
      req(input$dataFiles)
      
        tryCatch({
        all_samples <- c()
        for (i in seq_len(nrow(input$dataFiles))) {
          file_path <- input$dataFiles$datapath[i]
          file_ext <- tools::file_ext(tolower(input$dataFiles$name[i]))
          
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
    output$downloadExpDesignTemplate <- downloadHandler(
      filename = "experimental_design_template.csv",
      content = function(file) {
        # Get sample names from reactive
        sample_names <- template_sample_names()
        
        # Create template data frame with columnName, Experiment, and Group columns
        template_df <- data.frame(
          columnName = sample_names,
          Experiment = rep("", length(sample_names)),
          Group = rep("", length(sample_names)),
            stringsAsFactors = FALSE
          )
          
        # Write the template to file
        write.csv(template_df, file, row.names = FALSE)
      }
    )
    
    # Reactive output to control process button visibility for CSV/Excel
    output$expDesignFileUploaded <- reactive({
      return(!is.null(input$expDesignFile))
    })
    outputOptions(output, "expDesignFileUploaded", suspendWhenHidden = FALSE)
    
    
    

    # Process CSV/Excel data when experimental design is uploaded
    observeEvent(input$processCSVExcel, {
      req(input$expDesignFile)
      
      my_shinyalert_tryCatch(
        text.error = "<b>CSV/Excel Processing Error:</b>",
        append.error = TRUE,
        show.error = TRUE,
        return.error = NULL,
        expr = {
          # Process CSV/Excel files with progress indication
          withProgress(message = "Processing CSV/Excel files...", {
            setProgress(0.2, detail = "Reading experimental design")
            
            # Read experimental design
            exp_design <- readExperimentalDesign(input$expDesignFile$datapath)
            
            setProgress(0.5, detail = "Converting data to analysis format")
            
            # Process CSV/Excel files with per-dataset identifier columns
            identifier_columns <- csvExcel_identifier_columns_reactive()
            
            # Validate identifier columns
            if (is.null(identifier_columns) || any(identifier_columns == "")) {
              stop("Please select identifier columns for all datasets.")
            }
            
            # Get labels from input fields
            labels <- sapply(input$dataFiles$name, function(file) {
              input[[paste0('CSVExcelLabel_', file)]]
            })
            
            csv_excel_result <- processCSVExcelWorkflowWithPerDatasetIdentifiers(input$dataFiles, exp_design, identifier_columns, labels)
            
            setProgress(0.8, detail = "Setting up analysis parameters")
            
            # Store converted GCT objects and parameters for later processing (same as GCT workflow)
            GCTs_unprocessed_internal_reactive(csv_excel_result$GCTs)
            parameters_internal_reactive(csv_excel_result$parameters)
            
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