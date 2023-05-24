################################################################################
# Module: SETUP SIDEBAR
# Main shiny functions (server and UI)
################################################################################

# UI for the sidebar setup
setupSidebarUI <- function(id = "setupSidebar") {
  # namespace function, wrap inputId's and outputId's with this (e.g. `ns(id)`)
  ns <- NS(id) 
  
  tagList(
    # file input
    fileInput(ns("gctFiles"), 
              paste("Chose GCT file(s). All files should include the same samples."),
              multiple = TRUE,
              accept = ".gct"),
    hr(),
    
    # the main body of the sidebar, contents assigned in setupSidebarServer
    uiOutput(ns('sideBarMain')),
    
    # navigation buttons on the bottom left/right of sidebar
    fluidRow(
      column(6, uiOutput(ns('leftButton'))),
      column(6, uiOutput(ns('rightButton')))
    )
  ) # end tagList
}

# server for the sidebar setup
setupSidebarServer <- function(id = "setupSidebar") { moduleServer( 
  id,
  
  ## module function
  function (input, output, session) {
    
    # get namespace in case you need to use it in renderUI-like functions
    ns <- session$ns
    
    ### INITIALIZATION ###
    
    # initialize main outputs from this module
    GCTs_and_params <- reactiveVal() # GCT object and corresponding parameters
    globals <- reactiveVal() # globals values for plots, displays, etc.
    
    # initialize reactiveValues with back/next logic for when user navigates
    # through each GCT file to input parameters
    backNextLogic <- reactiveValues(placeChanged = 0)
    
    # initialize reactiveVal to indicate when labels & gcts are validated + submitted
    labelsGO <- reactiveVal(0)
    gctsGO <- reactiveVal(0)
    
    # read in default settings and choices from yamls
    default_parameters <- read_yaml(system.file('setup_parameters/setupDefaults.yaml', package = 'protigyRevamp'))
    parameter_choices <- read_yaml(system.file('setup_parameters/setupChoices.yaml', package = 'protigyRevamp'))
    
    
    ### STEP 1: LABEL ASSIGNMENT ###
    
    # once files uploaded, display label assignment
    observeEvent(
      eventExpr = input$gctFiles, 
      ignoreInit = TRUE,
      handlerExpr = {
        GCTs_and_params(NULL) # reset GCT files
        labelAssignment()
      })
    
    # also display label assignment if user navigates back to it
    observeEvent(
      eventExpr = input$backToLabelsButton,
      ignoreInit = TRUE,
      handlerExpr = {labelAssignment()})
    
    # validate labels once submitted
    observeEvent(input$submitLabelsButton, {
      out <- my_shinyalert_tryCatch({
        all_labels <- sapply(input$gctFiles$name, 
                             function(n) input[[paste0('Label_', n)]])
        validate_labels(all_labels)
      }, return.error = FALSE)
      
      # increment labelsGO if labels are valid
      if (out) labelsGO(labelsGO() + 1)
    })
    
    
    ### STEP 2: INPUT GCT PARAMETERS ###
    
    # once labels assignment submitted, set values for back/next logic
    observeEvent(labelsGO(), {
      # current place in the next/back logic
      backNextLogic$place <- 1 
      
      # maximum place (i.e. the total number of GCT files)
      backNextLogic$maxPlace <- length(input$gctFiles$name) 
      
      # indicates if place or something about GCT files changed
      backNextLogic$placeChanged <- backNextLogic$placeChanged + 1 
    }, ignoreInit = TRUE)
    
    # update GCT parameters with gct file paths and labels once labels are submitted
    observeEvent(labelsGO(), {
      new_parameters <- list()
      apply(input$gctFiles, 1, function(file) {
        file <- as.list(file)
        label <- input[[paste0('Label_', file$name)]] # same inputId notation as in labelSetupUI()
        new_parameters[[label]] <<- c(gct_file_path = file$datapath,
                                      gct_file_name = file$name,
                                      default_parameters)
      })
      GCTs_and_params(list(parameters = new_parameters)) # update GCT parameters reactiveVal
    }, ignoreInit = TRUE)
    
    # display the correct GCT processing page, handling back/next logic
    observeEvent(
      eventExpr = backNextLogic$placeChanged, 
      ignoreInit = TRUE,
      handlerExpr = {
        # get the correct label for this file
        label = names(GCTs_and_params()$parameters)[backNextLogic$place]

        # main GCT processing UI
        output$sideBarMain <- renderUI({gctSetupUI(ns = ns,
                                                   label = label,
                                                   parameter_choices = parameter_choices,
                                                   parameters = GCTs_and_params()$parameters,
                                                   current_place = backNextLogic$place,
                                                   max_place = backNextLogic$maxPlace)})
        
        # left button (back to labels or just back)
        if (backNextLogic$place == 1) {
          output$leftButton <- renderUI({actionButton(ns("backToLabelsButton"), "Back")})
        } else {
          output$leftButton <- renderUI({actionButton(ns("backButton"), "Back")})
        }
        
        # right button (next or submit gct for processing)
        if (backNextLogic$place == backNextLogic$maxPlace) {
          output$rightButton <- renderUI({
            actionButton(ns("submitGCTButton"),
                         "Submit",
                         class = "btn btn-primary")})
        } else {
          output$rightButton <- renderUI({actionButton(ns("nextButton"), "Next")})
        }
    })
    
    # update parameter choices when intensity data is toggled
    current_intensity <- reactive({
      label <- names(GCTs_and_params()$parameters)[isolate(backNextLogic$place)]
      input[[paste0(label, '_intensity_data')]]})
    observeEvent(current_intensity(), {
      # first, collect all the current inputs
      collectInputs()
      
      # gather current label and parameters
      label = names(GCTs_and_params()$parameters)[backNextLogic$place]
      parameters = GCTs_and_params()$parameters[[label]]
      
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
      
      # update data filter
      updateSelectInput(
        inputId = paste0(label, '_data_filter'),
        choices = parameter_choices$data_filter[[ind]],
        selected = ifelse(
          parameters$data_filter %in% parameter_choices$data_filter[[ind]],
          parameters$data_filter,
          default_parameters$data_filter))
      
      # update max missing
      updateNumericInput(
        inputId = paste0(label, '_max_missing'),
        min = parameter_choices$max_missing[[ind]]$min,
        max = parameter_choices$max_missing[[ind]]$max,
        step = parameter_choices$max_missing[[ind]]$step,
        value = min(parameters$max_missing, parameter_choices$max_missing[[ind]]$max))
    })
    
    # change next/back buttons if applyToAll == TRUE
    observeEvent(input$applyToAll, {
      
      # change next button to submit
      if (input$applyToAll | backNextLogic$place == backNextLogic$maxPlace) {
        output$rightButton <- renderUI({actionButton(ns("submitGCTButton"), 
                                                     "Submit", 
                                                     class = "btn btn-primary")})
      } else {
        output$rightButton <- renderUI({actionButton(ns("nextButton"), "Next")})
      }
      
      # change back button to "back to labels"
      if (input$applyToAll | backNextLogic$place == 1) {
        output$leftButton <- renderUI({actionButton(ns("backToLabelsButton"), "Back")})
      } else {
        output$leftButton <- renderUI({actionButton(ns("backButton"), "Back")})
      }
      
      actionButton(ns("backToLabelsButton"), "Back")
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
      parameters <- GCTs_and_params()$parameters
      
      # call processGCTs function in a tryCatch
      GCTs <- my_shinyalert_tryCatch(
        expr = {processGCT(parameters)},
        return.error = NULL)
      
      if (!is.null(GCTs)) {
        # set GCTs_and_params reactiveVal
        GCTs_and_params(list(GCTs = GCTs, parameters = parameters)) 
        
        # increment gctsGO reactiveVal to show that processing is done
        gctsGO(gctsGO() + 1)
      }
    })
    
    
    ### STEP 4: ADVANCED SETTINGS ###
    
    # once GCT setup submitted, go to advanced settings
    observeEvent(gctsGO(), {
      labels = names(GCTs_and_params()$parameters)
      output$sideBarMain <- renderUI({advancedSettingsUI(ns = ns, labels = labels)})
      output$leftButton <- renderUI({actionButton(ns("backToLabelsButton"), "Back to setup")})
      output$rightButton <- NULL
    }, ignoreInit = TRUE)
    
    # add default -ome to globals
    observeEvent(input$default_ome, {
      current_globals <- globals()
      current_globals$default_ome <- input$default_ome
      globals(current_globals)
    })
    

    
    
    ### LOCAL HELPER FUNCTIONS ###
    # these functions interact with the session's input/output and are used in 
    # multiple observeEvent() calls, so it's easier to have them defined as
    # as local helper functions
    
    # collect user inputs, has to be used in separate observeEvent() calls
    collectInputs <- function() {
      # get the current label
      all_labels <- names(GCTs_and_params()$parameters)
      current_label <- all_labels[backNextLogic$place]
      
      # select labels for assignment
      applyToAll <- ifelse(is.null(input$applyToAll), FALSE, input$applyToAll)
      if (applyToAll) {
        assignment_labels = names(GCTs_and_params()$parameters) # all labels
      } else {
        assignment_labels <- current_label # just the current label
      }
      
      # assign outputs based on current user selections
      new_parameters <- GCTs_and_params()$parameters
      for (label in assignment_labels) {
        new_parameters[[label]]$log_transformation <- input[[paste0(current_label, '_log_transformation')]]
        new_parameters[[label]]$data_normalization <- input[[paste0(current_label, '_data_normalization')]]
        new_parameters[[label]]$data_filter <- input[[paste0(current_label, '_data_filter')]]
        new_parameters[[label]]$max_missing <- input[[paste0(current_label, '_max_missing')]]
        new_parameters[[label]]$intensity_data <- input[[paste0(current_label, '_intensity_data')]]
      }
      
      # get the current GCTs
      GCTs <- GCTs_and_params()$GCTs
      
      # assign reactiveVal
      GCTs_and_params(list(parameters = new_parameters, GCTs = GCTs)) 
    }
    
    # label assignment, has to be used in separate observeEvent() calls
    labelAssignment <- function() {
      output$sideBarMain <- renderUI({labelSetupUI(ns = ns, 
                                                   gctFileNames = input$gctFiles$name)})
      output$rightButton <- renderUI({actionButton(ns("submitLabelsButton"), 
                                                   "Submit",
                                                   class = "btn btn-primary")})
      output$leftButton <- NULL
      
      # update with saved labels if they exist
      lapply(names(GCTs_and_params()$parameters), function(label) {
        filename <- GCTs_and_params()$parameters[[label]]$gct_file_name
        updateTextInput(inputId = paste0('Label_', filename), value = label)
      })
    }
    
    # return GCTs and parameters together in one list
    return(list(GCTs_and_params = GCTs_and_params,
                globals = globals))
    
  }) # end moduleServer
} # end setupSidebarServer

