################################################################################
# Module: SETUP SIDEBAR
################################################################################


################################################################################
# Shiny functions (server and UI)
################################################################################

# UI for the summary tab
setupSidebarUI <- function(id = "setupSidebar") {
  ns <- NS(id) # namespace function, wrap UI inputId's and outputId's with this (e.g. `ns(id)`)
  
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

# server for the summary tab
setupSidebarServer <- function(id = "setupSidebar") { moduleServer( 
  id,
  
  ## module function
  function (input, output, session) {
    
    # get namespace in case you need to use it in renderUI-like functions
    ns <- session$ns
    
    # initialize object containing GCT and parameters
    GCTs_and_params <- reactiveVal()
    
    # initialize reactiveValues with back/next logic for when user navigates
    # through each GCT file to input parameters
    backNextLogic <- reactiveValues(placeChanged = 0)
    labelsGO <- reactiveVal(0)
    
    # initialize reactiveVal to indicate when labels & gcts are validated + submitted
    labelsGO <- reactiveVal(0)
    gctsGO <- reactiveVal(0)
    
    # read in default settings and choices from yamls
    default_parameters <- read_yaml(system.file('setup_parameters/setupDefaults.yaml', package = 'protigyRevamp'))
    parameter_choices <- read_yaml(system.file('setup_parameters/setupChoices.yaml', package = 'protigyRevamp'))
    
    # code for label assignment because it has to be used in 2 separate observeEvent() calls
    labelAssignment <- function() {
      output$sideBarMain <- renderUI({labelSetupUI(ns = ns, gctFileNames = input$gctFiles$name)})
      output$rightButton <- renderUI({actionButton(ns("submitLabelsButton"), 
                                                   "Submit",
                                                   class = "btn btn-primary")})
      output$leftButton <- NULL
    }
    
    # once files uploaded, display label assignment
    observeEvent(
      eventExpr = input$gctFiles, 
      ignoreInit = TRUE,
      handlerExpr = {labelAssignment()})
    
    # also display label assignment if user navigates back to it
    observeEvent(
      eventExpr = input$backToLabelsButton,
      ignoreInit = TRUE,
      handlerExpr = {labelAssignment()})
    
    # validate labels once submitted
    observeEvent(input$submitLabelsButton, {
      out <- my_shinyalert_tryCatch({
        # check that each label is a valid name
        for (filename in input$gctFiles$name) {
          label = input[[paste0('Label_', filename)]]
          if (make.names(label) != label) {
            stop(paste("Invalid label for", filename))
          }
        }
        TRUE # return value if there is no error
      }, return.error = FALSE)
      
      # increment labelsGO if labels are valid
      if (out) labelsGO(labelsGO() + 1)
    })
    
    
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
                                                   parameters = GCTs_and_params()$parameters,
                                                   parameter_choices = parameter_choices,
                                                   default_parameters = default_parameters)})
        
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
    
    # change next button to submit button if applyToAll == TRUE
    observeEvent(input$applyToAll, {
      if (input$applyToAll | backNextLogic$place == backNextLogic$maxPlace) {
        output$rightButton <- renderUI({actionButton(ns("submitGCTButton"), 
                                                     "Submit", 
                                                     class = "btn btn-primary")})
      } else {
        output$rightButton <- renderUI({actionButton(ns("nextButton"), "Next")})
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
    
    # once GCT setup submitted, go to advanced settings
    observeEvent(gctsGO(), {
      labels = names(GCTs_and_params()$parameters)
      output$sideBarMain <- renderUI({advancedSettingsUI(ns = ns, labels = labels)})
      output$leftButton <- renderUI({actionButton(ns("backToLabelsButton"), "Back to setup")})
      output$rightButton <- NULL
    })
    
    # code to collect user inputs because it has to be used in separate observeEvent() calls
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
        new_parameters[[label]]$log_transform <- input[[paste0(current_label, '_log_transform')]]
        new_parameters[[label]]$data_normalization <- input[[paste0(current_label, '_data_normalization')]]
        new_parameters[[label]]$data_filter <- input[[paste0(current_label, '_data_filter')]]
        new_parameters[[label]]$max_missing <- input[[paste0(current_label, '_max_missing')]]
        new_parameters[[label]]$intensity_data <- input[[paste0(current_label, '_intensity_data')]]
      }
      GCTs_and_params(list(parameters = new_parameters)) # assign reactiveVal
    }
    
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
    
    # process GCTs 
    observeEvent(input$submitGCTButton, {
      parameters <- GCTs_and_params()$parameters
      
      # call processGCTs function in a tryCatch
      GCTs <- my_shinyalert_tryCatch(
        expr = {processGCT(parameters)},
        return.error = NULL)
      
      # set GCTs_and_params reactiveVal
      GCTs_and_params(list(GCTs = GCTs, parameters = parameters)) 
      
      # increment gctsGO reactiveVal to show that processing is done
      if (!is.null(GCTs)) gctsGO(gctsGO() + 1)
    })
    
    # return GCTs and parameters together in one list
    return(GCTs_and_params)
    
  }) # end moduleServer()
} # end setupSidebarServer





################################################################################
# Helper UI functions. Most of these are called in renderUI() functions inside 
# of setupSidebarServer()
################################################################################

# function for label assignment UI
labelSetupUI <- function(ns, gctFileNames) {
  tagList(
    p(strong('Assign labels')),
    lapply(gctFileNames, function(file) {
      textInput(inputId = ns(paste0('Label_', file)),
                label = file,
                placeholder = "Proteome or Prot")
    })
  )
}

# function containing setup elements for a single GCT file
gctSetupUI <- function(ns, label, parameters, parameter_choices, default_parameters) {
  tagList(
    p(strong(paste('Setup for', label))),
    
    ## intentisy data input
    fluidRow(column(12, selectInput(ns(paste0(label, '_intensity_data')), 
                                    'Intensity data',
                                    choices = parameter_choices$intensity_data,
                                    selected = parameters[[label]]$intensity_data))),
    
    ## log transformation input
    fluidRow(column(12, selectInput(ns(paste0(label, '_log_transform')),
                                    label = 'Log-transformation',
                                    choices = parameter_choices$log_transformation,
                                    selected = parameters[[label]]$log_transform))),
    
    ## data normalization input (2 conditional panels)
    conditionalPanel(
      condition = paste0("input['", label, "_intensity_data'] == 'Yes'"),
      fluidRow(
        column(12,
               selectInput(ns(paste0(label, '_data_normalization')),
                           label = 'Data normalization',
                           choices = parameter_choices$data_normalization$intensity_data_yes,
                           selected = parameters[[label]]$data_normalization))),
      ns = ns
    ),
    conditionalPanel(
      condition = paste0("input['", label, "_intensity_data'] == 'No'"),
      fluidRow(
        column(12, 
               selectInput(ns(paste0(label, '_data_normalization')),
                           label = 'Data normalization',
                           choices = parameter_choices$data_normalization$intensity_data_no,
                           selected = parameters[[label]]$data_normalization))),
      ns = ns
    ),
    
    ## max missing value input (2 conditional panels)
    conditionalPanel(
      condition = paste0("input['", label, "_intensity_data'] == 'Yes'"),
      fluidRow(
        column(12,
               numericInput(ns(paste0(label, '_max_missing')), 
                            'Max. % missing values',
                            min = parameter_choices$max_missing$intensity_data_yes$min,
                            max = parameter_choices$max_missing$intensity_data_yes$max,
                            value = min(c(parameters[[label]]$max_missing,
                                          parameter_choices$max_missing$intensity_data_yes$max)),
                            step = parameter_choices$max_missing$intensity_data_yes$step))),
      ns = ns
    ),
    conditionalPanel(
      condition = paste0("input['", label, "_intensity_data'] == 'No'"),
      fluidRow(
        column(12, 
               numericInput(ns(paste0(label, '_max_missing')), 
                            'Max. % missing values',
                            min = parameter_choices$max_missing$intensity_data_no$min,
                            max = parameter_choices$max_missing$intensity_data_no$max,
                            value = min(c(parameters[[label]]$max_missing,
                                          parameter_choices$max_missing$intensity_data_no$max)),
                            step = parameter_choices$max_missing$intensity_data_no$step))),
      ns = ns
    ),
    
    ## data filter input (2 conditional panels)
    conditionalPanel(
      condition = paste0("input['", label, "_intensity_data'] == 'Yes'"),
      fluidRow(
        column(12,
               selectInput(ns(paste0(label, '_data_filter')),
                           label = 'Filter data',
                           choices = parameter_choices$data_filter$intensity_data_yes,
                           selected = parameters[[label]]$data_filter))),
      ns = ns
    ),
    conditionalPanel(
      condition = paste0("input['", label, "_intensity_data'] == 'No'"),
      fluidRow(
        column(12, 
               selectInput(ns(paste0(label, '_data_filter')),
                           label = 'Filter data',
                           choices = parameter_choices$data_filter$intensity_data_no,
                           selected = parameters[[label]]$data_filter))),
      ns = ns
    ),
    
    ## apply to all checkbox
    if (length(parameters) > 1) {
      fluidRow(column(12, checkboxInput(ns('applyToAll'), 'Apply settings to all -omes')))
    }
  )
}

# function for advanced settings UI
advancedSettingsUI <- function(ns, labels) {
  tagList(
    p(strong("Advanced settings")),
    if (length(labels) > 1) {
      fluidRow(column(12, selectInput(ns('defaultOme'),
                                      "Default -ome",
                                      choices = labels)))
    },
    fluidRow(column(12, actionButton(ns('selectGroupsButton'), 'Select groups'))),
    fluidRow(column(12, actionButton(ns('customizeColorsButton'), 'Customize colors'))),
    hr()
  )
}


################################################################################
# Helper functions for the setup sidebar module
################################################################################

# function to parse, normalize, filter, etc. GCT file(s)
# INPUT: parameters list from setup 
# OUTPUT: named list of processed GCTs
processGCT <- function(parameters) {
  
  message("\nProcessing GCTs...")
  
  # parse GCTs
  GCTs <- lapply(parameters, function(p) parse_gctx(p$gct_file_path))
  
  # validate GCTs
  
  # handle intensity data?
  
  # max missing value filter
  
  # data filter
  
  # data normalization
  
  # log transformation
  
  # return processed GCT files
  message("DONE WITH GCT PROCESSING")
  return(GCTs)
}




