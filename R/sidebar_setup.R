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
    
    # initialize objects for GCT and parameters
    GCT_parameters <- reactiveVal()
    GCTs <- reactiveVal()
    
    # initialize reactiveValues for back/next logic
    backNextLogic <- reactiveValues(placeChanged = 0)
    
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
    
    # once labels assignment submitted, set values for back/next logic
    observeEvent(input$submitLabelsButton, {
      # current place in the next/back logic
      backNextLogic$place <- 1 
      
      # maximum place (i.e. the total number of GCT files)
      backNextLogic$maxPlace <- length(input$gctFiles$name) 
      
      # indicates if place or something about GCT files changed
      backNextLogic$placeChanged <- backNextLogic$placeChanged + 1 
    })
    
    # update GCT_parameters with gct file paths and labels once labels are submitted
    observeEvent(input$submitLabelsButton, {
      new_parameters <- list()
      apply(input$gctFiles, 1, function(file) {
        file <- as.list(file)
        label <- input[[paste0('Label_', file$name)]] # same inputId notation as in labelSetupUI()
        new_parameters[[label]] <<- c(gct_file_path = file$datapath,
                                      gct_file_name = file$name,
                                      default_parameters)
      })
      GCT_parameters(new_parameters) # update GCT_parameters reactiveVal
    })
    
    # display the correct GCT processing page, handling back/next logic
    observeEvent(
      eventExpr = backNextLogic$placeChanged, 
      ignoreInit = TRUE,
      handlerExpr = {
        # get the correct label for this file
        label = names(GCT_parameters())[backNextLogic$place]
        
        # main GCT processing UI
        output$sideBarMain <- renderUI({gctSetupUI(ns = ns, 
                                                   label = label,
                                                   parameters = GCT_parameters(),
                                                   parameter_choices = parameter_choices)})
        
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
    
    # reactive to the next button
    observeEvent(input$nextButton, {
      if (backNextLogic$place < backNextLogic$maxPlace) {
        backNextLogic$place <- backNextLogic$place + 1
        backNextLogic$placeChanged <- backNextLogic$placeChanged + 1
      }
    })

    # reactive to the back button
    observeEvent(input$backButton, {
      if (backNextLogic$place > 1) {
        backNextLogic$place <- backNextLogic$place - 1
        backNextLogic$placeChanged <- backNextLogic$placeChanged + 1
      }
    })
    
    # once GCT setup submitted, go to advanced settings
    observeEvent(input$submitGCTButton, {
      labels = names(GCT_parameters())
      output$sideBarMain <- renderUI({advancedSettingsUI(ns = ns, labels = labels)})
      output$leftButton <- renderUI({actionButton(ns("backToLabelsButton"), "Back to setup")})
      output$rightButton <- NULL
    })
    
    # code to collect user inputs because it has to be used in separate observeEvent() calls
    collectInputs <- function() {
      # get the current label
      current_label <- names(GCT_parameters())[backNextLogic$place]
      
      # select labels for assignment
      if (input$applyToAll) {
        assignment_labels = names(GCT_parameters()) # all labels
      } else {
        assignment_labels <- current_label # just the current label
      }

      # assign outputs based on current user selections
      new_parameters <- GCT_parameters()
      for (label in assignment_labels) {
        new_parameters[[label]]$log_transform <- input[[paste0(current_label, '_log_transform')]]
        new_parameters[[label]]$data_normalization <- input[[paste0(current_label, '_data_normalization')]]
        new_parameters[[label]]$data_filter <- input[[paste0(current_label, '_data_filter')]]
        new_parameters[[label]]$max_missing <- input[[paste0(current_label, '_max_missing')]]
        new_parameters[[label]]$intensity_data <- input[[paste0(current_label, '_intensity_data')]]
      }
      GCT_parameters(new_parameters) # assign reactiveVal
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
      GCTs(processGCT(GCT_parameters())) # set GCTs reactiveVal
    })
    
    # return GCT parameters and GCTs (both are reactiveVals)
    return(list(
      GCT_parameters = GCT_parameters,
      GCTs = GCTs
    ))
    
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
gctSetupUI <- function(ns, label, parameters, parameter_choices) {
  tagList(
    p(strong(paste('Setup for', label))),
    fluidRow(column(12, selectInput(ns(paste0(label, '_intensity_data')), 
                                    'Intensity data',
                                    choices = parameter_choices$intensity_data,
                                    selected = parameters[[label]]$intensity_data))),
    fluidRow(column(12, selectInput(ns(paste0(label, '_log_transform')),
                                    label = 'Log-transformation',
                                    choices = parameter_choices$log_transformation,
                                    selected = parameters[[label]]$log_transform))),
    fluidRow(column(12, selectInput(ns(paste0(label, '_data_normalization')),
                                    label = 'Data normalization',
                                    choices = parameter_choices$data_normalization,
                                    selected = parameters[[label]]$data_normalization))),
    fluidRow(column(12, numericInput(ns(paste0(label, '_max_missing')), 
                                     'Max. % missing values',
                                     min = parameter_choices$max_missing$min,
                                     max = parameter_choices$max_missing$max,
                                     value = parameters[[label]]$max_missing,
                                     step = parameter_choices$max_missing$step))),
    fluidRow(column(12, selectInput(ns(paste0(label, '_data_filter')),
                                    label = 'Filter data',
                                    choices = parameter_choices$data_filter,
                                    selected = parameters[[label]]$data_filter))),
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
# OUTPUT: list of processed GCTs
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
