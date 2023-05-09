################################################################################
# Module: SETUP SIDEBAR
# This code contains the UI and Server modules
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
    
    # initialize object for GCT parameters
    GCT_parameters <- reactiveValues()
    
    # read in default settings
    default_parameters <- read_yaml('src/SetupSidebar/setupDefaults.yaml')
    
    # initialize reactiveValues for back/next logic
    backNextLogic <- reactiveValues(placeChanged = 0)
    
    # code for label assignment because it has to be used in 2 separate observeEvent() calls
    labelAssignment <- function() {
      output$sideBarMain <- renderUI({labelSetupUI(ns = ns, gctFileNames = input$gctFiles$name)})
      output$rightButton <- renderUI({actionButton(ns("submitLabelsButton"), "Submit")})
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
      GCT_parameters <<- reactiveValues() # remove current entries
      apply(input$gctFiles, 1, function(file) {
        file = as.list(file)
        label <- input[[paste0('Label_', file$name)]]
        GCT_parameters[[label]] <<- do.call('reactiveValues', 
                                            c(gct_file_path = file$datapath, 
                                              gct_file_name = file$name,
                                              default_parameters))
      })
    })
    
    # display the correct GCT processing page
    observeEvent(
      eventExpr = backNextLogic$placeChanged, 
      ignoreInit = TRUE,
      handlerExpr = {
        # get the correct label for this file
        label = names(GCT_parameters)[backNextLogic$place]
        
        # main GCT processing UI
        output$sideBarMain <- renderUI({gctSetupUI(ns = ns, 
                                                   label = label,
                                                   parameters = GCT_parameters[[label]])})
        
        # left button (back to labels or just back)
        if (backNextLogic$place == 1) {
          output$leftButton <- renderUI({actionButton(ns("backToLabelsButton"), "Back")})
        } else {
          output$leftButton <- renderUI({actionButton(ns("backButton"), "Back")})
        }
        
        # right button (next or submit gct for processing)
        if (backNextLogic$place == backNextLogic$maxPlace) {
          output$rightButton <- renderUI({actionButton(ns("submitGCTButton"), "Submit")})
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
    
    # change next button to submit button if applyToAll == TRUE
    observeEvent(input$applyToAll, {
      if (input$applyToAll) {
        output$rightButton <- renderUI({actionButton(ns("submitGCTButton"), "Submit")})
      } else {
        output$rightButton <- renderUI({actionButton(ns("nextButton"), "Next")})
      }
    }) 
    
    # once GCT setup submitted, go to advanced settings
    observeEvent(input$submitGCTButton, {
      output$sideBarMain <- renderUI({advancedSettingsUI(ns = ns)})
      output$leftButton <- renderUI({actionButton(ns("backToLabelsButton"), "Back to setup")})
      output$rightButton <- NULL
    })
    
    # code to collect user inputs because it has to be used in separate observeEvent() calls
    collectInputs <- function() {
      # get the current label
      current_label <- names(GCT_parameters)[backNextLogic$place]
      
      # select labels for assignment
      if (input$applyToAll) {
        # assign to all labels
        assignment_labels = names(GCT_parameters)
      } else {
        assignment_labels <- current_label
      }
      
      # assign outputs based on current user selection
      for (label in assignment_labels) {
        GCT_parameters[[label]]$log_transform <<- input[[paste0(current_label, '_log_transform')]]
        GCT_parameters[[label]]$data_normalization <<- input[[paste0(current_label, '_data_normalization')]]
        GCT_parameters[[label]]$data_filter <<- input[[paste0(current_label, '_data_filter')]]
        GCT_parameters[[label]]$max_missing <<- input[[paste0(current_label, '_max_missing')]]
        GCT_parameters[[label]]$intensity_data <<- input[[paste0(current_label, '_intensity_data')]]
      }
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
    
    # return GCT parameters and submitGCTButton so GCT processing can happen in main server
    return(list(
      GCT_parameters = reactive(GCT_parameters),
      submitGCTButton = reactive(input$submitGCTButton)
    ))
    
  }) # end moduleServer()
  
  
} # end setupSidebarServer
    

################################################################################
# Helper UI functions
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
gctSetupUI <- function(ns, label, parameters) {
  tagList(
    p(strong(paste('Setup for', label))),
    fluidRow(column(12, selectInput(ns(paste0(label, '_intensity_data')), 
                                    'Intensity data',
                                    choices = c('Yes', 'No'),
                                    selected = parameters$intensity_data))),
    fluidRow(column(12, selectInput(ns(paste0(label, '_log_transform')),
                                    label = 'Log-transformation',
                                    choices = SETUP_LOG_TRANSFORM$choices,
                                    selected = parameters$log_transform))),
    fluidRow(column(12, selectInput(ns(paste0(label, '_data_normalization')),
                                    label = 'Data normalization',
                                    choices = SETUP_DATA_NORMALIZATION$choices,
                                    selected = parameters$data_normalization))),
    numericInput(ns(paste0(label, '_max_missing')), 
                 'Max. % missing values',
                 min = 0,
                 max = 100,
                 value = parameters$max_missing,
                 step = 1),
    fluidRow(column(12, selectInput(ns(paste0(label, '_data_filter')),
                                    label = 'Filter data',
                                    choices = SETUP_FILTER_DATA$choices,
                                    selected = parameters$data_filter))),
    checkboxInput(ns('applyToAll'), 'Apply settings to all -omes')
  )
}

# function for advanced settings UI
advancedSettingsUI <- function(ns) {
  tagList(
    p(strong("Advanced settings")),
    fluidRow(column(12, actionButton(ns('selectGroupsButton'), 'Select groups'))),
    fluidRow(column(12, actionButton(ns('customizeColorsButton'), 'Customize colors'))),
    hr()
  )
}
