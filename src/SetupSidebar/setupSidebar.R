################################################################################
# Module: SETUP
# This code contains the UI and Server modules
################################################################################


# UI for the summary tab
setupSidebarUI <- function(id = "setupSidebar") {
  ns <- NS(id) # namespace function, wrap UI inputID's with this ns(id)
  
  tagList(
    
    # file input
    fileInput(ns("gctFiles"), paste("Chose GCT file(s). All files should include the same samples."),
              multiple = TRUE,
              accept = ".gct"),
    
    uiOutput(ns('gctSetUp')),
    uiOutput(ns('statTesting')),
    
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
    
    # initialize once files are uploaded
    values <- reactiveValues()
    observeEvent(input$gctFiles,{
      values$gctFileNames <- input$gctFiles$name
      values$numFiles <- length(values$gctFileNames)
      values$place <- 1
    })
    
    # function containing setup elements for a single GCT file
    gctSetupUI <- function(gctFileName) {
      tagList(
        fluidRow(column(12, strong(paste('Setup for', basename(gctFileName))))),
        fluidRow(column(12, textInput(ns('label'),
                                      'Label',
                                      placeholder = 'Prot or Proteome'))),
        fluidRow(column(12, selectInput(ns('intentisyData'), 
                                        'Intensity data',
                                        choices = c('Yes', 'No'),
                                        selected = 'No'))),
        fluidRow(column(12, selectInput(ns('logTransformation'),
                                        label = 'Log-transformation',
                                        choices = SETUP_LOG_TRANSFORM$choices,
                                        selected = SETUP_LOG_TRANSFORM$selected))),
        fluidRow(column(12, selectInput(ns('dataNormalization'),
                                        label = 'Data normalization',
                                        choices = SETUP_DATA_NORMALIZATION$choices,
                                        selected = SETUP_DATA_NORMALIZATION$selected))),
        numericInput('maxMissing', 
                     'Max. % missing values',
                     min = 0,
                     max = 100,
                     value = 100,
                     step = 1),
        checkboxInput(ns('applyToAll'), 'Apply to all -omes')
      )
    }
    
    # reactive to the next button
    observeEvent(input$nextButton, {
      if (values$place == values$numFiles) return()
      values$place <- values$place + 1
    })
    
    # reactive to the back button
    observeEvent(input$backButton, {
      if (values$place == 1) return()
      values$place <- values$place - 1
    })
    
    # display GCT setup with back/next logic
    observeEvent(c(values$place, values$gctFileNames), {
      print(paste("place changed to", values$place))
      
      output$gctSetUp <- renderUI({
          gctSetupUI(values$gctFileNames[values$place])
      })
      
      # should the next or submit button show up
      if (values$place < values$numFiles) {
        output$rightButton <- renderUI({actionButton(ns('nextButton'), 'Next')})
      } else {
        output$rightButton <- renderUI({actionButton(ns('submitGCTButton'), 'Submit')})
      }
      
      # should the back button show up
      if (values$place > 1) {
        output$leftButton <- renderUI({actionButton(ns('backButton'), 'Back')})
      } else {
        output$leftButton <- NULL
      }
    }) # end observeEvent
    
    # display statistical testing 
    observeEvent(input$submitGCTButton, {
      # remove GCT setup UI
      output$gctSetUp <- NULL
      
      # add statistical testing UI
      output$statTesting <- renderUI({ tagList(
        fluidRow(column(12, strong("Statistical Testing"))),
        fluidRow(column(12, selectInput(
          inputId = ns('statTestColumn'),
          label = "Select column for test",
          choices = {warning("hard coded cdesc"); names(GCTs$Prot@cdesc)}
        ))),
        fluidRow(column(12, selectInput(ns('filterData'),
                                        label = 'Filter data',
                                        choices = SETUP_FILTER_DATA$choices,
                                        selected = SETUP_FILTER_DATA$selected))),
        fluidRow(column(12, selectInput(
          inputId = ns('statTest'),
          label = "Select test",
          choices = SETUP_SELECT_TEST$choices,
          selected = SETUP_SELECT_TEST$selected
        )))
      )})
      
      # add new back button
      output$leftButton <- renderUI({
        actionButton('backToSetupButton', 'Back to setup')
      })
      
      # add new submit button
      output$rightButton <- renderUI({
        actionButton('submitStatsButton', 'Submit')
      })
    })
    
    
  })
}
