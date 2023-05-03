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
    hr(),
    
    uiOutput(ns('sideBarMain')),
    
    fluidRow(
      column(6, uiOutput(ns('leftButton'))),
      column(6, uiOutput(ns('rightButton')))
    ),
    
    uiOutput(ns("advancedSettings"))
  ) # end tagList
}

# server for the summary tab
setupSidebarServer <- function(id = "setupSidebar") { moduleServer( 
  id,
  
  ## module function
  function (input, output, session) {
    
    # get namespace in case you need to use it in renderUI-like functions
    ns <- session$ns
    
    # once files uploaded, get label assignment
    observeEvent(input$gctFiles, {
      output$sideBarMain <- renderUI({tagList("Labels")})
      output$rightButton <- renderUI({actionButton(ns("submitLabelsButton"), "Submit")})
    })
    
    
    # once labels assignment submitted, initialize values for back/next logic
    values <- reactiveValues(placeChanged = 0)
    observeEvent(input$submitLabelsButton, {
      values$place <- 1 # place in the next/back logic
      values$maxPlace <- length(input$gctFiles$name) # maximum place
      values$placeChanged <- values$placeChanged + 1 # indicate if place or something about GCT files changed
    })
    
    # display the correct GCT processing page
    observeEvent(
      eventExpr = values$placeChanged, 
      ignoreInit = TRUE,
      handlerExpr = {
        # main GCT processing UI
        output$sideBarMain <- renderUI({tagList(paste("GCT processing options", values$place))})
        
        # left button (back to labels or just back)
        if (values$place == 1) {
          output$leftButton <- renderUI({actionButton(ns("backToLabelsButton"), "Back")})
        } else {
          output$leftButton <- renderUI({actionButton(ns("backButton"), "Back")})
        }
        
        # right button (next or submit gct for processing)
        if (values$place == values$maxPlace) {
          output$rightButton <- renderUI({actionButton(ns("submitGCTButton"), "Submit")})
        } else {
          output$rightButton <- renderUI({actionButton(ns("nextButton"), "Next")})
        }
    })
    
    # reactive to the backToLabelsButton
    observeEvent(input$backToLabelsButton, {
      output$sideBarMain <- renderUI({tagList("Labels")})
      output$rightButton <- renderUI({actionButton(ns("submitLabelsButton"), "Submit")})
    })
    
    # reactive to the next button
    observeEvent(input$nextButton, {
      if (values$place < values$maxPlace) {
        values$place <- values$place + 1
        values$placeChanged <- values$placeChanged + 1
      }
    })

    # reactive to the back button
    observeEvent(input$backButton, {
      if (values$place > 1) {
        values$place <- values$place - 1
        values$placeChanged <- values$placeChanged + 1
      }
    })
    
    # once GCT setup submitted, go to advanced settings
    observeEvent(input$submitGCTButton, {
      output$sideBarMain <- renderUI({tagList("Advanced options")})
      output$leftButton <- renderUI({actionButton(ns("backToGCTButton"), "Back")})
      output$rightButton <- NULL
    })
    
    observeEvent(input$backToGCTButton, {
      output$sideBarMain <- renderUI({tagList("Edit GCT processing")})
      output$leftButton <- renderUI({actionButton(ns("backToLabelsButton"), "Back")})
      output$rightButton <- renderUI({actionButton(ns("submitGCTButton"), "Submit")})
    })
    
    
    
    
    
    
    
    
    
    
    
    
    # # initialize once files are uploaded
    # values <- reactiveValues()
    # observeEvent(input$gctFiles,{
    #   # gct file names
    #   values$gctFileNames <- input$gctFiles$name
    # 
    #   # place in the next/back logic
    #   values$place <- 1
    # 
    #   # maximum place
    #   values$maxPlace <- length(values$gctFileNames) + 1
    # })
    # 
    # # reactive to the next button
    # observeEvent(input$nextButton, {
    #   if (values$place < values$maxPlace) values$place <- values$place + 1
    # })
    # 
    # # reactive to the back button
    # observeEvent(input$backButton, {
    #   if (values$place > 1) values$place <- values$place - 1
    # })
    # 
    # # display GCT setup with back/next logic
    # observeEvent(c(values$place, values$gctFileNames), {
    #   print(paste("place changed to", values$place))
    #   
    #   if (values$place == 1) {
    #     output$sideBarMain <- renderUI({
    #       labelSetupUI(
    #         ns = ns,
    #         gctFileNames = values$gctFileNames)
    #     })
    #   } else {
    #     output$sideBarMain <- renderUI({
    #       gctSetupUI(
    #         ns = ns,
    #         gctFileName = values$gctFileNames[values$place - 1])
    #     })
    #   }
    #   
    #   # should the next or submit button show up
    #   if (values$place < values$maxPlace) {
    #     output$rightButton <- renderUI({actionButton(ns('nextButton'), 'Next')})
    #   } else {
    #     output$rightButton <- renderUI({actionButton(ns('submitGCTButton'), 'Submit')})
    #   }
    #   
    #   # should the back button show up
    #   if (values$place > 1) {
    #     output$leftButton <- renderUI({actionButton(ns('backButton'), 'Back')})
    #   } else {
    #     output$leftButton <- NULL
    #   }
    # }) # end observeEvent
    # 
    # # display statistical testing 
    # observeEvent(input$submitGCTButton, {
    #   # remove GCT setup UI
    #   output$gctSetUp <- NULL
    #   
    #   # add statistical testing UI
    #   output$statTesting <- renderUI({ tagList(
    #     fluidRow(column(12, strong("Statistical Testing"))),
    #     fluidRow(column(12, selectInput(
    #       inputId = ns('statTestColumn'),
    #       label = "Select column for test",
    #       choices = {warning("hard coded cdesc"); names(GCTs$Prot@cdesc)}
    #     ))),
    #     fluidRow(column(12, selectInput(ns('filterData'),
    #                                     label = 'Filter data',
    #                                     choices = SETUP_FILTER_DATA$choices,
    #                                     selected = SETUP_FILTER_DATA$selected))),
    #     fluidRow(column(12, selectInput(
    #       inputId = ns('statTest'),
    #       label = "Select test",
    #       choices = SETUP_SELECT_TEST$choices,
    #       selected = SETUP_SELECT_TEST$selected
    #     )))
    #   )})
    #   
    #   # add new back button
    #   output$leftButton <- renderUI({
    #     actionButton(ns('backToSetupButton'), 'Back to setup')
    #   })
    #   
    #   # add new submit button
    #   output$rightButton <- renderUI({
    #     actionButton(ns('submitStatsButton'), 'Submit')
    #   })
    #   
    #   # add advanced settings
    #   output$advancedSettings <- renderUI({
    #     tagList(
    #       hr(), p(strong("Advanced Settings")),
    #       fluidRow(column(12, actionButton(ns('selectGroupsButton'), 'Select groups'))),
    #       fluidRow(column(12, actionButton(ns('customizeColorsButton'), 'Customize colors'))),
    #       fluidRow(column(12, selectInput(ns('filterOption'), 
    #                                       'Filter based on:',
    #                                       choices = c("nom.p","adj.p", "top.n", "none"),
    #                                       selected = "none")))
    #       
    #     )
    #   })
    # })
    
    
  })
}

# function containing setup elements for a single GCT file
gctSetupUI <- function(ns, gctFileName) {
  tagList(
    p(strong(paste('Setup for', basename(gctFileName)))),
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

# function for label assignment UI
labelSetupUI <- function(ns, gctFileNames) {
  tagList("Labels")
}

