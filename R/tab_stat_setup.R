################################################################################
# Module: Stat_Setup
#
# Allow users to setup the test type and parameters
################################################################################

################################################################################
# Shiny functions (UI and server)
################################################################################

source("R/tab_stat_setup_helpers.R")
#shared <- reactiveValues()

# UI for the statSetup tab
statSetup_Tab_UI <- function(id = "statSetupTab") {
  ns <- NS(id)
  tagList(
    
    fluidPage(
      titlePanel("Test Setup"),
      fluidRow(
        column(3,
               selectInput(ns("select_test"), "Select test:", c("One-sample Moderated T-test","Two-sample Moderated T-test","Moderated F test")),
        ),
        column(3,
               uiOutput(ns("select_omes_ui")),
               uiOutput(ns("select_groups_ui"))
        ),
        column(3,
               conditionalPanel(
                 condition = "input['statSetupTab-select_test'] == 'Two-sample Moderated T-test'",
                 uiOutput(ns("select_contrast_ui"))
               )
        ),
        column(3,
              actionButton(ns("run_test_button"),"Run Test"),
              uiOutput(ns("results_ui"))
        )
      )
    )
  )}



# server for the statSetup tab
# contains the structure for the big tabbed box with omes
statSetup_Tab_Server <- function(id = "statSetupTab",
                                 GCTs_and_params, 
                                 globals,
                                 GCTs_original
){ 
  
  
  ## module function
  moduleServer(id, function (input, output, session) {
    
    ## GATHERING INPUTS ##
    
    # get namespace in case you need to use it in renderUI-like functions
    ns <- session$ns
    
    # GCTs to use for analysis/visualization
    GCTs <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$GCTs
    })
    
    # parameters used to process GCTs
    parameters <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$parameters
    })
    
    # Large merged GCT with all omes containing `protigy.ome` column in `rdesc`
    GCTs_merged <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$GCTs_merged
    })
    
    # named list of default annotation columns for each ome
    default_annotations <- reactive({
      req(parameters())
      sapply(parameters(), function(p) p$annotation_column, simplify = FALSE)
    })
    
    # vector of all omes
    all_omes <- reactive(names(GCTs())) # don't remove
    
    # vector of all groups in omes
    groups_in_all_omes <- reactive({
      base::Reduce(base::intersect, lapply(GCTs(), function(gct) names(gct@cdesc)))
    })
    
    # gather relevant variables from globals
    default_ome <- reactive(globals$default_ome) # don't remove this variable!
    custom_colors <- reactive(globals$colors)
    
    ## MODULE SERVER LOGIC ##
    
    ##Selecting which omes to test
    output$select_omes_ui <- renderUI({
      req(all_omes())
        selectInput(
          inputId = ns("selected_omes"),
          label = "Select omes to test:",
          choices = all_omes(),
          multiple = TRUE,
          selected = all_omes()
        )
    })
    
    #############################################################
    chosen_omes_reactive <- reactive({
      input$selected_omes
    })
    observe({
      shared$chosen_omes <- chosen_omes_reactive()
    })
    
    cdesc <- reactive({
      req(GCTs(),input$selected_omes)
      GCTs()[[input$selected_omes[1]]]@cdesc
    })
    
    default_annotation_column <- reactive({
      req(default_annotations(),input$selected_omes)
      default_annotations()[[input$selected_omes[1]]]
    })
    
    ##############################################################
    
    #Selecting which groups to test
    output$select_groups_ui <- renderUI({
      req(cdesc(), default_annotation_column())
        
        #Selecting which groups to test
        choices<- unique(cdesc()[[default_annotation_column()]])
        
        checkboxGroupInput(
          ns("stat_setup_annotation"),
          "Choose groups:",
          choices = choices,
          selected = choices
        )
    })
    
    chosen_groups_reactive <- reactive({
      input$stat_setup_annotation
    })
    
    
    #Selecting the contrast- ONLY FOR TWO SAMPLE TEST
    output$select_contrast_ui <- renderUI({
      req(input$select_test == "Two-sample Moderated T-test")
      req(length(input$stat_setup_annotation) >= 2)
      
      #Create pairwise comparisons
      pairwise_contrasts <- combn(input$stat_setup_annotation, 2, simplify = FALSE)
      
      lapply(seq_along(pairwise_contrasts), function(i) {
        pair <- pairwise_contrasts[[i]]
        flip_id <- ns(paste0("flip_", i))
        contrast_id <- ns(paste0("contrast_", i))
        
        fluidRow(
          column(5,
            uiOutput(ns(paste0("label_contrast_", i)))
          ),
          column(5,
            checkboxInput(flip_id, "Flip?", FALSE)
          )
        )
      })
    })
    
    #Flip the contrast over if the flip box is clicked
    observe({
      req(input$select_test == "Two-sample Moderated T-test")
      pairwise_contrasts <- combn(input$stat_setup_annotation, 2, simplify = FALSE)
      
      for (i in seq_along(pairwise_contrasts)) {
        local({
          j <- i
          pair <- pairwise_contrasts[[j]]
          flip_input_id <- paste0("flip_", j)
          label_output_id <- paste0("label_contrast_", j)
          contrast_input_id <- paste0("contrast_", j)
          
          output[[label_output_id]] <- renderUI({
            flipped <- input[[flip_input_id]]
            label <- if (!is.null(flipped) && flipped) {
              paste(pair[2], "over", pair[1])
            } else {
              paste(pair[1], "over", pair[2])
            }
            checkboxInput(ns(contrast_input_id), label, TRUE)
          })
        })
      }
    })
    
    #final contrasts passed into the function
    selected_contrasts_reactive <- reactive({
      req(input$stat_setup_annotation)
      if(length(input$stat_setup_annotation) < 2) {
        return(list())  # Return empty list when fewer than 2 groups selected
      }
      
      selected_contrasts <- list()
      pairwise_contrasts <- combn(input$stat_setup_annotation, 2, simplify = FALSE)
      
      for (i in seq_along(pairwise_contrasts)) {
        pair <- pairwise_contrasts[[i]]
        flip_id <- paste0("flip_", i)
        contrast_id <- paste0("contrast_", i)
        
        if (isTRUE(input[[contrast_id]])) {
          flipped <- isTRUE(input[[flip_id]])
          contrast <- if (flipped) rev(pair) else pair
          selected_contrasts[[length(selected_contrasts) + 1]] <- contrast
        }
      }
      selected_contrasts
    })
    
    
    #The actual tests after the run test button is clicked
    observeEvent(input$run_test_button, {
      req(selected_contrasts_reactive())
      req(chosen_omes_reactive())
      req(chosen_groups_reactive())
      req(GCTs())
      req(input$select_test)
      req(default_annotation_column())
      
      # #Capture system output in a file
      # timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
      # filename <- paste0("C:/Users/dabburi/Documents/run_", timestamp, ".txt")
      # sink(filename)

      #calling the statistical testing function
      stat.results<- stat.testing(test=input$select_test, annotation_col=default_annotation_column(), chosen_omes=chosen_omes_reactive(), gct=GCTs(), chosen_groups=chosen_groups_reactive(), selected_contrasts=selected_contrasts_reactive(), intensity=FALSE)
      #stat.results<- stat.results[order(rownames(stat.results)), sort(colnames(stat.results)), drop = FALSE]
      
      if (!is.null(stat.results)) {
        assign("stat.results",stat.results,envir=.GlobalEnv)
      }
      
      #sink()
    })
    
    
  })
}
