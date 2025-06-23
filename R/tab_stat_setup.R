################################################################################
# Module: Stat_Setup
#
# Allow users to setup the test type and parameters
################################################################################

################################################################################
# Shiny functions (UI and server)
################################################################################

source("R/tab_stat_setup_helpers.R")

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
        ),
        column(3,
               conditionalPanel(
                 condition = "input.select_test == 'Two-sample Moderated T-test'",
                 uiOutput(ns("select_contrast_ui")),
                 #uiOutput(ns("flip_contrast_ui"))
               ),
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
    
    ##Selected omes and groups
    chosen_omes_reactive <- reactive({
      input$selected_omes
    })
    
    chosen_groups_reactive <- reactive({
      input$stat_setup_annotation
    })
    
    cdesc <- reactive({
      GCTs()@cdesc
    })
    
    default_annotation_column <- reactive({
      default_annotations()[[input$selected_omes]]
    })
    
    
    ##Selecting which omes to test
    output$select_omes_ui <- renderUI({
      req(all_omes())
      req(groups_in_all_omes())
      #req(cdesc(), default_annotation_column())
      groups_in_all <- groups_in_all_omes()
      
      tagList(
        selectInput(
          inputId = ns("selected_omes"),
          label = "Select omes to test:",
          choices = all_omes(),
          multiple = TRUE,
          selected = all_omes()
        ),
        
        checkboxGroupInput(
          ns("selected_groups"),
          "Group by:",
          choices = groups_in_all,
          selected = groups_in_all
        )
        
        ##Selecting which groups to test
        # choices<- unique(cdesc()[[default_annotation_column()]]),
        # 
        # checkboxGroupInput(
        #     ns("stat_setup_annotation"),
        #     "Choose groups:",
        #     choices = choices,
        #     selected = choices
        # )
      )    
    })
    
    
    
    
    
    #Selecting the contrast- ONLY FOR TWO SAMPLE TEST
    output$select_contrast_ui <- renderUI({
      req(chosen_groups_reactive())
      chosen_groups<-chosen_groups_reactive()
      
      #Create pairwise comparisons
      pairwise_contrasts <- combn(chosen_groups, 2, FUN = function(x) paste(x[1], "over", x[2]), simplify = TRUE)
      pairwise_contrasts_2 <- combn(chosen_groups, 2, FUN = function(x) paste(x[2], "over", x[1]), simplify = TRUE)
      checkboxGroupInput("choose_contrast", "Choose contrast:", choices = c(pairwise_contrasts,pairwise_contrasts_2), selected=c(pairwise_contrasts,pairwise_contrasts_2)) 
    })
    
    
    ##Flip contrasts for the two-sample test
    # output$flip_contrast_ui <- renderUI({
    #   req(input$choose_contrast)
    #   checkbox_list <- lapply(seq_along(input$choose_contrast), function(y) {
    #     contrast <- input$choose_contrast[y]
    #     checkboxInput("flip_contrast", paste("Flip", contrast, "?"), value = FALSE)
    #   })
    #   
    #   tagList(checkbox_list)
    # })
    
  })
}
