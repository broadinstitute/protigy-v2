


# UI for the summary tab
summaryTabUI <- function(id = "summaryTab") {
  ns <- NS(id) # namespace function
  
  tagList(
    fluidRow(uiOutput(ns("summary_plots_tabs")))
  ) # end tagList
}

# server for the summary tab
summaryTabServer <- function(id = "summaryTab", GCTs_and_params, globals, GCTs_original) { 
  
  ## module function
  moduleServer(id, function (input, output, session) {
    
    # get namespace
    ns <- session$ns
    
    # gather GCTs and parameters
    GCTs <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$GCTs
    })
    parameters <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$parameters
    })
    
    # gather relevant variables from globals
    all_omes <- reactive(globals()$omes)
    default_ome <- reactive(globals()$default_ome)
    default_annotations <- reactive(globals()$default_annotations)
    
    # summary plots tabs
    output$summary_plots_tabs <- renderUI({
      req(all_omes(), default_ome(), default_annotations(), GCTs())

      tabs <- lapply(all_omes(), function(ome){
        tabPanel(
          title = ome,
          
          fluidRow(
            # Data summary box
            shinydashboardPlus::box(
              tableOutput(ns(paste0(ome, "_summary_dataset"))),
              title = "Dataset Information",
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              headerBorder = TRUE
            ),
            
            # Data workflow card
            shinydashboardPlus::box(
              tableOutput(ns(paste0(ome, "_summary_workflow"))),
              title = "Workflow Parameters",
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              headerBorder = TRUE
            ),
          ),
          
          # Quantified features box
          fluidRow(shinydashboardPlus::box(
            plotlyOutput(ns(paste0(ome, "_summary_quant_features_plot"))),
            sidebar = boxSidebar(
              add_css_attributes(
                selectInput(
                  ns(paste0(ome, "_summary_quant_features_annotation")),
                  "Group by",
                  choices = names(GCTs()[[ome]]@cdesc),
                  selected = default_annotations()[[ome]]),
                classes = "small-input",
                styles = "margin-right: 10px"
              ),
              id = ns(paste0(ome, "_quant_features_sidebar")),
              width = 25,
              icon = icon("gears", class = "fa-2xl")
            ),
            status = "primary",
            width = 12,
            title = "Quantified Features",
            headerBorder = TRUE,
            solidHeader = TRUE
          )),
          
          # Missing values distribution box
          fluidRow(shinydashboardPlus::box(
            plotlyOutput(ns(paste0(ome, "_summary_missing_value_distribution_plot"))),
            status = "primary",
            width = 12,
            title = "Missing Values",
            headerBorder = TRUE,
            solidHeader = TRUE
          ))
          
        ) # end tabPanel
      })
      
      # combine all tabs into tabSetPanel
      tab_set_panel <- do.call(tabsetPanel, c(tabs, list(id = ns("summary_plots"),
                                   selected = isolate(default_ome()))))
      
      # put everything in a box and return
      add_css_attributes(
        shinydashboardPlus::box(
          tab_set_panel,
          width = 12
        ), 
        classes = c("box-no-header", "box-with-tabs")
      )
    })
    
    # update selected tab based on default -ome
    observe({
      updateTabsetPanel(inputId = "summary_plots", selected = default_ome())
    })
    
    
    ## SUMMARY WORKFLOW ##
    
    # workflow data tables list
    summary_workflows_list <- reactive({
      req(parameters(), all_omes())
      generate_summary_workflows_list(parameters = parameters(), 
                                      all_omes = all_omes())
    })
    
    # render workflow for each -ome
    observeEvent(input$summary_plots, {
      current_ome <- input$summary_plots
      output[[paste0(current_ome, "_summary_workflow")]] <- renderTable({
        summary_workflows_list()[[current_ome]]
      }, rownames = TRUE, colnames = FALSE)
    })
    
    
    ## SUMMARY DATASET ##
    
    # dataset description list
    summary_dataset_list <- reactive({
      req(parameters(), all_omes(), GCTs_original(), GCTs()) 
      
      generate_summary_dataset_list(parameters = parameters(),
                                    all_omes = all_omes(),
                                    GCTs_original = GCTs_original(),
                                    GCTs_processed = GCTs())
    })
    
    # render dataset description for each -ome
    observeEvent(input$summary_plots, {
      current_ome <- input$summary_plots
      output[[paste0(current_ome, "_summary_dataset")]] <- renderTable({
        summary_dataset_list()[[current_ome]]
      }, rownames = TRUE, colnames = TRUE)
    })
    
    
    ## SUMMARY QUANTIFIED FEATURES PLOT ##
    
    # summary quant features plots annotations
    summary_quant_features_annotations <- reactive({
      req(default_annotations())
      
      sapply(all_omes(), function(ome) {
        if (paste0(ome, "_summary_quant_features_annotation") %in% names(input)) {
          input[[paste0(ome, "_summary_quant_features_annotation")]]
        } else {
          default_annotations()[[ome]]
        }
      }, simplify = FALSE)
    })
    
    # summary plots list
    summary_quant_features_plots_list <- reactive({
      validate(
        need(GCTs(), "GCTs not yet processed") %then%
        need(all_omes(), "Omes not avaliable") %then%
        need(summary_quant_features_annotations(), "Annotation not avaliable"))
      
      all_gcts <- GCTs()
      all_annotations <- summary_quant_features_annotations()

      sapply(all_omes(), function(ome) {
        summary.quant.features(all_gcts[[ome]], all_annotations[[ome]], ome)
      }, simplify = FALSE)
    })
    
    # render summary plot for each -ome
    observeEvent(input$summary_plots, {
      current_ome <- input$summary_plots
      output[[paste0(current_ome, "_summary_quant_features_plot")]] <- renderPlotly({
        req(summary_quant_features_plots_list()[[current_ome]])
        ggplotly(summary_quant_features_plots_list()[[current_ome]])
      })
    })
    
    
    ## MISSING VALUES PLOT ##
    
    # missing values plots list
    summary_missing_value_distribution_list <- reactive({
      validate(
        need(GCTs_original(), "GCTs not avaliable") %then%
          need(all_omes(), "Omes not avaliable") %then%
          need(parameters(), "Parameters not avaliable"))
      
      all_gcts <- GCTs_original()
      params <- parameters()
      
      sapply(all_omes(), function(ome) {
        summary_missing_value_distribution(
          gct = all_gcts[[ome]],
          missing_val_cutoff = params[[ome]]$max_missing,
          ome = ome
        )
      }, simplify = FALSE)
    })
    
    # render missing values plot for each -ome
    observeEvent(input$summary_plots, {
      current_ome <- input$summary_plots
      output[[paste0(current_ome, "_summary_missing_value_distribution_plot")]] <- renderPlotly({
        req(summary_missing_value_distribution_list()[[current_ome]])
        ggplotly(summary_missing_value_distribution_list()[[current_ome]],
                 tooltip = c())
      })
    })
    
    
    ## COMPILE PLOTS FOR EXPORT ##
    
    all_summary_plots <- reactive({
      # gather all of the lists of plots
      quant_features_plots = summary_quant_features_plots_list()
      missing_val_dist_plots <- summary_missing_value_distribution_list()
      
      # make a list with all plots for each ome
      # the names for this list are the omes
      sapply(all_omes(), function(ome) {
        list(
          quant_features_plot = quant_features_plots[[ome]],
          missing_values_distribution_plot = missing_val_dist_plots[[ome]]
        )
      }, simplify = FALSE)

    })
    
    return(all_summary_plots)
  })
}

