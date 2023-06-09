################################################################################
# Module: SUMMARY
# Main shiny functions (server and UI)
################################################################################


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
    default_annotations <- reactive({
      req(parameters())
      sapply(parameters(), function(p) p$annotation_column, simplify = FALSE)
    })
    
    # gather relevant variables from globals
    all_omes <- reactive(globals$omes)
    default_ome <- reactive(globals$default_ome)
    
    
    # summary plots tabs
    output$summary_plots_tabs <- renderUI({
      req(all_omes(), default_ome(), default_annotations(), GCTs())

      tabs <- lapply(all_omes(), function(ome){
        tabPanel(
          title = ome,
          
          fluidRow(
            # Data summary box
            shinydashboardPlus::box(
              tableOutput(ns(paste0(ome, "_dataset_table"))),
              title = "Dataset Information",
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              headerBorder = TRUE
            ),
            
            # Data workflow card
            shinydashboardPlus::box(
              tableOutput(ns(paste0(ome, "_workflow_table"))),
              title = "Workflow Parameters",
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              headerBorder = TRUE
            ),
          ),
          
          # Quantified features box
          fluidRow(shinydashboardPlus::box(
            plotlyOutput(ns(paste0(ome, "_quant_features_plot"))),
            sidebar = boxSidebar(
              add_css_attributes(
                selectInput(
                  ns(paste0(ome, "_quant_features_annotation")),
                  "Group by",
                  choices = names(GCTs()[[ome]]@cdesc),
                  selected = default_annotations()[[ome]]),
                classes = "small-input",
                styles = "margin-right: 10px"
              ),
              id = ns(paste0(ome, "_quant_features_sidebar")),
              width = 25,
              icon = icon("gears", class = "fa-2xl"),
              background = "rgba(91, 98, 104, 0.9)"
            ),
            status = "primary",
            width = 12,
            title = "Quantified Features",
            headerBorder = TRUE,
            solidHeader = TRUE
          )),
          
          # Missing values distribution box
          fluidRow(shinydashboardPlus::box(
            plotlyOutput(ns(paste0(ome, "_missing_value_distribution_plot"))),
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
    
    # function to compile summary workflow for a given ome
    # only call this in a reactive setting
    summary_workflow_reactive <- function(ome) {
      req(parameters(), ome %in% names(parameters())) 
      summary_workflow(params = parameters()[[ome]])
    }
    
    # render workflow info for each ome
    observeEvent(all_omes(), {
      lapply(all_omes(), function(ome) {
        output[[paste0(ome, "_workflow_table")]] <- renderTable(
          summary_workflow_reactive(ome),
          rownames = TRUE, 
          colnames = FALSE
        )
      })
    })
    
    
    ## SUMMARY DATASET INFO ##
    
    # function to compile summary dataset for a given ome
    # only call this in a reactive setting
    summary_dataset_reactive <- function(ome) {
        req(parameters(), GCTs_original(), GCTs(), 
            ome %in% names(GCTs_original()),
            ome %in% names(GCTs()),
            ome %in% names(parameters())) 
        
        summary_dataset(params = parameters()[[ome]], 
                        gct_original = GCTs_original()[[ome]],
                        gct_processed = GCTs()[[ome]])
    }
    
    # render dataset info for each ome
    observeEvent(all_omes(), {
      lapply(all_omes(), function(ome) {
        output[[paste0(ome, "_dataset_table")]] <- renderTable(
          summary_dataset_reactive(ome),
          rownames = TRUE, 
          colnames = TRUE
        )
      })
    })
    
    
    ## SUMMARY QUANTIFIED FEATURES PLOT ##
    
    # quantified features plot function
    # only to be called in a reactive setting
    summary_quant_features_reactive <- function(ome) {
      req(GCTs(), ome %in% names(GCTs()), 
          default_annotations(), ome %in% names(default_annotations()))
      
      # get annotation column
      if (paste0(ome, "_quant_features_annotation") %in% names(input)) {
        col <- input[[paste0(ome, "_quant_features_annotation")]]
      } else {
        col <- default_annotations()[[ome]]
      }
      
      print(paste("Generating plot for", ome))
      
      # generate plot
      summary_quant_features(gct = GCTs()[[ome]], 
                             col_of_interest = col,
                             ome = ome)
    }
    
    # render summary plot for each -ome
    observeEvent(all_omes(), {
      lapply(all_omes(), function(ome) {
        output[[paste0(ome, "_quant_features_plot")]] <- renderPlotly(
          ggplotly(summary_quant_features_reactive(ome))
        )
      })
    })
    
    
    ## MISSING VALUES PLOT ##
    
    # generate missing values plot for a given ome, use in reactive setting
    summary_missing_value_distribution_reactive <- function(ome) {
      req(GCTs_original(), ome %in% names(GCTs_original()),
          parameters(), ome %in% names(parameters()))
      
      summary_missing_value_distribution(
        gct = GCTs_original()[[ome]],
        missing_val_cutoff = parameters()[[ome]]$max_missing,
        ome = ome
      )
    }
    
    # render missing values plot for each -ome
    observeEvent(all_omes(), {
      lapply(all_omes(), function(ome) {
        output[[paste0(ome, "_missing_value_distribution_plot")]] <- renderPlotly({
          gg <- summary_missing_value_distribution_reactive(ome)
          summary_missing_value_distribution_to_ggplotly(gg)
        })
      })
    })
    
    
    ## COMPILE PLOTS FOR EXPORT ##
    
    all_summary_plots <- eventReactive(all_omes(), {
      # make a list with all plots for each ome
      # the names for this list are the omes
      sapply(all_omes(), function(ome) {
        list(
          quant_features_plot = summary_quant_features_reactive(ome),
          missing_values_distribution_plot = summary_missing_value_distribution_reactive(ome)
        )
      }, simplify = FALSE)

    })
    
    return(all_summary_plots)
  })
}

