################################################################################
# Module: SUMMARY
# Main shiny functions (server and UI)
################################################################################


# UI for the templatePlots tab
# contains the structure for the big tabbed box with omes
summaryTabUI <- function(id = "summaryTab") {
  ns <- NS(id) # namespace function, wrap UI inputId's with this `ns("inputId")`
  
  tagList(
    
    # display omes tabs
    fluidRow(uiOutput(ns("ome_tabset_box")))
    
  ) # end tagList
}

# server for the summary tab
# contains the structure for the big tabbed box with omes
summaryTabServer <- function(id = "summaryTab",
                             GCTs_and_params, 
                             globals, 
                             GCTs_original) { 
  
  ## module function
  moduleServer(id, function (input, output, session) {
    
    ## GATHERING INPUTS ##
    
    # get namespace
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
    
    # named list of default annotation columns for each ome
    default_annotations <- reactive({
      req(parameters())
      sapply(parameters(), function(p) p$annotation_column, simplify = FALSE)
    })
    
    all_omes <- reactive(names(GCTs()))
    
    # gather relevant variables from globals
    default_ome <- reactive(globals$default_ome)
    custom_colors <- reactive(globals$colors)
    
    
    ## OME TABS ##
    
    # handles compiling ome tabs into styled tabset box
    output$ome_tabset_box <- renderUI({
      req(all_omes(), default_ome())
      
      # generate a tab for each -ome
      tabs <- lapply(all_omes(), function(ome){
        tabPanel(
          title = ome,
          
          # call the UI function for each individual ome
          summaryOmeUI(id = ns(ome))
          
        ) # end tabPanel
      }) # end lapply
      
      # combine all tabs into tabSetPanel
      tab_set_panel <- do.call(
        tabsetPanel, 
        c(tabs, list(id = ns("ome_tabs"), selected = isolate(default_ome())))
      )
      
      # put everything in a big box with ome tabs and return
      # add necessary CSS classes
      add_css_attributes(
        shinydashboardPlus::box(
          tab_set_panel,
          width = 12
        ), 
        classes = c("box-no-header", "box-with-tabs")
      )
    }) # end renderUI
    
    # update selected tab based on default -ome
    observe({
      updateTabsetPanel(inputId = "ome_tabs", selected = default_ome())
    })
    
    # call the server function for each individual ome
    all_plots <- reactiveVal() # initialize
    observeEvent(all_omes(), {
      output_plots <- sapply(all_omes(), function(ome) {
        summaryOmeServer(
          id = ome,
          ome = ome,
          GCT_processed = reactive(GCTs()[[ome]]),
          parameters = reactive(parameters()[[ome]]),
          GCT_original = reactive(GCTs_original()[[ome]]),
          default_annotation_column = reactive(default_annotations()[[ome]]),
          color_map = reactive(custom_colors())
        )
      }, simplify = FALSE)
      
      all_plots(output_plots) # set reactive value with outputs
    })
    
    return(all_plots)
  })
}



# UI for an individual ome
summaryOmeUI <- function (id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      # Dataset information box
      shinydashboardPlus::box(
        tableOutput(ns("dataset_table")),
        title = "Dataset Information",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        headerBorder = TRUE
      ),
      
      # Data workflow box
      shinydashboardPlus::box(
        tableOutput(ns("workflow_table")),
        title = "Workflow Parameters",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        headerBorder = TRUE
      ),
    ), # end fluidRow
    
    # Quantified features box
    fluidRow(shinydashboardPlus::box(
      plotlyOutput(ns("quant_features_plot")),
      sidebar = boxSidebar(
        uiOutput(ns("quant_features_sidebar_contents")),
        id = ns("quant_features_sidebar"),
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
      plotlyOutput(ns("missing_value_distribution_plot")),
      status = "primary",
      width = 12,
      title = "Missing Values",
      headerBorder = TRUE,
      solidHeader = TRUE,
      dropdownMenu = boxDropdown(
        icon = icon("question", class = "fa-xl"),
        uiOutput(ns("missing_value_distribution_help"))
      )
    ))
  )
}


# server for an individual ome
summaryOmeServer <- function(id, ome,
                             GCT_processed,
                             parameters,
                             GCT_original,
                             default_annotation_column,
                             color_map) {
  
  ## module function
  moduleServer(id, function (input, output, session) {
    
    # get namespace, use in renderUI-like functions
    ns <- session$ns
    
    ## DATASET INFO ##
    
    # reactive function for dataset info table
    dataset_info_reactive <- reactive({
      req(parameters(), GCT_original(), GCT_processed()) 
      
      summary_dataset(params = parameters(), 
                      gct_original = GCT_original(),
                      gct_processed = GCT_processed())
    })
    
    # render dataset info
    output$dataset_table <- renderTable(
      dataset_info_reactive(),
      rownames = TRUE, 
      colnames = TRUE
    )
    
    
    ## WORKFLOW INFO ##
    
    # reactive to compile summary workflow information
    workflow_info_reactive <- reactive({
      req(parameters()) 
      summary_workflow(params = parameters())
    })
    
    # render workflow info
    output$workflow_table <- renderTable(
      workflow_info_reactive(),
      rownames = TRUE, 
      colnames = FALSE
    )
    
    
    ## QUANTIFIED FEATURES PLOT ##
    
    # reactive for quantified features plot
    quant_features_plot_reactive <- reactive({
      req(GCT_processed(), default_annotation_column(), color_map())
      
      # get annotation column
      if ("quant_features_annotation" %in% names(input)) {
        annot_column <- input$quant_features_annotation
      } else {
        annot_column <- isolate(default_annotation_column())
      }
      
      print(paste("Generating plot for", ome))
      
      # get custom colors
      custom_colors <- color_map()
      if (annot_column %in% names(custom_colors)) {
        annot_color_map <- custom_colors[[annot_column]]
      } else {
        annot_color_map <- NULL
      }
      
      # generate plot
      summary_quant_features(gct = GCT_processed(), 
                             col_of_interest = annot_column,
                             ome = ome,
                             custom_color_map = annot_color_map)
    })
    
    # render summary plot
    output$quant_features_plot <- renderPlotly(
      ggplotly(quant_features_plot_reactive(), tooltip = "text")
    )
    
    # sidebar contents
    output$quant_features_sidebar_contents <- renderUI({
      add_css_attributes(
        selectInput(
          ns("quant_features_annotation"),
          "Group by",
          choices = names(GCT_processed()@cdesc),
          selected = default_annotation_column()),
        classes = "small-input",
        styles = "margin-right: 10px"
      )
    })
    
    
    ## MISSING VALUES PLOT ##
    
    # generate missing values plot
    missing_value_distribution_reactive <- reactive({
      req(GCT_original(), parameters())

      summary_missing_value_distribution(
        gct = GCT_original(),
        missing_val_cutoff = parameters()$max_missing,
        ome = ome
      )
    })
    
    # render missing values plot
    output$missing_value_distribution_plot <- renderPlotly({
      gg <- missing_value_distribution_reactive()
      summary_missing_value_distribution_to_ggplotly(gg)
    })
    
    # render help text
    output$missing_value_distribution_help <- renderUI(
      p("This is a description of the plot.", 
        style = "margin-left: 5px")
    )
    
    
    ## COMPILE PLOTS FOR EXPORT ##
    
    return(list(
      quant_features_plot = quant_features_plot_reactive(),
      missing_value_distribution = missing_value_distribution_reactive()
    ))
  })
}










