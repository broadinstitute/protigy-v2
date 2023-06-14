################################################################################
# Module: MULTI-OME HEATMAP
################################################################################


################################################################################
# Shiny functions (server and UI)
################################################################################

## UI for heatmap tab
multiomeHeatmapTabUI <- function(id = 'multiomeHeatmapTab', GENEMAX = 20) {
  ns <- NS(id) # namespace function
  
  ## main box
  fluidRow(shinydashboardPlus::box(
    plotOutput(ns("heatmap")),
    status = "primary",
    width = 12,
    title = "Multi-Ome Heatmap",
    headerBorder = TRUE,
    solidHeader = TRUE,
    sidebar = boxSidebar(
      uiOutput(ns("sidebar_content")),
      id = "heatmap_sidebar",
      background = "rgba(91, 98, 104, 0.9)",
      startOpen = TRUE,
      icon = icon("gears", class = "fa-2xl")
    )
  )) # end box, fluidRow
}

multiomeHeatmapTabServer <- function(id = 'multiomeHeatmapTab',
                                     GCTs_and_params, 
                                     globals) {
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
    
    # gather relevant variables from globals
    custom_colors <- reactive(globals$colors)
    
    # vector of all omes
    all_omes <- reactive(names(GCTs()))
    
    
    ## Merge GCTs ##
    
    # merge the GCTs, get mat, rdesc, and sample annotations
    merged_gcts_list <- reactive(preprocess_gct_files(GCTs()))
    merged_mat <- reactive(merged_gcts_list()$merged_mat)
    merged_rdesc <- reactive(merged_gcts_list()$merged_rdesc)
    sample_anno <- reactive(merged_gcts_list()$merged_cdesc)
    
    
    ## Setup ##
    
    output$sidebar_content <- renderUI({
      setup_multiomeHeatmapTabUI(id = ns("setup"))
    })
    
    setupmultiomeHeatmapTabServer(id = "setup",
                                  GCTs = GCTs)
    
    

  }) # end moduleServer
}
