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
      setup_multiomeHeatmapTabUI(id = ns("setup")),
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
    
    
    ## Setup ##
    gcts_merged <- setupmultiomeHeatmapTabServer(id = "setup", GCTs = GCTs)
    
    observe(print(gcts_merged()))
    
    

  }) # end moduleServer
}
