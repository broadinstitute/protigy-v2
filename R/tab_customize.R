################################################################################
# Module: CUSTOMIZE
#
# Shiny funcions (UI and server)
################################################################################

# UI for the summary tab
customizeTabUI <- function(id = "customizeTab") {
  ns <- NS(id) # namespace function, wrap UI inputId's with this `ns("inputId")`
  
  tagList(
    
    # TODO: add your UI elements
    
  ) # end tagList
}

# server for the summary tab
customizeTabServer <- function(id = "customizeTab", GCTs_and_params, globals) { 
  
  ## module function
  moduleServer(id, function (input, output, session) {
    
    ## GATHERING INPUTS ##
    
    # get namespace in case you need to use it in renderUI-like functions
    ns <- session$ns
    
    # GCTs of individual omes to use for analysis/visualization
    GCTs <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$GCTs
    })
    
    # Large merged GCT with all omes containing `protigy.ome` column in `rdesc`
    GCTs_merged <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$GCTs_merged
    })
    
    # parameters used to process GCTs
    parameters <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$parameters
    })
    
    # all omes present
    all_omes <- reactive(names(GCTs()))
    
    
    ## MODULE SERVER LOGIC ##
    
    custom_colors <- reactive({
      req(GCTs(), GCTs_merged())
      make_custom_colors(GCTs(), GCTs_merged())
    })
    
    return(custom_colors)
    
  })
}
