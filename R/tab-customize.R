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
    
    # gather relevant variables from globals
    all_omes <- reactive(globals$omes)
    
    
    ## MODULE SERVER LOGIC ##
    
    custom_colors <- reactive({
      req(all_omes(), GCTs())
      
      sapply(all_omes(), 
             function(ome) set_annot_colors(GCTs()[[ome]]@cdesc),
             simplify = FALSE)
    })
    
    return(custom_colors)
    
  })
}
