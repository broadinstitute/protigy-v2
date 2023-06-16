################################################################################
# Module: TEMPLATE
################################################################################

## Steps for adding a new module
# 0a. Create a copy of the script and rename it with your new module's name
# 0b. Cmd/Ctr + F the word "template" and replace all with the module's name
# 1. add core UI and server logic here
# 2. add helper functions in the corresponding helper script
# 3. add necessary imports in protigyRevamp-package.R
# 4. call your UI function in app_UI (located in app_ui.R)
# 5. call your server function in app_server (located in app_server.R)

################################################################################
# Shiny functions (UI and server)
################################################################################

# UI for the summary tab
templateTabUI <- function(id = "templateTab") {
  ns <- NS(id) # namespace function, wrap UI inputId's with this `ns("inputId")`
  
  tagList(
    
    # TODO: add your UI elements
    
  ) # end tagList
}

# server for the summary tab
templateTabServer <- function(id = "templateTab", 
                              GCTs_and_params, 
                              globals, 
                              GCTs_original) { 
  
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
    
    # named list of default annotations for each ome
    default_annotations <- reactive({
      req(parameters())
      sapply(parameters(), function(p) p$annotation_column, simplify = FALSE)
    })
    
    # vector of all omes
    all_omes <- reactive(names(GCTs()))
    
    # gather relevant variables from globals
    default_ome <- reactive(globals$default_ome)
    custom_colors <- reactive(globals$colors)
    
    
    ## MODULE SERVER LOGIC ##
    
    # TODO: add server logic here
    
  })
}
