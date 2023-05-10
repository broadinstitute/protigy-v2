################################################################################
# Module: SERVER
# This function contains the entire app's server logic. It calls on module
# server functions and handles any global variable logic.
################################################################################

shinyServer(function(input, output, session) { 
  
  # initialize global variables
  GCTs <- reactiveVal()
  GCT_parameters <- reactiveVal()

  
  ## sidebar set up server
  # returns list() with fields:
  # 1. GCT_parameters, reactiveVal (a named list with parameter selection)
  # 2. GCTs, reactiveVal (a named list of processed GCT files)
  setup_output <- setupSidebarServer()
  
  # assign sidebar set up outputs
  observeEvent(setup_output$GCTs(), {
    GCT_parameters(setup_output$GCT_parameters()) # reactiveVal assignment
    GCTs(setup_output$GCTs()) # reactiveVal assignment
    
    # print for now
    print(GCT_parameters())
    print(GCTs())
  })
  
  
  ## module server function calls
  heatmapTabServer()
  summaryTabServer()
  

})