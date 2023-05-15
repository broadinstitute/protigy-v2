################################################################################
# Module: SERVER
# This function contains the entire app's server logic. It calls on module
# server functions and handles any global variable logic.
################################################################################

shinyServer(function(input, output, session) { 

  ## sidebar set up server
  # output: GCTs_and_params reactiveVal
  # this is a nested list with fields:
  #   $GCTs = named list of parsed and processed GCT objects
  #   $parameters = names list of input parameters from setup
  # names always correspond to GCT labels (typed by user)
  GCTs_and_params <- setupSidebarServer()
  
  
  ## module server function calls
  heatmapTabServer()
  summaryTabServer(GCTs_and_params = GCTs_and_params)
  

})


