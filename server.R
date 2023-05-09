shinyServer(function(input, output, session) { 
  
  # initialize global variables
  GCT_parameters <- NULL
  GCTs <- NULL
  
  ## sidebar set up server
  # returns reactiveVals() with fields:
  # 1. GCT_parameters, reactive (a reactiveVals() list with parameter selection)
  # 2. submitGCTButton, reactive (actionButton output)
  setup_output <- setupSidebarServer()
  
  # process GCTs with options from setupSidebarServer()
  observeEvent(setup_output$submitGCTButton(), {
    # convert GCT_parameters from all reactiveValues to lists
    parameters <- reactiveValuesToList(setup_output$GCT_parameters())
    parameters <- lapply(parameters, reactiveValuesToList)
    
    # set global GCT parameters
    GCT_parameters <<- parameters
    
    # process GCT, set global variable
    GCTs <<- processGCT(GCT_parameters)
  })
  
  
  
  
  # module server function calls
  heatmapTabServer()
  summaryTabServer()
  

})