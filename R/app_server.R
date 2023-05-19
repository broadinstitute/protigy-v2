################################################################################
# SERVER
# This function contains the entire app's server logic. It calls on module
# server functions and handles any global variable logic.
################################################################################

app_server <- function(input, output, session) { 
  ## sidebar set up server
  # output: GCTs_and_params reactiveVal
  # this is a nested list with fields:
  #   $GCTs = named list of parsed and processed GCT objects
  #   $parameters = names list of input parameters from setup
  # names always correspond to GCT labels (typed by user)
  GCTs_and_params <- setupSidebarServer()
  
  
  ## module server function calls
  # heatmapTabServer()
  all_summary_plots <- summaryTabServer(GCTs_and_params = GCTs_and_params)
  
  ## export server
  all_plots <- reactive({c(all_summary_plots())})
  exportTabServer(all_plots = all_plots)
  

}


