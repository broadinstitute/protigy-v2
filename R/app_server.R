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
  sidebar_output <- setupSidebarServer()
  
  
  ## module server function calls
  # heatmapTabServer()
  all_summary_plots <- summaryTabServer(
    GCTs_and_params = sidebar_output$GCTs_and_params,
    globals = sidebar_output$globals)
  
  ## gather all plots
  all_plots <- reactive({
    validate(need(names(sidebar_output$GCTs_and_params()$GCTs), "No GCTs"),
             need(all_summary_plots(), "summary plots"))
    list(
      omes = names(sidebar_output$GCTs_and_params()$GCTs),
      summary_plots = all_summary_plots()
    )
  })
  
  ## export tab
  exportTabServer(all_plots = all_plots)
  

}


