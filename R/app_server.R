################################################################################
# SERVER
# This function contains the entire app's server logic. It calls on module
# server functions and handles any global variable logic.
################################################################################

app_server <- function(input, output, session) { 
  ## sidebar set up server
  # OUTPUT: `GCTs_and_params`, a reactiveVal list with these fields
  #   $GCTs = named list of parsed and processed GCT objects
  #   $parameters = named list of input parameters from setup
  # OUTPUT: `globals`, reactiveVal list with relevant global variables
  # OUTPUT: `GCTs_original`, reactiveVal names list with original GCTs
  # names always correspond to GCT labels/omes (typed by user)
  sidebar_output <- setupSidebarServer(parent = session)
  GCTs_and_params <- sidebar_output$GCTs_and_params
  globals <- sidebar_output$globals
  GCTs_original <- sidebar_output$GCTs_original
  
  
  ## module server function calls
  all_summary_plots <- summaryTabServer(
    GCTs_and_params = GCTs_and_params,
    globals = globals,
    GCTs_original = GCTs_original)
  
  ## gather all plots
  all_plots <- list(
      omes = reactive(names(GCTs_and_params()$GCTs)),
      plots = list(
        summary_plots = all_summary_plots
      )
    )

  ## export tab
  exportTabServer(all_plots = all_plots, GCTs_and_params = GCTs_and_params)

}


