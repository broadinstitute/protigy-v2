################################################################################
# SERVER
#
# This function contains the entire app's server logic. It calls on module
# server functions and handles any global variable logic.
################################################################################


app_server <- function(input, output, session) { 
  
  ## sidebar set up server
  # OUTPUT: `GCTs_and_params`, a reactiveVal list with these fields
  #   $GCTs = named list of parsed and processed GCT objects
  #   $parameters = named list of input parameters from setup
  # OUTPUT: `globals`, reactiveValues list with relevant global variables
  # OUTPUT: `GCTs_original`, reactiveVal named list with original GCTs
  # NOTE: names always correspond to GCT labels/omes (typed by user)
  sidebar_output <- setupSidebarServer(parent = session)
  GCTs_and_params <- sidebar_output$GCTs_and_params
  globals <- sidebar_output$globals
  GCTs_original <- sidebar_output$GCTs_original
  
  
  ## Customize module
  custom_colors <- customizeTabServer(GCTs_and_params = GCTs_and_params,
                                      globals = globals)
  observeEvent(custom_colors(), globals$colors <- custom_colors())
  
  
  ## Summary module
  all_summary_exports <- summaryTabServer(
    GCTs_and_params = GCTs_and_params,
    globals = globals,
    GCTs_original = GCTs_original
  )
  
  ## QC boxplots module
  all_QCBoxplots_exports <- QCBoxplots_Tab_Server(
    GCTs_and_params = GCTs_and_params,
    globals = globals,
    GCTs_original = GCTs_original
  )
  
  ## QC profile plots module
  all_QCProfilePlots_exports <- QCProfilePlots_Tab_Server(
    GCTs_and_params = GCTs_and_params,
    globals = globals,
    GCTs_original = GCTs_original
  )
  
  ## QC correlation module
  all_QCCorrelation_exports <- QCCorrelation_Tab_Server(
    GCTs_and_params = GCTs_and_params,
    globals = globals
  )
  
  ## QC PCA module
  all_QCPCA_exports <- QCPCA_Tab_Server(
    GCTs_and_params = GCTs_and_params,
    globals = globals
  )
  
  ## Statistics Setup module
  all_statSetup_exports <- statSetup_Tab_Server(
    GCTs_and_params = GCTs_and_params,
    globals = globals
  )
  
  ## Statistics Summary module
  all_statSummary_exports <- statSummary_Tab_Server(
    GCTs_and_params = GCTs_and_params,
    globals = globals,
  )
  
  ## Statistics Plot module
  all_statPlot_exports <- statPlot_Tab_Server(
    GCTs_and_params = GCTs_and_params,
    globals = globals,
  )

  # ## Statistics Table module
  # all_statTable_exports <- statTable_Tab_Server(
  #   GCTs_and_params = GCTs_and_params,
  #   globals = globals,
  #   GCTs_original = GCTs_original
  # )
  

  
  ## Multi-ome Heatmap module
  all_multiomeHeatmap_exports <- multiomeHeatmapTabServer(
    GCTs_and_params = GCTs_and_params,
    globals = globals
  )
  
  ## TEMPLATE module
  # all_template_exports <- templateSingleOme_Tab_Server(
  #   GCTs_and_params = GCTs_and_params,
  #   globals = globals,
  #   GCTs_original = GCTs_original
  # )
  
  ## gather all exports
  all_exports <- list(
      omes = reactive(c(names(GCTs_and_params()$GCTs), 'multi_ome')),
      exports = list(
        summary_exports = all_summary_exports,
        #template_exports = all_template_exports,
        QCBoxplot_exports = all_QCBoxplots_exports,
        QCProfilePlots_exports = all_QCProfilePlots_exports,
        QCCorrelation_exports = all_QCCorrelation_exports,
        QCPCA_exports = all_QCPCA_exports,
        multiomeHeatmap_exports = all_multiomeHeatmap_exports,
        statSummary_exports = all_statSummary_exports,
        statPlot_exports = all_statPlot_exports
      )
    )

  ## export tab
  exportTabServer(all_exports = all_exports, GCTs_and_params = GCTs_and_params)

}


