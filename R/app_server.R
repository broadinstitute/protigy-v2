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

  # Record the client's WebGL capability (reported by the app_UI probe) onto the
  # shared globals so every module can read it. Default-capable until reported:
  # webgl_capability() returns TRUE for the pre-report NULL, so capable clients
  # never flip. An explicit FALSE makes the Statistics volcano render as SVG.
  observeEvent(input$webgl_supported, {
    globals$webgl_supported <- webgl_capability(input$webgl_supported)
  }, ignoreNULL = FALSE)

  # Export reactive state for shinytest2 integration tests.
  # These are no-ops in production (shiny.testmode is FALSE by default).
  shiny::exportTestValues(
    GCTs_and_params = { GCTs_and_params() },
    globals_colors  = { globals$colors }
  )

  ## Clear all notifications functionality
  observeEvent(input$clear_all_notifications_header, {
    shinyjs::runjs("$('.shiny-notification').remove();")
  })


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
  
  ## QC CV module
  all_QCCV_exports <- QCCV_Tab_Server(
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
  stat_setup_output <- statSetup_Tab_Server(
    GCTs_and_params = GCTs_and_params,
    globals = globals,
    parent = session
  )
  
  ## Statistics Summary module
  all_statSummary_exports <- statSummary_Tab_Server(
    GCTs_and_params = GCTs_and_params,
    globals = globals,
    stat_results = stat_setup_output$stat_results,
    stat_params = stat_setup_output$stat_params
  )
  
  ## Statistics Plot module
  all_statPlot_exports <- statPlot_Tab_Server(
    GCTs_and_params = GCTs_and_params,
    globals = globals,
    stat_results = stat_setup_output$stat_results,
    stat_params = stat_setup_output$stat_params
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
  
  ## PELSA container: app-level dataset switcher + active-dataset coordination.
  ## Lives at top-level session scope (not a module) so one switcher input
  ## drives all three PELSA sections. Returns $active_dataset (reactive) +
  ## $set_analyzed_datasets (the Phase-4/5D seam setter Start-Analysis drives).
  pelsa_container <- pelsaContainer_Server(
    input = input,
    output = output,
    session = session,
    GCTs_and_params = GCTs_and_params
  )
  pelsa_active_dataset <- pelsa_container$active_dataset

  ## Shared marker-add channel: the Volcano (Section 3) requests an accession be
  # added to the marker list, the Setup module (Section 1) - the marker list's
  # single owner - observes the request and merges it. A reactiveVal holding the
  # last requested data.frame(accession, gene); Section 1 keeps removal authority.
  pelsa_marker_add_request <- reactiveVal(NULL)

  ## PELSA Section 1 module (Setup)
  # Returns list(exports = <per-ome export reactiveVal>, setup_state = <live
  # reactiveValues>, analysis = <per-dataset analysis cache reactiveVal>).
  # $exports feeds the export gathering below (unchanged contract);
  # $setup_state is the shared run-config seam Phases 5B/6/7 read;
  # $analysis is the 5D Start-Analysis cache Phases 6/7 READ (never recompute).
  # set_analyzed_datasets is threaded in so Start-Analysis can drive the
  # container's analyzed-datasets seam on success.
  all_PELSASection1 <- PELSASection1_Tab_Server(
    GCTs_and_params = GCTs_and_params,
    globals = globals,
    GCTs_original = GCTs_original,
    active_dataset = pelsa_active_dataset,
    setup_active_dataset = pelsa_container$setup_active_dataset,
    set_analyzed_datasets = pelsa_container$set_analyzed_datasets,
    marker_add_request = pelsa_marker_add_request,
    parent_session = session
  )
  all_PELSASection1_exports <- all_PELSASection1$exports
  pelsa_setup_state <- all_PELSASection1$setup_state  # consumed by Phases 5B-7
  pelsa_analysis <- all_PELSASection1$analysis        # consumed by Phases 6-7

  ## PELSA Section 2 module (Summary)
  # Reads the 5D analysis cache (pelsa_analysis) + setup_state (for the canonical
  # sample / condition ordering). NO recompute in render.
  all_PELSASection2_exports <- PELSASection2_Tab_Server(
    GCTs_and_params = GCTs_and_params,
    globals = globals,
    GCTs_original = GCTs_original,
    active_dataset = pelsa_active_dataset,
    pelsa_analysis = pelsa_analysis,
    pelsa_setup_state = pelsa_setup_state
  )

  ## PELSA Section 3 module (Volcano Plot)
  # Consumes the Statistics tab's stat_results/stat_params (Decision A: PELSA
  # does NOT recompute differential stats) + the 5D analysis cache + setup_state
  # (markers + species for feature annotation). NO recompute in render.
  all_PELSASection3_exports <- PELSASection3_Tab_Server(
    GCTs_and_params = GCTs_and_params,
    globals = globals,
    GCTs_original = GCTs_original,
    active_dataset = pelsa_active_dataset,
    stat_results = stat_setup_output$stat_results,
    stat_params = stat_setup_output$stat_params,
    pelsa_analysis = pelsa_analysis,
    pelsa_setup_state = pelsa_setup_state,
    marker_add_request = pelsa_marker_add_request
  )

  ## TEMPLATE module
  # all_template_exports <- templateSingleOme_Tab_Server(
  #   GCTs_and_params = GCTs_and_params,
  #   globals = globals,
  #   GCTs_original = GCTs_original
  # )
  
  ## PELSA: merge the three section export reactives into ONE "pelsa_exports" tab
  ## so the exporter writes a single nested tree per ome (<ome>/pelsa_exports/<stage>/
  ## ...). Each section's export functions carve their own stage subfolder; names are
  ## unique across sections (setup / qc / volcano / intensity / woods).
  all_pelsa_exports <- reactive({
    s1 <- tryCatch(all_PELSASection1_exports(), error = function(e) {
      warning("PELSA export (section 1) failed: ", conditionMessage(e)); NULL
    }) %||% list()
    s2 <- tryCatch(all_PELSASection2_exports(), error = function(e) {
      warning("PELSA export (section 2) failed: ", conditionMessage(e)); NULL
    }) %||% list()
    s3 <- tryCatch(all_PELSASection3_exports(), error = function(e) {
      warning("PELSA export (section 3) failed: ", conditionMessage(e)); NULL
    }) %||% list()
    omes <- union(union(names(s1), names(s2)), names(s3))
    stats::setNames(lapply(omes, function(o) {
      c(s1[[o]] %||% list(), s2[[o]] %||% list(), s3[[o]] %||% list())
    }), omes)
  })

  ## gather all exports
  all_exports <- list(
      omes = reactive(c(names(GCTs_and_params()$GCTs), 'multi_ome')),
      exports = list(
        summary_exports = all_summary_exports,
        #template_exports = all_template_exports,
        QCBoxplot_exports = all_QCBoxplots_exports,
        QCProfilePlots_exports = all_QCProfilePlots_exports,
        QCCV_exports = all_QCCV_exports,
        QCCorrelation_exports = all_QCCorrelation_exports,
        QCPCA_exports = all_QCPCA_exports,
        multiomeHeatmap_exports = all_multiomeHeatmap_exports,
        statSummary_exports = all_statSummary_exports,
        statPlot_exports = all_statPlot_exports,
        pelsa_exports = all_pelsa_exports
      )
    )

  ## export tab
  exportTabServer(all_exports = all_exports, GCTs_and_params = GCTs_and_params, globals = globals)

}


