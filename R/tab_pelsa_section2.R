################################################################################
# Module: PELSA - Section 2
#
# Second section of the PELSA tab. Displays one inner tab per ome (per-ome
# tabset), following the same nested-module pattern as the QC tabs and
# tab_TEMPLATE_SINGLE-OME.R.
#
# This is currently a SCAFFOLD: the per-ome UI renders a placeholder box and the
# server returns no exports. See tab_pelsa_section1.R for the build-out steps.
################################################################################

################################################################################
# Tab-level UI and Server (handles the per-ome tabset)
################################################################################

PELSASection2_Tab_UI <- function(id = "PELSASection2Tab") {
  ns <- NS(id)
  tagList(
    fluidRow(uiOutput(ns("ome_tabset_box")))
  )
}

PELSASection2_Tab_Server <- function(id = "PELSASection2Tab",
                                     GCTs_and_params,
                                     globals,
                                     GCTs_original,
                                     active_dataset) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## GATHERING INPUTS ##

    GCTs <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$GCTs
    })

    parameters <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$parameters
    })

    default_annotations <- reactive({
      req(parameters())
      sapply(parameters(), function(p) p$annotation_column, simplify = FALSE)
    })

    all_omes      <- reactive(names(GCTs()))   # don't remove
    default_ome   <- reactive(globals$default_ome) # don't remove
    custom_colors <- reactive(globals$colors)

    ## ACTIVE-DATASET VIEW ##
    # The app-level switcher (R/tab_pelsa_container.R) chooses which dataset is
    # active. This section renders a single-dataset view for active_dataset()
    # only (not a per-ome tabset). Phases 5-7 replace the placeholder body.

    output$ome_tabset_box <- renderUI({
      ome <- active_dataset()
      req(ome, ome %in% all_omes())
      add_css_attributes(
        shinydashboardPlus::box(
          PELSASection2_Ome_UI(id = ns(ome), ome = ome),
          width = 12
        ),
        classes = c("box-no-header", "box-with-tabs")
      )
    })

    # Call per-ome server and collect export functions.
    # NOTE: exports must eventually recompute ALL analyzed datasets, not just
    # the active one. For now we instantiate the per-ome server for every ome so
    # the existing export wiring is preserved.
    # TODO (Phase 5-7): drive this off the analyzed-datasets seam and have each
    # export function recompute its dataset from scratch. Beyond that, the
    # memory contract from the planning doc must be honored:
    #   (a) do NOT re-instantiate / leak per-ome (or per-dataset) module
    #       instances on every all_omes()/analyzed_datasets() change — create
    #       each instance once and reuse it; and
    #   (b) keep ONLY the active dataset's heavy objects "hot": free the
    #       previous active dataset's heavy objects on switch and lazily compute
    #       only the active dataset's. (Exports remain the exception — they
    #       recompute all analyzed datasets from scratch at export time.)
    all_exports <- reactiveVal()
    observeEvent(all_omes(), {
      ome_exports <- sapply(all_omes(), function(ome) {
        PELSASection2_Ome_Server(
          id                        = ome,
          ome                       = ome,
          GCT_processed             = reactive(GCTs()[[ome]]),
          parameters                = reactive(parameters()[[ome]]),
          default_annotation_column = reactive(default_annotations()[[ome]]),
          color_map                 = reactive(custom_colors()[[ome]])
        )
      }, simplify = FALSE)
      all_exports(ome_exports)
    })

    return(all_exports)
  })
}

################################################################################
# Per-ome UI and Server (content of a single ome tab)
################################################################################

PELSASection2_Ome_UI <- function(id, ome) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("section_contents"))
  )
}

PELSASection2_Ome_Server <- function(id,
                                     ome,
                                     GCT_processed,
                                     parameters,
                                     default_annotation_column,
                                     color_map) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## MODULE SERVER LOGIC ##
    # TODO: build out Section 2 analysis for a single ome here.

    output$section_contents <- renderUI({
      pelsa_placeholder_box(
        ns      = ns,
        ome     = ome,
        title   = "PELSA - Section 2",
        message = "This section will house the second part of the PELSA workflow."
      )
    })

    ## EXPORTS ##
    # TODO: return a named list of export functions (see dev/module_requirements.md).
    return(list())
  })
}
