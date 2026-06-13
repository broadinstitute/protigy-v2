################################################################################
# PELSA container: app-level dataset switcher + active-dataset coordination
#
# The three PELSA sub-tabs (Setup / Summary / Volcano Plot) are driven by ONE
# app-level dataset switcher rather than each section building its own per-ome
# tabset. The active dataset is chosen once and persists across the sub-tabs.
#
# NAMESPACING CHOICE
#   The switcher input (`pelsa_active_dataset`) lives at the TOP LEVEL of the
#   app (app_server session scope), NOT inside any section module namespace.
#   This is deliberate: a single input must drive all three section modules,
#   which each have their own namespace. A namespaced input could not be read
#   by sibling modules.
#
#   A navbarMenu only accepts tabPanels, so the switcher bar cannot live in one
#   shared node above the tabset. Each PELSA tab therefore renders its OWN
#   switcher-bar uiOutput (`pelsa_active_dataset_bar_<suffix>`) — distinct DOM
#   ids, so no invalid duplicate-id HTML — but every bar is rendered from the
#   SAME `active_dataset` reactive and emits the SAME `pelsa_active_dataset`
#   input id. That input is the single source of truth driving all sections.
#
#   Because these functions operate at top-level session scope (they are NOT
#   wrapped in moduleServer), they use bare ids (no ns()).
#
# PHASE-5 SEAM (analyzed_datasets)
#   `pelsa_analyzed_datasets` is a reactiveVal that currently defaults to ALL
#   uploaded omes (`names(GCTs_and_params()$GCTs)`). Phase 5 (PELSA Setup) will
#   replace its contents with "the checked subset of datasets the user selected
#   in Setup" simply by calling `pelsa_analyzed_datasets(<chosen names>)`. No
#   other code needs to change: the switcher and every section already read the
#   `analyzed_datasets()` reactive derived from it.
################################################################################

# Output id for a tab's switcher bar, e.g. pelsa_active_dataset_bar_setup.
pelsa_switcher_bar_output_id <- function(suffix) {
  paste0("pelsa_active_dataset_bar_", suffix)
}

# UI: the switcher bar placed at the top of each PELSA section's Tab_UI.
# Each tab passes a unique `suffix` so the three bars get distinct DOM ids (no
# duplicate-id HTML). The bars are all rendered from the same active-dataset
# reactive and all drive the single top-level `pelsa_active_dataset` input.
#
# @param suffix  short unique tab key, e.g. "setup" / "summary" / "volcano"
pelsa_switcher_bar_UI <- function(suffix) {
  div(
    class = "pelsa-dataset-switcher",
    style = paste(
      "position: sticky; top: 0; z-index: 100;",
      "padding: 8px 4px; margin-bottom: 8px;",
      "background-color: rgba(255,255,255,0.95);"
    ),
    uiOutput(pelsa_switcher_bar_output_id(suffix))
  )
}

# Server: owns the analyzed-datasets seam, renders the switcher, and returns an
# `active_dataset()` reactive to thread into each section server.
#
# Operates at top-level session scope (NOT moduleServer), so input/output ids
# are bare. Call once from app_server() and pass its return value into each
# PELSASection*_Tab_Server(active_dataset = ...).
#
# @param input,output,session  the app_server reactive context
# @param GCTs_and_params        reactiveVal data-flow contract object
# @return a reactive resolving to the active dataset name (character scalar)
pelsaContainer_Server <- function(input, output, session, GCTs_and_params) {

  # Phase-5 seam: defaults to all uploaded omes; Phase 5 repoints this by
  # calling pelsa_analyzed_datasets(<checked subset from Setup>).
  pelsa_analyzed_datasets <- reactiveVal(NULL)

  # Keep the default in sync with uploaded omes until Phase 5 overrides it.
  observe({
    gp <- GCTs_and_params()
    if (is.null(gp) || is.null(gp$GCTs)) {
      pelsa_analyzed_datasets(NULL)
    } else {
      pelsa_analyzed_datasets(names(gp$GCTs))
    }
  })

  analyzed_datasets <- reactive({
    pelsa_analyzed_datasets()
  })

  # Build the switcher control (or NULL when < 2 datasets, so the bar is hidden
  # and the sole dataset is implicitly active). Every tab's bar renders this.
  switcher_control <- reactive({
    datasets <- analyzed_datasets()
    if (length(datasets) < 2) {
      return(NULL)
    }
    # All bars share one inputId so the input stays the single source of truth.
    # Selecting in any tab updates pelsa_active_dataset for every section.
    shinyWidgets::radioGroupButtons(
      inputId  = "pelsa_active_dataset",
      label    = NULL,
      choices  = datasets,
      selected = isolate(input$pelsa_active_dataset) %||% datasets[[1]],
      status   = "primary",
      size     = "sm"
    )
  })

  # One uiOutput per PELSA tab (distinct DOM ids), all from switcher_control().
  for (suffix in c("setup", "summary", "volcano")) {
    local({
      out_id <- pelsa_switcher_bar_output_id(suffix)
      output[[out_id]] <- renderUI(switcher_control())
    })
  }

  # Active dataset: the selected button when shown, otherwise the sole dataset.
  active_dataset <- reactive({
    datasets <- analyzed_datasets()
    req(length(datasets) >= 1)
    if (length(datasets) < 2) {
      return(datasets[[1]])
    }
    selected <- input$pelsa_active_dataset
    # Guard against a selection not present in analyzed_datasets(). This also
    # intentionally covers the brief window where input$pelsa_active_dataset
    # lags the radioGroupButtons re-render by one reactive flush (e.g. right
    # after analyzed_datasets() changes), falling back to the first dataset
    # until the input catches up.
    if (is.null(selected) || !(selected %in% datasets)) {
      return(datasets[[1]])
    }
    selected
  })

  return(active_dataset)
}
