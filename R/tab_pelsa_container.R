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
#   switcher-bar uiOutput (`pelsa_active_dataset_bar_<suffix>`) - distinct DOM
#   ids, so no invalid duplicate-id HTML - but every bar is rendered from the
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
# @return a LIST:
#   $active_dataset         reactive resolving to the active dataset name.
#   $set_analyzed_datasets  function(datasets) - the Phase-4/5D SEAM SETTER. The
#                           Setup tab's Start-Analysis calls this with the
#                           checked subset so the switcher + sections then show
#                           ONLY the analyzed datasets. Passing NULL / character(0)
#                           restores the "all uploaded omes" default (the
#                           observe() below re-syncs on the next GCT change).
#
# BACKWARD-COMPAT (documented): the previous contract returned the bare
# active_dataset reactive. app_server now reads $active_dataset; any caller that
# only needs the active dataset uses res$active_dataset.
pelsaContainer_Server <- function(input, output, session, GCTs_and_params) {

  # Phase-5/5D seam: defaults to all uploaded omes; Start-Analysis repoints this
  # via set_analyzed_datasets(<checked subset from Setup>).
  pelsa_analyzed_datasets <- reactiveVal(NULL)

  # User-driven override flag: once Start-Analysis sets the analyzed set, the
  # auto-sync below must NOT clobber it on an unrelated reactive flush. It is
  # cleared (back to auto-sync) only when a NEW upload replaces the GCTs, which
  # is detected by a change in the uploaded ome-name signature.
  user_pinned <- reactiveVal(FALSE)
  last_upload_sig <- reactiveVal(NULL)

  # Keep the default in sync with uploaded omes until Start-Analysis overrides
  # it. A new upload (GCT ome set changes) un-pins so the default tracks fresh
  # uploads; an unrelated reactive flush leaves a user-pinned set untouched.
  observe({
    gp <- GCTs_and_params()
    sig <- if (is.null(gp) || is.null(gp$GCTs)) NULL else names(gp$GCTs)

    upload_changed <- !identical(sig, isolate(last_upload_sig()))
    if (upload_changed) {
      last_upload_sig(sig)
      user_pinned(FALSE)
    }

    if (is.null(sig)) {
      pelsa_analyzed_datasets(NULL)
    } else if (!isolate(user_pinned())) {
      pelsa_analyzed_datasets(sig)
    }
  })

  # The Phase-4 seam setter handed to the Setup tab. Validates + pins.
  set_analyzed_datasets <- function(datasets) {
    datasets <- as.character(datasets %||% character(0))
    datasets <- datasets[!is.na(datasets) & nzchar(datasets)]
    if (length(datasets) == 0L) {
      user_pinned(FALSE)
      gp <- isolate(GCTs_and_params())
      pelsa_analyzed_datasets(if (is.null(gp) || is.null(gp$GCTs)) NULL
                              else names(gp$GCTs))
      return(invisible())
    }
    user_pinned(TRUE)
    pelsa_analyzed_datasets(datasets)
    invisible()
  }

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

  # M4: the per-tab switcher bars all share the inputId `pelsa_active_dataset`
  # (the single source of truth), but they are separate widgets, so selecting on
  # one tab left the OTHER tabs' bars showing a stale highlight. Push the current
  # selection back to every bar on change so the visible state can never diverge
  # from active_dataset(). Shiny dispatches the update to all bindings whose id
  # matches, syncing all three; setting the same value re-emits nothing, so this
  # does not loop. (DOM-level sync is verified by manual app smoke, not testServer.)
  observeEvent(input$pelsa_active_dataset, {
    shinyWidgets::updateRadioGroupButtons(
      session,
      inputId  = "pelsa_active_dataset",
      selected = input$pelsa_active_dataset
    )
  }, ignoreInit = TRUE)

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

  list(
    active_dataset        = active_dataset,
    set_analyzed_datasets = set_analyzed_datasets
  )
}
