################################################################################
# Module: PELSA - Section 1 (Setup)
#
# The Setup tab of the PELSA workflow. It owns the SHARED/app-wide controls that
# configure a PELSA run plus the reactive marker-protein table:
#
#   1. Datasets to analyze   (checkboxGroupInput)
#   2. Species               (selectInput, read LIVE from inst/database/)
#   3. Treatment compound    (selectInput from compound_markers.yaml; autofills markers)
#   4. Marker paste box      (textAreaInput + "Add markers" button)
#   5. Marker reactive table (DT, columns Accession / Gene Symbol)
#                            + Remove selected / Clear all buttons
#   6. Condition grouping column + Replicate identifier column (selectInputs
#      driven by the ACTIVE dataset's cdesc)
#
# The pure, testable logic lives in tab_pelsa_section1_helpers.R; this server
# stays thin (wiring + reactivity only).
#
# SETUP-STATE OBJECT (extended by 5B/5C/5D)
#   The Tab server exposes a `setup_state` reactiveValues that downstream
#   sub-tasks read/extend:
#     setup_state$datasets       chr — checked datasets to analyze (5D drives
#                                      the container's pelsa_analyzed_datasets
#                                      off this; see SEAM below)
#     setup_state$species        chr scalar — selected species
#     setup_state$compound       chr scalar — selected treatment compound
#     setup_state$marker_rows    data.frame(accession, gene) — the marker table
#     setup_state$condition_col  chr scalar — condition grouping column (5A: shared)
#     setup_state$replicate_col  chr scalar — replicate identifier column (5A: shared)
#   It is returned alongside the export functions as
#   list(exports = <all_exports reactive>, setup_state = setup_state).
#
# DEFERRED SEAMS (documented; built later)
#   - 5B: PER-DATASET condition/replicate config + "apply to all" checkbox, and
#         the shinyjqui orderInput condition/replicate ORDERING widgets. The
#         condition/replicate selectors below are SHARED (active-dataset-driven)
#         placeholders to be upgraded per-dataset in 5B.
#   - 5C: species UniProt-refresh button + progress.
#   - 5D: Start-Analysis button + validation + withProgress + the compute
#         pipeline; accession<->gene UniProt/org.db resolution (the marker-table
#         resolver seam); and DRIVING pelsa_analyzed_datasets(setup_state$datasets).
################################################################################

################################################################################
# Tab-level UI and Server
################################################################################

PELSASection1_Tab_UI <- function(id = "PELSASection1Tab") {
  ns <- NS(id)
  tagList(
    fluidRow(uiOutput(ns("setup_box")))
  )
}

# Resolve the PELSA database directory live.
#   - installed package: system.file("database", package = "Protigy")
#   - dev/load_all:      the same call resolves to inst/database
# Returns "" when unavailable (pelsa_list_species() then yields character(0)).
# @noRd
pelsa_database_dir <- function() {
  system.file("database", package = "Protigy")
}

# Resolve the compound-marker preset yaml path live (same install/dev rule).
# @noRd
pelsa_compound_markers_path <- function() {
  system.file("pelsa", "compound_markers.yaml", package = "Protigy")
}

PELSASection1_Tab_Server <- function(id = "PELSASection1Tab",
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

    # cdesc columns of the ACTIVE dataset (drives the condition/replicate
    # selectors). 5B upgrades these to per-dataset selectors.
    active_cdesc_cols <- reactive({
      ome <- active_dataset()
      req(ome, ome %in% all_omes())
      gct <- GCTs()[[ome]]
      req(gct)
      cols <- names(gct@cdesc)
      validate(need(
        length(cols) > 0,
        "selected dataset has no sample-annotation columns"
      ))
      cols
    })

    ## SETUP STATE (extended by 5B/5C/5D) ##
    setup_state <- reactiveValues(
      datasets      = character(0),
      species       = NULL,
      compound      = NULL,
      marker_rows   = pelsa_empty_marker_rows(),
      condition_col = NULL,
      replicate_col = NULL
    )

    # Re-read the compound presets on Setup entry (and whenever the box renders)
    # so user edits to compound_markers.yaml show up without an app restart.
    compound_markers <- reactive({
      pelsa_read_compound_markers(pelsa_compound_markers_path())
    })

    ## SETUP UI ##
    output$setup_box <- renderUI({
      ome <- active_dataset()
      req(ome, ome %in% all_omes())

      datasets  <- all_omes()
      species   <- pelsa_list_species(pelsa_database_dir())
      compounds <- names(compound_markers()$compounds)
      cdesc     <- active_cdesc_cols()

      add_css_attributes(
        shinydashboardPlus::box(
          width = 12,
          title = "PELSA Setup",
          solidHeader = TRUE,
          status      = "primary",

          # 1. Datasets to analyze (FIRST control).
          checkboxGroupInput(
            ns("pelsa_datasets"),
            label    = "Datasets to analyze",
            choices  = datasets,
            selected = datasets
          ),

          # 2. Species (live list of inst/database/ subfolders). 5C adds refresh.
          selectInput(
            ns("pelsa_species"),
            label   = "Species",
            choices = species,
            selected = if (length(species)) species[[1]] else NULL
          ),

          # 3. Treatment compound (presets from compound_markers.yaml).
          #    Selecting a compound autofills the marker table (server observer).
          selectInput(
            ns("pelsa_compound"),
            label   = "Treatment compound",
            choices = c("(none)" = "", compounds)
          ),

          # 4. Marker paste box + add button.
          textAreaInput(
            ns("pelsa_marker_input"),
            label       = "Add marker proteins (accessions)",
            placeholder = "P12345 Q99999 ... (space/comma/semicolon/newline)",
            rows        = 3
          ),
          actionButton(ns("pelsa_add_markers"), "Add markers"),

          tags$hr(),

          # 5. Marker reactive table + remove/clear.
          tags$label("Marker proteins"),
          DT::dataTableOutput(ns("pelsa_marker_table")),
          div(
            style = "margin-top: 8px;",
            actionButton(ns("pelsa_remove_markers"), "Remove selected"),
            actionButton(ns("pelsa_clear_markers"), "Clear all")
          ),

          tags$hr(),

          # 6. Condition grouping + replicate identifier columns (SHARED; 5B
          #    upgrades to per-dataset + ordering widgets).
          selectInput(
            ns("pelsa_condition_col"),
            label   = "Condition grouping column",
            choices = cdesc
          ),
          selectInput(
            ns("pelsa_replicate_col"),
            label   = "Replicate identifier column",
            choices = cdesc
          ),

          # 5B SEAM: per-dataset condition/replicate ORDERING (shinyjqui
          # orderInput) goes here. Placeholder only for 5A.
          div(
            id = ns("pelsa_ordering_placeholder"),
            style = "color:#6c757d; font-style:italic; margin-top:8px;",
            "Condition / replicate ordering and per-dataset configuration ",
            "are added in a later step."
          )
        ),
        classes = c("box-no-header", "box-with-tabs")
      )
    })

    ## CONTROL -> SETUP STATE WIRING ##

    observeEvent(input$pelsa_datasets, {
      setup_state$datasets <- input$pelsa_datasets
    }, ignoreNULL = FALSE)

    observeEvent(input$pelsa_species, {
      setup_state$species <- input$pelsa_species
    }, ignoreNULL = FALSE)

    observeEvent(input$pelsa_compound, {
      setup_state$compound <- input$pelsa_compound
    }, ignoreNULL = FALSE)

    observeEvent(input$pelsa_condition_col, {
      setup_state$condition_col <- input$pelsa_condition_col
    }, ignoreNULL = FALSE)

    observeEvent(input$pelsa_replicate_col, {
      setup_state$replicate_col <- input$pelsa_replicate_col
    }, ignoreNULL = FALSE)

    ## MARKER TABLE ##
    # Backed by a reactiveVal data.frame; ALWAYS replaced wholesale (immutable),
    # never mutated in place.
    marker_rows <- reactiveVal(pelsa_empty_marker_rows())

    # Keep the exposed setup_state in sync with the marker table.
    observeEvent(marker_rows(), {
      setup_state$marker_rows <- marker_rows()
    }, ignoreNULL = FALSE)

    # Compound selection AUTOFILLS the table with that compound's preset markers
    # (accession + gene both known from the yaml), merged into existing rows.
    #
    # IMPORTANT: input$pelsa_compound re-emits its value whenever output$setup_box
    # re-renders (e.g. an active_dataset() switch recreates the selectInput). Left
    # unguarded, that would RESURRECT autofilled markers the user had cleared. We
    # therefore track the last-autofilled compound and only merge when the value
    # GENUINELY CHANGES (user picked a DIFFERENT compound). Autofilling the same
    # compound again is a no-op regardless of intervening clears.
    #
    # CHOSEN BEHAVIOR (documented): the tracker is NOT reset by "Clear all". This
    # makes echo-safety robust — a same-value re-emit after a clear (a re-render
    # echo, indistinguishable from a deliberate re-pick) will NOT resurrect the
    # cleared markers. To re-autofill the same compound, the user picks a
    # different compound and then re-picks it (a genuine change each time).
    last_autofilled_compound <- reactiveVal(NULL)
    observeEvent(input$pelsa_compound, {
      compound <- input$pelsa_compound
      if (is.null(compound) || !nzchar(compound)) return()
      if (identical(compound, last_autofilled_compound())) return()  # echo / re-pick
      new_rows <- pelsa_compound_marker_rows(compound_markers(), compound)
      marker_rows(pelsa_merge_marker_rows(marker_rows(), new_rows))
      last_autofilled_compound(compound)
    })

    # Paste box + Add: parse tokens (2J helper) -> rows (gene NA; resolver seam
    # filled in 5D) -> merged into the table.
    observeEvent(input$pelsa_add_markers, {
      tokens <- pelsa_parse_markers(input$pelsa_marker_input)
      new_rows <- pelsa_marker_rows_from_input(tokens, resolver = NULL)
      marker_rows(pelsa_merge_marker_rows(marker_rows(), new_rows))
      updateTextAreaInput(session, "pelsa_marker_input", value = "")
    })

    # Remove selected rows (immutable replace).
    observeEvent(input$pelsa_remove_markers, {
      selected <- input$pelsa_marker_table_rows_selected
      current  <- marker_rows()
      if (length(selected) == 0L || nrow(current) == 0L) return()
      keep <- setdiff(seq_len(nrow(current)), selected)
      marker_rows(current[keep, , drop = FALSE])
    })

    # Clear all. The autofill tracker is intentionally NOT reset here (see the
    # compound observer above) so a same-value re-emit cannot resurrect cleared
    # markers.
    observeEvent(input$pelsa_clear_markers, {
      marker_rows(pelsa_empty_marker_rows())
    })

    output$pelsa_marker_table <- DT::renderDataTable({
      rows <- marker_rows()
      display <- data.frame(
        Accession     = rows$accession,
        `Gene Symbol` = rows$gene,
        check.names   = FALSE,
        stringsAsFactors = FALSE
      )
      DT::datatable(
        display,
        rownames = FALSE,
        selection = "multiple",
        options = list(pageLength = 10, searching = FALSE, dom = "tip")
      )
    })

    ## EXPORTS (per-ome wiring preserved from the scaffold) ##
    # NOTE: exports must eventually recompute ALL analyzed datasets, not just the
    # active one. For now we instantiate the per-ome server for every ome so the
    # existing export wiring is preserved.
    # TODO (5D/Phase 5-7): drive this off the analyzed-datasets seam and have
    # each export function recompute its dataset from scratch; honor the
    # planning-doc memory contract (no per-ome instance leaks; keep only the
    # active dataset's heavy objects hot).
    all_exports <- reactiveVal()
    observeEvent(all_omes(), {
      ome_exports <- sapply(all_omes(), function(ome) {
        PELSASection1_Ome_Server(
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

    # Return an EXPLICIT list: $exports (the per-ome export reactiveVal consumed
    # by app_server()'s export gathering) + $setup_state (the live reactiveValues
    # contract read by Phases 5B/6/7). An attribute would drop silently on any
    # object reconstruction; setup_state is load-bearing for later phases, so it
    # is a first-class field. 5D will, e.g.:
    #   ss <- all_PELSASection1$setup_state
    #   observe(pelsa_analyzed_datasets(ss$datasets))   # the analyzed-datasets seam
    #
    # ASYMMETRY (documented): Sections 2 & 3 still return the BARE exports
    # reactiveVal — only Setup carries a setup_state companion, because only
    # Setup owns shared run-configuration state. Revisit if 2/3 grow their own.
    list(exports = all_exports, setup_state = setup_state)
  })
}

################################################################################
# Per-ome UI and Server (export wiring placeholder; analysis built in 5D)
################################################################################

PELSASection1_Ome_UI <- function(id, ome) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("section_contents"))
  )
}

PELSASection1_Ome_Server <- function(id,
                                     ome,
                                     GCT_processed,
                                     parameters,
                                     default_annotation_column,
                                     color_map) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## EXPORTS ##
    # TODO (5D): return a named list of export functions (see
    # dev/module_requirements.md).
    return(list())
  })
}
