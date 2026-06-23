################################################################################
# Module: PELSA - Section 1 (Setup)
#
# The Setup tab of the PELSA workflow. It is configured PER DATASET: a dedicated
# switcher (setup_active_dataset, over ALL uploaded omes) selects which dataset's
# form is shown, and EVERYTHING is per-ome. The form for the active dataset:
#
#   0. Skip toggle           (checkboxInput "Skip PELSA analysis for this
#                            dataset") - when on, the rest of the form is greyed
#                            out (.pelsa-skipped on the form wrapper). ALL of the
#                            dataset's per-ome config (species/compound/markers/
#                            condition+replicate columns/orders) is PRESERVED, so
#                            un-skipping restores it unchanged - skipping only
#                            excludes the dataset from the run and from the
#                            Summary/Volcano switcher. Per-dataset state is pruned
#                            ONLY when a new upload removes the dataset.
#   1. FASTA + annotation    (per-dataset fileInputs; a self-curated checkbox
#                            greys out the annotation uploader) - PER-OME.
#   2. Treatment compound    (selectInput from compound_markers.yaml; autofills
#                            THIS dataset's markers) - PER-OME.
#   3. Marker paste box      (textAreaInput + "Add markers") - PER-OME.
#   4. Marker reactive table (DT) + Remove selected / Clear all - PER-OME.
#   5. Condition/replicate configuration: condition-grouping + replicate-
#      identifier selectInputs (default "(none)"), a draggable condition-ORDER
#      widget, and one draggable replicate-order widget per condition.
#   6. "Apply this dataset's setup to all others" BUTTON: copies the active tab's
#      species/compound/markers verbatim + condition/replicate columns+order
#      (best-effort) to every other NON-SKIPPED dataset.
#
# The pure, testable logic lives in tab_pelsa_section1_helpers.R; this server
# stays thin (wiring + reactivity only).
#
# SETUP-STATE OBJECT (the documented contract read/extended by 6/7)
#   The Tab server exposes a `setup_state` reactiveValues. EVERY field except
#   `datasets` is a NAMED LIST keyed by dataset name (ome):
#     setup_state$datasets       chr - the NON-SKIPPED (analyzed) omes, stamped at
#                                      Start-Analysis (drives the container's
#                                      pelsa_analyzed_datasets via
#                                      set_analyzed_datasets; see SEAM below)
#     setup_state$fasta_path[[ds]]      chr - uploaded FASTA temp path
#     setup_state$fasta_name[[ds]]      chr - original uploaded FASTA filename
#     setup_state$annotation_path[[ds]] chr - uploaded annotation temp path (NULL
#                                            when self-curated / not yet uploaded)
#     setup_state$annotation_name[[ds]] chr - original annotation filename
#     setup_state$self_curated[[ds]]    logical - TRUE = self-curated DB (first-
#                                            token FASTA parse, no annotation file)
#     setup_state$compound[[ds]]     chr scalar - selected treatment compound
#     setup_state$marker_rows[[ds]]  data.frame(accession, gene) - marker table
#     setup_state$skip[[ds]]         logical - TRUE = skip this dataset
#     setup_state$condition_col[[ds]]   chr scalar - condition grouping column
#     setup_state$replicate_col[[ds]]   chr scalar - replicate identifier column
#     setup_state$condition_order[[ds]] chr - chosen order of that ds's conditions
#     setup_state$replicate_order[[ds]][[cond]] chr - chosen sample order within
#                                            each condition (named by condition)
#     setup_state$sample_order[[ds]]    chr - the CANONICAL ordered sample-name
#                                            vector (column order downstream
#                                            plots respect), built by
#                                            pelsa_build_sample_order().
#   It is returned alongside the export functions as a REACTIVE snapshot:
#   list(exports = <all_exports>, setup_state = reactive(pelsa_setup_snapshot(...)),
#   analysis = <pelsa_analysis>). The consumers (2/3) is.function()-guard the seam
#   and CALL it with (); a bare reactiveValues fails that guard, so the producer
#   wraps the snapshot in reactive() (see the return-block comment for the seam
#   contract).
#
#   APPLY-ALL SOURCE: the ACTIVE setup tab. Uploaded FASTA/annotation/compound/
#   markers copy verbatim; condition/replicate columns + condition order copy
#   best-effort (only where the target's cdesc has those columns) to NON-SKIPPED
#   targets.
#
# UPLOADS ARE PER-DATASET: Start-Analysis resolves each dataset's FASTA +
#   annotation file PER DATASET (memoized per ds in pelsa_run_analysis) from the
#   uploaded paths; a self-curated dataset parses its FASTA first-token and uses
#   an empty feature frame (no annotation file).
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

# pelsa_database_dir() + pelsa_compound_markers_path() live in
# tab_pelsa_section1_helpers.R (pure path resolvers, shared with Section 3).

PELSASection1_Tab_Server <- function(id = "PELSASection1Tab",
                                     GCTs_and_params,
                                     globals,
                                     GCTs_original,
                                     active_dataset,
                                     setup_active_dataset = NULL,
                                     set_analyzed_datasets = NULL,
                                     marker_add_request = NULL,
                                     parent_session = NULL) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # The Setup page has its OWN dataset switcher over ALL uploaded datasets
    # (pelsa_setup_active_dataset), independent of the analyzed-only switcher the
    # Summary/Volcano sections share (active_dataset). When not supplied (older
    # callers / isolated tests), fall back to active_dataset so the module still
    # works standalone.
    setup_active_dataset <- if (is.function(setup_active_dataset)) {
      setup_active_dataset
    } else {
      active_dataset
    }

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

    ## SETUP STATE (the documented per-dataset contract; see header) ##
    # SHARED scalars + per-dataset NAMED LISTS keyed by dataset (ome).
    setup_state <- reactiveValues(
      datasets        = character(0), # NON-SKIPPED omes (set at Start-Analysis)
      fasta_path      = list(),  # [[ds]] -> uploaded FASTA temp path
      fasta_name      = list(),  # [[ds]] -> original uploaded FASTA filename
      annotation_path = list(),  # [[ds]] -> uploaded annotation temp path (NULL if none)
      annotation_name = list(),  # [[ds]] -> original annotation filename
      self_curated    = list(),  # [[ds]] -> logical (TRUE = self-curated, no annotation)
      compound        = list(),  # [[ds]] -> chr scalar
      marker_rows     = list(),  # [[ds]] -> data.frame(accession, gene)
      skip            = list(),  # [[ds]] -> logical (TRUE = skip this dataset)
      condition_col   = list(),  # [[ds]] -> chr scalar
      replicate_col   = list(),  # [[ds]] -> chr scalar
      condition_order = list(),  # [[ds]] -> chr (condition order)
      replicate_order = list(),  # [[ds]] -> list(cond -> chr sample order)
      sample_order    = list()   # [[ds]] -> chr (canonical sample order)
    )

    # Bumping this forces compound_markers() to re-read the YAML (after an in-app
    # add-compound / set-default write) without re-rendering the whole Setup box.
    compound_markers_version <- reactiveVal(0)

    # Re-read the compound presets on Setup entry, whenever the box renders, and
    # whenever a write bumps the version, so user edits show up without a restart.
    compound_markers <- reactive({
      compound_markers_version()
      pelsa_read_compound_markers(pelsa_compound_markers_path())
    })

    ## SETUP UI ##
    # The Setup box's PURE markup lives in pelsa_setup_box_ui() (helpers); this
    # renderUI just gates on a valid active dataset, gathers the live choices,
    # and delegates. The per-dataset config (5B) + marker table render into the
    # uiOutput/DT placeholders the builder emits.
    output$setup_box <- renderUI({
      ome <- setup_active_dataset()
      req(ome, ome %in% all_omes())

      # Seed the recreated inputs from the persisted setup_state (isolated, so
      # reading it does not add a reactive dependency). Without this, an
      # active-dataset switch re-renders the box at hardcoded defaults, whose
      # re-emitted values clobber setup_state$compound/self_curated via the
      # control observers below. Defaults (first load) fall back when unset.
      # compound defaults to the blank "(none)" so the user must consciously
      # choose (and the validator can flag a blank). skip + self_curated default
      # to FALSE.
      sel_compound <- isolate(setup_state$compound[[ome]]) %||% ""
      sel_skip     <- isTRUE(isolate(setup_state$skip[[ome]]))
      sel_self_cur <- isTRUE(isolate(setup_state$self_curated[[ome]]))

      pelsa_setup_box_ui(
        # isolate(): the dropdown is driven by a targeted updateSelectInput after
        # a write, so renderUI must NOT take a dependency on compound_markers()
        # (that would re-render the entire Setup box on every preset write).
        compounds = isolate(names(compound_markers()$compounds)),
        ns        = ns,
        selected_compound = sel_compound,
        selected_skip     = sel_skip,
        self_curated      = sel_self_cur
      )
    })

    ## CONTROL -> SETUP STATE WIRING (PER-DATASET) ##
    # Every setup control now writes the ACTIVE setup dataset's per-ome slot.
    # set_ds (defined below in the per-dataset section) is referenced here; it is
    # only CALLED from inside reactive observers, which fire after the whole
    # module body has run, so the forward reference resolves fine.

    # The active setup dataset (the tab whose form is on screen). NULL-safe.
    active_setup_ome <- reactive({
      ome <- setup_active_dataset()
      if (is.null(ome) || !(ome %in% all_omes())) NULL else ome
    })

    # This ome's marker rows (always a data.frame; empty when unset).
    cur_markers <- function(ome) {
      mr <- setup_state$marker_rows[[ome]]
      if (is.null(mr) || !is.data.frame(mr)) pelsa_empty_marker_rows() else mr
    }
    set_markers <- function(ome, rows) set_ds("marker_rows", ome, rows)

    # Shared message when a preset write fails (read-only package library). Note:
    # ASCII-only; the repo link lets the user clone + run from source to persist
    # presets. Reused by the add-compound and set-default handlers.
    pelsa_readonly_save_msg <- paste0(
      "Could not save the preset: the package library is not writable. ",
      "Run Protigy from the source tree (devtools::load_all) to manage presets. ",
      "Source: https://github.com/broadinstitute/protigy-v2.git"
    )

    # Per-dataset upload wiring: each uploader writes the ACTIVE dataset's slot.
    observeEvent(input$pelsa_fasta, {
      ome <- active_setup_ome(); req(ome)
      set_ds("fasta_path", ome, pelsa_fileinput_path(input$pelsa_fasta))
      set_ds("fasta_name", ome, pelsa_fileinput_name(input$pelsa_fasta))
    })
    observeEvent(input$pelsa_annotation, {
      ome <- active_setup_ome(); req(ome)
      set_ds("annotation_path", ome, pelsa_fileinput_path(input$pelsa_annotation))
      set_ds("annotation_name", ome, pelsa_fileinput_name(input$pelsa_annotation))
    })
    observeEvent(input$pelsa_self_curated, {
      ome <- active_setup_ome(); req(ome)
      set_ds("self_curated", ome, isTRUE(input$pelsa_self_curated))
      # Grey out / re-enable the annotation uploader to match.
      shinyjs::toggleState("pelsa_annotation_wrap",
                           condition = !isTRUE(input$pelsa_self_curated))
    }, ignoreNULL = FALSE)

    # NOTE: input$pelsa_compound is handled by a SINGLE merged observer further
    # down (record + marker autofill, both under the per-ome echo guard), so a
    # setup-box re-render re-emit cannot redundantly re-record or clobber edits.

    # Skip toggle: write the per-ome flag. Greying of the rest of the form is
    # handled by the observe() below (CSS class toggle).
    observeEvent(input$pelsa_skip, {
      ome <- active_setup_ome(); req(ome)
      set_ds("skip", ome, isTRUE(input$pelsa_skip))
    }, ignoreNULL = FALSE)

    # Grey out (disable interaction with) the rest of the form when this dataset
    # is skipped. A CSS class on the wrapper covers orderInputs + the DT that a
    # bare :input selector would miss; state underneath is PRESERVED (purely
    # visual / interaction disable), so un-skipping restores the config.
    observe({
      shinyjs::toggleClass(id = "pelsa_setup_form", class = "pelsa-skipped",
                           condition = isTRUE(input$pelsa_skip))
    })

    ## MARKER TABLE (PER-DATASET) ##
    # Markers are per-ome: each setup tab owns its own marker list. The compound
    # autofill, paste box, remove/clear, and the cross-module add-request all
    # read/write setup_state$marker_rows[[active ome]].

    # Compound selection REPLACES this ome's table with the compound's preset
    # markers (a genuine reselect wipes any prior rows; "(none)" clears).
    #
    # ECHO GUARD (per-ome): input$pelsa_compound re-emits whenever output$setup_box
    # re-renders (e.g. a setup-tab switch recreates the selectInput). Left
    # unguarded, that would CLOBBER markers the user edited after autofill. We
    # track the last-applied compound PER OME and only act when the value
    # genuinely changes for THIS ome. The tracker is NOT reset by "Clear all".
    last_autofilled_compound <- reactiveVal(list())  # [[ome]] -> compound
    observeEvent(input$pelsa_compound, {
      ome <- active_setup_ome(); req(ome)
      compound <- input$pelsa_compound
      tracker  <- last_autofilled_compound()
      # Skip when unchanged FOR THIS OME, so a re-emit cannot clobber edits or
      # redundantly re-record the compound.
      if (identical(compound, tracker[[ome]])) return()

      # Record the per-dataset compound selection (merged from the former
      # standalone observer so record + autofill stay in lockstep under the same
      # per-ome guard).
      set_ds("compound", ome, compound)

      if (is.null(compound) || !nzchar(compound)) {
        # "(none)" -> clear the table entirely.
        set_markers(ome, pelsa_empty_marker_rows())
      } else {
        # A genuine reselect REPLACES the table with this compound's presets
        # (a brand-new compound has none -> empty table).
        new_rows <- pelsa_compound_marker_rows(compound_markers(), compound)
        set_markers(ome, new_rows)
      }
      tracker[[ome]] <- compound
      last_autofilled_compound(tracker)
    })

    # Paste box + Add: parse tokens -> rows (gene NA) -> merged into this ome.
    observeEvent(input$pelsa_add_markers, {
      ome <- active_setup_ome(); req(ome)
      tokens <- pelsa_parse_markers(input$pelsa_marker_input)
      new_rows <- pelsa_marker_rows_from_input(tokens, resolver = NULL)
      set_markers(ome, pelsa_merge_marker_rows(cur_markers(ome), new_rows))
      updateTextAreaInput(session, "pelsa_marker_input", value = "")
    })

    # Add a brand-new compound (empty preset) and persist it to the YAML. On
    # success the dropdown is updated + the new compound selected (which the
    # existing pelsa_compound observers then persist + autofill-empty); on a
    # read-only library the write fails and we surface the actionable error.
    observeEvent(input$pelsa_add_compound_btn, {
      req(active_setup_ome())
      v <- pelsa_validate_compound_name(input$pelsa_new_compound)
      if (!isTRUE(v$ok)) {
        showNotification(v$message, type = "warning", duration = 5)
        return()
      }
      cm <- compound_markers()
      if (pelsa_compound_exists(cm, v$name)) {
        # Block + select the existing one (by its primary key) so the user can
        # edit its markers instead.
        existing <- .pelsa_resolve_compound_name(cm, v$name)
        showNotification(
          sprintf("Compound '%s' already exists.", existing),
          type = "warning", duration = 5
        )
        updateSelectInput(session, "pelsa_compound", selected = existing)
        return()
      }
      new_cm <- pelsa_add_compound(cm, v$name)
      ok <- pelsa_write_compound_markers(pelsa_compound_markers_path(), new_cm)
      if (!ok) {
        showNotification(pelsa_readonly_save_msg, type = "error", duration = 10)
        return()
      }
      # Re-read, refresh the dropdown choices, select the new compound (rides the
      # existing pelsa_compound observers), and clear the text field.
      compound_markers_version(compound_markers_version() + 1)
      choices <- c("(none)" = "", names(compound_markers()$compounds))
      updateSelectInput(session, "pelsa_compound",
                        choices = choices, selected = v$name)
      updateTextInput(session, "pelsa_new_compound", value = "")
      showNotification(sprintf("Added compound '%s'.", v$name),
                       type = "message", duration = 4)
    })

    # "Set as default marker list for this compound": opens a confirm modal that
    # names the compound + the marker count, then (on confirm) rewrites that
    # compound's preset in the YAML to the table's CURRENT markers (full replace,
    # empty table allowed = clears the preset).
    observeEvent(input$pelsa_set_default_markers_btn, {
      ome <- active_setup_ome(); req(ome)
      compound <- input$pelsa_compound
      if (is.null(compound) || !nzchar(compound)) {
        showNotification("Select a compound first.", type = "warning",
                         duration = 5)
        return()
      }
      n <- nrow(cur_markers(ome))
      showModal(modalDialog(
        title = "Set default marker list",
        sprintf(
          paste0("This will replace the saved preset for '%s' with the %d ",
                 "marker(s) currently in the table. This rewrites ",
                 "compound_markers.yaml. Continue?"),
          compound, n
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("pelsa_confirm_set_default"), "Confirm",
                       class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    # Confirm: write the current table as the selected compound's preset. The
    # confirm button id is namespaced via session$ns() in the modal markup, but
    # input$ references it BARE (the module ns() rule).
    observeEvent(input$pelsa_confirm_set_default, {
      ome <- active_setup_ome(); req(ome)
      compound <- input$pelsa_compound
      if (is.null(compound) || !nzchar(compound)) {
        removeModal()
        return()
      }
      new_cm <- pelsa_set_compound_markers(compound_markers(), compound,
                                           cur_markers(ome))
      ok <- pelsa_write_compound_markers(pelsa_compound_markers_path(), new_cm)
      removeModal()
      if (!ok) {
        showNotification(pelsa_readonly_save_msg, type = "error", duration = 10)
        return()
      }
      compound_markers_version(compound_markers_version() + 1)
      showNotification(
        sprintf("Saved %d marker(s) as the default for '%s'.",
                nrow(cur_markers(ome)), compound),
        type = "message", duration = 4
      )
    })

    # Cross-module: the Volcano (Section 3) requests an accession be added via the
    # shared `marker_add_request` handle. The payload is list(ome=, rows=) so the
    # request targets the SPECIFIC dataset the volcano was viewing (markers are
    # per-ome). Merge into that ome's table; removal stays here in Setup. Merge is
    # idempotent (dedupes by accession), so a re-request is a no-op.
    if (is.function(marker_add_request)) {
      observeEvent(marker_add_request(), {
        req <- marker_add_request()
        if (is.null(req) || !is.list(req) ||
            length(req$ome) != 1L || !is.character(req$ome) ||
            is.null(req$rows) || !is.data.frame(req$rows) ||
            !all(c("accession", "gene") %in% names(req$rows)) ||
            nrow(req$rows) == 0L) {
          return()
        }
        ome <- req$ome
        if (!(ome %in% all_omes())) return()
        set_markers(ome, pelsa_merge_marker_rows(cur_markers(ome), req$rows))
        # M6: reset the shared channel so an identical re-request still registers
        # as a value change and re-fires this observer.
        marker_add_request(NULL)
      }, ignoreNULL = TRUE)
    }

    # Remove selected rows from the active ome (immutable replace).
    observeEvent(input$pelsa_remove_markers, {
      ome <- active_setup_ome(); req(ome)
      selected <- input$pelsa_marker_table_rows_selected
      current  <- cur_markers(ome)
      if (length(selected) == 0L || nrow(current) == 0L) return()
      keep <- setdiff(seq_len(nrow(current)), selected)
      set_markers(ome, current[keep, , drop = FALSE])
    })

    # Clear all for the active ome. The autofill tracker is intentionally NOT
    # reset (see the compound observer) so a same-value re-emit cannot resurrect.
    observeEvent(input$pelsa_clear_markers, {
      ome <- active_setup_ome(); req(ome)
      set_markers(ome, pelsa_empty_marker_rows())
    })

    output$pelsa_marker_table <- DT::renderDataTable({
      ome <- active_setup_ome(); req(ome)
      rows <- cur_markers(ome)
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

    ###########################################################################
    ## PER-DATASET CONDITION / REPLICATE CONFIG + ORDERING (5B)
    ## Input ids are positional (dataset index i, condition index j) so arbitrary
    ## dataset/condition strings can't collide; see pelsa_setup_ids() for the id
    ## scheme. i = position in all_omes(); j = position in the distinct-condition
    ## list. Includes orderInput drag widgets + reset + keyboard-rank fallback.
    ###########################################################################

    # SAFETY (do NOT change to a checked-subset index): .ds_index maps over the
    # STABLE FULL ome list (all_omes() == names(GCTs())), NOT input$pelsa_datasets.
    # So a dataset's index is fixed for the life of this reactive context and
    # index-keyed observers/inputs never desync as datasets are checked/unchecked.
    # The only thing that reorders all_omes() is a wholesale GCTs replacement,
    # which rebuilds this whole module context anyway. Keying off the checked
    # subset instead WOULD desync observers from their inputs.
    .ds_index <- function(ome) match(ome, all_omes())

    .ids <- pelsa_setup_ids()
    id_condition_col   <- .ids$condition_col
    id_replicate_col   <- .ids$replicate_col
    id_condition_order <- .ids$condition_order
    id_condition_reset <- .ids$condition_reset
    id_replicate_order <- .ids$replicate_order
    id_replicate_reset <- .ids$replicate_reset

    # cdesc data.frame for a dataset (rownames = sample names) or NULL.
    cdesc_for <- function(ome) {
      if (is.null(ome) || !(ome %in% all_omes())) return(NULL)
      gct <- GCTs()[[ome]]
      if (is.null(gct)) return(NULL)
      gct@cdesc
    }

    # cdesc column names for a dataset; character(0) if unknown/columnless.
    cdesc_cols_for <- function(ome) {
      cd <- cdesc_for(ome)
      if (is.null(cd)) character(0) else names(cd)
    }

    # Immutable per-dataset setter: replace setup_state[[field]] with a copy that
    # has [[ome]] set to `value` (value NULL drops the entry). Used pervasively
    # below to keep the named-list updates immutable + terse.
    set_ds <- function(field, ome, value) {
      cur <- setup_state[[field]]
      cur[[ome]] <- value
      setup_state[[field]] <- cur
    }

    # Immutable nested setter for replicate_order[[ome]][[cond]] (one condition's
    # sample order). Keeps the doubly-nested named-list update immutable + terse.
    set_ds_rep <- function(ome, cond, value) {
      by_cond <- setup_state$replicate_order[[ome]] %||% list()
      by_cond[[cond]] <- value
      set_ds("replicate_order", ome, by_cond)
    }

    # The ANALYZED (non-skipped) datasets, in all_omes() order. This is the
    # single source of truth for "which datasets PELSA runs" now that the old
    # checkbox group is gone - it is derived from the per-ome skip flags. The
    # per-dataset config machinery (pruning, sample_order, observer registration)
    # all keys off this, exactly as it used to key off the checkbox subset.
    checked_datasets <- reactive({
      pelsa_analyzed_omes(setup_state$skip, all_omes())
    })

    # The apply-all SOURCE dataset: the ACTIVE setup tab (the dataset whose form
    # is on screen). NULL when there is no valid active dataset.
    apply_all_source <- function() active_setup_ome()

    # ---- per-dataset config UI (ONLY the active setup tab's panel) ------------
    # With per-dataset tabs, one dataset's config is shown at a time (the active
    # setup tab), not a stacked list. The observers + sample_order still iterate
    # ALL non-skipped datasets (registered as tabs are visited), so summary/
    # volcano get orders even for not-yet-opened tabs.
    output$pelsa_perdataset_config <- renderUI({
      ome <- active_setup_ome()
      if (is.null(ome)) {
        return(helpText("Select a dataset to configure conditions."))
      }
      i     <- .ds_index(ome)
      cdesc <- cdesc_for(ome)
      cols  <- if (is.null(cdesc)) character(0) else names(cdesc)

      # Condition / replicate columns default to the blank "(none)" (the user
      # must consciously choose; the validator flags a blank). Honor a persisted
      # choice when it still exists in this dataset's columns.
      sel_cond <- setup_state$condition_col[[ome]] %||% "(none)"
      sel_rep  <- setup_state$replicate_col[[ome]] %||% "(none)"
      if (!identical(sel_cond, "(none)") && length(cols) && !(sel_cond %in% cols)) {
        sel_cond <- "(none)"
      }
      if (!identical(sel_rep, "(none)") && length(cols) && !(sel_rep %in% cols)) {
        sel_rep <- "(none)"
      }

      # Born-populated condition orderInput (empty until a real column is chosen).
      available_conds <- if (!is.null(cdesc) && !identical(sel_cond, "(none)") &&
                              sel_cond %in% names(cdesc)) {
        pelsa_distinct_conditions(cdesc, sel_cond)
      } else {
        character(0)
      }
      cond_order <- pelsa_merge_ordering(
        setup_state$condition_order[[ome]], available_conds
      )

      pelsa_dataset_config_panel(
        ome = ome, cols = cols, sel_cond = sel_cond, sel_rep = sel_rep,
        ids = list(
          condition_col   = ns(id_condition_col(i)),
          replicate_col   = ns(id_replicate_col(i)),
          condition_order = ns(id_condition_order(i)),
          condition_reset = ns(id_condition_reset(i)),
          replicate_cards = ns(sprintf("pelsa_replicate_cards_d%d", i))
        ),
        cond_order = cond_order
      )
    })

    # ---- distinct conditions per dataset (reactive on the chosen cond col) ----
    # Returns a named-by-ome list of distinct conditions for each checked dataset.
    distinct_conditions_for <- function(ome) {
      cdesc <- cdesc_for(ome)
      if (is.null(cdesc)) return(character(0))
      i <- .ds_index(ome)
      cond_col <- input[[id_condition_col(i)]] %||% setup_state$condition_col[[ome]]
      if (is.null(cond_col) || !(cond_col %in% names(cdesc))) return(character(0))
      pelsa_distinct_conditions(cdesc, cond_col)
    }

    # ---- per-condition replicate cards (one renderUI per dataset index) -------
    # Guarded by a registry so each renderer is created once (re-assigning
    # output$<id> replaces rather than stacks, but the guard keeps it explicit).
    rendered_card_outputs <- reactiveVal(character(0))

    register_replicate_card_renderer <- function(ome) {
      i  <- .ds_index(ome)
      out_id <- sprintf("pelsa_replicate_cards_d%d", i)
      reg <- rendered_card_outputs()
      if (out_id %in% reg) return(invisible())

      local({
        ome_local <- ome
        i_local   <- i
        output[[out_id]] <- renderUI({
          cdesc <- cdesc_for(ome_local)
          if (is.null(cdesc)) return(NULL)
          conds <- distinct_conditions_for(ome_local)
          if (length(conds) == 0L) {
            return(helpText("Choose a condition column to order replicates."))
          }
          cond_col <- input[[id_condition_col(i_local)]] %||%
            setup_state$condition_col[[ome_local]]
          rep_col  <- input[[id_replicate_col(i_local)]] %||%
            setup_state$replicate_col[[ome_local]]
          cards <- lapply(seq_along(conds), function(j) {
            cond <- conds[[j]]
            samples <- pelsa_samples_for_condition(cdesc, cond_col, rep_col, cond)
            pelsa_replicate_card(
              cond     = cond,
              samples  = samples,
              order_id = ns(id_replicate_order(i_local, j)),
              reset_id = ns(id_replicate_reset(i_local, j))
            )
          })
          do.call(tagList, cards)
        })
      })

      rendered_card_outputs(unique(c(reg, out_id)))
      invisible()
    }

    # ---- seeding the orderInputs (default / merge with saved) -----------------
    seed_condition_order <- function(ome) {
      cdesc <- cdesc_for(ome)
      if (is.null(cdesc)) return(invisible())
      i <- .ds_index(ome)
      available <- distinct_conditions_for(ome)
      saved <- setup_state$condition_order[[ome]]
      order <- pelsa_merge_ordering(saved, available)
      updateOrderInput(session, inputId = id_condition_order(i), items = order)
      set_ds("condition_order", ome, order)
    }

    seed_replicate_orders <- function(ome) {
      cdesc <- cdesc_for(ome)
      if (is.null(cdesc)) return(invisible())
      i <- .ds_index(ome)
      conds <- distinct_conditions_for(ome)
      cond_col <- input[[id_condition_col(i)]] %||% setup_state$condition_col[[ome]]
      rep_col  <- input[[id_replicate_col(i)]] %||% setup_state$replicate_col[[ome]]
      saved_by_cond <- setup_state$replicate_order[[ome]] %||% list()
      new_by_cond <- list()
      for (j in seq_along(conds)) {
        cond <- conds[[j]]
        default_samples <- pelsa_samples_for_condition(cdesc, cond_col, rep_col, cond)
        order <- pelsa_merge_ordering(saved_by_cond[[cond]], default_samples)
        new_by_cond[[cond]] <- order
        if (length(default_samples) > 1L) {
          updateOrderInput(session, inputId = id_replicate_order(i, j), items = order)
        }
      }
      set_ds("replicate_order", ome, new_by_cond)
    }

    # ---- observer-dedup registry (prevents leaks on re-render) ----------------
    # Dynamic per-dataset/per-condition inputs are a classic observer-leak
    # source. Track which observer KEYS exist and never create one twice
    # (mirrors tab_stat_plot.R's poi_observer_registry).
    setup_observer_registry <- reactiveVal(character(0))

    register_dataset_observers <- function(ome) {
      i   <- .ds_index(ome)
      key <- sprintf("ds_%d", i)
      reg <- setup_observer_registry()
      if (key %in% reg) return(invisible())

      local({
        ome_local <- ome
        i_local   <- i

        # Condition / replicate COLUMN selectors -> setup_state + reseed orders.
        # A column change invalidates this dataset's saved orders (drop -> reseed)
        # AND changes the distinct-condition set. We MUST re-register the
        # per-condition replicate observers so positions that became
        # multi-replicate under the new column get wired (the positional observers
        # already retarget to the live condition at their position; this only adds
        # observers for newly-multi-replicate positions, deduped by the registry -
        # no leak across repeated A->B->A switches).
        observeEvent(input[[id_condition_col(i_local)]], {
          set_ds("condition_col", ome_local, input[[id_condition_col(i_local)]])
          set_ds("condition_order", ome_local, NULL)
          set_ds("replicate_order", ome_local, NULL)
          seed_condition_order(ome_local)
          seed_replicate_orders(ome_local)
          register_condition_observers(ome_local)
        }, ignoreNULL = TRUE)

        observeEvent(input[[id_replicate_col(i_local)]], {
          set_ds("replicate_col", ome_local, input[[id_replicate_col(i_local)]])
          set_ds("replicate_order", ome_local, NULL)
          seed_replicate_orders(ome_local)
        }, ignoreNULL = TRUE)

        # Condition order drag -> setup_state.
        observeEvent(input[[id_condition_order(i_local)]], {
          set_ds("condition_order", ome_local, input[[id_condition_order(i_local)]])
        }, ignoreNULL = FALSE)

        # Condition reset -> default order.
        observeEvent(input[[id_condition_reset(i_local)]], {
          set_ds("condition_order", ome_local, NULL)
          seed_condition_order(ome_local)
        }, ignoreNULL = TRUE)
      })

      setup_observer_registry(unique(c(reg, key)))
      invisible()
    }

    # Per-condition replicate observers: ONE stable observer per (dataset i,
    # POSITION j). Keyed POSITIONALLY (ds_<i>_cond_<j>) on the SAME dedup registry
    # so re-rendering cards / switching the condition column never leaks (the
    # positional orderInput input ids are reused across columns - re-registering
    # by-value would stack a second observer on the same input id).
    #
    # H1 FIX: the condition VALUE the observer writes to is resolved LIVE at
    # observe-time from the CURRENT distinct conditions at position j (cond_at_j),
    # NOT captured at registration. Switching the condition column re-renders the
    # cards in the new conditions' order; the stable position-j observer then
    # writes set_ds_rep(ome, <new condition at j>, ...), so user replicate ordering
    # for the new column is RETAINED (pelsa_build_sample_order looks the order up by
    # the new condition name). Capturing the value at registration would write to
    # the OLD condition value and silently drop the new column's replicate order.
    #
    # A position j is registered the FIRST time it ever holds a multi-replicate
    # condition (under ANY column). It is then permanently bound to "whatever
    # multi-replicate condition currently sits at position j", which is exactly the
    # input/card the user is interacting with. Single-replicate positions get no
    # observer (dead wiring); if a later column makes that position multi-replicate
    # it is registered then.
    register_condition_observers <- function(ome) {
      cdesc <- cdesc_for(ome)
      if (is.null(cdesc)) return(invisible())
      i     <- .ds_index(ome)
      conds <- distinct_conditions_for(ome)
      reg   <- setup_observer_registry()
      new_keys <- character(0)

      # Skip single-replicate conditions: their card collapses to a static label
      # with no order/reset/rank inputs, so observing them would be dead wiring.
      cond_col <- input[[id_condition_col(i)]] %||% setup_state$condition_col[[ome]]
      rep_col  <- input[[id_replicate_col(i)]] %||% setup_state$replicate_col[[ome]]

      for (j in seq_along(conds)) {
        if (length(pelsa_samples_for_condition(cdesc, cond_col, rep_col, conds[[j]])) <= 1L) next

        key <- sprintf("ds_%d_cond_%d", i, j)
        if (key %in% reg) next
        local({
          i_local    <- i
          j_local    <- j
          ome_local  <- ome

          # The condition VALUE currently at position j (resolved LIVE so a
          # condition-column switch retargets this observer to the new column's
          # condition at this position). NULL when position j no longer exists.
          cond_at_j <- function() {
            cur <- distinct_conditions_for(ome_local)
            if (j_local > length(cur)) return(NULL)
            cur[[j_local]]
          }

          # Default replicate (sample) order for the condition currently at j.
          default_samples_local <- function() {
            cond <- cond_at_j()
            if (is.null(cond)) return(character(0))
            cd  <- cdesc_for(ome_local)
            cc  <- input[[id_condition_col(i_local)]] %||% setup_state$condition_col[[ome_local]]
            rc  <- input[[id_replicate_col(i_local)]] %||% setup_state$replicate_col[[ome_local]]
            pelsa_samples_for_condition(cd, cc, rc, cond)
          }

          # Replicate order drag -> setup_state (keyed by the LIVE condition at j).
          observeEvent(input[[id_replicate_order(i_local, j_local)]], {
            cond <- cond_at_j()
            if (is.null(cond)) return()
            set_ds_rep(ome_local, cond,
                       input[[id_replicate_order(i_local, j_local)]])
          }, ignoreNULL = FALSE)

          # Replicate reset -> default sample sort for the condition at j.
          observeEvent(input[[id_replicate_reset(i_local, j_local)]], {
            cond <- cond_at_j()
            if (is.null(cond)) return()
            default_samples <- default_samples_local()
            updateOrderInput(session, inputId = id_replicate_order(i_local, j_local),
                             items = default_samples)
            set_ds_rep(ome_local, cond, default_samples)
          }, ignoreNULL = TRUE)
        })
        new_keys <- c(new_keys, key)
      }
      if (length(new_keys)) {
        setup_observer_registry(unique(c(reg, new_keys)))
      }
      invisible()
    }

    # ---- wire everything when the checked-dataset set changes -----------------
    observeEvent(checked_datasets(), {
      checked <- checked_datasets()

      # Prune per-dataset state to the datasets that STILL EXIST (all_omes()),
      # NOT to the non-skipped subset: skipping a dataset must PRESERVE its
      # config (greying is purely visual) so un-skipping restores it. Only a new
      # upload that removes a dataset should drop its state - that is keyed off
      # all_omes(), which changes only on a wholesale GCTs replacement.
      fields <- c("condition_col", "replicate_col",
                  "condition_order", "replicate_order", "sample_order")
      pruned <- pelsa_prune_perdataset_state(
        stats::setNames(lapply(fields, function(f) setup_state[[f]]), fields),
        all_omes())
      for (f in fields) setup_state[[f]] <- pruned[[f]]

      for (ome in checked) {
        register_replicate_card_renderer(ome)
        register_dataset_observers(ome)
        # NO column auto-seed: condition/replicate columns default to the blank
        # "(none)" (the user must consciously choose; the validator flags a
        # blank). Seeding orders is a no-op until a real column is chosen.
        register_condition_observers(ome)
        seed_condition_order(ome)
        seed_replicate_orders(ome)
      }
    }, ignoreNULL = FALSE)

    # Register a dataset's config observers/cards when its setup tab is first
    # visited (a freshly-uploaded dataset is non-skipped by default, so the
    # observeEvent(checked_datasets()) above already covers it; this also covers
    # any tab the user opens directly). Deduped by setup_observer_registry.
    observeEvent(active_setup_ome(), {
      ome <- active_setup_ome(); req(ome)
      register_replicate_card_renderer(ome)
      register_dataset_observers(ome)
      register_condition_observers(ome)
      seed_condition_order(ome)
      seed_replicate_orders(ome)
    }, ignoreNULL = TRUE)

    # ---- APPLY THIS DATASET'S SETUP TO ALL OTHERS -----------------------------
    # A button (not a checkbox): copies the ACTIVE setup tab's full config to
    # every OTHER non-skipped tab. Uploaded FASTA + annotation / compound /
    # markers copy VERBATIM (the user can re-upload per dataset if they differ).
    # Condition/replicate COLUMNS + condition ORDER copy
    # best-effort (only where the target's cdesc has those columns). The per-
    # condition replicate ORDER is NOT copied (it holds the source's SAMPLE names,
    # which don't exist in the target); each target reseeds its own default.
    #
    # Meaningless with a single uploaded dataset, so disable then.
    observe({
      shinyjs::toggleState("pelsa_apply_all", condition = length(all_omes()) > 1L)
    })

    observeEvent(input$pelsa_apply_all, {
      src <- apply_all_source()
      if (is.null(src)) {
        showNotification("Select a dataset before applying its setup to others.",
                         type = "warning", duration = 3)
        return()
      }
      i_src    <- .ds_index(src)
      src_fasta_path  <- setup_state$fasta_path[[src]]
      src_fasta_name  <- setup_state$fasta_name[[src]]
      src_annot_path  <- setup_state$annotation_path[[src]]
      src_annot_name  <- setup_state$annotation_name[[src]]
      src_self_cur    <- isTRUE(setup_state$self_curated[[src]])
      src_compound <- setup_state$compound[[src]]
      src_markers  <- cur_markers(src)
      src_cond <- input[[id_condition_col(i_src)]] %||% setup_state$condition_col[[src]]
      src_rep  <- input[[id_replicate_col(i_src)]] %||% setup_state$replicate_col[[src]]
      src_cond_order <- setup_state$condition_order[[src]]

      applied <- FALSE
      col_skipped <- character(0)
      tracker <- last_autofilled_compound()
      for (ome in checked_datasets()) {       # non-skipped targets only
        if (identical(ome, src)) next

        # Uploaded FASTA + annotation / compound / markers copy verbatim. NOTE:
        # the targets share the SOURCE's uploaded temp file paths until the user
        # re-uploads per dataset (read-only sharing behind an explicit action -
        # the same verbatim-copy semantics compound/markers already use).
        set_ds("fasta_path", ome, src_fasta_path)
        set_ds("fasta_name", ome, src_fasta_name)
        set_ds("annotation_path", ome, src_annot_path)
        set_ds("annotation_name", ome, src_annot_name)
        set_ds("self_curated", ome, src_self_cur)
        set_ds("compound", ome, src_compound)
        set_markers(ome, src_markers)
        # Mark this ome's compound as already-autofilled, so the first visit to
        # the target tab (which re-emits the copied compound) does NOT re-fire
        # the autofill and re-merge / resurrect rows.
        if (!is.null(src_compound) && nzchar(src_compound)) {
          tracker[[ome]] <- src_compound
        }

        # Condition/replicate columns + order: best-effort where columns exist.
        cols <- cdesc_cols_for(ome)
        cond_ok <- !is.null(src_cond) && !identical(src_cond, "(none)") &&
          (src_cond %in% cols)
        rep_ok  <- !is.null(src_rep) && !identical(src_rep, "(none)") &&
          (src_rep %in% cols)
        if (cond_ok && rep_ok) {
          i <- .ds_index(ome)
          set_ds("condition_col", ome, src_cond)
          set_ds("replicate_col", ome, src_rep)
          updateSelectInput(session, id_condition_col(i), selected = src_cond)
          updateSelectInput(session, id_replicate_col(i), selected = src_rep)
          set_ds("condition_order", ome, src_cond_order)
          set_ds("replicate_order", ome, NULL)
          seed_condition_order(ome)
          seed_replicate_orders(ome)
        } else {
          col_skipped <- c(col_skipped, ome)
        }
        applied <- TRUE
      }
      last_autofilled_compound(tracker)

      if (applied) {
        showNotification(sprintf(
          paste0("Applied %s's FASTA, annotation, compound, and markers to all ",
                 "other datasets; condition/replicate columns copied where present."),
          src), type = "message", duration = 5)
      } else {
        showNotification("No other datasets to apply this setup to.",
                         type = "warning", duration = 3)
      }
      if (length(col_skipped)) {
        showNotification(sprintf(
          paste0("Condition/replicate columns not copied to %d dataset(s) ",
                 "lacking matching columns: %s (FASTA/annotation/compound/markers ",
                 "still applied)."),
          length(col_skipped), paste(col_skipped, collapse = ", ")
        ), type = "warning", duration = 6)
      }
    }, ignoreInit = TRUE)

    # ---- canonical sample_order (what Summary/Volcano consume) ----------------
    # Recomputed for every checked dataset on any ordering / column change.
    observe({
      checked <- checked_datasets()
      so <- list()
      for (ome in checked) {
        cdesc <- cdesc_for(ome)
        if (is.null(cdesc)) next
        cond_col <- setup_state$condition_col[[ome]]
        rep_col  <- setup_state$replicate_col[[ome]]
        if (is.null(cond_col) || is.null(rep_col)) next
        if (!(cond_col %in% names(cdesc)) || !(rep_col %in% names(cdesc))) next
        so[[ome]] <- pelsa_build_sample_order(
          condition_order              = setup_state$condition_order[[ome]],
          replicate_order_by_condition = setup_state$replicate_order[[ome]],
          cdesc                        = cdesc,
          condition_col                = cond_col,
          replicate_col                = rep_col
        )
      }
      setup_state$sample_order <- so
    })

    ## START ANALYSIS (5D) ##
    # The compute pipeline that assembles the verified Phase-2 helpers into a
    # per-dataset cache the Summary (Phase 6) + Volcano (Phase 7) sections READ
    # (never recompute in render). The PURE validation + assembly logic lives in
    # tab_pelsa_analysis_helpers.R (pelsa_validate_setup / pelsa_run_analysis);
    # this observer stays thin: snapshot setup_state under isolate(), validate,
    # then run the assembly under STAGED withProgress, surface errors as inline
    # UI + notifications, and drive the analyzed-datasets seam on success.
    #
    # DECISIONS (see the helper banner): cache-as-is feature annotation (no
    # UniProt top-up on this path - refresh is 5C's job); compute ALL checked
    # datasets at Start (matches the analyzed-datasets semantics).
    #
    # The cache the sections read. Keyed by dataset; each value is the per-dataset
    # object from pelsa_run_analysis_one (matched/unmatched/cv/depth/coverage/
    # peptide_metrics/annotation/unannotated/qc), or list(error=) on failure.
    pelsa_analysis <- reactiveVal(NULL)
    last_validation <- reactiveVal(list(ok = TRUE, errors = character(0)))
    analysis_in_flight <- reactiveVal(FALSE)

    output$pelsa_validation_msgs <- renderUI({
      pelsa_validation_msg_ui(last_validation())
    })

    observeEvent(input$pelsa_start, {
      if (isTRUE(analysis_in_flight())) return()  # ignore overlapping clicks

      # The datasets to analyze = the NON-SKIPPED omes. Stamp it onto
      # setup_state$datasets so the snapshot (and everything downstream that reads
      # snapshot$datasets: validation, run loop, the analyzed-datasets seam) sees
      # exactly the analyzed set.
      isolate(setup_state$datasets <- pelsa_analyzed_omes(setup_state$skip,
                                                          all_omes()))

      # Snapshot setup_state under isolate() so mid-compute input edits cannot
      # corrupt this run.
      snapshot <- isolate(pelsa_setup_snapshot(setup_state))
      gp <- isolate(GCTs_and_params())
      gcts_processed <- if (is.null(gp)) NULL else gp$GCTs
      gcts_raw       <- isolate(GCTs_original())

      # Per-dataset log base for the CV delinearize. GCTs_original is the
      # LOG-transformed matrix; the CV is defined on raw LINEAR intensities, so
      # pelsa_run_analysis delinearizes each dataset's CV input by this base
      # (None/NA => already linear). Snapshot it under isolate() with the rest.
      params_snap <- if (is.null(gp)) list() else (gp$parameters %||% list())
      log_base_by_ds <- lapply(params_snap, function(p) {
        p$log_transformation %||% NA_character_
      })

      # Pre-flight validation (pure). Render inline + bail on failure.
      validation <- pelsa_validate_setup(snapshot, gcts_processed, NULL)
      last_validation(validation)
      if (!isTRUE(validation$ok)) {
        showNotification(
          "Cannot start analysis - see the checklist below the button.",
          type = "warning", duration = 4)
        return()
      }

      analysis_in_flight(TRUE)
      shinyjs::disable("pelsa_start")
      on.exit({
        shinyjs::enable("pelsa_start")
        analysis_in_flight(FALSE)
      }, add = TRUE)

      result <- tryCatch(
        withProgress(message = "Running PELSA analysis", value = 0, {
          setProgress(value = 0.05, detail = "Loading FASTA")
          # PER-DATASET resolvers: each dataset supplies its OWN uploaded FASTA +
          # annotation file. A self-curated dataset parses its FASTA first-token
          # and uses an empty feature frame (no annotation); otherwise the FASTA
          # is parsed pipe-aware (UniProt) and the uploaded raw annotation file is
          # read + classified. pelsa_run_analysis memoizes these per dataset.
          resolve_fasta <- function(ds) {
            fasta_path <- snapshot$fasta_path[[ds]]
            fasta_mode <- if (isTRUE(snapshot$self_curated[[ds]]))
              "self_curated" else "uniprot"
            # Surface any reader warning (e.g. duplicated accessions), then muffle
            # so it does not abort the progress block.
            withCallingHandlers(
              pelsa_read_fasta(fasta_path, mode = fasta_mode),
              warning = function(w) {
                showNotification(conditionMessage(w), type = "warning",
                                 duration = NULL)
                invokeRestart("muffleWarning")
              }
            )
          }
          resolve_feat <- function(ds) {
            if (isTRUE(snapshot$self_curated[[ds]])) {
              pelsa_empty_feature_frame()
            } else {
              pelsa_read_annotation_file(snapshot$annotation_path[[ds]])
            }
          }

          pelsa_run_analysis(
            gcts           = gcts_processed,
            gcts_original  = gcts_raw,
            setup_snapshot = snapshot,
            resolve_fasta  = resolve_fasta,
            resolve_feat   = resolve_feat,
            log_base_by_ds = log_base_by_ds,
            set_progress   = function(value, detail) {
              # Map the assembly's 0..1 onto the 0.15..1.0 remaining band.
              setProgress(value = 0.15 + 0.85 * value, detail = detail)
            }
          )
        }),
        error = function(e) {
          showNotification(sprintf("Analysis failed: %s", conditionMessage(e)),
                           type = "error", duration = NULL)
          NULL
        }
      )

      if (is.null(result)) return()

      pelsa_analysis(result)

      # Drive the Phase-4 analyzed-datasets seam: the switcher + sections now
      # show ONLY the analyzed datasets.
      if (is.function(set_analyzed_datasets)) {
        set_analyzed_datasets(snapshot$datasets)
      }

      # Use the canonical predicate (the ONE place that defines fail-vs-success)
      # so Phase 6/7 + this observer agree on the rule.
      failed <- names(Filter(pelsa_analysis_failed, result))
      if (length(failed) > 0L) {
        showNotification(sprintf(
          "Analysis complete with %d dataset error(s): %s",
          length(failed), paste(failed, collapse = ", ")),
          type = "warning", duration = 8)
      } else {
        showNotification(sprintf("Analysis complete for %d dataset(s).",
                                 length(result)), type = "message", duration = 5)
      }

      # Redirect to the PELSA Summary tab so the user lands on the results (the
      # Summary surfaces per-dataset failures too, so we navigate even when some
      # datasets errored). The navbar lives in app_ui's navbarPage(id =
      # "navbar-tabs"); the Summary tabPanel's value is "PELSA-Summary". We need
      # the PARENT (app) session for the navbar - the module session can't reach
      # it. Guarded so the module still works if invoked without a parent session
      # (e.g. an isolated test harness).
      if (!is.null(parent_session)) {
        updateTabsetPanel(session = parent_session, inputId = "navbar-tabs",
                          selected = "PELSA-Summary")
      }
    }, ignoreInit = TRUE)

    ## EXPORTS (per-ome wiring preserved from the scaffold) ##
    # NOTE: exports must eventually recompute ALL analyzed datasets, not just the
    # active one. For now we instantiate the per-ome server for every ome so the
    # existing export wiring is preserved.
    # TODO (5D/Phase 5-7): drive this off the analyzed-datasets seam and have
    # each export function recompute its dataset from scratch; honor the
    # planning-doc memory contract (no per-ome instance leaks; keep only the
    # active dataset's heavy objects hot).
    # The 01_setup export bundle for one ome: run configuration (pelsa_setup.yaml)
    # + the marker table (pelsa_markers.csv). Reads the shared setup_state at
    # export time so it reflects the latest run configuration.
    make_setup_export <- function(ome) function(dir_name) {
      out <- pelsa_export_stage_dir(dir_name, .PELSA_STAGE_SETUP)
      ss <- tryCatch(isolate(pelsa_setup_snapshot(setup_state)),
                     error = function(e) NULL)
      if (is.null(ss)) return(invisible(NULL))
      cfg <- list(
        self_curated     = isTRUE(ss$self_curated[[ome]]),
        fasta_file       = ss$fasta_name[[ome]] %||% NA,
        annotation_file  = ss$annotation_name[[ome]] %||% NA,
        compound         = ss$compound[[ome]] %||% NA,
        condition_column = ss$condition_col[[ome]] %||% NA,
        condition_order  = as.list(ss$condition_order[[ome]] %||% character(0)),
        sample_order     = as.list(ss$sample_order[[ome]] %||% character(0))
      )
      yaml::write_yaml(cfg, file.path(out, "pelsa_setup.yaml"))
      mr <- ss$marker_rows[[ome]]
      if (is.data.frame(mr) && nrow(mr) > 0L) {
        utils::write.csv(mr, file.path(out, "pelsa_markers.csv"),
                         row.names = FALSE)
      }

      # Copy the uploaded FASTA + annotation file verbatim (under their original
      # names) for future reference, plus the missing-accessions list (dataset
      # accessions absent from the annotation file = res$unannotated). Self-curated
      # datasets export the FASTA only.
      res <- tryCatch(isolate(pelsa_analysis())[[ome]], error = function(e) NULL)
      missing <- if (!is.null(res) && is.null(res$error)) {
        res$unannotated %||% character(0)
      } else {
        character(0)
      }
      pelsa_export_input_files(
        out,
        fasta_path      = ss$fasta_path[[ome]],
        fasta_name      = ss$fasta_name[[ome]],
        annotation_path = if (isTRUE(ss$self_curated[[ome]])) NULL
                          else ss$annotation_path[[ome]],
        annotation_name = if (isTRUE(ss$self_curated[[ome]])) NULL
                          else ss$annotation_name[[ome]],
        missing_accessions = missing
      )
      invisible(out)
    }

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
      # Attach the shared 01_setup export to each ome (setup_state is shared).
      ome_exports <- stats::setNames(lapply(names(ome_exports), function(ome) {
        c(ome_exports[[ome]] %||% list(),
          list(setup = make_setup_export(ome)))
      }), names(ome_exports))
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
    # reactiveVal - only Setup carries a setup_state companion, because only
    # Setup owns shared run-configuration state. Revisit if 2/3 grow their own.
    #
    # $analysis (5D): the per-dataset analysis cache reactiveVal Start-Analysis
    # populates. Phases 6 (Summary) and 7 (Volcano) READ this; they never
    # recompute the heavy objects in render. NULL until the first Start-Analysis.
    #
    # SEAM CONTRACT: setup_state is returned as a REACTIVE that yields a plain
    # snapshot LIST (pelsa_setup_snapshot), NOT the bare reactiveValues. The
    # consumers (Sections 2 & 3) guard with is.function() and CALL the seam with
    # () to read ss$sample_order / $condition_order / $marker_rows / $condition_col
    # / $species. A bare reactiveValues is NOT a function (is.function() is FALSE),
    # so it would be silently downgraded to reactive(NULL) in production AND would
    # error when called with (). Returning reactive(pelsa_setup_snapshot(...)) makes
    # is.function() TRUE and delivers the live snapshot list the consumers expect.
    list(exports = all_exports,
         setup_state = reactive(pelsa_setup_snapshot(setup_state)),
         analysis = pelsa_analysis)
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
