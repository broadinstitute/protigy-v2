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

    # Immutable per-dataset setter: replace setup_state[[field]] with a copy that
    # has [[ome]] set to `value` (value NULL drops the entry). Shared across the
    # whole server (marker-table wiring here + the per-dataset condition/
    # replicate config wired by pelsa_wire_dataset_config() below), so it is
    # defined once, at top level, and passed into that helper rather than
    # redefined there.
    set_ds <- function(field, ome, value) {
      cur <- setup_state[[field]]
      cur[[ome]] <- value
      setup_state[[field]] <- cur
    }

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
    ##
    ## The wiring itself lives in pelsa_wire_dataset_config()
    ## (tab_pelsa_section1_server_helpers.R) to keep this server function under
    ## the file's line/size budget; it is Shiny reactive plumbing (not pure
    ## logic), so it stays out of tab_pelsa_section1_helpers.R. checked_datasets
    ## and setup_observer_registry are created HERE (not inside the helper) and
    ## passed in by reference, because test-pelsa-setup.R's testServer() cases
    ## call them by bare name from this function's own environment.
    ###########################################################################

    # The ANALYZED (non-skipped) datasets, in all_omes() order. This is the
    # single source of truth for "which datasets PELSA runs" now that the old
    # checkbox group is gone - it is derived from the per-ome skip flags. The
    # per-dataset config machinery (pruning, sample_order, observer registration)
    # all keys off this, exactly as it used to key off the checkbox subset.
    checked_datasets <- reactive({
      pelsa_analyzed_omes(setup_state$skip, all_omes())
    })

    # Observer-dedup registry (prevents leaks on re-render) for the dynamic
    # per-dataset/per-condition inputs pelsa_wire_dataset_config() registers.
    setup_observer_registry <- reactiveVal(character(0))

    pelsa_wire_dataset_config(
      input = input, output = output, session = session, ns = ns,
      setup_state = setup_state, GCTs = GCTs, all_omes = all_omes,
      active_setup_ome = active_setup_ome, set_ds = set_ds,
      cur_markers = cur_markers, set_markers = set_markers,
      last_autofilled_compound = last_autofilled_compound,
      checked_datasets = checked_datasets,
      setup_observer_registry = setup_observer_registry
    )

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
              # Surface any reader warning (e.g. malformed coordinate coercion),
              # then muffle so it does not abort the progress block.
              withCallingHandlers(
                pelsa_read_annotation_file(snapshot$annotation_path[[ds]]),
                warning = function(w) {
                  showNotification(conditionMessage(w), type = "warning",
                                   duration = NULL)
                  invokeRestart("muffleWarning")
                }
              )
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
    # The 01_setup export bundle factory (run configuration + marker table; see
    # pelsa_make_setup_export() in tab_pelsa_section1_server_helpers.R) reads the
    # shared setup_state/pelsa_analysis at export time so it reflects the latest
    # run configuration.
    make_setup_export <- pelsa_make_setup_export(setup_state, pelsa_analysis)

    # Per-ome (per-dataset) server: instantiate ONCE per ome and reuse. all_omes()
    # re-emits on every dataset add/remove (GCTs_and_params is replaced whole
    # during setup); re-calling PELSASection1_Ome_Server() for an already-started
    # ome would stack duplicate observers/outputs once it grows beyond today's
    # empty placeholder. Guard with a started-registry, mirroring Section 3.
    ome_export_store <- reactiveVal(list())  # ome -> exports, persists re-emits
    started_omes     <- reactiveVal(character(0))
    observeEvent(all_omes(), {
      new_omes <- setdiff(all_omes(), started_omes())
      if (length(new_omes) == 0L) return()
      new_exports <- sapply(new_omes, function(ome) {
        PELSASection1_Ome_Server(
          id                        = ome,
          ome                       = ome,
          GCT_processed             = reactive(GCTs()[[ome]]),
          parameters                = reactive(parameters()[[ome]]),
          default_annotation_column = reactive(default_annotations()[[ome]]),
          color_map                 = reactive(custom_colors()[[ome]])
        )
      }, simplify = FALSE)
      # Attach the shared 01_setup export to each new ome (setup_state is shared).
      new_exports <- stats::setNames(lapply(names(new_exports), function(ome) {
        c(new_exports[[ome]] %||% list(),
          list(setup = make_setup_export(ome)))
      }), names(new_exports))
      started_omes(c(started_omes(), new_omes))
      ome_export_store(modifyList(ome_export_store(), new_exports))
    })

    # Expose exports for the CURRENTLY-present omes only (a removed ome's stored
    # exports are dropped from the gathered set without re-instantiating others).
    all_exports <- reactive({
      store <- ome_export_store()
      store[intersect(all_omes(), names(store))]
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
