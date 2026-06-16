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
#   6. PER-DATASET condition/replicate configuration (5B): for each CHECKED
#      dataset, a condition-grouping + replicate-identifier selectInput (driven
#      by THAT dataset's cdesc), a draggable condition-ORDER widget, and one
#      draggable replicate-order widget per condition. An "apply the same setup
#      to all datasets" checkbox copies the source dataset's column choices +
#      ordering to every checked dataset.
#
# The pure, testable logic lives in tab_pelsa_section1_helpers.R; this server
# stays thin (wiring + reactivity only).
#
# SETUP-STATE OBJECT (the documented contract read/extended by 5C/5D + 6/7)
#   The Tab server exposes a `setup_state` reactiveValues:
#     setup_state$datasets       chr - checked datasets to analyze (5D drives
#                                      the container's pelsa_analyzed_datasets
#                                      off this; see SEAM below)
#     setup_state$species        chr scalar - selected species (SHARED)
#     setup_state$compound       chr scalar - selected treatment compound (SHARED)
#     setup_state$marker_rows    data.frame(accession, gene) - marker table (SHARED)
#
#   PER-DATASET fields (5B) - NAMED LISTS keyed by dataset name (ome). Only the
#   checked datasets have entries; toggling a dataset adds/removes its entry:
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
#   APPLY-ALL SOURCE (documented): when "apply to all" is ticked, the SOURCE
#   dataset is the ACTIVE dataset if it is among the checked datasets, else the
#   first checked dataset. Only the per-dataset condition/replicate COLUMNS +
#   ORDERING are copied; species/compound/markers stay shared and untouched.
#
# DEFERRED SEAMS (documented; built later)
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

# pelsa_database_dir() + pelsa_compound_markers_path() live in
# tab_pelsa_section1_helpers.R (pure path resolvers, shared with Section 3).

PELSASection1_Tab_Server <- function(id = "PELSASection1Tab",
                                     GCTs_and_params,
                                     globals,
                                     GCTs_original,
                                     active_dataset,
                                     set_analyzed_datasets = NULL,
                                     marker_add_request = NULL,
                                     parent_session = NULL) {

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

    ## SETUP STATE (the documented per-dataset contract; see header) ##
    # SHARED scalars + per-dataset NAMED LISTS keyed by dataset (ome).
    setup_state <- reactiveValues(
      datasets        = character(0),
      species         = NULL,
      compound        = NULL,
      marker_rows     = pelsa_empty_marker_rows(),
      condition_col   = list(),  # [[ds]] -> chr scalar
      replicate_col   = list(),  # [[ds]] -> chr scalar
      condition_order = list(),  # [[ds]] -> chr (condition order)
      replicate_order = list(),  # [[ds]] -> list(cond -> chr sample order)
      sample_order    = list()   # [[ds]] -> chr (canonical sample order)
    )

    # Re-read the compound presets on Setup entry (and whenever the box renders)
    # so user edits to compound_markers.yaml show up without an app restart.
    compound_markers <- reactive({
      pelsa_read_compound_markers(pelsa_compound_markers_path())
    })

    ## SETUP UI ##
    # The Setup box's PURE markup lives in pelsa_setup_box_ui() (helpers); this
    # renderUI just gates on a valid active dataset, gathers the live choices,
    # and delegates. The per-dataset config (5B) + marker table render into the
    # uiOutput/DT placeholders the builder emits.
    output$setup_box <- renderUI({
      ome <- active_dataset()
      req(ome, ome %in% all_omes())

      pelsa_setup_box_ui(
        datasets  = all_omes(),
        species   = pelsa_list_species(pelsa_database_dir()),
        compounds = names(compound_markers()$compounds),
        ns        = ns
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
    # makes echo-safety robust - a same-value re-emit after a clear (a re-render
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

    # Cross-module: the Volcano (Section 3) requests an accession be added via the
    # shared `marker_add_request` handle (data.frame(accession, gene)). Merge it
    # into the marker table - removal stays here in Setup. Merge is idempotent
    # (dedupes by accession), so a re-request of an existing marker is a no-op.
    if (is.function(marker_add_request)) {
      observeEvent(marker_add_request(), {
        req <- marker_add_request()
        if (is.null(req) || !is.data.frame(req) ||
            !all(c("accession", "gene") %in% names(req)) || nrow(req) == 0L) {
          return()
        }
        marker_rows(pelsa_merge_marker_rows(marker_rows(), req))
      }, ignoreNULL = TRUE)
    }

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

    # The currently-checked datasets, intersected with the available omes and
    # kept in all_omes() order (stable, deterministic).
    checked_datasets <- reactive({
      sel <- input$pelsa_datasets
      if (is.null(sel)) sel <- character(0)
      all_omes()[all_omes() %in% sel]
    })

    # The apply-all SOURCE dataset (documented): active dataset if checked, else
    # the first checked dataset. NULL when nothing is checked.
    apply_all_source <- function() {
      checked <- checked_datasets()
      if (length(checked) == 0L) return(NULL)
      act <- active_dataset()
      if (!is.null(act) && act %in% checked) act else checked[[1]]
    }

    # ---- per-dataset config UI (one panel per CHECKED dataset) ----------------
    output$pelsa_perdataset_config <- renderUI({
      checked <- checked_datasets()
      if (length(checked) == 0L) {
        return(helpText("Check at least one dataset to configure conditions."))
      }

      panels <- lapply(checked, function(ome) {
        i     <- .ds_index(ome)
        cdesc <- cdesc_for(ome)
        cols  <- if (is.null(cdesc)) character(0) else names(cdesc)

        sel_cond <- setup_state$condition_col[[ome]] %||%
          (if (length(cols)) cols[[1]] else NULL)
        sel_rep  <- setup_state$replicate_col[[ome]] %||%
          (if (length(cols)) cols[[1]] else NULL)
        if (length(cols) && !(sel_cond %in% cols)) sel_cond <- cols[[1]]
        if (length(cols) && !(sel_rep  %in% cols)) sel_rep  <- cols[[1]]

        # Born-populated condition orderInput: compute the initial order HERE so
        # the drag blocks render with their items already present. Seeding the
        # widget post-render via updateOrderInput races this renderUI (the
        # message can arrive before the orderInput element exists, so the blocks
        # would stay empty until some later input forced a reseed).
        available_conds <- if (!is.null(cdesc) && !is.null(sel_cond) &&
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

      do.call(tagList, panels)
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

      # Drop unchecked datasets from every per-dataset field (pure, tested).
      fields <- c("condition_col", "replicate_col",
                  "condition_order", "replicate_order", "sample_order")
      pruned <- pelsa_prune_perdataset_state(
        stats::setNames(lapply(fields, function(f) setup_state[[f]]), fields),
        checked)
      for (f in fields) setup_state[[f]] <- pruned[[f]]

      for (ome in checked) {
        register_replicate_card_renderer(ome)
        register_dataset_observers(ome)

        # Seed column defaults if not yet set.
        cols <- cdesc_cols_for(ome)
        if (length(cols) > 0L) {
          if (is.null(setup_state$condition_col[[ome]])) set_ds("condition_col", ome, cols[[1]])
          if (is.null(setup_state$replicate_col[[ome]])) set_ds("replicate_col", ome, cols[[1]])
        }
        register_condition_observers(ome)
        seed_condition_order(ome)
        seed_replicate_orders(ome)
      }
    }, ignoreNULL = FALSE)

    # ---- APPLY TO ALL ---------------------------------------------------------
    # Copy the SOURCE dataset's condition/replicate columns + ordering to every
    # checked dataset whose cdesc has both columns. Species/compound/markers stay
    # shared and untouched.
    #
    # "Apply to all" is meaningless with a single uploaded dataset (there is no
    # other dataset to copy the setup TO), so grey the checkbox out in a
    # single-ome session. The checkbox lives inside the server-rendered setup box,
    # so re-toggle on every (re)render; updateOrderInput-style races don't apply
    # to shinyjs::toggleState because shinyjs re-applies disabled state on bind.
    observe({
      shinyjs::toggleState("pelsa_apply_all", condition = length(all_omes()) > 1L)
    })

    observeEvent(input$pelsa_apply_all, {
      if (!isTRUE(input$pelsa_apply_all)) return()
      src <- apply_all_source()
      if (is.null(src)) {
        showNotification("Check at least one dataset before applying to all.",
                         type = "warning", duration = 3)
        updateCheckboxInput(session, "pelsa_apply_all", value = FALSE)
        return()
      }
      i_src    <- .ds_index(src)
      src_cond <- input[[id_condition_col(i_src)]] %||% setup_state$condition_col[[src]]
      src_rep  <- input[[id_replicate_col(i_src)]] %||% setup_state$replicate_col[[src]]
      src_cond_order <- setup_state$condition_order[[src]]

      # WHAT TRANSFERS (and what does NOT): the condition/replicate COLUMN
      # choices and the condition ORDER reference column NAMES + condition VALUES,
      # which are shared across datasets, so they copy faithfully. The per-
      # condition replicate ORDER, however, is keyed by condition value but holds
      # the SOURCE's SAMPLE NAMES - targets have different sample names, so
      # copying it would be dropped by pelsa_merge_ordering's intersection and
      # silently fall back to each target's default. We therefore do NOT copy it;
      # each target keeps its own default replicate ordering. The toast below says
      # exactly this (honest apply-all - never claim a transfer that didn't happen).
      applied <- FALSE
      skipped <- character(0)
      for (ome in checked_datasets()) {
        if (identical(ome, src)) next
        cols <- cdesc_cols_for(ome)
        if (!(src_cond %in% cols) || !(src_rep %in% cols)) {
          skipped <- c(skipped, ome)
          next
        }
        i <- .ds_index(ome)
        # Columns + selectInputs.
        set_ds("condition_col", ome, src_cond)
        set_ds("replicate_col", ome, src_rep)
        updateSelectInput(session, id_condition_col(i), selected = src_cond)
        updateSelectInput(session, id_replicate_col(i), selected = src_rep)
        # Condition order copies (condition VALUES are shared); replicate order is
        # NOT copied (source sample names don't exist in the target) - re-seed it
        # to the target's own default instead.
        set_ds("condition_order", ome, src_cond_order)
        set_ds("replicate_order", ome, NULL)
        seed_condition_order(ome)
        seed_replicate_orders(ome)
        applied <- TRUE
      }
      if (applied) {
        showNotification(sprintf(
          paste0("Applied %s's condition/replicate columns and condition order ",
                 "to all compatible datasets; replicate ordering uses each ",
                 "dataset's default."),
          src), type = "message", duration = 5)
      }
      if (length(skipped)) {
        showNotification(sprintf(
          "Skipped %d dataset(s) lacking column(s) '%s'/'%s': %s",
          length(skipped), src_cond, src_rep, paste(skipped, collapse = ", ")
        ), type = "warning", duration = 5)
      }
      updateCheckboxInput(session, "pelsa_apply_all", value = FALSE)
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

    ## SPECIES UNIPROT-ANNOTATION REFRESH (5C) ##
    # Maintenance action, OFF the reactive path (once per button click). The
    # multi-species loop, universe resolution, fetch + MERGE + atomic write, and
    # error capture all live in pelsa_run_species_refresh()
    # (tab_pelsa_refresh_helpers.R) so this observer stays thin: gather inputs,
    # (optionally) confirm a large fetch, drive a live progress bar, and surface
    # the result INLINE under the button. fetch_fn is the real pelsa_fetch_uniprot
    # here; tests inject a stub into the helper directly (no network).
    #
    # WHY CONFIRM: with no datasets uploaded the universe falls back to the WHOLE
    # FASTA proteome (~70k accessions for human). That is a multi-minute fetch a
    # user can trigger by accident, so above a threshold we show the count + a
    # rough ETA and require confirmation first.
    #
    # WHY INLINE STATUS: the per-page progress bar renders LIVE in the modal
    # (Shiny flushes setProgress mid-loop), and the final result renders in a
    # PERSISTENT inline panel under the button (pelsa_refresh_result_ui) instead
    # of a dismissible toast - so the outcome can never be cleared off-screen.
    #
    # RE-CLICK GUARD: an in-flight reactiveVal + a disabled button prevent a
    # second click mid-fetch from starting an overlapping write (which would
    # race the atomic rename).
    refresh_in_flight  <- reactiveVal(FALSE)
    refresh_result     <- reactiveVal(NULL)   # last run's results (inline panel)
    # Above this universe size we confirm before fetching (the proteome-fallback
    # foot-gun guard). A normal dataset-driven refresh is well under this.
    REFRESH_CONFIRM_THRESHOLD <- 5000L

    output$pelsa_refresh_status <- renderUI({
      pelsa_refresh_result_ui(refresh_result())
    })

    # The actual run (shared by the direct + confirmed paths). Drives the live
    # progress modal, runs the orchestrator, and stores the results for the
    # inline panel. `selected` + `uploaded_gcts` are captured by the callers.
    run_refresh <- function(selected, uploaded_gcts) {
      refresh_in_flight(TRUE)
      shinyjs::disable("pelsa_refresh_btn")
      on.exit({
        shinyjs::enable("pelsa_refresh_btn")
        refresh_in_flight(FALSE)
      }, add = TRUE)

      results <- withProgress(
        message = "Refreshing UniProt annotation library", value = 0, {
          pelsa_run_species_refresh(
            species       = selected,
            database_dir  = pelsa_database_dir(),
            uploaded_gcts = uploaded_gcts,
            fetch_fn      = pelsa_fetch_uniprot,
            set_progress  = function(value, detail) {
              setProgress(value = value, detail = detail)
            }
          )
        }
      )
      refresh_result(results)
    }

    observeEvent(input$pelsa_refresh_btn, {
      if (isTRUE(refresh_in_flight())) return()   # ignore overlapping clicks

      selected <- input$pelsa_refresh_species
      if (is.null(selected) || length(selected) == 0L) {
        showNotification("Select at least one species to refresh.",
                         type = "warning", duration = 4)
        return()
      }
      gp <- GCTs_and_params()
      uploaded_gcts <- if (is.null(gp)) NULL else gp$GCTs
      database_dir  <- pelsa_database_dir()

      # Size the universe up front; confirm before a large (proteome) fetch.
      size <- tryCatch(
        pelsa_refresh_universe_size(selected, database_dir, uploaded_gcts),
        error = function(e) list(total = NA_integer_, per_species = integer(0)))

      if (!is.na(size$total) && size$total > REFRESH_CONFIRM_THRESHOLD) {
        per <- paste(sprintf("%s: %s", names(size$per_species),
                             vapply(size$per_species, pelsa_refresh_eta_text,
                                    character(1))),
                     collapse = "<br/>")
        shinyalert::shinyalert(
          title = "Refresh a large annotation set?",
          text = sprintf(
            paste0("About to fetch <b>%s</b> total.<br/><br/>%s<br/><br/>",
                   "This runs against UniProt and cannot be stopped once ",
                   "started. Continue?"),
            pelsa_refresh_eta_text(size$total), per),
          html = TRUE, type = "warning",
          showCancelButton = TRUE, confirmButtonText = "Fetch",
          cancelButtonText = "Cancel",
          callbackR = function(confirmed) {
            if (isTRUE(confirmed)) run_refresh(selected, uploaded_gcts)
          }
        )
        return()
      }

      run_refresh(selected, uploaded_gcts)
    }, ignoreInit = TRUE)

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

      # Snapshot setup_state under isolate() so mid-compute input edits cannot
      # corrupt this run.
      snapshot <- isolate(pelsa_setup_snapshot(setup_state))
      gp <- isolate(GCTs_and_params())
      gcts_processed <- if (is.null(gp)) NULL else gp$GCTs
      gcts_raw       <- isolate(GCTs_original())
      database_dir   <- pelsa_database_dir()

      # Per-dataset log base for the CV delinearize. GCTs_original is the
      # LOG-transformed matrix; the CV is defined on raw LINEAR intensities, so
      # pelsa_run_analysis delinearizes each dataset's CV input by this base
      # (None/NA => already linear). Snapshot it under isolate() with the rest.
      params_snap <- if (is.null(gp)) list() else (gp$parameters %||% list())
      log_base_by_ds <- lapply(params_snap, function(p) {
        p$log_transformation %||% NA_character_
      })

      # Pre-flight validation (pure). Render inline + bail on failure.
      validation <- pelsa_validate_setup(snapshot, gcts_processed, database_dir)
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
          # Read the species FASTA + feature cache ONCE (shared across datasets
          # of the same species). Off the per-dataset loop.
          setProgress(value = 0.05, detail = "Loading FASTA")
          fasta_path <- pelsa_species_fasta_path(database_dir, snapshot$species)
          fasta_map  <- pelsa_read_fasta(fasta_path)

          setProgress(value = 0.15, detail = "Reading feature annotation cache")
          species_dir <- file.path(database_dir, snapshot$species)
          feat_df <- pelsa_read_feature_cache(species_dir)

          pelsa_run_analysis(
            gcts           = gcts_processed,
            gcts_original  = gcts_raw,
            setup_snapshot = snapshot,
            fasta_map      = fasta_map,
            feat_df        = feat_df,
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
