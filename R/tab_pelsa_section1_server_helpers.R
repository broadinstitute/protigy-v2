################################################################################
# PELSA Setup section (Section 1) - reactive server-wiring helper.
#
# Extracted from PELSASection1_Tab_Server() (see tab_pelsa_section1.R) to keep
# that function under the file's coding-style size budget. This is NOT pure
# logic (it wires observers/outputs against a live Shiny session), so it lives
# alongside the tab server rather than in tab_pelsa_section1_helpers.R (which
# is reserved for logic that unit-tests without a reactive context).
#
# pelsa_wire_dataset_config() wires the PER-DATASET CONDITION / REPLICATE
# CONFIG + ORDERING block (5B): the per-dataset config panel, per-condition
# replicate cards, the observer-dedup registries, the "apply this dataset's
# setup to all others" button, and the canonical sample_order computation.
# It is called ONCE from PELSASection1_Tab_Server with that function's shared
# reactives/closures (setup_state, session/input/output/ns, GCTs/all_omes,
# active_setup_ome, set_ds, cur_markers/set_markers, last_autofilled_compound,
# checked_datasets, setup_observer_registry) and performs its wiring entirely
# via side effects (observers/outputs registered on the passed-in
# `session`/`input`/`output`); it returns invisible(NULL).
#
# NOTE: checked_datasets and setup_observer_registry are created in
# PELSASection1_Tab_Server (not here) and passed BY REFERENCE, because
# test-pelsa-setup.R's testServer() test cases call them by bare name from
# that function's own environment (testServer evaluates test code inside the
# module server's environment, which does not see this helper's locals).
#
# pelsa_make_setup_export() is a small factory: given the shared setup_state
# reactiveValues + the pelsa_analysis reactiveVal, it returns
# make_setup_export(ome), itself a factory for the 01_setup export bundle
# function (pelsa_setup.yaml + pelsa_markers.csv + copied FASTA/annotation)
# that PELSASection1_Tab_Server attaches to every ome's export list.
################################################################################

# @noRd
pelsa_wire_dataset_config <- function(input, output, session, ns,
                                      setup_state, GCTs, all_omes,
                                      active_setup_ome, set_ds,
                                      cur_markers, set_markers,
                                      last_autofilled_compound,
                                      checked_datasets,
                                      setup_observer_registry) {

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

  # set_ds() (the immutable per-dataset setter) is passed in from
  # PELSASection1_Tab_Server: it is shared with the marker-table wiring defined
  # there, so it is defined once at that scope rather than redefined here.

  # Immutable nested setter for replicate_order[[ome]][[cond]] (one condition's
  # sample order). Keeps the doubly-nested named-list update immutable + terse.
  set_ds_rep <- function(ome, cond, value) {
    by_cond <- setup_state$replicate_order[[ome]] %||% list()
    by_cond[[cond]] <- value
    set_ds("replicate_order", ome, by_cond)
  }

  # checked_datasets (the ANALYZED/non-skipped datasets, in all_omes() order)
  # is passed in from PELSASection1_Tab_Server rather than created here, so
  # test-pelsa-setup.R's testServer() cases can call it by bare name from that
  # function's own environment.

  # The apply-all SOURCE dataset: the ACTIVE setup tab (the dataset whose form
  # is on screen). NULL when there is no valid active dataset.
  apply_all_source <- function() active_setup_ome()

  # ---- per-dataset config UI (ONLY the active setup tab's panel) ------------
  # With per-dataset tabs, one dataset's config is shown at a time (the active
  # setup tab), not a stacked list. The observers + sample_order still iterate
  # ALL non-skipped datasets (registered as tabs are visited), so summary/
  # volcano get orders even for not-yet-opened tabs.
  output$pelsa_perdataset_config <- shiny::renderUI({
    ome <- active_setup_ome()
    if (is.null(ome)) {
      return(shiny::helpText("Select a dataset to configure conditions."))
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
  rendered_card_outputs <- shiny::reactiveVal(character(0))

  register_replicate_card_renderer <- function(ome) {
    i  <- .ds_index(ome)
    out_id <- sprintf("pelsa_replicate_cards_d%d", i)
    reg <- rendered_card_outputs()
    if (out_id %in% reg) return(invisible())

    local({
      ome_local <- ome
      i_local   <- i
      output[[out_id]] <- shiny::renderUI({
        cdesc <- cdesc_for(ome_local)
        if (is.null(cdesc)) return(NULL)
        conds <- distinct_conditions_for(ome_local)
        if (length(conds) == 0L) {
          return(shiny::helpText("Choose a condition column to order replicates."))
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
        do.call(shiny::tagList, cards)
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
    shinyjqui::updateOrderInput(session, inputId = id_condition_order(i), items = order)
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
        shinyjqui::updateOrderInput(session, inputId = id_replicate_order(i, j), items = order)
      }
    }
    set_ds("replicate_order", ome, new_by_cond)
  }

  # ---- observer-dedup registry (prevents leaks on re-render) ----------------
  # Dynamic per-dataset/per-condition inputs are a classic observer-leak
  # source. Track which observer KEYS exist and never create one twice
  # (mirrors tab_stat_plot.R's poi_observer_registry). setup_observer_registry
  # is passed in (see the top-of-file note) rather than created here.

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
      shiny::observeEvent(input[[id_condition_col(i_local)]], {
        set_ds("condition_col", ome_local, input[[id_condition_col(i_local)]])
        set_ds("condition_order", ome_local, NULL)
        set_ds("replicate_order", ome_local, NULL)
        seed_condition_order(ome_local)
        seed_replicate_orders(ome_local)
        register_condition_observers(ome_local)
      }, ignoreNULL = TRUE)

      shiny::observeEvent(input[[id_replicate_col(i_local)]], {
        set_ds("replicate_col", ome_local, input[[id_replicate_col(i_local)]])
        set_ds("replicate_order", ome_local, NULL)
        seed_replicate_orders(ome_local)
      }, ignoreNULL = TRUE)

      # Condition order drag -> setup_state.
      shiny::observeEvent(input[[id_condition_order(i_local)]], {
        set_ds("condition_order", ome_local, input[[id_condition_order(i_local)]])
      }, ignoreNULL = FALSE)

      # Condition reset -> default order.
      shiny::observeEvent(input[[id_condition_reset(i_local)]], {
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
        shiny::observeEvent(input[[id_replicate_order(i_local, j_local)]], {
          cond <- cond_at_j()
          if (is.null(cond)) return()
          set_ds_rep(ome_local, cond,
                     input[[id_replicate_order(i_local, j_local)]])
        }, ignoreNULL = FALSE)

        # Replicate reset -> default sample sort for the condition at j.
        shiny::observeEvent(input[[id_replicate_reset(i_local, j_local)]], {
          cond <- cond_at_j()
          if (is.null(cond)) return()
          default_samples <- default_samples_local()
          shinyjqui::updateOrderInput(session, inputId = id_replicate_order(i_local, j_local),
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
  shiny::observeEvent(checked_datasets(), {
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
  shiny::observeEvent(active_setup_ome(), {
    ome <- active_setup_ome(); shiny::req(ome)
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
  shiny::observe({
    shinyjs::toggleState("pelsa_apply_all", condition = length(all_omes()) > 1L)
  })

  shiny::observeEvent(input$pelsa_apply_all, {
    src <- apply_all_source()
    if (is.null(src)) {
      shiny::showNotification("Select a dataset before applying its setup to others.",
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
        shiny::updateSelectInput(session, id_condition_col(i), selected = src_cond)
        shiny::updateSelectInput(session, id_replicate_col(i), selected = src_rep)
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
      shiny::showNotification(sprintf(
        paste0("Applied %s's FASTA, annotation, compound, and markers to all ",
               "other datasets; condition/replicate columns copied where present."),
        src), type = "message", duration = 5)
    } else {
      shiny::showNotification("No other datasets to apply this setup to.",
                       type = "warning", duration = 3)
    }
    if (length(col_skipped)) {
      shiny::showNotification(sprintf(
        paste0("Condition/replicate columns not copied to %d dataset(s) ",
               "lacking matching columns: %s (FASTA/annotation/compound/markers ",
               "still applied)."),
        length(col_skipped), paste(col_skipped, collapse = ", ")
      ), type = "warning", duration = 6)
    }
  }, ignoreInit = TRUE)

  # ---- canonical sample_order (what Summary/Volcano consume) ----------------
  # Recomputed for every checked dataset on any ordering / column change.
  shiny::observe({
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

  invisible(NULL)
}

# The 01_setup export bundle factory for one ome: run configuration
# (pelsa_setup.yaml) + the marker table (pelsa_markers.csv), plus the copied
# FASTA/annotation input files + missing-accessions list. Reads the shared
# setup_state/pelsa_analysis at EXPORT TIME (not capture time) via isolate(),
# so the exported bundle reflects the latest run configuration.
# @noRd
pelsa_make_setup_export <- function(setup_state, pelsa_analysis) {
  function(ome) function(dir_name) {
    out <- pelsa_export_stage_dir(dir_name, .PELSA_STAGE_SETUP)
    ss <- tryCatch(shiny::isolate(pelsa_setup_snapshot(setup_state)),
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
    res <- tryCatch(shiny::isolate(pelsa_analysis())[[ome]], error = function(e) NULL)
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
}
