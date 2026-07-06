################################################################################
# Module: PELSA - Section 3 (Volcano Plot)
#
# Renders the PELSA volcano for the ACTIVE dataset. The PELSA volcano does NOT
# compute differential statistics (Decision A): it CONSUMES the Statistics tab's
# stat_results()/stat_params() (logFC.<c> / adj.P.Val.<c> / P.Value.<c> columns)
# and the 5D analysis cache (pelsa_analysis) + setup_state (markers + species).
#
# Layers (this pass, 7A-7C):
#   7A  stat-source gate - grey out until a stat analysis is run for the active
#       dataset (mirrors tab_stat_plot.R's validate(need(stat_results(), ...))).
#   7B  a per-contrast POI registry (marker accessions), reused VERBATIM from
#       tab_stat_plot.R, seeded with the Setup marker accessions; lazy contrast
#       loading - only the ACTIVE contrast's heavy volcano df is held, the prior
#       contrast's df is freed on switch (the registry persists user settings).
#       Label mode is a SEPARATE, per-OME (not per-contrast) registry: one
#       selection applies to every contrast of the active dataset.
#   7C  the WebGL volcano: 3A pelsa_build_volcano_df() (cached per contrast) ->
#       ggplot -> ggplotly + plotly::toWebGL, with a single color toggle
#       (two-sided significance vs UniProt feature class), a magenta marker
#       overlay always on top, label modes (all marker peptides / all
#       significant peptides, either/both/neither), the empirical y-cutoff
#       threshold line, and a metadata hover tooltip. The FULL df is rendered
#       (every point) - toWebGL handles 100k+ points on the GPU, so NO
#       background downsampling is applied (per user decision: draw all
#       points). The 3B pelsa_thin_background() helper remains in the package
#       but is intentionally NOT wired into the volcano render.
#
# Pass 2 (built): 7D best-peptide second panel (lazy, panel="best_peptide");
# 7E a single selection() (a left-click or a Find-accession) drives the volcano
# highlight via a GOLD OVERLAY: a separate scattergl trace (plus an optional
# label trace) pushed/removed with plotlyProxyInvoke addTraces/deleteTraces
# (apply_gold_overlay), so the ~100k-point base figure is NEVER rebuilt on
# click/find/clear (per-point marker.color restyle is unreliable on WebGL
# scattergl, so an overlay trace is used rather than a proxy restyle) AND opens
# the per-protein intensity line panel (3C); 7F per-ome exports.
#
# Pure plot-assembly / shaping logic: R/tab_pelsa_volcano_helpers.R (tested).
################################################################################

# --- Volcano base-rebuild / overlay-reset trigger contract -------------------
# The gold-overlay (overlay_n) tracks how many proxy-added highlight traces sit
# on the CLIENT figure. Anything that rebuilds the ~100k-point base figure wipes
# those traces, so overlay_n MUST be reset (to 0) and the overlay re-applied on
# the new figure. The base rebuilds for two families of reasons:
#   (1) display controls (color mode, contrast, label mode, WebGL flip), and
#   (2) volcano_df_cache clears (marker-add, significance cutoff, significance stat)
#       -> active_volcano_df() recomputes -> renderPlotly rebuilds the base.
# These two pure helpers are the SINGLE SOURCE OF TRUTH the section3 observers
# key off. The reset set MUST be a superset of the clear set, else a cache clear
# rebuilds the base without resetting overlay_n and the next apply_gold_overlay()
# deletes trace indices that no longer exist (dropping the markers trace). The
# unit test test-pelsa-overlay-reset.R enforces that invariant so the two lists
# cannot drift apart. @noRd
.pelsa_volcano_cache_clear_reasons <- function() {
  c("markers", "sig_cutoff", "sig_stat")
}
.pelsa_volcano_overlay_reset_reasons <- function() {
  c("color_mode", "contrast", "label_mode", "n_top_significant",
    "n_top_markers", "use_webgl", .pelsa_volcano_cache_clear_reasons())
}

################################################################################
# Tab-level UI and Server (active-dataset view + parent-level registries)
################################################################################

PELSASection3_Tab_UI <- function(id = "PELSASection3Tab") {
  ns <- NS(id)
  tagList(
    fluidRow(uiOutput(ns("ome_tabset_box")))
  )
}

PELSASection3_Tab_Server <- function(id = "PELSASection3Tab",
                                     GCTs_and_params,
                                     globals,
                                     GCTs_original,
                                     active_dataset,
                                     stat_results = NULL,
                                     stat_params = NULL,
                                     pelsa_analysis = NULL,
                                     pelsa_setup_state = NULL,
                                     marker_add_request = NULL) {

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

    # Client WebGL capability (set by the app_UI probe via app_server). Reactive
    # so the volcano re-renders into SVG if the probe reports FALSE after the
    # first paint. Default TRUE (webgl_capability(NULL)) keeps the WebGL path for
    # capable clients with no extra render.
    use_webgl <- reactive(webgl_capability(globals$webgl_supported))

    # Tolerate being called without the Statistics-tab / cache seams (older test
    # callers). Each is a no-arg reactive returning NULL when absent.
    stat_results_r <- if (is.function(stat_results)) stat_results else reactive(NULL)
    stat_params_r  <- if (is.function(stat_params)) stat_params else reactive(NULL)
    analysis_r     <- if (is.function(pelsa_analysis)) pelsa_analysis else reactive(NULL)
    setup_state_r  <- if (is.function(pelsa_setup_state)) pelsa_setup_state else reactive(NULL)

    ## ACTIVE-DATASET VIEW ##
    # The app-level switcher chooses the active dataset; we render a single
    # active-dataset view (not a per-ome tabset).

    output$ome_tabset_box <- renderUI({
      ome <- active_dataset()
      req(ome, ome %in% all_omes())
      add_css_attributes(
        shinydashboardPlus::box(
          PELSASection3_Ome_UI(id = ns(ome), ome = ome),
          width = 12
        ),
        classes = c("box-no-header", "box-with-tabs")
      )
    })

    ## PARENT-LEVEL PER-CONTRAST REGISTRIES (reused from tab_stat_plot.R) ##
    # Keyed "<ome>::<contrast>", passed BY REFERENCE into each ome server so
    # marker list + per-contrast settings survive contrast switches while the
    # heavy plot for the inactive contrast is freed.
    poi_registry        <- reactiveVal(list())  # <key> -> character() marker/POI accessions
    label_mode_registry        <- reactiveVal(list())  # ome -> character() label mode
    n_top_significant_registry <- reactiveVal(list())  # ome -> integer N (top_n_significant)
    n_top_markers_registry     <- reactiveVal(list())  # ome -> integer N (top_n_markers)

    # Per-ome (per-dataset) server: instantiate ONCE per ome and reuse. Each
    # PELSASection3_Ome_Server() registers many live observers (volcano/Woods
    # click, marker-add, cache persisters); re-calling it for an already-started
    # ome stacks duplicate observers (a leak). all_omes() re-emits on every
    # dataset add/remove (GCTs_and_params is replaced whole during setup), so we
    # guard with a started-registry and only instantiate omes we have not seen.
    ome_export_store <- reactiveVal(list())  # ome -> exports, persists re-emits
    started_omes     <- reactiveVal(character(0))
    observeEvent(all_omes(), {
      new_omes <- setdiff(all_omes(), started_omes())
      if (length(new_omes) == 0L) return()
      new_exports <- sapply(new_omes, function(ome) {
        PELSASection3_Ome_Server(
          id                        = ome,
          ome                       = ome,
          GCT_processed             = reactive(GCTs()[[ome]]),
          parameters                = reactive(parameters()[[ome]]),
          default_annotation_column = reactive(default_annotations()[[ome]]),
          color_map                 = reactive(custom_colors()[[ome]]),
          stat_results              = stat_results_r,
          stat_params               = stat_params_r,
          pelsa_analysis            = analysis_r,
          pelsa_setup_state         = setup_state_r,
          poi_registry              = poi_registry,
          label_mode_registry       = label_mode_registry,
          n_top_significant_registry = n_top_significant_registry,
          n_top_markers_registry    = n_top_markers_registry,
          marker_add_request        = marker_add_request,
          use_webgl                 = use_webgl
        )
      }, simplify = FALSE)
      started_omes(c(started_omes(), new_omes))
      ome_export_store(modifyList(ome_export_store(), new_exports))
    })

    # Expose exports for the CURRENTLY-present omes only (a removed ome's stored
    # exports are dropped from the gathered set without re-instantiating others).
    all_exports <- reactive({
      store <- ome_export_store()
      store[intersect(all_omes(), names(store))]
    })

    return(all_exports)
  })
}

################################################################################
# Per-ome (per-dataset) UI and Server
################################################################################

PELSASection3_Ome_UI <- function(id, ome) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("section_contents"))
  )
}

PELSASection3_Ome_Server <- function(id,
                                     ome,
                                     GCT_processed,
                                     parameters,
                                     default_annotation_column,
                                     color_map,
                                     stat_results = reactive(NULL),
                                     stat_params = reactive(NULL),
                                     pelsa_analysis = reactive(NULL),
                                     pelsa_setup_state = reactive(NULL),
                                     poi_registry = NULL,
                                     label_mode_registry = NULL,
                                     n_top_significant_registry = NULL,
                                     n_top_markers_registry = NULL,
                                     marker_add_request = NULL,
                                     use_webgl = reactive(TRUE)) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # The PELSA volcano always renders SVG (scatter), regardless of the
    # injected use_webgl reactive (client WebGL-capability probe). scattergl's
    # WebGL renderer can silently mis-paint per-point marker.color arrays on
    # some GPU/driver clients -- the color/coloring bug this masks is not a
    # "renders blank" failure (which the probe catches) but a "renders wrong
    # colors while reporting WebGL-capable" failure, which is undetectable
    # from the client probe. The R-built figure JSON is proven correct in
    # both scattergl and scatter modes (verified by direct reproduction), so
    # forcing SVG trades away WebGL's GPU rendering speed (unneeded at PELSA's
    # realistic point counts) for reliable significance/feature coloring.
    use_webgl <- reactive(FALSE)

    ## ------------------------------------------------------------------------
    ## 7A - STAT-SOURCE GATE
    ## ------------------------------------------------------------------------
    # stat_results()[[ome]] must exist and be non-empty before any volcano. The
    # check reactive carries the validate() so dependent outputs grey out with a
    # clear notice (mirrors tab_stat_plot.R's stat_results_check()).
    stat_df_raw <- reactive({
      validate(need(
        is.data.frame(stat_results()[[ome]]) && nrow(stat_results()[[ome]]) > 0L,
        "Run a statistical analysis in the Statistics tab first."
      ))
      stat_results()[[ome]]
    })

    # The active dataset's analysis cache entry (or NULL). The volcano feature
    # annotation reads its matched frame + species feature table.
    cache_entry <- reactive({
      cache <- pelsa_analysis()
      if (is.null(cache) || is.null(cache[[ome]])) return(NULL)
      entry <- cache[[ome]]
      if (pelsa_analysis_failed(entry)) return(NULL)
      entry
    })

    # Setup marker accessions (the volcano overlay + POI seed).
    marker_accessions <- reactive({
      ss <- pelsa_setup_state()
      # Markers are PER-OME: read THIS dataset's marker list.
      rows <- if (is.null(ss)) NULL else ss$marker_rows[[ome]]
      if (is.null(rows) || !is.data.frame(rows) ||
          !"accession" %in% colnames(rows)) {
        return(character(0))
      }
      acc <- as.character(rows$accession)
      unique(acc[!is.na(acc) & nzchar(acc)])
    })

    # The active dataset's confirmed condition ORDER (x-axis factor levels for
    # the 3C intensity line plot). NULL when setup_state lacks it.
    condition_order_r <- reactive({
      ss <- pelsa_setup_state()
      co <- if (is.null(ss)) NULL else ss$condition_order
      if (is.null(co)) NULL else co[[ome]]
    })

    # The active dataset's processed numeric matrix (peptides x samples), AS-IS
    # log2 intensities the 3C intensity builder reads. NULL when no GCT.
    processed_mat_r <- reactive({
      gct <- tryCatch(GCT_processed(), error = function(e) NULL)
      if (is.null(gct)) return(NULL)
      tryCatch({
        if (methods::is(gct, "GCT")) methods::slot(gct, "mat")
        else if (is.matrix(gct)) gct
        else NULL
      }, error = function(e) NULL)
    })

    # sample -> condition map aligned to processed_mat columns (3C consumes a
    # named char vector). Built from the dataset's cdesc condition column.
    condition_map_r <- reactive({
      gct <- tryCatch(GCT_processed(), error = function(e) NULL)
      pm <- processed_mat_r()
      ss <- pelsa_setup_state()
      if (is.null(gct) || is.null(pm) || is.null(ss)) return(NULL)
      cond_col <- ss$condition_col[[ome]]
      if (is.null(cond_col) || !nzchar(cond_col)) return(NULL)
      cdesc <- tryCatch(.pelsa_gct_cdesc(gct), error = function(e) NULL)
      if (is.null(cdesc)) return(NULL)
      tryCatch(
        pelsa_condition_map_for(cdesc, colnames(pm), cond_col),
        error = function(e) NULL
      )
    })

    # Narrow seam: feat_df reads ONLY this dataset's uploaded annotation path off
    # setup_state. Depending on the whole setup_state() would re-read the
    # annotation file on ANY setup-state change (condition columns, orders, ...);
    # scoping to annotation_path_r() means feat_df re-reads only when the uploaded
    # annotation file actually changes.
    annotation_path_r <- reactive({
      ss <- pelsa_setup_state()
      # Self-curated datasets have no annotation file -> NULL (3A colors "none").
      if (is.null(ss) || isTRUE(ss$self_curated[[ome]])) return(NULL)
      ss$annotation_path[[ome]]
    })

    # Per-dataset feature table (2I/3A feat_df), read + classified once per
    # uploaded annotation file via pelsa_read_annotation_file(). Read-only; NO
    # network. NULL when unavailable / self-curated (3A then colors "none").
    #
    # Cache preference (D10/D11): when a run cache entry exists and carries the
    # feat_raw table captured at run time, return it directly so the volcano,
    # Woods, and exports all colour from the SAME annotation snapshot the
    # Summary QC counts used.  Fall through to the live-file path only when no
    # cache entry is present (pre-run state or older cache without feat_raw).
    feat_df <- reactive({
      entry <- cache_entry()
      if (!is.null(entry) && is.data.frame(entry$feat_raw)) {
        return(entry$feat_raw)
      }
      # Fallback (no run yet / older cache): read the live annotation file.
      ap <- annotation_path_r()
      # Silent NULL: self-curated or nothing uploaded (no path to fail).
      if (is.null(ap) || length(ap) != 1L || is.na(ap) || !nzchar(ap)) {
        return(NULL)
      }
      # A path WAS provided but the file is gone -> visible failure, not silent.
      if (!file.exists(ap)) {
        message("PELSA feat_df: annotation file missing at ", ap)
        showNotification(
          "Feature annotation file is missing -- peptides shown unannotated.",
          type = "warning", duration = 8
        )
        return(NULL)
      }
      tryCatch(
        pelsa_read_annotation_file(ap),
        error = function(e) {
          message("PELSA feat_df: annotation read error: ", conditionMessage(e))
          showNotification(
            paste0("Could not read the feature annotation file -- peptides ",
                   "shown unannotated: ", conditionMessage(e)),
            type = "warning", duration = 8
          )
          NULL
        }
      )
    })

    # Whether this dataset is a self-curated database (no annotation file). Read
    # straight off setup_state -- NO network, NO disk. Gates the
    # annotation-dependent UI + forces accession labels.
    is_self_curated_r <- reactive({
      ss <- pelsa_setup_state()
      isTRUE(if (is.null(ss)) FALSE else ss$self_curated[[ome]])
    })

    ## ------------------------------------------------------------------------
    ## 7B - CONTRAST SELECTOR + PER-CONTRAST REGISTRIES + LAZY LOADING
    ## ------------------------------------------------------------------------
    # Contrast choices: named vector label("A / B") -> suffix("A_over_B").
    contrast_choices <- reactive({
      pelsa_volcano_contrast_choices(stat_params(), ome)
    })

    # The active contrast SUFFIX (stat-column key), driven by the selector.
    active_contrast <- reactive({
      choices <- contrast_choices()
      if (length(choices) == 0L) return(NULL)
      sel <- input$pelsa_volcano_contrast
      if (is.null(sel) || !nzchar(sel) || !(sel %in% unname(choices))) {
        return(unname(choices)[[1]])  # default to the first contrast
      }
      sel
    })

    # The registry key "<ome>::<contrast>".
    current_contrast_key <- reactive({
      pelsa_volcano_contrast_key(ome, active_contrast())
    })

    # Seed poi_registry[[key]] with the Setup marker accessions the FIRST time a
    # contrast is visited (so the marker list is retained per contrast). Never
    # overwrites a slot the user has since edited.
    observeEvent(current_contrast_key(), {
      key <- current_contrast_key()
      if (is.null(key) || is.null(poi_registry)) return()
      reg <- poi_registry()
      if (is.null(reg[[key]])) {
        reg[[key]] <- isolate(marker_accessions())
        poi_registry(reg)
      }
    }, ignoreNULL = TRUE)

    # Label mode is PER-OME, not per-contrast: selecting a mode applies to
    # EVERY contrast of the active dataset by default (there is no per-
    # contrast override). Default character(0) - no labels. A CHARACTER
    # VECTOR: zero or more of "all_markers" / "all_significant" (the
    # checkbox-group selection). Keyed by `ome` alone in label_mode_registry
    # (still threaded in from PELSASection3_Tab_Server so every Ome_Server
    # instance has its own slot, matching the shape of poi_registry).
    label_mode_for_ome <- reactive({
      reg <- if (is.null(label_mode_registry)) list() else label_mode_registry()
      reg[[ome]] %||% .PELSA_VOLCANO_DEFAULT_LABEL_MODE
    })
    set_label_mode <- function(modes) {
      if (is.null(label_mode_registry)) return()
      reg <- label_mode_registry()
      reg[[ome]] <- as.character(modes %||% character(0))
      label_mode_registry(reg)
    }

    # Persist label-mode edits into the shared per-ome registry slot. Because
    # the slot is keyed by `ome` (not `"<ome>::<contrast>"`), this single write
    # is immediately visible to every contrast's render call (they all read
    # label_mode_for_ome()) - no separate "apply to all contrasts" action is
    # needed; changing the checkboxes IS applying to every contrast.
    # ignoreNULL = FALSE: an all-unchecked checkboxGroupInput reports NULL,
    # not character(0), and that NULL must still clear the stored selection.
    observeEvent(input$pelsa_label_mode, {
      set_label_mode(input$pelsa_label_mode)
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    # Per-ome N for "top_n_significant" (default 5); same ome-only scope as
    # label_mode_registry - applies to every contrast automatically.
    n_top_significant_for_ome <- reactive({
      reg <- if (is.null(n_top_significant_registry)) list() else n_top_significant_registry()
      reg[[ome]] %||% 5L
    })
    set_n_top_significant <- function(n) {
      if (is.null(n_top_significant_registry)) return()
      reg <- n_top_significant_registry()
      val <- suppressWarnings(as.integer(n)[1L])
      reg[[ome]] <- if (is.na(val)) 1L else max(1L, val)
      n_top_significant_registry(reg)
    }
    observeEvent(input$pelsa_n_top_significant, {
      set_n_top_significant(input$pelsa_n_top_significant)
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    # Per-ome N for "top_n_markers" (default 5); same ome-only scope.
    n_top_markers_for_ome <- reactive({
      reg <- if (is.null(n_top_markers_registry)) list() else n_top_markers_registry()
      reg[[ome]] %||% 5L
    })
    set_n_top_markers <- function(n) {
      if (is.null(n_top_markers_registry)) return()
      reg <- n_top_markers_registry()
      val <- suppressWarnings(as.integer(n)[1L])
      reg[[ome]] <- if (is.na(val)) 1L else max(1L, val)
      n_top_markers_registry(reg)
    }
    observeEvent(input$pelsa_n_top_markers, {
      set_n_top_markers(input$pelsa_n_top_markers)
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    # Mutual exclusion, two INDEPENDENT pairs (significant pair does not
    # affect the marker pair): checking "top_n_significant" unchecks+disables
    # "all_significant" and vice versa; checking "top_n_markers"
    # unchecks+disables "all_markers" and vice versa. Mirrors the pattern in
    # R/tab_stat_plot.R:315-350 (a different subsystem's volcano, same idiom).
    observeEvent(input$pelsa_label_mode, {
      grp_id <- ns("pelsa_label_mode")
      modes <- input$pelsa_label_mode %||% character(0)

      if ("top_n_significant" %in% modes) {
        updateCheckboxGroupInput(session, "pelsa_label_mode",
          selected = setdiff(modes, "all_significant"))
        shinyjs::runjs(sprintf(
          "$('#%s input[value=\"all_significant\"]').prop('disabled', true).closest('label').css('opacity', 0.4);",
          grp_id))
      } else if ("all_significant" %in% modes) {
        updateCheckboxGroupInput(session, "pelsa_label_mode",
          selected = setdiff(modes, "top_n_significant"))
        shinyjs::runjs(sprintf(
          "$('#%s input[value=\"top_n_significant\"]').prop('disabled', true).closest('label').css('opacity', 0.4);",
          grp_id))
      } else {
        shinyjs::runjs(sprintf(
          "$('#%s input[value=\"all_significant\"], #%s input[value=\"top_n_significant\"]').prop('disabled', false).closest('label').css('opacity', 1);",
          grp_id, grp_id))
      }

      if ("top_n_markers" %in% modes) {
        updateCheckboxGroupInput(session, "pelsa_label_mode",
          selected = setdiff(modes, "all_markers"))
        shinyjs::runjs(sprintf(
          "$('#%s input[value=\"all_markers\"]').prop('disabled', true).closest('label').css('opacity', 0.4);",
          grp_id))
      } else if ("all_markers" %in% modes) {
        updateCheckboxGroupInput(session, "pelsa_label_mode",
          selected = setdiff(modes, "top_n_markers"))
        shinyjs::runjs(sprintf(
          "$('#%s input[value=\"top_n_markers\"]').prop('disabled', true).closest('label').css('opacity', 0.4);",
          grp_id))
      } else {
        shinyjs::runjs(sprintf(
          "$('#%s input[value=\"all_markers\"], #%s input[value=\"top_n_markers\"]').prop('disabled', false).closest('label').css('opacity', 1);",
          grp_id, grp_id))
      }
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    ## --- LAZY per-active-contrast volcano df cache --------------------------
    # Holds ONLY the active contrast's heavy 3A df, keyed by contrast suffix.
    # On contrast switch the prior contrast's df is FREED (the list is replaced
    # with a single-entry list for the new active contrast). The registries -
    # not this cache - persist user-facing settings across switches.
    volcano_df_cache <- reactiveVal(list())

    # M5: when the marker list changes, drop the cached volcano dfs so the active
    # contrast rebuilds with the updated markers. active_volcano_df() and
    # best_volcano_df() each read their own cache and bake the markers in at build
    # time, so BOTH must be cleared - otherwise a newly-added marker is not flagged
    # on the live view (all-peptide or best-peptide panel) until a contrast /
    # color-mode switch happens to free that cache. best_volcano_df_cache is
    # defined later (it is only referenced when this observer fires, so the forward
    # reference is safe).
    observeEvent(marker_accessions(), {
      volcano_df_cache(list())
      best_volcano_df_cache(list())
    }, ignoreInit = TRUE)

    # Significance cutoff: SINGLE source of truth shared with the Statistics tab.
    # The user sets it once in Statistics > Summary (stat_params()[[ome]]$cutoff)
    # and it drives the PELSA volcano's significance coloring + the dashed
    # threshold line + the export annotation. Falls back to the default constant
    # when stats are not set up yet or the value is missing/invalid.
    sig_cutoff_r <- reactive({
      sp <- stat_params()
      cut <- if (is.null(sp) || is.null(sp[[ome]])) NULL else sp[[ome]]$cutoff
      cut <- suppressWarnings(as.numeric(cut))
      if (length(cut) != 1L || is.na(cut) || cut <= 0 || cut > 1)
        .PELSA_EXPORT_SIG_CUTOFF
      else cut
    })

    # Changing the cutoff (in Statistics) must rebuild the PELSA volcano: drop the
    # cached dfs so active_volcano_df()/best_volcano_df() recompute Significant /
    # sig_direction and the empirical y_cutoff at the new threshold.
    observeEvent(sig_cutoff_r(), {
      volcano_df_cache(list())
      best_volcano_df_cache(list())
    }, ignoreInit = TRUE)

    # Significance STATISTIC: also SHARED with the Statistics tab (Statistics >
    # Summary, stat_params()[[ome]]$stat). "nom.p.val" classifies significance on
    # the raw P.Value and draws the dashed line at -log10(cutoff); "adj.p.val"
    # (the default) classifies on adj.P.Val. The PELSA volcano must honor the same
    # choice so it agrees with the Statistics volcano on identical data + cutoff.
    sig_stat_r <- reactive({
      sp <- stat_params()
      st <- if (is.null(sp) || is.null(sp[[ome]])) NULL else sp[[ome]]$stat
      if (identical(st, "nom.p.val")) "nom.p.val" else "adj.p.val"
    })

    # A stat change (adj.p.val <-> nom.p.val) must also rebuild: it flips
    # Significant / sig_direction and the y_cutoff for the SAME cutoff.
    observeEvent(sig_stat_r(), {
      volcano_df_cache(list())
      best_volcano_df_cache(list())
    }, ignoreInit = TRUE)

    active_volcano_df <- reactive({
      contrast <- active_contrast()
      req(contrast)
      stat_raw <- stat_df_raw()  # carries the gate
      validate(need(
        pelsa_volcano_has_contrast(stat_raw, contrast),
        "Selected contrast has no statistics columns yet."
      ))
      entry <- cache_entry()
      validate(need(!is.null(entry),
                    "Run Start Analysis in the PELSA Setup tab first."))

      cache <- volcano_df_cache()
      if (!is.null(cache[[contrast]])) return(cache[[contrast]])

      matched <- entry$matched %||% data.frame()
      fdf <- feat_df() %||% data.frame(accession = character(0),
                                       start = integer(0), end = integer(0),
                                       feature_class = character(0))
      stat_df <- pelsa_volcano_stat_df(stat_raw, matched)
      df <- tryCatch(
        pelsa_build_volcano_df(
          stat_df       = stat_df,
          matched_cache = if (nrow(matched) > 0L) matched else
            pelsa_volcano_empty_matched(),
          feat_df       = fdf,
          markers       = isolate(marker_accessions()),
          contrast      = contrast,
          opts          = list(panel = "all_peptide",
                               sig_cutoff = sig_cutoff_r(),
                               sig_stat = sig_stat_r()),
          is_self_curated = is_self_curated_r()
        ),
        error = function(e) {
          showNotification(
            paste0("Could not build volcano data: ", conditionMessage(e)),
            type = "error", duration = 8
          )
          NULL
        }
      )
      validate(need(!is.null(df), "Volcano data could not be built."))

      # FREE the prior contrast: replace the cache with a SINGLE-entry list.
      volcano_df_cache(stats::setNames(list(df), contrast))
      df
    })

    # The frame the plot consumes = the FULL active volcano df (every point).
    # Background downsampling (3B pelsa_thin_background) was intentionally removed
    # per user decision: toWebGL renders all points on the GPU, so no thinning.
    plot_df <- reactive(active_volcano_df())

    ## ------------------------------------------------------------------------
    ## 7D - BEST-PEPTIDE SECOND PANEL (lazy: only when the checkbox is ON)
    ## ------------------------------------------------------------------------
    # Same stat_df/cache/feat_df/markers/contrast as the all-peptide df - only
    # opts$panel differs ("best_peptide" -> one dot per distinct best-peptide via
    # the 2G rollup). Cached per contrast like the all-peptide df, FREED on
    # switch. NEVER built when the checkbox is OFF (the reactive short-circuits).
    best_show <- reactive(isTRUE(input$pelsa_show_best_panel))
    best_volcano_df_cache <- reactiveVal(list())

    best_volcano_df <- reactive({
      req(best_show())
      contrast <- active_contrast()
      req(contrast)
      stat_raw <- stat_df_raw()
      validate(need(pelsa_volcano_has_contrast(stat_raw, contrast),
                    "Selected contrast has no statistics columns yet."))
      entry <- cache_entry()
      validate(need(!is.null(entry),
                    "Run Start Analysis in the PELSA Setup tab first."))

      cache <- best_volcano_df_cache()
      if (!is.null(cache[[contrast]])) return(cache[[contrast]])

      matched <- entry$matched %||% data.frame()
      fdf <- feat_df() %||% data.frame(accession = character(0),
                                       start = integer(0), end = integer(0),
                                       feature_class = character(0))
      stat_df <- pelsa_volcano_stat_df(stat_raw, matched)
      df <- tryCatch(
        pelsa_build_volcano_df(
          stat_df       = stat_df,
          matched_cache = if (nrow(matched) > 0L) matched else
            pelsa_volcano_empty_matched(),
          feat_df       = fdf,
          markers       = isolate(marker_accessions()),
          contrast      = contrast,
          opts          = list(panel = "best_peptide",
                               sig_cutoff = sig_cutoff_r(),
                               sig_stat = sig_stat_r()),
          is_self_curated = is_self_curated_r()
        ),
        error = function(e) {
          showNotification(
            paste0("Could not build best-peptide data: ", conditionMessage(e)),
            type = "error", duration = 8
          )
          NULL
        }
      )
      validate(need(!is.null(df), "Best-peptide data could not be built."))
      best_volcano_df_cache(stats::setNames(list(df), contrast))
      df
    })

    # Best-peptide panel: also the FULL df (no thinning - toWebGL renders all).
    best_plot_df <- reactive(best_volcano_df())

    # Free the best-panel cache whenever the checkbox goes OFF.
    observeEvent(best_show(), {
      if (!best_show()) best_volcano_df_cache(list())
    }, ignoreInit = TRUE)

    ## ------------------------------------------------------------------------
    ## 7E - PINNED peptide selection (left-click). reactiveVal holds the resolved
    ## click; HOVER never touches this path (the heavy intensity plot is a
    ## SEPARATE output computed only when this reactiveVal is set).
    ## ------------------------------------------------------------------------
    selection <- reactiveVal(NULL)  # list(origin, peptide_seq, accession, label, row)

    find_query  <- reactiveVal(NULL)   # last submitted Find text (or NULL)
    find_result <- reactiveVal(NULL)   # list(mask, accessions, count) or NULL

    # ONE place to clear the whole transient selection + find highlight.
    clear_selection <- function() {
      selection(NULL); find_query(NULL); find_result(NULL)
      updateTextInput(session, "pelsa_find_acc", value = "")
    }

    # CLEAR the selection on contrast switch: a selection made under contrast A
    # would otherwise survive into contrast B, where its highlighted siblings /
    # metadata / intensity panel describe a peptide selected under the OLD
    # contrast's coordinates (misleading). The contrast key changes on switch.
    observeEvent(current_contrast_key(), {
      clear_selection()
    }, ignoreInit = TRUE)

    ## ------------------------------------------------------------------------
    ## LAYOUT
    ## ------------------------------------------------------------------------
    output$section_contents <- renderUI({
      # COMBINED DUAL GATE (7A + cache): the volcano needs BOTH the Statistics
      # tab's results AND the PELSA Start-Analysis cache. Surface the whole
      # dependency as ONE thing at the section altitude so the user hits a single
      # clear notice (the in-plot guards below remain as defense-in-depth).
      #   1. stats missing  -> "Run a statistical analysis in the Statistics tab"
      #   2. cache missing   -> "Run Start Analysis in the PELSA Setup tab"
      # stat_df_raw() carries its own validate(); test it explicitly first so the
      # cache notice does not mask a missing-stats state.
      has_stats <- tryCatch({
        sr <- stat_results()
        is.list(sr) && is.data.frame(sr[[ome]]) && nrow(sr[[ome]]) > 0L
      }, error = function(e) FALSE)
      if (!isTRUE(has_stats)) {
        return(h4("Run a statistical analysis in the Statistics tab first."))
      }
      if (is.null(cache_entry())) {
        return(h4("Run Start Analysis in the PELSA Setup tab first."))
      }

      if (length(contrast_choices()) == 0L) {
        return(h4(paste0(
          "No two-sample contrast available. The PELSA volcano needs a ",
          "Two-sample Moderated T-test in the Statistics tab."
        )))
      }

      # L-SHAPED PINNED CARD: the pinned-peptide views form one continuous card -
      # the upper-left arm (metadata + intensity line plot) is visually continuous
      # with the full-width coverage/feature/Woods tracks along the bottom (the
      # `pelsa-pin-card` class draws them as one bordered surface). The volcano +
      # its controls are a SEPARATE card sitting in the top-right notch of the L.
      # The bottom tracks render only once a peptide is pinned.
      tagList(
        fluidRow(
          class = "pelsa-pin-card-top",
          # Upper-LEFT arm of the L: pinned metadata + intensity line plot.
          column(3,
            shinydashboardPlus::box(
              uiOutput(ns("pelsa_add_marker_ui")),
              uiOutput(ns("pelsa_pin_metadata")),
              plotly::plotlyOutput(ns("pelsa_intensity_plot"), height = "440px"),
              helpText("Click a point to pin its peptide profile."),
              width = NULL, title = "Pinned Peptide", headerBorder = TRUE,
              class = "pelsa-pin-arm"
            )
          ),
          # Top-RIGHT notch: the volcano (+ best-peptide panel) - its own card.
          column(6,
            tagList(
              shinydashboardPlus::box(
                plotly::plotlyOutput(ns("pelsa_volcano_plot"), height = "680px"),
                helpText(textOutput(ns("pelsa_marker_count"))),
                status = "primary", width = NULL, title = "PELSA Volcano",
                headerBorder = TRUE, solidHeader = TRUE
              ),
              conditionalPanel(
                condition = sprintf("input['%s']",
                                    ns("pelsa_show_best_panel")),
                shinydashboardPlus::box(
                  plotly::plotlyOutput(ns("pelsa_volcano_best_plot"),
                                       height = "560px"),
                  status = "primary", width = NULL,
                  title = "PELSA Volcano (best peptide per protein)",
                  headerBorder = TRUE, solidHeader = TRUE
                )
              )
            )
          ),
          column(3,
            shinydashboardPlus::box(
              uiOutput(ns("pelsa_volcano_sidebar")),
              width = NULL, title = "Plot Controls", headerBorder = TRUE
            )
          )
        ),
        # Bottom arm of the L: the full-width per-protein 3-track panel (coverage
        # ruler + UniProt features + Woods), revealed on pin. Same `pelsa-pin-card`
        # styling as the upper-left arm so they read as ONE card.
        fluidRow(
          class = "pelsa-pin-card-bottom",
          column(12,
            shinydashboardPlus::box(
              plotly::plotlyOutput(ns("pelsa_woods_panel"), height = "420px"),
              helpText(paste0("Coverage (gold = residues with peptide evidence); ",
                              "UniProt features (hover for overlapping peptides); ",
                              "Woods plot (y = logFC direction; color = significance ",
                              "magnitude, -log10 adj.P). Click a Woods peptide to ",
                              "select it.")),
              width = NULL, title = "Protein coverage & Woods plot",
              headerBorder = TRUE, class = "pelsa-pin-arm"
            )
          )
        )
      )
    })

    output$pelsa_volcano_sidebar <- renderUI({
      choices <- contrast_choices()
      req(length(choices) > 0L)
      tagList(
        selectInput(
          ns("pelsa_volcano_contrast"), "Select Contrast:",
          choices  = choices,                 # named: label -> suffix
          selected = isolate(active_contrast())
        ),
        hr(),
        tags$strong("Find / highlight a protein:"),
        textInput(ns("pelsa_find_acc"), label = NULL,
                  placeholder = "accession e.g. P12345"),
        actionButton(ns("pelsa_find_go"), "Highlight", class = "btn-sm"),
        actionButton(ns("pelsa_clear_sel"), "Clear selection & highlight",
                     class = "btn-sm"),
        uiOutput(ns("pelsa_find_notice")),
        hr(),
        # SINGLE color toggle (one source of truth) - NOT two checkboxes.
        # Self-curated species have no UniProt feature classes: disable the
        # feature option and force Significance. Gated on the RESOLVED TYPE.
        if (isTRUE(is_self_curated_r())) {
          tagList(
            radioButtons(
              ns("pelsa_color_mode"), "Color points by:",
              choices = c("Significance (two-sided)" = "significance"),
              selected = "significance"
            ),
            helpText("UniProt feature-class coloring is unavailable for a ",
                     "self-curated database.")
          )
        } else {
          radioButtons(
            ns("pelsa_color_mode"), "Color points by:",
            choices = c("Significance (two-sided)" = "significance",
                        "UniProt feature class"     = "feature"),
            selected = "significance"
          )
        },
        hr(),
        strong("Label peptides:"),
        checkboxGroupInput(
          ns("pelsa_label_mode"), label = NULL,
          choices = c("All marker peptides"        = "all_markers",
                      "All significant peptides"   = "all_significant",
                      "Top N significant peptides" = "top_n_significant",
                      "Top N marker peptides"       = "top_n_markers"),
          selected = isolate(label_mode_for_ome())
        ),
        conditionalPanel(
          condition = sprintf(
            "input['%s'].indexOf('top_n_significant') > -1", ns("pelsa_label_mode")),
          numericInput(ns("pelsa_n_top_significant"), "N (significant, per up/down):",
                       value = isolate(n_top_significant_for_ome()),
                       min = 1, step = 1, width = "220px")
        ),
        conditionalPanel(
          condition = sprintf(
            "input['%s'].indexOf('top_n_markers') > -1", ns("pelsa_label_mode")),
          numericInput(ns("pelsa_n_top_markers"), "N (markers, per up/down):",
                       value = isolate(n_top_markers_for_ome()),
                       min = 1, step = 1, width = "220px")
        ),
        helpText("Applies to every contrast for this dataset."),
        hr(),
        # 7D best-peptide second panel toggle (lazy: the best-peptide df is built
        # only while this is ON; freed when toggled off).
        checkboxInput(ns("pelsa_show_best_panel"),
                      "Show best peptide per protein", value = FALSE),
        helpText("Marker-protein peptides are always drawn in magenta on top."),
        hr(),
        fluidRow(
          # LEFT column: the volcano point color key. Narrower than the feature
          # column - the key labels are short, so it cedes width to the right.
          column(5,
            tags$strong("Color key"),
            tags$ul(class = "pelsa-color-key",
              style = "list-style:none; padding-left:0; margin:0;",
              tags$li(tags$span(style = "color:#FF00FF;", "\u25cf"),
                      " marker protein"),
              tags$li(tags$span(style = sprintf("color:%s;", .PELSA_GOLD),
                                "\u25cf"), " highlighted"),
              tags$li(tags$span(style = "color:darkred;", "\u25cf"),
                      " significant up"),
              tags$li(tags$span(style = "color:#1f4e9c;", "\u25cf"),
                      " significant down"),
              tags$li(tags$span(style = "color:gray;", "\u25cf"),
                      " not significant")
            )
          ),
          # RIGHT column: the COMPLETE UniProt feature color reference - every
          # class in the palette, shown even when absent from this protein, so the
          # user has a full key to the Woods feature track. Wider than the color
          # key (7 vs 5) - its labels wrap ("low complexity / disorder"). For a
          # self-curated species there are no UniProt features, so the full color
          # key would map to nothing -- replace it with a short note instead.
          column(7,
            tags$strong("UniProt feature colors"),
            if (isTRUE(is_self_curated_r())) {
              tags$p(class = "text-muted",
                     "Feature annotations unavailable - self-curated database.")
            } else {
              .pelsa_feature_legend_ui()
            }
          )
        )
      )
    })

    ## ------------------------------------------------------------------------
    ## FIND / CLEAR + notice. Find resolves an accession to a single peptide
    ## selection (origin="find") when unambiguous, else lights up a find_result
    ## highlight (mask) over all matching peptides.
    ## ------------------------------------------------------------------------
    observeEvent(input$pelsa_find_go, {
      df <- tryCatch(active_volcano_df(), error = function(e) NULL)
      if (is.null(df)) return()
      fm <- pelsa_volcano_find_mask(df, input$pelsa_find_acc)
      find_query(input$pelsa_find_acc)
      if (fm$count == 0L) { find_result(fm); selection(NULL); return() }
      if (length(fm$accessions) == 1L) {
        rows <- which(fm$mask)
        best_j <- which.min(as.numeric(df$adj.P.Val[rows]))
        if (length(best_j) == 0L) best_j <- 1L  # all-NA adj.P -> first matched row
        best <- rows[best_j]
        selection(list(origin = "find",
                       accession = as.character(df$winning_accession[best]),
                       peptide_seq = as.character(df$id[best]),
                       label = as.character(df$label[best]), row = best))
        find_result(NULL)
      } else {
        selection(NULL)
        find_result(fm)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$pelsa_clear_sel, { clear_selection() }, ignoreInit = TRUE)

    output$pelsa_find_notice <- renderUI({
      fr <- find_result(); sel <- selection()
      if (!is.null(sel) && identical(sel$origin, "find")) {
        return(helpText(sprintf("Opened %s below.", sel$accession)))
      }
      if (is.null(fr)) return(NULL)
      if (fr$count == 0L)
        return(helpText(sprintf("No peptides found for '%s'.", find_query())))
      helpText(sprintf("%d proteins / %d peptides highlighted - type one accession to open it.",
                       length(fr$accessions), fr$count))
    })

    ## ------------------------------------------------------------------------
    ## 7C - THE WEBGL VOLCANO PLOT (shared assembly; click registered)
    ## ------------------------------------------------------------------------
    # The plot assembly is factored into pelsa_volcano_build_plot() so the
    # all-peptide AND best-peptide (7D) panels share ONE code path. The FULL df
    # is rendered (no thinning - toWebGL handles every point on the GPU).
    #
    # SELECTION HIGHLIGHT - gold OVERLAY trace (the fast path): the BASE volcano
    # is built ONCE and does NOT depend on selection()/find_result(), so a
    # click/find never rebuilds the ~100k-point cloud. The gold highlight is a
    # separate scattergl trace pushed/removed via plotlyProxyInvoke
    # addTraces/deleteTraces (see the two gold observers below). Adding a NEW
    # trace renders reliably on WebGL scattergl (a per-point marker.color restyle
    # silently fails there, so we do NOT use that).
    #
    # LABELS - baked into the build (same proven path as the best panel + the
    # static export). The render depends on label_mode_for_ome(), so a
    # Label-peptides change rebuilds the cloud WITH the labels. (A
    # relayout-proxy fast-path was tried to avoid this rebuild but did not
    # deliver annotations reliably on the WebGL plot; baking is the robust
    # path.) Pan / zoom / selection / find do NOT change the label mode, so
    # they still never rebuild - the gold highlight stays a proxy overlay.
    output$pelsa_volcano_plot <- plotly::renderPlotly({
      df <- plot_df()
      validate(need(nrow(df) > 0L, "No peptides to plot for this contrast."))
      pelsa_volcano_build_plot(
        df = df, full_df = df,
        color_mode = input$pelsa_color_mode %||% "significance",
        label_mode = label_mode_for_ome(),
        n_top_significant = n_top_significant_for_ome(),
        n_top_markers = n_top_markers_for_ome(),
        source_id = ns("pelsa_volcano"),
        selection = NULL, find_mask = NULL,
        register_click = TRUE,
        use_webgl = use_webgl())
    })

    ## ------------------------------------------------------------------------
    ## GOLD HIGHLIGHT OVERLAY (proxy addTraces/deleteTraces - no rebuild)
    ## ------------------------------------------------------------------------
    # The base figure has exactly TWO point traces: index 0 = background
    # (meta "pelsa_bg"), index 1 = markers (meta "pelsa_mk"). The overlay set is
    # pushed on top: the gold highlight at index 2 and, when a peptide is clicked,
    # its larger emphasized clicked-point dot at index 3. overlay_n tracks how
    # many overlay traces currently exist on the client so we never delete a
    # trace that is not there.
    overlay_n  <- reactiveVal(0L)  # how many overlay traces (gold, click) on client
    gold_proxy   <- plotly::plotlyProxy("pelsa_volcano_plot", session)

    # Re-apply the overlay set for the CURRENT selection/find: remove the prior
    # overlay traces (if any) then add the fresh ones. The base build is untouched.
    # The base figure has exactly TWO point traces (bg=0, markers=1), so overlays
    # start at index 2: the gold highlight is index 2, and the emphasized
    # clicked-point dot (when present) rides on top at index 3.
    apply_gold_overlay <- function() {
      # isolate(): this runs both inside reactive observers AND from
      # session$onFlushed() (after a base rebuild), where reading the
      # active_volcano_df() reactive directly would error out of context and
      # the tryCatch would silently drop the gold overlay. The triggering
      # observers already establish the needed dependencies.
      df <- tryCatch(isolate(active_volcano_df()), error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0L) return()
      # Overlay backend must match the base figure (scattergl vs scatter). Read
      # via isolate(): this also runs from session$onFlushed(), outside a
      # reactive context, where use_webgl() would otherwise error.
      uw <- isolate(use_webgl())
      # Delete existing overlays HIGHEST-index-first (click=3, gold=2) so the
      # remaining indices stay valid mid-delete. overlay_n() / find_result() /
      # selection() are read via isolate() for the same reason as above: this
      # also runs from session$onFlushed(), outside a reactive context, where a
      # bare reactiveVal/reactive read would otherwise error.
      n <- isolate(overlay_n())
      if (n >= 2L) plotly::plotlyProxyInvoke(gold_proxy, "deleteTraces", list(3L))
      if (n >= 1L) plotly::plotlyProxyInvoke(gold_proxy, "deleteTraces", list(2L))
      overlay_n(0L)

      sel <- isolate(selection())
      fr <- isolate(find_result())
      gold_tr <- pelsa_volcano_gold_trace(
        df, sel, if (is.null(fr)) NULL else fr$mask, use_webgl = uw)
      added <- 0L
      if (!is.null(gold_tr)) {
        plotly::plotlyProxyInvoke(gold_proxy, "addTraces", gold_tr)
        added <- added + 1L
        # Emphasize the clicked peptide (a larger gold dot with a thicker black
        # ring) on top of the gold markers, at index 3. Only meaningful when a
        # single peptide is selected (a click / single-accession Find).
        click_tr <- pelsa_volcano_clicked_point_trace(df, sel,
                                                       use_webgl = uw)
        if (!is.null(click_tr)) {
          plotly::plotlyProxyInvoke(gold_proxy, "addTraces", click_tr)
          added <- added + 1L
        }
      }
      overlay_n(added)
    }

    # (a) SELECTION/FIND observer. The base cloud is unchanged, so the OLD gold
    # trace is still on the client; apply_gold_overlay() deletes it (tracked) and
    # adds the new one. Covers click->click (B's gold replaces A's), click->clear
    # (selection() -> NULL -> gold removed, nothing added).
    observeEvent(list(selection(), find_result()), {
      apply_gold_overlay()
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # (b) BASE-REBUILD observer. Anything that re-renders the WHOLE figure clears
    # ALL extra traces on the client, so the old overlay traces are GONE - reset
    # overlay_n(0L) WITHOUT a delete (deleting the now-absent traces would
    # error / drop the markers), then re-add the current overlay set once the new
    # figure has flushed. Triggers = the canonical overlay-reset reasons (see
    # .pelsa_volcano_overlay_reset_reasons): display controls (color mode /
    # contrast / label mode - labels are baked into the build, not
    # relayout-applied) and the WebGL flip (a client WebGL->SVG flip rebuilds the
    # base), PLUS the volcano_df_cache clears (markers / sig cutoff / sig stat):
    # each clears the cache -> active_volcano_df() recomputes -> renderPlotly
    # rebuilds the base, wiping the overlay traces, so overlay_n MUST reset here
    # too (otherwise the next apply_gold_overlay() deletes absent trace indices
    # and drops the markers trace). The trigger list MUST cover every reason in
    # .pelsa_volcano_cache_clear_reasons() - enforced by test-pelsa-overlay-reset.
    observeEvent(
      list(input$pelsa_color_mode, active_contrast(),
           label_mode_for_ome(), n_top_significant_for_ome(),
           n_top_markers_for_ome(), use_webgl(),
           marker_accessions(), sig_cutoff_r(), sig_stat_r()),
      {
        session$onFlushed(function() {
          overlay_n(0L)   # the rebuild already cleared the overlay traces
          apply_gold_overlay()
        }, once = TRUE)
      }, ignoreNULL = FALSE, ignoreInit = TRUE)

    output$pelsa_marker_count <- renderText({
      df <- tryCatch(active_volcano_df(), error = function(e) NULL)
      if (is.null(df)) return("")
      n <- length(unique(df$winning_accession[df$is_marker %in% TRUE]))
      sprintf("%d marker protein(s) shown in magenta.", n)
    })

    ## ------------------------------------------------------------------------
    ## 7D - BEST-PEPTIDE PANEL PLOT (same shared assembly, own source id)
    ## ------------------------------------------------------------------------
    output$pelsa_volcano_best_plot <- plotly::renderPlotly({
      req(best_show())
      df <- best_plot_df()
      validate(need(nrow(df) > 0L,
                    "No best peptides to plot for this contrast."))
      pelsa_volcano_build_plot(
        df             = df,
        full_df        = df,
        color_mode     = input$pelsa_color_mode %||% "significance",
        label_mode     = label_mode_for_ome(),
        n_top_significant = n_top_significant_for_ome(),
        n_top_markers      = n_top_markers_for_ome(),
        source_id      = ns("pelsa_volcano_best"),
        register_click = FALSE,
        use_webgl      = use_webgl()
      )
    })

    ## ------------------------------------------------------------------------
    ## 7E - LEFT-CLICK SELECT: resolve clicked peptide -> selection() reactiveVal
    ## ------------------------------------------------------------------------
    # event_data() returns the clicked point's (x, y) == (logFC, logP); the pure
    # resolver maps that to the volcano-df peptide + its representative accession
    # (winning_accession). tryCatch so a bad click never crashes the session.
    observeEvent(
      suppressWarnings(
        plotly::event_data("plotly_click", source = ns("pelsa_volcano"))), {
      ev <- suppressWarnings(
        plotly::event_data("plotly_click", source = ns("pelsa_volcano")))
      res <- tryCatch(pelsa_volcano_resolve_click(ev, active_volcano_df()),
                      error = function(e) NULL)
      find_result(NULL)            # a click replaces any find highlight
      if (is.null(res)) { selection(NULL); return() }
      selection(c(res, list(origin = "click")))  # new list, no in-place mutation
    }, ignoreInit = TRUE)

    # SELECTION/FIND HIGHLIGHT - applied as a gold OVERLAY trace (no rebuild).
    # Setting selection()/find_result() fires the gold observer (a) above, which
    # pushes/replaces the gold scattergl trace via plotlyProxyInvoke addTraces/
    # deleteTraces. The base figure is never rebuilt on click/find/clear. Adding
    # a new trace renders reliably on WebGL scattergl (a per-point marker.color
    # restyle silently fails there, so it is NOT used).

    ## ------------------------------------------------------------------------
    ## 7E - PINNED metadata table + per-protein intensity LINE plot (3C)
    ## ------------------------------------------------------------------------
    # The 3C line data is computed ONLY here (on pin) - the hover path never
    # reaches it. tryCatch around the whole render so a bad click is inert.
    pinned_line_data <- reactive({
      pin <- selection()
      req(pin, pin$accession, nzchar(pin$accession))
      entry <- cache_entry()
      req(entry)
      contrast <- active_contrast(); req(contrast)
      pm <- processed_mat_r(); req(pm)
      cmap <- condition_map_r(); req(cmap)
      corder <- condition_order_r()
      req(length(corder) > 0L)
      matched <- entry$matched %||% data.frame()
      req(nrow(matched) > 0L)
      stat_df <- pelsa_volcano_stat_df(stat_df_raw(), matched)

      acc <- pin$accession
      is_mk <- acc %in% isolate(marker_accessions())
      tryCatch(
        pelsa_intensity_line_data(
          accession = acc, stat_df = stat_df, matched_cache = matched,
          processed_mat = pm, condition_map = cmap, condition_order = corder,
          contrast = contrast, sig_cutoff = sig_cutoff_r(), is_marker = is_mk,
          show_all = TRUE,   # pinned panel shows ALL peptides of the protein
          sig_stat = sig_stat_r()),
        error = function(e) NULL)
    })

    output$pelsa_pin_metadata <- renderUI({
      sel <- selection()
      if (is.null(sel)) return(helpText("No peptide selected yet."))
      df <- tryCatch(active_volcano_df(), error = function(e) NULL)
      if (is.null(df)) return(helpText("No peptide selected yet."))
      row <- sel$row
      if (is.null(row) || is.na(row)) {
        row <- match(sel$peptide_seq, as.character(df$id))
      }
      if (is.na(row)) return(helpText("No peptide selected yet."))
      w  <- tryCatch(pinned_woods(), error = function(e) NULL)
      n_pep <- if (!is.null(w) && is.data.frame(w$pep))
        length(unique(w$pep$peptide_seq)) else NA_integer_
      cov_frac <- if (!is.null(w)) w$coverage_frac %||% NA_real_ else NA_real_
      rows <- pelsa_pin_metadata_rows(df, row, n_pep, coverage_frac = cov_frac)
      tags$table(class = "table table-condensed",
        tags$tbody(lapply(seq_len(nrow(rows)), function(i)
          tags$tr(tags$td(tags$strong(rows$label[i])), tags$td(rows$value[i])))))
    })

    ## ------------------------------------------------------------------------
    ## ADD-TO-MARKER-LIST button (under the "Pinned Peptide" title). Resolves the
    ## pinned accession + gene, pushes them onto the shared marker_add_request
    ## handle so Setup (Section 1) merges them. Removal stays in Setup. The button
    ## disables + relabels "Already a marker" when the accession is already listed.
    ## ------------------------------------------------------------------------
    # The pinned (accession, gene) as a 1-row marker frame, or NULL when nothing
    # is pinned / unresolvable. Gene falls back winning_gene -> PG.Genes -> "".
    pinned_marker_row <- reactive({
      sel <- selection()
      if (is.null(sel) || is.null(sel$accession) || is.na(sel$accession) ||
          !nzchar(sel$accession)) {
        return(NULL)
      }
      df <- tryCatch(active_volcano_df(), error = function(e) NULL)
      gene <- ""
      if (!is.null(df)) {
        row <- sel$row
        if (is.null(row) || is.na(row)) row <- match(sel$peptide_seq,
                                                     as.character(df$id))
        if (!is.na(row)) {
          r <- df[row, , drop = FALSE]
          g <- if (!is.na(r$winning_gene) && nzchar(r$winning_gene))
            r$winning_gene else as.character(r$PG.Genes)[1L]
          if (!is.na(g) && nzchar(g)) gene <- g
        }
      }
      data.frame(accession = as.character(sel$accession), gene = gene,
                 stringsAsFactors = FALSE)
    })

    output$pelsa_add_marker_ui <- renderUI({
      mr <- pinned_marker_row()
      if (is.null(mr)) return(NULL)  # nothing pinned -> no button
      already <- mr$accession %in% isolate(marker_accessions())
      if (already) {
        tags$div(style = "margin-bottom:8px;",
          shiny::actionButton(ns("pelsa_add_marker"),
            label = "Already a marker", icon = shiny::icon("check"),
            class = "btn-sm", disabled = "disabled"))
      } else {
        tags$div(style = "margin-bottom:8px;",
          shiny::actionButton(ns("pelsa_add_marker"),
            label = "Add accession to marker list", icon = shiny::icon("plus"),
            class = "btn-sm btn-primary"))
      }
    })

    observeEvent(input$pelsa_add_marker, {
      mr <- pinned_marker_row()
      if (is.null(mr)) return()
      acc <- mr$accession
      if (acc %in% isolate(marker_accessions())) {
        showNotification(sprintf("%s is already a marker.", acc),
                         type = "message", duration = 3)
        return()
      }
      # Markers are PER-OME: tag the request with THIS dataset's ome so Setup
      # routes it to the right per-dataset marker list.
      if (is.function(marker_add_request)) {
        marker_add_request(list(ome = ome, rows = mr))
        showNotification(sprintf("Added %s to the marker list.", acc),
                         type = "message", duration = 3)
      }
    })

    output$pelsa_intensity_plot <- plotly::renderPlotly({
      ld <- tryCatch(pinned_line_data(), error = function(e) NULL)
      validate(need(!is.null(ld) && nrow(ld) > 0L,
                    "Click a point to pin its peptide profile."))
      # Highlight the clicked peptide's line/legend in gold: resolve its aa_label
      # from the pinned peptide sequence (the line data carries peptide_seq +
      # aa_label). NULL when the pinned peptide is not among the plotted lines.
      pin <- selection()
      pinned_lab <- NULL
      if (!is.null(pin) && !is.null(pin$peptide_seq)) {
        hit <- ld$aa_label[ld$peptide_seq == pin$peptide_seq]
        if (length(hit) > 0L) pinned_lab <- as.character(hit[[1]])
      }
      pelsa_intensity_line_plot(ld, pinned_label = pinned_lab)
    })

    ## ------------------------------------------------------------------------
    ## 7G - PINNED protein COVERAGE + FEATURE + WOODS panel (the L's bottom arm)
    ## ------------------------------------------------------------------------
    # Built ONLY on pin, off the same cache + stats the intensity plot uses. The
    # protein length comes from the cache's coverage frame (no FASTA re-read);
    # peptide spans + logFC/adj.P from matched + stat_df; UniProt features from
    # feat_df. Woods peptides carry a .tip listing overlapping annotation regions.
    pinned_woods <- reactive({
      pin <- selection()
      req(pin, pin$accession, nzchar(pin$accession))
      entry <- cache_entry(); req(entry)
      contrast <- active_contrast(); req(contrast)
      matched <- entry$matched %||% data.frame()
      req(nrow(matched) > 0L)
      acc <- pin$accession

      stat_df <- pelsa_volcano_stat_df(stat_df_raw(), matched)
      pep <- pelsa_woods_peptide_data(acc, matched, stat_df, contrast,
                                      sig_cutoff = sig_cutoff_r(),
                                      sig_stat = sig_stat_r())

      # Protein length: prefer the cache coverage frame; fall back to the max
      # mapped residue so the axis still spans the peptides. cov_frac is the
      # validated fractional coverage (NA unless FASTA length resolved) - surfaced
      # for the metadata panel's "Sequence coverage" row.
      cov <- entry$coverage %||% data.frame()
      plen <- NA_integer_
      cov_frac <- NA_real_
      if (is.data.frame(cov) && all(c("accession", "protein_length") %in%
                                     colnames(cov))) {
        idx <- which(as.character(cov$accession) == acc)
        if (length(idx) > 0L) {
          plen <- as.integer(cov$protein_length[idx[1L]])
          if ("coverage" %in% colnames(cov))
            cov_frac <- as.numeric(cov$coverage[idx[1L]])
        }
      }
      if (is.na(plen) || plen < 1L) {
        plen <- if (nrow(pep) > 0L) max(pep$pep_end, na.rm = TRUE) else 1L
      }

      # Per-accession UniProt features (raw rows) -> lane-packed.
      fdf <- feat_df() %||% data.frame()
      feats <- if (is.data.frame(fdf) && "accession" %in% colnames(fdf)) {
        fdf[as.character(fdf$accession) == acc, , drop = FALSE]
      } else {
        fdf[0, , drop = FALSE]
      }
      lanes <- pelsa_feature_lanes(feats)

      # Woods tooltip: append the overlapping annotation regions per peptide.
      if (nrow(pep) > 0L) {
        ann <- pelsa_woods_overlap_annotations(pep$pep_start, pep$pep_end, feats)
        ann_line <- ifelse(nzchar(ann), paste0("\nAnnotations: ", ann), "")
        pep$.tip <- sprintf(
          "%s\naa %d-%d (len %d)\nlogFC: %.2f\nadj.P: %.2g%s",
          pep$peptide_seq, pep$pep_start, pep$pep_end,
          pep$pep_end - pep$pep_start + 1L, pep$logFC, pep$adj.P.Val, ann_line)
      }

      # Per-feature overlapping peptides (for the feature-lane hover).
      if (is.data.frame(lanes) && nrow(lanes) > 0L && nrow(pep) > 0L) {
        lanes$.overlap_peps <- pelsa_feature_overlap_peptides(
          lanes$start, lanes$end, pep$pep_start, pep$pep_end)
      }

      list(pep = pep, lanes = lanes,
           intervals = pelsa_coverage_intervals(pep$pep_start, pep$pep_end),
           prot_len = plen, coverage_frac = cov_frac)
    })

    output$pelsa_woods_panel <- plotly::renderPlotly({
      w <- tryCatch(pinned_woods(), error = function(e) NULL)
      validate(need(!is.null(w),
                    "Click a point to pin its protein's coverage & Woods plot."))
      suppressWarnings(pelsa_woods_panel(
        peptides = w$pep, features_lanes = w$lanes, intervals = w$intervals,
        prot_len = w$prot_len, source_id = ns("pelsa_woods"),
        sig_stat = sig_stat_r()))
    })

    # CROSS-PLOT HIGHLIGHT: click a Woods peptide -> resolve it to a peptide and
    # set selection(origin="click"). Setting selection() fires the gold-overlay
    # observer (which pushes the gold trace via the proxy) - NO volcano rebuild.
    # The clicked segment is resolved by coordinate (x in [pep_start, pep_end],
    # y ~ logFC).
    observeEvent(
      suppressWarnings(
        plotly::event_data("plotly_click", source = ns("pelsa_woods"))), {
      ev <- suppressWarnings(
        plotly::event_data("plotly_click", source = ns("pelsa_woods")))
      w  <- tryCatch(pinned_woods(), error = function(e) NULL)
      if (is.null(ev) || is.null(w) || nrow(w$pep) == 0L) return()
      pep <- w$pep
      j <- .pelsa_woods_click_index(pep, ev$x, ev$y)
      if (is.null(j)) return()
      sel_seq <- pep$peptide_seq[[j]]
      cur <- selection()
      selection(list(origin = "click",
                     accession = if (is.null(cur)) NA_character_ else cur$accession,
                     peptide_seq = sel_seq, label = sel_seq, row = NA_integer_))
    }, ignoreInit = TRUE)

    ## ------------------------------------------------------------------------
    ## 7F - EXPORTS (per-ome export list; re-derive from cache + stat_results)
    ## ------------------------------------------------------------------------
    # Each export_fn writes ONE file into dir_name, recomputing from the cache +
    # the Statistics-tab results (NOT the on-screen objects). The all-peptide
    # volcano PDF reuses the shared plot builder.
    # Common tryCatch wrapper: log the failure (with the ome + a label) and
    # no-op so one bad export never aborts the whole zip.
    safe_export <- function(label, body) function(dir_name) tryCatch(
      body(dir_name),
      error = function(e) {
        message("PELSA ", label, " export failed for ", ome, ": ",
                conditionMessage(e))
        invisible(NULL)
      })

    # ---- 03_volcano/01_volcano : one volcano per contrast (all + best) -------
    # Coloring follows input$pelsa_color_mode; labels follow the ome-level
    # label mode (baked into the static ggplot); markers magenta; NO gold.
    export_volcano <- safe_export("volcano figures", function(dir_name) {
      out <- pelsa_export_stage_dir(dir_name, .PELSA_STAGE_VOLCANO,
                                    .PELSA_SUB_VOLCANO)
      sr <- stat_results()[[ome]]
      entry <- cache_entry()
      matched <- if (is.null(entry)) NULL else entry$matched
      fdf <- feat_df()
      markers <- isolate(marker_accessions())
      color_mode <- isolate(input$pelsa_color_mode) %||% "significance"
      # Label mode is ome-scoped: read the ONE stored selection for this ome
      # once, reused for every contrast in the loop below (previously this was
      # a per-contrast registry lookup inside the loop, back when label mode
      # varied by contrast).
      lab_mode <- isolate(label_mode_for_ome())
      n_top_sig <- isolate(n_top_significant_for_ome())
      n_top_mk  <- isolate(n_top_markers_for_ome())
      want_best <- isTRUE(isolate(best_show()))
      # Single significance threshold for the whole export: drives the df build
      # (Significant / sig_direction / dashed y_cutoff) AND the annotation text,
      # so the dashed-line label always matches the cutoff in force. Sourced from
      # the SAME user-set cutoff as the in-app volcano (Statistics > Summary), so
      # the export mirrors exactly what the user sees on screen.
      sig_cutoff <- isolate(sig_cutoff_r())
      sig_stat <- isolate(sig_stat_r())
      self_curated <- isolate(is_self_curated_r())
      choices <- contrast_choices()
      # Precompute the contrast-invariant stat frame ONCE (its inputs sr/matched
      # are fixed for this export). Guarded so a malformed/empty sr falls back to
      # the per-call path inside pelsa_volcano_export_df (which safe_export's
      # outer tryCatch still protects) rather than throwing before the loop.
      stat_df_once <- if (is.data.frame(sr) && nrow(sr) > 0L) {
        tryCatch(pelsa_volcano_stat_df(sr, matched %||% data.frame()),
                 error = function(e) NULL)
      } else {
        NULL
      }
      for (i in seq_along(choices)) {
        contrast <- unname(choices[[i]])
        df_all <- pelsa_volcano_export_df(sr, matched, fdf, markers, contrast,
                                          "all_peptide", sig_cutoff = sig_cutoff,
                                          is_self_curated = self_curated,
                                          sig_stat = sig_stat,
                                          .stat_df = stat_df_once)
        if (!is.null(df_all) && nrow(df_all) > 0L) {
          pelsa_save_figure(
            .pelsa_export_ggplot(df_all, df_all, color_mode, lab_mode,
                                 n_top_significant = n_top_sig,
                                 n_top_markers = n_top_mk,
                                 contrast = contrast,
                                 volcano_label = "All-peptide volcano",
                                 sig_cutoff = sig_cutoff),
            out, paste0("all_peptide_volcano_", pelsa_safe_name(contrast)),
            width = 6, height = 4.5)
        }
        if (want_best) {
          df_best <- pelsa_volcano_export_df(sr, matched, fdf, markers, contrast,
                                             "best_peptide",
                                             sig_cutoff = sig_cutoff,
                                             is_self_curated = self_curated,
                                             sig_stat = sig_stat,
                                             .stat_df = stat_df_once)
          if (!is.null(df_best) && nrow(df_best) > 0L) {
            pelsa_save_figure(
              .pelsa_export_ggplot(df_best, df_best, color_mode, lab_mode,
                                   n_top_significant = n_top_sig,
                                   n_top_markers = n_top_mk,
                                   contrast = contrast,
                                   volcano_label = "Best-peptide volcano",
                                   sig_cutoff = sig_cutoff),
              out, paste0("best_peptide_volcano_", pelsa_safe_name(contrast)),
              width = 6, height = 4.5)
          }
        }
      }
      invisible(out)
    })

    # ---- 03_volcano/02_intensity_line : per protein (marker | significant) ---
    # Contrast-independent: one figure per protein. The significant set + the
    # panel split use the union-across-contrasts adj.P (synthetic min column).
    export_intensity <- safe_export("intensity figures", function(dir_name) {
      entry <- cache_entry(); if (is.null(entry)) return(invisible(NULL))
      matched <- entry$matched %||% data.frame()
      if (nrow(matched) == 0L) return(invisible(NULL))
      pm <- isolate(processed_mat_r()); cmap <- isolate(condition_map_r())
      corder <- isolate(condition_order_r())
      if (is.null(pm) || is.null(cmap) || length(corder) == 0L)
        return(invisible(NULL))
      # Use the SAME user-set cutoff as the on-screen intensity panel so the
      # exported Significant/Non-significant split matches what the user sees.
      sig_cutoff <- isolate(sig_cutoff_r())
      sig_stat <- isolate(sig_stat_r())
      stat_df <- pelsa_export_add_any_contrast(
        pelsa_volcano_stat_df(stat_results()[[ome]], matched))
      markers <- isolate(marker_accessions())
      prot <- tryCatch(
        pelsa_intensity_proteins(stat_df, matched, markers, .PELSA_ANY_CONTRAST,
                                 sig_cutoff, sig_stat = sig_stat),
        error = function(e) NULL)
      if (is.null(prot) || nrow(prot) == 0L) return(invisible(NULL))
      d_mk <- pelsa_export_stage_dir(dir_name, .PELSA_STAGE_VOLCANO,
                                     .PELSA_SUB_INTENSITY, .PELSA_GRP_MARKER)
      d_sg <- pelsa_export_stage_dir(dir_name, .PELSA_STAGE_VOLCANO,
                                     .PELSA_SUB_INTENSITY, .PELSA_GRP_SIGNIF)
      # y-axis label log base reflects this dataset's declared transform so a
      # log10 dataset is not mislabeled "log2(intensity)".
      log_xf <- isolate(parameters())$log_transformation %||% NA_character_
      log_base <- if (identical(tolower(as.character(log_xf)), "log10")) 10L else 2L
      cov <- entry$coverage %||% data.frame()
      cov_lookup <- function(acc) {
        if (is.data.frame(cov) &&
            all(c("accession", "coverage") %in% colnames(cov))) {
          idx <- which(as.character(cov$accession) == acc)
          if (length(idx) > 0L) return(as.numeric(cov$coverage[idx[1L]]))
        }
        NA_real_
      }
      intensity_idx <- pelsa_intensity_build_index(matched)
      for (i in seq_len(nrow(prot))) {
        acc <- prot$accession[i]; is_mk <- isTRUE(prot$is_marker[i])
        ld <- tryCatch(
          pelsa_intensity_line_data(acc, stat_df, matched, pm, cmap, corder,
            .PELSA_ANY_CONTRAST, sig_cutoff, is_marker = is_mk,
            show_all = TRUE, sig_stat = sig_stat, .index = intensity_idx),
          error = function(e) NULL)
        if (is.null(ld) || nrow(ld) == 0L) next
        gene <- pelsa_export_gene_for(matched, acc)
        p <- tryCatch(
          pelsa_intensity_export_ggplot(ld, gene, acc, log_base,
                                        coverage_frac = cov_lookup(acc)),
          error = function(e) NULL)
        if (is.null(p)) next
        base <- paste0("intensityLine_", pelsa_safe_name(gene), "_",
                       pelsa_safe_name(acc))
        pelsa_save_figure(p, if (is_mk) d_mk else d_sg, base,
                          width = 9, height = 5)
      }
      invisible(NULL)
    })

    # ---- 03_volcano/03_woods : per (protein x contrast), marker | significant -
    export_woods <- safe_export("woods figures", function(dir_name) {
      entry <- cache_entry(); if (is.null(entry)) return(invisible(NULL))
      matched <- entry$matched %||% data.frame()
      if (nrow(matched) == 0L) return(invisible(NULL))
      # Use the SAME user-set cutoff as the on-screen Woods panel.
      sig_cutoff <- isolate(sig_cutoff_r())
      sig_stat <- isolate(sig_stat_r())
      stat_df <- pelsa_volcano_stat_df(stat_results()[[ome]], matched)
      stat_any <- pelsa_export_add_any_contrast(stat_df)
      markers <- isolate(marker_accessions())
      prot <- tryCatch(
        pelsa_intensity_proteins(stat_any, matched, markers, .PELSA_ANY_CONTRAST,
                                 sig_cutoff, sig_stat = sig_stat),
        error = function(e) NULL)
      if (is.null(prot) || nrow(prot) == 0L) return(invisible(NULL))
      fdf <- feat_df() %||% data.frame()
      fdf_by_acc <- if (is.data.frame(fdf) && nrow(fdf) > 0L &&
                        "accession" %in% colnames(fdf)) {
        facc <- as.character(fdf$accession)
        fvalid <- !is.na(facc) & nzchar(facc)
        if (any(fvalid)) split(fdf[fvalid, , drop = FALSE], facc[fvalid]) else list()
      } else {
        list()
      }
      cov <- entry$coverage %||% data.frame()
      choices <- contrast_choices()
      woods_idx <- pelsa_woods_build_index(matched, stat_df)
      d_mk <- pelsa_export_stage_dir(dir_name, .PELSA_STAGE_VOLCANO,
                                     .PELSA_SUB_WOODS, .PELSA_GRP_MARKER)
      d_sg <- pelsa_export_stage_dir(dir_name, .PELSA_STAGE_VOLCANO,
                                     .PELSA_SUB_WOODS, .PELSA_GRP_SIGNIF)
      for (i in seq_len(nrow(prot))) {
        acc <- prot$accession[i]; is_mk <- isTRUE(prot$is_marker[i])
        feats <- fdf_by_acc[[acc]] %||% fdf[0, , drop = FALSE]
        gene <- pelsa_export_gene_for(matched, acc)
        target <- if (is_mk) d_mk else d_sg
        for (cj in seq_along(choices)) {
          contrast <- unname(choices[[cj]])
          pep <- tryCatch(
            pelsa_woods_peptide_data(acc, matched, stat_df, contrast,
                                     sig_cutoff, sig_stat = sig_stat,
                                     .index = woods_idx),
            error = function(e) NULL)
          if (is.null(pep) || nrow(pep) == 0L) next
          plen <- pelsa_export_prot_len(cov, acc, pep)
          p <- tryCatch(
            pelsa_woods_export_ggplot(pep, feats, plen, gene, acc, contrast,
                                      sig_cutoff, sig_stat = sig_stat),
            error = function(e) NULL)
          if (is.null(p)) next
          base <- paste0("woods_", pelsa_safe_name(gene), "_",
                         pelsa_safe_name(acc), "_contrast_",
                         pelsa_safe_name(contrast))
          pelsa_save_figure(p, target, base, width = 9, height = 4.2)
        }
      }
      invisible(NULL)
    })

    return(list(
      volcano   = export_volcano,
      intensity = export_intensity,
      woods     = export_woods
    ))
  })
}
