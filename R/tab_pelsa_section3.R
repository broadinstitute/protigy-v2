################################################################################
# Module: PELSA - Section 3 (Volcano Plot)
#
# Renders the PELSA volcano for the ACTIVE dataset. The PELSA volcano does NOT
# compute differential statistics (Decision A): it CONSUMES the Statistics tab's
# stat_results()/stat_params() (logFC.<c> / adj.P.Val.<c> / P.Value.<c> columns)
# and the 5D analysis cache (pelsa_analysis) + setup_state (markers + species).
#
# Layers (this pass, 7A-7C):
#   7A  stat-source gate — grey out until a stat analysis is run for the active
#       dataset (mirrors tab_stat_plot.R's validate(need(stat_results(), ...))).
#   7B  per-contrast registries (poi/top_n/label_mode), reused VERBATIM from
#       tab_stat_plot.R, seeded with the Setup marker accessions; lazy contrast
#       loading — only the ACTIVE contrast's heavy volcano df is held, the prior
#       contrast's df is freed on switch (registries persist user settings).
#   7C  the WebGL volcano: 3A pelsa_build_volcano_df() (cached per contrast) ->
#       ggplot -> ggplotly + plotly::toWebGL, with a single color toggle
#       (two-sided significance vs UniProt feature class), a magenta marker
#       overlay always on top, label modes (all markers / best per marker /
#       top-N=3), the empirical y-cutoff threshold line, and a metadata hover
#       tooltip. The FULL df is rendered (every point) — toWebGL handles 100k+
#       points on the GPU, so NO background downsampling is applied (per user
#       decision: draw all points). The 3B pelsa_thin_background() helper remains
#       in the package but is intentionally NOT wired into the volcano render.
#
# Pass 2 (built): 7D best-peptide second panel (lazy, panel="best_peptide");
# 7E left-click PIN -> per-protein intensity line panel (3C) + a client-side
# plotlyProxy-restyle sibling-protein FADE (the main volcano does NOT rebuild on
# pin — only the background trace's marker.opacity is restyled); 7F per-ome
# exports.
#
# Pure plot-assembly / shaping logic: R/tab_pelsa_section3_helpers.R (tested).
################################################################################

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
                                     pelsa_setup_state = NULL) {

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
    top_n_registry      <- reactiveVal(list())  # <key> -> integer top-N
    label_mode_registry <- reactiveVal(list())  # <key> -> character() label mode

    # Per-ome (per-dataset) server: instantiate once per ome and reuse.
    all_exports <- reactiveVal()
    observeEvent(all_omes(), {
      ome_exports <- sapply(all_omes(), function(ome) {
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
          top_n_registry            = top_n_registry,
          label_mode_registry       = label_mode_registry
        )
      }, simplify = FALSE)
      all_exports(ome_exports)
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
                                     top_n_registry = NULL,
                                     label_mode_registry = NULL) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## ------------------------------------------------------------------------
    ## 7A — STAT-SOURCE GATE
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
      rows <- if (is.null(ss)) NULL else ss$marker_rows
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

    # Narrow seam: feat_df reads ONLY the species off setup_state. Depending on
    # the whole setup_state() would re-read the 26MB feature TSV on ANY setup-
    # state change (condition columns, orders, ...); scoping to species_r() means
    # feat_df re-reads only when the species actually changes.
    species_r <- reactive({
      ss <- pelsa_setup_state()
      if (is.null(ss)) NULL else ss$species
    })

    # Species feature table (2I/3A feat_df), read once per species via the
    # on-disk cache. Read-only; NO network. NULL when unavailable (3A then
    # colors everything "none").
    feat_df <- reactive({
      species <- species_r()
      if (is.null(species) || length(species) != 1L || is.na(species) ||
          !nzchar(species)) {
        return(NULL)
      }
      species_dir <- file.path(pelsa_database_dir(), species)
      tryCatch(
        pelsa_read_feature_cache(species_dir),
        error = function(e) NULL
      )
    })

    ## ------------------------------------------------------------------------
    ## 7B — CONTRAST SELECTOR + PER-CONTRAST REGISTRIES + LAZY LOADING
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

    # Per-contrast label mode (default "top_n"); persisted across switches.
    label_mode_for_contrast <- reactive({
      key <- current_contrast_key()
      req(key)
      reg <- if (is.null(label_mode_registry)) list() else label_mode_registry()
      reg[[key]] %||% .PELSA_VOLCANO_DEFAULT_LABEL_MODE
    })
    set_label_mode <- function(mode) {
      key <- isolate(current_contrast_key())
      if (is.null(key) || is.null(label_mode_registry)) return()
      reg <- label_mode_registry()
      reg[[key]] <- as.character(mode)[1L]
      label_mode_registry(reg)
    }

    # Per-contrast top-N (default 3); persisted across switches.
    top_n_for_contrast <- reactive({
      key <- current_contrast_key()
      req(key)
      reg <- if (is.null(top_n_registry)) list() else top_n_registry()
      reg[[key]] %||% .PELSA_VOLCANO_DEFAULT_TOP_N
    })
    set_top_n <- function(n) {
      key <- isolate(current_contrast_key())
      if (is.null(key) || is.null(top_n_registry)) return()
      reg <- top_n_registry()
      reg[[key]] <- max(1L, as.integer(n)[1L])
      top_n_registry(reg)
    }

    # Persist label-mode / top-N edits into the shared registries.
    observeEvent(input$pelsa_label_mode, {
      set_label_mode(input$pelsa_label_mode)
    }, ignoreInit = TRUE)
    observeEvent(input$pelsa_top_n, {
      req(is.numeric(input$pelsa_top_n), !is.na(input$pelsa_top_n))
      set_top_n(input$pelsa_top_n)
    }, ignoreInit = TRUE)

    # Restore stored label-mode / top-N into the UI when the contrast changes.
    observeEvent(current_contrast_key(), {
      updateRadioButtons(session, "pelsa_label_mode",
                         selected = isolate(label_mode_for_contrast()))
      updateNumericInput(session, "pelsa_top_n",
                         value = isolate(top_n_for_contrast()))
    }, ignoreNULL = TRUE, ignoreInit = FALSE)

    ## --- LAZY per-active-contrast volcano df cache --------------------------
    # Holds ONLY the active contrast's heavy 3A df, keyed by contrast suffix.
    # On contrast switch the prior contrast's df is FREED (the list is replaced
    # with a single-entry list for the new active contrast). The registries —
    # not this cache — persist user-facing settings across switches.
    volcano_df_cache <- reactiveVal(list())

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
          opts          = list(panel = "all_peptide", sig_cutoff = 0.05)
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
    ## 7D — BEST-PEPTIDE SECOND PANEL (lazy: only when the checkbox is ON)
    ## ------------------------------------------------------------------------
    # Same stat_df/cache/feat_df/markers/contrast as the all-peptide df — only
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
          opts          = list(panel = "best_peptide", sig_cutoff = 0.05)
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

    # Best-peptide panel: also the FULL df (no thinning — toWebGL renders all).
    best_plot_df <- reactive(best_volcano_df())

    # Free the best-panel cache whenever the checkbox goes OFF.
    observeEvent(best_show(), {
      if (!best_show()) best_volcano_df_cache(list())
    }, ignoreInit = TRUE)

    ## ------------------------------------------------------------------------
    ## 7E — PINNED peptide selection (left-click). reactiveVal holds the resolved
    ## click; HOVER never touches this path (the heavy intensity plot is a
    ## SEPARATE output computed only when this reactiveVal is set).
    ## ------------------------------------------------------------------------
    pinned <- reactiveVal(NULL)  # list(peptide_seq, accession, label, row)

    # CLEAR the pin on contrast switch: a pin made under contrast A would
    # otherwise survive into contrast B, where its highlighted siblings /
    # metadata / intensity panel describe a peptide selected under the OLD
    # contrast's coordinates (misleading). The contrast key changes on switch.
    observeEvent(current_contrast_key(), {
      pinned(NULL)
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

      fluidRow(
        # 7E LEFT-side pinned intensity panel: a metadata table + the per-protein
        # 3C intensity LINE plot, populated on the left-click PIN of the volcano.
        # The line plot lives in its OWN plotlyOutput so it is computed ONLY on
        # click (the hover path never touches it).
        column(3,
          shinydashboardPlus::box(
            uiOutput(ns("pelsa_pin_metadata")),
            plotly::plotlyOutput(ns("pelsa_intensity_plot"), height = "320px"),
            helpText("Click a point to pin its peptide profile."),
            width = NULL, title = "Pinned Peptide", headerBorder = TRUE
          )
        ),
        column(6,
          tagList(
            shinydashboardPlus::box(
              plotly::plotlyOutput(ns("pelsa_volcano_plot"), height = "560px"),
              status = "primary", width = NULL, title = "PELSA Volcano",
              headerBorder = TRUE, solidHeader = TRUE
            ),
            # 7D best-peptide second panel, BELOW the all-peptide volcano, shown
            # only when the sidebar checkbox is ON.
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
        # SINGLE color toggle (one source of truth) — NOT two checkboxes.
        radioButtons(
          ns("pelsa_color_mode"), "Color points by:",
          choices = c("Significance (two-sided)" = "significance",
                      "UniProt feature class"     = "feature"),
          selected = "significance"
        ),
        hr(),
        strong("Label peptides:"),
        radioButtons(
          ns("pelsa_label_mode"), label = NULL,
          choices = c("All marker peptides"          = "all_markers",
                      "Best peptide per marker"       = "best_per_marker",
                      "Top-N per protein"             = "top_n"),
          selected = isolate(label_mode_for_contrast())
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'top_n'", ns("pelsa_label_mode")),
          numericInput(ns("pelsa_top_n"), "N (smallest adj.P.Val):",
                       value = isolate(top_n_for_contrast()),
                       min = 1, step = 1, width = "140px")
        ),
        hr(),
        # 7D best-peptide second panel toggle (lazy: the best-peptide df is built
        # only while this is ON; freed when toggled off).
        checkboxInput(ns("pelsa_show_best_panel"),
                      "Show best peptide per protein", value = FALSE),
        helpText("Marker-protein peptides are always drawn in magenta on top.")
      )
    })

    ## ------------------------------------------------------------------------
    ## 7C — THE WEBGL VOLCANO PLOT (shared assembly; click registered)
    ## ------------------------------------------------------------------------
    # The plot assembly is factored into pelsa_volcano_build_plot() so the
    # all-peptide AND best-peptide (7D) panels share ONE code path. The FULL df
    # is rendered (no thinning — toWebGL handles every point on the GPU).
    #
    # SIBLING FADE — ONE mechanism (a client-side plotlyProxy restyle, NOT a
    # rebuild): the base plot is built ONCE per (dataset, contrast, color-mode,
    # label-mode) with sibling_acc = NULL, so pinned() is ISOLATED out of this
    # render's reactive deps. A left-click pin then dims the background trace via
    # plotlyProxyInvoke("restyle", ...) (a small marker.opacity message), so the
    # ~100k-point / ~15MB figure is NOT re-serialized on every pin (~1.1-1.5s
    # saved per pin; the ggplotly conversion was the cost, not the GPU draw).
    # The proxy restyle is the PRIMARY fade mechanism (see the plotly_click
    # observeEvent below) — exactly ONE mechanism, not the old depends-on-pinned
    # redraw.
    output$pelsa_volcano_plot <- plotly::renderPlotly({
      df <- plot_df()
      validate(need(nrow(df) > 0L, "No peptides to plot for this contrast."))
      # sibling_acc = NULL on purpose: the fade is a proxy restyle, not a rebuild.
      pelsa_volcano_build_plot(
        df             = df,
        full_df        = df,
        color_mode     = input$pelsa_color_mode %||% "significance",
        label_mode     = label_mode_for_contrast(),
        n_top          = top_n_for_contrast(),
        source_id      = ns("pelsa_volcano"),
        sibling_acc    = NULL,
        register_click = TRUE
      )
    })

    ## ------------------------------------------------------------------------
    ## 7D — BEST-PEPTIDE PANEL PLOT (same shared assembly, own source id)
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
        label_mode     = label_mode_for_contrast(),
        n_top          = top_n_for_contrast(),
        source_id      = ns("pelsa_volcano_best"),
        sibling_acc    = NULL,
        register_click = FALSE
      )
    })

    ## ------------------------------------------------------------------------
    ## 7E — LEFT-CLICK PIN: resolve clicked peptide -> pinned() reactiveVal
    ## ------------------------------------------------------------------------
    # event_data() returns the clicked point's (x, y) == (logFC, logP); the pure
    # resolver maps that to the volcano-df peptide + its representative accession
    # (winning_accession). tryCatch so a bad click never crashes the session.
    observeEvent(plotly::event_data("plotly_click", source = ns("pelsa_volcano")), {
      ev <- plotly::event_data("plotly_click", source = ns("pelsa_volcano"))
      res <- tryCatch(
        pelsa_volcano_resolve_click(ev, active_volcano_df()),
        error = function(e) NULL
      )
      pinned(res)
    }, ignoreInit = TRUE)

    # SIBLING FADE via plotlyProxy restyle (the ONE mechanism). The base volcano
    # is built once (sibling_acc = NULL), so the main render does NOT rebuild on
    # pin. Here, whenever pinned() changes (pin / unpin / contrast-switch clear),
    # we restyle ONLY the background trace's per-point marker.opacity — full for
    # the pinned protein's peptides, dimmed for the rest (or the base default
    # everywhere when unpinned). This is a small message, not a ~15MB rebuild.
    #
    # Trace order in pelsa_volcano_build_plot is fixed: trace 0 = background
    # (every non-marker point, in marker-split background order), trace 1 =
    # markers, trace 2 = labels. We restyle ONLY trace 0 (JS index 0). The
    # opacity vector is computed over the SAME background split, so element j
    # targets background point j. pelsa_volcano_pin_opacity is ~0.001s.
    # NB: plotlyProxy targets the plotlyOutput's OUTPUT id ("pelsa_volcano_plot"),
    # not the plotly SOURCE id ("pelsa_volcano"). plotlyProxy auto-namespaces it
    # via session$ns, so pass the bare output id.
    volcano_proxy <- plotly::plotlyProxy("pelsa_volcano_plot", session)
    observeEvent(pinned(), {
      df <- tryCatch(active_volcano_df(), error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0L) return()
      pin <- pinned()
      acc <- if (is.null(pin)) NULL else pin$accession
      op <- pelsa_volcano_pin_opacity(df, acc)
      if (length(op$opacity) == 0L) return()
      # restyle: marker.opacity as a per-point array on the background trace (0).
      plotly::plotlyProxyInvoke(
        volcano_proxy, "restyle",
        list(`marker.opacity` = list(op$opacity)), list(0L)
      )
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    ## ------------------------------------------------------------------------
    ## 7E — PINNED metadata table + per-protein intensity LINE plot (3C)
    ## ------------------------------------------------------------------------
    # The 3C line data is computed ONLY here (on pin) — the hover path never
    # reaches it. tryCatch around the whole render so a bad click is inert.
    pinned_line_data <- reactive({
      pin <- pinned()
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
          contrast = contrast, sig_cutoff = 0.05, is_marker = is_mk),
        error = function(e) NULL)
    })

    output$pelsa_pin_metadata <- renderUI({
      pin <- pinned()
      if (is.null(pin)) {
        return(helpText("No peptide pinned yet."))
      }
      tags$table(class = "table table-condensed",
        tags$tbody(
          tags$tr(tags$td(strong("Peptide")), tags$td(pin$peptide_seq %||% "")),
          tags$tr(tags$td(strong("Protein")), tags$td(pin$accession %||% "")),
          tags$tr(tags$td(strong("Label")),   tags$td(pin$label %||% ""))
        )
      )
    })

    output$pelsa_intensity_plot <- plotly::renderPlotly({
      ld <- tryCatch(pinned_line_data(), error = function(e) NULL)
      validate(need(!is.null(ld) && nrow(ld) > 0L,
                    "Click a point to pin its peptide profile."))
      suppressWarnings(plotly::ggplotly(pelsa_intensity_line_ggplot(ld)))
    })

    ## ------------------------------------------------------------------------
    ## 7F — EXPORTS (per-ome export list; re-derive from cache + stat_results)
    ## ------------------------------------------------------------------------
    # Each export_fn writes ONE file into dir_name, recomputing from the cache +
    # the Statistics-tab results (NOT the on-screen objects). The all-peptide
    # volcano PDF reuses the shared plot builder.
    build_export_df <- function(panel) {
      entry <- cache_entry()
      pelsa_volcano_export_df(
        stat_results()[[ome]],
        if (is.null(entry)) NULL else entry$matched,
        feat_df(), isolate(marker_accessions()), active_contrast(), panel)
    }

    # Common tryCatch wrapper: log the failure (with the ome + a label) and
    # no-op so one bad export never aborts the whole zip.
    safe_export <- function(label, body) function(dir_name) tryCatch(
      body(dir_name),
      error = function(e) {
        message("PELSA ", label, " export failed for ", ome, ": ",
                conditionMessage(e))
        invisible(NULL)
      })

    export_volcano_plot <- safe_export("volcano PDF", function(dir_name) {
      df <- isolate(build_export_df("all_peptide"))
      if (is.null(df) || nrow(df) == 0L) return(invisible(NULL))
      path <- file.path(dir_name, paste0("pelsa_volcano_", ome, ".pdf"))
      # Static PDF: a plain re-derived ggplot via the grDevices pdf device — no
      # browser/network. FULL df plotted (no thinning — matches on-screen).
      grDevices::pdf(path, width = 9, height = 7)
      on.exit(grDevices::dev.off(), add = TRUE)
      print(.pelsa_export_ggplot(
        df, df, isolate(input$pelsa_color_mode) %||% "significance"))
      invisible(path)
    })

    export_proteins_of_interest <- safe_export("POI", function(dir_name) {
      key <- isolate(current_contrast_key())
      poi <- if (!is.null(poi_registry) && !is.null(key))
        isolate(poi_registry())[[key]] else NULL
      poi <- poi %||% isolate(marker_accessions())
      out <- data.frame(accession = as.character(poi %||% character(0)),
                        stringsAsFactors = FALSE)
      path <- file.path(dir_name,
                        paste0("pelsa_proteins_of_interest_", ome, ".csv"))
      utils::write.csv(out, path, row.names = FALSE)
      invisible(path)
    })

    export_volcano_labels <- safe_export("volcano-labels", function(dir_name) {
      df_all  <- isolate(build_export_df("all_peptide"))
      df_best <- if (isolate(best_show()))
        isolate(build_export_df("best_peptide")) else NULL
      parts <- list()
      if (!is.null(df_all))
        parts[[length(parts) + 1L]] <-
          pelsa_volcano_labels_sidecar(df_all, "all_peptide")
      if (!is.null(df_best))
        parts[[length(parts) + 1L]] <-
          pelsa_volcano_labels_sidecar(df_best, "best_peptide")
      if (length(parts) == 0L) return(invisible(NULL))
      path <- file.path(dir_name, paste0("pelsa_volcano_labels_", ome, ".csv"))
      utils::write.csv(do.call(rbind, parts), path, row.names = FALSE)
      invisible(path)
    })

    export_plotted_intensities <- safe_export("plotted-intensities",
                                              function(dir_name) {
      entry <- cache_entry()
      out <- pelsa_plotted_intensities_df(
        stat_results()[[ome]],
        if (is.null(entry)) data.frame() else entry$matched %||% data.frame(),
        isolate(marker_accessions()), active_contrast(),
        isolate(processed_mat_r()), isolate(condition_map_r()),
        isolate(condition_order_r()))
      if (is.null(out) || nrow(out) == 0L) return(invisible(NULL))
      path <- file.path(dir_name,
                        paste0("pelsa_plotted_intensities_", ome, ".csv"))
      utils::write.csv(out, path, row.names = FALSE)
      invisible(path)
    })

    return(list(
      volcano_plot         = export_volcano_plot,
      proteins_of_interest = export_proteins_of_interest,
      volcano_labels       = export_volcano_labels,
      plotted_intensities  = export_plotted_intensities
    ))
  })
}
