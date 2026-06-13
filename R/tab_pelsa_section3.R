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
#       3B pelsa_thin_background() -> ggplot -> ggplotly + plotly::toWebGL, with
#       a single color toggle (two-sided significance vs UniProt feature class),
#       a magenta marker overlay always on top, label modes (all markers / best
#       per marker / top-N=3), the empirical y-cutoff threshold line, and a
#       metadata hover tooltip.
#
# Pass 2 (7D best-peptide panel / 7E hover-pin intensity panel / 7F exports) is
# a SEPARATE later task — clean seams are left below (documented + TODO), but
# nothing for it is built here.
#
# Pure plot-assembly logic: R/tab_pelsa_section3_helpers.R (tested).
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

    # Species feature table (2I/3A feat_df), read once per species via the
    # on-disk cache. Read-only; NO network. NULL when unavailable (3A then
    # colors everything "none").
    feat_df <- reactive({
      ss <- pelsa_setup_state()
      species <- if (is.null(ss)) NULL else ss$species
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

    # The thinned frame the plot consumes + its honesty counts.
    thinned <- reactive({
      df <- active_volcano_df()
      pelsa_thin_background(df, keep_frac = 0.3, logfc_thresh = 0.5, seed = 1L)
    })

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
        # TODO (pass 2, 7E): a LEFT-side pinned intensity panel goes here, fed by
        # the left-click PIN on the plot (3C intensity builder) + a plotlyProxy
        # sibling-peptide fade. Documented seam only — not built this pass.
        column(8,
          tagList(
            shinydashboardPlus::box(
              plotly::plotlyOutput(ns("pelsa_volcano_plot"), height = "560px"),
              uiOutput(ns("pelsa_thin_note")),
              status = "primary", width = NULL, title = "PELSA Volcano",
              headerBorder = TRUE, solidHeader = TRUE
            )
          )
        ),
        column(4,
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
        # TODO (pass 2, 7D): a "Show best-peptide panel" toggle stub. The
        # best-peptide second panel reuses 3A's panel="best_peptide". Not built.
        checkboxInput(ns("pelsa_show_best_panel"),
                      "Show best-peptide panel (coming soon)", value = FALSE),
        helpText("Marker-protein peptides are always drawn in magenta on top.")
      )
    })

    output$pelsa_thin_note <- renderUI({
      note <- tryCatch(pelsa_volcano_thin_note(thinned()),
                       error = function(e) NULL)
      if (is.null(note)) return(NULL)
      helpText(note)
    })

    ## ------------------------------------------------------------------------
    ## 7C — THE WEBGL VOLCANO PLOT
    ## ------------------------------------------------------------------------
    output$pelsa_volcano_plot <- plotly::renderPlotly({
      th <- thinned()
      df <- th$df
      validate(need(nrow(df) > 0L, "No peptides to plot for this contrast."))

      color_mode <- input$pelsa_color_mode %||% "significance"
      label_mode <- label_mode_for_contrast()
      n_top      <- top_n_for_contrast()

      split <- pelsa_volcano_marker_split(df)
      bg     <- split$background
      mk     <- split$markers

      # Color for the background trace (the single toggle picks the column).
      bg_colors <- if (nrow(bg) > 0L)
        pelsa_volcano_color_column(bg, color_mode) else character(0)

      # Metadata-only hover tooltip (NO line plot — pass 2 adds the pinned
      # intensity panel). accession / gene / position span / peptide length.
      tip <- function(d) {
        if (nrow(d) == 0L) return(character(0))
        # Guard the pep-span-attach miss: a peptide with no matched_cache row
        # has NA pep_start/pep_end. Show "unknown" position + blank length
        # rather than a bare "NA-NA" / "NA".
        no_span <- is.na(d$pep_start) | is.na(d$pep_end)
        pos <- ifelse(no_span, "unknown",
                      paste0(d$pep_start, "-", d$pep_end))
        len_chr <- ifelse(no_span, "",
                          as.character(d$pep_end - d$pep_start + 1L))
        len_line <- ifelse(no_span, "", paste0("<br>Length: ", len_chr))
        paste0(
          "Accession: ", d$winning_accession %||% d$PG.ProteinAccessions, "<br>",
          "Gene: ", d$winning_gene %||% d$PG.Genes, "<br>",
          "Position: ", pos, len_line
        )
      }

      gg <- ggplot2::ggplot()
      if (nrow(bg) > 0L) {
        bg$.tip <- tip(bg)
        gg <- gg + ggplot2::geom_point(
          data = bg,
          ggplot2::aes(x = .data$logFC, y = .data$logP, text = .data$.tip),
          color = bg_colors, alpha = 0.6, size = 1
        )
      }
      # Marker overlay: magenta, black edge, ON TOP, ALWAYS.
      if (nrow(mk) > 0L) {
        mk$.tip <- tip(mk)
        gg <- gg + ggplot2::geom_point(
          data = mk,
          ggplot2::aes(x = .data$logFC, y = .data$logP, text = .data$.tip),
          fill = .PELSA_VOLCANO_MARKER_COLOR,
          color = .PELSA_VOLCANO_MARKER_EDGE,
          shape = 21, size = 2.4, stroke = 0.5
        )
      }

      # Threshold line: dashed horizontal at the empirical raw-p (y_cutoff attr).
      # NO line when nothing passes (y_cutoff == Inf).
      y_cut <- attr(active_volcano_df(), "y_cutoff")
      if (!is.null(y_cut) && is.finite(y_cut)) {
        gg <- gg + ggplot2::geom_hline(yintercept = y_cut, linetype = "dashed",
                                       color = "grey40")
      }

      # On-plot labels (label text fixed to the 3A <gene>_aa<pos> column).
      lab_idx <- tryCatch(
        pelsa_volcano_label_rows(df, mode = label_mode, n_top = n_top),
        error = function(e) integer(0)
      )
      if (length(lab_idx) > 0L) {
        lab_df <- df[lab_idx, , drop = FALSE]
        lab_df <- lab_df[!is.na(lab_df$label) & nzchar(lab_df$label), ,
                         drop = FALSE]
        if (nrow(lab_df) > 0L) {
          gg <- gg + ggplot2::geom_text(
            data = lab_df,
            ggplot2::aes(x = .data$logFC, y = .data$logP, label = .data$label),
            size = 2.6, vjust = -0.8, check_overlap = TRUE
          )
        }
      }

      gg <- gg + ggplot2::labs(x = "logFC", y = "-log10(P.Value)") +
        ggplot2::theme_bw()

      p <- plotly::ggplotly(gg, source = ns("pelsa_volcano"), tooltip = "text")
      # GPU render for 100k+ points. toWebGL emits cosmetic "scattergl has no
      # attribute 'hoveron'" notices when converting ggplot geom_point traces;
      # they are harmless (the scattergl trace ignores the dropped attribute).
      p <- suppressWarnings(plotly::toWebGL(p))
      plotly::event_register(p, "plotly_click")
      p
    })

    ## ------------------------------------------------------------------------
    ## PASS-2 SEAMS (documented placeholders — NOT built this pass)
    ## ------------------------------------------------------------------------
    # 7E: left-click PIN -> intensity panel (3C) + plotlyProxy sibling-peptide
    #     fade. The click source is already registered as ns("pelsa_volcano");
    #     a future observeEvent(event_data("plotly_click", source =
    #     ns("pelsa_volcano"))) resolves the clicked peptide and renders the
    #     pinned intensity panel in the left column placeholder above.
    # 7D: best-peptide second panel via pelsa_build_volcano_df(panel =
    #     "best_peptide"), gated by input$pelsa_show_best_panel.

    ## EXPORTS (7F is pass 2) ##
    return(list())
  })
}

# A canonical empty matched-cache frame (the columns 3A's all-peptide join
# reads), used when the active dataset has no matched rows so 3A still runs and
# yields an unlabeled (label = NA) frame rather than erroring. @noRd
pelsa_volcano_empty_matched <- function() {
  data.frame(
    PEP.StrippedSequence = character(0),
    accession            = character(0),
    gene                 = character(0),
    pep_start            = integer(0),
    pep_end              = integer(0),
    stringsAsFactors     = FALSE,
    check.names          = FALSE
  )
}
