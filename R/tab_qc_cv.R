################################################################################
# Module: QC_CV
#
# Compute and visualize coefficients of variation (CV) per group for each ome.
#
# Supports:
#  - Multi-column grouping (cross-product of selected cdesc columns)
#  - Live group-preview table showing sample counts per group
#  - Unfiltered violin plot
#  - Optional cutoff-based feature filtering with filtered violin and GCT export
#
# Exports (all under QCCV_exports/):
#  - cv_results_<ome>.csv               (unfiltered CV table)
#  - cv_violin_<ome>.pdf
#  - cv_results_filtered_<ome>_*.csv    (when filter enabled)
#  - cv_violin_filtered_<ome>_*.pdf     (when filter enabled)
#  - cv_filtered_<ome>_*.gct            (when filter enabled)
################################################################################

################################################################################
# Tab-level UI and Server
################################################################################

QCCV_Tab_UI <- function(id = "QCCVTab") {
  ns <- NS(id)
  tagList(
    fluidRow(uiOutput(ns("ome_tabset_box")))
  )
}

QCCV_Tab_Server <- function(id = "QCCVTab",
                             GCTs_and_params,
                             globals,
                             GCTs_original) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # GCTs to use for analysis/visualization
    GCTs <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$GCTs
    })

    # parameters used to process GCTs
    parameters <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$parameters
    })

    # named list of default annotation columns for each ome
    default_annotations <- reactive({
      req(parameters())
      sapply(parameters(), function(p) p$annotation_column, simplify = FALSE)
    })

    all_omes    <- reactive(names(GCTs()))
    default_ome <- reactive(globals$default_ome)
    custom_colors <- reactive(globals$colors)

    # Pre-normalization (log-transformed) GCTs -- the non-normalized CV source.
    original_GCTs <- reactive({
      validate(need(GCTs_original(), "Original GCTs not yet available"))
      GCTs_original()
    })

    ## Build per-ome tabset

    output$ome_tabset_box <- renderUI({
      req(all_omes(), default_ome())

      tabs <- lapply(all_omes(), function(ome) {
        tabPanel(
          title = ome,
          QCCV_Ome_UI(id = ns(ome), ome = ome)
        )
      })

      tab_set_panel <- do.call(
        tabsetPanel,
        c(tabs, list(id = ns("ome_tabs"), selected = isolate(default_ome())))
      )

      add_css_attributes(
        shinydashboardPlus::box(
          tab_set_panel,
          width = 12
        ),
        classes = c("box-no-header", "box-with-tabs")
      )
    })

    observe({
      updateTabsetPanel(inputId = "ome_tabs", selected = default_ome())
    })

    # Call per-ome server and collect export functions
    all_exports <- reactiveVal()
    observeEvent(all_omes(), {
      ome_exports <- sapply(all_omes(), function(ome) {
        QCCV_Ome_Server(
          id                       = ome,
          ome                      = ome,
          GCT_processed            = reactive(GCTs()[[ome]]),
          GCT_original             = reactive(original_GCTs()[[ome]]),
          parameters               = reactive(parameters()[[ome]]),
          default_annotation_column = reactive(default_annotations()[[ome]]),
          color_map                = reactive(custom_colors()[[ome]])
        )
      }, simplify = FALSE)
      all_exports(ome_exports)
    })

    return(all_exports)
  })
}


################################################################################
# Per-ome UI and Server
################################################################################

QCCV_Ome_UI <- function(id, ome) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("cv_contents"))
  )
}


QCCV_Ome_Server <- function(id,
                              ome,
                              GCT_processed,
                              GCT_original,
                              parameters,
                              default_annotation_column,
                              color_map) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # CV plots are only meaningful for intensity data.
    show_cv_plots <- reactive({
      req(parameters())
      intensity_data_param_is_yes(parameters()$intensity_data)
    })

    # Numeric log base detected from setup: 2 (log2), 10 (log10), or NA (None).
    # NA means the base is unknown (e.g. data log-transformed before upload), so
    # the user must enter it.
    detected_base <- reactive({
      req(parameters())
      qc_cv_detect_base(parameters()$log_transformation)
    })

    output$cv_contents <- renderUI({
      if (!isTRUE(show_cv_plots())) {
        return(
          fluidRow(
            shinydashboardPlus::box(
              h4("CV plots are unavailable for ratio data. Enable intensity data to view CV plots."),
              status       = "primary",
              width        = 12,
              title        = "Coefficient of Variation (CV)",
              headerBorder = TRUE,
              solidHeader  = TRUE
            )
          )
        )
      }

      # CV is undefined for a single sample (sd/mean needs replicates). Grey out
      # the whole panel for single-sample omes with the shared message.
      req(GCT_processed())
      cv_min_samples_msg <- min_samples_message(GCT_processed(), n = 2, analysis = "CV")
      if (!is.null(cv_min_samples_msg)) {
        return(
          fluidRow(
            shinydashboardPlus::box(
              h4(cv_min_samples_msg),
              status       = "primary",
              width        = 12,
              title        = "Coefficient of Variation (CV)",
              headerBorder = TRUE,
              solidHeader  = TRUE
            )
          )
        )
      }

      fluidRow(
        shinydashboardPlus::box(
          div(
            style = "background-color: #f8f9fa; border-left: 4px solid #007bff; padding: 12px; margin-bottom: 15px; border-radius: 0 4px 4px 0; color: #495057;",
            icon("info-circle", style = "color: #007bff; margin-right: 8px;"),
            strong("Note: ", style = "color: #495057;"),
            paste(
              "CV is computed on linear, non-normalized intensities by default.",
              "log2/log10 datasets are delinearized automatically; if your data",
              "was log-transformed before upload, enter its base in the settings",
              "panel. Use the toggle to compute CV on the normalized data instead."
            )
          ),
          uiOutput(ns("cv_plot_section")),
          status       = "primary",
          width        = 12,
          title        = "Coefficient of Variation (CV)",
          headerBorder = TRUE,
          solidHeader  = TRUE,
          sidebar = boxSidebar(
            uiOutput(ns("qc_cv_controls")),
            id         = ns("qc_cv_sidebar"),
            width      = 25,
            icon       = icon("gears", class = "fa-2xl"),
            background = "rgba(91, 98, 104, 0.9)"
          )
        )
      )
    })

    ## Controls UI -------------------------------------------------------------
    output$qc_cv_controls <- renderUI({
      req(show_cv_plots())
      req(GCT_processed())

      cdesc_cols <- names(GCT_processed()@cdesc)
      db <- detected_base()

      # Case #1 (log2/log10 at setup): prefilled + disabled. Case #2 (None at
      # setup): editable + blank; the user must enter the base (or 1 for linear).
      base_field <- if (!is.na(db)) {
        shinyjs::disabled(
          add_css_attributes(
            numericInput(
              ns("qc_cv_log_base"),
              label = "Log base (detected from setup)",
              value = db, min = 1, step = 1
            ),
            classes = "small-input"
          )
        )
      } else {
        tagList(
          add_css_attributes(
            numericInput(
              ns("qc_cv_log_base"),
              label = "Log base (if log-transformed before upload)",
              value = NA, min = 2, step = 1
            ),
            classes = "small-input"
          ),
          div(
            style = "background-color: #f8f9fa; border-left: 4px solid #007bff; padding: 8px; margin-bottom: 10px; border-radius: 0 4px 4px 0; color: #495057; font-size: 8pt;",
            icon("info-circle", style = "color: #007bff; margin-right: 6px;"),
            "If your data were log-transformed before upload, enter the base (e.g. 2 or 10). Leave blank if your data are already on a linear scale."
          )
        )
      }

      tagList(
        # Multi-column grouping selector
        add_css_attributes(
          selectInput(
            ns("qc_cv_annotation"),
            label    = "Group by",
            choices  = cdesc_cols,
            selected = default_annotation_column(),
            multiple = TRUE
          ),
          classes = "small-input"
        ),
        # Live preview table
        tableOutput(ns("group_preview_table")),
        # Log base for delinearization (detected or user-entered)
        base_field,
        # Normalized vs non-normalized source (default: non-normalized)
        add_css_attributes(
          checkboxInput(
            ns("qc_cv_use_normalized"),
            label = "Compute CV on normalized data",
            value = FALSE
          ),
          classes = "small-input"
        ),
        # Y-axis scale toggle
        add_css_attributes(
          radioButtons(
            ns("qc_cv_y_scale"),
            label    = "Y-axis scale",
            choices  = c("Linear" = "linear", "Logarithmic" = "log"),
            selected = "linear",
            inline   = TRUE
          ),
          classes = "small-input"
        ),
        # Filter checkbox
        add_css_attributes(
          checkboxInput(
            ns("qc_cv_filter_enabled"),
            label = "Apply CV filter",
            value = FALSE
          ),
          classes = "small-input"
        ),
        div(
          style = "background-color: #f8f9fa; border-left: 4px solid #007bff; padding: 8px; margin-bottom: 10px; border-radius: 0 4px 4px 0; color: #495057; font-size: 8pt;",
          icon("info-circle", style = "color: #007bff; margin-right: 6px;"),
          "This filter is local to this tab. It only affects CV plots/exports and the CV-tab filtered GCT export. It does not change the main processed GCT used by other tabs."
        ),
        # Filter options - only visible when filter is enabled
        # Per CLAUDE.md: plain input reference in condition string; ns = ns as arg
        conditionalPanel(
          condition = "input.qc_cv_filter_enabled == true",
          add_css_attributes(
            numericInput(
              ns("qc_cv_cutoff"),
              label = "CV cutoff",
              value = 0.2,
              min   = 0,
              step  = 0.05
            ),
            classes = "small-input"
          ),
          add_css_attributes(
            radioButtons(
              ns("qc_cv_min_groups"),
              label    = "Keep features where the CV cutoff is satisfied by:",
              choices  = c(
                "At least one group (keeps features reproducible in any condition)" = "one",
                "All groups (keeps only features reproducible across every condition)" = "all"
              ),
              selected = "one"
            ),
            classes = "small-input"
          ),
          ns = ns
        )
      )
    })

    ## Reactive helpers -------------------------------------------------------

    # Selected cdesc column(s) for grouping
    selected_cols <- reactive({
      cols <- input$qc_cv_annotation
      if (is.null(cols) || length(cols) == 0L) {
        default_annotation_column()
      } else {
        cols
      }
    })

    # Effective base for delinearization: the locked detected base wins (case #1);
    # otherwise the user-entered base (case #2). May be NA until the user enters one.
    effective_base <- reactive({
      db <- detected_base()
      if (!is.na(db)) return(db)
      input$qc_cv_log_base
    })

    # CV source GCT: non-normalized GCTs_original by default, or the normalized
    # processed GCT when the toggle is on. Delinearization (below) applies to both.
    source_gct <- reactive({
      if (isTRUE(input$qc_cv_use_normalized)) {
        req(GCT_processed())
        GCT_processed()
      } else {
        req(GCT_original(), GCT_processed())
        # GCTs_original is log-only and UNFILTERED; align it to the processed
        # analysis set so non-normalized CV reflects exactly the analyzed
        # samples/features, just without normalization.
        qc_cv_align_source(GCT_original(), GCT_processed())
      }
    })

    # Combined grouping vector (one label per sample), built from the SELECTED
    # source GCT's own cdesc so labels always align with that matrix's columns.
    grouping_vector <- reactive({
      req(source_gct(), selected_cols())
      combine_cdesc_cols(source_gct()@cdesc, selected_cols())
    })

    # Unfiltered CV table, computed on the delinearized source matrix.
    # base may be NA (blank = already linear); delinearize() passes the matrix
    # through unchanged in that case. Only reject an explicitly invalid entry.
    cv_table <- reactive({
      req(source_gct(), grouping_vector())
      base <- effective_base()
      validate(need(
        is.null(base) || (length(base) == 1L && (is.na(base) ||
          (is.numeric(base) && base > 1))),
        "Enter the log base your data were transformed with before upload (e.g. 2 or 10), or leave blank if your data are already on a linear scale."
      ))
      compute_cv_table(source_gct()@mat, grouping_vector(), base = base)
    })

    # Filtered CV table (NULL when filter is off)
    filtered_cv <- reactive({
      req(cv_table(), isTRUE(input$qc_cv_filter_enabled))
      cutoff     <- input$qc_cv_cutoff    %||% 0.2
      min_groups <- input$qc_cv_min_groups %||% "one"
      filter_cv_table(cv_table(), cutoff = cutoff, min_groups = min_groups)
    })

    # Filtered GCT. The export deliberately re-anchors to the PROCESSED GCT's row
    # set (not the CV source): filtered_cv() ids come from the selected source,
    # so the %in% intersection keeps only features that also survive processing.
    filtered_gct <- reactive({
      req(filtered_cv(), GCT_processed())
      keep_ids <- filtered_cv()$id
      subset_gct(GCT_processed(), rid = which(GCT_processed()@rid %in% keep_ids))
    })

    ## Group preview table ----------------------------------------------------
    output$group_preview_table <- renderTable({
      # Gate on the source actually consumed (grouping is built from source_gct).
      req(source_gct(), selected_cols())
      tab <- table(grouping_vector())
      data.frame(
        Group       = names(tab),
        `N samples` = as.integer(tab),
        check.names = FALSE
      )
    })

    output$cv_plot_section <- renderUI({
      req(show_cv_plots())
      tagList(
        hr(),
        # Unfiltered violin
        h4("CV distributions (unfiltered)"),
        plotOutput(ns("cv_violin_plot")),
        # Filtered violin
        conditionalPanel(
          condition = "input.qc_cv_filter_enabled == true",
          hr(),
          h4("CV distributions (filtered)"),
          plotOutput(ns("cv_violin_filtered_plot")),
          ns = ns
        )
      )
    })

    ## Y-axis helpers ----------------------------------------------------------
    log_scale <- reactive(identical(input$qc_cv_y_scale, "log"))

    ## Color palette reactive --------------------------------------------------
    # When a single grouping column is selected, reuse the customization color
    # mapping for that annotation column. For multi-variable grouping, generate
    # colors via set_annot_colors_discrete (same palettes as the Customize tab).
    cv_palette <- reactive({
      cols <- selected_cols()
      if (length(cols) == 1L && !is.null(color_map())) {
        annot_colors <- color_map()[[cols]]
        if (!is.null(annot_colors) && isTRUE(annot_colors$is_discrete)) {
          return(stats::setNames(annot_colors$colors, annot_colors$vals))
        }
      }
      # Multi-variable or missing color map: generate colors from the
      # project's standard discrete palette system.
      group_labels <- grouping_vector()
      group_df <- data.frame(Group = group_labels, stringsAsFactors = FALSE)
      color_info <- set_annot_colors_discrete(group_df, warn_for_interpolation = FALSE)[["Group"]]
      stats::setNames(color_info$colors, color_info$vals)
    })

    ## Unfiltered plots -------------------------------------------------------
    cv_violin_reactive <- reactive({
      req(show_cv_plots(), cv_table())
      source_label <- if (isTRUE(input$qc_cv_use_normalized)) "(normalized)" else "(non-normalized)"
      create_cv_violin_plot(cv_table(), title_suffix = source_label, palette = cv_palette(),
                            log_scale = log_scale())
    })

    output$cv_violin_plot <- renderPlot(cv_violin_reactive())

    ## Filtered plots ---------------------------------------------------------
    cv_violin_filtered_reactive <- reactive({
      req(show_cv_plots(), filtered_cv())
      label    <- paste("after filtering (cutoff",
                        input$qc_cv_cutoff %||% 0.2,
                        "-", input$qc_cv_min_groups %||% "one", "group)")
      create_cv_violin_plot(filtered_cv(), title_suffix = label, palette = cv_palette(),
                            log_scale = log_scale())
    })

    output$cv_violin_filtered_plot <- renderPlot(cv_violin_filtered_reactive())

    ## Export functions -------------------------------------------------------

    # CV needs >= 2 samples (sd/mean across replicates). For single-sample omes
    # the on-screen panel is greyed out; skip the exports too rather than write
    # an all-NA CV table/plot into the zip.
    cv_export_available <- function() {
      isTRUE(show_cv_plots()) &&
        is.null(min_samples_message(GCT_processed(), n = 2, analysis = "CV"))
    }

    cv_results_csv_export <- function(dir_name) {
      if (!cv_export_available()) return(invisible(NULL))
      write.csv(
        cv_table(),
        file      = file.path(dir_name, paste0("cv_results_", ome, ".csv")),
        row.names = FALSE
      )
    }

    cv_violin_export <- function(dir_name) {
      if (!cv_export_available()) return(invisible(NULL))
      ggsave_params <- get_ggsave_params()
      ggsave(
        filename = paste0("cv_violin_", ome, ".pdf"),
        plot     = cv_violin_reactive(),
        device   = "pdf",
        path     = dir_name,
        width    = ggsave_params$width,
        height   = ggsave_params$height,
        units    = ggsave_params$units
      )
    }

    cv_results_filtered_csv_export <- function(dir_name) {
      if (!cv_export_available()) return(invisible(NULL))
      if (!isTRUE(isolate(input$qc_cv_filter_enabled))) return(invisible(NULL))
      cutoff     <- isolate(input$qc_cv_cutoff)    %||% 0.2
      min_groups <- isolate(input$qc_cv_min_groups) %||% "one"
      fn <- paste0("cv_results_filtered_", ome, "_", cutoff, "_", min_groups, ".csv")
      write.csv(
        filtered_cv(),
        file      = file.path(dir_name, fn),
        row.names = FALSE
      )
    }

    cv_violin_filtered_export <- function(dir_name) {
      if (!cv_export_available()) return(invisible(NULL))
      if (!isTRUE(isolate(input$qc_cv_filter_enabled))) return(invisible(NULL))
      cutoff     <- isolate(input$qc_cv_cutoff)    %||% 0.2
      min_groups <- isolate(input$qc_cv_min_groups) %||% "one"
      ggsave_params <- get_ggsave_params()
      ggsave(
        filename = paste0("cv_violin_filtered_", ome, "_", cutoff, "_", min_groups, ".pdf"),
        plot     = cv_violin_filtered_reactive(),
        device   = "pdf",
        path     = dir_name,
        width    = ggsave_params$width,
        height   = ggsave_params$height,
        units    = ggsave_params$units
      )
    }

    cv_filtered_gct_export <- function(dir_name) {
      if (!cv_export_available()) return(invisible(NULL))
      if (!isTRUE(isolate(input$qc_cv_filter_enabled))) return(invisible(NULL))
      cutoff     <- isolate(input$qc_cv_cutoff)    %||% 0.2
      min_groups <- isolate(input$qc_cv_min_groups) %||% "one"
      fn <- file.path(
        dir_name,
        paste0("cv_filtered_", ome, "_", cutoff, "_", min_groups, ".gct")
      )
      write_gct(ds = filtered_gct(), ofile = fn, appenddim = FALSE)
    }

    return(list(
      cv_results_csv          = cv_results_csv_export,
      cv_violin               = cv_violin_export,
      cv_results_filtered_csv = cv_results_filtered_csv_export,
      cv_violin_filtered      = cv_violin_filtered_export,
      cv_filtered_gct         = cv_filtered_gct_export
    ))
  })
}
