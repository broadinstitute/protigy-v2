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
                             globals) {

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

      fluidRow(
        shinydashboardPlus::box(
          div(
            style = "background-color: #f8f9fa; border-left: 4px solid #007bff; padding: 12px; margin-bottom: 15px; border-radius: 0 4px 4px 0;",
            icon("info-circle", style = "color: #007bff; margin-right: 8px;"),
            strong("Note: ", style = "color: #495057;"),
            "CV is computed on raw (linear) intensities; log2/log10-transformed datasets are delinearized automatically before the calculation.",
            style = "color: #495057;"
          ),
          uiOutput(ns("qc_cv_controls")),
          uiOutput(ns("cv_plot_section")),
          status       = "primary",
          width        = 12,
          title        = "Coefficient of Variation (CV)",
          headerBorder = TRUE,
          solidHeader  = TRUE
        )
      )
    })

    ## Controls UI -------------------------------------------------------------
    output$qc_cv_controls <- renderUI({
      req(show_cv_plots())
      req(GCT_processed())

      cdesc_cols <- names(GCT_processed()@cdesc)

      tagList(
        fluidRow(
          column(4,
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
            tableOutput(ns("group_preview_table"))
          ),
          column(4,
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
            conditionalPanel(
              condition = "input.qc_cv_filter_enabled == true",
              div(
                style = "background-color: #f8f9fa; border-left: 4px solid #007bff; padding: 12px; margin-bottom: 15px; border-radius: 0 4px 4px 0;",
                icon("info-circle", style = "color: #007bff; margin-right: 8px;"),
                strong("Note: ", style = "color: #495057;"),
                "This CV filter is local to this tab and only affects CV plots/exports and the CV-tab filtered GCT export. It does not change the main processed GCT used by other tabs."
                ,
                style = "color: #495057;"
              ),
              ns = ns
            ),
            # Filter options — only visible when filter is enabled
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

    # Combined grouping vector (one label per sample)
    grouping_vector <- reactive({
      req(GCT_processed(), selected_cols())
      combine_cdesc_cols(GCT_processed()@cdesc, selected_cols())
    })

    # Unfiltered CV table. CV is computed on LINEAR intensities, so pass the
    # dataset's declared log base; compute_cv_table delinearizes accordingly.
    cv_table <- reactive({
      req(GCT_processed(), grouping_vector())
      log_base <- (parameters() %||% list())$log_transformation %||% "None"
      compute_cv_table(GCT_processed()@mat, grouping_vector(),
                       log_base = log_base)
    })

    # Filtered CV table (NULL when filter is off)
    filtered_cv <- reactive({
      req(cv_table(), isTRUE(input$qc_cv_filter_enabled))
      cutoff     <- input$qc_cv_cutoff    %||% 0.2
      min_groups <- input$qc_cv_min_groups %||% "one"
      filter_cv_table(cv_table(), cutoff = cutoff, min_groups = min_groups)
    })

    # Filtered GCT
    filtered_gct <- reactive({
      req(filtered_cv(), GCT_processed())
      keep_ids <- filtered_cv()$id
      subset_gct(GCT_processed(), rid = which(GCT_processed()@rid %in% keep_ids))
    })

    ## Group preview table ----------------------------------------------------
    output$group_preview_table <- renderTable({
      req(GCT_processed(), selected_cols())
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
      create_cv_violin_plot(cv_table(), palette = cv_palette(),
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

    cv_results_csv_export <- function(dir_name) {
      write.csv(
        cv_table(),
        file      = file.path(dir_name, paste0("cv_results_", ome, ".csv")),
        row.names = FALSE
      )
    }

    cv_violin_export <- function(dir_name) {
      if (!isTRUE(show_cv_plots())) return(invisible(NULL))
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
      if (!isTRUE(show_cv_plots())) return(invisible(NULL))
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
