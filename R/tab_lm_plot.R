################################################################################
# Module: LM_Plot
#
# Per-ome volcano plots for the Linear Model module with coefficient selector.
################################################################################

################################################################################
# Shiny functions (UI and server)
################################################################################

# UI for the lmPlot tab
lmPlot_Tab_UI <- function(id = "lmPlotTab") {
  ns <- NS(id)
  tagList(
    fluidRow(uiOutput(ns("ome_tabset_box")))
  )
}

# Server for the lmPlot tab
lmPlot_Tab_Server <- function(id = "lmPlotTab",
                              GCTs_and_params,
                              globals,
                              lm_results,
                              lm_params) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

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

    all_omes <- reactive(names(GCTs()))
    default_ome <- reactive(globals$default_ome)
    custom_colors <- reactive(globals$colors)

    lm_results_check <- reactive({
      validate(need(lm_results(), "Linear model not yet run."))
      lm_results()
    })

    ## OME TABS ##
    output$ome_tabset_box <- renderUI({
      req(GCTs(), parameters())
      req(lm_results_check())
      req(all_omes(), default_ome())

      tabs <- lapply(all_omes(), function(ome) {
        tabPanel(
          title = ome,
          lmPlot_Ome_UI(id = ns(ome), ome = ome)
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

    # Call server for each ome
    all_exports <- reactiveVal()
    observeEvent(all_omes(), once = TRUE, {
      output_exports <- sapply(all_omes(), function(ome) {
        lmPlot_Ome_Server(
          id = ome,
          ome = ome,
          GCT_processed = reactive(GCTs()[[ome]]),
          parameters = reactive(parameters()[[ome]]),
          default_annotation_column = reactive(default_annotations()[[ome]]),
          color_map = reactive(custom_colors()[[ome]]),
          lm_params = lm_params,
          lm_results = lm_results
        )
      }, simplify = FALSE)
      all_exports(output_exports)
    })

    return(all_exports)
  })
}


# UI for an individual ome
lmPlot_Ome_UI <- function(id, ome) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("ome_plot_contents"))
  )
}


# Server for an individual ome
lmPlot_Ome_Server <- function(id,
                              ome,
                              GCT_processed,
                              parameters,
                              default_annotation_column,
                              color_map,
                              lm_params,
                              lm_results) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Proteins of interest (POI) selected via click or search
    proteins_of_interest <- reactiveVal(character(0))
    hidden_label_count   <- reactiveVal(0L)

    # Get available coefficients (filter out intercept, apply display filter)
    lm_coefficients <- reactive({
      req(lm_results())
      df <- lm_results()[[ome]]
      if (is.null(df)) return(NULL)

      logfc_cols <- grep("^logFC\\.", colnames(df), value = TRUE)
      coefs <- sub("^logFC\\.", "", logfc_cols)
      coefs <- coefs[!grepl("^X\\.Intercept\\.$|^\\(Intercept\\)$", coefs)]

      params <- lm_params()[[ome]]
      if (!is.null(params) && !is.null(params$all_design_coefs)) {
        all_design_safe <- make.names(params$all_design_coefs)
        display_safe <- make.names(params$display_coefficients %||% character(0))
        hide <- setdiff(all_design_safe, display_safe)
        coefs <- setdiff(coefs, hide)
      }
      coefs
    })

    output$ome_plot_contents <- renderUI({
      req(lm_params())

      params <- lm_params()[[ome]]
      if (is.null(params)) {
        return(h4("No linear model configured for this dataset."))
      }

      req(lm_results())
      df <- lm_results()[[ome]]
      if (is.null(df)) {
        return(h4("No results available for this dataset."))
      }

      tagList(
        fluidRow(
          column(8,
            shinydashboardPlus::box(
              plotlyOutput(ns("volcano_plot")),
              status = "primary",
              width = NULL,
              title = "Volcano Plot",
              headerBorder = TRUE,
              solidHeader = TRUE
            )
          ),
          column(4,
            shinydashboardPlus::box(
              uiOutput(ns("volcano_sidebar_contents")),
              width = NULL,
              title = "Plot Controls",
              headerBorder = TRUE
            )
          )
        )
      )
    })

    ## SIDEBAR ##
    output$volcano_sidebar_contents <- renderUI({
      coefs <- lm_coefficients()
      req(coefs)

      # Determine searchable columns from results
      df <- lm_results()[[ome]]
      search_col_choices <- if (!is.null(df)) {
        char_cols <- names(df)[!sapply(df, is.numeric)]
        if (length(char_cols) == 0) char_cols <- names(df)[1]
        default_col <- grep("^id$", char_cols, value = TRUE, ignore.case = TRUE)
        if (length(default_col) == 0) default_col <- char_cols[1]
        list(choices = char_cols, selected = default_col[1])
      } else {
        list(choices = "id", selected = "id")
      }

      tagList(
        radioButtons(ns("volcano_coefficient"), "Select Coefficient:", choices = coefs),

        hr(),

        # Labeling mode
        strong("Label Proteins:"),
        checkboxGroupInput(
          ns("label_mode"),
          label   = NULL,
          choices = c(
            "Proteins of interest" = "poi",
            "Top 20 significant"   = "significant_top20",
            "All significant"      = "significant"
          ),
          selected = character(0)
        ),

        hr(),

        # Search section
        strong("Search Proteins:"),
        selectInput(
          ns("search_metadata_col"),
          label    = "Search column:",
          choices  = search_col_choices$choices,
          selected = search_col_choices$selected
        ),
        textAreaInput(
          ns("protein_search"),
          label       = NULL,
          placeholder = "Paste IDs separated by space, comma, or semicolon",
          rows        = 3
        ),
        actionButton(ns("search_btn"), "Search", class = "btn-sm btn-primary"),

        hr(),

        # POI list
        strong("Proteins of Interest:"),
        uiOutput(ns("poi_list_ui")),

        # Hidden label overflow warning
        uiOutput(ns("hidden_labels_warning"))
      )
    })

    ## POI list UI ##
    output$poi_list_ui <- renderUI({
      pois <- proteins_of_interest()
      if (length(pois) == 0) {
        return(p("No proteins selected.",
                 style = "color: #888; font-style: italic; font-size: 12px;"))
      }
      poi_rows <- lapply(pois, function(pid) {
        fluidRow(
          column(9, p(pid, style = "margin: 2px 0; font-size: 13px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;")),
          column(3, actionButton(
            inputId = ns(paste0("remove_poi_", make.names(pid))),
            label   = "\u00d7",
            class   = "btn-xs btn-danger",
            style   = "padding: 1px 5px; margin: 0;"
          ))
        )
      })
      tagList(
        do.call(tagList, poi_rows),
        br(),
        actionButton(ns("clear_all_poi"), "Clear all", class = "btn-xs btn-warning")
      )
    })

    # Per-protein remove observers
    poi_observer_registry <- reactiveVal(character(0))
    observeEvent(proteins_of_interest(), {
      pois     <- proteins_of_interest()
      existing <- poi_observer_registry()
      new_btn_ids <- setdiff(
        vapply(pois, function(p) paste0("remove_poi_", make.names(p)), character(1)),
        existing
      )
      lapply(new_btn_ids, function(btn_id) {
        pid <- pois[vapply(pois, function(p) paste0("remove_poi_", make.names(p)), character(1)) == btn_id]
        local({
          pid_local    <- pid
          btn_id_local <- btn_id
          observeEvent(input[[btn_id_local]], {
            proteins_of_interest(setdiff(proteins_of_interest(), pid_local))
            poi_observer_registry(setdiff(poi_observer_registry(), btn_id_local))
          }, ignoreNULL = TRUE, ignoreInit = TRUE, once = TRUE)
        })
      })
      poi_observer_registry(unique(c(existing, new_btn_ids)))
    })

    observeEvent(input$clear_all_poi, {
      proteins_of_interest(character(0))
      hidden_label_count(0L)
    })

    # Auto-enable POI checkbox when proteins are added
    observeEvent(proteins_of_interest(), {
      pois <- proteins_of_interest()
      if (length(pois) > 0 && !"poi" %in% isolate(input$label_mode)) {
        updateCheckboxGroupInput(session, "label_mode",
          selected = unique(c(isolate(input$label_mode), "poi")))
      }
    }, ignoreNULL = FALSE)

    ## Hidden label warning ##
    output$hidden_labels_warning <- renderUI({
      n <- hidden_label_count()
      if (is.null(n) || n == 0L) return(NULL)
      div(
        style = "margin-top: 8px; padding: 6px 8px; background: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; font-size: 12px; color: #856404;",
        icon("triangle-exclamation"),
        paste0(" Labels of ", n, " feature(s) were hidden due to overflow.")
      )
    })

    ## SEARCH OBSERVER ##
    observeEvent(input$search_btn, {
      req(lm_results(), input$protein_search, input$search_metadata_col)
      df         <- lm_results()[[ome]]
      search_col <- input$search_metadata_col
      if (!search_col %in% colnames(df)) {
        showNotification(paste0("Column '", search_col, "' not found."),
                         type = "error", duration = 4)
        return()
      }
      id_col <- grep("^id$", colnames(df), value = TRUE, ignore.case = TRUE)[1]
      if (is.na(id_col)) {
        showNotification("No 'id' column found in LM results.", type = "error", duration = 4)
        return()
      }
      tokens <- parse_protein_search_input(input$protein_search)
      if (length(tokens) == 0) return()

      matched   <- character(0)
      unmatched <- character(0)
      for (tok in tokens) {
        hit_rows <- df[tolower(as.character(df[[search_col]])) == tolower(tok), ]
        if (nrow(hit_rows) > 0) {
          matched <- c(matched, as.character(hit_rows[[id_col]]))
        } else {
          unmatched <- c(unmatched, tok)
        }
      }
      if (length(unmatched) > 0) {
        showNotification(paste0("Not found: ", paste(unmatched, collapse = ", ")),
                         type = "warning", duration = 5)
      }
      if (length(matched) > 0) {
        proteins_of_interest(unique(c(proteins_of_interest(), matched)))
      }
    })

    ## CLICK-TO-ADD POI ##
    observeEvent(plotly::event_data("plotly_click", source = ns("lm_volcano")), {
      click <- plotly::event_data("plotly_click", source = ns("lm_volcano"))
      req(click, lm_results(), input$volcano_coefficient)
      df      <- lm_results()[[ome]]
      df_plot <- build_lm_volcano_df(ome, input$volcano_coefficient, df, lm_params())
      if (is.null(df_plot)) return()
      fid <- get_clicked_feature_id(click, df_plot, tol = 0.05)
      if (!is.na(fid) && nchar(fid) > 0) {
        proteins_of_interest(unique(c(proteins_of_interest(), fid)))
      }
    })

    ## VOLCANO PLOT ##
    output$volcano_plot <- renderPlotly({
      req(lm_results(), lm_params(), input$volcano_coefficient)
      df <- lm_results()[[ome]]
      req(df)

      gg <- plotLmVolcano(
        ome         = ome,
        coefficient = input$volcano_coefficient,
        df          = df,
        lm_params   = lm_params()
      )
      p <- ggplotly(gg, source = ns("lm_volcano"), tooltip = "text")

      # Build standardized df for labeling
      df_plot <- tryCatch(
        build_lm_volcano_df(ome, input$volcano_coefficient, df, lm_params()),
        error = function(e) NULL
      )
      if (!is.null(df_plot)) {
        p <- add_volcano_labels(
          p,
          df              = df_plot,
          poi             = proteins_of_interest(),
          label_mode      = input$label_mode %||% character(0),
          y_cutoff        = attr(df_plot, "y_cutoff"),
          hidden_count_rv = hidden_label_count
        )
      }
      p
    })


    ## EXPORTS ##
    volcano_export <- function(dir_name) {
      params <- lm_params()[[ome]]
      if (is.null(params)) return()
      df <- lm_results()[[ome]]
      if (is.null(df)) return()
      coefs <- lm_coefficients()
      if (is.null(coefs)) return()

      pdf_path   <- file.path(dir_name, paste0("lm_volcano_plots_", ome, ".pdf"))
      pdf_params <- get_pdf_params()
      pdf(pdf_path, width = pdf_params$width, height = pdf_params$height)
      on.exit(dev.off(), add = TRUE)

      for (coef in coefs) {
        gg <- plotLmVolcano(
          ome            = ome,
          coefficient    = coef,
          df             = df,
          lm_params      = lm_params(),
          label_proteins = proteins_of_interest(),
          label_mode     = input$label_mode %||% character(0)
        )
        print(gg)
      }
    }

    # Export CSV of labeled proteins (mirrors stat plot pattern)
    lm_labeled_poi_export <- function(dir_name) {
      pois <- proteins_of_interest()
      if (length(pois) == 0) return()
      df <- lm_results()[[ome]]
      if (is.null(df)) return()
      id_col <- grep("^id$", colnames(df), value = TRUE, ignore.case = TRUE)[1]
      if (is.na(id_col)) return()
      poi_df <- df[as.character(df[[id_col]]) %in% pois, , drop = FALSE]
      if (nrow(poi_df) == 0) return()
      write.csv(poi_df,
                file = file.path(dir_name,
                                 paste0("lm_proteins_of_interest_", ome, ".csv")),
                row.names = FALSE)
    }

    return(list(
      lm_volcano_plot = volcano_export,
      lm_poi_export   = lm_labeled_poi_export
    ))
  })
}
