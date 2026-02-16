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

    # Get available coefficients (filter out intercept by default)
    lm_coefficients <- reactive({
      req(lm_results())
      df <- lm_results()[[ome]]
      if (is.null(df)) return(NULL)

      logfc_cols <- grep("^logFC\\.", colnames(df), value = TRUE)
      coefs <- sub("^logFC\\.", "", logfc_cols)

      # Filter out intercept
      coefs[!grepl("^X\\.Intercept\\.$|^\\(Intercept\\)$", coefs)]
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
          shinydashboardPlus::box(
            plotlyOutput(ns("volcano_plot")),
            sidebar = boxSidebar(
              uiOutput(ns("volcano_sidebar_contents")),
              id = ns("volcano_sidebar"),
              width = 25,
              icon = icon("gears", class = "fa-2xl"),
              background = "rgba(91, 98, 104, 0.9)"
            ),
            status = "primary",
            width = 12,
            title = "Volcano Plot",
            headerBorder = TRUE,
            solidHeader = TRUE
          )
        )
      )
    })

    ## SIDEBAR ##
    output$volcano_sidebar_contents <- renderUI({
      coefs <- lm_coefficients()
      req(coefs)
      radioButtons(ns("volcano_coefficient"), "Select Coefficient:", choices = coefs)
    })

    ## VOLCANO PLOT ##
    output$volcano_plot <- renderPlotly({
      req(lm_results(), lm_params(), input$volcano_coefficient)

      df <- lm_results()[[ome]]
      req(df)

      gg <- plotLmVolcano(
        ome = ome,
        coefficient = input$volcano_coefficient,
        df = df,
        lm_params = lm_params()
      )
      ggplotly(gg)
    })


    ## EXPORTS ##
    volcano_export <- function(dir_name) {
      params <- lm_params()[[ome]]
      if (is.null(params)) return()

      df <- lm_results()[[ome]]
      if (is.null(df)) return()

      coefs <- lm_coefficients()
      if (is.null(coefs)) return()

      pdf_path <- file.path(dir_name, paste0("lm_volcano_plots_", ome, ".pdf"))
      pdf_params <- get_pdf_params()
      pdf(pdf_path, width = pdf_params$width, height = pdf_params$height)
      on.exit(dev.off(), add = TRUE)

      for (coef in coefs) {
        gg <- plotLmVolcano(
          ome = ome,
          coefficient = coef,
          df = df,
          lm_params = lm_params()
        )
        print(gg)
      }
    }

    return(list(
      lm_volcano_plot = volcano_export
    ))
  })
}
