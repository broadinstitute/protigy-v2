################################################################################
# Module: LM_Results
#
# Display per-ome results from the Linear Model module, including
# results table, dataset info, workflow params, and p-value histograms.
################################################################################

################################################################################
# Shiny functions (UI and server)
################################################################################

# UI for the lmResults tab
lmResults_Tab_UI <- function(id = "lmResultsTab") {
  ns <- NS(id)
  tagList(
    fluidRow(uiOutput(ns("ome_tabset_box")))
  )
}

# Server for the lmResults tab
lmResults_Tab_Server <- function(id = "lmResultsTab",
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

      selected_tab <- isolate(input$ome_tabs)
      if (is.null(selected_tab) || !(selected_tab %in% all_omes())) {
        selected_tab <- default_ome()
      }

      tabs <- lapply(all_omes(), function(ome) {
        tabPanel(
          title = ome,
          lmResults_Ome_UI(id = ns(ome), ome = ome)
        )
      })

      tab_set_panel <- do.call(
        tabsetPanel,
        c(tabs, list(id = ns("ome_tabs"), selected = isolate(selected_tab)))
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
        local({
          ome_local <- ome
          lmResults_Ome_Server(
            id = ome_local,
            ome = ome_local,
            GCT_processed = reactive(GCTs()[[ome_local]]),
            parameters = reactive(parameters()[[ome_local]]),
            default_annotation_column = reactive(default_annotations()[[ome_local]]),
            color_map = reactive(custom_colors()[[ome_local]]),
            lm_params = lm_params,
            lm_results = lm_results
          )
        })
      }, simplify = FALSE)
      all_exports(output_exports)
    })

    return(all_exports)
  })
}


# UI for an individual ome
lmResults_Ome_UI <- function(id, ome) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("ome_results_contents"))
  )
}


# Server for an individual ome
lmResults_Ome_Server <- function(id,
                                 ome,
                                 GCT_processed,
                                 parameters,
                                 default_annotation_column,
                                 color_map,
                                 lm_params,
                                 lm_results) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Get available coefficients from results
    lm_coefficients <- reactive({
      req(lm_results())
      df <- lm_results()[[ome]]
      if (is.null(df)) return(NULL)

      # Extract coefficient names from column patterns like logFC.<coef>
      logfc_cols <- grep("^logFC\\.", colnames(df), value = TRUE)
      coefs <- sub("^logFC\\.", "", logfc_cols)
      coefs
    })

    output$ome_results_contents <- renderUI({
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
          # Cutoff box
          shinydashboardPlus::box(
            uiOutput(ns("adjustments_table")),
            title = "Cutoff Selection",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            headerBorder = TRUE
          ),

          # Dataset info box
          shinydashboardPlus::box(
            tableOutput(ns("dataset_table")),
            title = "Dataset Information",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            headerBorder = TRUE
          ),

          # Workflow params box
          shinydashboardPlus::box(
            tableOutput(ns("workflow_table")),
            title = "Workflow Parameters",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            headerBorder = TRUE
          ),

          # P-value histogram box
          shinydashboardPlus::box(
            fluidRow(
              column(6,
                div(style = "overflow-x: auto; width: 100%;",
                  plotlyOutput(ns("adj_pval_hist_plot"), height = "400px")
                )
              ),
              column(6,
                div(style = "overflow-x: auto; width: 100%;",
                  plotlyOutput(ns("nom_pval_hist_plot"), height = "400px")
                )
              )
            ),
            sidebar = boxSidebar(
              uiOutput(ns("pval_hist_sidebar_contents")),
              id = ns("pval_hist_sidebar"),
              width = 25,
              icon = icon("gears", class = "fa-2xl"),
              background = "rgba(91, 98, 104, 0.9)"
            ),
            status = "primary",
            width = 12,
            title = "P-value Histogram",
            headerBorder = TRUE,
            solidHeader = TRUE
          ),

          # Alpha-level Analysis box
          shinydashboardPlus::box(
            uiOutput(ns("alpha_analysis")),
            title = "Alpha-level Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            headerBorder = TRUE
          ),

          # Results table box
          shinydashboardPlus::box(
            uiOutput(ns("results_table_controls")),
            DT::dataTableOutput(ns("results_table")),
            title = "Results Table",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            headerBorder = TRUE
          )
        )
      )
    })


    ## ADJUSTMENTS ##
    output$adjustments_table <- renderUI({
      req(lm_params())
      params_ome <- lm_params()[[ome]]
      req(params_ome)
      current_stat <- params_ome$stat
      current_cutoff <- params_ome$cutoff

      tagList(
        h5("The following selections are applied to Volcano Plots as well"),
        selectInput(ns("select_stat"), "Choose stat:",
                    choices = c("adj.p.val", "nom.p.val"),
                    selected = current_stat),
        numericInput(ns("select_cutoff_text"), "Choose cutoff:",
                     min = 0.001, max = 1,
                     value = current_cutoff, step = 0.001),
        checkboxInput(ns("apply_cutoff_all"), "Apply cutoff to all datasets", value = FALSE)
      )
    })

    observeEvent(input$select_stat, {
      current <- lm_params()
      current[[ome]]$stat <- input$select_stat
      lm_params(current)
    })

    observeEvent(input$select_cutoff_text, {
      current <- lm_params()
      if (isTRUE(input$apply_cutoff_all)) {
        for (dataset_name in names(current)) {
          current[[dataset_name]]$cutoff <- input$select_cutoff_text
        }
      } else {
        current[[ome]]$cutoff <- input$select_cutoff_text
      }
      lm_params(current)
    })

    original_cutoff_params <- reactiveVal(NULL)

    observeEvent(input$apply_cutoff_all, {
      req(lm_params(), input$select_cutoff_text)
      current <- lm_params()
      cutoff_value <- input$select_cutoff_text

      if (input$apply_cutoff_all) {
        # Snapshot only this ome's original cutoff to avoid overwriting other
        # omes' independently-modified cutoffs on revert.
        if (is.null(original_cutoff_params())) {
          original_cutoff_params(list(ome = ome, cutoff = current[[ome]]$cutoff))
        }
        for (dataset_name in names(current)) {
          current[[dataset_name]]$cutoff <- cutoff_value
        }
        lm_params(current)
        showNotification("Applied cutoff to all datasets.", type = "message", duration = 3)
      } else {
        snapshot <- original_cutoff_params()
        if (!is.null(snapshot)) {
          current[[ome]]$cutoff <- snapshot$cutoff
          lm_params(current)
          original_cutoff_params(NULL)
          showNotification("Reverted to original cutoff.", type = "message", duration = 3)
        }
      }
    })


    ## WORKFLOW INFO ##
    output$workflow_table <- renderTable({
      req(lm_params())
      params <- lm_params()[[ome]]
      req(params)

      descriptions <- c("Formula", "Variables", "Variable Types", "Intercept",
                         "Blocking Variable", "Cutoff", "Stat")
      values <- c(
        params$formula_string,
        paste(params$variables, collapse = ", "),
        paste(sapply(names(params$variable_types), function(v) {
          paste0(v, "=", params$variable_types[[v]])
        }), collapse = ", "),
        as.character(params$include_intercept),
        ifelse(is.null(params$blocking_variable), "None", params$blocking_variable),
        as.character(params$cutoff),
        params$stat
      )

      if (!is.null(params$interactions) && length(params$interactions) > 0) {
        interaction_str <- paste(sapply(params$interactions, function(i) paste(i, collapse = ":")), collapse = ", ")
        descriptions <- c(descriptions, "Interactions")
        values <- c(values, interaction_str)
      }

      if (!is.null(params$contrasts) && length(params$contrasts) > 0) {
        descriptions <- c(descriptions, "Contrasts")
        values <- c(values, paste(params$contrasts, collapse = "; "))
      }

      data.frame(Description = descriptions, Value = values)
    })


    ## DATASET INFO ##
    output$dataset_table <- renderTable({
      req(lm_params(), lm_results())
      df <- lm_results()[[ome]]
      if (is.null(df)) return(data.frame(Description = character(0), Value = character(0)))

      params <- lm_params()[[ome]]
      sig_cutoff <- params$cutoff
      sig_stat <- params$stat

      # Count features tested (rows with numeric data)
      numeric_cols <- sapply(df, is.numeric)
      df_filtered <- df[rowSums(!is.na(df[, numeric_cols, drop = FALSE])) > 0, ]

      results_df <- data.frame(
        Description = "Features tested",
        Value = nrow(df_filtered),
        stringsAsFactors = FALSE
      )

      # Count significant features per coefficient
      coefs <- lm_coefficients()
      if (!is.null(coefs)) {
        for (coef in coefs) {
          safe_coef <- make.names(coef)
          adjP_col <- grep(paste0("^adj\\.P\\.Val\\.", gsub("\\.", "\\\\.", safe_coef), "$"),
                           colnames(df), value = TRUE, perl = TRUE)[1]
          pval_col <- grep(paste0("^P\\.Value\\.", gsub("\\.", "\\\\.", safe_coef), "$"),
                           colnames(df), value = TRUE, perl = TRUE)[1]

          if (!is.na(adjP_col) && !is.na(pval_col)) {
            adj_pvals <- as.numeric(df[[adjP_col]])
            nom_pvals <- as.numeric(df[[pval_col]])

            if (sig_stat == "adj.p.val") {
              sig_count <- sum(adj_pvals < sig_cutoff, na.rm = TRUE)
            } else {
              sig_count <- sum(nom_pvals < sig_cutoff, na.rm = TRUE)
            }

            results_df <- rbind(results_df, data.frame(
              Description = paste0("Significant (", coef, ")"),
              Value = sig_count,
              stringsAsFactors = FALSE
            ))
          }
        }
      }

      results_df
    })


    ## P-VALUE HISTOGRAM ##
    output$pval_hist_sidebar_contents <- renderUI({
      coefs <- lm_coefficients()
      req(coefs)
      radioButtons(ns("pval_coefficient"), "Select Coefficient:", choices = coefs)
    })

    output$adj_pval_hist_plot <- renderPlotly({
      req(lm_params(), lm_results(), input$pval_coefficient)
      pvals <- get_lm_pvals(ome, lm_results(), input$pval_coefficient, "adj.P.Val")
      gg <- plot_lm_pval_histogram(
        pvals,
        paste("Adjusted P-value Histogram for", ome, ":", input$pval_coefficient),
        "Adjusted P-value",
        lm_results(), lm_params(), ome, input$pval_coefficient, "adj.P.Val"
      )
      ggplotly(gg)
    })

    output$nom_pval_hist_plot <- renderPlotly({
      req(lm_params(), lm_results(), input$pval_coefficient)
      pvals <- get_lm_pvals(ome, lm_results(), input$pval_coefficient, "P.Value")
      gg <- plot_lm_pval_histogram(
        pvals,
        paste("Nominal P-value Histogram for", ome, ":", input$pval_coefficient),
        "Nominal P-value",
        lm_results(), lm_params(), ome, input$pval_coefficient, "P.Value"
      )
      ggplotly(gg)
    })


    ## ALPHA-LEVEL ANALYSIS ##
    output$alpha_analysis <- renderUI({
      req(lm_params(), lm_results(), input$pval_coefficient)
      coef <- input$pval_coefficient
      adj_pvals <- get_lm_pvals(ome, lm_results(), coef, "adj.P.Val")
      suggestion <- suggest_alpha_level(adj_pvals)

      color <- if (!is.na(suggestion$alpha)) "#28a745" else "#856404"
      bg    <- if (!is.na(suggestion$alpha)) "#d4edda"  else "#fff3cd"

      div(
        style = paste0("background-color:", bg, "; border-left: 4px solid ", color,
                       "; padding: 10px; border-radius: 0 4px 4px 0;"),
        icon("flask", style = paste0("color:", color, "; margin-right: 6px;")),
        strong("Coefficient: ", style = paste0("color:", color, ";")),
        coef,
        br(),
        suggestion$message,
        if (!is.na(suggestion$alpha)) {
          tagList(
            br(),
            actionButton(
              ns("apply_alpha_suggestion"),
              paste0("Apply \u03b1 = ", suggestion$alpha),
              class = "btn btn-sm btn-success",
              style = "margin-top: 6px;"
            )
          )
        }
      )
    })

    observeEvent(input$apply_alpha_suggestion, {
      req(lm_results(), input$pval_coefficient)
      coef <- input$pval_coefficient
      adj_pvals <- get_lm_pvals(ome, lm_results(), coef, "adj.P.Val")
      suggestion <- suggest_alpha_level(adj_pvals)
      if (!is.na(suggestion$alpha)) {
        updateNumericInput(session, "select_cutoff_text", value = suggestion$alpha)
      }
    })


    ## RESULTS TABLE ##
    output$results_table_controls <- renderUI({
      coefs <- lm_coefficients()
      req(coefs)
      selectInput(ns("table_coefficient"), "Select coefficient to display:",
                  choices = coefs, selected = coefs[1])
    })

    output$results_table <- DT::renderDataTable({
      req(lm_results(), input$table_coefficient)
      df <- lm_results()[[ome]]
      if (is.null(df)) return(DT::datatable(data.frame()))

      coef <- input$table_coefficient
      safe_coef <- make.names(coef)

      # Select relevant columns: id, geneSymbol (if exists), and per-coefficient stats
      id_col <- "id"
      gene_col <- grep("^geneSymbol$", colnames(df), value = TRUE, ignore.case = TRUE)[1]

      coef_pattern <- paste0("\\.", gsub("\\.", "\\\\.", safe_coef), "$")
      coef_cols <- grep(coef_pattern, colnames(df), value = TRUE, perl = TRUE)

      display_cols <- c(id_col)
      if (!is.na(gene_col)) display_cols <- c(display_cols, gene_col)
      display_cols <- c(display_cols, coef_cols)

      # Filter to columns that exist
      display_cols <- display_cols[display_cols %in% colnames(df)]

      display_df <- df[, display_cols, drop = FALSE]

      # Round numeric columns
      for (col in colnames(display_df)) {
        if (is.numeric(display_df[[col]])) {
          display_df[[col]] <- signif(display_df[[col]], 4)
        }
      }

      DT::datatable(
        display_df,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          order = list()
        ),
        filter = "top",
        rownames = FALSE
      )
    })


    ## EXPORTS ##
    adj_pval_hist_export <- function(dir_name) {
      params <- lm_params()[[ome]]
      if (is.null(params)) return()

      results <- lm_results()
      if (is.null(results)) return()

      coefs <- lm_coefficients()
      if (is.null(coefs)) return()

      pdf_path <- file.path(dir_name, paste0("lm_adj_pval_hist_", ome, ".pdf"))
      pdf_params <- get_pdf_params()
      pdf(pdf_path, width = pdf_params$width, height = pdf_params$height)
      on.exit(dev.off(), add = TRUE)

      for (coef in coefs) {
        pvals <- get_lm_pvals(ome, results, coef, "adj.P.Val")
        gg <- plot_lm_pval_histogram(
          pvals,
          paste("Adjusted P-value Histogram for", ome, "-", coef),
          "Adjusted P-value",
          results, lm_params(), ome, coef, "adj.P.Val"
        )
        print(gg)
      }
    }

    nom_pval_hist_export <- function(dir_name) {
      params <- lm_params()[[ome]]
      if (is.null(params)) return()

      results <- lm_results()
      if (is.null(results)) return()

      coefs <- lm_coefficients()
      if (is.null(coefs)) return()

      pdf_path <- file.path(dir_name, paste0("lm_nom_pval_hist_", ome, ".pdf"))
      pdf_params <- get_pdf_params()
      pdf(pdf_path, width = pdf_params$width, height = pdf_params$height)
      on.exit(dev.off(), add = TRUE)

      for (coef in coefs) {
        pvals <- get_lm_pvals(ome, results, coef, "P.Value")
        gg <- plot_lm_pval_histogram(
          pvals,
          paste("Nominal P-value Histogram for", ome, "-", coef),
          "Nominal P-value",
          results, lm_params(), ome, coef, "P.Value"
        )
        print(gg)
      }
    }

    lm_results_export <- function(dir_name) {
      params <- lm_params()[[ome]]
      if (is.null(params)) return()

      results <- lm_results()[[ome]]
      if (is.null(results)) return()

      write.csv(
        results,
        file = file.path(dir_name, paste0("lm_results_", ome, ".csv")),
        row.names = FALSE
      )
    }

    workflow_params_export <- function(dir_name) {
      params <- lm_params()[[ome]]
      if (is.null(params)) return()

      df <- data.frame(
        Parameter = c("Formula", "Variables", "Variable Types", "Intercept",
                       "Blocking Variable", "Cutoff", "Stat"),
        Value = c(
          params$formula_string,
          paste(params$variables, collapse = ", "),
          paste(sapply(names(params$variable_types), function(v) {
            paste0(v, "=", params$variable_types[[v]])
          }), collapse = ", "),
          as.character(params$include_intercept),
          ifelse(is.null(params$blocking_variable), "None", params$blocking_variable),
          as.character(params$cutoff),
          params$stat
        )
      )

      if (!is.null(params$interactions) && length(params$interactions) > 0) {
        interaction_str <- paste(sapply(params$interactions, function(i) paste(i, collapse = ":")), collapse = ", ")
        df <- rbind(df, data.frame(Parameter = "Interactions", Value = interaction_str))
      }
      if (!is.null(params$contrasts) && length(params$contrasts) > 0) {
        df <- rbind(df, data.frame(Parameter = "Contrasts", Value = paste(params$contrasts, collapse = "; ")))
      }

      write.table(
        df,
        file = file.path(dir_name, paste0("lm_parameters_", ome, ".txt")),
        sep = "\t",
        quote = FALSE,
        row.names = FALSE
      )
    }

    return(list(
      lm_adj_pval_hist = adj_pval_hist_export,
      lm_nom_pval_hist = nom_pval_hist_export,
      lm_results = lm_results_export,
      lm_workflow_parameters = workflow_params_export
    ))
  })
}
