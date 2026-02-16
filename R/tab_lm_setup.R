################################################################################
# Module: LM_Setup
#
# Allow users to configure and run a linear model (limma lmFit -> eBayes)
# with arbitrary formula-based regression, multiple covariates, interactions,
# and optional blocking factors.
################################################################################

################################################################################
# Shiny functions (UI and server)
################################################################################

# UI for the lmSetup tab
lmSetup_Tab_UI <- function(id = "lmSetupTab") {
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("Linear Model Setup"),
      uiOutput(ns("setup_controls"))
    )
  )
}


# Server for the lmSetup tab
lmSetup_Tab_Server <- function(id = "lmSetupTab", GCTs_and_params, globals, parent = NULL) {
  moduleServer(id, function(input, output, session) {

    ## REACTIVE VALUES ##
    lm_param <- reactiveVal(list())
    lm_results <- reactiveVal(list())

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

    # vector of all omes
    all_omes <- reactive(names(GCTs()))

    # gather relevant variables from globals
    default_ome <- reactive(globals$default_ome)
    custom_colors <- reactive(globals$colors)


    ## MAIN SETUP CONTROLS ##
    output$setup_controls <- renderUI({
      req(GCTs(), parameters())

      ome_names <- names(GCTs())

      tagList(
        fluidRow(
          # Column 1: Ome selector and run button
          column(2,
            div(class = "stat-setup-controls",
              selectInput(ns("selected_ome"), "Select dataset:",
                          choices = ome_names, selected = default_ome()),
              textOutput(ns("annotation_col")),
              if (length(ome_names) > 1) {
                checkboxInput(ns("apply_all"), "Apply to all datasets", value = FALSE)
              },
              actionButton(ns("run_lm_button"), "Run Linear Model",
                           class = "btn btn-primary")
            )
          ),

          # Column 2: Variable configuration
          column(4,
            div(class = "stat-setup-controls",
              uiOutput(ns("variable_picker_ui")),
              uiOutput(ns("variable_type_toggles_ui")),
              checkboxInput(ns("include_intercept"), "Include intercept", value = TRUE),
              uiOutput(ns("interaction_picker_ui")),
              uiOutput(ns("blocking_var_ui")),
              h5(strong("Model Formula:")),
              verbatimTextOutput(ns("formula_display"))
            )
          ),

          # Column 3: Design matrix preview and contrast builder
          column(6,
            div(
              h5(strong("Design Matrix Preview (first 10 rows):")),
              DT::dataTableOutput(ns("design_matrix_preview")),
              br(),
              uiOutput(ns("coefficient_names_display")),
              br(),
              uiOutput(ns("contrast_builder_ui"))
            )
          )
        ),

        br(),

        # Documentation section
        div(
          class = "well",
          h4("Linear Model Documentation"),
          div(
            style = "background-color: #f8f9fa; border-left: 4px solid #007bff; padding: 12px; margin-bottom: 15px; border-radius: 0 4px 4px 0; color: #495057;",
            icon("info-circle", style = "color: #007bff; margin-right: 8px;"),
            strong("Note: ", style = "color: #495057;"),
            "Linear models require log-transformed data. Please ensure your data have been log-transformed."
          ),
          h5(strong("About:"), style = "font-size: 16px; margin-top: 20px; margin-bottom: 10px;"),
          p("This module fits a linear model to each feature using limma's lmFit/eBayes pipeline. ",
            "Unlike the Statistics module which performs group-based tests, the Linear Model module ",
            "supports arbitrary formula-based regression with multiple covariates, interaction terms, ",
            "continuous variables, and blocking factors."),
          h5(strong("Instructions:"), style = "font-size: 16px; margin-top: 20px; margin-bottom: 10px;"),
          tags$ol(
            tags$li("Select variables from the sample metadata (cdesc) to include in the model"),
            tags$li("Set each variable as Factor or Continuous"),
            tags$li("Optionally add interaction terms and/or a blocking variable"),
            tags$li("Review the formula and design matrix preview"),
            tags$li("Optionally define custom contrasts"),
            tags$li("Click 'Run Linear Model' to fit the model")
          )
        )
      )
    })


    ## SELECTED OME ##
    selected_ome <- reactive({
      req(input$selected_ome)
      input$selected_ome
    })

    cdesc <- reactive({
      req(GCTs(), selected_ome())
      GCTs()[[selected_ome()]]@cdesc
    })

    default_annotation_column <- reactive({
      req(default_annotations(), selected_ome())
      default_annotations()[[selected_ome()]]
    })

    output$annotation_col <- renderText({
      req(default_annotation_column())
      paste("Annotation column:", default_annotation_column())
    })


    ## VARIABLE PICKER ##
    output$variable_picker_ui <- renderUI({
      req(cdesc())
      # Offer all cdesc columns as potential model variables
      col_names <- colnames(cdesc())
      pickerInput(
        ns("selected_variables"),
        "Select model variables:",
        choices = col_names,
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          noneSelectedText = "No variables selected"
        )
      )
    })


    ## VARIABLE TYPE TOGGLES ##
    output$variable_type_toggles_ui <- renderUI({
      req(input$selected_variables)
      vars <- input$selected_variables

      toggle_list <- lapply(vars, function(var) {
        # Auto-detect type
        col_data <- cdesc()[[var]]
        default_type <- if (is.discrete(col_data)) "factor" else "continuous"

        radioButtons(
          ns(paste0("vartype_", make.names(var))),
          label = var,
          choices = c("Factor" = "factor", "Continuous" = "continuous"),
          selected = default_type,
          inline = TRUE
        )
      })

      tagList(
        h5(strong("Variable Types:")),
        toggle_list
      )
    })


    ## INTERACTION PICKER ##
    output$interaction_picker_ui <- renderUI({
      req(input$selected_variables)
      vars <- input$selected_variables
      if (length(vars) < 2) return(NULL)

      # Generate all pairs
      pairs <- combn(vars, 2, simplify = FALSE)
      pair_labels <- sapply(pairs, function(p) paste(p[1], ":", p[2]))
      names(pairs) <- pair_labels

      pickerInput(
        ns("interaction_terms"),
        "Interaction terms (optional):",
        choices = pair_labels,
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE,
          noneSelectedText = "None"
        )
      )
    })


    ## BLOCKING VARIABLE ##
    output$blocking_var_ui <- renderUI({
      req(cdesc())
      vars <- input$selected_variables
      # Only offer discrete columns not already in the model
      col_names <- colnames(cdesc())
      discrete_cols <- col_names[sapply(col_names, function(cn) is.discrete(cdesc()[[cn]]))]
      available <- setdiff(discrete_cols, vars)

      if (length(available) == 0) return(NULL)

      tagList(
        selectInput(
          ns("blocking_variable"),
          "Blocking variable (optional):",
          choices = c("None" = "", available),
          selected = ""
        ),
        conditionalPanel(
          condition = paste0("input['", ns("blocking_variable"), "'] !== ''"),
          actionButton(ns("clear_blocking_var"), "Clear", class = "btn btn-sm btn-default")
        )
      )
    })

    observeEvent(input$clear_blocking_var, {
      updateSelectInput(session, "blocking_variable", selected = "")
    })


    ## FORMULA DISPLAY ##
    formula_string <- reactive({
      vars <- input$selected_variables
      if (is.null(vars) || length(vars) == 0) return(NULL)

      include_intercept <- isTRUE(input$include_intercept)

      # Parse interaction terms
      interactions <- list()
      if (!is.null(input$interaction_terms)) {
        vars_list <- input$selected_variables
        pairs <- combn(vars_list, 2, simplify = FALSE)
        pair_labels <- sapply(pairs, function(p) paste(p[1], ":", p[2]))
        selected_indices <- which(pair_labels %in% input$interaction_terms)
        interactions <- pairs[selected_indices]
      }

      build_formula_string(vars, include_intercept, interactions)
    })

    output$formula_display <- renderText({
      f <- formula_string()
      if (is.null(f)) return("(select variables to build formula)")
      f
    })


    ## VARIABLE TYPES (collected from toggles) ##
    variable_types <- reactive({
      vars <- input$selected_variables
      if (is.null(vars)) return(list())

      types <- list()
      for (var in vars) {
        toggle_id <- paste0("vartype_", make.names(var))
        val <- input[[toggle_id]]
        if (!is.null(val)) {
          types[[var]] <- val
        }
      }
      types
    })


    ## DESIGN MATRIX PREVIEW ##
    output$design_matrix_preview <- DT::renderDataTable({
      f <- formula_string()
      req(f, cdesc())

      # Prepare cdesc with correct types
      cdesc_work <- cdesc()
      vtypes <- variable_types()
      for (var_name in names(vtypes)) {
        if (var_name %in% colnames(cdesc_work)) {
          if (vtypes[[var_name]] == "factor") {
            cdesc_work[[var_name]] <- factor(cdesc_work[[var_name]])
          } else {
            cdesc_work[[var_name]] <- as.numeric(as.character(cdesc_work[[var_name]]))
          }
        }
      }

      # Remove rows with NAs in model variables
      model_vars <- all.vars(as.formula(f))
      complete_mask <- complete.cases(cdesc_work[, model_vars, drop = FALSE])
      cdesc_clean <- cdesc_work[complete_mask, , drop = FALSE]

      dm <- tryCatch(
        model.matrix(as.formula(f), data = cdesc_clean),
        error = function(e) NULL
      )

      if (is.null(dm)) {
        return(DT::datatable(data.frame(Error = "Could not build design matrix. Check variable types."),
                             options = list(dom = "t")))
      }

      # Show first 10 rows
      preview <- as.data.frame(dm[seq_len(min(10, nrow(dm))), , drop = FALSE])
      DT::datatable(preview, options = list(dom = "t", scrollX = TRUE, pageLength = 10))
    })


    ## COEFFICIENT NAMES DISPLAY ##
    output$coefficient_names_display <- renderUI({
      f <- formula_string()
      req(f, cdesc())

      cdesc_work <- cdesc()
      vtypes <- variable_types()
      for (var_name in names(vtypes)) {
        if (var_name %in% colnames(cdesc_work)) {
          if (vtypes[[var_name]] == "factor") {
            cdesc_work[[var_name]] <- factor(cdesc_work[[var_name]])
          } else {
            cdesc_work[[var_name]] <- as.numeric(as.character(cdesc_work[[var_name]]))
          }
        }
      }

      model_vars <- all.vars(as.formula(f))
      complete_mask <- complete.cases(cdesc_work[, model_vars, drop = FALSE])
      cdesc_clean <- cdesc_work[complete_mask, , drop = FALSE]

      dm <- tryCatch(
        model.matrix(as.formula(f), data = cdesc_clean),
        error = function(e) NULL
      )

      if (is.null(dm)) return(NULL)

      coefs <- colnames(dm)
      tagList(
        h5(strong("Model Coefficients:")),
        tags$ul(lapply(coefs, function(c) tags$li(c)))
      )
    })


    ## CONTRAST BUILDER ##
    output$contrast_builder_ui <- renderUI({
      tagList(
        checkboxInput(ns("use_contrasts"), "Define custom contrasts?", value = FALSE),
        uiOutput(ns("contrast_input_ui"))
      )
    })

    output$contrast_input_ui <- renderUI({
      if (!isTRUE(input$use_contrasts)) return(NULL)

      tagList(
        textAreaInput(
          ns("contrast_strings"),
          "Enter contrast strings (one per line):",
          placeholder = "e.g., TreatmentDrug - TreatmentPlacebo",
          rows = 4,
          width = "100%"
        ),
        helpText("Each line should be a valid contrast expression using coefficient names shown above."),
        uiOutput(ns("contrast_validation"))
      )
    })

    output$contrast_validation <- renderUI({
      req(input$contrast_strings)
      contrast_text <- trimws(input$contrast_strings)
      if (nchar(contrast_text) == 0) return(NULL)

      lines <- strsplit(contrast_text, "\n")[[1]]
      lines <- trimws(lines)
      lines <- lines[nchar(lines) > 0]

      if (length(lines) == 0) return(NULL)

      # Try to validate by building contrast matrix
      f <- formula_string()
      req(f, cdesc())

      cdesc_work <- cdesc()
      vtypes <- variable_types()
      for (var_name in names(vtypes)) {
        if (var_name %in% colnames(cdesc_work)) {
          if (vtypes[[var_name]] == "factor") {
            cdesc_work[[var_name]] <- factor(cdesc_work[[var_name]])
          } else {
            cdesc_work[[var_name]] <- as.numeric(as.character(cdesc_work[[var_name]]))
          }
        }
      }

      model_vars <- all.vars(as.formula(f))
      complete_mask <- complete.cases(cdesc_work[, model_vars, drop = FALSE])
      cdesc_clean <- cdesc_work[complete_mask, , drop = FALSE]

      dm <- tryCatch(model.matrix(as.formula(f), data = cdesc_clean), error = function(e) NULL)
      if (is.null(dm)) return(div(style = "color: red;", "Cannot validate: design matrix error"))

      result <- tryCatch({
        contrast_list <- as.list(setNames(lines, paste0("C", seq_along(lines))))
        do.call(limma::makeContrasts, c(contrast_list, list(levels = dm)))
        div(style = "color: green;", icon("check"), paste(length(lines), "contrast(s) valid"))
      }, error = function(e) {
        div(style = "color: red;", icon("times"), paste("Invalid contrast:", e$message))
      })

      result
    })


    ## APPLY TO ALL OMES ##
    original_lm_param <- reactiveVal(NULL)

    observeEvent(input$apply_all, {
      if (is.null(selected_ome()) || length(lm_param()) == 0 || is.null(all_omes())) {
        showNotification("Please wait for the application to fully load.", type = "warning", duration = 3)
        updateCheckboxInput(session, "apply_all", value = FALSE)
        return()
      }

      current <- lm_param()
      ome_source <- selected_ome()
      ome_list <- all_omes()

      if (input$apply_all) {
        if (is.null(original_lm_param())) {
          original_lm_param(lm_param())
        }
        if (!is.null(current[[ome_source]])) {
          for (ome in ome_list) {
            if (ome != ome_source) {
              current[[ome]] <- current[[ome_source]]
            }
          }
          lm_param(current)
          showNotification("Applied current settings to all datasets.", type = "message", duration = 3)
        } else {
          showNotification("No parameters set for current dataset.", type = "warning", duration = 3)
          updateCheckboxInput(session, "apply_all", value = FALSE)
        }
      } else {
        if (!is.null(original_lm_param())) {
          lm_param(original_lm_param())
          original_lm_param(NULL)
          showNotification("Reverted to original parameters.", type = "message", duration = 3)
        }
      }
    })


    ## RUN LINEAR MODEL ##
    observeEvent(input$run_lm_button, {
      req(GCTs(), parameters())

      f <- formula_string()
      blocking_var <- NULL
      if (!is.null(input$blocking_variable) && nchar(input$blocking_variable) > 0) {
        blocking_var <- input$blocking_variable
      }
      if (is.null(f) && is.null(blocking_var)) {
        showNotification("Please select at least one variable or a blocking variable.", type = "error", duration = 5)
        return()
      }

      vtypes <- variable_types()
      gcts <- GCTs()

      # Parse interactions
      interactions <- list()
      if (!is.null(input$interaction_terms) && !is.null(input$selected_variables)) {
        vars_list <- input$selected_variables
        if (length(vars_list) >= 2) {
          pairs <- combn(vars_list, 2, simplify = FALSE)
          pair_labels <- sapply(pairs, function(p) paste(p[1], ":", p[2]))
          selected_indices <- which(pair_labels %in% input$interaction_terms)
          interactions <- pairs[selected_indices]
        }
      }

      # Build contrasts list
      contrasts_list <- NULL
      if (isTRUE(input$use_contrasts) && !is.null(input$contrast_strings)) {
        contrast_text <- trimws(input$contrast_strings)
        if (nchar(contrast_text) > 0) {
          lines <- strsplit(contrast_text, "\n")[[1]]
          lines <- trimws(lines)
          lines <- lines[nchar(lines) > 0]
          if (length(lines) > 0) {
            contrasts_list <- setNames(lines, paste0("C", seq_along(lines)))
          }
        }
      }

      # Get intensity param
      intensity_param <- parameters()[[selected_ome()]]$intensity
      if (is.null(intensity_param)) {
        intensity_param <- FALSE
      }

      # Determine omes to run on
      if (isTRUE(input$apply_all)) {
        omes_to_run <- all_omes()
      } else {
        omes_to_run <- selected_ome()
      }

      # Save parameters
      current_params <- lm_param()
      for (ome in omes_to_run) {
        current_params[[ome]] <- list(
          variables = input$selected_variables,
          variable_types = vtypes,
          include_intercept = isTRUE(input$include_intercept),
          interactions = interactions,
          blocking_variable = blocking_var,
          contrasts = contrasts_list,
          formula_string = f,
          stat = "adj.p.val",
          cutoff = 0.05
        )
      }
      lm_param(current_params)

      # Run the model — preserve results from other omes
      test_results <- if (!is.null(lm_results()) && is.list(lm_results())) lm_results() else list()

      withProgress(message = "Fitting linear model", value = 0, {
        for (ome in omes_to_run) {
          incProgress(1 / length(omes_to_run), detail = paste("Processing", ome))

          result <- NULL
          my_shinyalert_tryCatch(
            text.error = paste0("<b>Linear Model Failed for ", ome, ":</b>"),
            append.error = TRUE,
            show.error = TRUE,
            return.error = NULL,
            expr = {
              result <- lm.regression(
                gct = gcts[[ome]],
                formula_string = f,
                variable_types = vtypes,
                blocking_var = blocking_var,
                contrasts_list = contrasts_list,
                intensity = intensity_param
              )
            }
          )

          if (!is.null(result)) {
            test_results[[ome]] <- result
          }
        }
      })

      lm_results(test_results)

      if (length(test_results) > 0) {
        showNotification(
          "Linear model completed successfully! Switching to Results tab...",
          type = "default",
          duration = 5
        )
        # Auto-navigate to Results tab
        if (!is.null(parent)) {
          updateNavbarPage(
            session = parent,
            inputId = "navbar-tabs",
            selected = "LinearModel-Results"
          )
        }
      }
    })


    # Return reactive values for other modules
    return(list(
      lm_params = lm_param,
      lm_results = lm_results
    ))
  })
}
