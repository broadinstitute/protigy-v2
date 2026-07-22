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

    # Structured contrast state: list of rows, each with stable schema
    #   list(
    #     id,             # stable internal key: "C1", "C2", ... - used as the
    #                     # column prefix in the fitted contrasts matrix
    #     type,           # "simple" (1x1) | "multi" (2x2)
    #     num,            # slot A: design-matrix column name (numerator)
    #     den,            # slot B: design-matrix column name (denominator)
    #     num2,           # multi slot C: design-matrix column name
    #     den2,           # multi slot D: design-matrix column name
    #     label,          # human-readable; default auto-generated, editable
    #     label_user_edited  # bool: true means don't auto-regenerate on level change
    #   )
    # simple expr = num - den; multi expr = (num - den) - (num2 - den2).
    # Empty rows are kept and ignored at submit time.
    contrast_rows <- reactiveVal(list())

    # Which design-matrix coefficients the user wants to *display* in Results /
    # Volcano tabs. Display-only filter - does NOT change the fitted model.
    coefficient_selection <- reactiveVal(character(0))

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
        # Layout override: narrow col1 to give col2 more room. Uses custom
        # percentage widths (Bootstrap column(N) only supports N=1..12 on a
        # 12-col grid, which would round col1 too narrow at 1/12 and too wide
        # at 2/12). flex-basis gives precise control.
        tags$style(HTML(
          paste0(
            ".lm-setup-row { display: flex; flex-wrap: wrap; margin-left: -15px; margin-right: -15px; }",
            ".lm-setup-col1 { flex: 0 0 14%;  max-width: 14%;  padding: 0 15px; }",
            ".lm-setup-col2 { flex: 0 0 36%;  max-width: 36%;  padding: 0 15px; }",
            ".lm-setup-col3 { flex: 0 0 50%;  max-width: 50%;  padding: 0 15px; }"
          )
        )),
        tags$div(
          class = "lm-setup-row",
          # Column 1: Ome selector and run button (narrower)
          tags$div(
            class = "lm-setup-col1",
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

          # Column 2: Variable configuration + contrasts (wider)
          tags$div(
            class = "lm-setup-col2",
            div(class = "stat-setup-controls",
              # Model Formula moved to top for prominence - no top padding so
              # the verbatim box top-aligns with col1's selectInput.
              tags$div(style = "padding-top: 0; padding-bottom: 6px;",
                h5(strong("Model Formula:")),
                verbatimTextOutput(ns("formula_display")),
                uiOutput(ns("blocking_formula_annotation"))
              ),
              tags$hr(style = "margin: 10px 0;"),
              tags$div(style = "padding-top: 4px;",
                uiOutput(ns("variable_picker_ui"))
              ),
              tags$div(style = "padding-top: 14px;",
                uiOutput(ns("variable_type_toggles_ui"))
              ),
              tags$div(style = "padding-top: 10px;",
                uiOutput(ns("reference_levels_ui"))
              ),
              tags$div(style = "padding-top: 10px;",
                checkboxInput(ns("include_intercept"),
                              "Include intercept",
                              value = TRUE),
                shinyBS::bsTooltip(
                  ns("include_intercept"),
                  title = paste(
                    "When checked, the model includes an intercept (reference",
                    "level for factors). Uncheck only if you want the model",
                    "to estimate group means directly (~ 0 + variables) \u2014",
                    "useful for building custom contrasts between all levels",
                    "without a reference. Leave checked in most cases."
                  ),
                  placement = "right",
                  trigger = "hover"
                )
              ),
              tags$div(style = "padding-top: 10px;",
                uiOutput(ns("interaction_picker_ui"))
              ),
              tags$div(style = "padding-top: 10px;",
                uiOutput(ns("blocking_var_ui"))
              ),
              tags$div(style = "padding-top: 14px;",
                uiOutput(ns("contrast_builder_ui"))
              )
            )
          ),

          # Column 3: Design matrix preview and coefficient list
          tags$div(
            class = "lm-setup-col3",
            style = "padding-top: 2px;",
            div(
              h5(strong("Design Matrix Preview (first 10 rows):")),
              # Sample-count caption - tells the user when complete.cases() will
              # drop samples and which column(s) caused the drops.
              tags$div(
                style = "font-size: 0.9em; color: #495057; margin-bottom: 6px;",
                textOutput(ns("sample_drop_summary"), inline = TRUE)
              ),
              DT::dataTableOutput(ns("design_matrix_preview")),
              br(),
              uiOutput(ns("coefficient_names_display"))
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
            tags$li("Optionally define custom contrasts using the numerator / denominator dropdowns"),
            tags$li("Review the formula and design matrix preview"),
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
      tagList(
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
        ),
        shinyBS::bsTooltip(
          ns("selected_variables"),
          title = paste(
            "Pick the sample-metadata columns to include as fixed effects",
            "in the model. Each selected column becomes a term in the formula.",
            "Avoid columns that uniquely identify each sample (e.g. sample IDs):",
            "they make the design rank-deficient."
          ),
          placement = "right",
          trigger = "hover"
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

      tagList(
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
        ),
        shinyBS::bsTooltip(
          ns("interaction_terms"),
          title = paste(
            "Add an interaction when you expect the effect of one variable",
            "to depend on the level of another (e.g. \"drug response differs",
            "across genotypes\"). Leave empty if you only need additive",
            "covariate adjustment."
          ),
          placement = "right",
          trigger = "hover"
        )
      )
    })


    ## BLOCKING VARIABLE ##
    output$blocking_var_ui <- renderUI({
      req(cdesc())
      vars <- input$selected_variables
      # Only offer discrete columns not already in the model (as fixed effects or interactions)
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
        shinyBS::bsTooltip(
          ns("blocking_variable"),
          title = paste(
            "Use this for repeated measures: pick the subject identifier",
            "(patient, donor, animal) when the same subject was measured",
            "more than once. Avoid using a batch/plate variable here - ",
            "those are usually covariates, not blocking factors."
          ),
          placement = "right",
          trigger = "hover"
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

      # Parse interaction terms. parse_interaction_terms() is crash-proof when
      # the selection shrinks to a single variable while a stale (non-NULL)
      # interaction_terms value persists -- it never calls combn() with < 2 vars.
      interactions <- parse_interaction_terms(vars, input$interaction_terms)

      build_formula_string(vars, include_intercept, interactions)
    })

    output$formula_display <- renderText({
      f <- formula_string()
      if (is.null(f)) return("(select variables to build formula)")
      f
    })

    # Annotate blocking variable under the formula (informational - not R syntax)
    output$blocking_formula_annotation <- renderUI({
      bv <- input$blocking_variable
      if (is.null(bv) || nchar(bv) == 0) return(NULL)
      div(
        style = "color: #6c757d; font-family: monospace; font-size: 12px; margin-top: 4px;",
        paste0("+ block(", bv, ")   # random effect via duplicateCorrelation")
      )
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


    ## REFERENCE LEVELS (one selectInput per factor variable) ##
    # The default (alphabetical first level) is almost never the right answer
    # for clinical studies. This UI lets the user pick explicitly, which avoids
    # silent coefficient sign flips. We only render this block for variables
    # that are flagged as factors in `variable_types()` AND have >=2 observed
    # levels.
    output$reference_levels_ui <- renderUI({
      vars <- input$selected_variables
      vtypes <- variable_types()
      cd <- cdesc()
      if (is.null(vars) || is.null(cd)) return(NULL)
      factor_vars <- vars[vapply(vars, function(v) {
        identical(vtypes[[v]], "factor") && v %in% colnames(cd)
      }, logical(1))]
      if (length(factor_vars) == 0) return(NULL)
      controls <- lapply(factor_vars, function(v) {
        lv <- levels(factor(cd[[v]]))
        if (length(lv) < 2) return(NULL)
        # Preserve previous selection if still present; otherwise apply the
        # smarter heuristic (control-token match -> modal -> alphabetical
        # fallback) so we don't silently flip coefficient signs.
        prev <- input[[paste0("reflev_", make.names(v))]]
        if (!is.null(prev) && prev %in% lv) {
          sel <- prev
          ann <- ""
        } else {
          pick <- pick_default_reference_level(cd[[v]])
          sel <- if (!is.null(pick$level) && !is.na(pick$level) && pick$level %in% lv) {
            pick$level
          } else {
            lv[1]  # absolute fallback when the helper can't pick (e.g. all NA)
          }
          ann <- format_reference_level_annotation(pick)
        }
        label_html <- if (nzchar(ann)) {
          tagList(
            paste0("Reference level for '", v, "': "),
            tags$span(style = "font-weight: normal; color: #6c757d; font-style: italic; font-size: 0.9em;",
                      ann)
          )
        } else {
          paste0("Reference level for '", v, "':")
        }
        reflev_id <- ns(paste0("reflev_", make.names(v)))
        tagList(
          selectInput(
            reflev_id,
            label = label_html,
            choices = lv,
            selected = sel
          ),
          shinyBS::bsTooltip(
            reflev_id,
            title = paste(
              "The baseline level for this factor. Other levels are reported",
              "relative to the reference (e.g. for Treatment with reference",
              "\"Vehicle\", the coefficient \"TreatmentDrug\" reads as",
              "Drug - Vehicle). Pick the biologically meaningful baseline."
            ),
            placement = "right",
            trigger = "hover"
          )
        )
      })
      controls <- controls[!vapply(controls, is.null, logical(1))]
      if (length(controls) == 0) return(NULL)
      # Reactive caveat: when the intercept is off, the first factor is cell-means
      # coded (its reference is inert) while other factors stay reference-coded.
      # Reading input$include_intercept here keeps the note live with the toggle.
      intercept_note <- reference_level_intercept_note(
        include_intercept = isTRUE(input$include_intercept),
        n_factors = length(controls)
      )
      note_ui <- if (nzchar(intercept_note)) {
        tags$div(
          style = paste(
            "font-size: 0.9em; color: #8a6d3b; background: #fcf8e3;",
            "border: 1px solid #faebcc; border-radius: 4px;",
            "padding: 6px 8px; margin-bottom: 6px;"
          ),
          intercept_note
        )
      } else {
        NULL
      }
      tagList(
        h5(strong("Reference levels:")),
        tags$div(style = "font-size: 0.9em; color: #666; margin-bottom: 6px;",
                 "Pick which level should be the baseline for each factor. ",
                 "Other levels are measured relative to this one."),
        note_ui,
        controls
      )
    })


    reference_levels <- reactive({
      vars <- input$selected_variables
      vtypes <- variable_types()
      if (is.null(vars)) return(list())
      out <- list()
      for (v in vars) {
        if (!identical(vtypes[[v]], "factor")) next
        val <- input[[paste0("reflev_", make.names(v))]]
        if (!is.null(val) && nzchar(as.character(val))) {
          out[[v]] <- as.character(val)
        }
      }
      out
    })


    ## SHARED DESIGN REACTIVE ##
    # Single source of truth for the design matrix, shared with the actual fit:
    # both the preview and lm.regression() call build_lm_design(), so what the
    # user previews is exactly what will be fit (same complete-case dropping
    # over formula + blocking vars, same single-level guard, same rank check,
    # same empty-formula + blocking repeated-measures support). Returns the full
    # structured result: $design, $cdesc_clean, $n_used/$n_total/$dropped,
    # $repeated_measures_only, $warnings, $error.
    design_built <- reactive({
      f <- formula_string()
      req(cdesc())
      bv <- input$blocking_variable
      if (!is.null(bv) && !nzchar(bv)) bv <- NULL

      build_lm_design(
        cdesc = cdesc(),
        formula_string = if (is.null(f)) "" else f,
        variable_types = variable_types(),
        reference_levels = reference_levels(),
        blocking_var = bv
      )
    })

    # Design matrix (or NULL on error) - kept for callers that only need the
    # matrix (coefficient attribution, contrast dropdowns).
    design_matrix <- reactive({
      design_built()$design
    })

    # Sorted coefficient names for contrast dropdowns
    design_coefs <- reactive({
      dm <- design_matrix()
      if (is.null(dm)) return(character(0))
      sort(colnames(dm))
    })


    ## SAMPLE DROP SUMMARY ##
    # Renders "Using N of M samples (K dropped ...)" above the design preview so
    # users notice when complete.cases() filtering reduces the effective sample
    # size. Uses the SAME drop accounting the shared builder / fit apply.
    output$sample_drop_summary <- renderText({
      cd <- cdesc()
      f <- formula_string()
      bv <- input$blocking_variable
      if (is.null(cd)) return("")
      # Variables that feed the complete.cases filter inside build_lm_design()
      # and lm.regression(): all formula vars plus the blocking variable (if any).
      vars <- character(0)
      if (!is.null(f) && nzchar(f)) {
        vars <- tryCatch(all.vars(as.formula(f)), error = function(e) character(0))
      }
      if (!is.null(bv) && nzchar(bv)) vars <- unique(c(vars, bv))
      if (length(vars) == 0) return("")
      summary <- summarize_sample_drops(cd, vars)
      if (is.null(summary)) return("")
      summary$message
    })


    ## DESIGN MATRIX PREVIEW ##
    # Faithful to the fit: on a fatal config the preview shows the SAME error the
    # fit would stop() with (e.g. a factor collapsing to one level), and it
    # surfaces non-fatal notices (rank deficiency, degenerate blocking) instead
    # of hiding them. A valid empty-formula + blocking config previews its
    # intercept-only design rather than "Could not build design matrix".
    output$design_matrix_preview <- DT::renderDataTable({
      built <- design_built()

      if (!is.null(built$error)) {
        return(DT::datatable(
          data.frame(Error = built$error),
          options = list(dom = "t")
        ))
      }

      dm <- built$design
      preview <- as.data.frame(dm[seq_len(min(10, nrow(dm))), , drop = FALSE])
      caption <- NULL
      if (length(built$warnings) > 0) {
        caption <- htmltools::tags$caption(
          style = "caption-side: top; color: #b45309; font-size: 90%;",
          paste(built$warnings, collapse = " ")
        )
      }
      DT::datatable(
        preview,
        caption = caption,
        options = list(dom = "t", scrollX = TRUE, pageLength = 10)
      )
    })


    ## COEFFICIENT NAMES DISPLAY (2-column grid with checkboxes) ##
    # Reset the selection whenever the set of design coefficients changes, so
    # newly-added coefficients default to "checked".
    observeEvent(design_coefs(), {
      coefficient_selection(design_coefs())
    }, ignoreNULL = FALSE)

    # Re-render trigger so Select/Clear-all can force a checkbox rebuild without
    # causing the per-checkbox-sync observer to loop.
    coef_ui_nonce <- reactiveVal(0)

    output$coefficient_names_display <- renderUI({
      coefs <- design_coefs()
      if (length(coefs) == 0) return(NULL)
      # Depend on the nonce so Select/Clear-all forces a re-render.
      coef_ui_nonce()

      selected <- isolate(coefficient_selection())

      cells <- lapply(coefs, function(cf) {
        cb_id <- paste0("coef_cb_", make.names(cf))
        # checkboxInput's default wrapper spreads the checkbox far from its
        # label, so we use the coefficient name AS the checkbox's label and
        # style the input for monospace / dark text.
        tags$div(
          class = "coef-checkbox-row",
          checkboxInput(ns(cb_id), label = cf, value = cf %in% selected)
        )
      })

      tagList(
        # Tighten checkboxInput wrapper so the checkbox sits next to its label
        # text (bootstrap's default .form-group .checkbox has a large bottom
        # margin and label uses padding-left 20px which wastes space).
        tags$style(HTML(
          ".coef-checkbox-row .form-group { margin-bottom: 0 !important; }
           .coef-checkbox-row .checkbox { margin: 0 !important; min-height: 0 !important; }
           .coef-checkbox-row .checkbox label {
             padding-left: 22px !important;
             min-height: 0 !important;
             font-family: monospace;
             color: #212529;
             font-weight: normal;
             word-break: break-word;
             display: inline-block;
             line-height: 1.2;
           }
           .coef-checkbox-row .checkbox label > input[type='checkbox'] {
             margin-left: -22px !important;
             margin-top: 3px !important;
           }"
        )),
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          h5(strong("Model Coefficients:"), style = "margin: 0;"),
          div(
            style = "display: flex; gap: 6px;",
            actionButton(ns("select_all_coefs"), "Select all",
                         class = "btn btn-sm btn-default"),
            actionButton(ns("clear_all_coefs"), "Clear all",
                         class = "btn btn-sm btn-danger")
          )
        ),
        helpText(
          "Checked coefficients appear in the Results and Volcano tabs. ",
          "Unchecking hides them from the report but does NOT refit the model \u2014 ",
          "to remove a term from the model, uncheck it in 'Select model variables' above."
        ),
        div(
          style = paste(
            "display: grid;",
            "grid-template-columns: 1fr 1fr;",
            "gap: 4px 16px;",
            "margin-top: 6px;"
          ),
          cells
        )
      )
    })

    # Sync checkbox state into coefficient_selection (per user click).
    observe({
      coefs <- design_coefs()
      if (length(coefs) == 0) return()
      checked <- vapply(coefs, function(cf) {
        val <- input[[paste0("coef_cb_", make.names(cf))]]
        isTRUE(val)
      }, logical(1))
      new_sel <- coefs[checked]
      current <- isolate(coefficient_selection())
      if (!identical(sort(new_sel), sort(current))) {
        coefficient_selection(new_sel)
      }
    })

    # Select all / Clear all: update selection state AND bump the nonce so the
    # checkbox list re-renders with new `value=` attributes.
    observeEvent(input$select_all_coefs, {
      coefficient_selection(design_coefs())
      coef_ui_nonce(coef_ui_nonce() + 1)
    })
    observeEvent(input$clear_all_coefs, {
      coefficient_selection(character(0))
      coef_ui_nonce(coef_ui_nonce() + 1)
    })


    ## STRUCTURED CONTRAST BUILDER (card-based) ##
    output$contrast_builder_ui <- renderUI({
      tagList(
        tags$style(HTML(
          ".lm-contrast-card {
             border: 1px solid #d0d7de;
             border-radius: 6px;
             padding: 10px 12px;
             margin-bottom: 10px;
             background-color: #fafbfc;
           }
           .lm-contrast-card.invalid { border-color: #d9534f; background-color: #fdf2f2; }
           .lm-contrast-card.valid   { border-color: #5cb85c; }
           .lm-contrast-card .form-group { margin-bottom: 6px !important; }
           .lm-contrast-card .selectize-control { margin-bottom: 0 !important; }
           .lm-contrast-card .badge-id {
             display: inline-block;
             background-color: #6c757d;
             color: white;
             padding: 2px 8px;
             border-radius: 4px;
             font-family: monospace;
             font-weight: bold;
             font-size: 12px;
           }
           .lm-contrast-card .type-radio .radio-inline { margin-left: 10px; }
           .lm-contrast-card .expr-preview {
             font-family: monospace; color: #495057; font-size: 11px;
             padding: 2px 6px; background: #f0f0f0; border-radius: 3px;
             margin-top: 4px; word-break: break-all;
           }
           .lm-contrast-card .validation-msg {
             font-size: 12px; margin-top: 4px;
           }
           .lm-contrast-card .validation-msg.ok { color: #28a745; }
           .lm-contrast-card .validation-msg.err { color: #d9534f; }"
        )),
        h5(strong("Custom Contrasts:")),
        helpText(
          "Each contrast card builds one linear combination of model coefficients, ",
          "tested AFTER fitting. Contrasts add columns to the results keyed by your ",
          "editable label (e.g. \"Drug-Vehicle\"). Use ",
          tags$b("Single coef (1x1)"), " for a one-coefficient-vs-one-coefficient ",
          "difference, or ", tags$b("Multi coef (2x2)"),
          " to build a difference-of-differences (interaction) contrast ",
          tags$code("(A - B) - (C - D)"), " from four coefficient dropdowns. ",
          "Empty cards are ignored at run time."
        ),
        uiOutput(ns("contrast_rows_ui")),
        div(
          style = "display: flex; justify-content: flex-start; margin-top: 10px; gap: 16px; flex-wrap: wrap;",
          actionButton(ns("add_contrast"), "+ Add contrast",
                       class = "btn btn-sm btn-default"),
          actionButton(ns("clear_contrasts"), "Clear all",
                       class = "btn btn-sm btn-danger")
        ),
        uiOutput(ns("contrast_validation_summary"))
      )
    })

    # Contrast row state helpers (internal) ------------------------------------
    # Seed one empty simple row on first render so the UI always shows a card.
    observe({
      if (length(contrast_rows()) == 0) {
        contrast_rows(list(list(
          id = new_contrast_row_id(),
          type = "simple",
          num = "",
          den = "",
          num2 = "",
          den2 = "",
          label = "",
          label_user_edited = FALSE
        )))
      }
    })

    # Fresh start when the set of model variables changes. Contrast rows store
    # design-coefficient names (e.g. "SubgroupB"); changing which variables are
    # selected invalidates those coefficients, so stale num/den references would
    # point at columns that no longer exist. Reseed to a single empty row and
    # clear the (now stale) interaction selection, matching the "Clear all"
    # state, so the user rebuilds contrasts against the new design. ignoreInit
    # avoids wiping anything on module startup / first selection.
    observeEvent(input$selected_variables, {
      contrast_rows(list(list(
        id = new_contrast_row_id(),
        type = "simple",
        num = "",
        den = "",
        num2 = "",
        den2 = "",
        label = "",
        label_user_edited = FALSE
      )))
      # Drop the stale interaction picker selection so formula_string() and the
      # fit never see an interaction referencing a removed variable.
      shinyWidgets::updatePickerInput(session, "interaction_terms",
                                      selected = character(0))
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    # Helper to stably derive the display id (C1, C2, ...) from position
    display_ids <- function(rows) {
      if (length(rows) == 0) return(character(0))
      paste0("C", seq_along(rows))
    }

    # Render all contrast cards
    output$contrast_rows_ui <- renderUI({
      rows <- contrast_rows()
      coefs <- design_coefs()
      if (length(rows) == 0) {
        return(helpText("No contrasts defined. Click '+ Add contrast' to add one."))
      }

      # Simple num/den dropdowns exclude "(Intercept)": subtracting or ratioing
      # the grand-mean term against a group coefficient is not a meaningful
      # between-group contrast. Advanced mode still sees the full `coefs` set
      # (its validator must match the real design, intercept included).
      simple_coefs <- setdiff(coefs, "(Intercept)")
      choices <- c(setNames("", "\u2014 choose \u2014"),
                   setNames(simple_coefs, simple_coefs))
      dids <- display_ids(rows)

      cards <- lapply(seq_along(rows), function(i) {
        r <- rows[[i]]
        did <- dids[i]
        # Per-row input ids
        type_id  <- paste0("type_", r$id)
        num_id   <- paste0("num_",  r$id)
        den_id   <- paste0("den_",  r$id)
        num2_id  <- paste0("num2_", r$id)
        den2_id  <- paste0("den2_", r$id)
        swap_id  <- paste0("swap_", r$id)
        label_id <- paste0("label_", r$id)
        rm_id    <- paste0("rm_",    r$id)

        # A single labelled coefficient dropdown, reused across slots.
        coef_dropdown <- function(input_id, lbl_text, selected_val) {
          tags$div(
            tags$label(lbl_text, style = "font-size: 12px; color: #6c757d; margin-bottom: 2px;"),
            selectizeInput(ns(input_id), label = NULL,
                           choices = choices, selected = selected_val %||% "",
                           options = list(placeholder = "choose coefficient"),
                           width = "100%")
          )
        }
        minus_glyph <- function() {
          tags$div(style = "text-align: center; font-weight: bold; font-size: 16px; padding-top: 18px;", "\u2212")
        }

        # Build type-specific panel + preview expr + validation
        if (identical(r$type, "multi")) {
          # Multi (2x2) panel: four dropdowns wired into (A - B) - (C - D).
          slot_panel <- tagList(
            tags$div(
              style = "font-size: 12px; color: #6c757d; margin-bottom: 4px;",
              "( A \u2212 B ) \u2212 ( C \u2212 D )  \u2014  difference of differences"
            ),
            div(
              style = paste(
                "display: grid;",
                "grid-template-columns: 1fr 24px 1fr;",
                "align-items: center; column-gap: 8px; margin-bottom: 6px;"
              ),
              coef_dropdown(num_id,  "A", r$num),
              minus_glyph(),
              coef_dropdown(den_id,  "B", r$den)
            ),
            tags$div(style = "text-align: center; font-weight: bold; font-size: 16px; margin: 2px 0;", "\u2212"),
            div(
              style = paste(
                "display: grid;",
                "grid-template-columns: 1fr 24px 1fr;",
                "align-items: center; column-gap: 8px;"
              ),
              coef_dropdown(num2_id, "C", r$num2),
              minus_glyph(),
              coef_dropdown(den2_id, "D", r$den2)
            )
          )
          expr_for_preview <- build_multi_expr(r$num %||% "", r$den %||% "",
                                                r$num2 %||% "", r$den2 %||% "")
          slots <- c(r$num %||% "", r$den %||% "", r$num2 %||% "", r$den2 %||% "")
          if (any(!nzchar(slots))) {
            validation <- list(ok = FALSE, message = "(choose all four coefficients A, B, C, D)",
                               unknown = character(0))
          } else {
            validation <- validate_advanced_expr(expr_for_preview, coefs)
          }
        } else {
          # Single (1x1) panel: numerator / denominator dropdowns + swap
          slot_panel <- div(
            style = paste(
              "display: grid;",
              "grid-template-columns: 1fr 24px 1fr 40px;",
              "align-items: center;",
              "column-gap: 8px;"
            ),
            coef_dropdown(num_id, "Numerator", r$num),
            minus_glyph(),
            coef_dropdown(den_id, "Denominator", r$den),
            tags$div(
              style = "text-align: center; padding-top: 18px;",
              actionButton(ns(swap_id), label = NULL,
                           icon = icon("exchange-alt"),
                           title = "Swap numerator and denominator",
                           class = "btn btn-sm btn-default")
            )
          )
          expr_for_preview <- build_simple_expr(r$num %||% "", r$den %||% "")
          if (!nzchar(r$num %||% "") || !nzchar(r$den %||% "")) {
            validation <- list(ok = FALSE, message = "(choose numerator and denominator)",
                               unknown = character(0))
          } else if (identical(r$num, r$den)) {
            validation <- list(ok = FALSE, message = "numerator and denominator are identical (zero contrast)",
                               unknown = character(0))
          } else {
            validation <- validate_advanced_expr(expr_for_preview, coefs)
          }
        }

        # Compute effective label (auto or user-edited)
        effective_label <- if (isTRUE(r$label_user_edited) && nzchar(r$label %||% "")) {
          r$label
        } else if (identical(r$type, "multi")) {
          make_multi_label(r$num %||% "", r$den %||% "", r$num2 %||% "", r$den2 %||% "")
        } else {
          make_simple_label(r$num %||% "", r$den %||% "")
        }

        card_class <- if (validation$ok) "lm-contrast-card valid" else "lm-contrast-card invalid"

        tags$div(
          class = card_class,
          # Header row: id badge, type radio, remove button
          div(
            style = "display: flex; align-items: center; gap: 12px; margin-bottom: 8px;",
            tags$span(class = "badge-id", did),
            tags$div(
              class = "type-radio",
              style = "margin-bottom: 0;",
              radioButtons(ns(type_id), label = NULL,
                           choices = c("Single coef (1x1)" = "simple",
                                       "Multi coef (2x2)" = "multi"),
                           selected = r$type %||% "simple",
                           inline = TRUE)
            ),
            tags$div(
              style = "margin-left: auto;",
              actionButton(ns(rm_id), label = NULL, icon = icon("times"),
                           title = "Remove this contrast",
                           class = "btn btn-sm btn-default")
            )
          ),

          # Type-specific body
          slot_panel,

          # Label input (always editable)
          div(
            style = "margin-top: 8px;",
            tags$label(
              tags$span(style = "font-weight: 600; font-size: 12px;", "Label:"),
              tags$span(style = "font-size: 11px; color: #6c757d; margin-left: 6px;",
                        "(used as CSV column prefix; edit to override auto-generated)")
            ),
            textInput(ns(label_id), label = NULL,
                      value = effective_label,
                      width = "100%",
                      placeholder = "auto-generated from numerator/denominator")
          ),

          # Expr preview
          if (nzchar(expr_for_preview)) div(class = "expr-preview",
                                             tags$span(style = "color: #6c757d;", "expr: "),
                                             expr_for_preview),

          # Validation message
          div(
            class = paste0("validation-msg ", if (validation$ok) "ok" else "err"),
            if (validation$ok) {
              tagList(icon("check-circle"), " ", validation$message)
            } else {
              tagList(icon("exclamation-circle"), " ", validation$message)
            }
          )
        )
      })

      do.call(tagList, cards)
    })

    # Persist per-row edits back into contrast_rows state. Also handles the
    # "user edited the label" flag: if the current label input differs from
    # what would be auto-generated, mark label_user_edited = TRUE.
    observe({
      rows <- contrast_rows()
      if (length(rows) == 0) return()
      changed <- FALSE
      new_rows <- lapply(rows, function(r) {
        # Type radio. On an actual mode switch, clear ALL slots (both modes
        # share num/den; multi adds num2/den2) so no stale value carries across
        # the Single<->Multi shape change.
        type_val <- input[[paste0("type_", r$id)]]
        if (!is.null(type_val) && !identical(type_val, r$type)) {
          r$type <- type_val
          r$num <- ""; r$den <- ""; r$num2 <- ""; r$den2 <- ""
          if (!isTRUE(r$label_user_edited)) r$label <- ""
          changed <<- TRUE
          # Skip reading the (now-stale) slot inputs this cycle; the re-render
          # will repopulate empty dropdowns.
          return(r)
        }
        # Slot A/B (both modes)
        num_val <- input[[paste0("num_", r$id)]]
        den_val <- input[[paste0("den_", r$id)]]
        if (!is.null(num_val) && !identical(num_val, r$num)) {
          r$num <- num_val; changed <<- TRUE
        }
        if (!is.null(den_val) && !identical(den_val, r$den)) {
          r$den <- den_val; changed <<- TRUE
        }
        # Slot C/D (multi only)
        if (identical(r$type, "multi")) {
          num2_val <- input[[paste0("num2_", r$id)]]
          den2_val <- input[[paste0("den2_", r$id)]]
          if (!is.null(num2_val) && !identical(num2_val, r$num2)) {
            r$num2 <- num2_val; changed <<- TRUE
          }
          if (!is.null(den2_val) && !identical(den2_val, r$den2)) {
            r$den2 <- den2_val; changed <<- TRUE
          }
        }
        # Label input
        label_val <- input[[paste0("label_", r$id)]]
        if (!is.null(label_val)) {
          label_clean <- sanitize_label(label_val)
          auto_label <- if (identical(r$type, "multi")) {
            make_multi_label(r$num %||% "", r$den %||% "", r$num2 %||% "", r$den2 %||% "")
          } else {
            make_simple_label(r$num %||% "", r$den %||% "")
          }
          if (!identical(label_clean, r$label %||% "")) {
            r$label <- label_clean
            r$label_user_edited <- !identical(label_clean, auto_label)
            changed <<- TRUE
          }
        }
        r
      })
      if (changed) contrast_rows(new_rows)
    })

    # Add Simple contrast card
    observeEvent(input$add_contrast, {
      contrast_rows(c(contrast_rows(),
                      list(list(id = new_contrast_row_id(),
                                type = "simple",
                                num = "", den = "",
                                num2 = "", den2 = "",
                                label = "",
                                label_user_edited = FALSE))))
    })

    # Clear all contrast rows (seed one empty simple row to keep UI showing)
    observeEvent(input$clear_contrasts, {
      contrast_rows(list(list(id = new_contrast_row_id(),
                              type = "simple",
                              num = "", den = "",
                              num2 = "", den2 = "",
                              label = "",
                              label_user_edited = FALSE)))
    })

    # Swap numerator <-> denominator for a simple row
    observe({
      rows <- contrast_rows()
      lapply(rows, function(r) {
        swap_id <- paste0("swap_", r$id)
        observeEvent(input[[swap_id]], {
          current <- contrast_rows()
          current <- lapply(current, function(x) {
            if (x$id == r$id && identical(x$type, "simple")) {
              tmp <- x$num; x$num <- x$den; x$den <- tmp
              # Regenerate label if user hadn't overridden it
              if (!isTRUE(x$label_user_edited)) {
                x$label <- make_simple_label(x$num %||% "", x$den %||% "")
              }
            }
            x
          })
          contrast_rows(current)
        }, ignoreInit = TRUE, once = TRUE)
      })
    })

    # Remove a specific row - delegate via observer across all current row ids
    observe({
      rows <- contrast_rows()
      lapply(rows, function(r) {
        rm_id <- paste0("rm_", r$id)
        observeEvent(input[[rm_id]], {
          current <- contrast_rows()
          current <- current[vapply(current, function(x) x$id != r$id, logical(1))]
          contrast_rows(current)
        }, ignoreInit = TRUE, once = TRUE)
      })
    })

    # Build list(id, label, expr) specs from the current rows.
    # - id: stable internal key "C1", "C2", ...
    # - label: auto-generated (or user-overridden), whitespace-stripped
    # - expr: limma contrast string ready for makeContrasts
    # Empty/invalid rows are dropped.
    contrast_specs <- reactive({
      rows <- contrast_rows()
      if (length(rows) == 0) return(list())
      dids <- display_ids(rows)
      specs <- lapply(seq_along(rows), function(i) {
        r <- rows[[i]]
        did <- dids[i]
        if (identical(r$type, "multi")) {
          slots <- c(r$num %||% "", r$den %||% "", r$num2 %||% "", r$den2 %||% "")
          if (any(!nzchar(slots))) return(NULL)
          e <- build_multi_expr(r$num, r$den, r$num2, r$den2)
          lbl <- if (isTRUE(r$label_user_edited) && nzchar(r$label %||% ""))
                    r$label else make_multi_label(r$num, r$den, r$num2, r$den2)
          list(id = did, label = lbl, expr = e, type = "multi")
        } else {
          if (!nzchar(r$num %||% "") || !nzchar(r$den %||% "")) return(NULL)
          if (identical(r$num, r$den)) return(NULL)
          e <- build_simple_expr(r$num, r$den)
          lbl <- if (isTRUE(r$label_user_edited) && nzchar(r$label %||% ""))
                    r$label else make_simple_label(r$num, r$den)
          list(id = did, label = lbl, expr = e, type = "simple")
        }
      })
      Filter(Negate(is.null), specs)
    })

    # Live validation summary: build contrast matrix against current design.
    # Mirrors the backend make.names() path in lm.regression().
    output$contrast_validation_summary <- renderUI({
      specs <- contrast_specs()
      dm <- design_matrix()
      if (is.null(dm)) return(div(style = "color: red;", "Cannot validate: design matrix unavailable"))
      if (length(specs) == 0) {
        return(helpText("Add at least one valid contrast to enable the Run button."))
      }
      tryCatch({
        dm_safe <- dm
        colnames(dm_safe) <- make.names(colnames(dm))
        # Rename each expr's tokens via make.names() for the validation pass
        strs_safe <- vapply(specs, function(s) {
          parts <- strsplit(s$expr, "(?=[-+*/() ])|(?<=[-+*/() ])", perl = TRUE)[[1]]
          renamed <- vapply(parts, function(tok) {
            if (nzchar(tok) && !grepl("^[-+*/() ]+$", tok) &&
                suppressWarnings(is.na(as.numeric(tok)))) {
              make.names(tok)
            } else tok
          }, character(1))
          paste(renamed, collapse = "")
        }, character(1))
        contrast_list <- as.list(setNames(strs_safe, vapply(specs, `[[`, character(1), "id")))
        do.call(limma::makeContrasts, c(contrast_list, list(levels = dm_safe)))
        div(
          style = "margin-top: 8px;",
          div(style = "color: green; font-weight: 600;",
              icon("check"),
              paste(length(specs), "contrast(s) valid and ready to run"))
        )
      }, error = function(e) {
        div(
          style = "margin-top: 8px;",
          div(style = "color: red; font-weight: 600;",
              icon("times"),
              paste("Invalid contrast matrix:", e$message))
        )
      })
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
      if ((is.null(f) || !nzchar(f)) && is.null(blocking_var)) {
        showNotification("Please select at least one variable or a blocking variable.", type = "error", duration = 5)
        return()
      }

      vtypes <- variable_types()
      gcts <- GCTs()

      # Parse interactions (shares the crash-proof helper with formula_string()
      # so the fit's interaction set can never diverge from the previewed one).
      interactions <- parse_interaction_terms(
        input$selected_variables, input$interaction_terms
      )

      # Build contrasts list from structured state (empty rows ignored).
      # Use the user-facing, whitespace-stripped `label` as the column key so
      # downstream CSV/XLSX headers are readable (e.g. "Drug-Vehicle.logFC").
      # Labels are guaranteed unique via make.unique() in case two cards
      # collide (e.g. user edited both to the same name).
      contrasts_list <- NULL
      contrast_meta  <- NULL
      specs <- contrast_specs()
      if (length(specs) > 0) {
        exprs  <- vapply(specs, `[[`, character(1), "expr")
        labels <- vapply(specs, `[[`, character(1), "label")
        ids    <- vapply(specs, `[[`, character(1), "id")
        types  <- vapply(specs, `[[`, character(1), "type")
        # Ensure uniqueness - same label twice would collapse columns silently
        labels_unique <- make.unique(labels, sep = "_")
        contrasts_list <- setNames(exprs, labels_unique)
        # Full metadata for the export layer (workflow_table, JSON)
        contrast_meta <- lapply(seq_along(specs), function(i) {
          list(id = ids[i], label = labels_unique[i], expr = exprs[i], type = types[i])
        })
      }

      # Determine omes to run on. The per-ome `intensity` flag is read INSIDE
      # the fit loop below so apply-to-all uses each ome's own flag.
      if (isTRUE(input$apply_all)) {
        omes_to_run <- all_omes()
      } else {
        omes_to_run <- selected_ome()
      }

      # Snapshot the design-coefficient filter at run-time. `display_coefficients`
      # is the subset the user chose to see in Results / Volcano; `all_design_coefs`
      # records the full set at run-time so we can derive what to hide later
      # (contrast columns like C1, C2 are NOT in all_design_coefs and remain visible).
      all_design_coefs_snapshot <- design_coefs()
      display_coefficients_snapshot <- intersect(coefficient_selection(), all_design_coefs_snapshot)

      # Save parameters. The actual `intensity` flag is recorded per-ome below
      # so the workflow params table and downstream JSON reflect what was used.
      ref_levels <- reference_levels()
      current_params <- lm_param()
      for (ome in omes_to_run) {
        current_params[[ome]] <- list(
          variables = input$selected_variables,
          variable_types = vtypes,
          reference_levels = ref_levels,
          include_intercept = isTRUE(input$include_intercept),
          interactions = interactions,
          blocking_variable = blocking_var,
          contrasts = contrasts_list,
          contrast_meta = contrast_meta,
          formula_string = f,
          stat = "adj.p.val",
          cutoff = 0.05,
          all_design_coefs = all_design_coefs_snapshot,
          display_coefficients = display_coefficients_snapshot,
          intensity = pick_intensity_for_ome(parameters(), ome)
        )
      }
      lm_param(current_params)

      # Run the model - preserve results from other omes
      test_results <- if (!is.null(lm_results()) && is.list(lm_results())) lm_results() else list()

      withProgress(message = "Fitting linear model", value = 0, {
        for (ome in omes_to_run) {
          incProgress(1 / length(omes_to_run), detail = paste("Processing", ome))

          # Read the intensity flag for THIS ome (apply-to-all must not leak
          # the selected ome's flag onto other omes - reviewer section 2.4).
          ome_intensity <- pick_intensity_for_ome(parameters(), ome)

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
                intensity = ome_intensity,
                reference_levels = ref_levels
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


    # Expose internals for shinytest2 introspection. `exportTestValues` only
    # adds an output binding read by `app$get_values()$export`; production code
    # paths are unaffected. Gated by the existing shinytest2 driver.
    shiny::exportTestValues(
      lm_results = lm_results(),
      lm_params  = lm_param()
    )

    # Return reactive values for other modules
    return(list(
      lm_params = lm_param,
      lm_results = lm_results
    ))
  })
}


# Contrast-row id helpers moved to R/tab_lm_setup_helpers_contrasts.R
# (see `new_contrast_row_id`).
