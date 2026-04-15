################################################################################
# Module: Stat_Plot
#
# Allow users to see the Volcano plot of their results
################################################################################

################################################################################
# Shiny functions (UI and server)
################################################################################


# UI for the statPlot tab
# contains the structure for the big tabbed box with omes
statPlot_Tab_UI <- function(id = "statPlotTab") {
  ns <- NS(id) # namespace function, wrap UI inputId's with this `ns("inputId")`
  
  tagList(
    
    # display omes tabs
    fluidRow(uiOutput(ns("ome_tabset_box")))
    
  ) # end tagList
}

# server for the statPlot tab
# contains the structure for the big tabbed box with omes
statPlot_Tab_Server <- function(id = "statPlotTab",
                                   GCTs_and_params, 
                                   globals,
                                   stat_results,
                                   stat_params) { 
 
  ## module function
  moduleServer(id, function (input, output, session) {
    
    ## GATHERING INPUTS ##
    
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
    all_omes <- reactive(names(GCTs())) # don't remove
    
    # gather relevant variables from globals
    default_ome <- reactive(globals$default_ome) # don't remove this variable!
    custom_colors <- reactive(globals$colors)
    
    # Check if statistical results exist
    stat_results_check <- reactive({
      validate(need(stat_results(), "Statistical testing not yet run."))
      stat_results()
    })
    
    ## OME TABS ##
    
    # handles compiling ome tabs into styled tabset box
    output$ome_tabset_box <- renderUI({
      # This will trigger the validate() statements and show "GCTs not yet processed"
      req(GCTs(), parameters())
      req(stat_results_check())  # stop if these reactiveVals don’t exist
      req(all_omes(), default_ome())
      
      # generate a tab for each -ome
      tabs <- lapply(all_omes(), function(ome){
        tabPanel(
          title = ome,
          
          # call the UI function for each individual ome
          statPlot_Ome_UI(id = ns(ome), ome = ome)
          
        ) # end tabPanel
      }) # end lapply
      
      # combine all tabs into tabSetPanel
      tab_set_panel <- do.call(
        tabsetPanel, 
        c(tabs, list(id = ns("ome_tabs"), selected = isolate(default_ome())))
      )
      
      # put everything in a big box with ome tabs and return
      # add necessary CSS classes
      add_css_attributes(
        shinydashboardPlus::box(
          tab_set_panel,
          width = 12
        ), 
        classes = c("box-no-header", "box-with-tabs")
      )
    }) # end renderUI
    
    # update selected tab based on default dataset
    observe({
      updateTabsetPanel(inputId = "ome_tabs", selected = default_ome())
    })
    
    # Shared reactiveVal: TRUE when any ome has "Across all omes" union toggled on.
    # Written by individual ome modules; read by all ome modules to compute global union.
    global_union_rv <- reactiveVal(FALSE)

    # call the server function for each individual ome
    all_plots <- reactiveVal() # initialize
    observeEvent(all_omes(), {
      output_plots <- sapply(all_omes(), function(ome) {
        statPlot_Ome_Server(
          # TODO: edit inputs to the ome server function, the last 4 may be unnecessary
          id = ome,
          ome = ome,
          GCT_processed = reactive(GCTs()[[ome]]),
          parameters = reactive(parameters()[[ome]]),
          default_annotation_column = reactive(default_annotations()[[ome]]),
          color_map = reactive(custom_colors()[[ome]]),
          stat_params = stat_params,
          stat_results = stat_results,
          global_union_rv = global_union_rv
        )
      }, simplify = FALSE)
      
      all_plots(output_plots) # set reactive value with outputs
    })
    
    return(all_plots)
  })
}



# UI for an individual ome
statPlot_Ome_UI <- function (id, ome) {
  
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("ome_plot_contents"))
  )
}


# server for an individual ome
statPlot_Ome_Server <- function(id,
                                   ome,
                                   GCT_processed,
                                   parameters,
                                   default_annotation_column,
                                   color_map,
                                   stat_params,
                                   stat_results,
                                   global_union_rv = NULL) {
  
  ## module function
  moduleServer(id, function (input, output, session) {
    
    # get namespace, use in renderUI-like functions
    ns <- session$ns

    ## PROTEIN SEARCH & LABELING ################################################
    # proteins_of_interest: per-ome reactiveVal storing feature IDs the user has
    # selected via search or by clicking on the plot.
    # hidden_label_count: count of labels suppressed by the overlap-avoidance algo.
    proteins_of_interest <- reactiveVal(character(0))
    hidden_label_count   <- reactiveVal(0L)

    # union_mode: "none" | "ome" | "global"
    # Driven by the two mutually-exclusive checkboxes in the sidebar.
    union_mode <- reactive({
      if (isTRUE(input$label_union_global)) return("global")
      if (isTRUE(input$label_union_ome))    return("ome")
      "none"
    })

    # global_union_ids: union of labeled IDs from every ome when "Across all omes" is on.
    global_union_ids <- reactive({
      req(union_mode() == "global", stat_results(), stat_params())
      all_omes <- names(stat_results())
      Reduce(union, lapply(all_omes, function(o) {
        sp <- stat_params()[[o]]
        sr <- stat_results()[[o]]
        if (is.null(sp) || is.null(sr)) return(character(0))
        volcano_label_union_for_ome(sr, sp, input$label_mode, proteins_of_interest())
      }), init = character(0))
    })

    output$ome_plot_contents <- renderUI({
      # fallback if stat_results not defined yet
      req(stat_params())
      
      test <- stat_params()[[ome]]$test
      
      if (is.null(test) || test == "None") {
        return(h4("No test selected to run on this dataset."))
      }
      
      if (test == "Moderated F test") {
        return(h4("No volcano plot for the Moderated F test."))
      }
      
      tagList(
        # Volcano plot + controls side by side
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
    
    ## RENDER VOLCANO PLOT ##
    #Sidebar
    output$volcano_sidebar_contents <- renderUI({
      req(stat_params())

      # --- existing group/contrast selector ---
      group_contrast_selector <- if (stat_params()[[ome]]$test == "One-sample Moderated T-test") {
        radioButtons(ns("volcano_groups"), "Select Group:", choices = stat_params()[[ome]]$groups)
      } else if (stat_params()[[ome]]$test == "Two-sample Moderated T-test") {
        selectInput(ns("volcano_contrasts"), "Select Contrast:",
                    choices = sort(stat_params()[[ome]]$contrasts))
      } else if (stat_params()[[ome]]$test == "Moderated F test") {
        h4("Cannot show a volcano plot for the Mod F test")
      } else {
        NULL
      }

      # --- search column choices: all non-numeric columns in stat_results ---
      search_col_choices <- if (!is.null(stat_results()) && !is.null(stat_results()[[ome]])) {
        df_cols   <- stat_results()[[ome]]
        char_cols <- names(df_cols)[!sapply(df_cols, is.numeric)]
        if (length(char_cols) == 0) char_cols <- names(df_cols)[1]
        default_col <- grep("^id$", char_cols, value = TRUE, ignore.case = TRUE)
        if (length(default_col) == 0) default_col <- char_cols[1]
        list(choices = char_cols, selected = default_col[1])
      } else {
        list(choices = "id", selected = "id")
      }

      tagList(
        group_contrast_selector,

        hr(),

        # --- Labeling mode ---
        strong("Label Proteins:"),
        checkboxGroupInput(
          ns("label_mode"),
          label    = NULL,
          choices  = c(
            "Proteins of interest" = "poi",
            "Top 20 significant"   = "significant_top20",
            "All significant"      = "significant"
          ),
          selected = character(0)
        ),

        # --- Label across contrasts ---
        # .volcano-union-checks targets the form-group margin so spacing matches
        # the tight checkboxGroupInput style (default form-group margin-bottom is 15px).
        strong("Label across contrasts:"),
        tags$style(HTML(
          ".volcano-union-checks .form-group { margin-bottom: 3px !important; }"
        )),
        tags$div(
          class = "volcano-union-checks",
          style = "margin-top: 5px;",
          checkboxInput(ns("label_union_ome"),    label = "Current ome only", value = FALSE),
          checkboxInput(ns("label_union_global"), label = "Across all omes",  value = FALSE)
        ),

        hr(),

        # --- Search section ---
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

        # --- POI list ---
        strong("Proteins of Interest:"),
        uiOutput(ns("poi_list_ui")),

        # --- Hidden label warning ---
        uiOutput(ns("hidden_labels_warning"))
      )
    })
    
    # POI list UI
    output$poi_list_ui <- renderUI({
      pois <- proteins_of_interest()

      if (length(pois) == 0) {
        return(p("No proteins selected.", style = "color: #888; font-style: italic; font-size: 12px;"))
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

    # Register per-protein remove button observers whenever POI list changes.
    # Track which buttons already have observers to avoid accumulating duplicates.
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
          pid_local <- pid
          btn_id_local <- btn_id
          observeEvent(input[[btn_id_local]], {
            proteins_of_interest(setdiff(proteins_of_interest(), pid_local))
            poi_observer_registry(setdiff(poi_observer_registry(), btn_id_local))
          }, ignoreNULL = TRUE, ignoreInit = TRUE, once = TRUE)
        })
      })

      # Update registry with union of existing and new
      poi_observer_registry(unique(c(existing, new_btn_ids)))
    })

    # Clear all POIs
    observeEvent(input$clear_all_poi, {
      proteins_of_interest(character(0))
      hidden_label_count(0L)
    })

    # Mutual exclusion: "Current ome only" and "Across all omes" cannot both be on.
    # When one is checked the other is unchecked and disabled; unchecking re-enables both.
    observeEvent(input$label_union_ome, {
      if (isTRUE(input$label_union_ome)) {
        updateCheckboxInput(session, "label_union_global", value = FALSE)
        shinyjs::disable("label_union_global")
      } else {
        shinyjs::enable("label_union_global")
        # Both unchecked — revert to per-contrast behavior
        if (!is.null(global_union_rv)) global_union_rv(FALSE)
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$label_union_global, {
      if (isTRUE(input$label_union_global)) {
        updateCheckboxInput(session, "label_union_ome", value = FALSE)
        shinyjs::disable("label_union_ome")
        # signal other omes that global union mode is now active
        if (!is.null(global_union_rv)) global_union_rv(TRUE)
      } else {
        shinyjs::enable("label_union_ome")
        if (!is.null(global_union_rv)) global_union_rv(FALSE)
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Auto-enable POI checkbox when proteins are added to the list
    observeEvent(proteins_of_interest(), {
      pois <- proteins_of_interest()
      if (length(pois) > 0 && !"poi" %in% isolate(input$label_mode)) {
        updateCheckboxGroupInput(session, "label_mode",
          selected = unique(c(isolate(input$label_mode), "poi")))
      }
    }, ignoreNULL = FALSE)

    # Hidden label overflow warning
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
      req(stat_results(), input$protein_search, input$search_metadata_col)

      raw_input  <- input$protein_search
      search_col <- input$search_metadata_col
      df         <- stat_results()[[ome]]

      query_tokens <- parse_protein_search_input(raw_input)
      if (length(query_tokens) == 0) return()

      if (!search_col %in% colnames(df)) {
        showNotification(
          paste0("Column '", search_col, "' not found in results."),
          type = "error", duration = 4
        )
        return()
      }

      matched_ids   <- character(0)
      unmatched_ids <- character(0)

      # Find the id column using case-insensitive match (consistent with poi_export_function)
      id_col <- grep("^id$", colnames(df), value = TRUE, ignore.case = TRUE)[1]
      if (is.na(id_col)) {
        showNotification("No 'id' column found in stat results.", type = "error", duration = 4)
        return()
      }

      for (token in query_tokens) {
        hit_rows <- df[tolower(as.character(df[[search_col]])) == tolower(token), ]
        if (nrow(hit_rows) > 0) {
          matched_ids <- c(matched_ids, as.character(hit_rows[[id_col]]))
        } else {
          unmatched_ids <- c(unmatched_ids, token)
        }
      }

      if (length(unmatched_ids) > 0) {
        showNotification(
          paste0("Not found: ", paste(unmatched_ids, collapse = ", ")),
          type = "warning", duration = 5
        )
      }

      if (length(matched_ids) > 0) {
        current <- proteins_of_interest()
        proteins_of_interest(unique(c(current, matched_ids)))
      }
    })

    #Plot
    output$volcano_plot <- renderPlotly({
      req(stat_results(), stat_params(), ome)

      test <- stat_params()[[ome]]$test
      if (test == "One-sample Moderated T-test") {
        req(input$volcano_groups)
      } else if (test == "Two-sample Moderated T-test") {
        req(input$volcano_contrasts)
      } else {
        return(NULL)
      }

      # Build base ggplot (no labels) — wrapped in tryCatch to show friendly error
      gg <- tryCatch(
        plotVolcano(
          ome               = ome,
          volcano_groups    = input$volcano_groups,
          volcano_contrasts = as.character(input$volcano_contrasts),
          df                = stat_results()[[ome]],
          stat_params       = stat_params,
          stat_results      = stat_results
        ),
        error = function(e) {
          showNotification(
            paste0("Could not render volcano plot: ", conditionMessage(e)),
            type = "error", duration = 8
          )
          NULL
        }
      )
      validate(need(!is.null(gg), "Volcano plot could not be generated. Check that required stat columns exist."))

      # Convert to plotly with named source for click events
      p <- ggplotly(gg, source = ns("volcano_click"), tooltip = "text")

      # Build the standardized df for labeling
      df_raw <- stat_results()[[ome]]
      cols   <- get_volcano_cols(df_raw, test, input$volcano_groups, input$volcano_contrasts)
      df_plot <- tryCatch(
        build_volcano_df(df_raw, cols,
                         sig_cutoff = stat_params()[[ome]]$cutoff,
                         sig_stat   = stat_params()[[ome]]$stat),
        error = function(e) { message("build_volcano_df failed: ", conditionMessage(e)); NULL }
      )

      if (!is.null(df_plot)) {
        # Compute the effective POI and label_mode based on union toggle state.
        effective_poi <- switch(
          union_mode(),
          "ome" = union(
            proteins_of_interest(),
            volcano_label_union_for_ome(
              stat_results()[[ome]], stat_params()[[ome]],
              input$label_mode, proteins_of_interest()
            )
          ),
          "global" = union(proteins_of_interest(), global_union_ids()),
          proteins_of_interest()  # "none"
        )
        # When union POI is non-empty, force "poi" into label_mode so labels render.
        effective_label_mode <- if (length(effective_poi) > 0) {
          unique(c(input$label_mode, "poi"))
        } else {
          input$label_mode
        }

        p <- add_volcano_labels(
          p,
          df              = df_plot,
          poi             = effective_poi,
          label_mode      = effective_label_mode,
          y_cutoff        = attr(df_plot, "y_cutoff"),
          hidden_count_rv = hidden_label_count
        )
      }

      p <- event_register(p, "plotly_click")
      p
    })

    ## CLICK-TO-ADD/REMOVE OBSERVER ##
    observeEvent(event_data("plotly_click", source = ns("volcano_click")), {
      click <- event_data("plotly_click", source = ns("volcano_click"))
      req(click, stat_results(), stat_params())

      test <- stat_params()[[ome]]$test

      if (test == "One-sample Moderated T-test") {
        req(input$volcano_groups)
      } else if (test == "Two-sample Moderated T-test") {
        req(input$volcano_contrasts)
      } else {
        return()
      }

      df_raw <- stat_results()[[ome]]
      cols   <- get_volcano_cols(df_raw, test, input$volcano_groups, input$volcano_contrasts)

      id_col   <- cols$id
      logfc_col <- cols$logfc
      logp_col  <- cols$logp

      req(!is.na(id_col), !is.na(logfc_col), !is.na(logp_col))

      df <- df_raw[!is.na(df_raw[[logp_col]]), ]
      df$id    <- as.character(df[[id_col]])
      df$logFC <- df[[logfc_col]]
      df$logP  <- df[[logp_col]]

      clicked_id <- get_clicked_feature_id(click, df)
      req(!is.na(clicked_id))

      current <- proteins_of_interest()
      if (clicked_id %in% current) {
        proteins_of_interest(setdiff(current, clicked_id))
      } else {
        proteins_of_interest(c(current, clicked_id))
      }
    })
    

    ## COMPILE EXPORTS ##
    volcano_plot_export_function <- function(dir_name) {
      test <- stat_params()[[ome]]$test
      
      # Skip export if test doesn't support volcano plots
      if (is.null(test) || test == "None" || test == "Moderated F test") {
        return()
      }
      
      df <- stat_results()[[ome]]
      
      # Create a single PDF file for all plots from this ome
      pdf_filename <- paste0("volcano_plots_", ome, ".pdf")
      pdf_path <- file.path(dir_name, pdf_filename)
      
      # Start PDF device
      pdf_params <- get_pdf_params()
      pdf(pdf_path, width = pdf_params$width, height = pdf_params$height)
      on.exit(dev.off(), add = TRUE)

      label_mode_export <- isolate(input$label_mode) %||% character(0)

      # Compute effective POI for export based on union toggle state.
      export_union_mode <- isolate(union_mode())
      export_poi <- switch(
        export_union_mode,
        "ome" = union(
          isolate(proteins_of_interest()),
          volcano_label_union_for_ome(
            stat_results()[[ome]], stat_params()[[ome]],
            label_mode_export, isolate(proteins_of_interest())
          )
        ),
        "global" = {
          all_omes_names <- names(stat_results())
          Reduce(union, lapply(all_omes_names, function(o) {
            sp <- stat_params()[[o]]
            sr <- stat_results()[[o]]
            if (is.null(sp) || is.null(sr)) return(character(0))
            volcano_label_union_for_ome(sr, sp, label_mode_export, isolate(proteins_of_interest()))
          }), init = character(0))
        },
        isolate(proteins_of_interest())  # "none"
      )
      # Force "poi" into label_mode when union POI is non-empty
      export_label_mode <- if (length(export_poi) > 0) {
        unique(c(label_mode_export, "poi"))
      } else {
        label_mode_export
      }

      if (test == "One-sample Moderated T-test") {
        groups <- stat_params()[[ome]]$groups
        for (group in groups) {
          tryCatch({
            gg <- plotVolcano(
              ome               = ome,
              volcano_groups    = group,
              volcano_contrasts = NULL,
              df                = df,
              stat_params       = stat_params,
              stat_results      = stat_results,
              label_proteins    = export_poi,
              label_mode        = export_label_mode
            )
            print(gg)
          }, error = function(e) {
            message("Volcano export failed for ", ome, " - ", group, ": ", conditionMessage(e))
          })
        }

      } else if (test == "Two-sample Moderated T-test") {
        contrasts <- stat_params()[[ome]]$contrasts
        for (contrast in contrasts) {
          tryCatch({
            gg <- plotVolcano(
              ome               = ome,
              volcano_groups    = NULL,
              volcano_contrasts = contrast,
              df                = df,
              stat_params       = stat_params,
              stat_results      = stat_results,
              label_proteins    = export_poi,
              label_mode        = export_label_mode
            )
            print(gg)
          }, error = function(e) {
            message("Volcano export failed for ", ome, " - ", contrast, ": ", conditionMessage(e))
          })
        }

      } else {
        warning("Volcano plot export not supported for test type: ", test)
      }

      cat("Saved volcano plots for", ome, "to:", pdf_path, "\n")
    }

    ## Volcano labeled proteins CSV (POI + any significant / top-20 labels matching the plot)
    labeled_volcano_csv_export_function <- function(dir_name) {
      tryCatch({
        test <- stat_params()[[ome]]$test
        if (is.null(test) || test == "None" || test == "Moderated F test") {
          return()
        }

        label_mode_export <- isolate(input$label_mode) %||% character(0)
        poi <- isolate(proteins_of_interest())

        show_poi <- "poi" %in% label_mode_export
        show_sig <- "significant" %in% label_mode_export
        show_sig_top <- "significant_top20" %in% label_mode_export
        # Union mode counts as "label all" even without an explicit mode selected
        csv_union_mode <- isolate(union_mode())
        if (!show_poi && !show_sig && !show_sig_top && csv_union_mode == "none") {
          message(
            "Volcano labeled export skipped for ", ome,
            ": enable at least one label option (POI, Top 20, or All significant)."
          )
          return()
        }

        df_raw <- stat_results()[[ome]]
        if (is.null(df_raw) || nrow(df_raw) == 0) {
          message("Volcano labeled export skipped: no stat results for ome ", ome)
          return()
        }

        sp <- stat_params()[[ome]]
        sig_cutoff <- sp$cutoff
        sig_stat <- sp$stat

        # Effective label mode: force "poi" when union POI will be non-empty
        effective_label_mode_csv <- if (csv_union_mode != "none") {
          unique(c(label_mode_export, "poi"))
        } else {
          label_mode_export
        }

        # Effective POI: include union IDs from other contrasts/omes when toggled
        effective_poi_csv <- switch(
          csv_union_mode,
          "ome" = union(poi, volcano_label_union_for_ome(df_raw, sp, label_mode_export, poi)),
          "global" = {
            all_omes_names <- names(stat_results())
            Reduce(union, lapply(all_omes_names, function(o) {
              sp_o <- stat_params()[[o]]
              sr_o <- stat_results()[[o]]
              if (is.null(sp_o) || is.null(sr_o)) return(character(0))
              volcano_label_union_for_ome(sr_o, sp_o, label_mode_export, poi)
            }), init = poi)
          },
          poi  # "none"
        )

        all_ids <- character(0)

        if (test == "One-sample Moderated T-test") {
          groups <- sp$groups
          for (group in groups) {
            cols <- get_volcano_cols(df_raw, test, group, NULL)
            df_plot <- build_volcano_df(df_raw, cols, sig_cutoff, sig_stat)
            all_ids <- union(all_ids, volcano_labeled_feature_ids(df_plot, effective_label_mode_csv, effective_poi_csv))
          }
        } else if (test == "Two-sample Moderated T-test") {
          contrasts <- sp$contrasts
          for (contrast in contrasts) {
            cols <- get_volcano_cols(df_raw, test, NULL, contrast)
            df_plot <- build_volcano_df(df_raw, cols, sig_cutoff, sig_stat)
            all_ids <- union(all_ids, volcano_labeled_feature_ids(df_plot, effective_label_mode_csv, effective_poi_csv))
          }
        } else {
          message("Volcano labeled export not supported for test type: ", test)
          return()
        }

        if (length(all_ids) == 0) {
          message("Volcano labeled export: no proteins matched label criteria for ", ome)
          return()
        }

        id_col <- grep("^id$", colnames(df_raw), value = TRUE, ignore.case = TRUE)[1]
        if (is.na(id_col)) {
          message("Volcano labeled export skipped: no 'id' column in stat results for ", ome)
          return()
        }

        out_rows <- df_raw[as.character(df_raw[[id_col]]) %in% all_ids, , drop = FALSE]
        out_path <- file.path(dir_name, paste0("volcano_labeled_proteins_", ome, ".csv"))
        write.csv(out_rows, file = out_path, row.names = FALSE)
        cat(
          "Saved", nrow(out_rows), "volcano-labeled protein row(s) for", ome, "to:", out_path, "\n"
        )
      }, error = function(e) {
        message("Volcano labeled export failed for ome ", ome, ": ", conditionMessage(e))
      })
    }

    return(list(
      volcano_plot         = volcano_plot_export_function,
      proteins_of_interest = labeled_volcano_csv_export_function
    ))
  })
}