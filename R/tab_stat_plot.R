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
    
    # poi_registry: parent-level named list keyed by "<ome>::<contrast_key>",
    # each slot is a character() of feature IDs chosen by the user. Passed by
    # reference into every ome module so union-across-contrasts works without
    # each module poking at another's input$ state.
    poi_registry <- reactiveVal(list())

    # Initialize / extend registry when the ome set changes.
    observeEvent(all_omes(), {
      reg <- poi_registry()
      missing_omes <- setdiff(all_omes(), names(reg))
      if (length(missing_omes) > 0) {
        reg[missing_omes] <- list(character(0))
        poi_registry(reg)
      }
    }, ignoreNULL = TRUE)

    # top_n_registry: parent-level named list keyed by "<ome>::<contrast_key>",
    # each slot is a single integer — how many top significant features to label
    # for that contrast. Independent per contrast; default 20L when not yet set.
    top_n_registry <- reactiveVal(list())

    # label_mode_registry: parent-level named list keyed by "<ome>::<contrast_key>",
    # each slot is a character vector of active label modes for that contrast
    # (e.g. c("poi", "significant_top20")). Independent per contrast so that
    # checking "Top significant" on one contrast does not affect others.
    label_mode_registry <- reactiveVal(list())

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
          poi_registry = poi_registry,
          top_n_registry = top_n_registry,
          label_mode_registry = label_mode_registry
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
                                   poi_registry = NULL,
                                   top_n_registry = NULL,
                                   label_mode_registry = NULL) {

  ## module function
  moduleServer(id, function (input, output, session) {

    # get namespace, use in renderUI-like functions
    ns <- session$ns

    ## FEATURE SEARCH & LABELING ################################################
    # POI is stored per contrast, not per ome, so features added while viewing
    # one contrast do not bleed into other contrasts.
    # Registry key format: "<ome>::<contrast_key>"
    # For one-sample tests: contrast_key = input$volcano_groups
    # For two-sample tests: contrast_key = input$volcano_contrasts

    # current_contrast_key: reactive string identifying the active contrast.
    # Returns NULL when the contrast input is not yet available.
    current_contrast_key <- reactive({
      req(stat_params())
      test <- stat_params()[[ome]]$test
      if (is.null(test) || test == "None" || test == "Moderated F test") return(NULL)
      if (test == "One-sample Moderated T-test") {
        req(input$volcano_groups)
        paste0(ome, "::", input$volcano_groups)
      } else {
        req(input$volcano_contrasts)
        paste0(ome, "::", input$volcano_contrasts)
      }
    })

    # proteins_of_interest: reads the current contrast's slot from the registry.
    proteins_of_interest <- reactive({
      key <- current_contrast_key()
      req(key)
      reg <- poi_registry()
      reg[[key]] %||% character(0)
    })

    # Setter helper — writes to the current contrast's slot in the shared registry.
    set_poi <- function(new_ids) {
      key <- isolate(current_contrast_key())
      if (is.null(key)) return()
      reg <- poi_registry()
      reg[[key]] <- unique(as.character(new_ids))
      poi_registry(reg)
    }

    # top_n_sig: reads this contrast's top-N value from the registry (default 20).
    top_n_sig <- reactive({
      key <- current_contrast_key()
      req(key)
      reg <- top_n_registry()
      reg[[key]] %||% 20L
    })

    # Setter — writes this contrast's top-N value into the shared registry.
    set_top_n <- function(n) {
      key <- isolate(current_contrast_key())
      if (is.null(key)) return()
      reg <- top_n_registry()
      reg[[key]] <- max(1L, as.integer(n)[1L])
      top_n_registry(reg)
    }

    # label_mode_for_contrast: reads this contrast's stored label modes from
    # the registry. Default character(0) when the contrast is first visited.
    label_mode_for_contrast <- reactive({
      key <- current_contrast_key()
      req(key)
      reg <- label_mode_registry()
      reg[[key]] %||% character(0)
    })

    # Setter — writes this contrast's label modes into the shared registry.
    set_label_mode <- function(modes) {
      key <- isolate(current_contrast_key())
      if (is.null(key)) return()
      reg <- label_mode_registry()
      reg[[key]] <- as.character(modes %||% character(0))
      label_mode_registry(reg)
    }

    # Persist checkbox state into registry when the user checks/unchecks.
    # ignoreInit = TRUE: the checkbox fires on load (value = character(0)), which
    # would overwrite a previously saved value before the contrast-sync observer
    # has had a chance to restore it. Skipping init is safe — the contrast-change
    # observer handles the initial population.
    observeEvent(input$label_mode, {
      set_label_mode(input$label_mode)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # When the active contrast changes, restore its stored label_mode into the UI.
    # isolate() around label_mode_for_contrast() avoids creating a reactive
    # dependency on the registry here (contrast key change is the only trigger).
    observeEvent(current_contrast_key(), {
      stored <- isolate(label_mode_for_contrast())
      updateCheckboxGroupInput(session, "label_mode", selected = stored)
      # Re-apply mutual-exclusion disable state when switching contrasts.
      grp_id <- ns("label_mode")
      if ("significant" %in% stored) {
        shinyjs::runjs(sprintf(
          "$('#%s input[value=\"significant_top20\"]').prop('disabled', true).closest('label').css('opacity', 0.4);",
          grp_id
        ))
      } else if ("significant_top20" %in% stored) {
        shinyjs::runjs(sprintf(
          "$('#%s input[value=\"significant\"]').prop('disabled', true).closest('label').css('opacity', 0.4);",
          grp_id
        ))
      } else {
        shinyjs::runjs(sprintf(
          "$('#%s input[value=\"significant_top20\"], #%s input[value=\"significant\"]').prop('disabled', false).closest('label').css('opacity', 1);",
          grp_id, grp_id
        ))
      }
    }, ignoreNULL = TRUE, ignoreInit = FALSE)

    # Mutual exclusion between "All significant" and "Top significant":
    # checking one unchecks and disables the other.
    observeEvent(input$label_mode, {
      grp_id <- ns("label_mode")
      if ("significant" %in% input$label_mode) {
        # Uncheck and disable "Top significant"
        updateCheckboxGroupInput(session, "label_mode",
          selected = setdiff(input$label_mode, "significant_top20"))
        shinyjs::runjs(sprintf(
          "$('#%s input[value=\"significant_top20\"]').prop('disabled', true).closest('label').css('opacity', 0.4);",
          grp_id
        ))
        shinyjs::runjs(sprintf(
          "$('#%s input[value=\"significant\"]').prop('disabled', false).closest('label').css('opacity', 1);",
          grp_id
        ))
      } else if ("significant_top20" %in% input$label_mode) {
        # Uncheck and disable "All significant"
        updateCheckboxGroupInput(session, "label_mode",
          selected = setdiff(input$label_mode, "significant"))
        shinyjs::runjs(sprintf(
          "$('#%s input[value=\"significant\"]').prop('disabled', true).closest('label').css('opacity', 0.4);",
          grp_id
        ))
        shinyjs::runjs(sprintf(
          "$('#%s input[value=\"significant_top20\"]').prop('disabled', false).closest('label').css('opacity', 1);",
          grp_id
        ))
      } else {
        # Neither checked — re-enable both
        shinyjs::runjs(sprintf(
          "$('#%s input[value=\"significant_top20\"], #%s input[value=\"significant\"]').prop('disabled', false).closest('label').css('opacity', 1);",
          grp_id, grp_id
        ))
      }
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    hidden_label_count <- reactiveVal(0L)

    # union_mode: "none" | "ome"
    # "ome" is driven by this ome's local label_union_ome checkbox (labeled
    # "Label features for all contrasts" in the UI).
    union_mode <- reactive({
      if (isTRUE(input$label_union_ome)) return("ome")
      "none"
    })

    # ome_union_poi: union of all POI slots belonging to this ome (all contrasts).
    # Used when union_mode() == "ome" to label across contrasts within the ome.
    ome_union_poi <- reactive({
      req(union_mode() == "ome")
      reg    <- poi_registry()
      prefix <- paste0(ome, "::")
      keys   <- names(reg)[startsWith(names(reg), prefix)]
      Reduce(union, lapply(keys, function(k) reg[[k]] %||% character(0)), init = character(0))
    })

    # top_n_ui: shows a numeric input below the "Top significant" checkbox,
    # only when that checkbox is checked. Value is per-contrast from top_n_registry.
    output$top_n_ui <- renderUI({
      req("significant_top20" %in% label_mode_for_contrast())
      numericInput(
        ns("top_n_sig_input"),
        label = "Number of top features:",
        value = top_n_sig(),
        min = 1, step = 1, width = "120px"
      )
    })

    # Persist the user's chosen top-N into the registry when changed.
    observeEvent(input$top_n_sig_input, {
      req(is.numeric(input$top_n_sig_input), !is.na(input$top_n_sig_input))
      set_top_n(input$top_n_sig_input)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # When the active contrast changes, sync the numeric input to that contrast's value.
    observeEvent(current_contrast_key(), {
      updateNumericInput(session, "top_n_sig_input", value = isolate(top_n_sig()))
    }, ignoreNULL = TRUE, ignoreInit = FALSE)

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
            tagList(
              shinydashboardPlus::box(
                plotlyOutput(ns("volcano_plot")),
                status = "primary",
                width = NULL,
                title = "Volcano Plot",
                headerBorder = TRUE,
                solidHeader = TRUE
              ),
              uiOutput(ns("hidden_labels_warning"))
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

      # --- non-numeric columns shared by label-column selector and search selector ---
      char_cols_available <- if (!is.null(stat_results()) && !is.null(stat_results()[[ome]])) {
        df_cols   <- stat_results()[[ome]]
        cols      <- names(df_cols)[!sapply(df_cols, is.numeric)]
        if (length(cols) == 0) cols <- names(df_cols)[1]
        cols
      } else {
        "id"
      }
      id_col_default <- {
        hit <- grep("^id$", char_cols_available, value = TRUE, ignore.case = TRUE)
        if (length(hit) > 0) hit[1] else char_cols_available[1]
      }
      # Ensure "id" (or the id column) appears first in the label-column list
      label_col_choices <- unique(c(id_col_default, char_cols_available))
      search_col_choices <- list(choices = char_cols_available, selected = id_col_default)

      tagList(
        group_contrast_selector,

        hr(),

        # --- Label column selector ---
        strong("Label column:"),
        selectInput(
          ns("label_column"),
          label    = NULL,
          choices  = label_col_choices,
          selected = id_col_default
        ),
        checkboxInput(
          ns("label_split_enabled"),
          label = "Delimited values",
          value = FALSE
        ),
        conditionalPanel(
          condition = "input.label_split_enabled == true",
          textInput(
            ns("label_split_sep"),
            label       = "Delimiter",
            value       = ";",
            placeholder = ";"
          ),
          helpText(
            "If a label has several parts separated by this character (for example A;B;C), only the first part is shown."
          ),
          ns = ns
        ),
        checkboxInput(
          ns("label_display_trim_enabled"),
          label   = "Shorten long labels on plot",
          value   = FALSE
        ),

        hr(),

        # --- Labeling mode ---
        strong("Label Features:"),
        checkboxGroupInput(
          ns("label_mode"),
          label    = NULL,
          choices  = c(
            "Feature(s) of interest" = "poi",
            "Top significant"        = "significant_top20",
            "All significant"        = "significant"
          ),
          selected = character(0)
        ),
        helpText(
          "Top significant and All significant pick plot labels from your results. ",
          "They do not add anything to the manual list below."
        ),
        uiOutput(ns("top_n_ui")),

        # --- Label across contrasts ---
        # .volcano-union-checks targets the form-group margin so spacing matches
        # the tight checkboxGroupInput style (default form-group margin-bottom is 15px).
        tags$style(HTML(
          ".volcano-union-checks .form-group { margin-bottom: 3px !important; }"
        )),
        tags$div(
          style = paste(
            "margin-top: 6px; margin-bottom: 4px; padding: 8px 10px;",
            "border: 1px solid #cfe2ff; border-radius: 6px;",
            "background-color: #f5f9ff;"
          ),
          tags$div(
            style = "font-size: 12px; font-weight: 600; color: #2c5282; margin-bottom: 4px;",
            "Across contrasts"
          ),
          tags$div(
            class = "volcano-union-checks",
            checkboxInput(ns("label_union_ome"), label = "Label features for all contrasts", value = FALSE)
          )
        ),

        hr(),

        # --- Search section ---
        strong("Search Features:"),
        selectInput(
          ns("search_metadata_col"),
          label    = "Search column:",
          choices  = search_col_choices$choices,
          selected = search_col_choices$selected
        ),
        textAreaInput(
          ns("protein_search"),
          label       = NULL,
          placeholder = "Paste feature IDs separated by space, comma, or semicolon",
          rows        = 3
        ),
        actionButton(ns("search_btn"), "Search", class = "btn-sm btn-primary"),

        hr(),

        # --- POI list ---
        strong("Feature(s) of Interest:"),
        helpText(
          "Only feature IDs you add with Search or by clicking the volcano appear here."
        ),
        uiOutput(ns("poi_list_ui"))
      )
    })
    
    # POI list UI
    # In "none" mode: per-contrast list with individual remove buttons.
    # In "ome" mode: read-only union list for all contrasts in this ome.
    output$poi_list_ui <- renderUI({
      mode <- union_mode()

      if (mode == "ome") {
        # Show union of all contrast slots in this ome (read-only)
        pois <- ome_union_poi()
        if (length(pois) == 0) {
          return(p("No features selected.", style = "color: #888; font-style: italic; font-size: 12px;"))
        }
        tagList(
          div(
            style = "font-size: 12px; color: #555; max-height: 120px; overflow-y: auto;",
            paste(pois, collapse = ", ")
          ),
          p("(Editing disabled while 'Label features for all contrasts' is on.)",
            style = "font-size: 11px; color: #888; margin-top: 4px;"),
          br(),
          actionButton(ns("clear_all_poi"), "Clear all (current contrast)", class = "btn-xs btn-warning")
        )

      } else {
        # "none" — per-contrast list with individual remove buttons
        pois <- proteins_of_interest()
        if (length(pois) == 0) {
          return(p("No features selected.", style = "color: #888; font-style: italic; font-size: 12px;"))
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
          # Scroll container keeps the sidebar height stable when many POIs
          # accumulate. Clear-all sits OUTSIDE so it stays visible without
          # scrolling. Matches the read-only "ome" branch above (which uses
          # 120px since it shows a comma-joined string instead of rows).
          div(
            style = "max-height: 220px; overflow-y: auto; overflow-x: hidden;",
            do.call(tagList, poi_rows)
          ),
          br(),
          actionButton(ns("clear_all_poi"), "Clear all", class = "btn-xs btn-warning")
        )
      }
    })

    # Register per-feature remove button observers whenever POI list changes.
    # Track which buttons already have observers to avoid accumulating duplicates.
    # Reset when the active contrast changes so each contrast gets a fresh slate.
    poi_observer_registry <- reactiveVal(character(0))

    observeEvent(current_contrast_key(), {
      poi_observer_registry(character(0))
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

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
            set_poi(setdiff(proteins_of_interest(), pid_local))
            poi_observer_registry(setdiff(poi_observer_registry(), btn_id_local))
          }, ignoreNULL = TRUE, ignoreInit = TRUE, once = TRUE)
        })
      })

      # Update registry with union of existing and new
      poi_observer_registry(unique(c(existing, new_btn_ids)))
    })

    # Clear all POIs
    observeEvent(input$clear_all_poi, {
      set_poi(character(0))
      hidden_label_count(0L)
    })

    # Auto-enable POI checkbox when proteins are added to the list
    observeEvent(proteins_of_interest(), {
      pois <- proteins_of_interest()
      if (length(pois) > 0 && !"poi" %in% isolate(label_mode_for_contrast())) {
        updateCheckboxGroupInput(session, "label_mode",
          selected = unique(c(isolate(label_mode_for_contrast()), "poi")))
      }
    }, ignoreNULL = FALSE)

    # Hidden label overlap note
    output$hidden_labels_warning <- renderUI({
      n <- hidden_label_count()
      if (is.null(n) || n == 0L) return(NULL)
      helpText(
        style = "margin-top: 6px; margin-bottom: 0; font-size: 12px;",
        paste0("Note: Labels for ", n, " feature(s) are hidden to reduce overlap.")
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
        set_poi(unique(c(proteins_of_interest(), matched_ids)))
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
          stat_results      = stat_results,
          label_column                = input$label_column        %||% "id",
          label_split_enabled         = isTRUE(input$label_split_enabled),
          label_split_sep             = input$label_split_sep     %||% ";",
          label_display_trim_enabled  = isTRUE(input$label_display_trim_enabled)
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
        # Apply user-selected label column to df_plot$geneSymbol before labeling
        lbl_col_live <- input$label_column %||% "id"
        if (lbl_col_live %in% colnames(df_raw)) {
          id_col_live <- grep("^id$", colnames(df_raw), value = TRUE, ignore.case = TRUE)[1]
          raw_vals <- df_raw[[lbl_col_live]][match(df_plot$id, as.character(df_raw[[id_col_live]]))]
          resolved <- resolve_volcano_label_text(
            raw_vals,
            split_enabled = isTRUE(input$label_split_enabled),
            separator     = input$label_split_sep %||% ";"
          )
          na_mask <- is.na(resolved) | !nzchar(resolved)
          resolved[na_mask] <- df_plot$id[na_mask]
          df_plot$geneSymbol <- resolved
        }

        # Compute the effective POI and label_mode based on union toggle state.
        # In "ome" mode, build a per-contrast union: each contrast contributes its
        # own POI, its own label_mode (significant / significant_top20), and its
        # own top-N count. The current contrast's label_mode drives the local render.
        effective_poi <- switch(
          union_mode(),
          "ome" = {
            lm_reg  <- label_mode_registry()
            tn_reg  <- top_n_registry()
            poi_reg <- poi_registry()
            sp <- stat_params()[[ome]]
            sr <- stat_results()[[ome]]
            # Use all known contrasts from stat_params as the source of truth —
            # not poi_reg keys — so contrasts with only label_mode set (no manual
            # POI) are still included in the union.
            all_contrasts <- if (sp$test == "One-sample Moderated T-test")
              sp$groups
            else
              sp$contrasts
            keys <- paste0(ome, "::", all_contrasts)
            Reduce(union, lapply(keys, function(k) {
              c_poi   <- poi_reg[[k]]   %||% character(0)
              c_lm    <- lm_reg[[k]]    %||% character(0)
              c_n_top <- tn_reg[[k]]    %||% 20L
              c_suffix <- sub(paste0("^", ome, "::"), "", k)
              cols <- tryCatch(
                if (sp$test == "One-sample Moderated T-test")
                  get_volcano_cols(sr, sp$test, c_suffix, NULL)
                else
                  get_volcano_cols(sr, sp$test, NULL, c_suffix),
                error = function(e) NULL
              )
              if (is.null(cols)) return(c_poi)
              df_c <- tryCatch(
                build_volcano_df(sr, cols, sp$cutoff, sp$stat),
                error = function(e) NULL
              )
              if (is.null(df_c)) return(c_poi)
              union(c_poi, volcano_labeled_feature_ids(df_c, c_lm, c_poi, c_n_top))
            }), init = character(0))
          },
          proteins_of_interest()           # "none" — per-contrast baseline
        )
        # Force "poi" into label_mode when union is active and produced IDs.
        effective_label_mode <- if (union_mode() != "none" && length(effective_poi) > 0) {
          unique(c(label_mode_for_contrast(), "poi"))
        } else {
          label_mode_for_contrast()
        }

        p <- add_volcano_labels(
          p,
          df              = df_plot,
          poi             = effective_poi,
          label_mode      = effective_label_mode,
          y_cutoff        = attr(df_plot, "y_cutoff"),
          hidden_count_rv = hidden_label_count,
          label_display_trim_enabled = isTRUE(input$label_display_trim_enabled),
          n_top           = top_n_sig()
        )
      }

      p <- event_register(p, "plotly_click")
      p
    })

    ## CLICK-TO-ADD/REMOVE OBSERVER ##
    # Read click events defensively: when no volcano widget is active for this
    # ome, querying event_data() can emit an "unregistered source" warning.
    volcano_click_event <- reactive({
      req(stat_params(), ome)
      test <- stat_params()[[ome]]$test
      if (is.null(test) || length(test) != 1L ||
          !(test %in% c("One-sample Moderated T-test", "Two-sample Moderated T-test"))) {
        return(NULL)
      }
      suppressWarnings(event_data("plotly_click", source = ns("volcano_click")))
    })

    observeEvent(volcano_click_event(), {
      click <- volcano_click_event()
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
        set_poi(setdiff(current, clicked_id))
      } else {
        set_poi(c(current, clicked_id))
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

      label_mode_export <- isolate(label_mode_for_contrast()) %||% character(0)
      n_top_export      <- isolate(top_n_sig())
      label_column_export <- isolate(input$label_column)        %||% "id"
      label_split_export  <- isTRUE(isolate(input$label_split_enabled))
      label_sep_export    <- isolate(input$label_split_sep)     %||% ";"
      label_trim_export   <- isTRUE(isolate(input$label_display_trim_enabled))

      # Compute effective POI for export based on union toggle state.
      export_union_mode <- isolate(union_mode())
      export_poi <- switch(
        export_union_mode,
        "ome" = {
          # Per-contrast union: each contrast uses its own POI, label_mode, top-N.
          lm_reg_exp  <- isolate(label_mode_registry())
          tn_reg_exp  <- isolate(top_n_registry())
          poi_reg_exp <- isolate(poi_registry())
          sp_exp <- stat_params()[[ome]]
          sr_exp <- stat_results()[[ome]]
          all_contrasts_exp <- if (sp_exp$test == "One-sample Moderated T-test")
            sp_exp$groups else sp_exp$contrasts
          keys_exp <- paste0(ome, "::", all_contrasts_exp)
          Reduce(union, lapply(keys_exp, function(k) {
            c_poi    <- poi_reg_exp[[k]] %||% character(0)
            c_lm     <- lm_reg_exp[[k]] %||% character(0)
            c_n_top  <- tn_reg_exp[[k]] %||% 20L
            c_suffix <- sub(paste0("^", ome, "::"), "", k)
            cols <- tryCatch(
              if (sp_exp$test == "One-sample Moderated T-test")
                get_volcano_cols(sr_exp, sp_exp$test, c_suffix, NULL)
              else
                get_volcano_cols(sr_exp, sp_exp$test, NULL, c_suffix),
              error = function(e) NULL
            )
            if (is.null(cols)) return(c_poi)
            df_c <- tryCatch(
              build_volcano_df(sr_exp, cols, sp_exp$cutoff, sp_exp$stat),
              error = function(e) NULL
            )
            if (is.null(df_c)) return(c_poi)
            union(c_poi, volcano_labeled_feature_ids(df_c, c_lm, c_poi, c_n_top))
          }), init = character(0))
        },
        {
          # "none" — use only the current contrast's POI for export
          isolate(proteins_of_interest())
        }
      )
      # Force "poi" into label_mode when union is active and produced IDs.
      export_label_mode <- if (export_union_mode != "none" && length(export_poi) > 0) {
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
              label_mode        = export_label_mode,
              label_column                = label_column_export,
              label_split_enabled         = label_split_export,
              label_split_sep             = label_sep_export,
              label_display_trim_enabled  = label_trim_export,
              n_top             = n_top_export
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
              label_mode        = export_label_mode,
              label_column                = label_column_export,
              label_split_enabled         = label_split_export,
              label_split_sep             = label_sep_export,
              label_display_trim_enabled  = label_trim_export,
              n_top             = n_top_export
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

        label_mode_export <- isolate(label_mode_for_contrast()) %||% character(0)
        label_column_export <- isolate(input$label_column)        %||% "id"
        label_split_export  <- isTRUE(isolate(input$label_split_enabled))
        label_sep_export    <- isolate(input$label_split_sep)     %||% ";"
        poi <- isolate(proteins_of_interest())

        show_poi <- "poi" %in% label_mode_export
        show_sig <- "significant" %in% label_mode_export
        show_sig_top <- "significant_top20" %in% label_mode_export
        # Union mode counts as "label all" even without an explicit mode selected
        csv_union_mode <- isolate(union_mode())
        if (!show_poi && !show_sig && !show_sig_top && csv_union_mode == "none") {
          message(
            "Volcano labeled export skipped for ", ome,
            ": enable at least one label option (POI, Top significant, or All significant)."
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

        n_top_csv <- isolate(top_n_sig())

        # Effective POI: per-contrast union when union mode is on.
        effective_poi_csv <- switch(
          csv_union_mode,
          "ome" = {
            # Per-contrast union: each contrast uses its own POI, label_mode, top-N.
            lm_reg_csv  <- isolate(label_mode_registry())
            tn_reg_csv  <- isolate(top_n_registry())
            poi_reg_csv <- isolate(poi_registry())
            all_contrasts_csv <- if (sp$test == "One-sample Moderated T-test")
              sp$groups else sp$contrasts
            keys_csv <- paste0(ome, "::", all_contrasts_csv)
            Reduce(union, lapply(keys_csv, function(k) {
              c_poi    <- poi_reg_csv[[k]] %||% character(0)
              c_lm     <- lm_reg_csv[[k]] %||% character(0)
              c_n_top  <- tn_reg_csv[[k]] %||% 20L
              c_suffix <- sub(paste0("^", ome, "::"), "", k)
              cols <- tryCatch(
                if (sp$test == "One-sample Moderated T-test")
                  get_volcano_cols(df_raw, sp$test, c_suffix, NULL)
                else
                  get_volcano_cols(df_raw, sp$test, NULL, c_suffix),
                error = function(e) NULL
              )
              if (is.null(cols)) return(c_poi)
              df_c <- tryCatch(
                build_volcano_df(df_raw, cols, sig_cutoff, sig_stat),
                error = function(e) NULL
              )
              if (is.null(df_c)) return(c_poi)
              union(c_poi, volcano_labeled_feature_ids(df_c, c_lm, c_poi, c_n_top))
            }), init = character(0))
          },
          poi  # "none" — current contrast's POI only
        )
        # Force "poi" into label_mode when union is active and produced IDs.
        effective_label_mode_csv <- if (csv_union_mode != "none" && length(effective_poi_csv) > 0) {
          unique(c(label_mode_export, "poi"))
        } else {
          label_mode_export
        }

        all_ids <- character(0)

        if (test == "One-sample Moderated T-test") {
          groups <- sp$groups
          for (group in groups) {
            cols <- get_volcano_cols(df_raw, test, group, NULL)
            df_plot <- build_volcano_df(df_raw, cols, sig_cutoff, sig_stat)
            all_ids <- union(all_ids, volcano_labeled_feature_ids(df_plot, effective_label_mode_csv, effective_poi_csv, n_top_csv))
          }
        } else if (test == "Two-sample Moderated T-test") {
          contrasts <- sp$contrasts
          for (contrast in contrasts) {
            cols <- get_volcano_cols(df_raw, test, NULL, contrast)
            df_plot <- build_volcano_df(df_raw, cols, sig_cutoff, sig_stat)
            all_ids <- union(all_ids, volcano_labeled_feature_ids(df_plot, effective_label_mode_csv, effective_poi_csv, n_top_csv))
          }
        } else {
          message("Volcano labeled export not supported for test type: ", test)
          return()
        }

        if (length(all_ids) == 0) {
          message("Volcano labeled export: no features matched label criteria for ", ome)
          return()
        }

        id_col <- grep("^id$", colnames(df_raw), value = TRUE, ignore.case = TRUE)[1]
        if (is.na(id_col)) {
          message("Volcano labeled export skipped: no 'id' column in stat results for ", ome)
          return()
        }

        row_mask <- as.character(df_raw[[id_col]]) %in% all_ids
        out_rows <- df_raw[row_mask, , drop = FALSE]

        # Prepend resolved plot_label column so CSV mirrors on-plot labels
        lbl_src <- if (label_column_export %in% colnames(df_raw)) label_column_export else id_col
        resolved_labels <- resolve_volcano_label_text(
          df_raw[[lbl_src]][row_mask],
          split_enabled = label_split_export,
          separator     = label_sep_export
        )
        na_lbl <- is.na(resolved_labels) | !nzchar(resolved_labels)
        resolved_labels[na_lbl] <- as.character(df_raw[[id_col]])[row_mask][na_lbl]
        out_rows <- cbind(plot_label = resolved_labels, out_rows)

        out_path <- file.path(dir_name, paste0("volcano_labeled_features_", ome, ".csv"))
        write.csv(out_rows, file = out_path, row.names = FALSE)
        cat(
          "Saved", nrow(out_rows), "volcano-labeled feature row(s) for", ome, "to:", out_path, "\n"
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