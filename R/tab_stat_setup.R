################################################################################
# Module: Stat_Setup
#
# Allow users to setup the test type and parameters
################################################################################

################################################################################
# Shiny functions (UI and server)
################################################################################


# UI for the statSetup tab
statSetup_Tab_UI <- function(id = "statSetupTab") {
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("Test Setup"),
      # Main setup controls wrapped in renderUI
      uiOutput(ns("setup_controls"))
    )
  )}



# server for the statSetup tab
# contains the structure for the big tabbed box with omes
statSetup_Tab_Server <- function(id = "statSetupTab", GCTs_and_params, globals, parent = NULL){
  ## module function
  moduleServer(id, function (input, output, session) {
    
    # Main setup controls
    output$setup_controls <- renderUI({
      # This will trigger the validate() statements and show "GCTs not yet processed"
      req(GCTs(), parameters())
      
      # Get the ome names directly here
      ome_names <- names(GCTs())
      
        tagList(
        # Show warning prominently in main panel if annotation is not suitable
        uiOutput(ns("annotation_testing_warning_main")),
        
        fluidRow(
          column(2,
                 div(class = "stat-setup-controls",
                     selectInput(ns("selected_omes"), "Select datasets to test:", choices = ome_names, selected = default_ome()),
                     textOutput(ns("annotation_col")),
                     if (length(ome_names) > 1) {
                       checkboxInput(ns("apply_all"),"Apply to all datasets" , value=FALSE)
                     },
                     uiOutput(ns("run_test_button_ui"))
                 )
          ),
          column(2,
                 div(class = "stat-setup-controls",
                     uiOutput(ns("select_test")),
                     uiOutput(ns("select_groups_ui"))
                 )
          ),
          column(8,
                 uiOutput(ns("select_contrast_ui"))
          )
        ),
        
        br(),
        
        # Documentation section
        div(
          class = "well",
          h4("Statistical Testing Documentation"),
          
          # Important note about log transformation
          div(
            style = "background-color: #f8f9fa; border-left: 4px solid #007bff; padding: 12px; margin-bottom: 15px; border-radius: 0 4px 4px 0;",
            icon("info-circle", style = "color: #007bff; margin-right: 8px;"),
            strong("Note: ", style = "color: #495057;"),
            "Statistical tests require log-transformed data. Please ensure your data have been log-transformed.",
            style = "color: #495057;"
          ),
          
          h5(strong("Available Tests:"), style = "font-size: 16px; margin-top: 20px; margin-bottom: 10px;"),
          tags$ul(
            tags$li("None: Default option - no statistical testing performed on this dataset"),
            tags$li("One-sample Moderated T-test: Compare each feature to a reference value (typically 0) - only meaningful for ratio data"),
            tags$li("Two-sample Moderated T-test: Compare two groups of samples"),
            tags$li("F-test: Compare multiple groups of samples (one-way ANOVA)")
          ),
          
          h5(strong("Instructions:"), style = "font-size: 16px; margin-top: 20px; margin-bottom: 10px;"),
          tags$ol(
            tags$li("Select datasets and test type (or 'None' to skip testing)"),
            tags$li("Choose groups or contrasts for comparison"),
            tags$li("Click 'Run Test' to perform statistical analysis")
          ),
          p(strong("Note:"), " Use 'Apply to all datasets' checkbox to apply the same test settings to multiple datasets")
        )
      )
    })

    ## GATHERING INPUTS ##
    stat_param <- reactiveVal(list())
    stat_results <- reactiveVal(list())
    manual_control_groups <- reactiveVal(character(0))
    use_manual_controls <- reactiveVal(FALSE)
    group_view_mode <- reactiveVal("list")  # For one-sample t-test group selection
    
    # get namespace in case you need to use it in renderUI-like functions
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

    # vector of all groups in omes
    groups_in_all_omes <- reactive({
      base::Reduce(base::intersect, lapply(GCTs(), function(gct) names(gct@cdesc)))
    })

    # gather relevant variables from globals
    default_ome <- reactive(globals$default_ome) # don't remove this variable!
    custom_colors <- reactive(globals$colors)

### MODULE SERVER LOGIC ########################################################
    
    #OME THAT IS CURRENTLY SELECTED
    selected_ome <- reactive({ 
      req(input$selected_omes)
      input$selected_omes
    })



    #CDESC OF SELECTED OME
    cdesc <- reactive({ 
      req(GCTs(), selected_ome())
      GCTs()[[selected_ome()]]@cdesc
    })

    #DEFAULT ANNOTATION COLUMN FOR SELECTED OME
    default_annotation_column <- reactive({ 
      req(default_annotations(), selected_ome())
      default_annotations()[[selected_ome()]]
    })

    #DISPLAY ANNOTATION COLUMN
    output$annotation_col <- renderText({
      req(default_annotation_column())
      paste("Selected annotation column:", default_annotation_column())  
    })
    
    # Check if annotation column has enough categories for statistical testing
    # An annotation is suitable if:
    # 1. It has at least 2 unique categories
    # 2. It is NOT an ID column (where every value is unique, meaning 1 sample per group)
    annotation_suitable_for_testing <- reactive({
      req(cdesc(), default_annotation_column())
      annot_col <- default_annotation_column()
      values <- cdesc()[[annot_col]]
      
      # Get unique non-NA values
      choices <- unique(values)
      choices <- choices[!is.na(choices)]
      
      # Check if it's an ID column (every value is unique)
      non_na_values <- values[!is.na(values)]
      is_id_column <- length(non_na_values) == length(unique(non_na_values)) && 
                      length(non_na_values) > 0 &&
                      is.character(non_na_values)
      
      # Suitable if: has >=2 categories AND is not an ID column
      length(choices) >= 2 && !is_id_column
    })
    
    # Display warning prominently in main panel if annotation column is not suitable for testing
    output$annotation_testing_warning_main <- renderUI({
      req(default_annotation_column())
      
      suitable <- tryCatch({
        annotation_suitable_for_testing()
      }, error = function(e) {
        FALSE
      })
      
      if (!suitable) {
        annot_col <- default_annotation_column()
        values <- cdesc()[[annot_col]]
        non_na_values <- values[!is.na(values)]
        is_id_column <- length(non_na_values) == length(unique(non_na_values)) && 
                        length(non_na_values) > 0 &&
                        is.character(non_na_values)
        
        if (is_id_column) {
          warning_msg <- paste0(
            "The selected annotation column '", annot_col, 
            "' is an ID column where every value is unique (1 sample per group). ",
            "Statistical testing requires multiple samples per group for comparison."
          )
        } else {
          choices <- unique(values)
          choices <- choices[!is.na(choices)]
          warning_msg <- paste0(
            "The selected annotation column '", annot_col, 
            "' has fewer than 2 categories (currently ", length(choices), " category). ",
            "Statistical testing requires at least 2 groups for comparison."
          )
        }
        
        div(
          style = "color: #856404; padding: 20px; background-color: #fff3cd; border: 2px solid #ffc107; border-radius: 8px; margin-bottom: 20px; text-align: center;",
          icon("exclamation-triangle", style = "font-size: 48px; color: #856404; margin-bottom: 15px;"),
          h3(strong("Statistical Testing Not Available"), style = "color: #856404; margin-top: 10px; margin-bottom: 15px;"),
          p(warning_msg, style = "font-size: 16px; color: #856404; margin-bottom: 15px;"),
          p(
            strong("To change the annotation column: "),
            "Please return to the ", strong("Setup"), " tab using the button in the sidebar, ",
            "then select a different annotation column that has multiple samples per group.",
            style = "font-size: 16px; color: #856404; margin-top: 15px;"
          )
        )
      } else {
        return(NULL)
      }
    })

######APPLY TO ALL OMES#########################################################
    original_stat_param <- reactiveVal(NULL)
    
    observeEvent(input$apply_all, {
      # Check if required reactive values are available
      if (is.null(selected_ome()) || is.null(stat_param()) || is.null(all_omes()) || 
          is.null(default_annotations()) || is.null(default_annotation_column())) {
        showNotification("Please wait for the application to fully load before using 'Apply to all datasets'.", type = "warning", duration = 3)
        updateCheckboxInput(session, "apply_all", value = FALSE)
        return()
      }
      
      current <- stat_param()
      ome_source <- selected_ome()
      ome_list <- all_omes()
      
      if (input$apply_all) {
        
          # Check default annotation columns are identical for all omes
          if (!all(sapply(default_annotations(), identical, default_annotation_column()))) {
            showNotification("Default annotation columns differ across datasets. Cannot apply settings to all.", type = "error", duration = 5)
            updateCheckboxInput(session, "apply_all", value=FALSE)
            return()
          }
          
          # Save original parameters before overwriting
          if (is.null(original_stat_param())) {
            original_stat_param(stat_param())
          }
          
          # Only copy if source ome parameters aren't empty
          if (!is.null(current[[ome_source]])) {
            for (ome in ome_list) {
              if (ome != ome_source) {
                current[[ome]] <- current[[ome_source]]
              }
            }
            stat_param(current)
            showNotification("Applied current dataset's parameters to all datasets.", type = "message", duration = 3)
          } else {
            showNotification("No parameters set for current dataset. Please configure parameters first.", type = "warning", duration = 3)
            updateCheckboxInput(session, "apply_all", value = FALSE)
          }
      } else {
        # Revert to original parameters if button unclicked
        if (!is.null(original_stat_param())) {
          stat_param(original_stat_param())
          original_stat_param(NULL)
          showNotification("Reverted to original parameters for each dataset.", type = "message", duration = 3)
        }
      }
    })
    
    
################################################################################
######TEST SELECTION############################################################
    #saving the selected test to stat_param
    observeEvent(input$select_test, {
      req(selected_ome())
      current <- stat_param()           
      ome <- selected_ome()
      
      # Check if annotation is suitable for testing
      suitable <- annotation_suitable_for_testing()
      
      # If annotation is not suitable and user tries to select a test other than "None", reset to "None"
      if (!suitable && input$select_test != "None") {
        showNotification(
          "Statistical testing is not available with the current annotation column. Please select a different annotation column in the Setup tab.",
          type = "warning",
          duration = 5
        )
        updateSelectInput(session, "select_test", selected = "None")
        return()
      }
      
      if (is.null(current[[ome]])) {current[[ome]] <- list()}
      
      current[[ome]]$test <- input$select_test
      
      # Only set stat and cutoff if not already set
      if (is.null(current[[ome]]$stat)) current[[ome]]$stat <- "adj.p.val"
      if (is.null(current[[ome]]$cutoff)) current[[ome]]$cutoff <- 0.05
      
      # Initialize groups if test is not "None"
      if (input$select_test != "None" && is.null(current[[ome]]$groups)) {
        req(cdesc(), default_annotation_column())
        choices <- unique(cdesc()[[default_annotation_column()]])
        choices <- choices[!is.na(choices)]
        current[[ome]]$groups <- choices
      }
      
      stat_param(current) 
    })
    
    #displaying the test choices
    output$select_test <- renderUI ({
      req(selected_ome())
      current <- stat_param()
      ome <- selected_ome()
      
      if (is.null(current[[ome]]$test)) {
        current[[ome]]$test <- "None"
        stat_param(current)
      }
      
      # Check if annotation is suitable for testing
      suitable <- tryCatch({
        annotation_suitable_for_testing()
      }, error = function(e) {
        FALSE
      })
      
      # If annotation is not suitable, reset test to "None" and only show "None" as option
      if (!suitable) {
        if (current[[ome]]$test != "None") {
          current[[ome]]$test <- "None"
          stat_param(current)
        }
        return(
          selectInput(ns("select_test"), 
                      "Select test:", 
                      choices = "None", 
                      selected = "None"
          )
        )
      }
      
      # Get intensity parameter for this ome
      intensity_param <- parameters()[[ome]]$intensity_data
      
      # Define test choices - exclude One-sample t-test for intensity data
      test_choices <- c("None", "Two-sample Moderated T-test", "Moderated F test")
      if (is.null(intensity_param) || intensity_param != "Yes") {
        test_choices <- c("None", "One-sample Moderated T-test", "Two-sample Moderated T-test", "Moderated F test")
      }
      
      selected_test <- current[[ome]]$test
      # If current test is One-sample and intensity is Yes, reset to None
      if (current[[ome]]$test == "One-sample Moderated T-test" && !is.null(intensity_param) && intensity_param == "Yes") {
        current[[ome]]$test <- "None"
        stat_param(current)
        selected_test <- "None"
      }
      
      selectInput(ns("select_test"), 
                  "Select test:", 
                  choices = test_choices, 
                  selected = selected_test
      )
    })
    
    # Render run test button with conditional enabling
    output$run_test_button_ui <- renderUI({
      suitable <- tryCatch({
        annotation_suitable_for_testing()
      }, error = function(e) {
        FALSE
      })
      
      if (suitable) {
        actionButton(ns("run_test_button"), "Run Test", class = "btn btn-primary")
      } else {
        actionButton(ns("run_test_button"), "Run Test", class = "btn btn-primary", disabled = TRUE)
      }
    })
    
################################################################################
######GROUP SELECTION############################################################
    #saving the selected groups to stat_param
    observeEvent(input$select_groups, {
      req(selected_ome())
      current <- stat_param()           
      ome <- selected_ome()                
      
      if (is.null(current[[ome]])) {current[[ome]] <- list()}
      
      new_groups <- input$select_groups
      old_groups <- current[[ome]]$groups
      current[[ome]]$groups <- new_groups

      #resets the contrasts to default if a group is selected/deselected (two-sample only)
      if (current[[ome]]$test=="Two-sample Moderated T-test"){
        if (is.null(current[[ome]]$contrasts) || !setequal(old_groups, new_groups)) {
          if (length(current[[ome]]$groups) < 2 || is.null(current[[ome]]$groups)) {
            showNotification("Please select at least two groups.", type = "error", duration = 5)
            return()
          }
  
          pairwise_contrasts <- combn(new_groups, 2, simplify = FALSE)
          all_pairs <- c(pairwise_contrasts, lapply(pairwise_contrasts, rev))
          labels <- sapply(all_pairs, function(p) paste(p[1], "/", p[2]))
  
          current[[ome]]$contrasts <- labels
        }
      }
      
      stat_param(current)    
    })
    
    #displaying the group choices
    output$select_groups_ui <- renderUI({
      current <- stat_param()
      ome <- selected_ome()
      req(cdesc(), default_annotation_column(),selected_ome(), stat_param())

      # Only show groups if a test other than "None" is selected
      if (is.null(current[[ome]]$test) || current[[ome]]$test == "None") {
        return(NULL)  # Don't show anything if no test or "None" test
      }
      
      # Check if annotation is suitable for testing
      suitable <- annotation_suitable_for_testing()
      if (!suitable) {
        return(NULL)  # Don't show groups if annotation is not suitable
      }

      choices<- unique(cdesc()[[default_annotation_column()]])
      choices <- choices[!is.na(choices)]

      # Initialize groups to empty for one-sample t-test and F-test, all groups for two-sample
      if (is.null(current[[ome]]$groups)) {
        if (current[[ome]]$test %in% c("One-sample Moderated T-test", "Moderated F test")) {
          current[[ome]]$groups <- character(0)
        } else {
          current[[ome]]$groups <- choices
        }
        stat_param(current)
      }

      # For two-sample t-test, use standard pickerInput in narrow column
      # For one-sample t-test and F-test, group selection will be shown in wide column
      if (current[[ome]]$test == "Two-sample Moderated T-test") {
        pickerInput(
          ns("select_groups"),
          "Select groups:",
          choices = choices,
          selected = current[[ome]]$groups,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            selectAllText = "Select All",
            deselectAllText = "Deselect All",
            noneSelectedText = "No groups selected"
          )
        )
      } else {
        # For one-sample t-test and F-test, return NULL - group selection shown in wide column
        return(NULL)
      }
    })

    # Observer for one-sample t-test view mode toggle (KEPT FOR FUTURE USE)
    # observeEvent(input$group_view_toggle, {
    #   group_view_mode(input$group_view_toggle)
    # })

    # Render group selection view for one-sample t-test and F-test
    output$group_selection_view <- renderUI({
      current <- stat_param()
      ome <- selected_ome()
      req(cdesc(), default_annotation_column())

      choices <- unique(cdesc()[[default_annotation_column()]])
      choices <- choices[!is.na(choices)]
      
      # Initialize groups if null, but allow empty groups
      if (is.null(current[[ome]]$groups)) {
        current[[ome]]$groups <- character(0)
        stat_param(current)
      }
      
      selected_groups <- current[[ome]]$groups %||% character(0)

      # Always use checkbox view
      render_group_selection_matrix(choices, selected_groups, ns)

      # LIST VIEW CODE (KEPT FOR FUTURE USE):
      # if (group_view_mode() == "matrix") {
      #   # Matrix view
      #   render_group_selection_matrix(choices, selected_groups, ns)
      # } else {
      #   # List view
      #   pickerInput(
      #     ns("select_groups_list"),
      #     label = "Select groups to test:",
      #     choices = choices,
      #     selected = selected_groups,
      #     multiple = TRUE,
      #     options = pickerOptions(
      #       actionsBox = TRUE,
      #       liveSearch = TRUE,
      #       liveSearchPlaceholder = "Search groups...",
      #       selectAllText = "Select All",
      #       deselectAllText = "Deselect All",
      #       noneSelectedText = "No groups selected",
      #       virtualScroll = if (length(choices) > 50) 50 else FALSE,
      #       size = 10
      #     ),
      #     width = "100%"
      #   )
      # }
    })

    # Handle group checkbox changes (one-sample t-test and F-test)
    observe({
      current <- stat_param()
      ome <- selected_ome()
      req(cdesc(), default_annotation_column(), selected_ome())
      
      # Only process if test is one-sample t-test or F-test
      if (is.null(current[[ome]]$test) || 
          !current[[ome]]$test %in% c("One-sample Moderated T-test", "Moderated F test")) {
        return(NULL)
      }
      
      choices <- unique(cdesc()[[default_annotation_column()]])
      choices <- choices[!is.na(choices)]
      
      # Collect selected groups from checkboxes
      selected_groups <- character(0)
      for (group in choices) {
        checkbox_id <- paste0("group_checkbox_", gsub("[^a-zA-Z0-9_]", "_", group))
        checkbox_value <- input[[checkbox_id]]
        if (!is.null(checkbox_value) && isTRUE(checkbox_value)) {
          selected_groups <- c(selected_groups, group)
        }
      }
      
      # Update stat_param only if selection changed
      if (!identical(sort(current[[ome]]$groups %||% character(0)), sort(selected_groups))) {
        current[[ome]]$groups <- selected_groups
        stat_param(current)
      }
    })
    
    # Handle Select All button for group selection (one-sample t-test and F-test)
    observeEvent(input$group_select_all, {
      req(selected_ome())
      current <- stat_param()
      ome <- selected_ome()
      
      # Only process if test is one-sample t-test or F-test
      if (is.null(current[[ome]]$test) || 
          !current[[ome]]$test %in% c("One-sample Moderated T-test", "Moderated F test")) {
        return(NULL)
      }
      
      req(cdesc(), default_annotation_column())
      choices <- unique(cdesc()[[default_annotation_column()]])
      choices <- choices[!is.na(choices)]
      
      # Select all groups
      current[[ome]]$groups <- choices
      stat_param(current)
      
      # Update all checkboxes to checked
      for (group in choices) {
        checkbox_id <- paste0("group_checkbox_", gsub("[^a-zA-Z0-9_]", "_", group))
        updateCheckboxInput(session, checkbox_id, value = TRUE)
      }
    })
    
    # Handle Clear All button for group selection (one-sample t-test and F-test)
    observeEvent(input$group_clear_all, {
      req(selected_ome())
      current <- stat_param()
      ome <- selected_ome()
      
      # Only process if test is one-sample t-test or F-test
      if (is.null(current[[ome]]$test) || 
          !current[[ome]]$test %in% c("One-sample Moderated T-test", "Moderated F test")) {
        return(NULL)
      }
      
      req(cdesc(), default_annotation_column())
      choices <- unique(cdesc()[[default_annotation_column()]])
      choices <- choices[!is.na(choices)]
      
      # Clear all groups
      current[[ome]]$groups <- character(0)
      stat_param(current)
      
      # Update all checkboxes to unchecked
      for (group in choices) {
        checkbox_id <- paste0("group_checkbox_", gsub("[^a-zA-Z0-9_]", "_", group))
        updateCheckboxInput(session, checkbox_id, value = FALSE)
      }
    })

    # Handle pickerInput selection for one-sample t-test (KEPT FOR FUTURE USE)
    # observeEvent(input$select_groups_list, {
    #   req(selected_ome())
    #   current <- stat_param()
    #   ome <- selected_ome()
    #
    #   current[[ome]]$groups <- input$select_groups_list
    #   stat_param(current)
    # })


################################################################################
######MANUAL CONTROL GROUP SELECTION############################################
    # Observer for manual control group checkbox
    observeEvent(input$use_manual_controls, {
      use_manual_controls(input$use_manual_controls)

      # Clear manual selections when unchecking
      if (!input$use_manual_controls) {
        manual_control_groups(character(0))
      }
    })

    # Observer for control group badge clicks
    observeEvent(input$control_group_click, {
      req(input$control_group_click)

      clicked_group <- input$control_group_click
      current_controls <- manual_control_groups()

      # Toggle selection
      if (clicked_group %in% current_controls) {
        manual_control_groups(setdiff(current_controls, clicked_group))
      } else {
        manual_control_groups(c(current_controls, clicked_group))
      }
    })

################################################################################
######CONTRAST SELECTION########################################################
    # Dropdown selector is not so efficient for experiments with a large number of possible contrast combinations
    # A checklist will nevertheless overflow the screen with poor readability 
    # Here, a matrix selector is implemented to overcome the issue 

    # Reactive to track contrast selection view mode (matrix or list)
    contrast_view_mode <- reactiveVal("matrix")

    # Observer for view mode toggle (KEPT FOR FUTURE USE)
    # Matrix view is default for > 10 groups
    # List view is default for =< 10 groups - enhanced dropdown selector with search function
    # A smart control detector (beta) is implemented to auto-identify control groups (e.g., control, WT, etc.)
    # observeEvent(input$contrast_view_toggle, {
    #   contrast_view_mode(input$contrast_view_toggle)
    # })

    # Quick select buttons
    # Smart control detector (beta)
    observeEvent(input$contrast_quick_all_pairwise, {
      req(selected_ome())
      current <- stat_param()
      ome <- selected_ome()

      if (!is.null(current[[ome]]$groups) && length(current[[ome]]$groups) >= 2) {
        all_contrasts <- generate_all_pairwise(current[[ome]]$groups, bidirectional = TRUE)
        current[[ome]]$contrasts <- all_contrasts
        stat_param(current)
      }
    })

    observeEvent(input$contrast_quick_all_vs_control, {
      req(selected_ome())
      current <- stat_param()
      ome <- selected_ome()

      if (!is.null(current[[ome]]$groups) && length(current[[ome]]$groups) >= 2) {
        # Require manual control selection - no auto-detection
        if (use_manual_controls() && length(manual_control_groups()) > 0) {
          # Use manually selected control groups
          control_groups <- manual_control_groups()
          # Only generate contrasts where control is in denominator (bidirectional = FALSE)
          control_contrasts <- generate_all_vs_multiple_references(
            current[[ome]]$groups,
            control_groups,
            bidirectional = FALSE
          )
          current[[ome]]$contrasts <- control_contrasts
          stat_param(current)

          control_msg <- if (length(control_groups) == 1) {
            paste0("Selected all contrasts vs '", control_groups[1], "'")
          } else {
            paste0("Selected all contrasts vs controls: ", paste(control_groups, collapse = ", "))
          }
          showNotification(control_msg, type = "message", duration = 3)
        } else {
          # No control groups selected - show error message
          showNotification(
            "Please check 'Manually specify control group(s)' and select at least one control group before using 'All vs Control'.",
            type = "error",
            duration = 5
          )
        }
      }
    })

    # Quick sequential selection automatically generates contrasts between consecutive groups in a sequence 
    # For example, if the groups are ordered as ["Time_1", "Time_2", "Time_3"], "quick sequential" would automatically create: Time_2/Time_1, Time_3/Time_2 (later/earlier)
    # This is useful for time-series or ordered experimental designs where you want to compare later timepoints/conditions to earlier ones
    observeEvent(input$contrast_quick_sequential, {
      req(selected_ome())
      current <- stat_param()
      ome <- selected_ome()

      if (!is.null(current[[ome]]$groups) && length(current[[ome]]$groups) >= 2) {
        sequential_contrasts <- generate_sequential_pairs(
          current[[ome]]$groups,
          bidirectional = FALSE
        )
        current[[ome]]$contrasts <- sequential_contrasts
        stat_param(current)
      }
    })

    # Clear all selections
    observeEvent(input$contrast_quick_clear, {
      req(selected_ome())
      current <- stat_param()
      ome <- selected_ome()

      current[[ome]]$contrasts <- character(0)
      stat_param(current)
    })

    # Handle matrix cell clicks
    observeEvent(input$contrast_matrix_click, {
      req(selected_ome(), input$contrast_matrix_click)
      current <- stat_param()
      ome <- selected_ome()

      clicked_contrast <- input$contrast_matrix_click

      if (is.null(current[[ome]]$contrasts)) {
        current[[ome]]$contrasts <- character(0)
      }

      # Toggle selection
      if (clicked_contrast %in% current[[ome]]$contrasts) {
        current[[ome]]$contrasts <- setdiff(current[[ome]]$contrasts, clicked_contrast)
      } else {
        current[[ome]]$contrasts <- c(current[[ome]]$contrasts, clicked_contrast)
      }

      stat_param(current)
    })

    # Handle pickerInput selection (list view) - KEPT FOR FUTURE USE
    # observeEvent(input$select_contrasts_list, {
    #   req(selected_ome())
    #   current <- stat_param()
    #   ome <- selected_ome()
    #
    #   current[[ome]]$contrasts <- input$select_contrasts_list
    #   stat_param(current)
    # })

    # Handle remove contrast from summary panel
    observeEvent(input$remove_contrast, {
      req(selected_ome(), input$remove_contrast)
      current <- stat_param()
      ome <- selected_ome()

      contrast_to_remove <- input$remove_contrast
      current[[ome]]$contrasts <- setdiff(current[[ome]]$contrasts, contrast_to_remove)
      stat_param(current)
    })

    # Display contrast choices (or group selection for one-sample/F-test)
    output$select_contrast_ui <- renderUI({
      current <- stat_param()
      ome <- selected_ome()
      test_type <- current[[ome]]$test
      
      # Check if annotation is suitable for testing
      suitable <- annotation_suitable_for_testing()
      if (!suitable || test_type == "None") {
        return(NULL)
      }

      # Show group selection for one-sample t-test and F-test in wide column
      if (test_type %in% c("One-sample Moderated T-test", "Moderated F test")) {
        req(cdesc(), default_annotation_column())
        
        choices <- unique(cdesc()[[default_annotation_column()]])
        choices <- choices[!is.na(choices)]
        
        # Initialize groups if null, but allow empty groups
        if (is.null(current[[ome]]$groups)) {
          current[[ome]]$groups <- character(0)
          stat_param(current)
        }
        
        selected_groups <- current[[ome]]$groups %||% character(0)
        
        # Show group selection in wide column
        tagList(
          div(
            class = "group-selection-section",
            uiOutput(ns("group_selection_view"))
          )
        )
      } else if (test_type == "Two-sample Moderated T-test") {
        # Show contrast selection for two-sample t-test

      if (length(current[[ome]]$groups) < 2 || is.null(current[[ome]]$groups)) {
        return(div(
          style = "color: #d9534f; padding: 10px; background-color: #f2dede; border: 1px solid #ebccd1; border-radius: 4px;",
          icon("exclamation-triangle"),
          " Need at least 2 groups to perform two-sample t-test"
        ))
      }

      groups <- current[[ome]]$groups
      n_groups <- length(groups)

      # Generate all possible contrasts
      pairwise_contrasts <- combn(groups, 2, simplify = FALSE)
      all_pairs <- c(pairwise_contrasts, lapply(pairwise_contrasts, rev))
      all_contrast_labels <- sapply(all_pairs, function(p) paste(p[1], "/", p[2]))

      # Initialize contrasts if not set
      if (is.null(current[[ome]]$contrasts)) {
        current[[ome]]$contrasts <- all_contrast_labels
        stat_param(current)
      }

      selected_contrasts <- current[[ome]]$contrasts

      # Always use matrix view for simplified UX (list view code kept for future use)
      contrast_view_mode("matrix")

      tagList(
        div(
          class = "contrast-selection-container",

          # Manual control group selection
          div(
            class = "manual-control-section",
            style = "margin-bottom: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 4px;",
            checkboxInput(
              ns("use_manual_controls"),
              "Manually specify control group(s)",
              value = use_manual_controls()  # Maintain checkbox state across re-renders
            ),
            uiOutput(ns("control_group_selector_ui"))
          ),

          # Quick select buttons
          div(
            class = "contrast-quick-buttons",
            h5("Quick Select:", style = "margin: 0 10px 0 0; display: inline-block;"),
            actionButton(
              ns("contrast_quick_all_pairwise"),
              "All Pairwise",
              class = "btn-sm btn-default",
              icon = icon("th")
            ),
            uiOutput(ns("all_vs_control_button")),
            actionButton(
              ns("contrast_quick_sequential"),
              "Sequential Pairs",
              class = "btn-sm btn-default",
              icon = icon("arrow-right")
            ),
            actionButton(
              ns("contrast_quick_clear"),
              "Clear All",
              class = "btn-sm btn-primary",
              icon = icon("times")
            )
          ),

          # View mode toggle (REMOVED FOR SIMPLIFIED UX - code kept for future use)
          # div(
          #   class = "contrast-view-toggle",
          #   radioButtons(
          #     ns("contrast_view_toggle"),
          #     label = NULL,
          #     choices = c("Matrix View" = "matrix", "List View" = "list"),
          #     selected = contrast_view_mode(),
          #     inline = TRUE
          #   )
          # ),

          # Matrix or List view
          uiOutput(ns("contrast_selection_view")),

          # Selected contrasts summary
          uiOutput(ns("selected_contrasts_summary"))
        )
      )
      } else {
        # For "None" test or other cases, return NULL
        return(NULL)
      }
    })

    # Render the appropriate selection view
    output$contrast_selection_view <- renderUI({
      current <- stat_param()
      ome <- selected_ome()
      req(current[[ome]]$groups)

      groups <- current[[ome]]$groups
      selected_contrasts <- current[[ome]]$contrasts %||% character(0)

      # Always use matrix view (list view code kept for future use)
      render_contrast_matrix(groups, selected_contrasts, ns)

      # LIST VIEW CODE (KEPT FOR FUTURE USE):
      # if (contrast_view_mode() == "matrix") {
      #   # Matrix view
      #   render_contrast_matrix(groups, selected_contrasts, ns)
      # } else {
      #   # List view
      #   pairwise_contrasts <- combn(groups, 2, simplify = FALSE)
      #   all_pairs <- c(pairwise_contrasts, lapply(pairwise_contrasts, rev))
      #   all_labels <- sapply(all_pairs, function(p) paste(p[1], "/", p[2]))
      #
      #   pickerInput(
      #     ns("select_contrasts_list"),
      #     label = "Select contrasts:",
      #     choices = all_labels,
      #     selected = selected_contrasts,
      #     multiple = TRUE,
      #     options = pickerOptions(
      #       actionsBox = TRUE,
      #       liveSearch = TRUE,
      #       liveSearchPlaceholder = "Search contrasts...",
      #       selectAllText = "Select All",
      #       deselectAllText = "Deselect All",
      #       noneSelectedText = "No contrasts selected",
      #       virtualScroll = if (length(all_labels) > 50) 50 else FALSE,
      #       size = 10
      #     ),
      #     width = "100%"
      #   )
      # }
    })

    # Render control group selector
    output$control_group_selector_ui <- renderUI({
      # Only show if checkbox is checked
      if (!isTRUE(input$use_manual_controls)) {
        return(NULL)
      }

      current <- stat_param()
      ome <- selected_ome()
      req(current[[ome]]$groups)

      groups <- current[[ome]]$groups
      selected_controls <- manual_control_groups()

      render_control_group_selector(groups, selected_controls, ns)
    })
    
    # Render "All vs Control" button with reactive disabled state
    output$all_vs_control_button <- renderUI({
      # Disable button if manual controls not enabled or no control groups selected
      is_disabled <- !(use_manual_controls() && length(manual_control_groups()) > 0)
      
      actionButton(
        ns("contrast_quick_all_vs_control"),
        "All vs Control",
        class = "btn-sm btn-default",
        icon = icon("bullseye"),
        disabled = is_disabled
      )
    })

    # Render selected contrasts summary
    output$selected_contrasts_summary <- renderUI({
      current <- stat_param()
      ome <- selected_ome()
      req(current[[ome]]$groups)

      selected_contrasts <- current[[ome]]$contrasts %||% character(0)
      n_total <- length(generate_all_pairwise(current[[ome]]$groups, bidirectional = TRUE))
      n_selected <- length(selected_contrasts)

      if (n_selected == 0) {
        return(div(
          class = "selected-contrasts-panel",
          div(class = "selected-contrasts-empty", "No contrasts selected")
        ))
      }

      # Create contrast badges
      contrast_badges <- lapply(selected_contrasts, function(contrast) {
        tags$span(
          class = "contrast-badge",
          contrast,
          tags$span(
            class = "remove-btn",
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                            ns("remove_contrast"), contrast),
            icon("times")
          )
        )
      })

      div(
        class = "selected-contrasts-panel",
        div(
          class = "panel-header",
          "Selected Contrasts ",
          span(class = "contrast-count", paste0("(", n_selected, " of ", n_total, ")"))
        ),
        div(contrast_badges)
      )
    })



################################################################################
##FLIPPING THE CONTRASTS- not implemented but can be in the future##############
    # observe({
    #   req(input$selected_ome)
    #   req(input$select_groups)
    #   if (length(input$select_groups) < 2) return(list())
    #   
    #   pairwise_contrasts <- combn(input$select_groups, 2, simplify = FALSE)
    #   all_pairs <- c(pairwise_contrasts, lapply(pairwise_contrasts, rev))
    #   
    #   selected_contrasts <- list()
    #   for (i in seq_along(all_pairs)) {
    #     contrast_id <- paste0("contrast_", i)
    #     if (isTRUE(input[[contrast_id]])) {
    #       selected_contrasts[[length(selected_contrasts) + 1]] <- all_pairs[[i]]
    #     }
    #   }
    #   contrast_store[[input$selected_ome]] <- selected
    # })
    
    # #selecting the contrast
    # output$select_contrast_ui <- renderUI({
    #   req(input$select_test == "Two-sample Moderated T-test")
    #   req(length(input$select_groups) >= 2)
    #    
    #   #create pairwise comparisons
    #   pairwise_contrasts <- combn(input$select_groups, 2, simplify = FALSE)
    #    
    #   lapply(seq_along(pairwise_contrasts), function(i) {
    #     pair <- pairwise_contrasts[[i]]
    #     flip_id <- ns(paste0("flip_", i))
    #     contrast_id <- ns(paste0("contrast_", i))
    #      
    #     fluidRow(
    #       column(5,
    #          uiOutput(ns(paste0("label_contrast_", i)))
    #       ),
    #       column(5,
    #          checkboxInput(flip_id, "Flip", FALSE)
    #       )
    #     )
    #   })
    # })
    #  
    # #flip the contrast over if the flip box is clicked
    #  observe({
    #    req(input$select_test == "Two-sample Moderated T-test")
    #    pairwise_contrasts <- combn(input$select_groups, 2, simplify = FALSE)
    #    
    #    for (i in seq_along(pairwise_contrasts)) {
    #      local({
    #        j <- i
    #        pair <- pairwise_contrasts[[j]]
    #        flip_input_id <- paste0("flip_", j)
    #        label_output_id <- paste0("label_contrast_", j)
    #        contrast_input_id <- paste0("contrast_", j)
    #        
    #        output[[label_output_id]] <- renderUI({
    #          flipped <- input[[flip_input_id]]
    #          label <- if (!is.null(flipped) && flipped) {
    #            paste(pair[2], "over", pair[1])
    #          } else {
    #            paste(pair[1], "over", pair[2])
    #          }
    #          checkboxInput(ns(contrast_input_id), label, TRUE)
    #        })
    #      })
    #    }
    #  })
    #  
    #  #final contrasts passed into the function
    #  selected_contrasts_reactive <- reactive({
    #    req(input$select_groups)
    #    if(length(input$select_groups) < 2) {
    #      return(list())  # Return empty list when fewer than 2 groups selected
    #    }
    #    
    #    selected_contrasts <- list()
    #    pairwise_contrasts <- combn(input$select_groups, 2, simplify = FALSE)
    #    
    #    for (i in seq_along(pairwise_contrasts)) {
    #      pair <- pairwise_contrasts[[i]]
    #      flip_id <- paste0("flip_", i)
    #      contrast_id <- paste0("contrast_", i)
    #      
    #      if (isTRUE(input[[contrast_id]])) {
    #        flipped <- isTRUE(input[[flip_id]])
    #        contrast <- if (flipped) rev(pair) else pair
    #        selected_contrasts[[length(selected_contrasts) + 1]] <- contrast
    #      }
    #    }
    #    selected_contrasts
    #  })
     
################################################################################
######TESTS RUN AFTER RUN BUTTON CLICKED########################################
    observeEvent(input$run_test_button, {
        req(GCTs(), default_annotations())
        param_list <- stat_param()
        gcts <- GCTs()
        
        # Check if annotation is suitable for testing for selected ome
        ome <- selected_ome()
        if (!annotation_suitable_for_testing()) {
          showNotification(
            paste0("Statistical testing is not available. The annotation column '", 
                   default_annotation_column(), 
                   "' has fewer than 2 categories. Please select a different annotation column in the Setup tab."),
            type = "error",
            duration = 10
          )
          return()
        }

        test_results<- list()
          
        for (ome in names(param_list)) {
          test <- param_list[[ome]]$test
          groups <- param_list[[ome]]$groups
          annotation_col <- default_annotations()[[ome]]
          contrasts <- param_list[[ome]]$contrasts
          
          # Skip if test is None
          if (test == "None") {
            next
          }
          
          # Check if this ome's annotation column has enough categories
          ome_choices <- unique(gcts[[ome]]@cdesc[[annotation_col]])
          ome_choices <- ome_choices[!is.na(ome_choices)]
          if (length(ome_choices) < 2) {
            showNotification(
              paste0("Skipping ", ome, ": annotation column '", annotation_col, 
                     "' has fewer than 2 categories."),
              type = "warning",
              duration = 5
            )
            next
          }

          contrasts_list <- NULL
          if (!is.null(contrasts)) {
            contrasts_list <- lapply(contrasts, function(x) strsplit(x, " / ")[[1]])
          }
            
          # Ensure proper number of contrasts for two sample t test
          if (test == "Two-sample Moderated T-test") {
            if (length(groups) < 2) {
              showNotification("Please select at least two groups for Two-sample test", type = "error")
              next
            }
            if (is.null(contrasts_list) || length(contrasts_list) == 0) {
              showNotification("Please select at least one contrast", type = "error")
              next
            }
          }
          
          # Ensure proper number of groups for f test 
          if (test== "Moderated F test" && length(groups) < 2) {
            showNotification("Please select at least two groups for F test", type = "error")
            next
          }
          
          # Ensure proper number of groups for one sample t test 
          if (test== "One-sample Moderated T-test" && length(groups) < 1) {
            showNotification("Please select at least one group for one sample t test", type = "error")
            next
          }
          
          # Run test  
          stat.results <- NULL
          my_shinyalert_tryCatch(
            text.error = paste0("<b>Statistical Test Failed for ", ome, ":</b>"),
            append.error = TRUE,
            show.error = TRUE,
            return.error = NULL,
            expr = {
              # Get intensity parameter from the processing parameters
              intensity_param <- parameters()[[ome]]$intensity
              if (is.null(intensity_param)) {
                intensity_param <- FALSE  # Default fallback if not specified
              }
              
              stat.results <- stat.testing(
                test = test,
                annotation_col = annotation_col,
                chosen_omes = ome,
                gct = gcts,
                chosen_groups = groups,
                selected_contrasts = contrasts_list,
                intensity = intensity_param
              )
            }
          )
            
          # Save results for that ome into test_results list  
          if (!is.null(stat.results)) {
            test_results[[ome]] <- as.data.frame(stat.results[[ome]])
          }
         
        }
        
        stat_results(test_results)

        # Check if tests completed successfully and switch to Summary tab
        if (length(test_results) > 0) {
          # Show success notification
          showNotification(
            "Statistical testing completed successfully! Switching to Summary tab...",
            type = "default",
            duration = 5  # Auto-dismiss after 5 seconds
          )

          # Auto-navigate to Statistics > Summary tab
          # Use parent session parameter to update main navbar
          updateNavbarPage(
            session = parent,
            inputId = "navbar-tabs",
            selected = "Statistics-Summary"
          )
        }
    })
    
    # Return the reactive values so other modules can access them
    return(list(
      stat_params = stat_param,
      stat_results = stat_results
    ))
  })
}
