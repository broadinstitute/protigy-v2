################################################################################
# Module: Stat_Setup
#
# Allow users to setup the test type and parameters
################################################################################

################################################################################
# Shiny functions (UI and server)
################################################################################

source("R/tab_stat_setup_helpers.R")

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
statSetup_Tab_Server <- function(id = "statSetupTab",GCTs_and_params, globals){
  ## module function
  moduleServer(id, function (input, output, session) {
    
    # Main setup controls
    output$setup_controls <- renderUI({
      # This will trigger the validate() statements and show "GCTs not yet processed"
      req(GCTs(), parameters())
      
      # Get the ome names directly here
      ome_names <- names(GCTs())
      
      tagList(
        # Warning about log transformation - only show after GCTs are processed
        div(
          style = "background-color: #f8f9fa; border-left: 4px solid #007bff; padding: 12px; margin-bottom: 20px; border-radius: 0 4px 4px 0;",
          icon("info-circle", style = "color: #007bff; margin-right: 8px;"),
          strong("Note: ", style = "color: #495057;"),
          "Statistical tests require log-transformed data. Please ensure your data have been log-transformed.",
          style = "color: #495057;"
        ),
        
        fluidRow(
          column(3,
                 selectInput(ns("selected_omes"), "Select datasets to test:", choices = ome_names, selected = default_ome()),
                 textOutput(ns("annotation_col")),
                 checkboxInput(ns("apply_all"),"Apply to all datasets" , value=FALSE),
                 actionButton(ns("run_test_button"),"Run Test")
          ),
          column(3,
                 uiOutput(ns("select_test")),
                 uiOutput(ns("select_groups_ui"))
          ),
          column(3,
                 uiOutput(ns("select_contrast_ui"))
          )
        )
      )
    })

    ## GATHERING INPUTS ##
    stat_param <- reactiveVal(list())
    stat_results <- reactiveVal(list())
    
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
      current <- stat_param()
      ome <- selected_ome()
      
      if (is.null(current[[ome]]$test)) {
        current[[ome]]$test <- "None"
        stat_param(current)
      }
      
      selectInput(ns("select_test"), 
                  "Select test:", 
                  choices= c("None","One-sample Moderated T-test","Two-sample Moderated T-test","Moderated F test"), 
                  selected= current[[ome]]$test
      )
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
      
      choices<- unique(cdesc()[[default_annotation_column()]])
      choices <- choices[!is.na(choices)]
      
      if (is.null(current[[ome]]$groups)) {
        current[[ome]]$groups <- choices 
        stat_param(current)
      }
      
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
    })
    

    
################################################################################
######CONTRAST SELECTION########################################################
    #saving the selected contrasts to stat_param
    observeEvent(input$select_contrasts, {
      req(selected_ome())
      current <- stat_param()
      ome <- selected_ome()

      current[[ome]]$contrasts <- input$select_contrasts
      stat_param(current)
    })
    
    #displaying the contrast choices
    output$select_contrast_ui <- renderUI({
      current <- stat_param()
      ome <- selected_ome()
      
      req(current[[ome]]$test=="Two-sample Moderated T-test")
      if (length(current[[ome]]$groups) < 2 || is.null(current[[ome]]$groups)) stop("need at least 2 groups to perform two-sample t-test")
      
      pairwise_contrasts <- combn(current[[ome]]$groups, 2, simplify = FALSE)
      all_pairs <- c(pairwise_contrasts, lapply(pairwise_contrasts, rev))
      labels <- sapply(all_pairs, function(p) paste(p[1], "/", p[2]))
      
      if (is.null(current[[ome]]$contrasts)) {
        current[[ome]]$contrasts <- labels
        stat_param(current)
      }
      
      pickerInput(
        ns("select_contrasts"), 
        "Select contrasts:", 
        choices = labels, 
        selected = current[[ome]]$contrasts,
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE,
          selectAllText = "Select All",
          deselectAllText = "Deselect All",
          noneSelectedText = "No contrasts selected"
        )
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

        test_results<- list()
          
        for (ome in names(param_list)) {
          test <- param_list[[ome]]$test
          groups <- param_list[[ome]]$groups
          annotation_col <- default_annotations()[[ome]]
          contrasts <- param_list[[ome]]$contrasts

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
          tryCatch({
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
          }, error = function(e) {
            showNotification(paste("Test failed for ome", ome, ":", e$message), type = "error")
            stat.results <<- NULL
          })
            
          # Save results for that ome into test_results list  
          if (!is.null(stat.results)) {
            test_results[[ome]] <- as.data.frame(stat.results[[ome]])
          }
         
        }
        
        stat_results(test_results)
        
        # Check if tests completed successfully and switch to Summary tab
        if (length(test_results) > 0) {
          # Show success notification with clear navigation instructions
          showNotification(
            "Statistical testing completed successfully! Please navigate to Statistics > Summary tab to view your results.", 
            type = "default", 
            duration = NULL
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
