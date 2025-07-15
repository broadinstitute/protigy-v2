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
      fluidRow(
        column(3,
               selectInput(ns("selected_omes"), "Select datasets to test:", choices = NULL),
               textOutput(ns("annotation_col")),
               actionButton(ns("run_test_button"),"Run Test")
        ),
        column(3,
               uiOutput(ns("select_test")),
               uiOutput(ns("select_groups_ui"))
        ),
        column(3,
               # conditionalPanel(
               # condition = "input['statSetupTab-select_test'] == 'Two-sample Moderated T-test'",
               uiOutput(ns("select_contrast_ui"))
               #)
        )
      )
    )
  )}



# server for the statSetup tab
# contains the structure for the big tabbed box with omes
statSetup_Tab_Server <- function(id = "statSetupTab",GCTs_and_params, globals,GCTs_original){
  ## module function
  moduleServer(id, function (input, output, session) {

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

    # Large merged GCT with all omes containing `protigy.ome` column in `rdesc`
    GCTs_merged <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$GCTs_merged
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

    #SETS THE OME SELECT INPUT
    observe({
      req(all_omes())
      ome_list <- all_omes()
      updateSelectInput(session, "selected_omes",choices = ome_list, selected = ome_list[[1]] )
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
    
######TEST SELECTION############################################################
    #saving the selected test to stat_param
    observeEvent(input$select_test, {
      req(selected_ome())
      current <- stat_param()             
      ome <- selected_ome()               
      
      if (is.null(current[[ome]])) {
        current[[ome]] <- list()           
      }
      
      current[[ome]]$test <- input$select_test 
      
      # Only set stat and cutoff if not already set
      if (is.null(current[[ome]]$stat)) current[[ome]]$stat <- "adj.p.val"
      if (is.null(current[[ome]]$cutoff)) current[[ome]]$cutoff <- 0.05
      
      stat_param(current) 
    })
    
    #displaying the test choices (default)
    output$select_test <- renderUI ({
      selectInput(ns("select_test"), 
                  "Select test:", 
                  choices= c("None","One-sample Moderated T-test","Two-sample Moderated T-test","Moderated F test"), 
                  selected="None"
      )
    })
    
    #displaying the previously chosen test from the parameters
    observe({
      req(selected_ome(),input$select_test)
      saved_test <- stat_param()[[selected_ome()]]$test
      if (is.null(saved_test)) saved_test <- "None"
      updateSelectInput(session, "select_test", selected = saved_test)
    })
################################################################################
######GROUP SELECTION############################################################
    #saving the selected groups to stat_param
    observeEvent(input$stat_setup_annotation, {
      req(selected_ome())
      current <- stat_param()           
      ome <- selected_ome()                
      
      if (is.null(current[[ome]])) {current[[ome]] <- list()}
      
      current[[ome]]$groups <- input$stat_setup_annotation
      
      stat_param(current)                  
    })
    
    #displaying the groups choices (default)
    output$select_groups_ui <- renderUI({
      
      req(cdesc(), default_annotation_column(),selected_ome(), stat_param())
      
      choices<- unique(cdesc()[[default_annotation_column()]])
      choices <- choices[!is.na(choices)]
      
      checkboxGroupInput(
        ns("stat_setup_annotation"),
        "Include groups:",
        choices = choices,
        selected = choices 
      )
    })
    
    #displaying the previously chosen groups from the parameters
    observe({
      req(selected_ome(),input$stat_setup_annotation)
      saved_groups <- stat_param()[[selected_ome()]]$groups
      if (is.null(saved_groups)) {saved_groups <- unique(cdesc()[[default_annotation_column()]])}
      updateCheckboxGroupInput(session, "stat_setup_annotation", selected = saved_groups)
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

    output$select_contrast_ui <- renderUI({
      current <- stat_param()
      ome <- selected_ome()
        
      req(current[[ome]]$test=="Two-sample Moderated T-test")
      if (length(current[[ome]]$groups) < 2 || is.null(current[[ome]]$groups)) stop("too few groups")
      
      pairwise_contrasts <- combn(current[[ome]]$groups, 2, simplify = FALSE)
      all_pairs <- c(pairwise_contrasts, lapply(pairwise_contrasts, rev))
      labels <- sapply(all_pairs, function(p) paste(p[1], "/", p[2]))
        
      if (is.null(current[[ome]]$contrasts)) {
        current[[ome]]$contrasts <- labels 
        stat_param(current)
      }
        
      checkboxGroupInput(ns("select_contrasts"), "Select contrasts:", choices=labels, selected=current[[ome]]$contrasts)
    })

 
    
    ##FLIPPING THE CONTRASTS##
    # observe({
    #   req(input$selected_ome)
    #   req(input$stat_setup_annotation)
    #   if (length(input$stat_setup_annotation) < 2) return(list())
    #   
    #   pairwise_contrasts <- combn(input$stat_setup_annotation, 2, simplify = FALSE)
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
    #   req(length(input$stat_setup_annotation) >= 2)
    #    
    #   #create pairwise comparisons
    #   pairwise_contrasts <- combn(input$stat_setup_annotation, 2, simplify = FALSE)
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
    #    pairwise_contrasts <- combn(input$stat_setup_annotation, 2, simplify = FALSE)
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
    #    req(input$stat_setup_annotation)
    #    if(length(input$stat_setup_annotation) < 2) {
    #      return(list())  # Return empty list when fewer than 2 groups selected
    #    }
    #    
    #    selected_contrasts <- list()
    #    pairwise_contrasts <- combn(input$stat_setup_annotation, 2, simplify = FALSE)
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
     
##############################################################################
        #TESTS RUN AFTER RUN BUTTON CLICKED
        observeEvent(input$run_test_button, {
          # #Capture system output in a file
          # timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
          # filename <- paste0("C:/Users/dabburi/Documents/run_", timestamp, ".txt")
          # sink(filename)
          
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
            
            # For two-sample test, ensure proper contrasts
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
            
            if (test== "Moderated F test" && length(groups) < 2) {
              showNotification("Please select at least two groups for F test", type = "error")
              next
            }
            
            stat.results <- NULL
            tryCatch({
              stat.results <- stat.testing(test = test,annotation_col = annotation_col,chosen_omes = ome,gct = gcts,chosen_groups = groups,selected_contrasts = contrasts_list,intensity = FALSE)
              }, error = function(e) {
              showNotification(paste("Test failed for ome", ome, ":", e$message), type = "error")
              stat.results <<- NULL
            })
            
            
            if (!is.null(stat.results)) {
              test_results[[ome]] <- as.data.frame(stat.results)
            }
          }
   
          stat_results(test_results)
          #print(head(stat_results()))
          assign("stat_results", stat_results, envir = .GlobalEnv)
          assign("stat_param", stat_param, envir = .GlobalEnv)
          globals$stat_param <- stat_param
          globals$stat_results <- stat_results
          
          
          # sink()
        })
     
  })
}






