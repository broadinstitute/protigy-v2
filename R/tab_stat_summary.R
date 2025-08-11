################################################################################
# Module: Stat_Summary
#
# Allow users to see the summary of their results
################################################################################

################################################################################
# Shiny functions (UI and server)
################################################################################
source("R/tab_stat_summary_helpers.R")
# UI for the statSummary tab
# contains the structure for the big tabbed box with omes
statSummary_Tab_UI <- function(id = "statSummaryTab") {
  ns <- NS(id) # namespace function, wrap UI inputId's with this `ns("inputId")`
  
  tagList(
    
    # display omes tabs
    fluidRow(uiOutput(ns("ome_tabset_box")))
    
  ) # end tagList
}

# server for the statSummary tab
# contains the structure for the big tabbed box with omes
statSummary_Tab_Server <- function(id = "statSummaryTab",
                                   GCTs_and_params,
                                   globals) {
  
  ## module function
  moduleServer(id, function (input, output, session) {
    
    ## GATHERING INPUTS ##
    
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
    
    # gather relevant variables from globals
    default_ome <- reactive(globals$default_ome) # don't remove this variable!
    custom_colors <- reactive(globals$colors)
    
    
    ## OME TABS ##
    
    # handles compiling ome tabs into styled tabset box
    output$ome_tabset_box <- renderUI({
      req(globals$stat_results)  # stop if these reactiveVals don’t exist
      validate(
        need(!is.null(globals$stat_results()) && length(globals$stat_results()) > 0, "Please do the setup first.")
      )
      
      req(all_omes(), default_ome())
      
      #preserve current selected tab
      selected_tab <- input$ome_tabs
      if (is.null(selected_tab) || !(selected_tab %in% all_omes())) {
        selected_tab <- default_ome()
      }
      
      # generate a tab for each -ome
      tabs <- lapply(all_omes(), function(ome){
        tabPanel(
          title = ome,
          
          # call the UI function for each individual ome
          statSummary_Ome_UI(id = ns(ome), ome = ome)
          
        ) # end tabPanel
      }) # end lapply
      
      # combine all tabs into tabSetPanel
      tab_set_panel <- do.call(
        tabsetPanel,
        c(tabs, list(id = ns("ome_tabs"), selected = isolate(selected_tab)))
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
    
    # update selected tab based on default -ome
    observe({
      updateTabsetPanel(inputId = "ome_tabs", selected = default_ome())
    })
    
    # call the server function for each individual ome
    all_plots <- reactiveVal() # initialize
    observeEvent(all_omes(), {
      output_plots <- sapply(all_omes(), function(ome) {
        local({
          ome_local <- ome  # capture the value NOW
          
          statSummary_Ome_Server(
            id = ome_local,
            ome = ome_local,
            GCT_processed = reactive(GCTs()[[ome_local]]),
            parameters = reactive(parameters()[[ome_local]]),
            default_annotation_column = reactive(default_annotations()[[ome_local]]),
            color_map = reactive(custom_colors()[[ome_local]])
          )
        })
      }, simplify = FALSE)
      
      all_plots(output_plots) # set reactive value with outputs
    })
    
    return(all_plots)
  })
}



# UI for an individual ome
statSummary_Ome_UI <- function (id, ome) {
  
  ns <- NS(id)
  
  tagList(
    
    uiOutput(ns("ome_summary_contents"))
    
  )
}


# server for an individual ome
statSummary_Ome_Server <- function(id,
                                   ome,
                                   GCT_processed,
                                   parameters,
                                   default_annotation_column,
                                   color_map) {
  
  ## module function
  moduleServer(id, function (input, output, session) {
    
    # get namespace, use in renderUI-like functions
    ns <- session$ns
    
    output$ome_summary_contents <- renderUI({
      # fallback if stat_results not defined yet
      if (!exists("stat_results", envir = .GlobalEnv)) {
        return(h4("Please go to the Statistics setup tab first."))
      }
      
      stat_param <- get("stat_param", envir = .GlobalEnv)
      test <- stat_param()[[ome]]$test
      
      if (is.null(test) || test == "None") {
        return(h4("No test selected to run on this dataset."))
      }
      
      tagList(
        fluidRow(
          
          # Adjustments box
          shinydashboardPlus::box(
            uiOutput(ns("adjustments_table")),
            title = "Cutoff Selection",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            headerBorder = TRUE
          ),
          
          # Dataset information box
          shinydashboardPlus::box(
            tableOutput(ns("dataset_table")),
            title = "Dataset Information",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            headerBorder = TRUE
          ),
          
          # Data workflow box
          shinydashboardPlus::box(
            tableOutput(ns("workflow_table")),
            title = "Workflow Parameters",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            headerBorder = TRUE
          ),
          
          # P.val histogram box
          fluidRow(shinydashboardPlus::box(
            fluidRow(
              column(6, plotlyOutput(ns("adj_pval_hist_plot"))),
              column(6, plotlyOutput(ns("nom_pval_hist_plot")))
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
            title = "P.value Histogram",
            headerBorder = TRUE,
            solidHeader = TRUE
          ))
          
        ) # end fluidRow
      )
    })
    
    ## ADJUSTMENTS INFO #######################################################
    
    output$adjustments_table <- renderUI({
      req(stat_param())
      current_stat <- stat_param()[[ome]]$stat
      current_cutoff <- stat_param()[[ome]]$cutoff

      tagList(
        h5("The following selections are applied to Volcano Plots as well"),
        selectInput(ns("select_stat"),"Choose stat:", choices= c("adj.p.val","nom.p.val"), selected = current_stat),
        sliderInput(ns("select_cutoff_slider"), "Choose cutoff:", min=0.001, max=1, value=current_cutoff, step=0.001),
        numericInput(ns("select_cutoff_text"), NULL, min=0.001, max=1, value=current_cutoff, step=0.001)
      )
    })

    # save selected stat to stat_param
    observeEvent(input$select_stat, {
      current <- stat_param()
      current[[ome]]$stat <-input$select_stat
      stat_param(current)
    })

    # save selected cutoff to stat_param
    observeEvent(input$select_cutoff_slider, {
      updateNumericInput(session, "select_cutoff_text", value = input$select_cutoff_slider)

      current <- stat_param()
      current[[ome]]$cutoff <- input$select_cutoff_slider
      stat_param(current)
    })

    # save selected cutoff to stat_param
    observeEvent(input$select_cutoff_text, {
      updateSliderInput(session, "select_cutoff_slider", value = input$select_cutoff_text)

      current <- stat_param()
      current[[ome]]$cutoff <- input$select_cutoff_text
      stat_param(current)
    })

    
    ## WORKFLOW INFO #######################################################
    output$workflow_table <- renderTable(
      data.frame(
        Description = c("Test", "Cutoff", "Stat"),
        Count = c(stat_param()[[ome]]$test, stat_param()[[ome]]$cutoff, stat_param()[[ome]]$stat)
      )
    )
    
    ## P.VALUE HISTOGRAM #######################################################
    #Sidebar
    output$pval_hist_sidebar_contents <- renderUI({
      req(stat_param())
      tagList(
        if (stat_param()[[ome]]$test=="One-sample Moderated T-test"){
          radioButtons(ns("pval_groups"), "Select Group:", choices=stat_param()[[ome]]$groups)
        } else if (stat_param()[[ome]]$test=="Two-sample Moderated T-test" ){
          radioButtons(ns("pval_contrasts"), "Select Contrast:", choices=stat_param()[[ome]]$contrasts)
        } else if (stat_param()[[ome]]$test=="Moderated F test"){
          h5("No group option for F-test.")
        }
      )
    })
    
    # Adjusted p-value histogram
    output$adj_pval_hist_plot <- renderPlotly({
      req(stat_param(), stat_results())
      
      test <- stat_param()[[ome]]$test
      
      if (test == "One-sample Moderated T-test") {
        req(input$pval_groups)
        pvals <- get_pvals(ome, stat_param(), stat_results(), input$pval_groups, NULL, "adj.P.Val")
        gg <- plot_pval_histogram(pvals, paste("Adjusted P-value Histogram for", ome, ": ", input$pval_groups), "Adjusted P-value", stat_param(),stat_results(), ome, input$pval_groups, NULL, "adj.P.Val")
      } else if (test == "Two-sample Moderated T-test") {
        req(input$pval_contrasts)
        pvals <- get_pvals(ome, stat_param(), stat_results(), NULL, as.character(input$pval_contrasts), "adj.P.Val")
        gg <- plot_pval_histogram(pvals, paste("Adjusted P-value Histogram for", ome, ": ", input$pval_contrasts), "Adjusted P-value", stat_param(),stat_results(), ome, NULL, as.character(input$pval_contrasts), "adj.P.Val")
      } else {
        pvals <- get_pvals(ome, stat_param(), stat_results(), NULL, NULL, "adj.P.Val")
        gg <- plot_pval_histogram(pvals, paste("Adjusted P-value Histogram for", ome), "Adjusted P-value", stat_param(),stat_results(), ome, NULL, NULL, "adj.P.Val")
      }
      
      #gg <- plot_pval_histogram(pvals, paste("Adjusted P-value Histogram for", ome), "Adjusted P-value", stat_param(), ome, input$pval_groups)
      ggplotly(gg)
    })
    
    # Nominal p-value histogram
    output$nom_pval_hist_plot <- renderPlotly({
      req(stat_param(), stat_results())
      
      test <- stat_param()[[ome]]$test
      
      if (test == "One-sample Moderated T-test") {
        req(input$pval_groups)
        pvals <- get_pvals(ome, stat_param(), stat_results(), input$pval_groups, NULL, "P.Value")
        gg <- plot_pval_histogram(pvals, paste("Nominal P-value Histogram for", ome, ": ", input$pval_groups), "Nominal P-value", stat_param(),stat_results(), ome, input$pval_groups, NULL, "P.Value")
      } else if (test == "Two-sample Moderated T-test") {
        req(input$pval_contrasts)
        pvals <- get_pvals(ome, stat_param(), stat_results(), NULL, as.character(input$pval_contrasts), "P.Value")
        gg <- plot_pval_histogram(pvals, paste("Nominal P-value Histogram for", ome, ": ", input$pval_contrasts), "Nominal P-value", stat_param(),stat_results(), ome, NULL, as.character(input$pval_contrasts), "P.Value")
      } else {
        pvals <- get_pvals(ome, stat_param(), stat_results(), NULL, NULL, "P.Value")
        gg <- plot_pval_histogram(pvals, paste("Nominal P-value Histogram for", ome), "Nominal P-value", stat_param(),stat_results(), ome, NULL, NULL, "P.Value")
      }
      
      #gg <- plot_pval_histogram(pvals, paste("Nominal P-value Histogram for", ome), "Nominal P-value", stat_param(), ome)
      ggplotly(gg)
    })
    
    
    ## DATASET INFO #######################################################
    output$dataset_table <- renderTable({
      req(stat_param(), stat_results())
      df <- stat_results()[[ome]]
      
      test_type <- stat_param()[[ome]]$test
      sig_cutoff <- stat_param()[[ome]]$cutoff
      sig_stat <- stat_param()[[ome]]$stat
      
      #get pval and adj pval column#
      if (test_type == "One-sample Moderated T-test") {
        req(input$pval_groups)
        keyword <- input$pval_groups
        adjP_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*adj\\.P\\.Val)")
        pval_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*P\\.Value)")
      } else if (test_type == "Two-sample Moderated T-test") {
        req(input$pval_contrasts)
        groups <- unlist(strsplit(as.character(input$pval_contrasts), " / "))
        adjP_pattern <- paste0("(?i)(?=.*", groups[1], ")(?=.*", groups[2], ")(?=.*adj\\.P\\.Val)")
        pval_pattern <- paste0("(?i)(?=.*", groups[1], ")(?=.*", groups[2], ")(?=.*P\\.Value)")
      } else if (test_type == "Moderated F test"){
        adjP_pattern <- paste0("(?i)(?=.*adj\\.P\\.Val)")
        pval_pattern <- paste0("(?i)(?=.*P\\.Value)")
      } 
      
      adjP_col <- grep(adjP_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
      pval_col <- grep(pval_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
      
      #check for missing columns
      req(!is.na(adjP_col), !is.na(pval_col))
      
      adj.P.Val <- as.numeric(df[[adjP_col]])
      P.Value <- as.numeric(df[[pval_col]])
      
      #calculate significance#
      significant <- rep(FALSE, length(P.Value))
      if (sig_stat == "adj.p.val") {
        significant <- adj.P.Val < sig_cutoff
      } else if (sig_stat == "nom.p.val") {
        significant <- P.Value < sig_cutoff
      }
      
      significant <- significant[!is.na(significant)]
      
      # Filter to only rows with at least one non-NA numeric value
      numeric_cols <- sapply(df, is.numeric)
      df_filtered <- df[rowSums(!is.na(df[, numeric_cols, drop=FALSE])) > 0, ]
      
      #RETURN DATAFRAME#
      data.frame(
        Description = c("Features tested", "Significant features"),
        Value = c(nrow(df_filtered), sum(significant))
      )
    })
    
    ## COMPILE EXPORTS #######################################################
    adj_pval_hist_plot_export_function <- function(dir_name) {
      test <- stat_param()[[ome]]$test
      df <- stat_results()[[ome]]
      
      # Create a single PDF file for all adjusted p-value histogram plots from this ome
      pdf_filename <- paste0("adj_pval_hist_plots_", ome, ".pdf")
      pdf_path <- file.path(dir_name, pdf_filename)
      
      # Start PDF device
      pdf(pdf_path, width = 10, height = 6)
      
      if (test == "One-sample Moderated T-test") {
        groups <- stat_param()[[ome]]$groups
        for (group in groups) {
          pvals <- get_pvals(ome, stat_param(), stat_results(), group, NULL, "adj.P.Val")
          gg <- plot_pval_histogram(pvals, paste("Adjusted P-value Histogram for", ome, "-", group), "Adjusted P-value", stat_param(),stat_results(), ome, group, NULL, "adj.P.Val")
          
          # Print plot to current page
          print(gg)
        }
        
      } else if (test == "Two-sample Moderated T-test") {
        contrasts <- stat_param()[[ome]]$contrasts
        for (contrast in contrasts) {
          pvals <- get_pvals(ome, stat_param(), stat_results(), NULL, contrast, "adj.P.Val")
          gg <- plot_pval_histogram(pvals, paste("Adjusted P-value Histogram for", ome, "-", contrast), "Adjusted P-value", stat_param(),stat_results(), ome, NULL, contrast, "adj.P.Val")
          
          # Print plot to current page
          print(gg)
        }
        
      } else if (test == "Moderated F test") {
        pvals <- get_pvals(ome, stat_param(), stat_results(), NULL, NULL, "adj.P.Val")
        gg <- plot_pval_histogram(pvals, paste("Adjusted P-value Histogram for", ome), "Adjusted P-value", stat_param(),stat_results(), ome, NULL, NULL, "adj.P.Val")
        
        # Print plot to current page
        print(gg)
      }
      
      # Close PDF device
      dev.off()
      
      cat("Saved adjusted p-value histogram plots for", ome, "to:", pdf_path, "\n")
    }
    
    nom_pval_hist_plot_export_function <- function(dir_name) {
      test <- stat_param()[[ome]]$test
      df <- stat_results()[[ome]]
      
      # Create a single PDF file for all nominal p-value histogram plots from this ome
      pdf_filename <- paste0("nom_pval_hist_plots_", ome, ".pdf")
      pdf_path <- file.path(dir_name, pdf_filename)
      
      # Start PDF device
      pdf(pdf_path, width = 10, height = 6)
      
      if (test == "One-sample Moderated T-test") {
        groups <- stat_param()[[ome]]$groups
        for (group in groups) {
          pvals <- get_pvals(ome, stat_param(), stat_results(), group, NULL, "P.Value")
          gg <- plot_pval_histogram(pvals, paste("Nominal P-value Histogram for", ome, "-", group), "Nominal P-value", stat_param(),stat_results(), ome, group, NULL, "P.Value")
          
          # Print plot to current page
          print(gg)
        }
        
      } else if (test == "Two-sample Moderated T-test") {
        contrasts <- stat_param()[[ome]]$contrasts
        for (contrast in contrasts) {
          pvals <- get_pvals(ome, stat_param(), stat_results(), NULL, contrast, "P.Value")
          gg <- plot_pval_histogram(pvals, paste("Nominal P-value Histogram for", ome, "-", contrast), "Nominal P-value", stat_param(),stat_results(), ome, NULL, contrast, "P.Value")
          
          # Print plot to current page
          print(gg)
        }
        
      } else if (test == "Moderated F test") {
        pvals <- get_pvals(ome, stat_param(), stat_results(), NULL, NULL, "P.Value")
        gg <- plot_pval_histogram(pvals, paste("Nominal P-value Histogram for", ome), "Nominal P-value", stat_param(),stat_results(), ome, NULL, NULL, "P.Value")
        
        # Print plot to current page
        print(gg)
      }
      
      # Close PDF device
      dev.off()
      
      cat("Saved nominal p-value histogram plots for", ome, "to:", pdf_path, "\n")
    }
    
    stat_results_export_function <- function(dir_name) {
      write.csv(
        stat_results()[[ome]],
        file = file.path(dir_name, paste0("stat_results_", ome, ".csv")),
        row.names = FALSE
      )
    }
    
    workflow_parameters_export_function <- function(dir_name) {
      df <- data.frame(
        Description = c("Test chosen", "Cutoff", "Stat"),
        Count = c(stat_param()[[ome]]$test, stat_param()[[ome]]$cutoff, stat_param()[[ome]]$stat)
      )
      
      write.table(
        df,
        file = file.path(dir_name, paste0("workflow_parameters_", ome, ".txt")),
        sep = "\t",
        quote = FALSE, #doesn't add quotation marks around text
        row.names = FALSE
      )
    }
    
    
    return(list(
      adj_pval_hist_plot = adj_pval_hist_plot_export_function,
      nom_pval_hist_plot = nom_pval_hist_plot_export_function,
      stat_results = stat_results_export_function,
      workflow_parameters = workflow_parameters_export_function
    ))
    
  })
}