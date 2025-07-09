################################################################################
# Module: Stat_Summary
#
# Allow users to see the summary of their results
################################################################################

################################################################################
# Shiny functions (UI and server)
################################################################################

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
                                 globals,
                                 GCTs_original) {

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

    # gather relevant variables from globals
    default_ome <- reactive(globals$default_ome) # don't remove this variable!
    custom_colors <- reactive(globals$colors)


   ## OME TABS ##

    # handles compiling ome tabs into styled tabset box
    output$ome_tabset_box <- renderUI({
      req(globals$stat_param, globals$stat_results)  # stop if these reactiveVals don’t exist
      
      stat_param_val <- globals$stat_param()
      stat_results_val <- globals$stat_results()
      
      validate(
        need(!is.null(stat_param_val) && length(stat_param_val) > 0, "Please do the setup first."),
        need(!is.null(stat_results_val) && length(stat_results_val) > 0, "Please do the setup first.")
      )
      
      req(all_omes(), default_ome())
    
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
            GCT_original = reactive(GCTs_original()[[ome_local]]),
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

    # TODO: add UI for each ome tab here. Make sure to use `ns`!
    # Example included below

    # example plot
    fluidRow(
      # Adjustments box
      shinydashboardPlus::box(
        uiOutput(ns("adjustments_table")),
        title = "Adjustments",
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
        plotlyOutput(ns("pval_hist_plot")),
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
}


# server for an individual ome
statSummary_Ome_Server <- function(id,
                                 ome,
                                 GCT_processed,
                                 parameters,
                                 GCT_original,
                                 default_annotation_column,
                                 color_map) {

  ## module function
  moduleServer(id, function (input, output, session) {

    # get namespace, use in renderUI-like functions
    ns <- session$ns
    
    # req(
    #   exists("stat_param", envir = .GlobalEnv),
    #   exists("stat_results", envir = .GlobalEnv)
    # )
    # 
    # stat_param_reactive <- get("stat_param", envir = .GlobalEnv)
    # stat_results_reactive <- get("stat_results", envir = .GlobalEnv)
    # 
    # stat_param <- stat_param_reactive()
    # stat_results <- stat_results_reactive()
    # 
    # validate(
    #   need(!is.null(stat_param), "Please run Setup: Stat parameters missing."),
    #   need(!is.null(stat_results), "Please run Setup: Stat results missing."),
    #   need(!is.null(stat_param[[ome]]), paste0("Please run Setup: Parameters for ", ome, " missing.")),
    #   need(!is.null(stat_results[[ome]]), paste0("Please run Setup: Results for ", ome, " missing."))
    # )
    
    
    ## ADJUSTMENTS INFO ##
    output$adjustments_table <- renderUI(
      tagList(
        selectInput(ns("select_stat"),"Choose stat:", c("adj.p.val","nom.p.val")),
        sliderInput(ns("select_cutoff"), "Choose cutoff:", min=0, max=1, value=0.05, step=0.01 )
      )
    )
    
    observeEvent(input$select_stat, {
      current <- stat_param() 
      current[[ome]]$stat <-input$select_stat
      stat_param(current)                  
    })
    
    observeEvent(input$select_cutoff, {
      current <- stat_param() 
      current[[ome]]$cutoff <-input$select_cutoff
      stat_param(current)                  
    })
    
    ## DATASET INFO ##
    # render dataset info
    output$dataset_table <- renderTable({
      df <- stat_results()[[ome]]
      col_name <- grep("significant", colnames(df), value = TRUE, ignore.case=TRUE)[1]
      req(col_name)
      sig_vals <- df[[col_name]]
      sig_vals <- sig_vals[!is.na(sig_vals)]
      data.frame(
        Description = c("Features tested", "Significant features"),
        Count = c(nrow(df), sum(as.logical(sig_vals)))
      )
    })

    # WORKFLOW INFO ##
    # render workflow info
    output$workflow_table <- renderTable(
      data.frame(
        Description = c("Test chosen", "Cutoff"),
        Count = c(stat_param()[[ome]]$test, 0.05)#replace 10 when I find out if the cutoff number should change as the user adjusts it on this page or whether this is just 0.05
      )
    )

    # P.VALUE HISTOGRAM ##
    output$pval_hist_sidebar_contents <- renderUI({
      req(stat_param())
      tagList(
        if (stat_param()[[ome]]$test=="One-sample Moderated T-test" || stat_param()[[ome]]$test=="Moderated F test"){
          radioButtons(ns("pval_groups"), "Select Group:", choices=stat_param()[[ome]]$groups)
        } else if (stat_param()[[ome]]$test=="Two-sample Moderated T-test" ){
          radioButtons(ns("pval_contrasts"), "Select Contrast:", choices=stat_param()[[ome]]$contrasts)
        }
      )
    })
    
    pval_hist_plot_reactive <- reactive({
      req(stat_results())
      
      # validate/need/require statements
      validate(
        need(GCT_processed(), "GCTs not processed") %then%
          need(default_annotation_column(), "don't know what annotation to use")
      )
      
      pval_stat<- stat_param()[[ome]]$stat 
      pval_cutoff<- stat_param()[[ome]]$cutoff
      pattern_base <- if (pval_stat == "adj.p.val") "adj\\.P\\.Val" else "P\\.Value"
      
      df <- stat_results()[[ome]]
      
      #FIND COLUMN IN RESULTS THAT SHOWS THE DESIRED STAT AND GROUP/CONTRAST
      if (stat_param()[[ome]]$test == "One-sample Moderated T-test") {
        req(input$pval_groups)
        keyword <- input$pval_groups
        pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", pattern_base, ")")

      } else if (stat_param()[[ome]]$test == "Two-sample Moderated T-test") {
        req(input$pval_contrasts)
        groups <- unlist(strsplit(input$pval_contrasts, " / "))
        pattern <- paste0("(?i)(?=.*", groups[1], ")(?=.*", groups[2], ")(?=.*", pattern_base, ")")

      } else {
        pattern <- pattern_base
      }
      
      # if (stat_param()[[ome]]$test == "One-sample Moderated T-test") {
      #   req(input$pval_groups)
      #   keyword <- input$pval_groups
      # } else if (stat_param()[[ome]]$test == "Two-sample Moderated T-test") {
      #   req(input$pval_contrasts)
      #   keyword <- input$pval_contrasts
      # } else {
      #   keyword <- NULL
      # }
      # 
      # if (is.null(keyword)) {
      #   pattern <- pattern_base
      # } else {
      #   # match both p-value type and group/contrast name (any order), case-insensitive
      #   pattern <- paste0("(?i)(?=.*", pattern_base, ")(?=.*", keyword, ")")
      # }
      
      col_name <- grep(pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
      req(col_name)
      
      pvals <- df[[col_name]]
      pvals <- pvals[!is.na(pvals)]
      
      #ACTUAL HISTOGRAM
      ggplot(data.frame(pval = pvals), aes(x = pval)) +
        geom_histogram(breaks = seq(0, 1, by = 0.01), fill = "skyblue", color = "white") +
        geom_vline(xintercept = pval_cutoff, color = "red", linetype = "dashed", size = 1) +
        labs(
          title = paste("P-value Histogram for", ome),
          x = ifelse(pval_stat == "adj.p.val", "Adjusted P-value", "Nominal P-value"),
          y = "Number of Features"
        ) +
        xlim(0, 1)
      
    })
    
    
    output$pval_hist_plot <- renderPlotly({
        gg <- pval_hist_plot_reactive()
        req(gg)
        ggplotly(gg)
        
    })
    
  
    


    ## COMPILE EXPORTS ##
    # stat_summary_csv_export_function <- function(stat.results,dir_name) {
    #   print(dir_name)
    #   write.csv(
    #     stat.results,
    #     file = file.path(dir_name, paste0("stat_results_", ome, ".csv")),
    #     row.names = FALSE
    #   )
    # }
    #
    # return(list(stat_summary= stat_summary_csv_export_function))


  })
}