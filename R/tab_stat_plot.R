################################################################################
# Module: Stat_Plot
#
# Allow users to see the Volcano plot of their results
################################################################################

################################################################################
# Shiny functions (UI and server)
################################################################################

source("R/tab_stat_plot_helpers.R")

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
          stat_results = stat_results
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
                                   stat_results) {
  
  ## module function
  moduleServer(id, function (input, output, session) {
    
    # get namespace, use in renderUI-like functions
    ns <- session$ns
    
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
        # Volcano plot
          fluidRow(shinydashboardPlus::box(
            plotlyOutput(ns("volcano_plot")),
            sidebar = boxSidebar(
              uiOutput(ns("volcano_sidebar_contents")),
              id = ns("volcano_sidebar"),
              width = 25,
              icon = icon("gears", class = "fa-2xl"),
              background = "rgba(91, 98, 104, 0.9)"
            ),
            status = "primary",
            width = 12,
            title = "Volcano Plot",
            headerBorder = TRUE,
            solidHeader = TRUE
          ))
      )
    })
    
    ## RENDER VOLCANO PLOT ##
    #Sidebar
    output$volcano_sidebar_contents <- renderUI({
      req(stat_params())
      tagList(
        if (stat_params()[[ome]]$test=="One-sample Moderated T-test"){
          radioButtons(ns("volcano_groups"), "Select Group:", choices=stat_params()[[ome]]$groups)
        } else if (stat_params()[[ome]]$test=="Two-sample Moderated T-test" ){
          radioButtons(ns("volcano_contrasts"), "Select Contrast:", choices=stat_params()[[ome]]$contrasts)
        } else if (stat_params()[[ome]]$test=="Moderated F test"){
          h4("Cannot show a volcano plot for the Mod F test")
        } else {
          return(NULL)
        }
      )
    })
    
    #Plot
    output$volcano_plot <- renderPlotly({
      req(stat_results())
      req(stat_params())
      req(ome)
      
      test <- stat_params()[[ome]]$test
      if (test == "One-sample Moderated T-test") {
        req(input$volcano_groups)
      } else if (test == "Two-sample Moderated T-test") {
        req(input$volcano_contrasts)
      } else {
        return(NULL)
      }
      
      #Run plot function
      gg<- plotVolcano(ome = ome, volcano_groups = input$volcano_groups, volcano_contrasts = as.character(input$volcano_contrasts), df= stat_results()[[ome]], stat_params = stat_params, stat_results = stat_results) 
      ggplotly(gg)
    })
    

    ## COMPILE EXPORTS ##
    volcano_plot_export_function <- function(dir_name) {
      test <- stat_params()[[ome]]$test
      df <- stat_results()[[ome]]
      
      # Create a single PDF file for all plots from this ome
      pdf_filename <- paste0("volcano_plots_", ome, ".pdf")
      pdf_path <- file.path(dir_name, pdf_filename)
      
      # Start PDF device
      pdf(pdf_path, width = 10, height = 6)
      
      if (test == "One-sample Moderated T-test") {
        groups <- stat_params()[[ome]]$groups
        for (group in groups) {
          gg <- plotVolcano(
            ome = ome,
            volcano_groups = group,
            volcano_contrasts = NULL,
            df = df,
            stat_params = stat_params,
            stat_results = stat_results
          )
          
          # Print plot to current page
          print(gg)
        }
        
      } else if (test == "Two-sample Moderated T-test") {
        contrasts <- stat_params()[[ome]]$contrasts
        for (contrast in contrasts) {
          gg <- plotVolcano(
            ome = ome,
            volcano_groups = NULL,
            volcano_contrasts = contrast,
            df = df,
            stat_params = stat_params,
            stat_results = stat_results
          )
          
          # Print plot to current page
          print(gg)
        }
        
      } else {
        warning("Volcano plot export not supported for test type: ", test)
      }
      
      # Close PDF device
      dev.off()
      
      cat("Saved volcano plots for", ome, "to:", pdf_path, "\n")
    }
    
    
    return(list(
      volcano_plot = volcano_plot_export_function
    ))
  })
}