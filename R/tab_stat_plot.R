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
                                   GCTs_original) { 
 
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
      validate(
        need(!is.null(globals$stat_param()) && length(globals$stat_param()) > 0, "Please do the setup first."),
        need(!is.null(globals$stat_results()) && length(globals$stat_results()) > 0, "Please do the setup first.")
      )
      
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
    
    # update selected tab based on default -ome
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
          GCT_original = reactive(GCTs_original()[[ome]]),
          default_annotation_column = reactive(default_annotations()[[ome]]),
          color_map = reactive(custom_colors()[[ome]])
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
                                   GCT_original,
                                   default_annotation_column,
                                   color_map) {
  
  ## module function
  moduleServer(id, function (input, output, session) {
    
    # get namespace, use in renderUI-like functions
    ns <- session$ns
    
    output$ome_plot_contents <- renderUI({
      # fallback if stat_param not defined yet
      if (!exists("stat_param", envir = .GlobalEnv)) {
        return(h4("Please go to the Statistics setup tab first."))
      }
      
      stat_param <- get("stat_param", envir = .GlobalEnv)
      test <- stat_param()[[ome]]$test
      
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
      req(stat_param())
      tagList(
        if (stat_param()[[ome]]$test=="One-sample Moderated T-test"){
          radioButtons(ns("volcano_groups"), "Select Group:", choices=stat_param()[[ome]]$groups)
        } else if (stat_param()[[ome]]$test=="Two-sample Moderated T-test" ){
          radioButtons(ns("volcano_contrasts"), "Select Contrast:", choices=stat_param()[[ome]]$contrasts)
        } else if (stat_param()[[ome]]$test=="Moderated F test"){
          h4("Cannot show a volcano plot for the Mod F test")
        } else {
          return(NULL)
        }
      )
    })
    
    #Plot
    output$volcano_plot <- renderPlotly({
      req(stat_results())
      req(stat_param())
      req(ome)
      
      test <- stat_param()[[ome]]$test
      if (test == "One-sample Moderated T-test") {
        req(input$volcano_groups)
      } else if (test == "Two-sample Moderated T-test") {
        req(input$volcano_contrasts)
      } else {
        return(NULL)
      }
      
      #Run plot function
      gg<- plotVolcano(ome = ome, volcano_groups = input$volcano_groups, volcano_contrasts = input$volcano_contrasts, df= stat_results()[[ome]]) 
      ggplotly(gg)
    })
    

    ## COMPILE EXPORTS ##
    volcano_plot_export_function <- function(dir_name) {
      test <- stat_param()[[ome]]$test
      df <- stat_results()[[ome]]
      
      if (test == "One-sample Moderated T-test") {
        groups <- stat_param()[[ome]]$groups
        for (group in groups) {
          gg <- plotVolcano(
            ome = ome,
            volcano_groups = group,
            volcano_contrasts = NULL,
            df = df
          )
          
          ggsave(
            filename = paste0("volcano_plot_", ome, "_", gsub(" ", "_", group), ".pdf"),
            plot = gg,
            device = "pdf",
            path = dir_name,
            width = 10,
            height = 6,
            units = "in"
          )
        }
        
      } else if (test == "Two-sample Moderated T-test") {
        contrasts <- stat_param()[[ome]]$contrasts
        for (contrast in contrasts) {
          gg <- plotVolcano(
            ome = ome,
            volcano_groups = NULL,
            volcano_contrasts = contrast,
            df = df
          )
            
          ggsave(
            filename = paste0("volcano_plot_", ome, "_", gsub(" / ", "_vs_", contrast), ".pdf"),
            plot = gg,
            device = "pdf",
            path = dir_name,
            width = 10,
            height = 6,
            units = "in"
          )  
        }
        
      } else {
        warning("Volcano plot export not supported for test type: ", test)
      }
    }
    
    
    return(list(
      volcano_plot = volcano_plot_export_function
    ))
  })
}