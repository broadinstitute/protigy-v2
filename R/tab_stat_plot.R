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
    
    
    ## RENDER VOLCANO PLOT ##
    output$volcano_sidebar_contents <- renderUI({
      req(stat_param())
      tagList(
        if (stat_param()[[ome]]$test=="One-sample Moderated T-test"){
          radioButtons(ns("volcano_groups"), "Select Group:", choices=stat_param()[[ome]]$groups)
        } else if (stat_param()[[ome]]$test=="Two-sample Moderated T-test" ){
          radioButtons(ns("volcano_contrasts"), "Select Contrast:", choices=stat_param()[[ome]]$contrasts)
        } else if (stat_param()[[ome]]$test=="Moderated F test"){
          h3("Cannot show a volcano plot for the Mod F test")
        } else {
          return(NULL)
        }
      )
    })
    
    # volcano_plot_reactive <- reactive({
    #   req(stat_results())
    #   
    #   # validate/need/require statements
    #   validate(
    #     need(GCT_processed(), "GCTs not processed") %then%
    #       need(default_annotation_column(), "don't know what annotation to use")
    #   )
    #   
    #   if (stat_param()[[ome]]$test=="One-sample Moderated T-test"){
    #     req(input$volcano_groups)
    #     group<- input$volcano_groups
    #   } else if (stat_param()[[ome]]$test=="Two-sample Moderated T-test" ){
    #     req(input$volcano_contrasts)
    #     group<- input$volcano_contrasts
    #   } else {
    #     group<-NULL
    #   }
    #   plotVolcano(group = group)
    #   
    #   ggplot() + ggtitle(paste("Volcano Plot for:", ome))
    # })
    # 
    # 
    # output$volcano_plot <- renderPlotly({
    #   
    #   gg <- volcano_plot_reactive()
    #   ggplotly(gg)
    # })
    # 
    
    output$volcano_plot <- renderPlot({
      req(stat_results())
      validate(
        need(GCT_processed(), "GCTs not processed") %then%
          need(default_annotation_column(), "don't know what annotation to use")
      )
      
      if (stat_param()[[ome]]$test=="One-sample Moderated T-test"){
        req(input$volcano_groups)
        group <- input$volcano_groups
      } else if (stat_param()[[ome]]$test=="Two-sample Moderated T-test" ){
        req(input$volcano_contrasts)
        group <- input$volcano_contrasts
      } else {
        return(NULL)
      }
      
      plotVolcano(group = group)  ## draws the plot directly
    })
    
    # ## COMPILE EXPORTS ##
    # # Example of export function
    # example_plot_export_function <- function(dir_name) {
    #   ggsave(
    #     filename = paste0("example_plot_", ome, ".pdf"), 
    #     plot = example_plot_reactive(), # call the reactive object
    #     device = 'pdf',
    #     path = dir_name # save in the desired output directory
    #   )
    # }
    # 
    # # TODO: return a named list of custom export functions, example included
    # return(
    #   list(
    #     example_plot = example_plot_export_function
    #   )
    # )
  })
}