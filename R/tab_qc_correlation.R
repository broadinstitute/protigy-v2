################################################################################
# Module: qc_correlation
#
# Produce boxplots before and after normalization
################################################################################

################################################################################
# Shiny functions (UI and server)
################################################################################

# UI for the QCCorrelation tab
# contains the structure for the big tabbed box with omes
QCCorrelation_Tab_UI <- function(id = "QCCorrelationTab") {
  ns <- NS(id) # namespace function, wrap UI inputId's with this `ns("inputId")`
  
  tagList(
    
    # display omes tabs
    fluidRow(uiOutput(ns("ome_tabset_box")))
    
  ) # end tagList
}

# server for the QCCorrelation tab
# contains the structure for the big tabbed box with omes
QCCorrelation_Tab_Server <- function(id = "QCCorrelationTab",
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
      req(all_omes(), default_ome())
      
      # generate a tab for each -ome
      tabs <- lapply(all_omes(), function(ome){
        tabPanel(
          title = ome,
          
          # call the UI function for each individual ome
          QCCorrelation_Ome_UI(id = ns(ome), ome = ome)
          
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
        QCCorrelation_Ome_Server(
          id = ome,
          ome = ome,
          GCT_processed = reactive(GCTs()[[ome]]),
          parameters = reactive(parameters()[[ome]]),
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
QCCorrelation_Ome_UI <- function (id, ome) {
                
  ns <- NS(id)
  
  tagList(
    # Correlation plots
    fluidRow(shinydashboardPlus::box(
      plotOutput(ns("qc_corr_heatmap"), height="auto"),
      plotlyOutput(ns("qc_corr_boxplot")),
      sidebar = boxSidebar(
        uiOutput(ns("qc_correlation_sidebar_contents")),
        id = ns("qc_correlation_sidebar"),
        width = 25,
        icon = icon("gears", class = "fa-2xl"),
        background = "rgba(91, 98, 104, 0.9)"
      ),
      status = "primary",
      width = 12,
      title = "Correlation Plots",
      headerBorder = TRUE,
      solidHeader = TRUE))
  )
}


# server for an individual ome
QCCorrelation_Ome_Server <- function(id,
                                   ome,
                                   GCT_processed,
                                   parameters,
                                   default_annotation_column,
                                   color_map) {
  
  ## module function
  moduleServer(id, function (input, output, session) {
    
    # get namespace, use in renderUI-like functions
    ns <- session$ns

    ## CORRELATION HEATMAP ##
    
    # reactive
    qc_corr_heatmap_out <- eventReactive(
      eventExpr = c(input$qc_correlation_annotation, color_map()), 
      valueExpr = {
        req(GCT_processed(), default_annotation_column(), color_map())
        
        # get annotation column
        if (!is.null(input$qc_correlation_annotation)) {
          annot_column <- input$qc_correlation_annotation
        } else {
          annot_column <- default_annotation_column()
        }
        
        # get custom colors
        custom_colors <- color_map()
        if (annot_column %in% names(custom_colors)) {
          annot_color_map <- custom_colors[[annot_column]]
        } else {
          annot_color_map <- NULL
        }
        
        # generate plot
         create_corr_heatmap(gct = GCT_processed(),
                              col_of_interest = annot_column,
                              ome = ome,
                              custom_color_map = annot_color_map)
      }
    )
    
    qc_corr_heatmap_reactive <- reactive({
      validate(need(qc_corr_heatmap_out()$HM, "Heatmap not avaliable"))
      draw_corr_HM(qc_corr_heatmap_out()$HM)
    })
    
    #get plot height
    Corr.HM.Table <- reactive(qc_corr_heatmap_out()$Table)
    corr_plot_height <- reactive({
      tryCatch(
        dynamicHeightHMCorr(nrow(Corr.HM.Table())), 
        error = function(c) 400
      )
    })
    
    # render heatmap
    output$qc_corr_heatmap <- renderPlot(
      qc_corr_heatmap_reactive(),
      height=corr_plot_height()
    )
    
    ## CORRELATION BOXPLOT ##
    
    # reactive
    qc_corr_boxplot_reactive <- eventReactive(
      eventExpr = c(input$qc_correlation_annotation, color_map()), 
      valueExpr = {
        req(GCT_processed(), default_annotation_column(), color_map())
        
        # get annotation column
        if (!is.null(input$qc_correlation_annotation)) {
          annot_column <- input$qc_correlation_annotation
        } else {
          annot_column <- default_annotation_column()
        }
        
        # get custom colors
        custom_colors <- color_map()
        if (annot_column %in% names(custom_colors)) {
          annot_color_map <- custom_colors[[annot_column]]
        } else {
          annot_color_map <- NULL
        }
        
        # generate plot
        create_corr_boxplot(gct = GCT_processed(),
                           col_of_interest = annot_column,
                           ome = ome,
                           custom_color_map = annot_color_map)
      }
    )
    
    # render summary plot
    output$qc_corr_boxplot <- renderPlotly(
      ggplotly(qc_corr_boxplot_reactive(), tooltip = "text")
    )
    
    # sidebar contents
    output$qc_correlation_sidebar_contents <- renderUI({
      req(GCT_processed())
      
      add_css_attributes(
        selectInput(
          ns("qc_correlation_annotation"),
          "Group by",
          choices = names(GCT_processed()@cdesc),
          selected = default_annotation_column()),
        classes = "small-input",
        styles = "margin-right: 10px"
      )
    })
    
    ## COMPILE EXPORTS ##
    
    
    qc_corr_heatmap_export_function <- function(dir_name) {
      req(qc_corr_heatmap_out()$HM)
      pdf(file = file.path(dir_name, paste0("qc_corr_heatmap", ome, ".pdf")),
          width = 1400/72,
          height = (corr_plot_height() + 48)/72)
      draw_corr_HM(qc_corr_heatmap_out()()$HM)
      dev.off()
    }
    
    qc_corr_boxplot_export_function <- function(dir_name) {
      ggsave(
        filename = paste0("qc_corr_boxplot", ome, ".pdf"), 
        plot = qc_corr_boxplot_reactive(), 
        device = 'pdf',
        path = dir_name,
        width = 10,
        height = 6, 
        units = "in"
      )
    }
    
    return(
      list(
        qc_corr_heatmap = qc_corr_heatmap_export_function,
        qc_corr_boxplot = qc_corr_boxplot_export_function
      )
    )
  })
}

