################################################################################
# Module: qc_profile_plots
#
# Produce profile plots before and after normalization
################################################################################

################################################################################
# Shiny functions (UI and server)
################################################################################

# UI for the QCProfilePlots tab
# contains the structure for the big tabbed box with omes
QCProfilePlots_Tab_UI <- function(id = "QCProfilePlotsTab") {
  ns <- NS(id) # namespace function, wrap UI inputId's with this `ns("inputId")`
  
  tagList(
    
    # display omes tabs
    fluidRow(uiOutput(ns("ome_tabset_box")))
    
  ) # end tagList
}

# server for the QCProfilePlots tab
# contains the structure for the big tabbed box with omes
QCProfilePlots_Tab_Server <- function(id = "QCProfilePlotsTab",
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
          QCProfilePlots_Ome_UI(id = ns(ome), ome = ome)
          
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
        QCProfilePlots_Ome_Server(
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
QCProfilePlots_Ome_UI <- function (id, ome) {
                
  ns <- NS(id)
  
  tagList(
    
    # profile plots
    fluidRow(shinydashboardPlus::box(
      plotlyOutput(ns("qc_profile_plot_org")),
      br(),
      plotlyOutput(ns("qc_profile_plot_norm")),
      sidebar = boxSidebar(
        uiOutput(ns("qc_profile_plots_sidebar_contents")),
        id = ns("qc_profile_plots_sidebar"),
        width = 25,
        icon = icon("gears", class = "fa-2xl"),
        background = "rgba(91, 98, 104, 0.9)"
      ),
      status = "primary",
      width = 12,
      title = "Profile Plots",
      headerBorder = TRUE,
      solidHeader = TRUE
    ))
  )
}


# server for an individual ome
QCProfilePlots_Ome_Server <- function(id,
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

    ## ORIGINAL PROFILE PlOT ##
    
    # reactive
    qc_profile_plot_org_reactive <- eventReactive(
      eventExpr = c(input$qc_profile_plots_annotation, color_map()), 
      valueExpr = {
        req(GCT_original(), default_annotation_column(), color_map())
        
        # get annotation column
        if (!is.null(input$qc_profile_plots_annotation)) {
          annot_column <- input$qc_profile_plots_annotation
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
        create_profile_plot(gct = GCT_original(),
                    col_of_interest = annot_column,
                    ome = ome,
                    custom_color_map = annot_color_map,
                    parameters=parameters(),
                    type="org")
      }
    )
    
    # render summary plot
    output$qc_profile_plot_org <- renderPlotly(
      ggplotly(qc_profile_plot_org_reactive(), tooltip = "text")
    )
    
    ## NORMALIZED PROFILE PLOT ##
    
    # reactive
    qc_profile_plot_norm_reactive <- eventReactive(
      eventExpr = c(input$qc_profile_plots_annotation, color_map()), 
      valueExpr = {
        req(GCT_processed(), default_annotation_column(), color_map())
        
        # get annotation column
        if (!is.null(input$qc_profile_plots_annotation)) {
          annot_column <- input$qc_profile_plots_annotation
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
        create_profile_plot(gct = GCT_processed(),
                       col_of_interest = annot_column,
                       ome = ome,
                       custom_color_map = annot_color_map,
                       parameters=parameters(),
                       type="norm")
      }
    )
    
    # render summary plot
    output$qc_profile_plot_norm <- renderPlotly(
      ggplotly(qc_profile_plot_norm_reactive(), tooltip = "text")
    )
    
    # sidebar contents
    output$qc_profile_plots_sidebar_contents <- renderUI({
      req(GCT_processed())
      
      add_css_attributes(
        selectInput(
          ns("qc_profile_plots_annotation"),
          "Group by",
          choices = names(GCT_processed()@cdesc),
          selected = default_annotation_column()),
        classes = "small-input",
        styles = "margin-right: 10px"
      )
    })
    
    ## COMPILE EXPORTS ##
    
    
    qc_profile_plot_org_export_function <- function(dir_name) {
      ggsave(
        filename = paste0("qc_profile_plot_org_", ome, ".pdf"), 
        plot = qc_profile_plot_org_reactive(), 
        device = 'pdf',
        path = dir_name,
        width = 10,
        height = 6, 
        units = "in"
      )
    }
    
    qc_profile_plot_norm_export_function <- function(dir_name) {
      ggsave(
        filename = paste0("qc_profile_plot_norm_", ome, ".pdf"), 
        plot = qc_profile_plot_norm_reactive(), 
        device = 'pdf',
        path = dir_name,
        width = 10,
        height = 6, 
        units = "in"
      )
    }
    
    return(
      list(
        qc_profile_plot_org = qc_profile_plot_org_export_function,
        qc_profile_plot_norm = qc_profile_plot_norm_export_function
      )
    )
  })
}

