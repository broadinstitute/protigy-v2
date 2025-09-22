################################################################################
# Module: QC_PCA
#
# Produce PCA plot after normalization
################################################################################

################################################################################
# Shiny functions (UI and server)
################################################################################

# UI for the QCPCA tab
# contains the structure for the big tabbed box with omes
QCPCA_Tab_UI <- function(id = "QCPCATab") {
  ns <- NS(id) # namespace function, wrap UI inputId's with this `ns("inputId")`
  
  tagList(
    
    # display omes tabs
    fluidRow(uiOutput(ns("ome_tabset_box")))
    
  ) # end tagList
}

# server for the QCPCA tab
# contains the structure for the big tabbed box with omes
QCPCA_Tab_Server <- function(id = "QCPCATab",
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
          QCPCA_Ome_UI(id = ns(ome), ome = ome)
          
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
        QCPCA_Ome_Server(
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
QCPCA_Ome_UI <- function (id, ome) {
                
  ns <- NS(id)
  
  tagList(
    # PCA plots
    fluidRow(shinydashboardPlus::box(
      plotlyOutput(ns("qc_PCA_plot")),
      br(),
      plotlyOutput(ns("qc_PCA_reg"), height="auto"),
      sidebar = boxSidebar(
        uiOutput(ns("qc_PCA_sidebar_contents")),
        id = ns("qc_PCA_sidebar"),
        width = 25,
        icon = icon("gears", class = "fa-2xl"),
        background = "rgba(91, 98, 104, 0.9)"
      ),
      status = "primary",
      width = 12,
      title = "PCA Plots",
      headerBorder = TRUE,
      solidHeader = TRUE))
  )
}


# server for an individual ome
QCPCA_Ome_Server <- function(id,
                                   ome,
                                   GCT_processed,
                                   parameters,
                                   default_annotation_column,
                                   color_map) {
  
  ## module function
  moduleServer(id, function (input, output, session) {
    
    # get namespace, use in renderUI-like functions
    ns <- session$ns
    
    # sidebar contents
    output$qc_PCA_sidebar_contents <- renderUI({
      req(GCT_processed())
      
      tagList(
        add_css_attributes(
          selectInput(
            ns("qc_PCA_annotation"),
            "Group by",
            choices = names(GCT_processed()@cdesc),
            selected = default_annotation_column()),
          classes = "small-input",
          styles = "margin-right: 10px"
        ),
        
        add_css_attributes(
          selectInput(
            ns("qc_PCA_PC1"),
            "PC1",
            choices = 1:10,
            selected = 1),
          classes = "small-input",
          styles = "margin-right: 10px"
        ),
        
        add_css_attributes(
          selectInput(
            ns("qc_PCA_PC2"),
            "PC2",
            choices = 1:10,
            selected = 2),
          classes = "small-input",
          styles = "margin-right: 10px"
        ),
        
        br(),
        
        add_css_attributes(
          checkboxInput(
            ns("qc_PCA_add_second_var"),
            "Add second variable for visualization",
            value = FALSE),
          classes = "small-input",
          styles = "margin-right: 10px"
        ),
        
        conditionalPanel(
          condition = paste0("input['", ns("qc_PCA_add_second_var"), "']"),
          
          add_css_attributes(
            selectInput(
              ns("qc_PCA_second_annotation"),
              "Second variable",
              choices = names(GCT_processed()@cdesc)),
            classes = "small-input",
            styles = "margin-right: 10px"
          ),
          
          add_css_attributes(
            selectInput(
              ns("qc_PCA_var1_display"),
              "First variable display",
              choices = c("Color" = "color", "Shape" = "shape"),
              selected = "color"),
            classes = "small-input",
            styles = "margin-right: 10px"
          ),
          
          add_css_attributes(
            selectInput(
              ns("qc_PCA_var2_display"),
              "Second variable display",
              choices = c("Color" = "color", "Shape" = "shape"),
              selected = "shape"),
            classes = "small-input",
            styles = "margin-right: 10px"
          )
        )
      )
    })

    ## PCA PLOT ##
    
    # reactive
    qc_PCA_plot_reactive <- eventReactive(
      eventExpr = c(input$qc_PCA_annotation, input$qc_PCA_PC1, input$qc_PCA_PC2, 
                    input$qc_PCA_add_second_var, input$qc_PCA_second_annotation,
                    input$qc_PCA_var1_display, input$qc_PCA_var2_display, color_map()), 
      valueExpr = {
        
        # get annotation column
        if (!is.null(input$qc_PCA_annotation)) {
          annot_column <- input$qc_PCA_annotation
        } else {
          annot_column <- default_annotation_column()
        }
        
        # get second annotation column if selected
        second_annot_column <- NULL
        var1_display <- "color"
        var2_display <- "shape"
        
        if (!is.null(input$qc_PCA_add_second_var) && input$qc_PCA_add_second_var) {
          if (!is.null(input$qc_PCA_second_annotation)) {
            second_annot_column <- input$qc_PCA_second_annotation
          }
          if (!is.null(input$qc_PCA_var1_display)) {
            var1_display <- input$qc_PCA_var1_display
          }
          if (!is.null(input$qc_PCA_var2_display)) {
            var2_display <- input$qc_PCA_var2_display
          }
          
          # validate that both variables don't use the same display method
          if (var1_display == var2_display) {
            validate(need(FALSE, "First and second variables cannot use the same display method (color or shape). Please select different display options."))
          }
          
          # validate that second variable is different from first
          if (!is.null(second_annot_column) && second_annot_column == annot_column) {
            validate(need(FALSE, "Second variable must be different from the first variable. Please select a different variable."))
          }
        }
        
        # get custom colors
        custom_colors <- color_map()
        if (annot_column %in% names(custom_colors)) {
          annot_color_map <- custom_colors[[annot_column]]
        } else {
          annot_color_map <- NULL
        }
        
        # generate plot
        create_PCA_plot(gct = GCT_processed(),
                            col_of_interest = annot_column,
                            ome = ome,
                            custom_color_map = annot_color_map,
                            comp.x = as.numeric(ifelse(is.null(input$qc_PCA_PC1), 1, input$qc_PCA_PC1)),
                            comp.y = as.numeric(ifelse(is.null(input$qc_PCA_PC2), 2, input$qc_PCA_PC2)),
                            second_col_of_interest = second_annot_column,
                            var1_display = var1_display,
                            var2_display = var2_display)
      }
    )
    
    # render summary plot
    output$qc_PCA_plot <- renderPlotly(
      ggplotly(qc_PCA_plot_reactive(), tooltip = "text")
    )
    
    ## PCA REGRESSION ##
    
    # reactive
    qc_PCA_reg_reactive <- eventReactive(
      eventExpr = c(input$qc_PCA_annotation, color_map()), 
      valueExpr = {
        req(GCT_processed(), default_annotation_column(), color_map())
        
        # get annotation column
        if (!is.null(input$qc_PCA_annotation)) {
          annot_column <- input$qc_PCA_annotation
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
        create_PCA_reg(gct = GCT_processed(),
                           col_of_interest = annot_column,
                           ome = ome,
                           custom_color_map = annot_color_map)
      }
    )
    
    # render summary plot
    output$qc_PCA_reg <- renderPlotly(
      ggplotly(qc_PCA_reg_reactive())
    )
    
    ## COMPILE EXPORTS ##
    
    
    qc_PCA_plot_export_function <- function(dir_name) {
      ggsave_params <- get_ggsave_params()
      ggsave(
        filename = paste0("qc_PCA_plot_", ome, ".pdf"), 
        plot = qc_PCA_plot_reactive(), 
        device = 'pdf',
        path = dir_name,
        width = ggsave_params$width,
        height = ggsave_params$height, 
        units = ggsave_params$units
      )
    }
    
    qc_PCA_reg_export_function <- function(dir_name) {
      ggsave_params <- get_ggsave_params()
      ggsave(
        filename = paste0("qc_PCA_reg_", ome, ".pdf"), 
        plot = qc_PCA_reg_reactive(), 
        device = 'pdf',
        path = dir_name,
        width = ggsave_params$width,
        height = ggsave_params$height, 
        units = ggsave_params$units
      )
    }
    
    return(
      list(
        qc_PCA_plot = qc_PCA_plot_export_function,
        qc_PCA_reg = qc_PCA_reg_export_function
      )
    )
  })
}

