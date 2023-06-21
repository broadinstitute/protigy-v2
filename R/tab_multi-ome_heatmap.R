################################################################################
# Module: MULTI-OME HEATMAP
################################################################################


################################################################################
# Shiny functions (server and UI)
################################################################################

## UI for heatmap tab
multiomeHeatmapTabUI <- function(id = 'multiomeHeatmapTab') {
  ns <- NS(id) # namespace function
  
  ## main box
  main_box <- shinydashboardPlus::box(
    plotOutput(ns("heatmap"), height = "auto"),
    status = "primary",
    width = 12,
    title = "Multi-Ome Heatmap",
    headerBorder = TRUE,
    solidHeader = TRUE,
    sidebar = boxSidebar(
      tags$div(uiOutput(ns("sidebar_content")), style = "margin-right:10px"),
      id = "heatmap_sidebar",
      background = "rgba(91, 98, 104, 0.9)",
      startOpen = TRUE,
      icon = icon("gears", class = "fa-2xl")
    )
  ) # end box
  
  # set minimum height of box body
  main_box$children[[1]]$children[[2]] <- add_css_attributes(
    main_box$children[[1]]$children[[2]],
    styles = "min-height: 500px"
  )
  
  # put the box in a fluidRow and return
  fluidRow(main_box)
}

multiomeHeatmapTabServer <- function(
    id = 'multiomeHeatmapTab',
    GCTs_and_params, 
    globals,
    GENEMAX = 20) {
  ## module function
  moduleServer(id, function (input, output, session) {
      
    ## GATHERING INPUTS ##
    
    # get namespace
    ns <- session$ns
    
    # GCTs to use for analysis/visualization
    GCTs <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$GCTs
    })
    
    # gather relevant variables from globals
    custom_colors <- reactive({
      req(globals$colors, sample_anno())
      multiome_heatmap_custom_colors(globals$colors, sample_anno())
    })
    
    observe(print(custom_colors()))
    
    # vector of all omes
    all_omes <- reactive(names(GCTs()))
    
    
    ## Setup ##
    setup_ui <- setup_multiomeHeatmapTabUI(id = ns("setup"))
    output$sidebar_content <- renderUI({setup_ui})
    gcts_merged <- setupmultiomeHeatmapTabServer(id = "setup", GCTs = GCTs)
    
    merged_rdesc <- reactive({
      validate(need(gcts_merged(), "GCTs not processed for multi-ome heatmap"))
      gcts_merged()@rdesc
    })
    sample_anno <- reactive({
      validate(need(gcts_merged(), "GCTs not processed for multi-ome heatmap"))
      gcts_merged()@cdesc
    })
    merged_mat <- reactive({
      validate(need(gcts_merged(), "GCTs not processed for multi-ome heatmap"))
      gcts_merged()@mat
    })
    
    
    # go to heatmap options
    observeEvent(gcts_merged(), {
      output$sidebar_content <- renderUI({
        tagList(
          options_multiomeHeatmapTabUI(ns("options"), GENEMAX = GENEMAX),
          hr(),
          actionButton(ns("back"), "Back to setup")
        )
      })
    })
    
    # go back to setup
    observeEvent(input$back, {
      output$sidebar_content <- renderUI({setup_ui})
    })
    
    
    ## Heatmap options 
    HM.params <- options_multiomeHeatmapTabServer("options",
                                                  merged_rdesc = merged_rdesc,
                                                  sample_anno = sample_anno)
    
    
    ## Generate Heatmap
    HM.out <- reactive({
      validate(
        need(merged_rdesc(), "Complete setup to see heatmap") %then%
        need(merged_mat(), "Complete setup to see heatmap") %then%
        need(sample_anno(), "Complete setup to see heatmap") %then%
        need(HM.params()$genes.char, "Input genes to see results") %then%
        need(HM.params()$min.val < HM.params()$max.val, "Input valid min and max")
      )
      
      myComplexHeatmap(params = HM.params(),
                       merged_rdesc = merged_rdesc(),
                       merged_mat = merged_mat(),
                       sample_anno = sample_anno(),
                       GENEMAX = GENEMAX)
    })
    
    HM <- reactive({
      validate(need(HM.out(), "Heatmap not avaliable"))
      draw(HM.out()$HM, annotation_legend_side = 'bottom')
    })
    HM.Table <- reactive(HM.out()$Table)
    
    plot_height <- reactive({
      tryCatch(
        dynamicHeightHM(nrow(HM.Table())), 
        error = function(c) 400
      )
    })
    
    output$heatmap <- renderPlot(
      HM(), 
      height = plot_height
    )

    

  }) # end moduleServer
}
