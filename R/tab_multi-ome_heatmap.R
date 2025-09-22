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
    
    # Use the merged GCT from setup instead of creating a new one
    GCTs_merged <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      validate(need(GCTs_and_params()$GCTs_merged, "GCTs not yet merged"))
      GCTs_and_params()$GCTs_merged
    })
    
    # vector of all omes
    all_omes <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      names(GCTs_and_params()$GCTs)
    })
    
            # Setup UI with annotation selection
            setup_ui <- renderUI({
              tagList(
                h4("Multi-Ome Heatmap Setup"),
                p("This module uses the merged GCT created during the main setup process."),
                hr(),
                
                # Annotation selection
                pickerInput(
                  ns("selected_annotations"),
                  label = "Select annotations to display:",
                  choices = available_annotations(),
                  selected = initial_selected_annotations(),
                  multiple = TRUE,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    selectAllText = "Select All",
                    deselectAllText = "Deselect All",
                    noneSelectedText = "No annotations selected"
                  )
                ),
                
                hr(),
                actionButton(ns("submit_setup"), "Plot Heatmap",
                            class = "btn btn-primary")
              )
            })
    
    output$sidebar_content <- setup_ui
    
    # Create a simple submit reactive for compatibility
    setup_submit <- reactive({
      input$submit_setup
    })
    
    # Track which screen we're on
    current_screen <- reactiveVal("setup")
    
    # Save selected annotations
    saved_annotations <- reactiveVal(NULL)
    
    # Save all heatmap settings
    saved_heatmap_settings <- reactiveVal(NULL)
    
    # Update screen when navigating
    observeEvent(input$submit_setup, {
      # Save current selections before moving to options
      if (!is.null(input$selected_annotations)) {
        saved_annotations(input$selected_annotations)
      }
      current_screen("options")
    })
    
    # Save heatmap settings when they change
    observeEvent(HM.params(), {
      if (current_screen() == "options") {
        saved_heatmap_settings(HM.params())
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$back, {
      current_screen("setup")
    })
    
    # Use the merged GCT directly from setup
    merged_rdesc <- reactive({
      validate(need(GCTs_merged(), "GCTs not yet processed"))
      GCTs_merged()@rdesc
    })
    
    sample_anno <- reactive({
      validate(need(GCTs_merged(), "GCTs not yet processed"))
      GCTs_merged()@cdesc
    })
    
    # Available annotations (excluding Sample.ID)
    available_annotations <- reactive({
      validate(need(sample_anno(), "Sample annotations not available"))
      # Exclude Sample.ID and any other system columns
      system_cols <- c("Sample.ID")
      setdiff(colnames(sample_anno()), system_cols)
    })
    
    # Default annotation for default ome
    default_annotation <- reactive({
      if (is.null(globals$default_ome) || is.null(globals$default_annotations)) {
        return(NULL)
      }
      default_annot <- globals$default_annotations[[globals$default_ome]]
      if (is.null(default_annot)) {
        return(NULL)
      }
      return(default_annot)
    })
    
    # Get initial selected annotations (saved or default)
    initial_selected_annotations <- reactive({
      if (!is.null(saved_annotations())) {
        return(saved_annotations())
      } else {
        return(default_annotation())
      }
    })
    
    
    merged_mat <- reactive({
      validate(need(GCTs_merged(), "GCTs not yet processed"))
      GCTs_merged()@mat
    })
    
    # gather colors from globals
    custom_colors <- reactive({
      req(globals$colors$multi_ome)
      globals$colors$multi_ome
    })
    

    ## Heatmap options 
    
    # get heatmap options UI
    options_ui <- renderUI({tagList(
      options_multiomeHeatmapTabUI(ns("options"), GENEMAX = GENEMAX),
      hr(),
      actionButton(ns("back"), "Back to setup")
    )})
    
    # go to heatmap options when setup is submitted
    observeEvent(setup_submit(), {
      output$sidebar_content <- options_ui
    })
    
    # go back to setup
    observeEvent(input$back, {
      output$sidebar_content <- setup_ui
      
      # Update pickerInput to restore saved selections
      if (!is.null(saved_annotations())) {
        updatePickerInput(
          session,
          inputId = "selected_annotations",
          selected = saved_annotations()
        )
      }
    })
    
    # Get heatmap parameters
    HM.params <- options_multiomeHeatmapTabServer("options",
                                                  merged_rdesc = merged_rdesc,
                                                  sample_anno = sample_anno,
                                                  setup_submit = setup_submit,
                                                  globals = globals,
                                                  selected_annotations = reactive({
                                                    tryCatch({
                                                      if (is.null(input$selected_annotations)) {
                                                        return(NULL)
                                                      }
                                                      input$selected_annotations
                                                    }, error = function(e) {
                                                      return(NULL)
                                                    })
                                                  }),
                                                  saved_settings = saved_heatmap_settings)
    
    
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
                       custom_colors = custom_colors(),
                       GENEMAX = GENEMAX,
                       selected_annotations = {
                         if (is.null(input$selected_annotations)) {
                           return(NULL)
                         }
                         input$selected_annotations
                       })
    })
    
    HM <- reactive({
      # Show blank plot when on setup screen
      if (current_screen() == "setup") {
        return(NULL)
      }
      
      validate(need(HM.out()$HM, "Heatmap not avaliable"))
      draw_multiome_HM(HM.out()$HM)
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
    
    
    ## function for exporting
    multiome_heatmap_export_function <- function(dir_path) {
      req(HM.out()$HM)
      
      # Use special dimensions for multi-ome heatmap
      pdf_params <- get_pdf_params("multiome_heatmap")
      pdf(file = file.path(dir_path, "multiome-heatmap.pdf"),
          width = pdf_params$width,
          height = pdf_params$height)
      draw_multiome_HM(HM.out()$HM)
      dev.off()
    }

    return(list(multi_ome = list(
      multiome_heatmap = multiome_heatmap_export_function
    )))

  }) # end moduleServer
}
