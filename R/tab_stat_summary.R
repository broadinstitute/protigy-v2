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
        # statSummary_Ome_Server(
        #   # TODO: edit inputs to the ome server function, the last 4 may be unnecessary
        #   id = ome,
        #   ome = ome,
        #   GCT_processed = reactive(GCTs()[[ome]]),
        #   parameters = reactive(parameters()[[ome]]),
        #   GCT_original = reactive(GCTs_original()[[ome]]),
        #   default_annotation_column = reactive(default_annotations()[[ome]]),
        #   color_map = reactive(custom_colors()[[ome]])
        # )
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

      # # P.val histogram box
      # fluidRow(shinydashboardPlus::box(
      #   plotlyOutput(ns("quant_features_plot")),
      #   sidebar = boxSidebar(
      #     uiOutput(ns("quant_features_sidebar_contents")),
      #     id = ns("quant_features_sidebar"),
      #     width = 25,
      #     icon = icon("gears", class = "fa-2xl"),
      #     background = "rgba(91, 98, 104, 0.9)"
      #   ),
      #   status = "primary",
      #   width = 12,
      #   title = "P.value Histogram",
      #   headerBorder = TRUE,
      #   solidHeader = TRUE
      # ))

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

    ## DATASET INFO ##
    #Capture system output in a file
    # timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    # filename <- paste0("C:/Users/dabburi/Documents/run_", timestamp, ".txt")
    # sink(filename)
    # 
    # 
    #   print("=== MODULE DEBUG ===\n")
    #   print("Module ID:")
    #   print(id)
    #   print("Received ome:")
    #   print(ome)
    #   print("Available stat_results keys:")
    #   print(names(stat_results()))
    #   
    #   sr <- stat_results()[[ome]]
    #   if (is.null(sr)) {
    #     print("stat_results()[[ome]] is NULL\n")
    #   } else {
    #     print("nrow:")
    #     print(nrow(sr))
    #     print(head(sr))
    #   }
    # 
    #   print("DEBUG: class of sr:")
    #   print(class(sr))
    #   #lapply(stat_results(), class)
    #   
    # 
    # sink()
    
    # render dataset info
    output$dataset_table <- renderTable(
      df <- stat_results()[[ome]],
      data.frame(
        Description = c("Features tested", "Significant features"),
        Count = c(nrow(df), 10)
        #Count = c(nrow(df), sum(df$significant == TRUE, na.rm = TRUE))
      )
    )

    # WORKFLOW INFO ##

    # render workflow info
    output$workflow_table <- renderTable(
      data.frame(
        Description = c("Test chosen", "Cutoff"),
        Count = c(stat_param()[[ome]]$test, 0.05)#replace 10 when I find out if the cutoff number should change as the user adjusts it on this page or whether this is just 0.05
      )
    )

    ## P.value Histogram- for both non-adj and adj p values ##
    # Get hist. code from Dr.Clark


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