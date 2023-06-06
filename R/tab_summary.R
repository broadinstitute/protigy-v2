


# UI for the summary tab
summaryTabUI <- function(id = "summaryTab") {
  ns <- NS(id) # namespace function
  
  tagList(
    fluidRow(uiOutput(ns("summary_plots_tabs")))
    
  ) # end tagList
}

# server for the summary tab
summaryTabServer <- function(id = "summaryTab", GCTs_and_params, globals, GCTs_original) { 
  
  ## module function
  moduleServer(id, function (input, output, session) {
    
    # get namespace
    ns <- session$ns
    
    # gather GCTs and parameters
    GCTs <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$GCTs
    })
    parameters <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$parameters
    })
    
    # gather relevant variables from globals
    all_omes <- reactive(globals()$omes)
    default_ome <- reactive(globals()$default_ome)
    default_annotations <- reactive(globals()$default_annotations)
    
    # summary plots tabs
    output$summary_plots_tabs <- renderUI({
      req(all_omes(), default_ome(), default_annotations(), GCTs())

      tabs <- lapply(all_omes(), function(ome){
        tabPanel(
          title = ome,
          
          fluidRow(
            # Data summary box
            shinydashboardPlus::box(
              tableOutput(ns(paste0(ome, "_summary_dataset"))),
              title = "Dataset",
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              headerBorder = TRUE
            ),
            
            # Data workflow card
            shinydashboardPlus::box(
              tableOutput(ns(paste0(ome, "_summary_workflow"))),
              title = "Workflow",
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              headerBorder = TRUE
            ),
          ),
          
          # Quantified features box
          fluidRow(shinydashboardPlus::box(
            plotlyOutput(ns(paste0(ome, "_summary_quant_features_plot"))),
            sidebar = boxSidebar(
              tags$div(
                add_classes(selectInput(
                  ns(paste0(ome, "_summary_quant_features_annotation")),
                  "Group by",
                  choices = names(GCTs()[[ome]]@cdesc),
                  selected = default_annotations()[[ome]]),
                  classes = "small-input"),
                style = "margin-right: 10px"
              ),
              id = "quant-feat-sidebar",
              width = 25
            ),
            status = "primary",
            width = 12,
            title = "Quantified Features",
            headerBorder = TRUE,
            solidHeader = TRUE
          )
        ))
      })
      
      # combine all tabs into tabSetPanel
      tab_set_panel <- do.call(tabsetPanel, c(tabs, list(id = ns("summary_plots"),
                                   selected = isolate(default_ome()))))
      
      # put everything in a box and return
      add_classes(
        shinydashboardPlus::box(
          tab_set_panel,
          width = 12
        ), classes = c("box-no-header", "box-with-tabs")
      )
    })
    
    # update selected tab based on default -ome
    observe({
      updateTabsetPanel(inputId = "summary_plots", selected = default_ome())
    })
    
    # workflow data tables list
    summary_workflows_list <- reactive({
      req(parameters(), all_omes())
      generate_summary_workflows_list(parameters = parameters(), 
                                      all_omes = all_omes())
    })
    
    # render workflow for each -ome
    observeEvent(input$summary_plots, {
      current_ome <- input$summary_plots
      output[[paste0(current_ome, "_summary_workflow")]] <- renderTable({
        summary_workflows_list()[[current_ome]]
      }, rownames = TRUE, colnames = FALSE)
    })
    
    # dataset description list
    summary_dataset_list <- reactive({
      req(parameters(), all_omes(), GCTs_original(), GCTs()) 
      
      generate_summary_dataset_list(parameters = parameters(),
                                    all_omes = all_omes(),
                                    GCTs_original = GCTs_original(),
                                    GCTs_processed = GCTs())
    })
    
    # render dataset description for each -ome
    observeEvent(input$summary_plots, {
      current_ome <- input$summary_plots
      output[[paste0(current_ome, "_summary_dataset")]] <- renderTable({
        summary_dataset_list()[[current_ome]]
      }, rownames = TRUE, colnames = TRUE)
    })
    
    # summary quant features plots annotations
    summary_quant_features_annotations <- reactive({
      req(default_annotations())
      
      sapply(all_omes(), function(ome) {
        if (paste0(ome, "_summary_quant_features_annotation") %in% names(input)) {
          input[[paste0(ome, "_summary_quant_features_annotation")]]
        } else {
          default_annotations()[[ome]]
        }
      }, simplify = FALSE)
    })
    
    # summary plots list
    summary_quant_features_plots_list <- reactive({
      validate(
        need(GCTs(), "GCTs not yet processed") %then%
        need(all_omes(), "Omes not avaliable") %then%
        need(default_annotations(), "Default annotation not avaliable"))
      
      all_gcts <- GCTs()
      all_annotations <- summary_quant_features_annotations()

      sapply(all_omes(), function(ome) {
        summary.quant.features(all_gcts[[ome]], all_annotations[[ome]])
      }, simplify = FALSE)
    })
    
    # render plot for each -ome
    observeEvent(input$summary_plots, {
      current_ome <- input$summary_plots
      output[[paste0(current_ome, "_summary_quant_features_plot")]] <- renderPlotly({
        req(summary_quant_features_plots_list()[[current_ome]])
        ggplotly(summary_quant_features_plots_list()[[current_ome]])
      })
    })
    
    ## COMPILE PLOTS FOR EXPORT
    all_summary_plots <- reactive({
      # gather all of the lists of plots
      quant_features_plots = summary_quant_features_plots_list()
      
      # make a list with all plots for each ome
      # the names for this list are the omes
      sapply(all_omes(), function(ome) {
        list(
          quant_features_plot = quant_features_plots[[ome]] + 
            ggtitle(paste("Quantified features:", ome))
        )
      }, simplify = FALSE)

    })
    
    return(all_summary_plots)
  })
}

# plots
summary.quant.features <- function (gct, col_of_interest) {
  # get number of non-missing per sample
  sample_id <- colnames(gct@mat)
  non.missing <- as.data.frame(apply(gct@mat, 2, function(x) sum(!is.na(x))))
  names(non.missing) <- "numFeatures"
  non.missing$SampleID <- as.factor(as.character(rownames(non.missing)))
  non.missing$group <- as.factor(as.character(gct@cdesc[[col_of_interest]]))
  
  non.missing$SampleID <- with(non.missing, reorder(SampleID, as.integer(group)))
  
  p <- ggplot(data = non.missing, aes(x = SampleID, y = numFeatures, fill = group)) +
    geom_bar(stat = 'identity') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    xlab("# Quantified Features") +
    ylab("Sample columns") + 
    labs(fill = col_of_interest)
  
  return(p)
}


# tables
generate_summary_workflows_list <- function(parameters, all_omes) {
  params_to_display <- list(
    "File name" = "gct_file_name",
    "Annotation column" = "annotation_column",
    "Intensity data" = "intensity_data",
    "Log transformation" = "log_transformation",
    "Data normalization" = "data_normalization",
    "Normalized by group" = "group_normalization",
    "Data filter" = "data_filter",
    "Max missing %" = "max_missing"
  )
  
  sapply(all_omes, function(ome) {
    params <- parameters[[ome]]
    
    # include group normalization column
    if (params$group_normalization) {
      params_to_display <- append(
        params_to_display,
        list("Group normalization col." = "group_normalization_column"),
        after = which(params_to_display == "group_normalization"))
    }
    
    # include filtering percentile
    if (params$data_filter == "StdDev") {
      params_to_display <- append(
        params_to_display,
        list("Std. Dev. filter percentile" = "data_filter_sd_pct"),
        after = which(params_to_display == "data_filter"))
    }
    
    df <- t(as.data.frame(params))
    df <- df[as.character(params_to_display), , drop = FALSE]
    rownames(df) <- names(params_to_display)
    df
  }, simplify = FALSE)
}

# dataset summary list
generate_summary_dataset_list <- function(parameters, all_omes, 
                                          GCTs_processed, GCTs_original) {

  sapply(all_omes, function(ome) {
    gct_original <- GCTs_original[[ome]]
    gct_processed <- GCTs_processed[[ome]]
    params <- parameters[[ome]]
    dataset_summary <- list(
      "Features (original)" = sum(
        apply(gct_original@mat, 1, function(x) sum(!is.na(x)) > 1)),
      "Features (post-filtering)" = sum(
        apply(gct_processed@mat, 1, function(x) sum(!is.na(x)) > 1)),
      "Expression columns" = dim(gct_processed@mat)[2],
      "Groups" = length(unique(gct_processed@cdesc[[params$annotation_column]]))
    )
    
    # check if there are any unquantified features
    unquantified_features <- apply(gct_processed@mat, 1, function(x) all(is.na(x)))
    if (any(unquantified_features)) {
      append(dataset_summary,
             list("Features w/o quantification" = sum(unquantified_features)),
             after = which(names(dataset_summary) == "Features (post-filtering)"))
    }
    
    # compile into a data frame
    df <- t(data.frame(dataset_summary))
    colnames(df) <- "Number"
    rownames(df) <- names(dataset_summary)
    df
  }, simplify = FALSE)

}