


# UI for the summary tab
summaryTabUI <- function(id = "summaryTab") {
  ns <- NS(id) # namespace function
  
  tagList(
    fluidRow(uiOutput(ns("summary_plots_tabs")))
    
  ) # end tagList
}

# server for the summary tab
summaryTabServer <- function(id = "summaryTab", GCTs_and_params, globals) { moduleServer( id,
  ## module function
  function (input, output, session) {
    
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
            column(
              6,
              # Data summary card
              card(
                class = "my-custom-card",
                card_header(
                  "Data"
                ),
                card_body( tagList(
                ))
              )
            ),
            column(
              6,
              # Data workflow card
              card(
                class = "my-custom-card",
                card_header(
                  "Workflow"
                ),
                card_body( 
                  tableOutput(
                    ns(paste0(ome, "_summary_workflow"))
                  )
                )
              )
            )
          ),
          
          br(),
          
          # quantified features card
          fluidRow(column(12, card(
            class = "my-custom-card",
            card_header(
              "Quantified Features"
              ),
            card_body( tagList(
              add_classes(selectInput(
                ns(paste0(ome, "_summary_quant_features_annotation")),
                "Group by",
                choices = names(GCTs()[[ome]]@cdesc),
                selected = default_annotations()[[ome]]),
                classes = "small-input"),
              
              plotlyOutput(ns(paste0(ome, "_summary_quant_features_plot")))
            ))
          )))
        )
      })
      
      do.call(tabBox, c(tabs, list(id = ns("summary_plots"),
                                   width = 12,
                                   selected = isolate(default_ome()))))
    })
    
    # update selected tab based on default -ome
    observe({
      updateTabsetPanel(inputId = "summary_plots", selected = default_ome())
    })
    
    # workflow data tables list
    summary_workflows_list <- reactive({
      validate(need(parameters(), "missing parameters"),
               need(all_omes(), "missing omes"))

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
      
      sapply(all_omes(), function(ome) {
        params <- parameters()[[ome]]
        
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
    })
    
    # render workflow for each -ome
    observeEvent(input$summary_plots, {
      current_ome <- input$summary_plots
      output[[paste0(current_ome, "_summary_workflow")]] <- renderTable({
        summary_workflows_list()[[current_ome]]
      }, rownames = TRUE, colnames = FALSE)
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
        ggplotly(summary_quant_features_plots_list()[[current_ome]])
      })
    })
    
    
    # # summary quant features plot
    # # summary.quant.features.plot <- reactive({
    #   validate(
    #     need(GCTs, "GCTs not yet processed") %then%
    #       need(input$ome, "Ome not selected") %then%
    #       need(input$ome %in% names(GCTs()), "Invalid -ome selection") %then%
    #       need(input$col_of_interest, "Missing annotation selection") %then%
    #       need(input$col_of_interest %in% names(GCTs()[[input$ome]]@cdesc),
    #            "invalid annotation selection"))
    #   summary.quant.features(GCTs()[[input$ome]], input$col_of_interest)
    # })
    # 
    # # reactive version of summary quant features for display
    # output$summary.quant.features <- renderPlotly({
    #   ggplotly(summary.quant.features.plot())})
    # 
    # # gather all plots
    # all_summary_plots <- reactive({
    #   
    #   validate(
    #     need(GCTs, "GCTs not yet processed") %then%
    #       need(input$ome, "Ome not selected") %then%
    #       need(input$ome %in% names(GCTs()), "Invalid -ome selection") %then%
    #       need(input$col_of_interest, "Missing annotation selection"))
    #   
    #   plots <- list()
    #   for (ome in names(GCTs())) {
    #     
    #     if (input$col_of_interest %in% names(GCTs()[[ome]]@cdesc)) {
    #       plots[[ome]] <- list(
    #         summary.quant.features = summary.quant.features(GCTs()[[ome]], input$col_of_interest),
    #         another.plot = ggplot()
    #       )
    #     } else {
    #       warning(paste(input$col_of_interest, "not found in", ome))
    #       
    #     }
    #     
    #     
    #   }
    #   plots
    # })
    
    
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