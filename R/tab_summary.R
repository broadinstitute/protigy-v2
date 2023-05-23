


# UI for the summary tab
summaryTabUI <- function(id = "summaryTab") {
  ns <- NS(id) # namespace function
  
  tagList(
    selectInput(ns('ome'), 'Select -ome', choices = NULL),
    selectInput(ns('col_of_interest'), "Annotation of interest", choices = NULL),
    fluidRow(box(
      title = "Quantified Features",
      solidHeader = TRUE,
      status = 'primary',
      width = 12,
      plotlyOutput(ns('summary.quant.features'))
    ))
    
  ) # end tagList
}

# server for the summary tab
summaryTabServer <- function(id = "summaryTab", GCTs_and_params, globals) { moduleServer( id,
  ## module function
  function (input, output, session) {
    
    # get namespace
    ns <- session$ns
    
    # for now, hard code this
    warning("Column of interest may not exist!")
    
    # gather GCTs and parameters
    GCTs <- reactive({
      validate(need(GCTs_and_params()$GCTs, "GCTs not yet processed"))
      GCTs_and_params()$GCTs
    })
    parameters <- reactive({
      validate(need(GCTs_and_params()$GCTs, "GCTs not yet processed"))
      GCTs_and_params()$parameters
    })
    
    # update -ome options once GCTs processed
    observe(updateSelectInput(inputId = 'ome', choices = names(GCTs())))
    
    # update annotaion of interest once GCTs processed
    observe({
      req(GCTs(), input$ome, input$ome %in% names(GCTs()))
      updateSelectInput(inputId = 'col_of_interest',
                        choices = names(GCTs()[[input$ome]]@cdesc))
    })
    
    # update-omes choice based on default set in globals
    observe({
      req(globals()$default_ome, "Default -ome not set")
      updateSelectInput(inputId = 'ome', selected = globals()$default_ome)
    })
    
    # summary quant features plot
    summary.quant.features.plot <- reactive({
      validate(
        need(GCTs, "GCTs not yet processed") %then%
          need(input$ome, "Ome not selected") %then%
          need(input$ome %in% names(GCTs()), "Invalid -ome selection") %then%
          need(input$col_of_interest, "Missing annotation selection") %then%
          need(input$col_of_interest %in% names(GCTs()[[input$ome]]@cdesc),
               "invalid annotation selection"))
      summary.quant.features(GCTs()[[input$ome]], input$col_of_interest)
    })
    
    # reactive version of summary quant features for display
    output$summary.quant.features <- renderPlotly({
      ggplotly(summary.quant.features.plot())})
    
    # gather all plots
    all_summary_plots <- reactive({
      
      validate(
        need(GCTs, "GCTs not yet processed") %then%
          need(input$ome, "Ome not selected") %then%
          need(input$ome %in% names(GCTs()), "Invalid -ome selection") %then%
          need(input$col_of_interest, "Missing annotation selection"))
      
      plots <- list()
      for (ome in names(GCTs())) {
        
        if (input$col_of_interest %in% names(GCTs()[[ome]]@cdesc)) {
          plots[[ome]] <- list(
            summary.quant.features = summary.quant.features(GCTs()[[ome]], input$col_of_interest),
            another.plot = ggplot()
          )
        } else {
          warning(paste(input$col_of_interest, "not found in", ome))
          
        }
        
        
      }
      plots
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
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  return(p)
}