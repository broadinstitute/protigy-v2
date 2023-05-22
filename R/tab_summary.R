


# UI for the summary tab
summaryTabUI <- function(id = "summaryTab") {
  ns <- NS(id) # namespace function
  
  tagList(
    selectInput(ns('ome'), 'Select -ome', choices = NULL),
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
    warning("hard coded column of interest in summary tab")
    col.of.interest <- "PAM50"
    
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
    
    # update-omes choice based on default set in globals
    observe({
      validate(need(globals()$default_ome, "Default -ome not set"))
      updateSelectInput(inputId = 'ome', selected = globals()$default_ome)
    })
    
    # summary quant features plot
    summary.quant.features.plot <- reactive({
      validate(
        need(GCTs, "GCTs not yet processed") %then%
          need(input$ome, "Ome not selected") %then%
          need(input$ome %in% names(GCTs()), "Invalid -ome selection"))
      summary.quant.features(GCTs()[[input$ome]], col.of.interest)
    })
    
    # reactive version of summary quant features for display
    output$summary.quant.features <- renderPlotly({
      ggplotly(summary.quant.features.plot())})
    
    # gather all plots
    all_summary_plots <- reactive({
      plots <- list()
      for (ome in names(GCTs())) {
        plots[[ome]] <- list(
          summary.quant.features = reactive(summary.quant.features(GCTs()[[ome]], col.of.interest)),
          another.plot = reactive(ggplot())
        )
      }
      plots
    })
    
    
    return(all_summary_plots)
  })
}

# plots
summary.quant.features <- function (gct, col.of.interest) {
  # get number of non-missing per sample
  sample_id <- colnames(gct@mat)
  non.missing <- as.data.frame(apply(gct@mat, 2, function(x) sum(!is.na(x))))
  names(non.missing) <- "numFeatures"
  non.missing$SampleID <- as.factor(as.character(rownames(non.missing)))
  non.missing$group <- as.factor(as.character(gct@cdesc[[col.of.interest]]))
  
  non.missing$SampleID <- with(non.missing, reorder(SampleID, as.integer(group)))
  
  p <- ggplot(data = non.missing, aes(x = SampleID, y = numFeatures, fill = group)) +
    geom_bar(stat = 'identity') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  return(p)
}