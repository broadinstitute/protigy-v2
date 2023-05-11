


# UI for the summary tab
summaryTabUI <- function(id = "summaryTab") {
  ns <- NS(id) # namespace function
  
  tagList(
    selectInput(ns('ome'), 'Select -ome', 
                choices = names(GCTs),
                selected = names(GCTs)[1]),
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
summaryTabServer <- function(id = "summaryTab") { moduleServer( id,
  ## module function
  function (input, output, session) {
    
    # get namespace
    ns <- session$ns
    
    output$summary.quant.features <- renderPlotly({
      summary.quant.features(GCTs[[input$ome]], col.of.interest)
      })
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
  
  if(interactive()) {
    return(ggplotly(p))
  } else {
    return(p)
  }
}