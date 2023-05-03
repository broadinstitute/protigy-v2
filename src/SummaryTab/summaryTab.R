


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