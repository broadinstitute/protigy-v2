## UI for heatmap options
options_multiomeHeatmapTabUI <- function(id, GENEMAX) {
  ns <- NS(id) # namespace function
  
  tagList(
    
    ## Instructions
    p(strong("Getting started")),
    p(paste("Enter your gene names of interest (official gene symbols, e.g. EGFR) into the text field below.",
            "You can enter up to", GENEMAX, "genes.")),
    hr(),
    
    ## text input
    selectizeInput(ns('genes'), 
                   label=paste('Enter your genes of interest (max. ',
                               GENEMAX,')', sep=''),
                   choices = NULL,
                   multiple=T),
    
    ## inputs to customize heatmap
    fluidRow(
      column(6, radioButtons(ns('zscore'), 
                             label='Z-score', 
                             choices=c('row', 'none'), 
                             selected='row')),
      
      column(6, radioButtons(ns('show.sample.label'),
                          label = "Sample labels",
                          choices = c('show' = TRUE, 'hide' = FALSE),
                          selected = FALSE))
    ), # end fluidRow
    
    fluidRow(
      column(6, textInput(ns('min.val'), 
                          label='min', 
                          value=-2, 
                          width='80%')),
      column(6, textInput(ns('max.val'), 
                          label='max', 
                          value=2, 
                          width='80%'))
    ), #end fluidRow
    
    ## inputs for sorting
    fluidRow(
      column(12, selectizeInput(ns('sort.after'), 'Sort by', 
                                choices = NULL,
                                multiple=FALSE))  
    ), #end fluidRow
    
    ## inputs for max levels in an annotation column
    tags$div(
      numericInput(
        ns('max.levels'),
        label = "Maximum number of levels:",
        value = 5,
        step = 1,
        min = 1
      ),
      id = "inline",
      style = "margin-bottom: 15px;"
    ),
    
    ## inputs for order
    fluidRow(column(12, strong("Data order (drag and drop to re-order)"), br(), orderInput(
      label = "",
      items = list(),
      inputId = ns('ome.order'),
      class = "btn-group-vertical",
      width = '150px'
    )))
    
  ) # end tagList
}

## server for heatmap options
options_multiomeHeatmapTabServer <- function(id, merged_rdesc, sample_anno, setup_submit) {
  moduleServer(
    id,
    ## module function
    function (input, output, session) {
      
      # update selectizeInput for genes
      observeEvent(setup_submit(), {
        updateSelectizeInput(
          session,
          inputId = "genes",
          choices = sort(unique(merged_rdesc()$geneSymbol)),
          server = T)
      })
      
      
      # update selectInput for annotations
      observeEvent(setup_submit(), {
        updateSelectInput(
          session,
          inputId = "sort.after",
          choices = setdiff(names(sample_anno()), 'Sample.ID'))
      })
      
      
      # update data ordering options
      observeEvent(setup_submit(), {
        # Use protigy.ome column if available, otherwise fall back to DataType
        ome_col <- if ("protigy.ome" %in% names(merged_rdesc())) {
          merged_rdesc()$protigy.ome
        } else if ("DataType" %in% names(merged_rdesc())) {
          merged_rdesc()$DataType
        } else {
          "Unknown"
        }
        updateOrderInput(
          session,
          inputId = 'ome.order',
          items = sort(unique(ome_col)))
      })
      
      
      ## get heatmap parameters
      HM.params <- reactive({list(genes.char = input$genes,
                                  zscore = input$zscore,
                                  min.val = as.numeric(input$min.val),
                                  max.val = as.numeric(input$max.val),
                                  sort.after = input$sort.after,
                                  show.sample.label = input$show.sample.label,
                                  ome.order = input$ome.order,
                                  max.levels = input$max.levels)})
      
      
      
      return(HM.params)
    }) # end moduleServer
}