

## UI for heatmap tab
heatmapTabUI <- function(id = 'heatmapTab') {
  ns <- NS(id) # namespace function
  
  sidebarLayout(
  sidebarPanel(tagList(
    br(),
    
    ## text input
    selectizeInput(ns('genes'), 
                   label=paste('Enter your genes of interest (max. ',
                               GENEMAX,')', sep=''),
                   choices = NULL,
                   multiple=T),
    
    br(),
    
    ## inputs to customize heatmap
    fluidRow(
      column(4, radioButtons(ns('zscore'), 
                             label='Z-score', 
                             choices=c('row', 'none'), 
                             selected='row')),
      
      column(4, 
             radioButtons(ns('PTMsites'), 
                          label='PTM sites', 
                          choices=c('most variable', 'all'), 
                          selected='most variable')),
      column(4,
             radioButtons(ns('show.sample.label'),
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
    
    ## inputs for order
    fluidRow(column(12, strong("Data order (drag and drop to re-order)"), br(), orderInput(
      label = "",
      items = list(),
      inputId = ns('ome.order'),
      class = "btn-group-vertical",
      width = '150px'
    ))),
    
    br(),
    
    ## download buttons
    fluidRow(column(6, downloadButton(ns('downloadHM'), 'Download PDF')),
             column(6, downloadButton(ns('downloadTab'), 'Download Excel'))),
    
    br(),
    
    ## Instructions
    p(strong("Getting started")),
    p(paste("Enter your gene names of interest (official gene symbols, e.g. EGFR) into the text field.",
            "You can enter up to", GENEMAX, "genes.")),
    br()
  )), # end tagList
  
  mainPanel(plotOutput(ns("HM"))))
}

heatmapTabServer <- function(id = 'heatmapTab') {
  moduleServer(
    id,
    ## module function
    function (input, output, session) {
      
    o <- preprocess_gct_files(GCTs)
    merged_mat <- o$merged_mat
    merged_rdesc <- o$merged_rdesc
    merged_cdesc <- o$merged_cdesc
    rm(o)
    
    # function to check if selected annotation columns are valid
    is_valid_anno <- function(col) {
      valid_categorical <- length(setdiff(unique(col), NA)) <= MAX_ANNO_LEVELS
      valid_numeric <- all(suppressWarnings(!is.na(as.numeric(as.character(setdiff(col, NA))))))
      return(valid_categorical | valid_numeric)
    }
    sample_anno <- merged_cdesc %>% select(Sample.ID | where(is_valid_anno))
      
    # update selectizeInput for genes
    updateSelectizeInput(session,
                         inputId = "genes",
                         choices = sort(unique(merged_rdesc$geneSymbol)),
                         server = T)
    
    # update selectTinput for annotations
    updateSelectInput(session,
                      inputId = "sort.after",
                      choices = setdiff(names(sample_anno), 'Sample.ID'))
    
    # update data ordering options
    updateOrderInput(session,
                     inputId = 'ome.order',
                     items = sort(unique(merged_rdesc$DataType)))
    
    ## get heatmap parameters
    params <- reactive({list(genes.char = input$genes,
                              zscore = input$zscore,
                              PTMsites = input$PTMsites,
                              min.val = as.numeric(input$min.val),
                              max.val = as.numeric(input$max.val),
                              sort.after = input$sort.after,
                              show.sample.label = input$show.sample.label,
                              custom_anno_colors = get0('custom_anno_colors'),
                              ome.order = input$ome.order)})
    
    out <- reactive({
      validate(
        need(params()$genes.char, "Input genes to see results"),
        need(params()$min.val < params()$max.val, "Input valid min and max")
      )
      
      myComplexHeatmap(merged_mat,
                       merged_rdesc,
                       sample_anno,
                       params = params())
    })
      
      
    ## download HM pdf
    output$downloadHM <- downloadHandler(
      filename = paste0(FILENAMESTRING, '_',
                        gsub(' |\\:','-', Sys.time()),
                        '.pdf'),
      content = function(file) {
        pdf(file = file,
            width = 1400/72,
            height = (dynamicHeightHM(nrow(out()$Table))+48)/72)
        draw(out()$HM, 
             annotation_legend_side='bottom')
        dev.off()
      }
    )
    
    ## download excel
    output$downloadTab <- downloadHandler(
      filename = paste0(FILENAMESTRING, '_',
                        gsub(' |\\:','-', Sys.time()),
                        '.xlsx'),
      content = function(file) {
        tab = out()$Table
        WriteXLS('tab',
                 ExcelFileName=file,
                 SheetNames=FILENAMESTRING,
                 FreezeRow = ncol(sample_anno),
                 FreezeCol=5,
                 row.names=T)
      }
    )
      
      
    ## render the heatmap
    
    plotHeight <- reactive({tryCatch({dynamicHeightHM(nrow(out()$Table))},
                                     error = function (cond) 'auto')})
    
    output$HM <- renderPlot({
      validate(
          need(params, "Complete setup steps to see heatmap.") %then%
          need(params()$genes.char, "Input genes to see results") %then%
          need(params()$min.val < params()$max.val, "Input valid min and max") %then%
          # need(HM$params()$sort.after %in% names(sample_anno) || dim(sample_anno)[2] == 1,
          #      "Invalid annotation selection") %then%
          need(out, "Heatmap not yet generated."))
      
      
      draw(out()$HM, annotation_legend_side='bottom')
    }, height = plotHeight)
  }) # end moduleServer
}
