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
    
    ## inputs for clustering
    fluidRow(
      column(12, checkboxInput(ns('cluster_columns'),
                              label = "Cluster columns",
                              value = TRUE))
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
    
    ## inputs for feature filtering
    fluidRow(
      column(12, numericInput(ns('max_features_per_gene'), 
                             label='Max features per gene per dataset (by std dev)', 
                             value=5, 
                             min=1, 
                             max=50,
                             step=1,
                             width='100%'))
    ), #end fluidRow
    
    ## inputs for sorting
    fluidRow(
      column(12, selectizeInput(ns('sort.after'), 'Sort by', 
                                choices = NULL,
                                multiple=FALSE))  
    ), #end fluidRow
    
    
    ## inputs for dataset order
    fluidRow(
      column(12,
        strong("Dataset order"),
        br(),
        p("Drag to reorder"),
        orderInput(
          label = "",
          items = list(),
          inputId = ns('ome.order'),
          class = "btn-group-vertical",
          width = '100%'
        )
      )
    )
    
  ) # end tagList
}

## server for heatmap options
options_multiomeHeatmapTabServer <- function(id, merged_rdesc, sample_anno, setup_submit, globals, selected_annotations = NULL, saved_settings = NULL) {
  moduleServer(
    id,
    ## module function
    function (input, output, session) {
      
      # update selectizeInput for genes
      observeEvent(setup_submit(), {
        # Get saved genes or use empty selection
        saved_genes <- if (!is.null(saved_settings) && !is.null(saved_settings())) {
          saved_settings()$genes.char
        } else {
          NULL
        }
        
        updateSelectizeInput(
          session,
          inputId = "genes",
          choices = sort(unique(merged_rdesc()$geneSymbol)),
          selected = saved_genes,
          server = T)
      })
      
      
      # update selectInput for annotations
      observeEvent(setup_submit(), {
        # Get default annotation for default ome
        default_annotation <- globals$default_annotations[[globals$default_ome]]
        
        # Use selected annotations if available, otherwise all annotations
        all_annotations <- if (!is.null(selected_annotations) && !is.null(selected_annotations())) {
          selected_annotations()
        } else {
          setdiff(names(sample_anno()), 'Sample.ID')
        }
        
        # Filter to only discrete categories for grouping/sorting
        # Filter annotations to only discrete ones for grouping
        available_choices <- all_annotations[sapply(sample_anno()[all_annotations], is.discrete)]
        
        # Get saved sort selection or use default
        saved_sort <- if (!is.null(saved_settings) && !is.null(saved_settings())) {
          saved_settings()$sort.after
        } else {
          default_annotation
        }
        
        updateSelectInput(
          session,
          inputId = "sort.after",
          choices = available_choices,
          selected = saved_sort)
      })
      
      
      # update dataset ordering options
      observeEvent(setup_submit(), {
        # Use protigy.ome column if available, otherwise fall back to DataType
        ome_col <- if ("protigy.ome" %in% names(merged_rdesc())) {
          merged_rdesc()$protigy.ome
        } else if ("DataType" %in% names(merged_rdesc())) {
          merged_rdesc()$DataType
        } else {
          "Unknown"
        }
        
        # Get unique datasets
        available_datasets <- sort(unique(ome_col))
        
        # Get saved order or use default order
        saved_order <- if (!is.null(saved_settings) && !is.null(saved_settings())) {
          saved_settings()$ome.order
        } else {
          available_datasets
        }
        
        # Ensure saved order only includes available datasets
        final_order <- saved_order[saved_order %in% available_datasets]
        
        # Add any new datasets to the end
        new_datasets <- setdiff(available_datasets, final_order)
        final_order <- c(final_order, new_datasets)
        
        updateOrderInput(
          session,
          inputId = 'ome.order',
          items = final_order
        )
      })
      
      # Restore other settings when options screen loads
      observeEvent(setup_submit(), {
        if (!is.null(saved_settings) && !is.null(saved_settings())) {
          settings <- saved_settings()
          
          # Restore zscore setting
          if (!is.null(settings$zscore)) {
            updateRadioButtons(
              session,
              inputId = "zscore",
              selected = settings$zscore
            )
          }
          
          # Restore min/max values
          if (!is.null(settings$min.val)) {
            updateTextInput(
              session,
              inputId = "min.val",
              value = as.character(settings$min.val)
            )
          }
          
          if (!is.null(settings$max.val)) {
            updateTextInput(
              session,
              inputId = "max.val",
              value = as.character(settings$max.val)
            )
          }
          
          # Restore sample label setting
          if (!is.null(settings$show.sample.label)) {
            updateRadioButtons(
              session,
              inputId = "show.sample.label",
              selected = if (settings$show.sample.label) "show" else "hide"
            )
          }
          
          # Restore column clustering setting
          if (!is.null(settings$cluster_columns)) {
            updateCheckboxInput(
              session,
              inputId = "cluster_columns",
              value = settings$cluster_columns
            )
          }
          
          # Restore max features per gene setting
          if (!is.null(settings$max_features_per_gene)) {
            updateNumericInput(
              session,
              inputId = "max_features_per_gene",
              value = settings$max_features_per_gene
            )
          }
        }
      })
      
      
      ## get heatmap parameters
      HM.params <- reactive({list(genes.char = input$genes,
                                  zscore = input$zscore,
                                  min.val = as.numeric(input$min.val),
                                  max.val = as.numeric(input$max.val),
                                  sort.after = input$sort.after,
                                  show.sample.label = input$show.sample.label,
                                  ome.order = input$ome.order,
                                  max_features_per_gene = input$max_features_per_gene,
                                  cluster_columns = input$cluster_columns)})
      
      
      
      return(HM.params)
    }) # end moduleServer
}