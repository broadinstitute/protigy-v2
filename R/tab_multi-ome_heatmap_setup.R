

## UI for setting up gct file upload
setup_multiomeHeatmapTabUI <- function(id) {
  ns <- NS(id) # namespace function
  
  tagList(
    uiOutput(ns("setup_ui")),
  )
}

## server for setting up gct file uploads
setupmultiomeHeatmapTabServer <- function (id, GCTs) {
  moduleServer(
    id,
    ## module function
    function (input, output, session) {
      
      # get namespace
      ns <- session$ns
      
      all_omes <- reactive(names(GCTs()))
      
      ## setup options
      output$setup_ui <- renderUI({
        req(all_omes(), GCTs())
        
        all_GCTs <- GCTs()
        
        tagList(
          lapply(all_omes(), function(ome) {renderUI({
            
            # taglist of UI elements for each file's setup
            tagList(
              h4("Setup for ", strong(ome)),
              
              tipify(
                add_css_attributes(
                  selectInput(ns(paste0("setup_data_type_", ome)), 
                              label = "Data processing type:",
                              choices = c("SpectrumMill", "Other"),
                              selected = "SpectrumMill"),
                  classes = "small-input"),
                title = paste('"SpectrumMill" input assumes gene symbol column name is "geneSymbol"', 
                              'and that the GCT row IDs are formatted as "AccessionNumber_VMSiteInfo" or "EnsembleID_VMSiteInfo",',
                              'If this is not true, select "Other" to provide custom inputs.'),
                placement = "left"
              ),
              
              conditionalPanel(
                condition = paste0("input['setup_data_type_", ome, "'] == 'Other'"),
                ns = ns,
                
                add_css_attributes(
                  selectInput(ns(paste0("setup_geneSymbol_column_", ome)), 
                              label = "Gene symbol column name:",
                              choices = names(all_GCTs[[ome]]@rdesc),
                              selected = NULL), 
                    classes = "small-input"),
                
                checkboxInput(ns(paste0("setup_is_VM_", ome)),
                              "Ome contains variable modification data"),
                
                conditionalPanel(
                  condition = paste0("input['setup_is_VM_", ome, "']"),
                  ns = ns,
                  tipify(
                    add_css_attributes(
                      selectInput(ns(paste0("setup_VM_column_", ome)),
                                  label = "Variable modification site column name:",
                                  choices = names(all_GCTs[[ome]]@rdesc),
                                  selected = NULL), 
                      classes = "small-input"),
                    title = paste("Entries in this column should contain variable modification site/location.",
                                  "They will be used as labels in the heatmap."),
                    placement = "left"))
              ), # end conditionalPanel
              
              if (ome != tail(all_omes(), 1)) hr()
            ) # end tagList
          }) # end renderUI
          }), # end lapply
          
          # action button to submit parameters
          actionButton(ns("submit"), "Submit", class = "btn btn-primary")
          
        ) # end tagList
      }) # end renderUI
      
      
      ## Preprocessing GCTs ##
      
      gcts_merged_reactive <- reactiveVal()
      
      # reset GCTs_merged to null each time the GCTs input changed
      observeEvent(GCTs(), gcts_merged_reactive(NULL))
      
      # run preprocessing on submit
      observeEvent(input$submit, {
        merged <- withProgress(
          message = "Preprocessing GCT files for multi-ome heatmap", 
          expr = {
          
            # gather inputs
            setup_inputs <- list()
            setup_inputs$labels <- c()
            setup_inputs$datatypes <- c()
            setup_inputs$geneSymbol_columns <- c()
            setup_inputs$is_VMs <- c()
            setup_inputs$VM_columns <- c()
            for (ome in all_omes()){
              this_label <- ome
              this_datatype <- input[[paste0('setup_data_type_', ome)]]
              
              if (this_datatype == 'Other') {
                this_geneSymbol <- input[[paste0('setup_geneSymbol_column_',ome)]]
                this_is_VM <- input[[paste0('setup_is_VM_',ome)]]
                this_VM_column <- input[[paste0('setup_VM_column_',ome)]]
              } else {
                this_geneSymbol <- NA
                this_is_VM <- NA
                this_VM_column <- NA
              }
              
              setup_inputs$labels <- c(setup_inputs$labels, this_label)
              setup_inputs$datatypes <- c(setup_inputs$datatypes, this_datatype)
              setup_inputs$geneSymbol_columns <- c(setup_inputs$geneSymbol_columns, this_geneSymbol)
              setup_inputs$is_VMs <- c(setup_inputs$is_VMs, this_is_VM)
              setup_inputs$VM_columns <- c(setup_inputs$VM_columns, this_VM_column)
            }
            
            # preprocess files, return output
            my_shinyalert_tryCatch({
              preprocess_gcts_multiome_heatmap(
                GCTs = GCTs(),
                setup_inputs = setup_inputs
              )
            }, return.error = NULL)
          }
        )
        
        # update the reactive val
        gcts_merged_reactive(merged)
      })
      
      return(list(
        gcts_merged = gcts_merged_reactive,
        submit = reactive(input$submit)))
      
    } # end module function
  ) # end moduleServer
}