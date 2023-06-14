

## UI for setting up gct file upload
setup_multiomeHeatmapTabUI <- function(id) {
  ns <- NS(id) # namespace function
  
  tagList(
    ## setup options
    uiOutput(ns("setup_ui")),
    
    ## submit button + message
    uiOutput(ns("submit_button"))
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
      observeEvent(GCTs(), {
        
        # create options for each inputted file
        output$setup_ui <- renderUI({
          tagList(
            lapply(all_omes(), function(ome) {renderUI({
              
              # taglist of UI elements for each file's setup
              tagList(
                h5("Setup for ", ome),
                
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
                                choices = names(GCTs()[[ome]]@cdesc),
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
                                    choices = names(GCTs()[[ome]]@cdesc),
                                    selected = NULL), 
                        class="not_bold"),
                      title = paste("Entries in this column should contain variable modification site/location.",
                                    "They will be used as labels in the heatmap."),
                      placement = "left"))
                ), # end conditionalPanel
                
                if (ome == tail(all_omes(), 1)) br() else hr()
                
              ) # end tagList
            }) # end renderUI
            }) # end lapply
          ) # end tagList
        }) # end renderUI
      }) # end observeEvent
      
      
      
      
      
      
      
      
      
      
      
      
      # initialize reactive values list for preprocessing outputs
      preprocessing <- reactiveValues()
      preprocessing$num <- 0 # need a number to increment each time preprocessing happens
      preprocessing$valid <- FALSE # whether or not preprocessing successfully completed
      
      ## setup options
      observeEvent(input$gctFiles, {
        
        # create options for each inputted file
        output$setup_ui <- renderUI({
          tagList(
            lapply(1:dim(input$gctFiles)[1], function(i) {renderUI({
              
              # get file name
              file_name <- basename(input$gctFiles$name[i])
              
              # only allow for .gct files
              # if (file_ext(file_name) != 'gct') {
              #   error_msg <- tagList(p(span(
              #     strong("Error: "),
              #     paste0("'", file_name, "' is invalid. Only .gct inputs are allowed.\n\n"),
              #     style = "color:red"
              #   )))
              #   return (error_msg)
              # }
              
              ## taglist of UI elements for each file's setup
              tagList(
                strong("Setup for ", basename(input$gctFiles$name[i])),
                
                # create class for UI elements that won't have bolded label
                tags$head(tags$style(HTML(".not_bold label {font-weight:normal;}"))),
                
                # label for this file
                tipify(
                  div(textInput(ns(paste0("setup_label_",i)),
                                'Label (e.g. "Proteome" or "Prot"):'), 
                      class="not_bold"),
                  title = "This label will appear in the heatmap to differentiate between omes.",
                  placement = "right"
                ),
                
                tipify(
                  div(radioButtons(ns(paste0("setup_data_type_",i)), 
                                   label = "Data processing type:",
                                   choices = c("SpectrumMill", "Other"),
                                   selected = "SpectrumMill"),
                      class = "not_bold"),
                  
                  title = paste('"SpectrumMill" input assumes gene symbol column name is "geneSymbol"', 
                                'and that the GCT row IDs are formatted as "AccessionNumber_VMSiteInfo" or "EnsembleID_VMSiteInfo",',
                                'If this is not true, select "Other" to provide custom inputs.'),
                  placement = "right"
                ),
                
                conditionalPanel(
                  condition = paste0("input.setup_data_type_", i, " == 'Other'"),
                  ns = ns,
                  
                  div(textInput(ns(paste0("setup_geneSymbol_column_", i)), 
                                label = "Gene symbol column name:",
                                value = "geneSymbol"), 
                      class="not_bold"),
                  
                  checkboxInput(ns(paste0("setup_is_VM_", i)),
                                "File contains variable modification data"),
                  
                  conditionalPanel(
                    condition = paste0("input.setup_is_VM_", i),
                    ns = ns,
                    tipify(div(textInput(ns(paste0("setup_VM_column_", i)), 
                                         label = "Variable modification site column name:"), 
                               class="not_bold"),
                           title = paste("Entries in this column should contain variable modification site/location.",
                                         "They will be used as labels in the heatmap."),
                           placement = "right"))
                ), # end conditionalPanel
                
                br()
                
              ) # end tagList
            }) # end renderUI
            }) # end lapply
          ) # end tagList
        }) # end renderUI
      }) # end observeEvent
      
      # toggle submit button only when all inputs are valid
      observe({
        # check for valid inputs
        input_names <- names(input)
        
        # get available names for all inputs
        setup_label_names <- sort(input_names[grep("setup_label_\\d", input_names)])
        setup_datatype_names <- sort(input_names[grep("setup_data_type_\\d", input_names)])
        setup_geneSymbol_names <- sort(input_names[grep("setup_geneSymbol_column_\\d", input_names)])
        setup_is_VM_names <- sort(input_names[grep("setup_is_VM_\\d", input_names)])
        setup_VM_column_names <- sort(input_names[grep("setup_VM_column_\\d", input_names)])
        
        # check inputs that are required for all files
        setup_strings_valid <- c()
        setup_strings_valid <- c(setup_strings_valid,
                                 sapply(setup_label_names, function(n) input[[n]] != ''))
        setup_strings_valid <- c(setup_strings_valid,
                                 sapply(setup_datatype_names, function(n) !is.null(input[[n]])))
        
        # check inputs that are required only for "other" data type files
        setup_strings_valid <- c(setup_strings_valid,
                                 unlist(sapply(seq_along(setup_datatype_names), function(i) {
                                   # first, check if this file is an "other" datatype and requires extra inputs
                                   # have to use `%in%` because otherwise you get
                                   # "Error in if: argument is of length zero"
                                   # if that input isn't initialized yet
                                   if ("Other" %in% input[[setup_datatype_names[i]]]) {
                                     this_input_names <- c(setup_geneSymbol_names[i],
                                                           setup_is_VM_names[i],
                                                           setup_VM_column_names[i])
                                     
                                     # return list of whether required inputs are valid
                                     return(c(input[[this_input_names[1]]] != '',
                                              if (TRUE %in% input[[this_input_names[2]]]) {
                                                input[[this_input_names[3]]] != ''
                                              }))
                                   }
                                 })))
        
        # single variable accounting for all valid conditions
        all_valid <- all(
          !is.null(input$gctFiles),
         # file_ext(input$gctFiles$name) == 'gct',
          setup_strings_valid
        )
        
        # activate submit button if all valid
        if (all_valid) {
          output$submit_button <- renderUI({
            tagList(
              actionButton(ns("submit"), "Submit", 
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            )})
          # otherwise inactivate submit button
        } else {
          output$submit_button <- renderUI({
            tagList(
              actionButton(ns("submit_INVALID"), "Submit",
                           style="color: #fff; background-color: #e9eae9; border-color: #c4c4c4")
            )})
        }
      }) # end observe
      
      # run preprocessing on valid submit
      observeEvent(input$submit, {
        withProgress(message = "Preprocessing GCT files", expr = {
          # get appropriate inputs for gct processing
          gct_files <- isolate({input$gctFiles$datapath})
          
          setup_inputs <- list()
          setup_inputs$labels <- c()
          setup_inputs$datatypes <- c()
          setup_inputs$geneSymbol_columns <- c()
          setup_inputs$is_VMs <- c()
          setup_inputs$VM_columns <- c()
          for (i in 1:length(gct_files)){
            this_label <- input[[paste0('setup_label_',i)]]
            this_datatype <- input[[paste0('setup_data_type_',i)]]
            
            if (this_datatype == 'Other') {
              this_geneSymbol <- input[[paste0('setup_geneSymbol_column_',i)]]
              this_is_VM <- input[[paste0('setup_is_VM_',i)]]
              this_VM_column <- input[[paste0('setup_VM_column_',i)]]
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
          
          tryCatch({ 
            # reset global gct data to null
            if (user_can_upload) {
              merged_mat <<- NULL
              merged_rdesc <<- NULL
              merged_cdesc <<- NULL
            }
            
            # preprocess files
            gct_processed <- preprocess_gct_files(gct_files, setup_inputs)
            
            # save global variables
            merged_mat <<- gct_processed$merged_mat
            merged_rdesc <<- gct_processed$merged_rdesc
            merged_cdesc <<- gct_processed$merged_cdesc
            rm(gct_processed) # don't want to have this saved in memory
            
            cat('Preprocessing complete!\n\n')
            
            # set outputs
            preprocessing$submit_message <- p(span(strong("Success!"), 
                                                   style = "color:green"),
                                              span("Continue to Annotations tab.",
                                                   style = "color:black"))
            
            preprocessing$valid <- TRUE
            preprocessing$status <- 'success'
          },
          error = function(cond) {
            message('Something went wrong with preprocessing :(')
            message("Here is the original error:")
            print(cond)
            
            # set outputs
            preprocessing$submit_message <- p(span(strong("Error:"),
                                                   style = "color:red"),
                                              span(paste(cond),
                                                   style = "color:black"))
            preprocessing$valid <- FALSE
            preprocessing$status <- 'error'
          },
          warning = function(cond) {
            message('Warning occured during preprocessing')
            message("Here is the original warning message:")
            print(cond)
            
            # set outputs
            preprocessing$submit_message <- p(span(strong("Warning:"),
                                                   style = "color: #fcc494"),
                                              span(paste(cond),
                                                   style = "color:black"))
            preprocessing$valid <- TRUE
            preprocessing$status <- 'warning'
          }, finally = {
            # increment preprocessing number so we know something happened
            preprocessing$num <- preprocessing$num + 1
            
            # show that preprocessing has been completed
            setProgress(value = 1)
          }) # end tryCatch
        }) # end withProgress
      }) # end observeEvent
      
      # generate shinyalert
      preprocessing$tabSelectionChanged <- 0 # initialize
      confirmButtonText <- list('success' = 'Continue', 'warning' = 'Continue anyways')
      observeEvent(preprocessing$num, ignoreInit = T,
                   handlerExpr = {
                     shinyalert(
                       text = tagList(preprocessing$submit_message),
                       html = TRUE,
                       type = preprocessing$status,
                       showCancelButton = T,
                       showConfirmButton = preprocessing$status %in% c('warning', 'success'),
                       confirmButtonText = confirmButtonText[[preprocessing$status]],
                       cancelButtonText = 'Return to setup',
                       confirmButtonCol = '#337ab7',
                       callbackR = function(v) {
                         if (v) {
                           preprocessing$tabSelectionChanged <- preprocessing$tabSelectionChanged + 1
                         }
                       })
                   })
      
      # return preprocessing outputs
      return(preprocessing)
      
    } # end module function
  ) # end moduleServer
}