################################################################################
# Module: EXPORT
#
# Export desired files.
################################################################################

################################################################################
# Shiny funcions (UI and server)
################################################################################

# UI for the summary tab
exportTabUI <- function(id = "exportTab") {
  ns <- NS(id) # namespace function, wrap UI inputId's with this `ns("inputId")`
  
  tagList(
    
    # Display content or "GCTs not yet processed" message
    uiOutput(ns("export_content"))
    
  ) # end tagList
}

# server for the summary tab
exportTabServer <- function(id = "exportTab", all_exports, GCTs_and_params) { 
  
  ## module function
  moduleServer(id, function (input, output, session) {
    
    # get namespace in case you need to use it in renderUI-like functions
    ns <- session$ns
    
    # get parameters
    parameters <- reactive({
      validate(need(GCTs_and_params(), "GCTs not yet processed"))
      GCTs_and_params()$parameters
    })
    
    output$export_content <- renderUI({
      # This will trigger the validate() statements and show "GCTs not yet processed"
      req(GCTs_and_params())
      
      tagList(
        # Omes for export using pickerInput
        pickerInput(
          ns("omesForExport"),
          "Omes for export:",
          choices = all_exports$omes(),
          selected = all_exports$omes(),
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            selectAllText = "Select All",
            deselectAllText = "Deselect All",
            noneSelectedText = "No omes selected"
          )
        ),
        
        # Tabs for export using pickerInput
        pickerInput(
          ns("tabsForExport"),
          "Tabs for export:",
          choices = names(all_exports$exports),
          selected = names(all_exports$exports),
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            selectAllText = "Select All",
            deselectAllText = "Deselect All",
            noneSelectedText = "No tabs selected"
          )
        ),
        
        downloadButton(ns("download"), label = "Download", class = "btn btn-primary"),
        
        br(),
        br(),
        
        # Documentation section
        div(
          class = "well",
          h4("Export Documentation"),
          
          h5(strong("What Gets Exported:"), style = "font-size: 16px; margin-top: 20px; margin-bottom: 10px;"),
          tags$ul(
            tags$li("summary_exports: Original and processed GCT datasets, overview plots (PDF)"),
            tags$li("QCBoxplot_exports: Boxplots before and after normalization (PDF)"),
            tags$li("QCProfilePlots_exports: Profile plots before and after normalization (PDF)"),
            tags$li("QCCorrelation_exports: Correlation heatmaps and boxplots (PDF)"),
            tags$li("QCPCA_exports: PCA plots and regression plots (PDF)"),
            tags$li("multiomeHeatmap_exports: Multi-omics heatmaps (PDF)"),
            tags$li("statSummary_exports: P-value histograms (PDF), statistical summary tables (CSV), ssGSEA-ready GCT"),
            tags$li("statPlot_exports: Volcano plots (PDF)")
          ),
          
          h5(strong("Instructions:"), style = "font-size: 16px; margin-top: 20px; margin-bottom: 10px;"),
          tags$ol(
            tags$li("Select datasets and tabs to export"),
            tags$li("Click 'Download' to get a ZIP file with organized folders")
          )
        )
      )
    })
    
    # update omes for export
    observe({
      req(GCTs_and_params())
      updatePickerInput(
        session = session,
        inputId = "omesForExport",
        choices = all_exports$omes(),
        selected = all_exports$omes()
      )
    })
    
    # update tabs for export 
    observe({
      req(GCTs_and_params())
      updatePickerInput(
        session = session,
        inputId = "tabsForExport",
        choices = names(all_exports$exports),
        selected = names(all_exports$exports)
      )
    })
    
    
    output$download <- downloadHandler(
      filename = "protigy_exports.zip",
      content = function(file) {
        
        # directory name where all exports will be saved
        dir_name <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(file))
        zip_dir <- tempdir(check = T)
        exports_dir <- file.path(zip_dir, dir_name)
        dir.create(exports_dir, recursive = T)
        
        # gather inputs
        exports <- all_exports$exports
        selected_omes <- input$omesForExport
        selected_tabs <- input$tabsForExport
        
        # make a folder for each -ome
        lapply(selected_omes, function(ome) dir.create(file.path(exports_dir, ome)))
        
        # save parameters from each -ome
        lapply(setdiff(selected_omes, "multi_ome"), function(ome) {
          params <- parameters()[[ome]]
          yaml::write_yaml(
            params[setdiff(names(params), "gct_file_path")],
            file.path(exports_dir, ome, paste0(ome, "_parameters.yaml")))
        })
        
        success_exports <- c()
        error_exports <- c()
        
        # Calculate total number of exports for progress tracking
        total_exports <- 0
        for (tab_name in selected_tabs) {
          if (is.reactive(exports[[tab_name]])) {
            exports_all_omes <- exports[[tab_name]]()
          } else {
            exports_all_omes <- exports[[tab_name]]
          }
          for (ome in intersect(selected_omes, names(exports_all_omes))) {
            exports_this_ome <- exports_all_omes[[ome]]
            total_exports <- total_exports + length(exports_this_ome)
          }
        }
        
        # Use withProgress for progress tracking
        withProgress(message = "Compiling exports...", value = 0, {
          current_export <- 0
          
          # loop through selected tabs
          lapply(selected_tabs, function(tab_name) {
          
          if (is.reactive(exports[[tab_name]])) {
            exports_all_omes <- exports[[tab_name]]()
          } else {
            exports_all_omes <- exports[[tab_name]]
          }
          
          # loop through selected omes
          lapply(intersect(selected_omes, names(exports_all_omes)), function(ome) {
            exports_this_ome <- exports_all_omes[[ome]]
            
            # make a folder for exports in this tab
            exports_in_tab_path <- file.path(exports_dir, ome, tab_name)
            dir.create(exports_in_tab_path)
            
            # save each plot for this ome
            for(i in seq_along(exports_this_ome)) {
              
              p <- exports_this_ome[[i]]
              p_name <- names(exports_this_ome)[i]
              if (is.reactive(p)) {
                p <- p()
              }
              
              # Update progress
              current_export <<- current_export + 1
              progress_text <- paste0("Exporting ", tab_name, " - ", ome, " (", current_export, "/", total_exports, ")")
              incProgress(1/total_exports, detail = progress_text)
              
              tryCatch({
                # save the plot using the p() function
                p(exports_in_tab_path)
                
                # add to successful exports list
                success_exports <<- c(success_exports, file.path(ome, tab_name, p_name))
                
              }, error = function(c) {
                warning("Export failed for ", p_name, ": ", c$message)
                
                # add to errored exports list
                error_exports <<- c(error_exports, file.path(ome, tab_name, p_name))
              })
              
            }
          })
          })
          
          # Update progress for zipping
          incProgress(0.1, detail = "Creating zip file...")
          
        }) # End withProgress
        
        # zip the outputs
        zip::zip(file, file.path(dir_name, list.files(exports_dir)), 
                 recurse = TRUE, root = zip_dir)
        
        # shinyalert the exports that succeeded and errored
        shinyalert::shinyalert(
          html = TRUE,
          type = "info",
          text = HTML(paste0(
            "<div style='text-align: left'>",
            strong("Successfully saved:"), br(),
            "<ul><li>",
            paste(success_exports, collapse = "</li><li>"),
            "</li></ul>",
            strong("Could not save:"), br(),
            "<ul><li>",
            paste(error_exports, collapse = "</li><li>"),
            "</li></ul></div>"
          ))
        )
      }
    )
    
  })
}
