################################################################################
# Module: TEMPLATE
################################################################################

## Steps for adding a new module
# 1. add core UI and server logic here
# 2. add helper functions in this same script
# 3. add necessary libraries in global.R
# 4. call your UI function in ui.R
# 5. call your server function in server.R

################################################################################
# Shiny funcions (UI and server)
################################################################################

# UI for the summary tab
exportTabUI <- function(id = "exportTab") {
  ns <- NS(id) # namespace function, wrap UI inputId's with this `ns("inputId")`
  
  tagList(
    
    checkboxGroupInput(ns("omesForExport"),"Omes for export:"),
    checkboxGroupInput(ns("plotsForExport"),"Plots for export:"),
    
    downloadButton(ns("downloadPlots"), label = "Download Plots")
    
  ) # end tagList
}

# server for the summary tab
exportTabServer <- function(id = "exportTab", all_plots) { moduleServer( 
  id,
  
  ## module function
  function (input, output, session) {
    
    # get namespace in case you need to use it in renderUI-like functions
    ns <- session$ns
    
    all_plot_types <- reactive({setdiff(names(all_plots()), "omes")})
    
    observe({
      # update omes for export
      updateCheckboxGroupInput(inputId = "omesForExport",
                               choices = all_plots$omes(),
                               selected = all_plots$omes())
    })
      
    observe({
      # update plots for export
      updateCheckboxGroupInput(inputId = "plotsForExport",
                               choices = names(all_plots$plots),
                               selected = names(all_plots$plots))
    })
    
    
    output$downloadPlots <- downloadHandler(
      filename = "my_plots.zip",
      content = function(file) {
        
        # show a notification that plots are downloading
        id <- showNotification(
          "Compiling plots...", 
          duration = NULL, 
          closeButton = FALSE
        )
        on.exit(removeNotification(id), add = TRUE)
        
        # directory name where all plots will be saved
        dir_name <- "Plots"
        zip_dir <- tempfile("plots")
        plots_dir <- file.path(zip_dir, dir_name)
        dir.create(plots_dir, recursive = T)
        
        # gather inputs
        plots <- all_plots$plots
        selected_omes <- input$omesForExport
        selected_plots <- input$plotsForExport
        
        # make a folder for each -ome
        lapply(selected_omes, function(ome) dir.create(file.path(plots_dir, ome)))
        
        # loop through selected plots
        lapply(selected_plots, function(tab_name) {
          plots_all_omes = plots[[tab_name]]()
          
          # loop through selected omes
          lapply(selected_omes, function(ome) {
            plots_this_ome <- plots_all_omes[[ome]]
            
            # save each plot for this ome
            for(i in seq_along(plots_this_ome)) {
              p = plots_this_ome[[i]]
              p_name = paste0(names(plots_this_ome)[[i]], '.pdf')
              
              # save ggplot
              if (is.ggplot(p)) {
                ggsave(
                  filename = p_name, 
                  plot = p, 
                  device = 'pdf',
                  path = file.path(plots_dir, ome)
                )
              } else {
                warning(paste("Invalid plot...skipping", p_name))
              }
              
            }
          })
        })
        
        # zip the outputs
        zip::zip(file, file.path(dir_name, list.files(plots_dir)), 
                 recurse = TRUE, root = zip_dir)
      }
    )
    
  })
}


################################################################################
# Helper functions
################################################################################

# add your helper functions here
