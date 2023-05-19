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
    
    # directory name where all plots will be saved
    dir_name <- "Plots"
    zip_dir <- tempfile("plots")
    plots_dir <- file.path(zip_dir, dir_name)
    dir.create(plots_dir, recursive = T)
    
    output$downloadPlots <- downloadHandler(
      filename = "my_plots.zip",
      content = function(file) {
        cat("Downloading plots\n")
        
        plots <- all_plots()
        
        lapply(seq_along(plots), function(i) {
          p = plots[[i]]
          p_name = paste0(names(plots)[[i]], '.pdf')
          
          ggsave(
            filename = p_name, 
            plot = p, 
            device = 'pdf',
            path = plots_dir
          )
        })
        
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
