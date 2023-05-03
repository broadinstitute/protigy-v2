################################################################################
# Module: TEMPLATE
# This code contains the UI and Server modules
################################################################################

## Steps for adding a new module
# 1. add core UI and server logic here
# 2. add helper functions or additional scripts in corresponding folder
# 3. add necessary libraries in global.R
# 4. call your UI function in ui.R
# 5. call your server function in server.R


# UI for the summary tab
templateTabUI <- function(id = "templateTab") {
  ns <- NS(id) # namespace function, wrap UI inputId's with this `ns("inputId")`
  
  tagList(
    
    # TODO: add your UI elements
    
  ) # end tagList
}

# server for the summary tab
templateTabServer <- function(id = "templateTab") { moduleServer( 
  id,
  
  ## module function
  function (input, output, session) {
    
    # get namespace in case you need to use it in renderUI-like functions
    ns <- session$ns
    
    # TODO: add server logic here
    
  })
}