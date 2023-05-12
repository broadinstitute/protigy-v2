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
    
    # TODO: source your helper functions locally
    source('helperCode.R', local=T)
    
    # TODO: add server logic here
    
  })
}


################################################################################
# Helper functions
################################################################################

# add your helper functions here
