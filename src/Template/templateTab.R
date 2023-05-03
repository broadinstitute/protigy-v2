################################################################################
# Module: TEMPLATE
# This code contains the UI and Server modules
################################################################################


# UI for the summary tab
templateTabUI <- function(id = "templateTab") {
  ns <- NS(id) # namespace function, wrap UI inputId's with this `ns("inputId")`
  
  tagList(
    
    # add your UI elements
    
  ) # end tagList
}

# server for the summary tab
templateTabServer <- function(id = "templateTab") { moduleServer( 
  id,
  
  ## module function
  function (input, output, session) {
    
    # get namespace in case you need to use it in renderUI-like functions
    ns <- session$ns
    
    # add server logic here
    
  })
}