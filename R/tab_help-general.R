################################################################################
# Module: HELP
################################################################################


# UI for the help tab
helpGeneralTabUI <- function(id = "helpTab") {
  ns <- NS(id) # namespace function, wrap UI inputId's with this `ns("inputId")`
  
  tagList(
    
    includeMarkdown(system.file("help_documentation/protigy-README.md", package = "protigyRevamp"))
    
  ) # end tagList
}