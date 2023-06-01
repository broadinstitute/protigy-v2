################################################################################
# Module: HELP
################################################################################


# UI for the help tab
helpAnalysisTabUI <- function(id = "helpTab") {
  ns <- NS(id) # namespace function, wrap UI inputId's with this `ns("inputId")`
  
  tagList(
    
    includeMarkdown(system.file("help_documentation/protigy_analysis_help.md", package = "protigyRevamp"))
    
  ) # end tagList
}