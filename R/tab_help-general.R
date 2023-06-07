################################################################################
# Module: HELP
################################################################################


# UI for the help tab
helpGeneralTabUI <- function(id = "helpTab") {
  ns <- NS(id) # namespace function, wrap UI inputId's with this `ns("inputId")`
  
  tagList(
    add_css_attributes(
      shinydashboardPlus::box(
        includeMarkdown(
          system.file("help_documentation/protigy-README.md", 
                      package = "protigyRevamp")
        ),
        width = 12,
        headerBorder = FALSE
      ),
      classes = "box-no-header")
  ) # end tagList
}