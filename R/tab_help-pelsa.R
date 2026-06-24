################################################################################
# Module: HELP - PELSA
################################################################################


# UI for the PELSA help tab
helpPELSATabUI <- function(id = "helpTab") {
  ns <- NS(id) # namespace function, wrap UI inputId's with this `ns("inputId")`

  tagList(
    add_css_attributes(
      shinydashboardPlus::box(
        includeMarkdown(
          system.file("help_documentation/protigy_pelsa_help.md",
                      package = "Protigy")
        ),
        width = 12,
        headerBorder = FALSE
      ),
      classes = "box-no-header")
  ) # end tagList
}
