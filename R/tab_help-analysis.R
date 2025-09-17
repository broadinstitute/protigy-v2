################################################################################
# Module: HELP
################################################################################


# UI for the help tab
helpAnalysisTabUI <- function(id = "helpTab") {
  ns <- NS(id) # namespace function, wrap UI inputId's with this `ns("inputId")`
  
  tagList(
    add_css_attributes(
      shinydashboardPlus::box(
        tabsetPanel(
          id = ns("analysisHelpTabs"),
          type = "tabs",
          tabPanel(
            "CSV/TSV/Excel Processing",
            includeMarkdown(
              system.file("help_documentation/protigy_csv_excel_processing_help.md", 
                          package = "Protigy")
            )
          ),
          tabPanel(
            "Dataset Setup",
            includeMarkdown(
              system.file("help_documentation/protigy_normalization_help.md", 
                          package = "Protigy")
            )
          ),
          tabPanel(
            "Statistics",
            includeMarkdown(
              system.file("help_documentation/protigy_statistics_help.md", 
                          package = "Protigy")
            )
          ),
          tabPanel(
            "Multi-ome",
            includeMarkdown(
              system.file("help_documentation/protigy_multiome_help.md", 
                          package = "Protigy")
            )
          )
        ),
        width = 12,
        headerBorder = FALSE
      ),
      classes = "box-no-header")
  ) # end tagList
}