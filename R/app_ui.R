################################################################################
# UI
# This function contains the entire app's UI setup. It mainly consists of the 
# dashboardPage and navbar. All module-specific UI content should be called 
# using UI module functions.
################################################################################

app_UI <- function(request) {dashboardPage(
  dashboardHeader(title = 'New Protigy?'),
  
  dashboardSidebar(
    setupSidebarUI()
  ),
  
  dashboardBody(
    # custom CSS for the navbar title and settings icon
    # tags$head(tags$style(
    #   type = 'text/css',
    #   HTML('.navbar-brand {display:none;}'),
    #   HTML('.main-header .sidebar-toggle:before {content: "\\2699";}'))
    # ),
    
    # include custom CSS
    includeCSS(system.file("custom.css", package = "protigyRevamp")),
  
    navbarPage(
      title = '',
      tabPanel("Summary", summaryTabUI()),
      navbarMenu(
        "Clustering",
        tabPanel("Static Heatmap"),
        tabPanel("Fan Plot")),
      navbarMenu(
        "Volcanos",
        tabPanel("1"),
        tabPanel("2")),
      navbarMenu(
        "Scatterplots",
        tabPanel("1"),
        tabPanel("2")),
      navbarMenu(
        "PCA",
        tabPanel("1"),
        tabPanel("2")),
      tabPanel("Table"),
      navbarMenu(
        "QC",
        tabPanel("1"),
        tabPanel("2")),
      navbarMenu(
        "Multi-ome",
        tabPanel('Heatmap'),
        tabPanel('Pair-wise correlation'),
        tabPanel('More ideas?')),
      tabPanel("Export", exportTabUI())
      ) #end navbarPage
  ) # end dashboardBody
) # end dashboardPage
}

