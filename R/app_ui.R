################################################################################
# UI
#
# This function contains the entire app's UI setup. It mainly consists of the 
# dashboardPage and navbar. All module-specific UI content should be called 
# using UI module functions.
################################################################################

app_UI <- function(request) {dashboardPage(
  dashboardHeader(title = 'Protigy v2.0'),
  
  shinydashboard::dashboardSidebar(
    setupSidebarUI()
  ),
  
  dashboardBody(
    # include custom CSS
    includeCSS(system.file("custom.css", package = "Protigy")),
  
    navbarPage(
      title = '',
      id = "navbar-tabs",
      navbarMenu(
        "Help",
        tabPanel("General", helpGeneralTabUI(), value = "Help-General"),
        tabPanel("Analysis", helpAnalysisTabUI(), value = "Help-Analysis"),
        icon = icon("question")
      ),
      # tabPanel("Customize", 
      #          customizeTabUI(),
      #          icon = icon("wand-magic-sparkles")),
      tabPanel("Summary", summaryTabUI()),
      navbarMenu(
        "QC",
        tabPanel("Boxplots",QCBoxplots_Tab_UI()),
        tabPanel("Profile plots", QCProfilePlots_Tab_UI()),
        tabPanel("Correlation", QCCorrelation_Tab_UI()),
        tabPanel("PCA")),
      #tabPanel("Statistics"),
      # navbarMenu(
      #   "Clustering",
      #   tabPanel("Static Heatmap"),
      #   tabPanel("Fan Plot")),
      # navbarMenu(
      #   "Volcanos",
      #   tabPanel("1"),
      #   tabPanel("2")),
      # navbarMenu(
      #   "Scatterplots",
      #   tabPanel("1"),
      #   tabPanel("2")),
      # tabPanel("Table"),
      navbarMenu(
        "Multi-ome",
        tabPanel('Heatmap', multiomeHeatmapTabUI())
        #tabPanel('Pair-wise correlation'),
        #tabPanel('More ideas?')
        ),
      tabPanel("Export", exportTabUI(), icon = icon("download")),
      
      #tabPanel("TEMPLATE", templateSingleOme_Tab_UI())
      
      ) #end navbarPage
  ) # end dashboardBody
) # end dashboardPage
}

