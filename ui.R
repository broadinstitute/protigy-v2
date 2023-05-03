## Define UI
shinyUI(dashboardPage(
  
  dashboardHeader(title = 'New Protigy?'),
  
  dashboardSidebar(
    setupSidebarUI()
  ),
  
  dashboardBody(
    
    tagList(
      tags$head(tags$style(type = 'text/css','.navbar-brand{display:none;}')),
    
      navbarPage(
        title = '', collapsible = TRUE,
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
          tabPanel('Heatmap', heatmapTabUI()),
          tabPanel('Pair-wise correlation'),
          tabPanel('More ideas?')),
        tabPanel("Export")
        ) #end navbarPage
    ) # end tagList
    
    
  )
))