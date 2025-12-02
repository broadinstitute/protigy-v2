################################################################################
# UI
#
# This function contains the entire app's UI setup. It mainly consists of the 
# dashboardPage and navbar. All module-specific UI content should be called 
# using UI module functions.
################################################################################

app_UI <- function(request) {dashboardPage(
  dashboardHeader(
    title = paste0('ProTIGY v', packageVersion('Protigy')),
    tags$li(class = "dropdown",
            actionButton(
              inputId = "clear_all_notifications_header",
              label = "Clear All Notifications",
              icon = icon("bell-slash"),
              class = "btn-sm"
            )
    )
  ),

  shinydashboard::dashboardSidebar(
    setupSidebarUI()
  ),

  dashboardBody(
    # include custom CSS
    includeCSS(system.file("custom.css", package = "Protigy")),
    # include shinyjs
    shinyjs::useShinyjs(),

    # JavaScript to manage Clear All Notifications button visibility
    tags$script(HTML("
      $(document).ready(function() {
        // Function to update button visibility based on notification presence
        function updateClearButton() {
          var notifications = $('.shiny-notification');
          if (notifications.length > 0) {
            $('#clear_all_notifications_header').show();
          } else {
            $('#clear_all_notifications_header').hide();
          }
        }

        // Use MutationObserver to watch for notification changes
        var observer = new MutationObserver(function(mutations) {
          updateClearButton();
        });

        // Observe the body for notification additions/removals
        observer.observe(document.body, {
          childList: true,
          subtree: true
        });

        // Initial check
        updateClearButton();
      });
    ")),

    navbarPage(
      title = '',
      id = "navbar-tabs",
      navbarMenu(
        "Help",
        tabPanel("General", helpGeneralTabUI(), value = "Help-General"),
        tabPanel("Customize", helpCustomizationTabUI(), value = "Help-Customize"),
        tabPanel("Analysis", helpAnalysisTabUI(), value = "Help-Analysis"),
        icon = icon("question")
      ),
      tabPanel("Customize",
               customizeTabUI(),
               icon = icon("wand-magic-sparkles")),
      tabPanel("Summary", summaryTabUI(), value = "Summary"),
      navbarMenu(
        "QC",
        tabPanel("Boxplots",QCBoxplots_Tab_UI(), value="QC-Boxplots"),
        tabPanel("Profile plots", QCProfilePlots_Tab_UI(),value="QC-Profile-Plots"),
        tabPanel("Correlation", QCCorrelation_Tab_UI(), value="QC-Correlation"),
        tabPanel("PCA", QCPCA_Tab_UI(), value="QC-PCA")),
     
      navbarMenu(
        "Statistics",
        tabPanel("Setup", statSetup_Tab_UI(), value = "Statistics-Setup"),
        tabPanel("Summary", statSummary_Tab_UI(), value = "Statistics-Summary"),
        tabPanel("Volcano Plot", statPlot_Tab_UI(), value = "Statistics-Volcano")
        # tabPanel("Table", statTable_Tab_UI())
      ),
      
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

