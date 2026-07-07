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

    # HTMLWIDGET LIBRARY PRE-LOAD (paired with addResourcePath in app_onStart;
    # do not remove). Load plotly.js and DataTables as NATIVE <script src> tags
    # at page startup so window.Plotly / $.fn.DataTable are defined before any
    # widget renders. Delivered this way, the webpack-UMD headers take the
    # browser branch and define their globals; delivered the htmlwidgets way (at
    # render time) Shiny injects them via jQuery globalEval, where the UMD header
    # diverts to the CommonJS branch and throws "require is not defined", leaving
    # every plot/table blank. The DataTables script is a jQuery plugin, so it is
    # listed AFTER useShinyjs() (Shiny's jQuery is already on the page here).
    # Filenames are resolved in app_onStart (via list.files() on the installed
    # plotly/DT lib dirs) rather than hardcoded here, so a package upgrade that
    # renames the bundle does not silently 404 this tag. ASCII-only.
    tags$head(
      tags$script(src = paste0("protigy-plotlyjs/", getOption("protigy.plotly_js", "plotly-latest.min.js"))),
      tags$script(src = paste0("protigy-datatables/", getOption("protigy.dt_js", "js/jquery.dataTables.min.js")))
    ),

    # One-time client WebGL capability probe. WebGL renders in the USER's
    # browser, not on the Shiny server, so server GPU is irrelevant; what matters
    # is whether THIS browser has a (hardware or software) WebGL context. On
    # shiny:connected we test for one and report it to the top-level
    # `webgl_supported` input. app_server() reads it to decide whether the PELSA
    # volcanoes render as WebGL scattergl or SVG scatter. ASCII-only (no Unicode).
    tags$script(HTML("
      (function() {
        function hasWebGL() {
          try {
            var c = document.createElement('canvas');
            return !!(window.WebGLRenderingContext &&
              (c.getContext('webgl') || c.getContext('experimental-webgl')));
          } catch (e) { return false; }
        }
        $(document).on('shiny:connected', function() {
          Shiny.setInputValue('webgl_supported', hasWebGL(), {priority: 'event'});
        });
      })();
    ")),

    # JavaScript to manage Clear All Notifications button visibility
    tags$script(HTML("
      $(document).ready(function() {
        var MAX_NOTIFICATIONS = 10;

        // Shiny appends new notifications; earliest in DOM are oldest - drop excess first.
        function trimNotificationsToMax() {
          var notifications = $('.shiny-notification');
          var extra = notifications.length - MAX_NOTIFICATIONS;
          if (extra > 0) {
            notifications.slice(0, extra).remove();
          }
        }

        // Function to update button visibility based on notification presence
        function updateClearButton() {
          trimNotificationsToMax();
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
        tabPanel("Setup + Analysis", helpAnalysisTabUI(), value = "Help-Analysis"),
        tabPanel("PELSA", helpPELSATabUI(), value = "Help-PELSA"),
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
        tabPanel("CV", QCCV_Tab_UI(), value="QC-CV"),
        tabPanel("Correlation", QCCorrelation_Tab_UI(), value="QC-Correlation"),
        tabPanel("PCA", QCPCA_Tab_UI(), value="QC-PCA")),
     
      navbarMenu(
        "Statistics",
        tabPanel("Setup", statSetup_Tab_UI(), value = "Statistics-Setup"),
        tabPanel("Summary", statSummary_Tab_UI(), value = "Statistics-Summary"),
        tabPanel("Volcano Plot", statPlot_Tab_UI(), value = "Statistics-Volcano")
        # tabPanel("Table", statTable_Tab_UI())
      ),

      navbarMenu(
        "PELSA",
        # The PELSA dataset switcher input (pelsa_active_dataset) lives at the
        # TOP LEVEL -- it is NOT module-namespaced -- so a single input drives all
        # three section modules together. See R/tab_pelsa_container.R for the
        # namespacing rationale.
        #
        # A navbarMenu only accepts tabPanels, so the switcher bar cannot sit in
        # one shared node above the tabset. Each tab therefore gets its OWN
        # uiOutput id (no duplicate DOM ids) but all three render the SAME bar
        # from the SAME active-dataset reactive and write the SAME input id --
        # so pelsa_active_dataset remains the single source of truth.
        tabPanel(
          "Setup",
          pelsa_switcher_bar_UI("setup"), PELSASection1_Tab_UI(),
          value = "PELSA-Setup"
        ),
        tabPanel(
          "Summary",
          pelsa_switcher_bar_UI("summary"), PELSASection2_Tab_UI(),
          value = "PELSA-Summary"
        ),
        tabPanel(
          "Volcano Plot",
          pelsa_switcher_bar_UI("volcano"), PELSASection3_Tab_UI(),
          value = "PELSA-Volcano"
        )
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

