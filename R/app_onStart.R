################################################################################
# ON START
#
# This function contains any code that should be executed before the app is run.
# Treat this like a global.R function. However, unfortunately this does NOT 
# allow for definition of global variables.
#
# If it becomes relevant, this is also where you should include a call to 
# shiny::onStop() for code to be executed when session ends
################################################################################

app_onStart <- function() {

  # set maximum upload size
  UPLOADMAX <- 500 # upload size in MB
  options(shiny.maxRequestSize = UPLOADMAX*1024^2)

  # HTMLWIDGET LIBRARY PRE-LOAD (see app_UI). plotly.js and DataTables ship as
  # webpack-UMD bundles. When htmlwidgets delivers them at render time, Shiny
  # injects them through jQuery's $.ajax(dataType="script") -> globalEval path;
  # in that eval scope `module`/`exports` are in scope, so the UMD header takes
  # the CommonJS branch, runs the webpack bootstrap, and calls a top-level
  # `require` that does not exist in the browser -> "require is not defined" ->
  # window.Plotly / DataTables never get assigned -> every plot/table is blank.
  # Loading the SAME library files as native <script src> tags at page startup
  # (app_UI head) lets the UMD header take the browser branch (t.Plotly = e()),
  # which defines the globals cleanly. Expose the installed lib dirs so the head
  # tags can reference them. Registration is idempotent across app restarts.
  # File basenames are derived from the installed package's own lib dir
  # (list.files()) rather than hardcoded, so a future plotly/DT upgrade that
  # renames its bundle (e.g. away from "plotly-latest.min.js") still resolves
  # to a working file instead of a silent 404. Options carry the resolved
  # basenames to app_UI (which builds the <script src> tags); fall back to
  # today's known filenames if resolution ever comes up empty.
  plotly_lib <- system.file("htmlwidgets/lib/plotlyjs", package = "plotly")
  dt_lib     <- system.file("htmlwidgets/lib/datatables/js", package = "DT")

  plotly_js <- "plotly-latest.min.js"
  if (nzchar(plotly_lib)) {
    shiny::addResourcePath("protigy-plotlyjs", plotly_lib)
    plotly_files <- list.files(plotly_lib, pattern = "^plotly.*\\.min\\.js$")
    if (length(plotly_files) > 0) {
      plotly_js <- plotly_files[1]
    }
  }

  dt_js <- "js/jquery.dataTables.min.js"
  if (nzchar(dt_lib)) {
    shiny::addResourcePath("protigy-datatables", dirname(dt_lib))
    dt_files <- list.files(dt_lib, pattern = "^jquery\\.dataTables.*\\.min\\.js$")
    if (length(dt_files) > 0) {
      dt_js <- file.path("js", dt_files[1])
    }
  }

  options(protigy.plotly_js = plotly_js, protigy.dt_js = dt_js)

}
