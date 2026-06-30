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
  plotly_lib <- system.file("htmlwidgets/lib/plotlyjs", package = "plotly")
  dt_lib     <- system.file("htmlwidgets/lib/datatables", package = "DT")
  if (nzchar(plotly_lib)) {
    shiny::addResourcePath("protigy-plotlyjs", plotly_lib)
  }
  if (nzchar(dt_lib)) {
    shiny::addResourcePath("protigy-datatables", dt_lib)
  }

}
