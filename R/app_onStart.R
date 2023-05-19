################################################################################
# ON START
# This function contains any code that should be executed before the app is run.
# Treat this like a global.R function. However, unfortunately this does NOT 
# allow for defenition of global variables.
# If it becomes relevant, this is also where you should include a call to 
# shiny::onStop() for code to be executed when session ends
################################################################################

app_onStart <- function() {

  # set maximum upload size
  UPLOADMAX <- 100 # upload size in MB
  options(shiny.maxRequestSize = UPLOADMAX*1024^2)
  
  
  
}