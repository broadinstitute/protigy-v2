#' @title Launch protigy revamp shiny application
#' @description Runs the app stored in the inst/shiny directory.
#' @export
launchApp <- function() {
  shiny::runApp(shiny::shinyApp(
    ui = app_UI, 
    server = app_server,
    onStart = app_onStart))
}
