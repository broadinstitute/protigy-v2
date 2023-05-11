#' @export
launchApp <- function() {
  shiny::runApp(appDir = system.file("shiny", package = "protigyRevamp"))
}
