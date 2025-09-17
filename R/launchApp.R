#' @title Launch protigy revamp shiny application
#' @description Runs the app by calling `app_UI`, `app_server`, `app_onStart`.
#' @importFrom shiny shinyApp
#' @export
launchApp <- function() {
  shinyApp(
    ui = app_UI, 
    server = app_server,
    onStart = app_onStart,
    options = list(
      width = 3000,    # Width in pixels
      height = 900     # Height in pixels
    )
  )
}
