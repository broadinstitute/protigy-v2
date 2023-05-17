#' @title Launch protigy revamp shiny application
#' @description Runs the app stored in the inst/shiny directory.
#' @export
#' @import shinydashboard
#' @import ggplot2
#' @import dplyr
#' @import ComplexHeatmap
#' @importFrom grid gpar
#' @importFrom cmapR parse_gctx write_gct meta mat merge_gct
#' @importFrom plotly ggplotly plotlyOutput renderPlotly
#' @importFrom circlize colorRamp2
#' @importFrom shinyBS tipify
#' @importFrom WriteXLS WriteXLS
#' @importFrom RColorBrewer brewer.pal
#' @importFrom yaml read_yaml
#' @importFrom shinyjqui orderInput updateOrderInput
launchApp <- function() {
  shiny::runApp(appDir = system.file("shiny", package = "protigyRevamp"))
}
