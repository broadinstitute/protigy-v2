## Help Button Function, add a help-button to anything
#' @import shinyWidgets
#' @import shinyBS
helpButton <- function(el, title = NULL, content = NULL, placement = "right", trigger = "hover", offset=0.5, col=10) {
  button <- shinyWidgets::circleButton(as.character(paste(sample(0:9, 10, replace = TRUE), collapse="")), # create a semi-random ID for help-button
                         icon = icon("question", class="fa-xs", #verify_fa=FALSE,
                                     style = "opacity: 0.3"),
                         #status= "default", 
                         size="xs")
  # button <- icon("circle-question")
  fluidRow(column(col, el),
           column(12-col, style=glue('padding-top:{offset}em'),
                  shinyBS::popify(button, title, content, placement=placement, trigger = trigger)))
}