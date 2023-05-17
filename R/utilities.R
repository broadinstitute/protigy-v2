
# helper function for some  validate() statements
# makes only the first erroneous need() statement show up
`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}

# wrapper for shinyalert and tryCatch to handle errors gracefully
# can specify the following:
#   show.warning/error = show the shiny alert
#   text.warning/error = message for shiny alert
#   return.error = what to return if there is an error
my_shinyalert_tryCatch <- function(expr,
                                   text.warning = NULL,
                                   show.warning = T,
                                   text.error = NULL,
                                   show.error = T,
                                   return.error = NULL) {
  tryCatch({
    
    # evaluate expression
    # catch warnings and continue execution
    withCallingHandlers(
      expr = expr, 
      warning = function(cond) {
        # display shiny alert
        if (show.warning) {
          if (is.null(text.warning)) {text.warning <- paste0(cond$message)}
          
          shinyalert::shinyalert(
            text = text.warning,
            type = "warning"
          )
        }
      }
    )
    
  ## error block
  }, error = function(cond) {
    
    # display shiny alert
    if (show.error) {
      if (is.null(text.error)) {text.error <- paste0(cond$message)}
      
      shinyalert::shinyalert(
        text = text.error,
        type = "error"
      )
    }
    
    message("Caught an error:")
    cat(paste0(cond))
    
    return(return.error)
  })
}