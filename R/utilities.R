
# helper function for some  validate() statements
# makes only the first erroneous need() statement show up
`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}

# wrapper for shinyalert and tryCatch to handle errors gracefully
# can specify the following:
#   show.success/warning/error = show the shiny alert
#   text.success/warning/error = message for shiny alert
#   return.success/warning/error = what to return for each condition
my_shinnyalert_tryCatch <- function(expr,
                                    text.success = NULL,
                                    show.success = F, 
                                    return.success = NULL, # default = return the output of the expression
                                    text.warning = NULL,
                                    return.warning = NULL, # default = return the output of the expression
                                    show.warning = T,
                                    text.error = NULL,
                                    show.error = T,
                                    return.error = "ERROR") {
  tryCatch({
    # evaluate the expression
    out <- eval(expression(expr))
    
    # display shiny alert
    if (show.success) {
      if (is.null(text.success)) {text.success <- "Success!"}

      shinyalert::shinyalert(
        text = text.success,
        type = "success"
      )
    }
    
    # return expression output (default) or user-specified output
    if (is.null(return.success)) {
      return(out)
    } else {
      return(return.success)
    }
    
  ## warning block
  }, warning = function(cond) {
    
    # display shiny alert
    if (show.warning) {
      if (is.null(text.warning)) {text.warning <- paste0(cond)}
      
      shinyalert::shinyalert(
        text = text.warning,
        type = "warning"
      )
    }
    
    message("Caught a warning:")
    cat(paste0(cond))
    
    # return user-specified output
    if (!is.null(return.warning)) {
      return(return.warning)
    } 
    
  ## error block
  }, error = function(cond) {
    
    # display shiny alert
    if (show.error) {
      if (is.null(text.error)) {text.error <- paste0(cond)}
      
      shinyalert::shinyalert(
        text = text.error,
        type = "error"
      )
    }
    
    message("Caught an error!")
    cat(paste0(cond))
    
    return(return.error)
  })
}