################################################################################
# Module: UTILITIES
# 
# This script contains functions that are used throughout the app. If you are 
# a developer, please feel free to use these functions generously.
#
# Current utility functions include:
# 1. `%then%`: used in `validate()` statements
# 2. Using roxygen import tags for dependencies instead of p_load()
################################################################################

# helper function for some  validate() statements
# makes only the first erroneous need() statement show up
`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}

# wrapper for shinyalert and tryCatch to handle errors gracefully
# can specify the following:
#   show.warning/error = show the shiny alert
#   text.warning/error = message for shiny alert, can include raw HTML
#   return.warning/error = append the warning/error message to text.warning/error
#   return.error = what to return if there is an error
my_shinyalert_tryCatch <- function(expr,
                                   text.warning = NULL,
                                   show.warning = TRUE,
                                   append.warning = FALSE,
                                   text.error = NULL,
                                   show.error = TRUE,
                                   return.error = NULL,
                                   append.error = TRUE) {
  tryCatch({
    # catch warnings and continue execution
    withCallingHandlers(
      expr = expr, 
      warning = function(cond) {
        # display shiny alert
        if (show.warning) {
          if (is.null(text.warning)) {
            text.warning <- paste0(cond$message)
          } else if (append.warning) {
            text.warning <- paste(text.warning, paste0(cond$message))
          }
          
          shinyalert::shinyalert(
            text = HTML(text.warning),
            type = "warning",
            html = TRUE
          )
        }
      }
    )
    
  ## error block
  }, error = function(cond) {
    
    # display shiny alert
    if (show.error) {
      if (is.null(text.error)) {
        text.error <- paste0(cond$message)
      } else if (append.error) {
        text.error <- paste(text.error, paste0(cond$message))
      }
      
      shinyalert::shinyalert(
        text = HTML(text.error),
        type = "error",
        html = TRUE
      )
    }
    
    message("Caught an error:")
    cat(paste0(cond))
    
    return(return.error)
  })
}


# Intelligently trim labels (e.g. sample labels) to the most unique substring
# This is useful for plots that may display very long labels
#' @importFrom stringr str_sub
smart_trim <- function(labels, trim_length=10, default_trim=c("beginning", "end")) {
  default_trim = match.arg(default_trim)
  
  check_length = trim_length # initialize check_length as the FULL trim_length
  # until we've run out of trim_lengths to check
  while (check_length > 0) {
    unique_at_beginning = length(unique(sapply(labels, function(x) {str_sub(x,1,check_length)} )))
    unique_at_end = length(unique(sapply(labels, function(x) {str_sub(x,-check_length,-1)} )))
    
    if (unique_at_beginning == unique_at_end) { # if we have the same number of unique strings
      check_length=check_length-1 # decrease the trim-length we're checking
      next # try again, with a shorter sub_str
    } else if (unique_at_beginning > unique_at_end) { # if we had more unique strings at the beginning of the string
      labels_trimmed = sapply(labels, function(x) {str_sub(x,1,trim_length)} ) # trim to the beginning
      break
    } else { # if we had more unique strings at the beginning of the string
      labels_trimmed = sapply(labels, function(x) {str_sub(x,-trim_length,-1)} ) # trim to the unique_at_end
      break
    }
  }
  
  # if we 
  if (check_length==0) {
    warning(paste0("Labels appear to be equally unique at the beginning and end. Defaulting to ", default_trim, ".\n"))
    labels_trimmed = switch(default_trim,
                            "beginning" = sapply(labels, function(x) {str_sub(x,1,trim_length)} ),
                            "end" = sapply(labels, function(x) {str_sub(x,-trim_length,1)} ))
  }
  
  return(unname(labels_trimmed)) # return the trimmed vectors, unnamed
}

# add css classes or styling to a shiny element
add_css_attributes <- function (el, classes = NULL, styles = NULL) {
  el$attribs$class <- paste(el$attribs$class, classes)
  el$attribs$style <- paste(el$attribs$style, paste(styles, collapse = "; "))
  el
}



