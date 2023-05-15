
# helper function for some  validate() statements
# makes only the first erroneous need() statement show up
`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}