options(protigy.enable_spectronaut = TRUE)

# Load the Protigy package from source via pkgload so the test app works
# without requiring the package to be installed in the user library.
# Protigy::launchApp() below triggers loadNamespace("Protigy") via `::`,
# which bypasses shinytest2's library()/require() override  -  so we must
# load the namespace explicitly here first.
pkg_root <- normalizePath(file.path(getwd(), "..", "..", "..", ".."))
pkgload::load_all(
  path            = pkg_root,
  export_all      = FALSE,
  helpers         = FALSE,
  attach_testthat = FALSE,
  quiet           = TRUE
)

Protigy::launchApp()
