options(protigy.enable_spectronaut = FALSE)
# Navigate to package root (4 levels up from tests/testthat/apps/full-app/)
pkg_root <- normalizePath(file.path(getwd(), "..", "..", "..", ".."))
pkgload::load_all(
  path            = pkg_root,
  export_all      = FALSE,
  helpers         = FALSE,
  attach_testthat = FALSE,
  quiet           = TRUE
)
Protigy::launchApp()
