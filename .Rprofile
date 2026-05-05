# Project-level .Rprofile for ProTIGY
#
# Ensures Bioconductor repositories are available in every R session opened
# under this project, so that:
#   * RStudio's "Install Required Packages" prompt can resolve Bioc deps
#     declared in DESCRIPTION (ComplexHeatmap, cmapR, AnnotationDbi,
#     org.Hs.eg.db, org.Mm.eg.db, limma, vsn, preprocessCore, ...).
#   * devtools::install('.') and install.packages() see Bioc as a repo.
#
# Without this, those packages silently fail to install from CRAN-only
# defaults and the app errors at launch.

local({
  cran <- "https://cloud.r-project.org"

  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    tryCatch(
      utils::install.packages("BiocManager", repos = cran),
      error = function(e) {
        message("Could not install BiocManager automatically: ", conditionMessage(e))
        message("Run install.packages('BiocManager') manually, then restart R.")
      }
    )
  }

  if (requireNamespace("BiocManager", quietly = TRUE)) {
    options(repos = BiocManager::repositories())
  } else {
    options(repos = c(CRAN = cran))
  }
})
