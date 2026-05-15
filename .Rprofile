# Project-level .Rprofile for ProTIGY
#
# Configures Bioconductor repositories for interactive RStudio sessions so
# that RStudio's "Install Required Packages" prompt and install.packages()
# can resolve the Bioc deps declared in DESCRIPTION (ComplexHeatmap, cmapR,
# AnnotationDbi, org.Hs.eg.db, org.Mm.eg.db, limma, vsn, preprocessCore).
#
# This file deliberately does NOTHING in non-interactive contexts (CI,
# R CMD check, install.packages() child processes, devtools::install(),
# pak, etc.) to avoid:
#   - Recursive install.packages("BiocManager") storms when this profile
#     is re-sourced by every child R session spawned during dep install.
#   - Overriding the repos= setting that CI workflows (RSPM) have already
#     configured.
#   - Triggering R CMD check NOTEs about non-standard top-level files.
#
# Run setup.R for a one-shot install path that does not rely on this
# profile being active.

local({
  if (!interactive()) return(invisible())
  if (nzchar(Sys.getenv("CI"))) return(invisible())
  if (nzchar(Sys.getenv("R_PROTIGY_RPROFILE_LOADED"))) return(invisible())
  Sys.setenv(R_PROTIGY_RPROFILE_LOADED = "1")

  if (requireNamespace("BiocManager", quietly = TRUE)) {
    options(repos = BiocManager::repositories())
  } else {
    message(
      "[ProTIGY] BiocManager not installed. ",
      "Run install.packages('BiocManager') (or source('setup.R')) ",
      "so Bioconductor packages resolve."
    )
  }
})
