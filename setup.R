# ProTIGY one-shot installer
#
# Run this ONCE after cloning the repo, before launching the app:
#
#   setwd("protigy-v2")
#   source("setup.R")
#
# Why this exists:
#   RStudio's "Install Required Packages" prompt only resolves CRAN packages.
#   ProTIGY also depends on Bioconductor packages (ComplexHeatmap, cmapR,
#   AnnotationDbi, org.Hs.eg.db, org.Mm.eg.db, limma, vsn, preprocessCore),
#   which the prompt silently skips. This script installs BiocManager,
#   pulls the Bioc deps explicitly, then installs ProTIGY from the local
#   source tree with all CRAN deps resolved.

cran_mirror <- "https://cloud.r-project.org"

# 1. BiocManager bootstrap ------------------------------------------------
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  message("Installing BiocManager from CRAN...")
  install.packages("BiocManager", repos = cran_mirror)
}

# Make Bioc visible to install.packages() / devtools::install() for the
# remainder of this session, in addition to whatever .Rprofile set.
options(repos = BiocManager::repositories())

# 2. Bioconductor dependencies -------------------------------------------
bioc_pkgs <- c(
  "ComplexHeatmap",
  "cmapR",
  "AnnotationDbi",
  "org.Hs.eg.db",
  "org.Mm.eg.db",
  "limma",
  "vsn",
  "preprocessCore"
)

missing_bioc <- bioc_pkgs[!vapply(bioc_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_bioc)) {
  message("Installing Bioconductor packages: ",
          paste(missing_bioc, collapse = ", "))
  BiocManager::install(missing_bioc, update = FALSE, ask = FALSE)
} else {
  message("All Bioconductor dependencies already installed.")
}

# 3. devtools + local package install ------------------------------------
if (!requireNamespace("devtools", quietly = TRUE)) {
  message("Installing devtools from CRAN...")
  install.packages("devtools")
}

message("Installing ProTIGY and remaining CRAN dependencies from source tree...")
devtools::install(".", dependencies = TRUE, upgrade = "never")

message("\nSetup complete. Launch the app with:")
message("  library(Protigy)")
message("  Protigy::launchApp()")
