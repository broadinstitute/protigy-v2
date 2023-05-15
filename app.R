################################################################################
# This file exists purely for deployment purposes
# To deploy:
# 1. run: `options(repos = c(BiocManager::repositories()))`
# 2. run: `rsconnect::deployApp()` OR use the blue button on top of this file
################################################################################

pkgload::load_all(".")
launchApp()
