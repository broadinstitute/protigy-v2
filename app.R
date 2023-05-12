# Launch the ShinyApp
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# Run this when publishing to RStudio Connect:
# options(repos = c(BiocManager::repositories()))

pkgload::load_all(".")
launchApp()