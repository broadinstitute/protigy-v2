###############################################################################
## Run this when publishing to RStudio Connect
# options(repos = c(BiocManager::repositories()))
###############################################################################

library(shiny)
library(shinydashboard)
library(cmapR)
library(ggplot2)
library(plotly)
library(tools)
library(dplyr)
library(circlize)
library(ComplexHeatmap)
library(shinyBS)
library(WriteXLS)
library(RColorBrewer)
library(shinyalert)
library(shinyjqui)

# source helper functions
lapply(list.files('src/', full.names = T, recursive = T), source)

# for now, load GCT
if(!('GCTs' %in% ls())) {
  # files to load
  gct_files <- c(Prot = 'test_data/BRCA/proteome-aggregate.gct', 
                 Phos = 'test_data/BRCA/phosphoproteome-aggregate.gct',
                 RNA = 'test_data/BRCA/rna-aggregate.gct')
  
  GCTs <- lapply(gct_files, parse_gctx)
}

# for now, hard code
col.of.interest <- "PAM50"

GENEMAX <- 20
FILENAMESTRING <- "New-Protigy"
MAX_ANNO_LEVELS <- 5

UPLOADMAX <- 100 # maximum file upload size in MB




# set maximum upload size
options(shiny.maxRequestSize = UPLOADMAX*1024^2)



