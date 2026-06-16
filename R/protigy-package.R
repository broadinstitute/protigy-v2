#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @export
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable, runExample))
#' @import ggplot2
#' @import ggrepel
#' @import dplyr
#' @import tidyr
#' @import ComplexHeatmap
#' @import markdown
#' @import limma
#' @import glue
#' @import ggfortify
#' @import cmapR
#' @import shinyWidgets
#' @import janitor
#' @import snakecase
#' @importFrom Matrix crossprod tcrossprod
#' @importFrom shinydashboard dashboardBody dashboardSidebar
#' @importFrom shinydashboardPlus dashboardPage dashboardHeader box boxSidebar boxDropdown
#' @importFrom grid gpar
#' @importFrom plotly ggplotly plotlyOutput renderPlotly style add_annotations event_register event_data layout
#' @importFrom circlize colorRamp2
#' @importFrom colourpicker colourInput updateColourInput
#' @importFrom shinyBS tipify
#' @importFrom scales label_percent
#' @importFrom RColorBrewer brewer.pal
#' @importFrom yaml read_yaml write_yaml
#' @importFrom shinyjqui orderInput updateOrderInput
#' @importFrom grDevices colorRampPalette dev.off pdf boxplot.stats
#' @importFrom methods new
#' @importFrom stats density mad median quantile sd aggregate coef qnorm qt setNames cor anova lm prcomp var complete.cases model.matrix
#' @importFrom zip zip
#' @importFrom utils tail stack combn compareVersion packageVersion write.csv write.table head
#' @importFrom rlang .data
#' @importFrom ggthemes geom_tufteboxplot
#' @importFrom DT datatable renderDataTable dataTableOutput
#' @importFrom Matrix Matrix
#' @importFrom matrixStats rowSds rowMeans2 rowMedians colMedians
#' @importFrom stringi stri_locate_all_fixed stri_replace_all_fixed
#' @importFrom data.table data.table setDT setkey foverlaps setorder rbindlist .SD :=
#' @importFrom httr2 request req_url_query req_url_path_append req_retry req_throttle req_perform resp_body_json resp_body_string resp_status req_user_agent req_error
#' @importFrom jsonlite write_json
#' @importFrom IRanges IRanges reduce disjointBins
#' @importFrom ragg agg_png
## usethis namespace: end
NULL

utils::globalVariables(c(
  ".SD", ".N", ".I", "accession", "start", "end",
  "pep_start", "pep_end", "feature_class", "class_score",
  "peptide_seq", "adj_p", "logFC", "gene",
  # PELSA annotation (Task 2I) data.table NSE symbols
  "_row_id", "token_idx", "gene_token", "_rank",
  # PELSA volcano builder (Task 3A) data.table NSE symbols
  ".key", "entry"
))
