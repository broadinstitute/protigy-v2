################################################################################
# Module: Stat_Summary
#
# Allow users to see the summary of their results
################################################################################

##TOOL TIP FUNCTION#######################################################
#' @import shinyWidgets
#' @import shinyBS
helpButton <- function(el, title = NULL, content = NULL, placement = "right", trigger = "hover", offset=0.5, col=10) {
  button <- shinyWidgets::circleButton(as.character(paste(sample(0:9, 10, replace = TRUE), collapse="")), # create a semi-random ID for help-button
                         icon = icon("question", class="fa-xs", #verify_fa=FALSE,
                                     style = "opacity: 0.3"),
                         #status= "default", 
                         size="xs")
  # button <- icon("circle-question")
  fluidRow(column(col, el),
           column(12-col, style=glue('padding-top:{offset}em'),
                  shinyBS::popify(button, title, content, placement=placement, trigger = trigger)))
}

##GETTING PVAL FOR THE HISTOGRAM FUNCTION#######################################################
get_pvals <- function(ome, stat_param, stat_results, group, contrast, pval_type = c("adj.P.Val", "P.Value")) {
  pval_type <- match.arg(pval_type)
  df <- stat_results[[ome]]
  test <- stat_param[[ome]]$test
  
  if (test == "One-sample Moderated T-test") {
    keyword <- group
    pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", pval_type, ")")
  } else if (test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(contrast, " / "))
    pattern <- paste0("(?i)(?=.*", groups[1], ")(?=.*", groups[2], ")(?=.*", pval_type, ")")
  } else {
    pattern <- pval_type
  }
  
  col_name <- grep(pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  stopifnot(!is.na(col_name))
  
  pvals <- df[[col_name]]
  pvals <- pvals[!is.na(pvals)]
  pvals
}

##PVAL HISTOGRAM FUNCTION#######################################################
plot_pval_histogram <- function(pvals, title, xlabel, stat_param, ome) {
  ggplot(data.frame(pval = pvals), aes(x = pval)) +
    geom_histogram(breaks = seq(0, 1, by = 0.01), fill = "#4d4d4d", color = "white") +
    geom_vline(xintercept = stat_param[[ome]]$cutoff, color = "red", linetype = "dashed", size = 1) +
    labs(title = title, x = xlabel, y = "Number of Features") +
    xlim(0, 1) +
    theme(
      panel.background = element_rect(fill = "#f0f0f0", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
}