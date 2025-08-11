################################################################################
# Module: Stat_Summary
#
# Allow users to see the summary of their results
################################################################################

##TOOL TIP FUNCTION#############################################################
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

##GETTING PVAL FOR THE HISTOGRAM FUNCTION#######################################
get_pvals <- function(ome, stat_params, stat_results, group, contrast, pval_type = c("adj.P.Val", "P.Value")) {
  pval_type <- match.arg(pval_type)
  df <- stat_results[[ome]]
  test <- stat_params[[ome]]$test
  
  if (test == "One-sample Moderated T-test") {
    keyword <- group
    pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", pval_type, ")")
  } else if (test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(contrast, " / "))
    contrast_name <- paste0(groups[1], "_vs_", groups[2])
    pattern <- paste0("(?i)", pval_type, "\\.", contrast_name)
    
    # groups <- unlist(strsplit(contrast, " / "))
    # pattern <- paste0("(?i)(?=.*", groups[1], ")(?=.*", groups[2], ")(?=.*", pval_type, ")")
  } else {
    pattern <- pval_type
  }
  
  col_name <- grep(pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  pvals <- df[[col_name]]
  pvals <- pvals[!is.na(pvals)]
  pvals
}

##PVAL HISTOGRAM FUNCTION#######################################################
plot_pval_histogram <- function(pvals, title, xlabel, stat_params, stat_results, ome, group, contrast, pval_type) {
  
  df<- stat_results[[ome]]
  ##ADJ P VALUE COLUMN##
  if (stat_params[[ome]]$test == "One-sample Moderated T-test") {
    keyword <- group
    adjP_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "adj.P.Val.", ")")
  } else if (stat_params[[ome]]$test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(contrast, " / "))
    contrast_name <- paste0(groups[1], "_vs_", groups[2])
    adjP_pattern  <- paste0("adj\\.P\\.Val.*", contrast_name)
  } else if (stat_params[[ome]]$test == "Moderated F test") {
    adjP_pattern <- "(?i)adj\\.P\\.Val"
  } 
  
  adjP_col <- grep(adjP_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  df$adj.P.Val <- as.numeric(df[[adjP_col]])
  
  ##P VAL COLUMN##
  if (stat_params[[ome]]$test == "One-sample Moderated T-test") {
    keyword <- group
    pval_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "P.value.", ")")
  } else if (stat_params[[ome]]$test == "Two-sample Moderated T-test") {
    groups <- unlist(strsplit(contrast, " / "))
    contrast_name <- paste0(groups[1], "_vs_", groups[2])
    pval_pattern  <- paste0("P\\.value.*", contrast_name)
  } else if (stat_params[[ome]]$test == "Moderated F test") {
    pval_pattern <- "(?i)P\\.Value"
  }
  
  pval_col <- grep(pval_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  df$P.Value <- as.numeric(df[[pval_col]])
  
  stat_choice <- stat_params[[ome]]$stat
  cutoff_val <- stat_params[[ome]]$cutoff
  
  if (pval_type == "P.Value") {
    if (stat_choice == "nom.p.val") {
      x_cutoff <- cutoff_val
    } else {
      passing.id <- which(df$adj.P.Val < cutoff_val)
      x_cutoff <- if (length(passing.id) > 0) max(df$P.Value[passing.id], na.rm = TRUE) else Inf
    }
  } else if (pval_type == "adj.P.Val") {
    if (stat_choice == "adj.p.val") {
      x_cutoff <- cutoff_val
    } else {
      passing.id <- which(df$P.Value < cutoff_val)
      x_cutoff <- if (length(passing.id) > 0) max(df$adj.P.Val[passing.id], na.rm = TRUE) else Inf
    }
  }
  
  ggplot(data.frame(pval = pvals), aes(x = pval)) +
    geom_histogram(breaks = seq(0, 1, by = 0.01), fill = "#4d4d4d", color = NA) +
    geom_vline(xintercept = x_cutoff, color = "red", linetype = "solid", size = 0.5) +
    labs(title = title, x = xlabel, y = "Number of Features") +
    xlim(0, 1) +
    theme(
      panel.background = element_rect(fill = "#f0f0f0", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 10)
    )
}