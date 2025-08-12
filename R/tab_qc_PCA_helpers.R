################################################################################
# Module: QC_PCA
#
# Produce PCA plots
################################################################################

## plot PCA
create_PCA_plot <- function (gct, col_of_interest, ome, custom_color_map = NULL, comp.x=1, comp.y=2, 
                            second_col_of_interest = NULL, var1_display = "color", var2_display = "shape") {
  # Check for valid PC inputs
  if (is.null(comp.x) || is.null(comp.y) || length(comp.x) == 0 || length(comp.y) == 0) {
    stop("PC1 and PC2 must be valid and non-empty.")
  }
  #print error message if PC1=PC2
  if(comp.x==comp.y){
    stop("PC1 and PC2 are equal. Please try again with different values for PC1 and PC2.")
  }
  
  # validate dual variable inputs
  if (!is.null(second_col_of_interest)) {
    if (var1_display == var2_display) {
      stop("Both variables cannot use the same display method. Please select different display options for each variable.")
    }
    if (second_col_of_interest == col_of_interest) {
      stop("Second variable must be different from the first variable.")
    }
  }
  
  #sort matrix by annotation
  mat <- gct@mat
  group <- as.character(gct@cdesc[[col_of_interest]])
  annot <- data.frame('sample'=colnames(mat),"annot"=group)
  #replace NA with characters so that colors map appropriately
  annot$annot[is.na(annot$annot)]="NA"
  annot$annot <- as.factor(annot$annot)
  
  # add second annotation if provided
  if (!is.null(second_col_of_interest)) {
    second_group <- as.character(gct@cdesc[[second_col_of_interest]])
    annot$second_annot <- second_group
    #replace NA with characters so that colors map appropriately
    annot$second_annot[is.na(annot$second_annot)] <- "NA"
    annot$second_annot <- as.factor(annot$second_annot)
    colnames(annot)[3] <- second_col_of_interest
  }
  
  mat <- mat[,order(annot$annot)]
  annot <- annot[order(annot$annot),]
  colnames(annot)[2] = col_of_interest
  
  #remove zero-variance columns and calculate PCA
  data.norm <- mat %>% data.frame() %>% drop_na() %>% t()
  data.norm <- data.norm[,apply(data.norm, 2, var, na.rm=TRUE) != 0]
  my_pca <- prcomp (data.norm, center=TRUE, scale=TRUE)
  
  # get variance explained
  vars <- my_pca$sdev^2
  prop_vars <- vars / sum(vars)
  
  # get color definition
  #NOTE: need to add NA as a color or else it doesn't show up properly in the legend
  if (is.null(custom_color_map)) {
    color_defintion <- NULL
  } else if (custom_color_map$is_discrete) {
    colors <- c(unlist(custom_color_map$colors),"gray50")
    names(colors) <- c(custom_color_map$vals,NA)
    color_definition <- scale_colour_manual(values = colors)
  } else {
    group <- as.numeric(group)
    color_definition <- scale_colour_gradient2(
      low = custom_color_map$colors[which(custom_color_map$vals == "low")],
      mid = custom_color_map$colors[which(custom_color_map$vals == "mid")],
      high = custom_color_map$colors[which(custom_color_map$vals == "high")],
      midpoint = mean(min(group), max(group)),
      na.value = custom_color_map$colors[which(custom_color_map$vals == "na_color")]
    )
  }
  
  # Use manual ggplot for PCA points with enhanced tooltip
  pca_df <- as.data.frame(my_pca$x)
  pca_df$sample <- rownames(pca_df)
  pca_df <- merge(pca_df, annot, by = "sample")
  
  # Compose tooltip text
  tooltip_text <- paste0(
    "Sample: ", pca_df$sample,
    "<br>PC", comp.x, ": ", signif(pca_df[[paste0("PC", comp.x)]], 4),
    "<br>PC", comp.y, ": ", signif(pca_df[[paste0("PC", comp.y)]], 4),
    "<br>", col_of_interest, ": ", pca_df[[col_of_interest]]
  )
  
  # Add second variable to tooltip if present
  if (!is.null(second_col_of_interest)) {
    tooltip_text <- paste0(tooltip_text, "<br>", second_col_of_interest, ": ", pca_df[[second_col_of_interest]])
  }
  pca_df$tooltip <- tooltip_text
  
  # Create aesthetic mappings based on display preferences
  if (is.null(second_col_of_interest)) {
    # Single variable - use existing logic
    plot_aes <- aes_string(
      x = paste0("PC", comp.x),
      y = paste0("PC", comp.y),
      color = col_of_interest,
      text = "tooltip"
    )
    plot_title <- paste0("PCA plot by ", col_of_interest, ": ", ome)
  } else {
    # Two variables - create appropriate aesthetic mapping
    if (var1_display == "color" && var2_display == "shape") {
      plot_aes <- aes_string(
        x = paste0("PC", comp.x),
        y = paste0("PC", comp.y),
        color = col_of_interest,
        shape = second_col_of_interest,
        text = "tooltip"
      )
    } else if (var1_display == "shape" && var2_display == "color") {
      plot_aes <- aes_string(
        x = paste0("PC", comp.x),
        y = paste0("PC", comp.y),
        color = second_col_of_interest,
        shape = col_of_interest,
        text = "tooltip"
      )
    } else {
      # Default fallback
      plot_aes <- aes_string(
        x = paste0("PC", comp.x),
        y = paste0("PC", comp.y),
        color = col_of_interest,
        text = "tooltip"
      )
    }
    plot_title <- paste0("PCA plot by ", col_of_interest, " and ", second_col_of_interest, ": ", ome)
  }
  
  g <- ggplot(pca_df, plot_aes) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, lty = "longdash", color = "darkgrey") +
    geom_vline(xintercept = 0, lty = "longdash", color = "darkgrey") +
    theme_bw() +
    theme(text = element_text(size = 12)) +
    ggtitle(plot_title) +
    labs(
      x = paste0("PC", comp.x, " (", round(prop_vars[comp.x] * 100, 1), "%)"),
      y = paste0("PC", comp.y, " (", round(prop_vars[comp.y] * 100, 1), "%)")
    )
  
  # Apply color definition
  if (!is.null(color_definition)) {
    g <- g + color_definition
  }
  
  g
}

## calculate PCA regression
pca_variance_explained <- function (pca,cdesc,components=c(1:10)){
  # Obtain the principal component coordinates
  p <- data.frame (pca$x[,components])
  
  # Intitialize the result data frame
  data <- data.frame (dims=character(),
                      pct.exp=numeric(),
                      experimental.factor = character() )
  
  # Loop through all component-metadata combinations
  for (i in colnames(cdesc)) {
    for (j in colnames(p)) {
      # Check if the current metadata vector is valid
      if ( (sum(is.na(cdesc[,i])) == length(cdesc[,i])) |
           (length(levels(as.factor(cdesc[,i]))) < 2) ) {
        next
      }
      
      # Fit a linear model between the principal component and metadata variable
      fit <- lm (p[,j] ~ cdesc[,i])
      af <- anova (fit)
      afss <- af$"Sum Sq"
      
      dimensions <- as.numeric (gsub("PC","",j))
      data <- rbind (data,
                     data.frame (dims = dimensions,
                                 pct.exp = afss[1]/sum(afss)*100,
                                 experimental.factor=i))
    }
  }
  var.explained <- pca$sdev^2
  pct.var <- var.explained * 100 / sum (var.explained)
  
  # calculate % sum total (over pca components in p) of variance attributable to each experimental factor
  expt.var <- data %>% group_by(.data$experimental.factor) %>% 
    summarize (sum.total.var.pct=sum(.data$pct.exp/100 * pct.var[.data$dims]/100 * 100))
  
  g <- ggplot (data=data,aes(as.factor(.data$dims),.data$pct.exp,group=.data$experimental.factor,color=.data$experimental.factor)) +
    geom_line() + geom_point() + labs (x="Component (% Total Variance Explained)",y="% Variance Explained within Component") + 
    theme_bw() + scale_x_discrete(labels = paste0 ("PC",data$dims, " (",round(pct.var,1),"%)")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return (list (plot=g, table=expt.var))
}

##plot PCA regression
create_PCA_reg <- function(gct, col_of_interest, ome, custom_color_map = NULL,components.max=10){
  
  #get data and annotations
  mat <- gct@mat
  cdesc <- gct@cdesc
  
  #remove zero-variance columns and calculate PCA
  data.norm <- mat %>% data.frame() %>% drop_na() %>% t()
  data.norm <- data.norm[,apply(data.norm, 2, var, na.rm=TRUE) != 0]
  my_pca <- prcomp (data.norm, center=TRUE, scale=TRUE)
  
  #perform batch effect check and plot PCA regression
  pca.var <- pca_variance_explained (my_pca, cdesc[col_of_interest], components=1:components.max)
  g <- pca.var$plot+ggtitle(glue("Cumulative Variance Explained by {col_of_interest} for {ome}: ",'{round(pca.var$table$sum.total.var.pct, digits=2)}'))
}

