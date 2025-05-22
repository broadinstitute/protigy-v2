################################################################################
# Module: QC_PCA
#
# Produce PCA plots
################################################################################

## plot PCA
create_PCA_plot <- function (gct, col_of_interest, ome, custom_color_map = NULL, comp.x=1, comp.y=2) {
  #browser()
  
  #sort matrix by annotation
  mat <- gct@mat
  group <- as.character(gct@cdesc[[col_of_interest]])
  annot <- data.frame('sample'=colnames(mat),"annot"=group)
  #replace NA with characters so that colors map appropriately
  annot$annot[is.na(annot$annot)]="NA"
  annot$annot <- as.factor(annot$annot)
  mat <- mat[,order(annot$annot)]
  annot <- annot[order(annot$annot),]
  colnames(annot)[2] = col_of_interest
  
  #remove zero-variance columns and calculate PCA
  data.norm <- mat %>% data.frame() %>% drop_na() %>% t()
  data.norm <- data.norm[,apply(data.norm, 2, var, na.rm=TRUE) != 0]
  my_pca <- prcomp (data.norm, center=TRUE, scale=TRUE)
  
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
  
  #create PCA plot using autoplot
  g <- autoplot(my_pca, x=comp.x,y=comp.y,data=annot, colour=col_of_interest) + 
    geom_hline(yintercept = 0, lty = "longdash", color = "darkgrey") +
    geom_vline(xintercept = 0, lty = "longdash", color = "darkgrey") +
    theme_bw() + #change theme
    theme(text= element_text(size=12)) + #change font sizes
    color_definition + #color scale
    ggtitle(paste0("PCA plot by ",col_of_interest,": ",ome)) + #main title
    labs(colour = col_of_interest)#legend title
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
  expt.var <- data %>% group_by(experimental.factor) %>% 
    summarize (sum.total.var.pct=sum(pct.exp/100 * pct.var[dims]/100 * 100))
  
  g <- ggplot (data=data,aes(as.factor(dims),pct.exp,group=experimental.factor,color=experimental.factor)) +
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

