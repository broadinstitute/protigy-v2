################################################################################
# Module: Stat_Setup
#
# Allow users to setup the test type and parameters
################################################################################

################################################################################
#Mod F Test
################################################################################
modF.test <- function (d, class.vector, output.prefix, id.col=NULL,
                       p.value.alpha=0.05, use.adj.pvalue=TRUE,
                       na.rm=FALSE, nastrings=c("NA", "<NA>", "#NUM!", "#DIV/0!", "#NA", "#NAME?"),
                       plot=TRUE, limits=NULL, xlab="", ylab="", plot.by.group=TRUE,
                       add.xy.axes=TRUE, intensity=FALSE, ...) {
  #
  # data.file should contain one peptide in each row.
  # The columns contain the normalized log-ratio from each replicate
  # (technical or biological), representing a group/class/comparison of
  # interest. The ratio is based one class of interest and its reference
  # or control i.e., ratio = intensity_class / intensity_ref;
  # this test calculates the p-value for determining if peptide p is
  # differentially regulated in any of the classes (i.e., if the log
  # ratio is different from 0 for any of the classes).
  # The class.vector provides the class label for each non-id column.
  # A pairs plot is generated for sample from each class.
  # An id column can be optionally included in the data.file to track
  # peptides (row numbers are used as id if a column is not specified).
  #
  # graphics can be controlled using ...
  #  when using scatterhist (for 2 replicates), this can be arguments to points
  #  when > 2 replicates are present, ... can include arguments to points in
  #   addition to: plot.col, subset.col, hist.col, hist.breaks,
  #                prefix (for correlation), cex.cor
  
  id <- id <- d[,id.col]
  data <-  d [, setdiff (colnames (d), id.col)]
  
  cat('\n-- modF.test --\n')
  
  # create design matrix
  f <- factor (class.vector)
  design <- model.matrix ( ~ 0 + f )
  
  # moderated 2-sample t / F test
  # use row centered data -- this does not affect 2-sample t test results,
  #  but makes the F test more interpretable (to identify groups that are different, as
  #  opposed to groups with non-zero average values)
  data.rownorm <- sweep (data, MARGIN=1, STATS=apply (data, 1, mean, na.rm=TRUE))
  fit <- lmFit (data.rownorm, design)
  #trend=TRUE for intensity data
  #if it fails, do trend=FALSE
  if(intensity){
    fit <- tryCatch({
      eBayes (fit, trend=TRUE, robust=TRUE)},
      error= function(e){
        shinyalert("Setting intensity-trend failed. Performing with trend=FALSE. This usually occurs when the distribution of detected features is not uniform across samples. Please evaluate your data and consider re-running analysis with a stricter missing value filter.",type="warning",immediate=T)
        eBayes (fit, trend=FALSE, robust=TRUE)
      })
  }else{
    fit <- eBayes (fit, robust=TRUE)
  }
  
  sig <- topTable (fit, number=nrow(data), sort.by='none')
  if (use.adj.pvalue) mod.sig <- sig [,'adj.P.Val'] <= p.value.alpha
  else  mod.sig <- sig [,'P.Value'] <= p.value.alpha
  non.na.n <- apply (data, 1, function (x) { sum (is.finite (x)) })
  
  # old code had coefficients in the table
  mod.f <- data.frame ( cbind (id=id, sig, significant=mod.sig, total.n=non.na.n, Log.P.Value=-10*log(sig[,'P.Value'] ,10)), stringsAsFactors=F )
  
  ##if(!is.null(label))
  ##    colnames(mod.f) <- paste(colnames(mod.f), label, sep='.')
  
  final.results <- mod.f
  colnames(final.results) <- sub("^f", "AveExpr.", colnames(final.results))
  
  #replace zero-centered average with the true average expression
  avg <- t(aggregate(t(data),by=list(class.vector),function(x) mean(x,na.rm=T)))
  avg <- avg[-1,]
  avg <- matrix(as.numeric(avg),ncol=ncol(avg))
  final.results[,grepl("AveExpr.",colnames(final.results))]=avg
  final.results[,colnames(final.results)=="AveExpr"]=rowMeans(avg,na.rm=T)
  
  cat('\n-- modF.test exit --\n')
  return( list( input=d, output=final.results, groups=class.vector)  )
}

################################################################################
#Mod T Test
################################################################################
modT.test <- function (d, output.prefix, id.col=NULL, data.col=NULL, fix.id=FALSE,
                       p.value.alpha=0.05, use.adj.pvalue=TRUE, apply.log=FALSE,
                       na.rm=FALSE, nastrings=c("NA", "<NA>", "#NUM!", "#DIV/0!", "#NA", "#NAME?"),
                       plot=TRUE, pairs.plot.2rep=FALSE, limits=NULL, xlab="", ylab="", label='', intensity=FALSE, ...) {
  #
  # data.file should contain one peptide in each row.
  # The columns contain the normalized log-ratio from each replicate
  # (technical or biological). The ratio is based on classes of interest
  # that need to be distinguished: i.e., ratio = intensity_A / intensity_B;
  # this test calculates the p-value for determining if peptide p is
  # differentially regulated between classes A and B (i.e., if the log
  # ratio is different from 0).
  # While the standard scatter plot routine can only handle 2 replicates,
  # a pairs plot is created when there are more than 2 replicates.
  # The moderated t-test can be applied to any number of replicates.
  # An id column can be optionally included in the data.file to track
  # peptides (row numbers are used as id if a column is not specified).
  #
  # graphics can be controlled using ...
  #  when using scatterhist (for 2 replicates), this can be arguments to points
  #  when > 2 replicates are present, ... can include arguments to points in
  #   addition to: plot.col, subset.col, hist.col, hist.breaks,
  #                prefix (for correlation), cex.cor
  
  cat('\n-- modT.test --\n')
  id <- d[, id.col]
  ##if ( any (duplicated (id)) ) stop ('IDs are not unique. Use fix.id=TRUE option')
  
  # extract data columns
  if (is.null (data.col)) data <- d [, setdiff (colnames (d), id.col)]
  else data <- d [, make.names (data.col)]
  
  data <- data.matrix(data)
  
  # log transform is required
  if (apply.log) data <- log2 (data)
  
  ## moderated t test
  mod.t.result <- moderated.t (data, intensity=intensity)
  ##View(data)
  if (use.adj.pvalue) mod.sig <- mod.t.result [,'adj.P.Val'] <= p.value.alpha
  else  mod.sig <- mod.t.result [,'P.Value'] <= p.value.alpha
  change <- apply (data, 1,
                   function (x) {
                     x <- x [is.finite (x)]
                     ret.value <- '?'
                     if ( all (x < 0) ) ret.value <- 'down'
                     else if ( all (x > 0)) ret.value <- 'up'
                     return (ret.value)
                   })
  ## 20151210 kk
  mod.t.result <- data.frame( mod.t.result, change=change, significant=mod.sig, Log.P.Value=-10*log(mod.t.result$P.Value,10), stringsAsFactors=F)
  
  ## add label
  if(!is.null(label))
    colnames(mod.t.result) <- paste(colnames(mod.t.result), label, sep='.')
  
  mod.t <- data.frame ( cbind (data.frame (id=id), data, mod.t.result), stringsAsFactors=F )
  ##colnames (mod.t)[1] <- id.col   # retain id.col (if provided)
  ##rownames(mod.t) <- make.unique( as.character(mod.t[,1]), sep='_' )
  rownames(mod.t) <- id
  
  
  final.results <- mod.t
  cat('\n-- modT.test exit --/n')
  return( list(input=d, output=final.results) )
}


moderated.t <- function (data, design=NULL, intensity=FALSE) {
  ## data is a table with rows representing peptides/proteins/genes
  ## and columns representing replicates
  
  cat('\n-- moderated.t --\n')
  
  data.matrix <- data.frame (data, stringsAsFactors=F)
  
  ## the design matrix is expected to be:
  ##    ref    comparison
  ##     1         0
  ##    1         0
  ##         ...
  ##     1         1
  ##  where comparison has 0's and 1's corresponding
  ##  to the column position of the 2 classes in data
  ##  (see limma user manual section 13)
  
  #cat('here1 ')
  ##save(data.matrix, file='data.matrix.RData')
  
  ## use robust eBayes
  ## see Phipson, B., Lee, S., Majewski, I. J., Alexander, W. S., & Smyth, G. (2013). Tech Report.
  ##     Empirical Bayes in the presence of exceptional cases, with application to microarray data.
  
  #############################################
  ## two sample test
  if(!is.null(design)){
    m <- lmFit (data.matrix, design)
    #try trend=TRUE for intensity data
    #if it fails, perform trend=FALSE
    if(intensity){
      m <- tryCatch({
        eBayes (m, trend=TRUE, robust=TRUE)},
        error= function(e){
          shinyalert("Setting intensity-trend failed. Performing with trend=FALSE. This usually occurs when the distribution of detected features is not uniform across samples. Please evaluate your data and consider re-running analysis with a stricter missing value filter.",type="warning",immediate=T)
          eBayes (m, trend=FALSE, robust=TRUE)
        })
    }else{
      m <- eBayes (m, robust=TRUE)
    }
    sig <- topTable (m, coef=colnames (design)[2], number=nrow(data), sort.by='none')
  } else {
    #############################################
    ## one sample test
    ##cat('here2 ')
    m <- lmFit (data.matrix, method='robust')
    ##cat('here3 ')
    #one-sample t-test is only run for ratio data
    m <- eBayes (m, trend=FALSE, robust=TRUE)
    ##at('here4 ')
    sig <- topTable (m, number=nrow(data), sort.by='none')
    ##cat('here5 ')
  }
  cat('\n-- moderated.t exit --\n')
  return (sig)
}

#################################################################
#function for one sample
#################################################################
calculate_fc <- function(tab, grp.vec, groups.comp, test, 
                         mode='sort',  ## for assigning pairwise comparisons
                         ## sort: group names are ordered alphanumerically
                         ##       e.g. "Exp_B" and "Exp_A" -> "Exp_A.vs.Exp_B",
                         center=F     ## if TRUE median centering will be applied
){
  #browser()
  groups <- unique(grp.vec)
  
  ## ##########################################
  ## average groups
  group_avg <- lapply(groups, function(gg){
    gg.idx = names(grp.vec)[ which(grp.vec == gg) ]
    apply(tab[, gg.idx], 1, mean, na.rm=T)
  })
  group_avg <- data.frame(Reduce('cbind', group_avg))
  colnames(group_avg) <- groups
  
  ## ##########################################
  ## one sample & mod F: just average the log values
  if(test %in% c("One-sample Moderated T-test","Moderated F test")){
    group_fc <- group_avg
  }

  ## ##########################################
  ## two sample: subtract averaged groups
  if(test %in% c("Two-sample Moderated T-test")){
    
    groups.comp.split <- strsplit(groups.comp, split = '\\.vs\\.')
    names(groups.comp.split) <- groups.comp
    
    ## calculate log FCs
    group_fc <- lapply(groups.comp.split, function(x){
      g1 <- x[1] 
      g2 <- x[2]
      
      ## subtract logs
      g2.over.g1 <- group_avg[, g2] - group_avg[, g1]
      g2.over.g1
    })
    group_fc <- data.frame(Reduce('cbind', group_fc))
    colnames(group_fc) <- groups.comp
  }
  
  ## median center data
  if(center)
    group_fc <- apply(group_fc, 2, function(column) column - median(column, na.rm=T))
  
  ## for one-sample/mod F/none, it is average, not FC
  if(test %in% c("One-sample Moderated T-test", "Moderated F test")){
    colnames(group_fc) <- paste0('RawAveExpr.', colnames(group_fc))
  }else{
    colnames(group_fc) <- paste0('RawlogFC.', colnames(group_fc))
  }
  return(group_fc)
}


# if(select_test == 'Moderated F test'){
#   
#   withProgress(message='moderated F-test', value=0, {
#     
#     tab.group <- cbind(tab[, id.col], tab[, names(groups)])
#     colnames(tab.group)[1] <- id.col
#     
#     res.comb <- modF.test( tab.group, id.col=id.col, class.vector=groups, nastrings=NASTRINGS, na.rm=FALSE, intensity=intensity)$output
#     colnames(res.comb) <- sub('^X', '', colnames(res.comb))
#   })
#   
#   ##################################    
#   ## add FC before normalization
#   res.comb <- merge(res.comb, fc.before.norm,by="row.names")
#   rownames(res.comb) <- res.comb[,1]
#   res.comb <- res.comb[,-1]
#   
#   ##################################
#   ## reorder columns of the data table
#   res.id <- res.comb$id ## id column
#   res.exprs <- tab[, names(groups)]
#   res.test <- res.comb[, grep('^logFC\\.|^AveExpr|^t|^P\\.Value|^adj\\.P\\.Val|^Log\\.P\\.Value|^RawlogFC\\.|^RawAveExpr\\.', colnames(res.comb))] ## test results
#   res.test <- res.test[, order(colnames(res.test))]
#                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
#   ## assemble new table
#   res.comb <- data.frame(id=res.id, res.test, res.exprs, stringsAsFactors=F)
#   ##res.comb <- res.comb[tab[, id.col], ]
#   res.comb <- res.comb[rownames(tab),]
# }





# observeEvent(input$run_test_button, {
# if(select_test == 'Moderated F test'){
#   req(chosen_omes_reactive())
#   req(chosen_groups_reactive())
#   req(GCTs())
#   
#   withProgress(message='moderated F-test', value=0, {
#     for (ome_name in chosen_omes_reactive()) {
#       ome_gct <- GCTs()[[ome_name]]
#       ome_data <- ome_gct@mat
#       tab <- as.data.frame(ome_data)
#       
#       tab.group <- cbind(tab[, id.col], tab[, names(groups)])
#       colnames(tab.group)[1] <- id.col
# 
#       res.comb <- modF.test( tab.group, id.col=id.col, class.vector=groups, nastrings=NASTRINGS, na.rm=FALSE, intensity=intensity)$output
#       colnames(res.comb) <- sub('^X', '', colnames(res.comb))
#     
# 
#     ##################################
#     ## add FC before normalization
#     res.comb <- merge(res.comb, fc.before.norm,by="row.names")
#     rownames(res.comb) <- res.comb[,1]
#     res.comb <- res.comb[,-1]
#   
#     ##################################
#     ## reorder columns of the data table
#     res.id <- res.comb$id ## id column
#     res.exprs <- tab[, names(groups)]
#     res.test <- res.comb[, grep('^logFC\\.|^AveExpr|^t|^P\\.Value|^adj\\.P\\.Val|^Log\\.P\\.Value|^RawlogFC\\.|^RawAveExpr\\.', colnames(res.comb))] ## test results
#     res.test <- res.test[, order(colnames(res.test))]
#   
#     ## assemble new table
#     res.comb <- data.frame(id=res.id, res.test, res.exprs, stringsAsFactors=F)
#     ##res.comb <- res.comb[tab[, id.col], ]
#     res.comb <- res.comb[rownames(tab),]
#     }
#   })
# }
# })



run_modF_test <- function(data, class.vector, id.col = NULL, 
                          p.value.alpha = 0.05, use.adj.pvalue = TRUE, 
                          intensity = FALSE, ...) {
  if (is.null(id.col)) id.col <- colnames(data)[1]
  library(limma)
  modF.test(
    d = data,
    class.vector = class.vector,
    output.prefix = NULL,
    id.col = id.col,
    p.value.alpha = p.value.alpha,
    use.adj.pvalue = use.adj.pvalue,
    intensity = intensity,
    ...
  )$output
}


################################################################################
#One sample mod T
################################################################################

# if(input$select_test== "One-sample Moderated T-test"){
#   req(chosen_omes_reactive())
#   req(chosen_groups_reactive())
#   req(GCTs())
#   
#   
#   for (ome_name in chosen_omes_reactive()) {
#     withProgress(message='One-sample T test', value=0, {
#       ome_gct <- GCTs()[[ome_name]]
#       ome_data <- ome_gct@mat
#       tab <- as.data.frame(ome_data)
#       rdesc <- ome_gct@rdesc
#       
#       id.col <- names(Filter(function(col) !is.numeric(col), rdesc))[1]
#       tab[[id.col]] <- rdesc[[id.col]]
#       
#       count=0
#       res.comb <- tab
#     ## loop over groups
#     for(g in unique(chosen_groups_reactive())){
#       
#       ## extract table of current group
#       tab.group <- cbind(tab[, id.col], tab[, names(groups)[which(groups == g)]])
#       colnames(tab.group)[1] <- id.col
#       
#       res.tmp <- modT.test( tab.group, id.col=id.col, plot=F, nastrings=NASTRINGS, label=g, na.rm=FALSE)$output
#       #previous code would incorrectly throw away a test result if a feature was missing
#       # if(count == 0){
#       #     res.comb <- res.tmp
#       # } else {
#       #     if(nrow(res.tmp ) != nrow(res.comb)) stop( "number of rows don't match!\n" )
#       #     res.tmp <- res.tmp[rownames(res.comb), ]
#       #     res.comb <- cbind(res.comb, res.tmp)
#       # }
#       
#       #create data frame of expression values and test results
#       res.test <- res.tmp[, !colnames(res.tmp)%in%colnames(res.comb)]
#       res.comb <- base::merge(res.comb,res.test,by="row.names",all=T)
#       rownames(res.comb) <- res.comb[,1]
#       res.comb <- res.comb[,-1]
#       
#       #############################################
#       ## update progress bar
#       incProgress( 1/length(unique(groups.comp) ), detail=g)
#       count=count + 1
#     }
#   })
#   
#   ##################################    
#   ## add FC before normalization
#   res.comb <- merge(res.comb, fc.before.norm,by="row.names")
#   rownames(res.comb) <- res.comb[,1]
#   res.comb <- res.comb[,-1]
#   
#   ##################################
#   ## reorder columns of the table
#   res.id <- res.comb$id ## id column
#   res.exprs <- res.comb[, names(groups)] ## expression values
#   res.test <- res.comb[, grep('^logFC\\.|^AveExpr\\.|^t\\.|^P\\.Value\\.|^adj\\.P\\.Val\\.|^Log\\.P\\.Value\\.|^RawlogFC\\.|^RawAveExpr\\.', colnames(res.comb))] ## test results
#   res.test <- res.test[, order(colnames(res.test))]
#   ## assemble new table
#   res.comb <- data.frame(id=res.id, res.test, res.exprs, stringsAsFactors=F)
#   ##res.comb <- res.comb[tab[, id.col], ]
#   res.comb <- res.comb[rownames(tab),]
# }
# }
