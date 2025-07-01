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
#Mod T Test- ONE SAMPLE
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

################################################################################
#Mod T Test- TWO SAMPLE
################################################################################

modT.test.2class <- function (d, output.prefix, groups, id.col=NULL, data.col=NULL,
                              group.na.rm=FALSE, nastrings=c("NA", "<NA>", "#NUM!", "#DIV/0!", "#NA", "#NAME?"), label=NULL, intensity=FALSE) {
  
  cat('\n-- modT.test.2class --\n')
  
  ## store group names
  groups.org <- groups
  groups <- as.numeric(factor(groups, levels=sort(unique(groups)))) ## kk 20170106
  
  id <- d[ , id.col]
  
  ## extract data columns
  if (is.null (data.col)) data <- d [, setdiff (colnames (d), id.col)]
  else data <- d [, make.names (data.col)]
  
  
  ## moderated t test for 2 classes
  design.mat <- cbind (ref=1, comparison=groups)
  mod.t.result <- moderated.t (data, design.mat, intensity)
  
  ## 20151211 kk
  mod.t.result <- data.frame( mod.t.result, Log.P.Value=-10*log(mod.t.result$P.Value,10), stringsAsFactors=F)
  
  ## add label
  if(!is.null(label))
    colnames(mod.t.result) <- paste(colnames(mod.t.result), label, sep='.')
  
  mod.t <- data.frame ( cbind (data.frame (id=id), data, mod.t.result), stringsAsFactors=F )
  rownames(mod.t) <- id ##make.unique(as.character(mod.t[, 1]), sep='_')
  
  ## write out / return results
  final.results <- mod.t
  
  ##invisible (final.results)
  return( list(input=d, output=final.results, groups=groups.org) )
}
##################################################################################
#STATISTICAL TESTING
##################################################################################

observeEvent(input$run_test_button, {
  req(chosen_omes_reactive())
  req(chosen_groups_reactive())
  req(GCTs())
  
  # Capture system output in a file 
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  filename <- paste0("C:/Users/dabburi/Documents/protigy-v2-main/protigy-v2-main/data/output/run_", timestamp, ".txt")
  sink(filename)
  
  ################################################################################
  #     Mod F Test
  ################################################################################
  
  if(input$select_test == 'Moderated F test'){
    
    withProgress(message='moderated F-test', value=0, {
      for (ome_name in chosen_omes_reactive()) {
        for (group_name in chosen_groups_reactive()){
          print(paste('running',ome_name,group_name))
          
          ome_gct <- GCTs()[[ome_name]]
          ome_data <- ome_gct@mat
          tab <- as.data.frame(ome_data)
          
          rdesc <- ome_gct@rdesc
          cdesc <- ome_gct@cdesc
          
          id.col <- names(Filter(function(col) !is.numeric(col), rdesc))[1]
          tab <- cbind(rdesc[[id.col]], tab)
          colnames(tab)[1] <- id.col
          
          sample_names <- colnames(ome_data) 
          groups <- cdesc[sample_names, group_name]
          names(groups) <- sample_names
          
          tab.group <- cbind(tab[[id.col]], tab[, sample_names])
          colnames(tab.group)[1] <- id.col
          
          print("\n ##########  \n TAB GROUP \n ##########")  
          print(head(tab.group))
          
          
          ###F TEST### 
          res.comb<-modF.test(tab.group, id.col=id.col, class.vector=groups, nastrings=NASTRINGS, na.rm=FALSE, intensity=FALSE)$output
          colnames(res.comb) <- sub('^X', '', colnames(res.comb))
          sink()
          
          ##################################
          ## add FC before normalization
          res.comb <- merge(res.comb, fc.before.norm,by="row.names")
          rownames(res.comb) <- res.comb[,1]
          res.comb <- res.comb[,-1]
          
          ##################################
          ## reorder columns of the data table
          res.id <- res.comb$id ## id column
          res.exprs <- tab[, expr_cols]
          res.test <- res.comb[, grep('^logFC\\.|^AveExpr|^t|^P\\.Value|^adj\\.P\\.Val|^Log\\.P\\.Value|^RawlogFC\\.|^RawAveExpr\\.', colnames(res.comb))] ## test results
          res.test <- res.test[, order(colnames(res.test))]
          
          ## assemble new table
          res.comb <- data.frame(id=res.id, res.test, res.exprs, stringsAsFactors=F)
          ##res.comb <- res.comb[tab[, id.col], ]
          res.comb <- res.comb[rownames(tab),]
        }
      }
    })
  }
  
  
  ################################################################################
  #One sample mod T
  ################################################################################
  
  if(input$select_test== "One-sample Moderated T-test"){
    
    for (ome_name in chosen_omes_reactive()) {
      count=0
      
      ome_gct <- GCTs()[[ome_name]]
      ome_data <- ome_gct@mat
      tab <- as.data.frame(ome_data)
      
      rdesc <- ome_gct@rdesc
      cdesc <- ome_gct@cdesc
      
      id.col <- names(Filter(function(col) !is.numeric(col), rdesc))[1]
      tab <- cbind(rdesc[[id.col]], tab)
      colnames(tab)[1] <- id.col
      
      res.comb <- tab
      
      for (group_name in chosen_groups_reactive()){
        print(paste('running',ome_name,group_name))
        
        sample_names <- colnames(ome_data) 
        groups <- cdesc[sample_names, group_name]
        names(groups) <- sample_names
        
        tab.group <- cbind(tab[[id.col]], tab[, sample_names])
        colnames(tab.group)[1] <- id.col
        
        res.tmp <- modT.test(tab.group, id.col=id.col, plot=F, nastrings=NASTRINGS, label=group_name, na.rm=FALSE)$output
        
        #create data frame of expression values and test results
        res.test <- res.tmp[, !colnames(res.tmp)%in%colnames(res.comb)]
        res.comb <- base::merge(res.comb,res.test,by="row.names",all=T)
        rownames(res.comb) <- res.comb[,1]
        res.comb <- res.comb[,-1]
        
        print('got to here 1')
        ##################################    
        ## add FC before normalization
        fc.before.norm <- calculate_fc(tab,groups, group_name,input$select_test)
        #add rownames
        rownames(fc.before.norm) <- rownames(tab)
        
        print('got to here 1.5')
        res.comb <- merge(res.comb, fc.before.norm,by="row.names")
        rownames(res.comb) <- res.comb[,1]
        res.comb <- res.comb[,-1]
        
        print('got to here 2')
        ##################################
        ## reorder columns of the table
        res.id <- res.comb$id ## id column
        res.exprs <- res.comb[, names(groups)] ## expression values
        res.test <- res.comb[, grep('^logFC\\.|^AveExpr\\.|^t\\.|^P\\.Value\\.|^adj\\.P\\.Val\\.|^Log\\.P\\.Value\\.|^RawlogFC\\.|^RawAveExpr\\.', colnames(res.comb))] ## test results
        res.test <- res.test[, order(colnames(res.test))]
        
        print('got to here 3')
        ## assemble new table
        res.comb <- data.frame(id=res.id, res.test, res.exprs, stringsAsFactors=F)
        ##res.comb <- res.comb[tab[, id.col], ]
        res.comb <- res.comb[rownames(tab),]
        
        print(res.comb)
      }
    }
  }
})


#################STARTING OVER###############################################
stat.testing <- function (test, annotation_col, chosen_omes, gct, chosen_groups, selected_contrasts, p.value.alpha = 0.05, use.adj.pvalue = TRUE, apply.log=FALSE, intensity, ...) {
  
  ################################################################################
  #Mod F Test
  ################################################################################
  if(test == 'Moderated F test'){
    withProgress(message='moderated F-test', value=0, {
      results_list <- list()  # store results by ome
      for (ome_name in chosen_omes) {
        
        incProgress(1/length(chosen_omes), detail = paste("Processing", ome_name))
        
        ome_data <- gct[[ome_name]]@mat
        rdesc <- gct[[ome_name]]@rdesc
        cdesc <- gct[[ome_name]]@cdesc
        tab <- as.data.frame(ome_data)
        
        #Add ID column to tab
        id.col <- names(Filter(function(col) !is.numeric(col), rdesc))[1]
        tab <- cbind(rdesc[[id.col]], tab)
        colnames(tab)[1] <- id.col
        
        #Run test on only the chosen groups
        sample_names <- colnames(ome_data) 
        all_groups <- cdesc[sample_names, annotation_col, drop=TRUE]
        keep_samples_logical <- all_groups %in% chosen_groups
        samples_to_keep <-sample_names[keep_samples_logical] #run test on only the chosen groups
        groups <- all_groups[match(samples_to_keep, sample_names)]
        
        tab.group <- cbind(tab[[id.col]], tab[, samples_to_keep])
        colnames(tab.group)[1] <- id.col
        
        #MOD F LOGIC
        id <- tab.group[,id.col]
        data <- tab.group[, setdiff (colnames (tab.group), id.col)]
        cat('\n-- modF.test --\n')
        
        f <- factor (groups)
        if (length(levels(f)) < 2) {
          message(paste("Skipping", ome_name, "- not enough of the selected groups found in the data."))
          next
        }
        
        # tryCatch({
        design <- model.matrix ( ~ 0 + f )
        
        #use row centered data -- this does not affect 2-sample t test results, but makes the F test more interpretable 
        data.rownorm <- sweep (data, MARGIN=1, STATS=apply (data, 1, mean, na.rm=TRUE))
        fit <- lmFit (data.rownorm, design)
        
        #check intensity
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
        mod.sig <- if (use.adj.pvalue) sig[, "adj.P.Val"] <= p.value.alpha else sig[, "P.Value"] <= p.value.alpha
        non.na.n <- apply (data, 1, function (x) { sum (is.finite (x)) })
        
        final.results<- data.frame ( cbind (id=id, sig, significant=mod.sig, total.n=non.na.n, Log.P.Value=-10*log(sig[,'P.Value'] ,10)), stringsAsFactors=F )
        colnames(final.results) <- sub("^f", "AveExpr.", colnames(final.results))
        
        #replace zero-centered average with the true average expression
        avg <- t(aggregate(t(data),by=list(groups),function(x) mean(x,na.rm=T)))
        avg <- avg[-1,]
        avg <- matrix(as.numeric(avg),ncol=ncol(avg))
        final.results[,grepl("AveExpr.",colnames(final.results))]<-avg
        final.results[,colnames(final.results)=="AveExpr"]<-rowMeans(avg,na.rm=T)
        
        cat('\n-- modF.test exit --\n')
        results_list[[ome_name]]<-final.results
      }
    })
  }
  
  ################################################################################
  #One sample Mod T Test
  ################################################################################
  if(test == 'One-sample Moderated T-test'){
    withProgress(message='one-sample moderated T-test', value=0, {
      results_list <- list()  # store results by ome
      
      for (ome_name in chosen_omes) {
        combined_results<-NULL
        
        ome_data <- gct[[ome_name]]@mat
        rdesc <- gct[[ome_name]]@rdesc
        cdesc <- gct[[ome_name]]@cdesc
        tab <- as.data.frame(ome_data)
        
        id.col <- names(Filter(function(col) !is.numeric(col), rdesc))[1]
        tab <- cbind(rdesc[[id.col]], tab)
        colnames(tab)[1] <- id.col
        
        for (group_name in chosen_groups){
          incProgress(1 / (length(chosen_omes) * length(chosen_groups)), detail = paste("Processing", ome_name, "-", group_name))
          
          #run test on chosen groups only
          sample_names <- colnames(ome_data) 
          all_groups <- cdesc[sample_names, annotation_col, drop=TRUE]
          keep_samples_logical <- all_groups %in% group_name
          samples_to_keep <-sample_names[keep_samples_logical] #run test on one group at a time
          groups <- all_groups[match(samples_to_keep, sample_names)]
          
          tab.group <- cbind(tab[[id.col]], tab[, samples_to_keep])
          colnames(tab.group)[1] <- id.col

          cat('\n-- modT.test --\n')
          id <- tab.group[, id.col]
          data <- tab.group[, setdiff (colnames (tab.group), id.col)]
          
          data <- data.matrix(data)
          
          # log transform is required
          if (apply.log) data <- log2 (data)
          
          #MOD T LOGIC
          data.matrix <- data.frame (data, stringsAsFactors=F)
          m <- lmFit (data.matrix, method='robust')
          m <- eBayes (m, trend=FALSE, robust=TRUE)#one-sample t-test is only run for ratio data
          sig <- topTable (m, number=nrow(data), sort.by='none')
          
          ##View(data)
          if (use.adj.pvalue) mod.sig <- sig [,'adj.P.Val'] <= p.value.alpha
          else  mod.sig <- sig [,'P.Value'] <= p.value.alpha
          # change <- apply (data, 1,
          #                  function (x) {
          #                    x <- x [is.finite (x)]
          #                    ret.value <- '?'
          #                    if ( all (x < 0) ) ret.value <- 'down'
          #                    else if ( all (x > 0)) ret.value <- 'up'
          #                    return (ret.value)
          #                  })
          
          ##MOD T test result
          mod.t.result <- data.frame(sig, significant=mod.sig, Log.P.Value=-10*log(sig$P.Value,10), stringsAsFactors=F)
          
          ##add label(group_name)
          if(!is.null(group_name))
            colnames(mod.t.result) <- paste(colnames(mod.t.result), group_name, sep='.')
          
          mod.t <- data.frame ( cbind (data.frame (id=id), mod.t.result), stringsAsFactors=F )
          rownames(mod.t) <- id
          
          cat('\n-- modT.test exit --/n')
          
          # Keep only id + renamed stats
          mod.t.sub <- mod.t[, c("id", grep(paste0("\\.", group_name, "$"), colnames(mod.t), value = TRUE))]
          
          # Merge into the combined table for this ome
          if (is.null(combined_results)) {
            combined_results <- mod.t.sub
          } else {
            combined_results <- merge(combined_results, mod.t.sub, by = "id", all = TRUE)
          }
          
        }
        results_list[[ome_name]]<-combined_results
      }
    })
  }
  
  ################################################################################
  #Two sample Mod T Test
  ################################################################################
  if(test == 'Two-sample Moderated T-test'){
    withProgress(message='two-sample moderated T-test', value=0, {
      results_list <- list()
      
      for (ome_name in chosen_omes) {
        combined_results<-NULL
        
        ome_data <- gct[[ome_name]]@mat
        rdesc <- gct[[ome_name]]@rdesc
        cdesc <- gct[[ome_name]]@cdesc
        tab <- as.data.frame(ome_data)
        
        #Add ID column to tab
        id.col <- names(Filter(function(col) !is.numeric(col), rdesc))[1]
        tab <- cbind(rdesc[[id.col]], tab)
        colnames(tab)[1] <- id.col

        for (contrast_name in selected_contrasts){
          
          group1 <- contrast_name[1]  
          group2 <- contrast_name[2]  
          contrast_name <- paste0(group1, "_vs_", group2)
  
          incProgress(1 / (length(chosen_omes) * length(selected_contrasts)), detail = paste("Processing", ome_name, "-", contrast_name))
          sample_names <- colnames(ome_data)
          all_groups <- cdesc[sample_names, annotation_col, drop=TRUE]
          keep_samples_logical <- all_groups %in% c(group1,group2)
          samples_to_keep <-sample_names[keep_samples_logical] #run test on only the chosen groups
          
          groups <- all_groups[match(samples_to_keep, sample_names)]

          tab.group <- cbind(tab[[id.col]], tab[, samples_to_keep])
          colnames(tab.group)[1] <- id.col

          #MOD T LOGIC- TWO SAMPLE
          cat('\n-- modT.test.2class --\n')

          ## store group names
          groups <- factor(groups, levels = c(group1, group2))
          id <- tab.group[,id.col]
          data <- tab.group[, setdiff (colnames (tab.group), id.col)]
          
          ## moderated t test for 2 classes
          design.mat <- cbind (ref=1, comparison=as.numeric(groups))
          data.matrix <- data.frame (data, stringsAsFactors=F)

          if(!is.null(design.mat)){
            m <- lmFit (data.matrix, design.mat)
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
            sig <- topTable (m, coef=colnames (design.mat)[2], number=nrow(data), sort.by='none')
          }

          mod.t.result <- data.frame(sig, Log.P.Value=-10*log(sig$P.Value,10), stringsAsFactors=F)

          ##add label(contrast_name)
          if(!is.null(contrast_name))
            colnames(mod.t.result) <- paste(colnames(mod.t.result), contrast_name, sep='.')

          final.results <- data.frame ( cbind (data.frame (id=id), mod.t.result), stringsAsFactors=F )
          rownames(final.results) <- id
          
          cat('\n-- modT.test.2class exit--\n')
          
          # Merge into the combined table for this ome
          if (is.null(combined_results)) {
            combined_results <- final.results
          } else {
            combined_results <- merge(combined_results, final.results, by = "id", all = TRUE)
          }

        }
        results_list[[ome_name]]<-combined_results
      }
    })
  }
  
  #Return the final results table from the chosen test
  return(results_list)
}