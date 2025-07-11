################################################################################
# Module: Stat_Setup
#
# Allow users to setup the test type and parameters
################################################################################

stat.testing <- function (test, annotation_col, chosen_omes, gct, chosen_groups, selected_contrasts, p.value.alpha = 0.05, use.adj.pvalue = TRUE, apply.log=FALSE, intensity, ...) {
  ################################################################################
  #None
  ################################################################################
  if(test == 'None'){
    results_list<-NULL
  }
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
        
        final.results<- data.frame ( cbind (id=id, sig, significant=mod.sig, total.n=non.na.n, Log.P.Value=-log(sig[,'P.Value'] ,10)), stringsAsFactors=F )
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
        
          ##MOD T test result
          mod.t.result <- data.frame(sig, significant=mod.sig, Log.P.Value=-log(sig$P.Value,10), stringsAsFactors=F)
          
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
            
            sig$significant <- if (use.adj.pvalue) {
              sig$adj.P.Val <= p.value.alpha
            } else {
              sig$P.Value <= p.value.alpha
            }
            
          }

          mod.t.result <- data.frame(sig, Log.P.Value=-log(sig$P.Value,10), stringsAsFactors=F)

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