################################################################################
# Module: Stat_Plot
#
# Allow users to see the Volcano plot of their results
################################################################################
# 
# plotVolcano <- function(group, interactors=NULL, 
#                         verbose=T, cex.axis=1.5, cex.lab=1.5, cex.leg=1.2, cex.main=1.8,
#                         sig.pch=21,
#                         bg.pch=21,
#                         sig.col='darkred',
#                         bg.col='black'){
#   
#   if(verbose)
#     cat('\n-- plotVolcano --\n')
#   #if(!is.null(error$msg)) return()
#   
#   ## hyperbolic curve filter?
#   hyperbol <- input[[paste('ppi.hyper.filt', group, sep='.' )]]
#   
#   ## maximal log10 p-value
#   max.logP <- input[[paste('max.logP', group, sep='.')]]
#   
#   ## pch for significant points
#   #sig.pch=23
#   
#   ## vectors for selected points/PP interactors
#   volc.add.X <- volc.add.Y <- volc.add.text <- volc.add.col <- c()
#   
#   ## unfiltered data set
#   res = as.data.frame( global.results$data$output )
#   
#   
#   ## #############################
#   ## p-values
#   if(global.param$which.test != 'mod F'){
#     logPVal <- res[, paste('Log.P.Value.', group, sep='')]
#   } else {
#     logPVal <- res[, paste('Log.P.Value',  sep='')]
#   }
#   
#   # ###############################
#   # adjusted p-values
#   adjPVal <- res[, paste('adj.P.Val.', group, sep='')]
#   PVal <- res[, paste('P.Value.', group, sep='')]
#   
#   ## ##############################
#   ## log fold change
#   logFC <- res[, paste('logFC.', group, sep='')]
#   
#   ## ##############################
#   ## ids
#   IDs <- res[ , 'id.concat']
#   
#   ## ###################################################
#   ##             use IDs as vector names
#   names(logPVal) <- names(adjPVal) <- names(logFC) <- names(PVal) <- IDs
#   
#   ## index of missing values
#   rm.idx <- union( which(is.na(logFC)), which(is.na(logPVal)) )
#   
#   if(length(rm.idx) > 0){
#     res <- res[-rm.idx, ]
#     logFC <- logFC[-rm.idx]
#     logPVal <- logPVal[-rm.idx]
#     PVal <- PVal[-rm.idx]
#     IDs <- IDs[-rm.idx]
#     adjPVal <- adjPVal[ -rm.idx ]
#   }
#   ## store a copy of IDs before zoom
#   IDs.all <- IDs
#   
#   ##change IDs depending on label
#   ##needed to plot PPI interactors in the same style
#   if(input[[paste('volc.label', group, sep='.')]]=="ID_Symbol" & !is.null(global.results$id.map ) ){
#     IDs <- res[ , 'id.concat']
#   }else if(input[[paste('volc.label', group, sep='.')]]=="Symbol" & !is.null(global.results$id.map ) ){
#     IDs <- res[ , 'id.mapped']
#   }else{
#     IDs <- res[ , 'id']
#   }
#   
#   ## which filter has been used?
#   filter.str <- paste('filter:', global.param$filter.type, '\ncutoff:', global.param$filter.value)
#   
#   ## ###################################################################
#   ##                 set maximal log p value
#   ## ###################################################################
#   if(!is.null( max.logP))
#     logPVal[which(logPVal > max.logP)] <- max.logP
#   
#   ## ###################################################################
#   ##                         limits
#   ## ###################################################################
#   xlim = max(abs(logFC), na.rm=T)
#   xlim = xlim + xlim*.1
#   
#   ylim = ifelse(is.null(max.logP), max(logPVal, na.rm=T), max.logP)
#   ylim = ylim + .2*ylim
#   
#   ## ###################################################################
#   ##                    hyperbolic curve
#   ## ###################################################################
#   if( hyperbol ){
#     
#     x0 <- as.numeric(input[[paste( "ppi.min.fc", group, sep='.')]])
#     c <- as.numeric(input[[paste( "ppi.curve", group, sep='.')]])
#     
#     y.hc=function(x, x0, y0, c) return(c/(x-x0) + y0)
#     x.hc <- seq(x0, xlim, 0.1)
#   }
#   
#   ######################################################################
#   ## extract significant proteins of current group/test
#   ######################################################################
#   ## one/two sample
#   if( global.param$which.test != 'mod F'){
#     if(global.param$filter.type == 'top.n'){
#       #PVal <- res[, paste('P.Value.', group, sep='')]
#       sig.idx = order(PVal, decreasing=F)[1:global.param$filter.value]
#       
#     }
#     if(global.param$filter.type == 'nom.p'){
#       #PVal <- res[, paste('P.Value.', group, sep='')]
#       sig.idx = which(PVal < global.param$filter.value)
#     }
#     ## ##################################
#     ## adjusted p
#     if(global.param$filter.type == 'adj.p'){
#       
#       ## hyperbol
#       if(hyperbol){
#         #adjPVal <- res[, paste('adj.P.Val.', group, sep='')]
#         
#         sig.idx = which(adjPVal < global.param$filter.value)
#         #names(sig.idx) <- IDs[sig.idx]
#         
#         y0 <- min(logPVal[names(sig.idx)], na.rm=T)
#         
#         sig.idx <- which( logPVal > y.hc( abs(logFC), x0=x0, y0=y0, c=c) & abs(logFC) > x0 )
#         
#         ## adjusted p only
#       } else {
#         #adjPVal <- res[, paste('adj.P.Val.', group, sep='')]
#         sig.idx = which(adjPVal < global.param$filter.value)
#         #names(sig.idx) <- IDs[sig.idx]
#         
#       }
#     }
#     ######################################
#     ## F-test
#     ## } else {
#     ##     if(global.param$filter.type == 'top.n'){
#     ##         PVal <- res[, paste('P.Value', sep='')]
#     ##         sig.idx = order(PVal, decreasing=F)[1:global.param$filter.value]
#     ##     }
#     ##     if(global.param$filter.type == 'nom.p'){
#     ##         PVal <- res[, paste('P.Value', sep='')]
#     ##         sig.idx = which(PVal < global.param$filter.value)
#     ##     }
#     ##     if(global.param$filter.type == 'adj.p'){
#     ##         adjPVal <- res[, paste('adj.P.Val', sep='')]
#     ##         sig.idx = which(adjPVal < global.param$filter.value)
#     ##     }
#   }
#   
#   if(global.param$filter.type == 'none')
#     sig.idx <- c()
#   ##sig.idx = 1:length(logFC)
#   
#   ## use IDs as names
#   if(length(sig.idx) >0 )
#     names(sig.idx) <- IDs[sig.idx]
#   
#   sig.idx.all <- sig.idx
#   
#   ## ####################################################
#   ## PPI?
#   ppi.bait <- input[[ gsub('\\.', '', paste0('ppi.bait.', group)) ]]
#   PPI <- toupper(ppi.bait) %in% toupper(IDs.all)
#   
#   ## #################################################
#   ##
#   ##              pch and cex
#   ##
#   ## ##################################################
#   pch.vec=rep(bg.pch, nrow(res))
#   cex.vec=rep( input[[paste('cex.volcano', group, sep='.')]], nrow(res))
#   
#   if(length(sig.idx) > 0){
#     pch.vec[ sig.idx ] <- sig.pch
#   }
#   
#   #############################
#   ## color gradient
#   if(PPI){
#     opac1 = 0.3
#     opac2 = 0.2
#   } else{
#     opac1 = 0.3
#     opac2 = 0.2
#   }
#   
#   col <- rep(my.col2rgb(bg.col, alpha = 255*opac1), length(IDs))
#   col[sig.idx] <- my.col2rgb(sig.col, alpha = 255*opac1)
#   
#   col.opac <- rep(my.col2rgb(bg.col, alpha = 255*opac2), length(IDs))
#   col.opac[sig.idx] <- my.col2rgb(sig.col, alpha = 255*opac2)
#   
#   
#   ## ########################################################
#   ##
#   ##                Query PPI databases
#   ##
#   ## ##########################################################
#   
#   ## ##############################################
#   ##
#   ## if the bait was detected
#   ##
#   ## ###############################################
#   
#   if(PPI) {
#     ppi.bait <-  toupper(sub('.*_(.*)$', '\\1', ppi.bait))
#     #debug(get.interactors)
#     # extract interactors
#     ppi.map <- get.interactors( ppi.bait=ppi.bait,
#                                 IDs=IDs.all,
#                                 sig.idx=sig.idx.all,
#                                 db=input[[ paste0('ppi.db.', group) ]],
#                                 ppi.db=ppi,
#                                 ppi.db.col=ppi.db.col
#     )
#     
#     ## ################################
#     ## extract results
#     ppi.int.idx <- ppi.map$ppi.int.idx
#     
#     leg <- ppi.map$leg
#     leg.col <- ppi.map$leg.col
#     ppi.col <- ppi.map$ppi.col
#     
#     ppi.bait.idx <- ppi.map$ppi.bait.idx
#     if(length(ppi.bait.idx) > 0)
#       ppi.col[ ppi.bait.idx ] <- 'green'
#     
#     ppi.int.vec <- rep(FALSE, length(IDs))
#     ppi.int.vec[ppi.int.idx] <- TRUE
#     
#     ## udpate color vector
#     if( sum(nchar(ppi.col) > 0) > 0){
#       col[ nchar(ppi.col) > 0] <- ppi.col[ nchar(ppi.col) > 0]
#       col.opac[ nchar(ppi.col) > 0] <- ppi.col[ nchar(ppi.col) > 0 ]
#     }
#     
#     
#     ## ###############################
#     ## plot if there are interactors
#     if(length(ppi.int.idx) > 0) {
#       ## labels
#       if(input[[paste('ppi.show.labels', group, sep='.')]]){
#         
#         volc.add.X <- c(volc.add.X, logFC[ppi.int.idx])
#         volc.add.Y <- c(volc.add.Y, logPVal[ppi.int.idx])
#         volc.add.text <- c( volc.add.text, as.character(IDs[ppi.int.idx]))
#         volc.add.col <- c(volc.add.col, col[ppi.int.idx])
#       }
#     }
#   }
#   
#   ## ##################################################################
#   ##
#   ##                   zoomed vs not zoomed
#   ##
#   ## ##################################################################
#   if( is.null( volc.brush[[ paste('xmin', group, sep='.') ]] ) ){
#     xlim = c(-xlim, xlim)
#     ## y-limits
#     ylim = c(0, ylim)
#     
#   } else {
#     
#     ## ##################################
#     ##        zoomed
#     ## x-axis
#     xlim = c(volc.brush[[ paste('xmin', group, sep='.') ]], volc.brush[[ paste('xmax', group, sep='.') ]])
#     logFC[ logFC < xlim[1] | logFC > xlim[2] ] <-  NA
#     
#     ## y-axis
#     ylim = c( max(0, volc.brush[[ paste('ymin', group, sep='.') ]]), volc.brush[[ paste('ymax', group, sep='.') ]])
#     logPVal[ logPVal < ylim[1] | logPVal > ylim[2] ] <-  NA
#     
#     ## remove missing  values
#     rm.idx <- union( which(is.na(logFC)), which(is.na(logPVal)) )
#     
#     if(length(rm.idx) > 0){
#       res <- res[-rm.idx, ]
#       logFC <- logFC[-rm.idx]
#       logPVal <- logPVal[-rm.idx]
#       IDs <- IDs[-rm.idx]
#       
#       ## colors, pch, cex
#       col <- col[-rm.idx]
#       col.opac <- col.opac[-rm.idx]
#       pch.vec <- pch.vec[-rm.idx]
#       cex.vec <- cex.vec[-rm.idx]
#       
#       ## update PPI stuff
#       if(PPI){
#         ppi.int.vec <- ppi.int.vec[-rm.idx]
#         ppi.int.idx <- which(ppi.int.vec)
#         ppi.bait.idx <- which(toupper(IDs) == toupper(ppi.bait))
#         ppi.bait.idx <- which(toupper(sub('.*_(.*)$', '\\1', IDs) ) == toupper(ppi.bait))
#         
#       }
#       
#       ## update index of significant stuff
#       sig.idx <- sig.idx[ !(sig.idx %in% rm.idx) ]
#     }
#   }
#   
#   
#   ## ##################################################
#   ##               set up the plot
#   ## ##################################################
#   par(mar=c(4,5,5,2))
#   plot.new()
#   plot.window( xlim=xlim, ylim=ylim, cex.axis=cex.axis, cex.lab=cex.lab)
#   ## title
#   mtext(group, side=3, cex=cex.main, line=2)
#   ## label axes
#   if(global.param$which.test == 'Two-sample mod T'){
#     mtext( paste("log(", sub('.*\\.vs\\.', '', group), "/", sub('\\.vs.*', '', group),")"), side=1, cex=cex.axis, line=3)
#   }else{
#     mtext(expression(log(FC)), side=1, cex=cex.axis, line=3)
#   }
#   # if(global.param$filter.type=="adj.p"){
#   #   mtext(expression(-10*log[10](adj.p-value)), side=2, cex=cex.axis, line=3)
#   # }else{
#   #   mtext(expression(-10*log[10](p-value)), side=2, cex=cex.axis, line=3)
#   # }
#   mtext(expression(-10*log[10](p-value)), side=2, cex=cex.axis, line=3)
#   ## draw axes
#   axis(1, cex.axis=cex.axis)
#   axis(2, las=2, cex.axis=cex.axis)
#   ## grid
#   if( input[[paste('grid.volcano', group, sep='.')]] )
#     grid()
#   
#   ## actual plot
#   points(logFC, logPVal, col=col, bg=col.opac, pch=pch.vec, cex=cex.vec, lwd=2)
#   
#   
#   ## #######################################
#   ## hyperbolic curve
#   if( hyperbol & global.param$filter.type == 'adj.p'){
#     lines( x.hc, y.hc(x.hc, x0, y0, c), col='grey30', lty='dashed')
#     lines(-x.hc, y.hc(x.hc, x0, y0, c), col='grey30', lty='dashed')
#     text( xlim[2]-(xlim[2]*.2), y0, paste(global.param$filter.type, "=", global.param$filter.value, ",", "logFC", "=", input[[paste( "ppi.min.fc", group, sep='.')]], sep=''), pos=1, col='grey30')
#   }
#   
#   ## ###################################
#   ## add filter
#   ## minimal log P-value for given filter
#   if(length(sig.idx) > 0 & !(input[[paste('ppi.hyper.filt', group, sep='.' )]])){
#     
#     filt.minlogPVal <- min(logPVal[names(sig.idx)], na.rm=T)
#     
#     abline(h=filt.minlogPVal, col=my.col2rgb('grey30', 50), lwd=2, lty='dashed')
#     text( xlim[2]-(xlim[2]*.1), filt.minlogPVal, paste(global.param$filter.type, global.param$filter.value, sep='='), pos=1, col='grey30')
#   }
#   
#   ## number of significant
#   legend(ifelse(PPI, 'topright', 'top'), bty='n', legend=paste(filter.str, '\nsig / tot: ', length(sig.idx),' / ', sum(!is.na(logFC) & !is.na(logPVal)), sep=''), cex=cex.leg)
#   
#   ## ############################
#   ## indicate directionality for two-sample tests
#   if(global.param$which.test == 'Two-sample mod T'){
#     mtext(sub('\\.vs.*', '', group), side=3, line=1, at=(xlim[1]+abs(xlim[1])*0.05), cex=cex.main, col='darkblue')
#     mtext(sub('.*\\.vs\\.', '', group), side=3, line=1, at=(xlim[2]-abs(xlim[2])*0.05), cex=cex.main, col='darkblue')
#   }
#   
#   
#   ## ###########################################################
#   ##
#   ##               selected points
#   ## ###########################################################
#   if(!is.null( volc[[paste('x', group, sep='.')]] ) & length(volc[[paste('x', group, sep='.')]]) ){
#     
#     for(i2 in 1:length( unlist( volc[[paste('x', group, sep='.')]]))){
#       
#       volc.add.X[i2] <- as.numeric( unlist( volc[[paste('x', group, sep='.')]][i2] ) )
#       volc.add.Y[i2] <- as.numeric( unlist( volc[[paste('y', group, sep='.')]][i2] ) )
#       volc.add.text[i2] <- as.character( unlist( volc[[paste('text', group, sep='.')]][i2]) )
#       volc.add.col[i2] <- 'black'
#     }
#   }
#   
#   ## ###########################################################
#   ##
#   ##                 plot PPI stuff
#   ##
#   ## ###########################################################
#   if(PPI){
#     if(length(ppi.int.idx) > 0)
#       points(logFC[ppi.int.idx], logPVal[ppi.int.idx], col=col[ppi.int.idx], bg=col[ppi.int.idx], pch=pch.vec[ppi.int.idx], cex=cex.vec[ppi.int.idx])
#     
#     if(length(ppi.bait.idx) > 0)
#       points(logFC[ppi.bait.idx], logPVal[ppi.bait.idx], col='green', bg='green', pch=pch.vec[ppi.bait.idx], cex=cex.vec[ppi.bait.idx])
#     
#     leg <- c(paste(ppi.bait), leg)
#     leg.col <- c('green', leg.col)
#     legend('topleft', legend=leg, col=leg.col, pch=20, cex=cex.leg, title=ifelse( length(ppi.int.idx) > 0 ,paste('interactors sig/det/tot') , ''), pt.cex=cex.vec[1])
#     
#   }
#   
#   ## ########################################
#   ##  draw ids of selected points
#   if(length(volc.add.X) > 0)
#     pointLabel(as.numeric(unlist(volc.add.X)), as.numeric(unlist(volc.add.Y)), labels=as.character(unlist(volc.add.text)), col=volc.add.col, offset=20, method='SANN', cex=input[[paste('cex.volcano.lab', group, sep='.')]])
#   
# } ## end plotVolcano

################################################################################
##RESTARTING FUNCTION
################################################################################
plotVolcano <- function(ome, verbose=TRUE, sig.col='darkred', bg.col='black', sig.size = 3, bg.size = 2){
  if(verbose) cat('\n-- plotVolcano --\n')
  
  req(stat_param())
  req(stat_results())
  
  df<- stat_results()[[ome]]
  
  ##LOG FC COLUMN##
  if (stat_param()[[ome]]$test == "One-sample Moderated T-test") {
    req(input$volcano_groups)
    keyword <- input$volcano_groups
    logfc_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "logFC.", ")")
  } else if (stat_param()[[ome]]$test == "Two-sample Moderated T-test") {
    req(input$volcano_contrasts)
    groups <- unlist(strsplit(input$volcano_contrasts, " / "))
    logfc_pattern <- paste0("(?i)(?=.*", groups[1], ")(?=.*", groups[2], ")(?=.*", "logFC.", ")")
  } 
  
  logFC_col <- grep(logfc_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  ##LOG P VALUE COLUMN##
  if (stat_param()[[ome]]$test == "One-sample Moderated T-test") {
    req(input$volcano_groups)
    keyword <- input$volcano_groups
    logP_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "Log.P.Value.", ")")
  } else if (stat_param()[[ome]]$test == "Two-sample Moderated T-test") {
    req(input$volcano_contrasts)
    groups <- unlist(strsplit(input$volcano_contrasts, " / "))
    logP_pattern <- paste0("(?i)(?=.*", groups[1], ")(?=.*", groups[2], ")(?=.*", "Log.P.Value.", ")")
  } 
  
  logP_col <- grep(logP_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  ##ADJ P VALUE COLUMN##
  if (stat_param()[[ome]]$test == "One-sample Moderated T-test") {
    req(input$volcano_groups)
    keyword <- input$volcano_groups
    adjP_pattern <- paste0("(?i)(?=.*", keyword, ")(?=.*", "adj.P.Val.", ")")
  } else if (stat_param()[[ome]]$test == "Two-sample Moderated T-test") {
    req(input$volcano_contrasts)
    groups <- unlist(strsplit(input$volcano_contrasts, " / "))
    adjP_pattern <- paste0("(?i)(?=.*", groups[1], ")(?=.*", groups[2], ")(?=.*", "adj.P.Val.", ")")
  } 
  
  adjP_col <- grep(adjP_pattern, colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  ##ID COLUMN##
  id_col <- grep("id", colnames(df), value = TRUE, perl = TRUE, ignore.case = TRUE)[1]
  
  ## Check columns exist
  required_cols <- c(logFC_col, logP_col, adjP_col, id_col)
  if(!all(required_cols %in% colnames(df))) {
    stop("Some required columns are missing in the result data.")
  }
  
  ## Keep only complete cases
  df <- df[complete.cases(df[, required_cols]), ]
  
  ## Add columns for plotting
  df$logFC <- df[[logFC_col]]
  df$logP  <- df[[logP_col]]
  df$adj.P.Val <- df[[adjP_col]]
  df$id <- df[[id_col]]
  
  ## Define significance based on adj.P.Val threshold
  sig_cutoff <- stat_param()[[ome]]$cutoff
  sig_stat <- stat_param()[[ome]]$stat
  
  if(sig_stat == "adj.p.val") {
    df$Significant <- df$adj.P.Val < sig_cutoff
  } else if(sig_stat == "nom.p.val") {
    df$Significant <- df$P.Value < sig_cutoff
  } else {
    df$Significant <- FALSE
  }
  
  ## Plot
  volcano <- ggplot(df, aes(x = logFC, y = logP, 
                       color = Significant, 
                       text = paste("ID:", id, "<br>logFC:", round(logFC,2), "<br>logP:", round(logP,2)))) +
    geom_point(aes(size = Significant)) +
    scale_color_manual(values = c(`TRUE` = sig.col, `FALSE` = bg.col)) +
    scale_size_manual(values = c(`TRUE` = sig.size, `FALSE` = bg.size)) +
    labs(title = "Volcano plot", x = "log2 Fold Change", y = expression(-log[10](p-value))) +
    theme_minimal()
  
  ggplotly(volcano, tooltip = "text")
}