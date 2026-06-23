################################################################################
# Module: SETUP SIDEBAR
# 
# This script contains the function from the original Protigy 
# app to perform data filtering (standard deviation). 
# Original code written by Karsten Krug and D R Mani.
################################################################################


sd.filter <- function(tab, grp.vec, id.col, sd.perc){
  
  perc <- as.numeric(sd.perc)
  
  ## extract groups
  groups <- unique(grp.vec)
  
  ## list to store index of filtered values per group
  ##values.filt <- vector('list', length(groups))
  ##names(values.filt) <- groups
  
  ## ##########################################
  ## get expression data
  ids=tab[, id.col]
  tab=data.matrix(tab[, names(grp.vec)])
  
  ## #########################################
  ## calculate sd across all measurements
  ## matrixStats::rowSds is the sample (n-1) SD with the same na.rm semantics as
  ## stats::sd, computed in one C-level pass. SD values can differ from the old
  ## apply(sd) by floating-point reduction order (~1e-16); this never changes the
  ## filtered row set (quantile threshold shifts in lockstep). names() are
  ## re-attached so the returned filtered indices carry the same names.
  sd.tab <- matrixStats::rowSds(tab, na.rm = TRUE)
  names(sd.tab) <- rownames(tab)
  
  ## #########################################
  ## determine percentile value used to filter
  sd.perc.val <- quantile(sd.tab, sd.perc/100, na.rm=T)
  
  ## #########################################
  ## index of values to filter
  filt.idx <- which(sd.tab < sd.perc.val)
  not.filt.idx <- which(sd.tab >= sd.perc.val)
  
  ## set filtered values to NA
  tab[filt.idx, ] <- NA
  
  tab <- data.frame(ids, tab)
  colnames(tab)[1] <- id.col
  
  values.filt <- lapply(groups, function(x) filt.idx)
  names(values.filt) <- groups
  
  return( 
    list(
      table=tab, 
      values.filtered=values.filt, 
      sd.perc.val=sd.perc.val
    )
  )
}
