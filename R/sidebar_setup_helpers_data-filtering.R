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

  ## ##########################################
  ## get expression data
  ids=tab[, id.col]
  tab=data.matrix(tab[, names(grp.vec)])

  ## #########################################
  ## calculate sd across all measurements
  ## Use vectorized row SD: sqrt of variance computed via rowMeans
  n_cols <- ncol(tab)
  row_means <- rowMeans(tab, na.rm = TRUE)
  # Count non-NA values per row for correct SD denominator
  n_valid <- rowSums(!is.na(tab))
  # Compute sum of squared deviations using vectorized operations
  tab_centered <- tab - row_means
  ss <- rowSums(tab_centered^2, na.rm = TRUE)
  sd.tab <- sqrt(ss / (n_valid - 1))
  # Handle rows with 0 or 1 valid values (sd is NA)
  sd.tab[n_valid <= 1] <- NA_real_

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
