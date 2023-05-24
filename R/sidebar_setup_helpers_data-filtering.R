################################################################################
# Module: SETUP SIDEBAR
# 
# This script contains (slightly modified) functions from the original Protigy 
# app to perform data filtering (reproducibility and standard deviation). 
# Original code written by Karsten Krug and D R Mani.
#
# Modifications include:
# 1. removing the id.col parameter and any related code....the main input is now
#    a single data matrix
# 2. Using roxygen import tags for dependencies instead of p_load()
################################################################################


########################################################################
## 20160224
##                   reproducibility filter
##
## n=2: Bland-Altman
## n>2: lmm-model written by DR Mani
##
## - replaces not reproducibly measuered values in 'tab' with 'NA'
## - done separately for each group in 'grp.vec'
##
########################################################################
my.reproducibility.filter <- function(tab, grp.vec, id.col='id', alpha=0.05){
  
  # add in the id column if it doesn't exist
  if (!(id.col %in% names(tab))) {
    tab <- data.frame(tab)
    tab[[id.col]] <- rownames(tab)
  }
  
  alpha <- as.numeric(alpha)
  
  ## extract groups
  groups <- unique(grp.vec)
  
  ## list to store index of filtered values per group
  values.filt <- vector('list', length(groups))
  names(values.filt) <- groups
  
  ## add rownames to tab
  ##rownames(tab) <- tab[, id.col]
  
  ##tab.repro.filter <- tab
  ## View(tab.repro.filter)
  
  ############################################
  ## loop over replicate groups
  for(gg in groups){
    
    gg.idx = names(grp.vec)[ which(grp.vec == gg) ]
    
    ########################################
    ## if there are more than 2 replicates
    ## use the Mani's lmm model
    if( length(gg.idx) > 2 ){
      repro.idx <- reproducibility.filter( tab[, c(id.col, gg.idx)], id.col=id.col, alpha=alpha)
      
      if(length(repro.idx) != nrow(tab)) stop('Reproducibility vector not of same length as matrix!\n')
      
      not.repro.idx <- which(!repro.idx)
      
      if(length(not.repro.idx) > 0)
        tab[not.repro.idx, gg.idx] <- NA
      
      values.filt[[gg]] <- rownames(tab)[ not.repro.idx ]#not.repro.idx
    }
    ########################################
    ## if there are two replicates use
    ## Blandt-Altmann filter
    ## R-package 'BlandAltmanLeh'
    if( length(gg.idx) == 2 ){
      
      ## Bland-Altman
      ##ba <-  bland.altman.stats(as.numeric( as.character( tab[, gg.idx[1] ]) ), as.numeric( as.character( tab[,  gg.idx[2] ] )), two=3.290527 )
      ba <-  bland.altman.stats(as.numeric( as.character( tab[, gg.idx[1] ]) ), as.numeric( as.character( tab[,  gg.idx[2] ] )), two=qnorm(1-alpha/2) )
      ## calculate diffs on my own..
      my.diffs <- tab[, gg.idx[1]] - tab[, gg.idx[2]]
      ## index of outliers
      ##not.repro.idx <- which( ba$diffs < ba$lower.limit | ba$diffs > ba$upper.limit)
      not.repro.idx <- which( my.diffs < ba$lower.limit | my.diffs > ba$upper.limit)
      
      ## set values of outliers to NA
      if(length(not.repro.idx) > 0)
        tab[not.repro.idx, gg.idx] <- NA
      
      ## store the results
      values.filt[[gg]] <- rownames(tab)[ not.repro.idx ]
      rm(not.repro.idx)
    }
    
  }
  return(list(table=tab, values.filtered=values.filt))
}


############################################################################################
##
##              Generalized reprodicibility filter for > 2 replicates
##
## written by D R Mani
############################################################################################
#' @importFrom data.table melt setDT
#' @importFrom nlme lme pdIdent
reproducibility.filter <- function (data, id.col='id', alpha=0.05) {
  ##
  ## Reproducibility Filter using lme4
  ## Theory: MethComp book (pp 58-61). Comparing Clinical Measurement Methods by Bendix Carstensen
  ## Implementation: MethComp book pg 142, but don't include item (=id) in the fixed effects
  ## -- this is unnecessary for the application and makes computations very time consuming;
  ## all we really need to assess reproducibility are the variances
  ##
  ## NB: using library (nlme) and lmer is much more convoluted since incorporating the  
  #      var-cov matrix stratified by replicate (=method) is not easy
  #      (see https://stat.ethz.ch/pipermail/r-sig-mixed-models/2007q3/000248.html)
  #      something like:
  #        model <- lmer (y ~ rep + (rep1|id)+(rep2|id)+..., data=data.long)
  #      is possible, but the above does not work for 2 replicates (gives same results for >2 reps)
  #
  
  
  
  d <- data [, setdiff (colnames (data), id.col)]     # data part of input
  data.long <- melt (data.frame (data), id=id.col)    # convert to long format
  colnames (data.long) <- c ('id', 'rep', 'y')
  # keep column order in data so that (i,j) below correctly corresponds to columns
  data.long [,'rep'] <- factor (data.long[,'rep'], levels=colnames (d))  
  
  # exclude missing data points (only missing measurements are removed instead of entire rows)
  data.long <- data.long [ !is.na (data.long[,'y']), ]
  
  # Model: y_mi = a_m + c_mi + e_mi,  c_mi ~ N(0,tau_m^2), e_mi ~ N(0, sigma_m^2)  
  # where m = method and i = item (=id)
  # [Eq 5.2, pg 58, MethComp book]. Also see interpretation of effect on pg 59-61
  model <- lme (y ~ rep, 
                random=list (id=pdIdent(~rep)), 
                weights=varIdent(form=~1|rep), 
                data=data.long)
  n <- nlevels (data.long[,'rep'])
  p <- length (unique (data.long[,'id'])) 
  df <- p - 1    # approx df for confidence interval (p=# of independent items)
  
  rep.all <- rep (TRUE, nrow (d))  # vector summarizing reproducibility of each input id
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      # variance of method_i - method_j: pg 58
      # var (y_i0 - y_j0) = tau_i^2 + tau_i^2 + sigma_i^2 + sigma_j^2
      # where tau is the sd of the between-item variation for a method
      # and sigma is the sd of the within-item variation for the method
      tau <- as.numeric (unlist (VarCorr(model)[c(i,j),'StdDev']))  # returns tau_i and tau_j
      # VarCorr(model)[n+1,'StdDev'] is sigma_1 (ie sigma for method 1)
      # for methods 2-n, coef (model$modelStruct$varStruct, uncons=F, allcoef=T) has the 
      #  multiplying factor to obtain sigma_m using sigma_1
      sigma <- as.numeric (unlist (VarCorr(model)[n+1,'StdDev']))   # 
      sigma1 <- sigma * ifelse (i==1, 1, coef (model$modelStruct$varStruct, uncons=F, allcoef=T)[i-1])
      sigma2 <- sigma * coef (model$modelStruct$varStruct, uncons=F, allcoef=T)[j-1]
      total.sd <- sqrt (tau[1]^2 + tau[2]^2 + sigma1^2 + sigma2^2)
      
      # bias of method_i - method_j: alpha_i - alpha_j 
      alpha1 <- ifelse (i==1, 0, fixef(model)[i])
      alpha2 <- fixef (model)[j]
      bias <- alpha1 - alpha2
      
      # limits of agreement (assuming approx df = p-1)
      t.crit <- qt ( (1-alpha/2), df ) * sqrt ( (p+1) / p )
      ci.low <- bias - t.crit * total.sd
      ci.high <- bias + t.crit * total.sd
      
      # record reproducibility for method_i - method_j
      rep.ij <- (d[,i] - d[,j]) >= ci.low & (d[,i] - d[,j]) <= ci.high
      # if data is missing, assume that data is reproducible
      rep.ij [ is.na (rep.ij) ] <- TRUE
      rep.all <- rep.all & rep.ij
      
      # print bias and LoA for sanity check
      cat ('rep', i, ' - rep', j, ': bias=', bias, ' ci=(', ci.low, ',', ci.high, ')\n', sep='')
    } 
  }
  
  # return reproducibility vector
  return (rep.all)
}
