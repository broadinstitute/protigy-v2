################################################################################
# Module: SETUP SIDEBAR
# 
# This script contains (slightly modified) functions from the original Protigy 
# app and R-utilities to perform data normalization. Original code written by 
# Karsten Krug and D R Mani
#
# Modifications include:
# 1. removing the id.col parameter and any related code....the main input is now
#    a single data matrix
# 2. Using roxygen import tags for dependencies instead of p_load()
# 3. Change instances of `class(x) == "try-error"` to `inherits(x, "try-error")`
################################################################################



#############################################################################################
# Helper function to safely set colnames on apply() results
# Handles the case where apply() returns a vector instead of a matrix
safe_set_colnames <- function(data.norm, original.data) {
  if (is.matrix(data.norm)) {
    colnames(data.norm) <- paste(colnames(original.data), sep='.')
  } else {
    # data.norm is a vector (single-row case), convert back to matrix
    data.norm <- matrix(data.norm, nrow = 1, ncol = length(data.norm))
    colnames(data.norm) <- paste(colnames(original.data), sep='.')
    rownames(data.norm) <- rownames(original.data)
  }
  return(data.norm)
}

normalize.data  <- function(data, # a data matrix
                            method=c('Median',
                                     'Median (non-zero)',
                                     'Quantile', 
                                     'VSN', 
                                     'Median-MAD',
                                     'Median-MAD (non-zero)',
                                     '2-component', 
                                     'Upper-quartile'),
                            grp.vec=NULL  ## if NULL apply global normalization strategy
                            ## if vector of group assingments, normalization will be applied to each group separately  
){
  
  #################################
  ## global normalization
  if(is.null(grp.vec)){
    
    data.norm <- normalize.data.helper(data, method=match.arg(method))
    
    ################################  
    ## group-specific normalization  
  } else { 
    
    ## extract groups
    groups <- unique(grp.vec)
    
    ############################################
    ## loop over replicate groups
    for(gg in groups){
      
      gg.idx = names(grp.vec)[ which(grp.vec == gg) ]
      
      data.group <- data[, gg.idx, drop = FALSE]
      
      data.group.norm <- normalize.data.helper(data.group, method=match.arg(method), per_group = TRUE)
      
      if(gg == groups[1]){
        data.norm <- data.group.norm  
      } else {
        data.norm <- cbind(data.norm, data.group.norm)
      }
    } ## end for
    
  } ## end else
  
  return(data.norm)
}
#############################################################################################
##
##              different normalization methods for expression data
##
## 20160235
#############################################################################################
#' @importFrom preprocessCore normalize.quantiles
#' @importFrom vsn justvsn
normalize.data.helper <- function(data, 
                                  method=c('Median',
                                           'Median (non-zero)',
                                           'Quantile', 
                                           'VSN', 
                                           'Median-MAD',
                                           'Median-MAD (non-zero)',
                                           '2-component', 
                                           'Upper-quartile'),
                                  per_group=FALSE ## for Median & Median-MAD
){
  cat('\n\n-- normalize data --\n\n')
  
  method = match.arg(method)
  
  cat('   normalization method: ', method, '\n')
  
  data <- data.matrix(data)
  
  # Check for single-row matrices - normalization doesn't make sense
  if (nrow(data) == 1) {
    warning("Single-row matrices cannot be meaningfully normalized column-wise. Returning data unchanged.")
    return(data)
  }
  
  ## quantile
  if(method == 'Quantile'){
    data.norm <- normalize.quantiles(data)
    rownames(data.norm) <- rownames(data)
    colnames(data.norm) <- paste( colnames(data))
    
    ## shift median to zero
    ## data.norm <- apply(data.norm, 2, function(x) x - median(x, na.rm=T))
  }
  ## median only
  if(method == 'Median'){
    
    data.norm <- apply(data, 2, function(x) x - median(x, na.rm=T))
    data.norm <- safe_set_colnames(data.norm, data)
    
    if(per_group){
      all_medians <- apply(data, 2, median, na.rm=T)
      data.norm <- data.norm + median( all_medians, na.rm=T)
    }
  }
  ## median plus shifting by medians of medians
  if(method == 'Median (non-zero)'){
    
    all_medians <- apply(data, 2, median, na.rm=T)
    data.norm <- apply(data, 2, function(x) x - median(x, na.rm=T))
    
    data.norm <- data.norm + median( all_medians, na.rm=T )
    data.norm <- safe_set_colnames(data.norm, data)
  }
  ## median & MAD
  if(method == 'Median-MAD'){
    data.norm <- apply(data, 2, function(x) (x - median(x, na.rm=T))/mad(x, na.rm=T) )
    data.norm <- safe_set_colnames(data.norm, data)
    
    if(per_group){
      all_medians <-  apply(data, 2, median, na.rm=T)
      data.norm <- data.norm + median( all_medians, na.rm=T)
    }
  }
  ## median & MAD plus shifting by medians of medians
  if(method == 'Median-MAD (non-zero)'){
    
    all_medians <- apply(data, 2, median, na.rm=T)
    data.norm <- apply(data, 2, function(x) (x - median(x, na.rm=T))/mad(x, na.rm=T) )
    
    data.norm <- data.norm + median( all_medians, na.rm=T )
    data.norm <- safe_set_colnames(data.norm, data)
    
  }
  
  ## 2-component normalization
  if(method == '2-component'){
    
    data.norm.list <- vector('list', ncol(data))
    names(data.norm.list) <- colnames(data)
    
    for(x in colnames(data)){  
      res <- try(two.comp.normalize(data[, x], type="unimodal"))
      data.norm.list[[x]] <- res
      if(inherits(res, 'try-error')) break;
    }
    
    ## check if all runs were successful
    ## return the 'try-error' object to 
    ## catch the error in server.R
    for(i in 1:length(data.norm.list)){
      if(inherits(data.norm.list[[i]], 'try-error')){
        msg <- data.norm.list[[i]]
        return(msg)
      }
    }
    
    ## if 2-comp was successful on all data column convert list to matrix 
    data.norm = matrix( unlist(lapply(data.norm.list, function(x)x$norm.sample)), ncol=length(data.norm.list), dimnames=list(rownames(data), names(data.norm.list)) )
  }
  ## Upper quartile
  if(method == 'Upper-quartile'){
    data.norm <- apply(data, 2, function(x) x - quantile(x, c(0.75),na.rm=T))
    data.norm <- safe_set_colnames(data.norm, data)
  }
  
  ## VSN - variance stabilizing normalization
  if(method == 'VSN'){
    data.norm <- justvsn(data)
  }
  
  cat('\n\n-- normalize data exit--\n\n')
  
  return(data.norm)
}

#' @importFrom mixtools normalmixEM
#' @importFrom mclust Mclust mclustBIC
two.comp.normalize <- function (sample, type='default', mode.lower.bound=-3) {
  #   1. For all sample types, fit a 2-component gaussian mixture model using normalmixEM.
  #   2. For the bimodal samples, find the major mode M1 by kernel density estimation
  #     2a. Fit the model with one component mean constrained to equal M1
  #     2b. Normalize (standardize) samples using mean (M1) and resulting std. dev.
  #   3. For unimodal (default) samples, find the mode M using kernel density estimation
  #     3a. Fit the model with mean for both components constrained to be equal to M
  #     3b. Normalize (standardize) samples using mean M and smaller std. dev. from model fit
  #
  #  the major mode should be located at a value larger than mode.lower.bound 
  
  # WARNING:
  # This code has a lot of hacks to fix the flakiness of normalmixEM, and the idiosyncracies
  # of the actual data. Carefully re-examine code for new or altered input data
  # Currently works for log-ratio data approximately centered around 0
  
  is.error <- function(x) inherits(x, "try-error")             # function to check for error
  
  data <- sample [ !is.na (sample) ]
  data.range <- diff (range (data))
  dens <- try (density (data, kernel='gaussian', bw='SJ'))     # gaussian kernel with S-J bandwidth
  if (is.error (dens))                                         # sometimes, SJ bw estimation fails
    dens <- density (data, kernel='gaussian', bw='ucv')        # in such cases, use unbiased CV
  # (see Venalbles & Ripley, 2002, pg, 129
  #  and Density Estimation, S.J.Sheather, Stat. Sci. 2004)
  # find major (highest) mode > -3 (to avoid problems with lower mode having higher density than higher mode)
  x.range <- dens$x > mode.lower.bound  
  dens.x <- dens$x [x.range];  dens.y <- dens$y [x.range]
  mode <- dens.x[which.max(dens.y)]                                                        
  if (type=='bimodal') mean.constr <- c (NA, mode) else mean.constr <- c (mode, mode)
  model <- normalmixEM (data, k=2, mean.constr=mean.constr, maxit=10000)
  model.rep <- normalmixEM (data, k=2, mean.constr=mean.constr, maxit=10000)
  model.alt <- Mclust (data, G=2, modelNames=c ("V","E"))
  # V results is separate SDs for each cluster; E fits a single SD for both clusters
  if (length (model.alt$parameters$variance$sigmasq)==1)  # latter code expects two SD values
    model.alt$parameters$variance$sigmasq <- rep (model.alt$parameters$variance$sigmasq, 2)
  alt.mu <- model.alt$parameters$mean
  alt.sd <- sqrt (model.alt$parameters$variance$sigmasq)
  # find reproducible model fit that is close to Mclust fit
  # if not, re-fit model -- without this condition
  # normalmixEM produces one-off model fits 
  n.try <- 1
  if (type=='bimodal') model.mode <- which(model$mu==mode)
  else model.mode <- which(model$mu==mode)[which.min (model$sigma)]
  model.other <- model.mode %% 2 + 1
  alt.mode <- ifelse (diff (alt.mu) < data.range*0.05,          # means are close -- 
                      which.min (alt.sd),                       # use min sd to pick alt.mode
                      which.min(abs(model.alt$par$mean-mode)))  #  else use alt.mu closest to mode
  # always using latter can result in consistently picking the wrong alt.mode => no convergence
  alt.other <- alt.mode %% 2 + 1
  while ( abs (model$mu[model.mode] - alt.mu[alt.mode]) > data.range*0.05 || 
          abs (model$sigma[model.mode]-alt.sd[alt.mode]) > data.range*0.05 ||
          model$sigma[model.mode] < min (0.1, data.range*0.01) ||   # some samples can have very small SD (eg CR replicates)
          (type=='bimodal' && (abs (model$mu[model.other] - alt.mu[alt.other]) > data.range*0.25)) ||
          abs (sum (c (model$mu, model$sigma) - c (model.rep$mu, model.rep$sigma))) > 1e-3 ) {
    # if major mode (and SD of mode) is not within 5% of data range, or if the other mean (for bimodals only) 
    # is not within 25% of the Mclust result, try again
    model <- normalmixEM (data, k=2, mean.constr=mean.constr, maxit=10000)
    model.rep <- normalmixEM (data, k=2, mean.constr=mean.constr, maxit=10000)
    
    if (n.try > 50) stop (paste ("Can't fit mixture model ... giving up\n"))
    n.try <- n.try + 1
  }
  
  
  if (type=='bimodal') {
    # sometimes (esp. in phosphoproteome) the minor (lower) mode can be larger than the major (higher) mode
    # this situation is not possible in the unimodal samples
    corrected.mode <- model$mu [which.max(model$mu)]
    if (corrected.mode != mode) {
      cat ('  Lower mode larger than higher mode\n')
      mode <- corrected.mode
    }
  }
  norm.mean <- mode
  norm.sd <- ifelse (type=='bimodal', model$sigma[which(model$mu==mode)], min (model$sigma))
  
  # normalize by standardizing
  data <- data - norm.mean
  data <- data / norm.sd
  
  # return normalized data reorganized to original order
  sample [ !is.na (sample) ] <- data
  return ( list (norm.sample=sample, norm.mean=norm.mean, norm.sd=norm.sd, fit=unlist (c(model$mu, model$sigma))) )
}


