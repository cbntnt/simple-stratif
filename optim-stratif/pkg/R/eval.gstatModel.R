# Purpose        : Summary stats using geostat simulations
# Maintainer     : Ichsani Wheeler <ichsani.wheeler@gmail.com>
# Contributions  : ; 
# Status         : Pre-alpha
# Note           : Calculation with many simulations can be time consuming; 
 

# produces summaries for a given gstatModel:
setMethod("eval.gstatModel", signature(model = "gstatModel", predictionLocations = "SpatialPixelsDataFrame"), function(model, predictionLocations, N = 5:length(model@sp), S = 10, nsim = 50, ...){

    require(GSIF)
    require(gstat)
    require(raster)
    
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){  abs(x - round(x)) < tol }
    if(!is.wholenumber(N)|!is.wholenumber(S)){
      stop("'N' and 'S' must be integers")
    }

    ov <- overlay(predictionLocations, model@sp)
    sel <- !is.na(ov)
    tv <- all.vars(model@regModel$formula)[1]

    # check N:     
    if(max(N) > length(ov[sel])){
      warning("The submitted N exceeds the total number of samples in the 'model' object")
      N = N[-which(N > length(ov[sel]))]
    }
    
    # generate S simulations:
    ss <- predict(object = model, predictionLocations = predictionLocations, nsim = nsim, ...)
    
    # for each N, derive summary stats and save:
    m.l <- rep(list(list(NULL)), length(N)) 
    mo.l <- rep(list(list(NULL)), length(N)) 
    min.l <- rep(list(list(NULL)), length(N))
    mino.l <- rep(list(list(NULL)), length(N))
    max.l <- rep(list(list(NULL)), length(N))
    maxo.l <- rep(list(list(NULL)), length(N))
    v.l <- rep(list(list(NULL)), length(N))
    vo.l <- rep(list(list(NULL)), length(N))    
    message(paste("Randomly drawing", length(N)*S, "samples"))
    pb <- txtProgressBar(min=0, max=length(N), style=3)
    for(j in 1:length(N)){
      for(k in 1:S){
        # randomly subset to N:
        rnd <- runif(length(ov[sel]))<(N[j]/length(ov[sel]))
        # repeat until rnd reaches N:
        while(!length(rnd[rnd])==N[j]){ rnd <- runif(length(ov[sel]))<(N[j]/length(ov[sel])) }
        sm <- model@sp[sel,][rnd,]
        # summary stats (samples):
        mo.l[[j]][[k]] <- rep(mean(model@regModel$model[sel,tv][rnd]), nsim)
        vo.l[[j]][[k]] <- rep(var(model@regModel$model[sel,tv][rnd]), nsim)
        mino.l[[j]][[k]] <- rep(min(model@regModel$model[sel,tv][rnd]), nsim)
        maxo.l[[j]][[k]] <- rep(max(model@regModel$model[sel,tv][rnd]), nsim)
        # derive summary stats (realizations):
        ovr <- extract(ss@realizations, sm)
        m.l[[j]][[k]] <- lapply(data.frame(ovr), mean)
        v.l[[j]][[k]] <- lapply(data.frame(ovr), var)
        min.l[[j]][[k]] <- lapply(data.frame(ovr), min)
        max.l[[j]][[k]] <- lapply(data.frame(ovr), max)
      }
    setTxtProgressBar(pb, j)
    }
    close(pb) 
    
    # bind to a single data.frame:
    out <- expand.grid(S=paste("sim", 1:nsim, sep="_"), R=1:S, N=N, KEEP.OUT.ATTRS = FALSE)
    out$Min <- as.vector(unlist(min.l))
    out$Mean <- as.vector(unlist(m.l))
    out$Var <- as.vector(unlist(v.l))
    out$Max <- as.vector(unlist(max.l))
    out$Min.sample <- as.vector(unlist(mino.l))
    out$Mean.sample <- as.vector(unlist(mo.l))
    out$Var.sample <- as.vector(unlist(vo.l))
    out$Max.sample <- as.vector(unlist(maxo.l))
   
    return(out)
})    


# end of script;