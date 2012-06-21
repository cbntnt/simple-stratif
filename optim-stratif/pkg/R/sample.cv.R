# Purpose        : Cross-validation of sampling designs using multiple simulations
# Maintainer     : Ichsani Wheeler <ichsani.wheeler@gmail.com>
# Contributions  : ; 
# Status         : Pre-alpha
# Note           : Calculation with many simulations can be time consuming; 
 

# evaluate different designs:
sample.cv <- function(x, obj, variable = x@title, pprob = 1, N = 2:25, S = 50, type=list("rpoint", "strata.LH")[[1]], Ls){
    
    require(raster)
    require(spatstat)
    require(stratification)
    
    # check input objects:
    if(!class(x)=="RasterBrick"){
      stop("Object of class 'RasterBrick' required for argument 'obj'")
    }
    if(!class(obj)=="SpatialPixelsDataFrame"){
      stop("Object of class 'SpatialPixelsDataFrame' required for argument 'obj'")
    }
    # sampling strategy:
    # if(!type=="rpoint"|!type=="eval.LH"){
    #  stop("This sampling algorithm has not been implemented yet")
    # }

    # Prior probability image:
    if(length(pprob)==1){ 
      pprob = rep(pprob, length(obj)) 
      message(paste("Running simulations with method ", type, " and constant prior probability...", sep=""))
    }
    else{ 
      if(!length(pprob)==length(obj)){
        stop("'pprob' object is not of same size as the 'obj' object")
      }
      message(paste("Running simulations with method ", type, " and varying prior probabilities...", sep=""))
    }
    # copy values:
    obj$pprob <- pprob
    grd <- as.im.SpatialGridDataFrame(as(obj["pprob"], "SpatialGridDataFrame"))
    
    # summary statistics:
    x.r <- range(getValues(x), na.rm=TRUE)
    x.qr <- quantile(getValues(x), c(0.025, 0.975), na.rm=TRUE)
    stats <- c(x.r[1], x.qr[1], mean(getValues(x), na.rm=TRUE), x.qr[2], x.r[2], sd(as.vector(getValues(x)), na.rm=TRUE), sd(as.vector(x@data@values), na.rm=TRUE)/sqrt(N[length(N)]))
    attr(stats, "names") <- c("min", "quantile (0.025)", "mean", "quantile (0.975)", "max", "sd", "SEM")
    
    # run S samples:
    s.pnt <- rep(list(list(NULL)), length(N))
    m.dif <- rep(list(list(NULL)), length(N))
    s.dif <- rep(list(list(NULL)), length(N))
    t.prob <- rep(list(list(NULL)), length(N))

    pb <- txtProgressBar(min=0, max=length(N), style=3)
    for(j in 1:length(N)){
      for(i in 1:S){
        
        if(type=="rpoint"){
          smp <- as.SpatialPoints.ppp(rpoint(n=N[j], f=grd)) 
        }
        if(type=="strata.LH"){
          hmin <- min(obj@data[,1], na.rm=TRUE)
          hmax <- max(obj@data[,1], na.rm=TRUE)
          initbh <- hmin + (1:(Ls-1)) * (hmax - hmin) / Ls
          # stratification:
          output <- strata.LH(x=obj@data[,1], initbh = initbh, n = N[j], CV = NULL, Ls = Ls, certain = NULL, alloc = list(q1 = 0.5, q2 = 0, q3 = 0.5), takenone = 0, bias.penalty = 1, takeall = 0, rh = rep(1, Ls = Ls), model = c("none"), model.control = list(), algo = c("Kozak"), algo.control = list())
          # stratify:
          obj$strata <- cut(x=obj@data[,1], breaks=c(hmin, output$bh, hmax), labels = paste("L", 1:Ls, sep=""), include.lowest = TRUE)
          obj.sr <- lapply(levels(obj$strata), FUN=function(L){obj[obj$strata==L,"pprob"]})
          smp <- list(NULL)
          # sample per strata:
          for(k in 1:length(levels(obj$strata))){
            grds <- as.im.SpatialGridDataFrame(as(obj.sr[[k]]["pprob"], "SpatialGridDataFrame"))
            smp[[k]] <- rpoint(n=output$nh[k], f=grds)
          }
          smp <- lapply(smp, as.SpatialPoints.ppp)
          smp <- do.call(rbind, smp)
          proj4string(smp) <- obj@proj4string
        }
        
        # derive difference from the mean for each realization:
        ov <- extract(om.rk@realizations, smp)
        if(length(ov)==0){
          warning("No points were produced as a result of overlay")
        }
        colm <- colMeans(ov, na.rm = FALSE)
        # cross-validation:
        m.dif[[j]][[i]] <- sd(stats[3] - colm)
        s.dif[[j]][[i]] <- mean(stats[7] - apply(ov, 2, FUN=sd, na.rm = FALSE)/sqrt(N[j]))
        # standardized t-value:
        td <- abs((colm - stats[3]) / (stats[7]-1))
        t.prob[[j]][[i]] <- mean(2*pnorm(td, lower.tail=FALSE))
        s.pnt[[j]][[i]] <- data.frame(smp)
        s.pnt[[j]][[i]]$design <- paste(type, "_N", N[j], "_r", i, sep="")
      }
      setTxtProgressBar(pb, j)
    } 
    close(pb)
    
    # put all point designs in one big table:
    xy <- list(NULL)
    for(j in 1:length(N)){
      xy[[j]] <- do.call(rbind, s.pnt[[j]])
    }
    xy <- do.call(rbind, xy)
    
    # return output data.frame:
    mat <- expand.grid(Design=paste(type, 1:S, sep="_"), N=unlist(N), KEEP.OUT.ATTRS = FALSE)
    mat$Md <- as.vector(unlist(m.dif))
    mat$Msd <- as.vector(unlist(s.dif))
    mat$Dp <- as.vector(unlist(t.prob))
    
    # put everything into a big list:
    out <- list(variable=variable, stats=stats, samples=xy, cv=mat)    
    return(out)
}    


# end of script;