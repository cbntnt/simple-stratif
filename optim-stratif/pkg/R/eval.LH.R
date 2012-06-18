# Purpose        : Evaluation of sampling designs produced using  stratification
# Maintainer     : Ichsani Wheeler <ichsani.wheeler@gmail.com>
# Contributions  : Jaap de Gruijter; 
# Status         : Pre-alpha
# Note           : Sampling optimisation can be time-consuming; 
 

# evaluate different designs:
eval.LH <- function(obj, tvar = names(obj)[1], n, det.lim, Ls, smpvar.t, pprob = 1){
    
    require(maptools)
    require(spatstat)
    require(stratification)
    
    # range of the target variable:
    hmin <- min(obj@data[,tvar], na.rm=TRUE)
    hmax <- max(obj@data[,tvar], na.rm=TRUE)
    
    if(missing(Ls)){ 
      if((hmax-hmin)/det.lim < floor(n/2)){
        Ls <- round((hmax-hmin)/det.lim, 0)
      } else {
        Ls = nclass.Sturges(obj@data[,tvar])  
        if(Ls > floor(n/2)){Ls <- floor(n/2)}
      }         
    }

    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){  abs(x - round(x)) < tol }
    if(Ls < 2|!is.wholenumber(Ls)){
         stop("Maximum number of strata must be >= 2")
    }
    
    # derive initial boundary positions:
    # the default initial boundaries are bh = min(X) + h * (max(X)-min(X))/Ls for  h=1 ,..., Ls-1

    # Step 1: derive optimal allocation and boundaries
    mout <- list(NULL)
    RMSE.out <- list(NULL)
    pb <- txtProgressBar(min=0, max=Ls-2, style=3)
    for(j in 2:Ls){
      initbh <- hmin + (1:(j-1)) * (hmax - hmin) / Ls
      output <- strata.LH(x=obj@data[,tvar], initbh = initbh, n = n, CV = NULL, Ls = j, certain = NULL, alloc = list(q1 = 0.5, q2 = 0, q3 = 0.5), takenone = 0, bias.penalty = 1, takeall = 0, rh = rep(1, Ls = j), model = c("none"), model.control = list(), algo = c("Kozak"), algo.control = list())
     
      mout[[j-1]] <- data.frame(Ah = output$Nh/sum(output$Nh), nh = output$nh, bh = c(hmin, output$bh), varh = output$varh)
      RMSE.out[[j-1]] <- output$RMSE
      # sdh = (output$Nh/sum(output$Nh))^2 *(output$varh)               
      # IW: Must check 'varh' -> looks to be only estimated population variance within strata;

    # update progress bar
    setTxtProgressBar(pb, j)
    }
    close(pb)
    
    ## derive total variance per design ?
    # smpvar <- sapply(mout, function(x){sum(x$sdh)}) 
    smpvar <- unlist(RMSE.out)
    # RMSE = the root mean squared error (or standard error of the anticipated global mean)
        
    # The best design:    
    if(missing(smpvar.t)) {
        mout.m <- which(smpvar==min(smpvar))
    }
    else{ 
      mout.m <- which(smpvar < smpvar.t)[1] 
      if(is.na(mout.m)){
      stop("None of designs is below threshold value for the total variance across the stratified design")
      }
    }
    
    strata.LH <- mout[[mout.m[1]]]

    # Step 2: cluster using the optimized classes
    obj$strata <- cut(x=obj@data[,tvar], breaks=c(strata.LH$bh, hmax), labels = paste("L",1:nrow(strata.LH),sep=""), include.lowest = TRUE)
    # add sampling probs:
    if(length(pprob)==1){ pprob = rep(pprob, length(obj$strata)) }
    obj$pprob <- pprob
    obj.sr <- lapply(levels(obj$strata), FUN=function(L){obj[obj$strata==L,"pprob"]})
    
    # Step 3: sample each cluster using random sampling:
    smp <- list(NULL)
    for(j in 1:length(levels(obj$strata))){
        grd <- as.im.SpatialGridDataFrame(as(obj.sr[[j]]["pprob"], "SpatialGridDataFrame"))
        smp[[j]] <- rpoint(n=strata.LH$nh[j], f=grd)
        # spsample(obj.sr[[j]]["strata"], n=strata.LH$nh[j], "random")
        ## we decided to use 'rpoint', which takes a bit more time compute
        ## 'spsample' prints a warning message every 20-30% of cases - possibly a bug?
    }
    smp <- lapply(smp, as.SpatialPoints.ppp)
    smp <- do.call(rbind, smp)
    proj4string(smp) <- obj@proj4string

    out <- new("SpatialStratifiedSample", variable = tvar, locations = smp, strata = obj[c("strata","pprob")], LH = strata.LH, eval = data.frame(Ls=2:Ls, smpvar=unlist(smpvar)))
    
    return(out)
}    


# default plot method:
setMethod("plot", signature(x = "SpatialStratifiedSample", y = "missing"), function(x){
  require(sp)
  require(raster)
  Ls = length(levels(x@strata@data[,1]))
  pal = rainbow(Ls)[rank(runif(Ls))]
  par(mfrow=c(1,2), mar=c(3.5,3.5,.5,.5), oma=c(0,0,0,0))
  # TO DO: specify aspect of the new window
  image(raster(x@strata[1]), col=pal, axes = FALSE, xlab="", ylab="")
  points(x@locations, pch="+", col="black", cex=1.2)
  plot(x@eval$Ls, x@eval$smpvar, type="l", ylab="", xlab="")
})


# end of script;