# Purpose        : Evaluation of sampling designs produced using  stratification
# Maintainer     : Ichsani Wheeler <ichsani.wheeler@gmail.com>
# Contributions  : Jaap de Gruijter; 
# Status         : Pre-alpha                             
# Note           : 
 

# evaluate different designs:
eval.LH <- function(obj, tvar = names(obj)[1], n, det.lim, Ls, Ls.min = 2, desvar.t, R2, pprob = 1, silent = FALSE){
    
    require(maptools)
    require(spatstat)
    require(stratification)
    require(RSAGA)

    if(!class(obj)=="SpatialPixelsDataFrame"){
     stop("Object of class 'SpatialPixelsDataFrame' required for argument 'obj'")
    }
    
    # range of the target variable:
    hmin <- min(obj@data[,tvar], na.rm=TRUE)
    hmax <- max(obj@data[,tvar], na.rm=TRUE)
    
    if(missing(Ls)){ 
      if((hmax-hmin)/det.lim < floor(n/3)){
        Ls <- round((hmax-hmin)/det.lim, 0)
      } else {
        Ls = nclass.Sturges(obj@data[,tvar])  
        if(Ls > floor(n/3)){Ls <- floor(n/3)}
      }         
    }

    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){  abs(x - round(x)) < tol }
    if(Ls < 2|!is.wholenumber(Ls)){
         stop("Maximum number of strata must be >= 2")
    }
    
    # derive initial boundary positions:
    # the default initial boundaries are bh = min(X) + h * (max(X)-min(X))/Ls for  h=1 ,..., Ls-1

    # Step 1: derive optimal allocation and boundaries with min alloc of 3 each strata
    mout <- list(NULL)
    RMSE.out <- list(NULL)
    pb <- txtProgressBar(min=0, max=Ls-Ls.min, style=3) 
 
    for(j in Ls.min:Ls){
      initbh <- hmin + (1:(j-1)) * (hmax - hmin) / Ls
      output <- strata.LH(x=obj@data[,tvar], initbh = initbh, n = n, CV = NULL, Ls = j, certain = NULL, alloc = list(q1 = 0.5, q2 = 0, q3 = 0.5), takenone = 0, bias.penalty = 1, takeall = 0, rh = rep(1, Ls = j), model = c("none"), model.control = list(), algo = c("Kozak"), algo.control = list(method="modified", minNh = 3))
     
      mout[[j-1]] <- data.frame(strata = seq(along = output$nh), nh = output$nh, Nh = output$Nh, Ah = output$Nh/sum(output$Nh), initbh = c(hmin, output$initbh), bh = c(hmin, output$bh), p_var = output$varh, smpvar = output$varh/output$nh, desvar = (output$Nh/(sum(output$Nh)))^2 * (output$varh/output$nh))
         
      RMSE.out[[j-1]] <- output$RMSE
      
       
    # update progress bar
    if(silent==FALSE){
       setTxtProgressBar(pb, j)
    }
    }

    close(pb)
    
    # RMSE = the root mean squared error (or standard deviation of the anticipated global mean); desvar = the sampling variance for the given design
    RMSE <- unlist(RMSE.out)
    
    # (sampling) design variance is penalised by 1/R2 to compensate for the underestimation of spatial variance by the spatial prediction model
    desvar <- RMSE^2 * (1/R2) 
      
    # The best design either specified design variance threshold or via 10% of initial 2 strata (Ls=2) design variance:    
    if(missing(desvar.t)) {
        mout.m <- which(desvar < (max(desvar)/100*10))[1]
    }
          else{
      mout.m <- which(desvar < desvar.t)[1]
      if(is.na(mout.m)){
      warning("No designs are below the specified threshold value for the (predicted) sampling variance")
      }
    }
    
    if(!is.na(mout.m)){
    strata.LH <- mout[[mout.m[1]]]
         
                                                             
    # Step 2: cluster using the optimized classes
    obj$strata <- cut(x=obj@data[,tvar], breaks=c(strata.LH$bh, hmax), labels = paste("L", 1:nrow(strata.LH), sep=""), include.lowest = TRUE)
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
        ## IW: we decided to use 'rpoint', which takes a bit more time compute
        ## 'spsample' prints a warning message every 20-30% of cases - possibly a bug?
    }
    smp <- lapply(smp, as.SpatialPoints.ppp)
    smp <- do.call(rbind, smp)
    proj4string(smp) <- obj@proj4string

    out <- new("SpatialStratifiedSample", variable = tvar, locations = smp, strata = obj[c("strata","pprob")], LH = strata.LH, eval = data.frame(Ls=Ls.min:Ls, desvar = (unlist(RMSE))^2 * (1/R2)) )
    } else {
    out <- NULL
    }
  
    return(out)
}    
                                   
# default plot method:
setMethod("plot", signature(x = "SpatialStratifiedSample", y = "missing"), function(x){
  require(sp)
  require(raster)
  Ls = length(levels(x@strata@data[,1]))
  #pal = cm.colors(Ls)[rank(runif(Ls))]
  pal = rev(palette(gray(seq(.4, .95, len = Ls))))

  par(mfrow=c(1,2), mar=c(5,5,3.5,3.5), oma=c(0,0,0,0))
   #win.graph(width = 8, height = 4)
  
  # TO DO: specify aspect of the new window
  image(raster(x@strata[1]), col=pal, axes = TRUE, xlab="Northings", ylab="Eastings", main=paste("Number of strata chosen:", Ls))
  points(x@locations, pch=17, col="black", cex=0.8)
  plot(x@eval$Ls, x@eval$desvar, type="l", ylab="Sampling variance", xlab="Number of strata tested", lwd=2)
})


# end of script;