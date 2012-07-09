# Purpose        : Initial settings;
# Maintainer     : Ichsani Wheeler <ichsani.wheeler@gmail.com>)
# Contributions  : ; 
# Dev Status     : Pre-Alpha
# Note           : for more info see [http://cran.r-project.org/doc/manuals/R-exts.html];


################## NEW CLASSES ##############

### A new class for stratified samples:
setClass("SpatialStratifiedSample", representation(variable = "character", locations = "SpatialPoints", strata = "SpatialPixelsDataFrame", LH = "data.frame", eval = "data.frame"), validity = function(object) {
    if(!sum(object@LH[,2], na.rm=TRUE)==length(object@locations))
      warning("Number of allocations in the slot 'locations' and table 'LH' do not match")
    if(is.null(object@strata@data[,2]))
      stop("Prior probabilities missing, specify 0 to 1")  
    if(range(object@strata@data[,2], na.rm = FALSE)[1]<0)
      stop("Prior probabilities below zero, not expected")
    if(range(object@strata@data[,2], na.rm = FALSE)[2]>1)
      stop("Prior probabilities above one, not expected")
})



################## generic functions ##############

if(!isGeneric("spsample.cv")){
  setGeneric("spsample.cv", function(x, obj, ...){standardGeneric("spsample.cv")})
}

if(!isGeneric("eval.gstatModel")){
  setGeneric("eval.gstatModel", function(model, predictionLocations, ...){standardGeneric("eval.gstatModel")})
}

# end of script;
