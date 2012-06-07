# Purpose        : Initial settings;
# Maintainer     : Ichsani Wheeler <ichsani.wheeler@gmail.com>)
# Contributions  : ; 
# Dev Status     : Pre-Alpha
# Note           : for more info see [http://cran.r-project.org/doc/manuals/R-exts.html];


################## NEW CLASSES ##############

### A new class for SpeciesDistributionMap:
setClass("SpatialStratifiedSample", representation(variable = "character", locations = "SpatialPoints", strata = "SpatialPixelsDataFrame", LH = "data.frame", eval = "data.frame"), validity = function(object) {
    if(!sum(object@LH[,2], na.rm=TRUE)==length(object@locations))
      warning("Number of allocations in the slot 'locations' and table 'LH' do not match")    
})

################## generic functions ##############

if(!isGeneric("getID")){
  setGeneric("getID", function(obj, ...){standardGeneric("getID")})
}



# end of script;
