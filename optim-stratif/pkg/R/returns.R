# Purpose        : Expected gross & net return to farmer from measured changes in soil organic carbon levels confirming a net sequestration for the period
# Maintainer     : Ichsani Wheeler <ichsani.wheeler@gmail.com>
# Contributions  : ; 
# Status         : Pre-alpha
# Note           : Given assumptions: 1: independant sample selection between rounds; 2: approximately the same sampling variance as the first round; 3: roughly the same total cost of sampling (i.e. the same number of n); 

# estimate returns to landholder
returns <- function(obj, permanence = 100, CO2e = 23, sd1, costs, add.costs){

#----------------------------------
# STEP 1: Define prior inputs -
#----------------------------------

    require(rgdal)

    if(!class(obj)=="SpatialPixelsDataFrame"){
     stop("Object of class 'SpatialPixelsDataFrame' required for argument 'obj'")
    }


#----------------------------------
# STEP 2: Estimate of change ....
#----------------------------------
    
}

    
# end of script;    