#Expected gross & net return to farmer from measured changes in soil organic carbon levels confirming a net sequestration for the period

# title         : Returns_SOC.R
# purpose       : estimate of gross & net returns to farmer from measured period based on previous round & optimisation second round sampling
# reference     :
# producer      : Prepared by I. Wheeler
# last update   : In Sydney, AU, July 2012.
# inputs        : predicted organic carbon (raster map) derived from first(previous) sampling
# outputs       : summary table ................
# remarks 1     : Given assumptions: 1: independant sample selection between rounds; 2: approximately the same sampling variance as the first round; 3: roughly the same total cost of sampling (i.e. the same number of n)


#----------------------------------
# STEP 1: Define prior inputs -
#----------------------------------

# define institution period of permanence (i.e. 100 yrs)
permanence <- 100
# define expected CO2e offset price per tonne for given period ($/tonne/permanence period)       (Note - insert option to call vector of expected price fluctuations per year)
CO2e <- 23
# approx sampling variance from initial round (else target sampling variance)
smp_var0 <- NULL
# cost for two sampling rounds
2rnd_costs <- NULL
# cost for additional (or reduced) cores
addit_costs <- NULL




#----------------------------------
# STEP 2: Estimate of change ....
#----------------------------------

# estimate returns to landholder

return_SOC <- function(obj, permanence, CO2e, samp_var0, 2rnd_costs, addit_costs,  silent = FALSE){

    require(.)
    require(.)
    require(.)

    if(!class(obj)=="SpatialPixelsDataFrame"){
     stop("Object of class 'SpatialPixelsDataFrame' required for argument 'obj'")
    }
    
    