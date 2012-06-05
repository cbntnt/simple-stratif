
# title         : stratif_optim.R
# purpose       : optimisation of the sampling by using stratification;
# reference     : 
# producer      : Prepared by I. Wheeler
# last update   : In Sydney, AU, June 2012.
# inputs        : predicted organic carbon (raster map); 
# outputs       : summary table stratification boundaries and allocation;
# remarks 1     : ;

#---------------------------------- 
# STEP 1: stratification
#---------------------------------- 

# import the predicted values:
PRED_OC <- read.csv("PRED_OC.csv")
str(PRED_OC)
# sort data in ascending order 
PRED_OC <- PRED_OC[order(PRED_OC$oc_kgm3),]
# create cdf function for this data:
OCcdf <- ecdf(PRED_OC$oc_kgm3)  

# Specify fixed parameters:
# total number of cells 
cells <- nrow(PRED_OC)
# number of strata  
ns <- 6
# total number of sampling points
n <- 48
nmin = 2

# derive strata:
PRED_OC$sqf <- sqrt(OCcdf(PRED_OC$oc_kgm3))
PRED_OC$strata_name <- cut(x=PRED_OC$sqf, breaks=0:ns/ns, labels = paste("L",1:ns,sep=""))

# plot the strata:
xsp <- PRED_OC
gridded(xsp) <- ~x+y
spplot(xsp["strata_name"])
xsp$strata_name_i <- as.integer(xsp$strata_name)
writeGDAL(xsp["strata_name_i"], "strata_name.sdat", "SAGA", mvFlag=-99999)

#---------------------------------- 
# STEP 2: aggregate values and allocate points
#---------------------------------- 

# derive variance and allocation (number of samples per strata):
strata_output_i <- data.frame(Strata=levels(PRED_OC$strata_name), StD_i=rep(NA, ns), Var_i=rep(NA, ns), rel_area_i=rep(NA, ns), alloc_i=rep(NA, ns), bound_i=rep(NA, ns))
strata_output_i$StD_i <- aggregate(PRED_OC$oc_kgm3, by=list(PRED_OC$strata_name), FUN=sd)$x
strata_output_i$Var_i <- strata_output_i$StD_i^2
strata_output_i$rel_area_i <- summary(PRED_OC$strata_name, maxsum=ns)/cells
strata_output_i$alloc_i <-  round(n * strata_output_i$rel_area_i * strata_output_i$StD_i / sum(strata_output_i$rel_area_i * strata_output_i$StD_i), 0) # sd(PRED_OC$oc_kgm3)
# fix the allocation so that there are minumum 2 samples per strata:
strata_output_i$alloc_i <- round(strata_output_i$alloc_i / n * (n - ns*2), 0) + nmin 
strata_output_i$bound_i <- (0:ns/ns)[-1]

#---------------------------------- 
# STEP 3: # Optimise stratum limits  
#---------------------------------- 

# function to derive total variance for a given vector of values
totVar <- function(alloc_i, bound_i, vals, nmax){
   # 'bound_i' - needs to be in format low_upper , ... , 1;
   # 'alloc_i' - sampling numbers per strata;
   # 'vals' - vector of values;
   alloc_i <- exp(alloc_i)
   if(length(alloc_i)==length(bound_i)){
   if(any(alloc_i < 0)){
     stop("Negative number of samples not permitted")
   }
   Xcdf <- ecdf(vals)
   Lc <- cut(x=sqrt(Xcdf(vals)), breaks=c(0, bound_i), labels = paste("L",1:length(alloc_i), sep=""))
   Var <- aggregate(vals, by=list(Lc), FUN=var)$x
   # relative area:
   ra <- summary(Lc, maxsum=length(bound_i))/length(vals)
   # adjust numbers:
   alloc_a <- round(alloc_i / sum(alloc_i) * (nmax-2*length(bound_i))+2, 0)
   totvar <- sum(ra^2*Var/alloc_a)
   return(totvar)
  }  
   else{ stop("Number of elements in alloc_i and bound_i must correspond") }
}

# examples:
totVar(alloc_i=log(c(2,3,4,10,12,17)), bound_i=1:6/6, vals=PRED_OC$oc_kgm3, nmax=48)
totVar(alloc_i=log(c(23,31,40,100,121,17)), bound_i=1:6/6, vals=PRED_OC$oc_kgm3, nmax=48)
# optimize the allocation (number of samples per strata):
alloc_i = log(c(2,3,4,10,12,17))
alloc_i = log(c(23,31,40,100,121,17))
# alloc_i_LW = rep(0, length(alloc_i))
st.opt <- optim(par=alloc_i, fn=totVar, bound_i=1:6/6, vals=PRED_OC$oc_kgm3, nmax=48) 
st.opt
alloc_out <- round(exp(st.opt$par), 0)
alloc_out <- round(alloc_out / sum(alloc_out) * (nmax-2*length(bound_i))+2, 0)
alloc_out

# function to optimise allocation of points and boundaries for stratification
optim.strata <- function(
    obj, # raster map with values of the target var
    tvar, # target variable (column name)
    ns = 6, # number of strata
    n, # number of samples
    nmin = 2 # minimum number of samples
){

  cells <- nrow(obj)
  # derive strata:
  Xcdf <- ecdf(obj[,tvar])
  Lc <- cut(x=sqrt(Xcdf(obj[,tvar])), breaks=0:ns/ns, labels = paste("L",1:ns,sep=""))
  # prepare the output object:
  xs <- data.frame(Strata=levels(Lc), StD_i=rep(NA, ns), Var_i=rep(NA, ns), rel_area_i=rep(NA, ns), alloc_i=rep(NA, ns), bound_i=rep(NA, ns))
  xs$StD_i <- aggregate(obj[,tvar], by=list(Lc), FUN=sd)$x
  xs$Var_i <- xs$StD_i^2
  xs$rel_area_i <- summary(Lc, maxsum=ns)/cells
  xs$alloc_i <-  round(n * xs$rel_area_i * xs$StD_i / sum(xs$rel_area_i * xs$StD_i), 0) # sd(PRED_OC$oc_kgm3)
  # fix the allocation so that there are minumum 2 samples per strata:
  xs$alloc_i <- round(xs$alloc_i / n * (n - ns*2), 0) + nmin 
  xs$bound_i <- (0:ns/ns)[-1]
  
  # assign new boundaries based on

}
  

# end of scripts;


