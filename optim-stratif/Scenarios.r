#Scenarios to test with strata.LH

# title         : Scenarios.R
# purpose       : optimisation of the sampling by using stratification;
# reference     :
# producer      : Prepared by I. Wheeler
# last update   : In Sydney, AU, July 2012.
# inputs        : predicted organic carbon (raster map), various versions;
# outputs       : summary table stratification boundaries and allocation;
# remarks 1     : ;

#----------------------------------
# STEP 1: Data prep - all farms & C prediction versions (C maps)
#----------------------------------

# import the predicted values:
    #PRED_OC <- read.csv("PRED_OC.csv")
    #str(PRED_OC)
# sort data in ascending order
    #PRED_OC <- PRED_OC[order(PRED_OC$oc_kgm3),]


#----------------------------------
# STEP 2: Project data & save in compressed format
#----------------------------------

 # plot the strata:
    #xsp <- PRED_OC
    #gridded(xsp) <- ~x+y
    #spplot(xsp["strata_name"])
    #xsp$strata_name_i <- as.integer(xsp$strata_name)
    #writeGDAL(xsp["strata_name_i"], "strata_name.sdat", "SAGA", mvFlag=-99999)
    #springf <- xsp["oc_kgm3"]
    #names(springf) <- "SOC"
    #proj4string(springf) <- CRS("+init=epsg:28355")
    #springf <- data.frame(springf)
    #save(springf, file="springf.rda", compress="xz")

#----------------------------------
# STEP 3: Analysis of each farm & prediction version
#----------------------------------

#---------------
# Farm 1: Spring
#---------------



#---------------
# Farm 2: Inver
#---------------



#---------------
# Farm 3: Wanga
#---------------


#---------------
# Farm 4: Winona
#---------------







# Load the data:
data(springf)
gridded(springf) <- ~x+y
# Target variable -> SOC:
var(springf$SOC, na.rm=TRUE)
proj4string(springf) <- CRS("+init=epsg:28355")


# 1. Let the method select optimal Ls (default max Ls):
smp1 <- eval.LH(springf, n=48, det.lim=.3)

# 2. Set the threshold variance manually:
smp2 <- eval.LH(springf, n=48, det.lim=.3, smpvar.t=.05)

# 3. Set the Ls manually:
smp3 <- eval.LH(springf, n=48, det.lim=.3, Ls=24)
str(smp3@LH)

write.table(smp3@LH, file = "Optim_Ls.txt", row.names=TRUE, col.names=TRUE, sep=" ")

# plot sampling design and total variances;
plot(smp3)
# Test eval.LH (smp2) using varying n
n.l <- seq(10,110, by=20)
smp3.l <- lapply(n.l, FUN=function(x){eval.LH(springf, n=x, det.lim=.3)})
# evaluates stratification for 6 sampling intensities;

# Plot summary eval's:
sumd <- lapply(smp3.l, slot, "eval")
names(sumd) <- n.l
par(mar=c(4,4,2,1))
plot(sumd[[1]], type="l", ylab="Sampling var", ylim=c(0,max(sumd[[1]]$smpvar)), xlim=c(2,14))
for(j in 2:length(n.l)){
   lines(sumd[[j]], lty=j+1)
}


# add legend:
legend("topright", inset=.05, title="Sampling intensity (N)", as.character(n.l), lty=1:length(n.l), cex=.7)
\dontrun{# plot the resulting design in GE:
library(plotKML)
# create a new object for plotting in Google Earth:
sp.domain <- as.SpatialPolygons.SpatialPixels(smp@strata["strata"])
sp.domain <- SpatialPolygonsDataFrame(sp.domain, data.frame(ID=smp3@strata$strata), match.ID = FALSE)
smp.ssp <- new("SpatialSamplingPattern", method = "Generalized Lavallee-Hidiroglou Method",
    pattern = smp3@locations, sp.domain = sp.domain)
# the same plot now in Google Earth:
shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png"
plotKML(smp.ssp, shape = shape)
}}
\keyword{evaluate}

