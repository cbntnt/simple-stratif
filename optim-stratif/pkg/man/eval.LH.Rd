\name{eval.LH}
\alias{eval.LH}
\title{Evaluates multiple possible stratified designs}
\description{Tests multiple possible stratified designs and selects the optimal design for given sample number based on several criteria (INSERT DETAIL test!!) - nominally the design with the lowest total variance. Looks at different combinations of strata number, breaks and sample allocations per n}
\usage{eval.LH(obj, tvar = names(obj)[1], n, Ls = floor(n/2), smpvar.t, pprob = 1)}
\arguments{
  \item{obj}{object of type \code{"SpatialGridDataFrame"}}
  \item{tvar}{character; target variable name}
  \item{n}{integer; number of samples}
  \item{Ls}{integer; maximum number of strata}
  \item{smpvar.t}{numeric; threshold value for the total variance across the stratified design}
  \item{pprob}{numeric in the range 0-1; prior probability can be set also manually}
}
\value{Object of type \code{"SpatialStratifiedSample"}.}
\note{If not specified otherwise, functions tries to estimate the threshold value for the total within-stratum sampling variance. }
\author{Ichsani Wheeler}
\seealso{
\code{stratification::strata.LH}
}
\examples{
# Load the data:
data(springf)
# download.file("http://simple-stratif.googlecode.com/svn/optim-stratif/pkg/data/springf.rda", "springf.rda")
# load("springf.rda")
gridded(springf) <- ~x+y
# Target variable -> SOC:
var(springf$SOC, na.rm=TRUE)
proj4string(springf) <- CRS("+init=epsg:28355")
# 1. Let the method select max Ls:
smp1 <- eval.LH(springf, n=48, det.lim=.3)
# 2. Set the threshold variance manually:
smp2 <- eval.LH(springf, n=48, det.lim=.3, smpvar.t=.05)
# 3. Set the Ls manually:
smp3 <- eval.LH(springf, n=48, det.lim=.3, Ls=3)
str(smp3@LH)
# plot sampling design and total variances;
plot(smp3)
# Test eval.LH (smp2) using varying n
n.l <- seq(10,110, by=20) 
smp2.l <- lapply(n.l, FUN=function(x){eval.LH(springf, n=x, det.lim=.3)})  
# evaluates stratification for 6 sampling intensities;  
# Plot summary eval's:
sumd <- lapply(smp2.l, slot, "eval")
names(sumd) <- n.l
plot(sumd[[1]], type="l", ylab="RMSE", ylim=c(0,max(sumd[[1]]$smpvar)), xlim=c(2,14))
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
\keyword{color}

