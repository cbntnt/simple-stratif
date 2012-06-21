\name{droadt}
\docType{data}
\alias{droadt}
\title{Relative distance to road network}
\description{A relative distance to road network map prepared for the Meuse case study. The road networks were digitized from the 1:25K topo map. The values can be used as prior probabilities to allocate samples closer to the road network.}
\usage{data(droadt)}
\format{
Data frame; contains the following columns:
  \describe{
  \item{\code{pprob}}{numeric; soil organic carbon in kilograms per cubic meter}
  \item{\code{x}}{numeric; easting (m) in Rijksdriehoek (RDH) (Netherlands topographical) map coordinates}
  \item{\code{y}}{numeric; northing (m) in RDH coordinates}
}
}
\author{ Ichsani Wheeler}
\seealso{ \code{spatstat::rpoint}
}
\examples{
# Load the data:
data(droadt)
gridded(droadt) <- ~x+y
# generate a random sample:
library(spatstat)
grd.pp <- as.im.SpatialGridDataFrame(as(droadt["pprob"], "SpatialGridDataFrame"))
smp1 <- rpoint(100, f=grd.pp)
image(grd.pp)
points(smp1)
}
\keyword{datasets}
