\name{springf}
\docType{data}
\alias{springf}
\title{Springfield}
\description{A gridded map showing predicted soil organic carbon stock for a study area in NSW, Australia.}
\usage{data(springf)}
\format{
Data frame; contains the following columns:
  \describe{
  \item{\code{SOC}}{numeric; soil organic carbon in kilograms per cubic meter}
  \item{\code{x}}{numeric; x-coordinate in GDA94 / MGA zone 55}
  \item{\code{y}}{numeric; y-coordinate in GDA94 / MGA zone 55}
}
}
\author{ Ichsani Wheeler}
\note{ Predictions of organic carbon were produced using a regression modeling only.
}
\examples{
# Load the data:
data(springf)
gridded(springf) <- ~x+y
proj4string(springf) <- CRS("+init=epsg:28355")
\dontrun{# visualize the map:
library(plotKML)
data(R_pal)
spplot(springf, col.regions=R_pal[["soc_pal"]])
}}
\keyword{datasets}
