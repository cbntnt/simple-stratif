\name{eval.LH}
\alias{eval.LH}
\title{Evaluates multiple possible stratified designs}
\description{Tests multiple possible stratified designs and selects the one with the lowest total variance. Looks at different combinations of strata number, breaks and sample allocations.}
\usage{eval.LH(obj, tvar = names(obj)[1], n, Ls = floor(n/2), pprob = 1)}
\arguments{
  \item{obj}{object of type \code{"SpatialGridDataFrame"}}
  \item{tvar}{character; target variable name}
  \item{n}{integer; number of samples}
  \item{Ls}{integer; maximum number of strata}
  \item{pprob}{numeric in the range 0-1; prior probability can be set also manually}
}
\value{Object of type \code{"SpatialStratifiedSample"}.}
\author{Ichsani Wheeler}
\seealso{
\code{stratification::strata.LH}
}
\examples{
# Load the data:
data(springf)
gridded(springf) <- ~x+y
proj4string(springf) <- CRS("+init=epsg:28355")
smp <- eval.LH(springf, n=48, det.lim=.3)
plot(smp)
}
\keyword{color}

