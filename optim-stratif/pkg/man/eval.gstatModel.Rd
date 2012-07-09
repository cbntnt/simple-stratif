\name{eval.gstatModel}
\docType{methods}
\alias{eval.gstatModel}
\alias{eval.gstatModel,model,predictionLocations-method}
\title{Evaluates summary statistics for a gstatModel}
\description{Randomly subsets the existing point sample and derives summary statistics (minimum, mean, variance, maximum) for varying sampling intensity (N-list), for a number of geostatistical realizations (S-times). The outputs of \code{eval.gstatModel} can be used to determine: (1) over or under-estimation bias given the true population mean, and (2) the minimum sample size required to achieve a precise enough estimate of the population mean and variance.}
\usage{eval.gstatModel(model, predictionLocations, N = 5:length(model@sp), S = 10, nsim = 50, ...)}
\arguments{
  \item{model}{object of type \code{"gstatModel"} containing regression model, variogram model and sampling locations (see \code{GSIF::gstatModel})}
  \item{predictionLocations}{object of type \code{"SpatialPixelsDataFrame"} containing values of the covariates at new locations}
  \item{N}{integer; sampling intensity list}
  \item{S}{integer; number of repetitions}
  \item{nsim}{integer; number of realizations}
  \item{\dots}{optional arguments that can be passed to \code{predict.gstatModel}}
}
\value{Object of type \code{"data.frame"} containing:
\describe{
 \item{S}{character; simulation name}
 \item{R}{integer; repetition number}
 \item{N}{integer; sample size}
 \item{Min}{numeric; sampled minimum for a corresponding N, drawn out of \code{nsim} simulations}
 \item{Mean}{numeric; sampled mean for a corresponding N, drawn out of \code{nsim} simulations}
 \item{Var}{numeric; sampled variance for a corresponding N, drawn out of \code{nsim} simulations}
 \item{Max}{numeric; sampled maximum value for a corresponding N, drawn out of \code{nsim} simulations}
 \item{Min.sample}{numeric; sampled minimum for a corresponding N}
 \item{Mean.sample}{numeric; sampled mean for a corresponding N}
 \item{Var.sample}{numeric; sampled variance for a corresponding N}
 \item{Max.sample}{numeric; sampled maximum for a corresponding N}
}
}
\note{This method assumes that the sampling locations were selected using some probability sampling i.e. that they are unbiased and representative for the area of interest (\code{predictionLocations}). Note that the results are conditional to the \code{"gstatModel"} because the sampling is done by subsetting.}
\author{Ichsani Wheeler}
\seealso{
\code{\link{spsample.cv}}, \code{\link{eval.LH}}
}
\examples{
\dontrun{# Load the data:
library(sp)
data(meuse)
coordinates(meuse) <- ~x+y
proj4string(meuse) <- CRS("+init=epsg:28992")
# load grids:
data(meuse.grid)
gridded(meuse.grid) <- ~x+y
proj4string(meuse.grid) <- CRS("+init=epsg:28992")
# fit a gstat model:
library(GSIF)
omm <- fit.gstatModel(observations = meuse, formulaString = om~dist, family = gaussian(log), covariates = meuse.grid)
# estimate the population mean and var:
om.rk <- predict(omm, predictionLocations = meuse.grid, nsim = 10)
x.m <- mean(om.rk@realizations@data@values, na.rm=TRUE)
x.var <- var(as.vector(om.rk@realizations@data@values), na.rm=TRUE)
# evaluate the gstatModel:
N.l = 5:50
e.omm <- eval.gstatModel(omm, meuse.grid, N = N.l, S = 10, nsim = 10)
# plot the output results:
par(mfrow=c(1,2), mar=c(5,4,.5,.5))
plot(ylim=c(0,13), e.omm$N, e.omm$Mean, pch="+", cex=.6, xlab="Sampling intensity (N)", ylab="Sample mean")
lines(x=N.l, y=rep(x.m, length(N.l)), lwd=2, col="yellow")
plot(e.omm$N, e.omm$Var, pch=21, cex=.6, xlab="Sampling intensity (N)", ylab="Sample variance")
lines(x=N.l, y=rep(x.var, length(N.l)), lwd=2, col="yellow")
# sample mean in average over-estimates the population mean!
dev.off()
}}
\keyword{methods}