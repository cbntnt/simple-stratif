\name{sample.cv}
\alias{sample.cv}
\title{Cross-validation for multiple realizations of sampling strategy}
\description{Derives mean error for varying sampling intensity (N-list) and summary statistics for a number of realizations of the targeted sampling strategy (S-times).}
\usage{sample.cv(x, obj, variable = x@title, pprob = 1, N = 2:25, S = 50, type=list("rpoint", "strata.LH")[[1]], ...)}
\arguments{
  \item{x}{object of type \code{"RasterBrick"} containing multiple equiprobable realizations}
  \item{obj}{object of type \code{"SpatialGridDataFrame"} containing the target variable used for stratification}
  \item{variable}{character; variable title}
  \item{pprob}{numeric in the range 0-1; prior probability can be set also manually}
  \item{N}{integer; sampling intensity list}
  \item{S}{integer; number of repetitions}
  \item{type}{character; sampling design strategy}
  \item{Ls}{number of strata to be passed to the \code{strata.LH} function}
}
\value{Object of type \code{"list"}.}
\note{This operation can be tim-consuming for large data sets. The \code{sample.cv} at the moment implements only two sampling strategies --- random sampling with varying probability (\code{"rpoint"}) and stratified sampling (\code{"strata.LH"}). The results of cross validation are highly dependent on how accurate are geostistical simulations and should be taken with care.}
\author{Ichsani Wheeler}
\seealso{
\code{\link{eval.LH}}, \code{stratification::strata.LH}
}
\examples{
# Load the data:
library(sp)
data(meuse)
coordinates(meuse) <- ~x+y
proj4string(meuse) <- CRS("+init=epsg:28992")
# load grids:
data(meuse.grid)
gridded(meuse.grid) <- ~x+y
\dontrun{# fit a gstat model:
library(plotKML)
omm <- fit.gstatModel(observations = meuse, formulaString = om~dist, family = gaussian(log), covariates = meuse.grid)
# produce 100 simulations:
om.rk <- predict(omm, predictionLocations = meuse.grid, nsim = 100)
meuse.grid$om.glm <- predict.glm(omm@regModel, meuse.grid, type = "response")
SRS.cv <- sample.cv(x=om.rk@realizations, meuse.grid["om.glm"], N=2:20)
STR_3.cv <- sample.cv(x=om.rk@realizations, meuse.grid["om.glm"], N=2:20, S=1, type="strata.LH", Ls=3)
STR_4.cv <- sample.cv(x=om.rk@realizations, meuse.grid["om.glm"], N=2:20, S=1, type="strata.LH", Ls=4)
STR_5.cv <- sample.cv(x=om.rk@realizations, meuse.grid["om.glm"], N=2:20, S=1, type="strata.LH", Ls=5)
# compare two designs:
par(mfrow=c(1,2))
plot(x=SRS.cv$cv$N, y=SRS.cv$cv$Md, xlab="Sampling intensity (N)", ylab="Error (Md)", pch=21, col="darkgrey", ylim=c(0.2,3), cex.main=1, main="Simple Random Sampling", font.main=4, cex=2)
plot(x=STR_3.cv$cv$N, y=STR_3.cv$cv$Md, xlab="Sampling intensity (N)", ylab="", pch=21, col="darkgrey", ylim=c(0.2,3), cex.main=1, main="Stratified sampling", font.main=4, cex=2)
}
}
\keyword{color}

