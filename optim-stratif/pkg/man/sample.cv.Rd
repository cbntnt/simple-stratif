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
\value{Object of type \code{"list"} containing:
\describe{
 \item{variable}{character; variable name}
 \item{stats}{numeric; summary statistics derived for the stack of realizations ('x' object)}
 \item{samples}{data frame; generated samples with "design" column containing the unique sampling realization}
 \item{cv}{data frame; results of cross validation, aggregated per all realizations ('x' object)}
}
}
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
data(droadt)
gridded(droadt) <- ~x+y
\dontrun{# fit a gstat model:
library(plotKML)
omm <- fit.gstatModel(observations = meuse, formulaString = om~dist, family = gaussian(log), covariates = meuse.grid)
# produce 100 simulations:
om.rk <- predict(omm, predictionLocations = meuse.grid, nsim = 100)
meuse.grid$om.glm <- predict.glm(omm@regModel, meuse.grid, type = "response")
SRS1.cv <- sample.cv(x=om.rk@realizations, obj=meuse.grid, N=seq(2,200, by=10))
Ns = seq(2, 50, by=5)
SRS.cv <- sample.cv(x=om.rk@realizations, obj=meuse.grid, N=Ns)
PPS.cv <- sample.cv(x=om.rk@realizations, obj=meuse.grid, pprob=droadt$pprob, N=Ns)
STR_3.cv <- sample.cv(x=om.rk@realizations, obj=meuse.grid["om.glm"], N=Ns, S=1, type="strata.LH", Ls=3)
STR_4.cv <- sample.cv(x=om.rk@realizations, obj=meuse.grid["om.glm"], N=2:20, S=1, type="strata.LH", Ls=4)
STR_5.cv <- sample.cv(x=om.rk@realizations, obj=meuse.grid["om.glm"], N=2:20, S=1, type="strata.LH", Ls=5)
# model the three designs and compare in one plot:
SRS1_Md.m <- glm(Md~log1p(N)+log1p(N^2), SRS1.cv$cv, family = gaussian(link="log"))
SRS_Md.m <- glm(Md~log1p(N)+log1p(N^2), SRS.cv$cv, family = gaussian(link="log"))
PPS_Md.m <- glm(Md~log1p(N)+log1p(N^2), PPS.cv$cv, family = gaussian(link="log"))
STR_Md.m <- glm(Md~log1p(N)+log1p(N^2), STR_3.cv$cv, family = gaussian(link="log"))
# compare results for SRS:
par(mfrow=c(1,2), mar=c(5,4,.5,.5))
plot(x=SRS1.cv$cv$N, y=SRS1.cv$cv$Md, xlab="Sampling intensity (N)", ylab="Error (Md)", pch=21, col="darkgrey", ylim=c(0,3), xlim=c(2,200))
lines(x=2:200, y=predict.glm(SRS_Md.m, newdata=data.frame(N=2:200), type = "response"), lwd=2)
plot(x=SRS1.cv$cv$N, y=SRS1.cv$cv$Dp, xlab="Sampling intensity (N)", ylab="Normal probability (Dp)", pch=21, col="darkgrey", ylim=c(0,1), xlim=c(2,200))
lines(x=2:200, y=rep(0.05, length(2:200)), lty=4)
dev.off()
# proof that prior probabilities were used:
par(mfrow=c(1,2), mar=c(5, 4, .5, .5))
plot(PPS.cv$samples[,1:2], pch="+", cex=.6, xlab="x", ylab="y")
plot(x=2:50, y=predict.glm(SRS_Md.m, newdata=data.frame(N=2:50), type = "response"), lwd=1, type="l", xlab="Sampling intensity (N)", ylab="Mean error (Md)")
lines(x=2:50, y=predict.glm(PPS_Md.m, newdata=data.frame(N=2:50), type = "response"), lwd=1, type="l", lty=2)
lines(x=2:50, y=predict.glm(STR_Md.m, newdata=data.frame(N=2:50), type = "response"), lwd=2, type="l", lty=3)
legend("topright", inset=.05, title="Sampling design:", c("SRS","PPS","STR"), lty=c(1,2,3), cex=.9)
dev.off()
# compare three designs:
par(mfrow=c(1,3))
scatter.smooth(x=PPS.cv$cv$N, y=PPS.cv$cv$Dp, main="Sampling with prior probability", ylim=c(0,.6), xlim=c(2,50), xlab="Sampling intensity (N)", ylab="", pch=21, bg="white", col="black", cex.main=1.2, font.main=4, cex=1.5)
scatter.smooth(x=SRS.cv$cv$N, y=SRS.cv$cv$Dp, main="Simple Random Sampling", ylim=c(0,.6), xlim=c(2,50), xlab="Sampling intensity (N)", ylab="Normal probability (Pd)", pch=21, bg="white", col="black", cex.main=1.2, font.main=4, cex=1.5)
scatter.smooth(x=STR_3.cv$cv$N, y=STR_3.cv$cv$Dp, main="Stratified sampling (LH method)", ylim=c(0,.6), xlim=c(2,50), xlab="Sampling intensity (N)", ylab="", pch=21, bg="white", col="black", cex.main=1.2, font.main=4, cex=1.5)
dev.off()
}}
\keyword{validation}

