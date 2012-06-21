\name{eval.Delta}
\alias{eval.Delta}
\title{Estimates the minimum required difference between two populations}
\description{Runs N-times \code{t.test} for a given variances in samples x1 and x2. The results can be used to suggest the minimum required difference between the two measurements that will satisfy the \code{t.test} at some significance level.}
\usage{eval.Delta(Delta = seq(0, 1.5, by=.2), N, sd1, sd2, S = 100, plot = FALSE)}
\arguments{
  \item{Delta}{numeric; list of values, usually a sequence}
  \item{N}{integer; sample size}
  \item{sd1}{numeric; standard deviation of the first sample (x1)}
  \item{sd2}{numeric; standard deviation of the second sample (x2)}
  \item{S}{integer; number of simulations}  
  \item{plot}{logical; species whether the function should immediately plot the results}  
}
\value{Object of type \code{"data.frame"}.}
\note{Simulatations can be time-consuming for S > 100. Results obtained for S < 50 should be taken with care as they are subject to random effects.}
\author{Ichsani Wheeler}
\seealso{
\code{stats::rnorm}
}
\examples{
\dontrun{
Nd <- eval.Delta(N=20, sd1=1, sd2=1)
plot(x=Nd$difference, y=Nd$p.value, xlab="Difference (x1|x2)", ylab="Probability value (t-test)", pch=21, col="darkgrey")
lines(x=Nd$difference, y=rep(0.05, length(Nd$difference)))
# suggested threshold value:
Nd.glm <- glm(p.value~log1p(difference)+log1p(difference^2), Nd, family = gaussian(link="log"))
dr = range(Nd$difference)
xs = seq(dr[1], dr[2], length.out=100)
lines(x=xs, predict(Nd.glm, newdata=data.frame(difference=xs), type="response"))
Nd.E <- predict(Nd.glm, newdata=data.frame(difference=xs), type="response")
xs[which(Nd.E<0.05)][1]
}}
\keyword{evaluate}

