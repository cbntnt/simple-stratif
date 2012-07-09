\name{returns}
\alias{returns}
\title{Gross and net returns for measured SOC sequestration}
\description{Estimates gross and net returns to landholder for measured soil carbon sequestration for a defined period.}
\usage{returns(obj, permanence = 100, CO2e = 23, sd1, costs, add.costs)}
\arguments{
  \item{obj}{object of type \code{"SpatialPixelsDataFrame"}}
  \item{permanence}{numeric; institution period of permanence (i.e. 100 yrs)}
  \item{CO2e}{numeric; expected CO2e offset price per tonne for given period (dollars per tonne per permanence period)}
  \item{sd1}{numeric; approximate sampling variance from initial round (else target sampling variance)}
  \item{costs}{numeric; cost for two sampling rounds}
  \item{add.costs}{cost for additional (or reduced) cores}
}
\author{Ichsani Wheeler}
\seealso{
\code{\link{sensitivity}}
}