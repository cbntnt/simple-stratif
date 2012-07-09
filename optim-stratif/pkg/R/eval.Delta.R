# Purpose        : Estimates the minimum required difference between two populations
# Maintainer     : Ichsani Wheeler <ichsani.wheeler@gmail.com>
# Contributions  : ; 
# Status         : Pre-alpha
# Note           : This algorithm needs to be extended so that it works for any given distribution; 
 

# evaluate different designs:
eval.Delta <- function(Delta = seq(0, 1.5, by=.2), N, sd1, sd2, S = 100, plot = FALSE){

  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){  abs(x - round(x)) < tol }
  if(!is.wholenumber(N)|!is.wholenumber(S)){
    stop("'N' and 'S' must be integers")
  }

  dif <- rep(list(list(NULL)), length(Delta))
  p.v <- rep(list(list(NULL)), length(Delta))
  for(j in 1:length(Delta)){
    for(i in 1:S){
      tt <- t.test(rnorm(N, mean=0, sd=sd1), rnorm(N, sd=sd2, mean=Delta[j]))
      p.v[[j]][[i]] <- signif(as.numeric(tt$p.value), 4)
      dif[[j]][[i]] <- signif(abs(diff(as.numeric(tt$estimate))), 4)
      }
  }
  out <- expand.grid(S=paste("sim", 1:S, sep="_"), Delta=unlist(Delta), KEEP.OUT.ATTRS = FALSE)
  out$p.value <- as.vector(unlist(p.v))
  out$difference <- as.vector(unlist(dif))    
  if(plot == TRUE){
    plot(x=out$difference, y=out$p.value, xlab="Difference (x1|x2)", ylab="Probability value (t-test)", pch=21, col="darkgrey", xlim=c(0,max(Delta, na.rm=TRUE)), main=paste("Sampling size:", N, "; Number of simulations:", S))
    lines(x=out$difference, y=rep(0.05, length(out$difference)))
  }
     
  return(out)
}    

# end of script;