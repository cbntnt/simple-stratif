
neyman <-function(OC,cutp,cells,strat,n)
  # neyman allocation
{
  s1<- sequence(strat)
  
  
  for (k in s1)
  {
    idx<- if(k==1) {which(OC <= cutp[k])} else {if(k==strat) {which(OC > cutp[k-1])} else {which(OC > cutp[k-1] & OC <= cutp[k])}}
    strata_name[idx]<-k
  }
  
  aj<-matrix(0,nrow=strat,ncol=1)
  Vj<-matrix(0,nrow=strat,ncol=1)
  Sj<-matrix(0,nrow=strat,ncol=1)
  for (j in s1)
  {
    ij<-which(strata_name==j)
    aj[j] <- length(ij)/cells
    Vj[j] <- var(OC[ij])
    Sj[j] <- sd(OC[ij])
  }
  
  asj <- sum(aj*Sj)
  
  for (j in s1)
  {
    nj[j] <- n*aj[j]*Sj[j]/asj  
  }   
  round(nj)
}


## start
setwd('C:/budi/data/R')
PRED_OC <- read.csv("PRED_OC.csv")

#data in ascending order 
PRED_OC <- PRED_OC[order(PRED_OC$oc_kgm3) ,]

# cdf
#OCcdf <- ecdf(PRED_OC$oc_kgm3)  

OC <- PRED_OC$oc_kgm3

# total number of cells 
cells <- 5055

# number of strata  
strat <- 6

sout<-strata.cumrootf(x=OC, nclass=1000, n = 48, CV = NULL, Ls = 6, certain = NULL,
                alloc = list(q1 = 0.5, q2 = 0, q3 = 0.5), rh = rep(1, Ls=6),
                model = c("none"),
                model.control = list())

cutp<-sout$bh



# n
n <- 48

# allocate no samples per stratum based on neyman's allocation
nj<-neyman(OC,cutp,cells,strat,n)



# optimisation functions
fr <- function(x,OC,cells,strat,n)  
  # function to optimise
  # Optimise stratum limits & no. samples  
{
  s1<- sequence(strat)
  cutp<-x[1:strat-1]
  nj<-x[strat:(strat*2-1)]
  for (k in s1)
  {
    idx<- if(k==1) {which(OC <= cutp[k])} else {if(k==strat) {which(OC > cutp[k-1])} else {which(OC > cutp[k-1] & OC <= cutp[k])}}
    strata_name[idx]<-k
  }
  
aj<-matrix(0,nrow=strat,ncol=1)
Vj<-matrix(0,nrow=strat,ncol=1)
for (j in s1)
{
  ij<-which(strata_name==j)
  aj[j] <- length(ij)/cells
  Vj[j] <- var(OC[ij])
}
  nj <- round(nj)  
  sumn<-sum(nj)
#  nj <- round(nj/sumn*n) 
  pen<-abs(sumn-n)  
  sum(aj^2*Vj/nj)*(pen+1)
}




fr3 <- function(x,OC,nj,cells,strat,n)  
# function to optimise
#  only boundaries
{
  s1<- sequence(strat)
  cutp<-x[1:strat-1]
  
  for (k in s1)
  {
    idx<- if(k==1) {which(OC <= cutp[k])} else {if(k==strat) {which(OC > cutp[k-1])} else {which(OC > cutp[k-1] & OC <= cutp[k])}}
    strata_name[idx]<-k
  }
  
  aj<-matrix(0,nrow=strat,ncol=1)
  Vj<-matrix(0,nrow=strat,ncol=1)
  Sj<-matrix(0,nrow=strat,ncol=1)
  s1<- sequence(strat)
  for (j in s1)
  {
    ij<-which(strata_name==j)
    aj[j] <- length(ij)/cells
    Vj[j] <- var(OC[ij])
    Sj[j] <- sd(OC[ij])
  }
  
  sum(aj^2*Vj/nj)
}




# optimisation

# optimise both boundaries & no. samples
x<-matrix(0,nrow=strat*2-1,ncol=1)
x[1:strat-1]=cutp # boundaries to optimise
x[6:11,1]=nj # no. samples for each stratum (change in relation)
# optimisation 1, optimse boundaries and no. samples
opt1<-optim(x,fr,OC=OC,cells=cells,strat=strat,n=n)

# extract relevant parameters
bound<-(opt1$par[1:strat-1])
nj<-opt1$par[6:11] # change as required
nj <- round(nj)  
Vmin<-opt1$value # sampling variance minimised


# optimisation  2, only optimising boundaries, nh is calculated from Neyman's alloc
x1<-matrix(0,nrow=strat-1,ncol=1)
x1[1:strat-1]=cutp # boundaries to optimise
opt2<-optim(x1,fr3,nj=nj,OC=OC,cells=cells,strat=strat,n=n)
# extract relevant parameters
bound<-(opt2$par)
Vmin<-opt2$value # sampling variance minimised



