


PRED_OC <- read.csv("C:/PhD Stuff/2012_PhD/R_scripts/simple-stratif/optim-stratif/PRED_OC.csv")

#data in ascending order 

PRED_OC <- PRED_OC[order(PRED_OC$oc_kgm3) ,]

# cdf

OCcdf <- ecdf(PRED_OC$oc_kgm3)  

# total number of cells 

cells <- 5055

# number of strata  

strat <- 6

# n

n <- 48

#specify strata cum(sqrt(f)) rule

f<-OCcdf(PRED_OC$oc_kgm3)
OC<-PRED_OC$oc_kgm3

sqf<-sqrt(f)

s1<- sequence(strat)
s1

bound1<-s1/strat
bound1

strata_name<-seq(1,cells)
for (k in s1)
{
  idx<-if (k==1) {which(sqf <= bound1[k])} else {which (sqf > bound1[k-1] & sqf <= bound1[k])}
  strata_name[idx]<-k
}

#allocation 
aj<-matrix(0,nrow=strat,ncol=1)
nj<-matrix(0,nrow=strat,ncol=1)
Sj<-matrix(0,nrow=strat,ncol=1) 
Vj<-matrix(0,nrow=strat,ncol=1) 

for (j in s1)
{
  ij<-which(strata_name==j)
  aj[j] <- length(ij)/cells
  Sj[j] <- sd(OC[ij])
  Vj[j] <- var(OC[ij])
}

asj <- sum(aj*Sj)
  
for (j in s1)
{
  nj[j] <- n*aj[j]*Sj[j]/asj  
}  
  
nj <- round(nj)  
  
n2<-matrix(2,nrow=strat,ncol=1)  
  
sumn2 <- sum(n2)
nj <- round(nj/n*(n-sumn2)) + n2

#Stratum var

SV1 <- sum(aj^2*Vj/nj)


# Optimise stratum limits  # function to optimise

fr <- function(x,s1,sqf,OC,cells,strat)  

{

  bound1<-cbind(x[1:strat-1],1)
  nj<-x[strat:(strat*2-1)]
  for (k in s1)
  {
    idx<-if (k==1) {which(sqf <= bound1[k])} else {which (sqf > bound1[k-1] & sqf <= bound1[k])}
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
  n2<-matrix(2,nrow=strat,ncol=1)  
  sumn2 <- sum(n2)
  nj <- round(nj/n*(n-sumn2)) + n2
sum(aj^2*Vj/nj)
}


# optimisation
x<-matrix(0,nrow=strat*2-1,ncol=1)

x[1:strat-1]=bound1[1:strat-1] # boundaries to optimise

x[6:11,1]=nj # no. samples for each stratum (change in relation)


opt1<-optim(x,fr,s1=s1,sqf=sqf,OC=OC,cells=cells,strat=strat)

# extract relevant parameters
bound<-(opt1$par[1:strat-1])
bound[strat]=1
nj<-opt1$par[6:11] # chamge as required

nj <- round(nj)  
n2<-matrix(2,nrow=strat,ncol=1)  
sumn2 <- sum(n2)
nj <- round(nj/n*(n-sumn2)) + n2

Vmin<-opt1$value # sampling variance minimised


