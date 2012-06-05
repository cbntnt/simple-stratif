


PRED_OC <- read.csv("PRED_OC.csv")

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

#write data 
data <- cbind(PRED_OC, sqf, strata_name)
colnames(data) <- c("x", "y", "OC_kgm2", "Sqrt(cdf)", "strata_i")
#write data

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

###write data
strata_output_i <- cbind((seq(1,strat)), Sj, Vj, aj, nj, bound1)
colnames(strata_output_i) <- c("Strata", "StD_i", "Var_i", "rel_area_i", "alloc_i", "bound_i")
###write data


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
nj<-opt1$par[6:11] # change as required

nj <- round(nj)  
n2<-matrix(2,nrow=strat,ncol=1)  
sumn2 <- sum(n2)
nj <- round(nj/n*(n-sumn2)) + n2

Vmin<-opt1$value # sampling variance minimised


###write data

strata_output_o <- cbind(Vj, aj, nj, bound)
colnames(strata_output_o) <- c("Var_o", "rel_area_o", "alloc_o", "bound_o")

strata_detail <- cbind(strata_output_i, strata_output_o)

###write data




## to here ##


###write data 
optim_perform <- cbind(SV1, Vmin)
colnames(optim_perform) <- c("Var", "Var_optim")
###write data


# output needed... 
#   SV1 = initial variance; Vmin = optimised variance; 
#   Vj = initial stratum variance (pre-optim);  Vj.. stratum optim (post optim)
#   bound1 = initial boundaries; bound = optimised boundaries; (plus equivalent OC values)
#   nj = Neyman alloc; n2 = optimised allocation 


######

#output <- cbind((seq(1,strat)) , Sj, Vj, aj, nj)
#colnames(output) <- c("Strata", "StD", "Var", "cells", "alloc")



#


write.table(Vj, file = "test.txt", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")


