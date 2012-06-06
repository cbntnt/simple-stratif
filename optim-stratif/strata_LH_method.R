
PRED_OC <- read.csv("PRED_OC.csv")

library("stratification")

Ls <- 6 #number of strata
initbh <- min (PRED_OC$oc_kgm3) + (1:(Ls-1)) * (max((PRED_OC$oc_kgm3) - min (PRED_OC$oc_kgm3)) / Ls)
initbh

output<-strata.LH(x=PRED_OC$oc_kgm3, initbh = initbh, n = 48, CV = NULL, Ls = Ls, certain = NULL,
          alloc = list(q1 = 0.5, q2 = 0, q3 = 0.5), takenone = 0,
          bias.penalty = 1, takeall = 0, rh = rep(1, Ls = 6),
          model = c("none"),
          model.control = list(), algo = c("Kozak"),
          algo.control = list())


### exclude ###
cutp<-output$bh
nj<-output$nh
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
nj <- floor(nj)  
#  n2<-matrix(2,nrow=strat,ncol=1)  
#  sumn2 <- sum(n2)
#  nj <- round(nj/n*(n-sumn2)) + n2
sum(aj^2*Vj/nj)

#exclude###

############################ DO NOT RUN #########################

Strata information:
          rh initbh |    bh anticip. mean anticip. var   Nh nh   fh
stratum 1  1  15.35 | 14.88         13.73         0.48 1011  8 0.01
stratum 2  1  18.30 | 17.69         16.09         0.70  770  7 0.01
stratum 3  1  21.12 | 20.73         19.29         0.72  766  8 0.01
stratum 4  1  23.93 | 23.62         22.14         0.67  918  9 0.01
stratum 5  1  26.80 | 26.39         25.07         0.62  740  7 0.01
stratum 6  1  35.74 | 35.74         27.76         0.76  850  9 0.01
Total                                                  5055 48 0.01

Total sample size: 48 
Anticipated population mean: 20.47737 
Anticipated CV: 0.005648659 
Note: CV=RRMSE (Relative Root Mean Squared Error) because takenone=0.
Warning messages:
  1: In strata.LH(x = PRED_OC$oc_kgm3, initbh = initbh, n = 48, CV = NULL,  :
  some initial sampled strata contain less than minNh units : robust initial boundaries have been used
  2: In strata.LH(x = PRED_OC$oc_kgm3, initbh = initbh, n = 48, CV = NULL,  :
   some initial sampled strata have non-positive nh : robust initial boundaries have been used
                              
############################ DO NOT RUN #########################                  