# Purpose :  Evaluation of stratifications for study area ??;
# Last update : April 2012
# Note : testing different stratification calculus;

     #works#


#setwd ("D:/simple-stratif/Strata_evaluation_v1")
# setwd ("F:/R/Strata_evaluation")
strata_data <- read.csv("Input_data_structure.csv")
strata_detail <- read.csv("control_file.csv")
#View(strata_data)
#View(strata_detail)


#single inputs
cell_res <- 30 #resolution of grid cells in meters
p_samp_var_SI <- 0.16 #sampling variance from prior stratification as if the sample were random 
n <- 48  #actual sample size

# multiplication by strata: 
strata_detail$p_st_a <- with(strata_detail, (p_cell_n) * (cell_res^2))
strata_detail$p_st_ra <- with(strata_detail, (p_st_a)/(sum(p_st_a))) 
strata_detail$o_st_a <- with(strata_detail, (o_cell_n) * (cell_res^2))
strata_detail$o_st_ra <- with(strata_detail, (o_st_a)/(sum(o_st_a))) 
strata_detail$p_ah_nh <- with(strata_detail, (p_st_ra)/(p_count))
 
#str(strata_detail)


# Convert strata names to factors:
strata_data$p_st <- as.factor(strata_data$p_st)
strata_data$o_st <- as.factor(paste("o", strata_data$o_st, sep="_"))
strata_detail$o_st_ID <- as.factor(paste("o", strata_detail$o_st_ID, sep="_"))      #convert optimised strata to factor in strata_detail


#numerator of Zrd (mean of intersection), requires reference to cells in strata_detail based on prior area of intersecting strata & prior allocation
Zrd <- NULL       

for(j in 1:length(levels(strata_data$o_st))){
  x <- strata_data[strata_data$o_st==levels(strata_data$o_st)[j],c("p_st","pred_C","o_st")]
  Zrd[[levels(strata_data$o_st)[j]]] <- aggregate(x[,c("pred_C")], by=list(x$p_st), FUN=sum)
}

Zrd <- do.call(rbind, Zrd)
Zrd$o_st <- sapply(strsplit(row.names(Zrd), "\\."), function(x){x[1]})
Zrd$p_st <- Zrd$Group.1

Zrd$combo <- sapply(strsplit(row.names(Zrd), "\\_"), function(x){x[2]})


#Attach data from strata_detail$p_ah_nh ...rename column, merge & finalise
names(strata_detail)[names(strata_detail)=="o_st_ID"]="o_st"
names(strata_detail)[names(strata_detail)=="p_st_ID"]="p_st"
Zrd <- merge(strata_detail, Zrd, by="p_st")

names(Zrd)[names(Zrd)=="x"]="sum_c"
Zrd$o_st.x <- NULL
names(Zrd)[names(Zrd)=="o_st.y"]="o_st"
Zrd$Group.1 <- NULL
Zrd <- Zrd[order(Zrd$combo),]
Zrd$eq_8.9n <- with(Zrd, (p_ah_nh)*(sum_c))


#number of intersects per opt & prior stratum.
strata_data$occr <- (1)
strata_data$occr <- as.numeric(strata_data$occr)

n_hd = NULL

for(j in 1:length(levels(strata_data$o_st))){
  x <- strata_data[strata_data$o_st==levels(strata_data$o_st)[j],c("p_st", "occr", "o_st")]
  n_hd[[levels(strata_data$o_st)[j]]] <- aggregate(x[,c("occr")], by=list(x$p_st), FUN=sum)
}

n_hd <- do.call(rbind, n_hd)
Zrd$n_hd <- n_hd$x

n_hd = NULL

Zrd$eq_8.9d <-with(Zrd, (p_st_ra)*((n_hd)/(p_count)))

# ah = (ah^2/(nh(nh-1))  

Zrd$ah <- with(Zrd,((p_st_ra)^2)/(p_count*(p_count-1)))

# aggregate to areal mean for optimal stratif. & merge strata_data with optim_stata   
optim_strata = NULL

Zrd$o_st <- as.factor(Zrd$o_st)

for(j in 1:length(levels(Zrd$o_st))){
  x <- Zrd[Zrd$o_st==levels(Zrd$o_st)[j],c("o_st", "eq_8.9n", "eq_8.9d")]
  optim_strata[[levels(Zrd$o_st)[j]]] <- aggregate(x[,c("eq_8.9n")], by=list(x$o_st), FUN=sum)
  
}

optim_strata <- do.call(rbind, optim_strata)
optim_strata$eq_8.9n <- optim_strata$x

eq_8.9d <-NULL
for(j in 1:length(levels(Zrd$o_st))){
  x <- Zrd[Zrd$o_st==levels(Zrd$o_st)[j],c("o_st", "eq_8.9n", "eq_8.9d")]
  eq_8.9d[[levels(Zrd$o_st)[j]]] <- aggregate(x[,c("eq_8.9d")], by=list(x$o_st), FUN=sum)
  
}

eq_8.9d <- do.call(rbind, eq_8.9d)

optim_strata$eq_8.9d <- eq_8.9d$x

eq_8.9d <-NULL

#Means of optimal strata  
optim_strata$Zrd <- with(optim_strata, (optim_strata$eq_8.9n)/(optim_strata$eq_8.9d))

# calc zshd and join to Zrd 

Zshd <- NULL

for(j in 1:length(levels(strata_data$o_st))){
  x <- strata_data[strata_data$o_st==levels(strata_data$o_st)[j],c("p_st","pred_C","o_st")]
  Zshd[[levels(strata_data$o_st)[j]]] <- aggregate(x[,c("pred_C")], by=list(x$p_st), FUN=mean)
}

Zshd <- do.call(rbind, Zshd)
Zrd$Zshd <- cbind(Zshd$x)


### xx2 = (Zhdi-Zshd)^2 

  #create columns in Zshd
  Zshd$o_st <- sapply(strsplit(row.names(Zshd), "\\."), function(x){x[1]})
  Zshd$p_st <- Zshd$Group.1
  Zshd$Zshd <- Zshd$x


  #join Zshd to Zrd
  strata_data$combo <- paste(strata_data$o_st, strata_data$p_st, sep = ".", collapse = NULL)
  Zshd$combo <- paste(Zshd$o_st, Zshd$p_st, sep = ".", collapse = NULL )
  strata_data <- merge(strata_data, Zshd, by = "combo") 

  #clean excess cols & rename
  
  strata_data$o_st.y <- NULL
  strata_data$p_st.y <- NULL
  strata_data$x <- NULL
  
  names(strata_data)[names(strata_data)=="o_st.x"]="o_st"
  names(strata_data)[names(strata_data)=="p_st.x"]="p_st"

  strata_data$xx2 <- with(strata_data, ((pred_C)-(Zshd))^2)   

Zshd <- NULL
  
# xx3 = nh(1-nhd/nh)
Zrd$xx3 <- with(Zrd, (p_count)*(1-((n_hd)/(p_count))))


#attach stratum averages 

Zrd <- merge(Zrd, optim_strata, by.x = "o_st", by.y = "Group.1")
Zrd$eq_8.9n.y <- NULL
Zrd$eq_8.9d.y <- NULL
Zrd$x <- NULL

# xx4 = (Zshd-zRd)^2
Zrd$xx4 <- with(Zrd, ((Zshd)-(Zrd))^2)


# xx5  
  #sum xx2 per stratum

sum_xx2 <- NULL

for(j in 1:length(levels(strata_data$o_st))){
  x <- strata_data[strata_data$o_st==levels(strata_data$o_st)[j],c("p_st","xx2","o_st")]
  sum_xx2[[levels(strata_data$o_st)[j]]] <- aggregate(x[,c("xx2")], by=list(x$p_st), FUN=sum)
}

sum_xx2 <- do.call(rbind, sum_xx2)
Zrd$sum_xx2 <- cbind(sum_xx2$x)

Zrd$xx5 <- with(Zrd, ((ah)*((sum_xx2)+(xx3)*(xx4))))

sum_xx2 <- NULL

# sub_xx6, sum_xx5 & xx6

Zrd$sub_xx6 <- with(Zrd,(p_st_ra)*((n_hd)/(p_count)))

xx5 = NULL

Zrd$o_st <- as.factor(Zrd$o_st)

for(j in 1:length(levels(Zrd$o_st))){
  x <- Zrd[Zrd$o_st==levels(Zrd$o_st)[j],c("o_st", "xx5", "sub_xx6")]
  xx5[[levels(Zrd$o_st)[j]]] <- aggregate(x[,c("xx5")], by=list(x$o_st), FUN=sum)
  
}

xx5 <- do.call(rbind, xx5)
optim_strata$xx5 <- xx5$V1

xx5 = NULL

xx6 = NULL

for(j in 1:length(levels(Zrd$o_st))){
  x <- Zrd[Zrd$o_st==levels(Zrd$o_st)[j],c("o_st", "xx5", "sub_xx6")]
  xx6[[levels(Zrd$o_st)[j]]] <- aggregate(x[,c("sub_xx6")], by=list(x$o_st), FUN=sum)
  
}

xx6 <- do.call(rbind, xx6)
optim_strata$xx6 <- xx6$x

xx6 = NULL

# calc VZrd 

optim_strata$VZrd <- with(optim_strata, ((1/(xx6)^2)*(xx5)))

#standard calcs 

optim_strata$se <- with(optim_strata, sqrt(VZrd))

optim_strata$o_st_ra <- strata_detail$o_st_ra

optim_strata$r.Zrd <- with(optim_strata, (o_st_ra)*(Zrd))


# averages with sqare values - make eq_8.9n & eq_8.9n (s_n2; s_d2) as sums of squares
  
  strata_data$pred_C2 <- with(strata_data, (pred_C)^2)

s_n2 <- NULL

for(j in 1:length(levels(strata_data$o_st))){
  x <- strata_data[strata_data$o_st==levels(strata_data$o_st)[j],c("p_st","pred_C2","o_st")]
  s_n2[[levels(strata_data$o_st)[j]]] <- aggregate(x[,c("pred_C2")], by=list(x$p_st), FUN=sum)
}

s_n2 <- do.call(rbind, s_n2)
s_n2$o_st <- sapply(strsplit(row.names(s_n2), "\\."), function(x){x[1]})

Zrd$sub_s_n2 <- s_n2$x

Zrd$eq_8.9n2 <- with(Zrd, (sub_s_n2)*((p_st_ra)/(p_count)))

s_n2 <- NULL

            # Zrd$s_d2 <- Zrd$eq_8.9d.x   therefore use corresponding in optim_strata
  
eq_8.9n2 <-NULL
for(j in 1:length(levels(Zrd$o_st))){
  x <- Zrd[Zrd$o_st==levels(Zrd$o_st)[j],c("o_st", "eq_8.9n2")]
  eq_8.9n2[[levels(Zrd$o_st)[j]]] <- aggregate(x[,c("eq_8.9n2")], by=list(x$o_st), FUN=sum)
  
}

eq_8.9n2 <- do.call(rbind, eq_8.9n2)

optim_strata$eq_8.9n2 <- eq_8.9n2$x

eq_8.9n2 <-NULL

optim_strata$Zrd2 <- with(optim_strata, (eq_8.9n2)/(eq_8.9d))
optim_strata$sp_var <- with(optim_strata, (Zrd2)-((Zrd)^2)+(VZrd))
optim_strata$samp_var <- optim_strata$sp_var / strata_detail$p_o_intsect  
optim_strata$o_st_ra <- cbind(strata_detail$o_st_ra)

optim_strata$samp_var.r <- with(optim_strata, ((samp_var) * (o_st_ra)^2))

#for the stratif_perform

stratif_perform <- NULL

x<-data.frame(o_Zrd=sum(optim_strata$r.Zrd),o_samp_var=sum(optim_strata$samp_var.r))

stratif_perform <- as.data.frame(stratif_perform)
stratif_perform$design_eff <- p_samp_var_SI/(stratif_perform$o_samp_var)
stratif_perform$n_eq <- stratif_perform$design_eff * n
stratif_perform$eff_pct <- with(stratif_perform, (n_eq)/(n/100))


# end of script;