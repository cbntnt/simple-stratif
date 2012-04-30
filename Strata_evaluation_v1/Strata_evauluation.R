# Purpose :  Evaluation of stratifications for study area ??;
# Last update : April 2012
# Note : testing different stratification calculus;


# setwd ("F:/R/Strata_evaluation")
strata_data <- read.csv("Input_data_structure.csv")
strata_detail <- read.csv("control_file.csv")
#View(strata_data)
#View(strata_detail)


#single inputs
cell_res <- 30 #resolution of grid cells in meters


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

  
# xx3 = nh(1-nhd/nh)
Zrd$xx3 <- with(Zrd, (p_count)*(1-((n_hd)/(p_count))))



#...................to here................


# xx4 = (Zshd-zRd)^2
Zrd$xx4 <- with(Zrd, (Zshd)-(Zrd))


#...................to here................  







# end of script;