# Purpose :  Evaluation of stratifications for study area ??;
# Last update : April 2012
# Note : testing different stratification calculus;


# setwd ("F:/R/Strata_evaluation")

strata_data <- read.csv("Input_data_structure.csv")
strata_detail <- read.csv("control_file.csv")
View(strata_data)
View(strata_detail)

#single inputs
cell_res <- 30 #resolution of grid cells in meters

# multiplication by strata: 
strata_detail$p_st_a <- with(strata_detail, (p_cell_n) * (cell_res^2))
strata_detail$p_st_ra <- with(strata_detail, (p_st_a)/(sum(p_st_a))) 
strata_detail$o_st_a <- with(strata_detail, (o_cell_n) * (cell_res^2))
strata_detail$o_st_ra <- with(strata_detail, (o_st_a)/(sum(o_st_a))) 
strata_detail$p_ah_nh <- with(strata_detail, (p_st_ra)/(p_count))
 
str(strata_detail)

# Convert strata names to factors:
strata_data$p_st <- as.factor(strata_data$p_st)
strata_data$o_st <- as.factor(paste("o", strata_data$o_st, sep="_"))
strata_detail$o_st_ID <- as.factor(paste("o", strata_detail$o_st_ID, sep="_"))      #convert optimised strata to factor in strata_detail

#numerator of Zrd (mean of intersection), requires reference to cells in starta_detail based on prior area of intersecting strata & prior allocation
eq_8.9n <- NULL       

for(j in 1:length(levels(strata_data$o_st))){
  x <- strata_data[strata_data$o_st==levels(strata_data$o_st)[j],c("p_st","pred_C","o_st")]
  eq_8.9n[[levels(strata_data$o_st)[j]]] <- aggregate(x[,c("pred_C")], by=list(x$p_st), FUN=sum)
}

eq_8.9n <- do.call(rbind, eq_8.9n)
eq_8.9n$o_st <- sapply(strsplit(row.names(eq_8.9n), "\\."), function(x){x[1]})

#Attach data from strata_detail$p_ah_nh ...rename column, merge & finalise
names(strata_detail)[names(strata_detail)=="o_st_ID"]="o_st"
eq_8.9n <- merge(eq_8.9n, strata_detail, by="o_st")
eq_8.9n$eq_8.9n <- with(eq_8.9n, (p_ah_nh)*(x))










# Optimize stratum averages per strata
n_st <- NULL
s_st <- NULL
for(j in 1:length(levels(strata_data$o_st))){
      x <- strata_data[strata_data$o_st==levels(strata_data$o_st)[j],c("p_st","pred_C","o_st")]
      n_st[[levels(strata_data$o_st)[j]]] <- aggregate(x[,c("pred_C")], by=list(x$p_st), FUN=mean)
      s_st[[levels(strata_data$o_st)[j]]] <- aggregate(x[,c("pred_C")], by=list(x$p_st), FUN=var)
}
n_st <- do.call(rbind, n_st)
n_st$o_st <- sapply(strsplit(row.names(n_st), "\\."), function(x){x[1]})
s_st <- do.call(rbind, s_st)
s_st$o_st <- sapply(strsplit(row.names(s_st), "\\."), function(x){x[1]})
View(n_st)
View(s_st)


# TH: these are just simple examples; you need to adjust the code to fit your purpose;


n_st[n_st$o_st=="c_1",]

# end of script;