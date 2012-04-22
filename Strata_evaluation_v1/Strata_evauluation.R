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

# multiplicatio by strata: 
strata_detail$p_st_a <- with(strata_detail, (p_cell_n) * (cell_res^2))
strata_detail$p_st_ra <- (p_st_a/sum(p_st_a)) 
strata_detail$o_st_a <- with(strata_detail, (o_cell_n) * (cell_res^2))
strata_detail$o_st_ra <- (o_st_a/sum(o_st_a)) 
str(strata_detail)

# Convert strata names to factors:
strata_data$p_st <- as.factor(strata_data$p_st)
strata_data$o_st <- as.factor(paste("o", strata_data$o_st, sep="_"))
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