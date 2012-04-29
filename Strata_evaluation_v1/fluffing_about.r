

     # playing around...

                    #http://cran.r-project.org/doc/manuals/R-lang.html




 strata_detail$eq8.9d <-
 
 

 # multiplicatio by strata:



eq_8.9n <- NULL       #numerator of Zrd (mean of intersection), requires reference to cells in starta_detail based on prior area of intersecting strata & prior allocation


for(j in 1:length(levels(strata_data$o_st))){
      x <- strata_data[strata_data$o_st==levels(strata_data$o_st)[j],c("p_st","pred_C","o_st")]
      eq_8.9n[[levels(strata_data$o_st)[j]]] <- aggregate(x[,c("pred_C")], by=list(x$p_st), FUN=sum)
      

     # s_st[[levels(strata_data$o_st)[j]]] <- aggregate(x[,c("pred_C")], by=list(x$p_st), FUN=var)
}
eq_8.9n <- do.call(rbind, eq_8.9n)
eq_8.9n$o_st <- sapply(strsplit(row.names(eq_8.9n), "\\."), function(x){x[1]})
#s_st <- do.call(rbind, s_st)
#s_st$o_st <- sapply(strsplit(row.names(s_st), "\\."), function(x){x[1]})
View(eq_8.9n)

#merge??

strata_detail <- strata_detail #not needed if script is run from beginning
eq_8.9n <- merge(eq_8.9n, strata_detail, by="o_st")

# strata_detail$p_st_a <- with(strata_detail, (p_cell_n) * (cell_res^2))
eq_8.9n$eq_8.9n <- with(eq_8.9n, (p_ah_nh)*(x))
 
 #now need number of intersects per stratum... n_hd to be attached to eq_8.9n
 
 n_hd = NULL

 strata_data$occr <- (1)
 strata_data$occr <- as.numeric(strata_data$occr)
 

 for(j in 1:length(levels(strata_data$o_st))){
  y <- strata_data[strata_data$o_st==levels(strata_data$o_st)[j],c("p_st", "occr", "o_st")]
   n_hd[[levels(strata_data$o_st)[j]]] <- aggregate(y[,c("occr")], by=list(y$p_st), FUN=sum)
 }
 
 
 
 
n_hd <- do.call(rbind, n_hd)
 n_hd$o_st <- sapply(strsplit(row.names(n_hd), "\\."), function(x){x[1]})
  
 ##
 
 eq_8.9n <- NULL       
 
 for(j in 1:length(levels(strata_data$o_st))){ 
   x <- strata_data[strata_data$o_st==levels(strata_data$o_st)[j],c("p_st","pred_C","o_st")]
   eq_8.9n[[levels(strata_data$o_st)[j]]] <- aggregate(x[,c("pred_C")], by=list(x$p_st), FUN=sum)
 }
 
 eq_8.9n <- do.call(rbind, eq_8.9n)
 eq_8.9n$o_st <- sapply(strsplit(row.names(eq_8.9n), "\\."), function(x){x[1]})
 
 ##
 
 
 grp = c(1,1,1, 1,2, 2,2) 
 val = c(2,1,5,NA,3,NA,1) 
 dta = data.frame(grp=grp, val=val) 
 ddply(dta,"grp",summarise,count=length(na.omit(val))) 
 
 
  
  aggregate(x[,c("p_st")], by=list(x$p_st), FUN=count)
   
   
 i <- aggregate(strata_data$p_st - strata_data$o_st, n_hd, length)
  
  
 y <- length(levels(strata_data$o_st))
 
 
 
 
 
 
 
 
 
 
 #use plyr...?
 #split into list 
p_ah_nh <- split(eq_8.9n, list(eq_8.9n$o_st, eq_8.9n$Group.1)
 #apply function
bound <- vector("list", length(p_ah_nh))
                 for(i in seq_along(p_ah_nh)) {
                   p_ah_nh <- p_ah_nhs[[i]]
                   p_ah_nh <- transform(p_ah_nh,
                                        rank = ...
                 }

#eq_8.9n$p_ah_nh <- match(eq_8.9n$o_st,strata_detail$p_ah_nh) #uneven strings 
  

eq_8.9n$p_ah_nh <- if(eq_8.9n$o_st==strata_detail$o_st_ID) {eq_8.9n$p_ah_nh <- strata_detail$p_ah_nh} #row conflicts..

eq_8.9n$p_ah_nh <- if(eq_8.9n$o_st==strata_detail$o_st_ID) {match eq_8.9n$p_ah_nh <- strata_detail$p_ah_nh} #row conflicts..
 
#merge...
                 
#View(s_st)

#other stuff to work out

eq_8.9d <- NULL       #denominator of Zrd (mean of intersection)
Zrd <- NULL           #Zrd = mean of new strata
n_hd <- NULL          #n_hd = count of intersecting points
count <-....