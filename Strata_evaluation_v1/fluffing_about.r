

     # playing around...

                    #http://cran.r-project.org/doc/manuals/R-lang.html


strata_detail$o_st_ID <- as.factor(paste("o", strata_detail$o_st_ID, sep="_"))      #convert optimised strata to factor in strata_detail


 #add colums with areal proportions from strata_detail



 strata_detail$eq8.9d <-
 
 

 # multiplicatio by strata:
strata_detail$p_st_a <- with(strata_detail, (p_cell_n) * (cell_res^2))
strata_detail$p_st_ra <- (p_st_a/sum(p_st_a))
strata_detail$o_st_a <- with(strata_detail, (o_cell_n) * (cell_res^2))
strata_detail$o_st_ra <- (o_st_a/sum(o_st_a))
strata_detail$p_ah_nh <- with(strata_detail, (p_st_ra)/(p_count))

str(strata_detail)



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

8.9n <- match(


#View(s_st)

#other stuff to work out

eq_8.9d <- NULL       #denominator of Zrd (mean of intersection)
Zrd <- NULL           #Zrd = mean of new strata
n_hd <- NULL          #n_hd = count of intersecting points
count <-....