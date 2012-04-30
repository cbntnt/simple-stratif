

     # playing around...

#notes....

                    #http://cran.r-project.org/doc/manuals/R-lang.html


#working bits...

    #merge - of different length vectors (successful)
      eq_8.9n <- merge(eq_8.9n, strata_detail, by="o_st")

    #with - simple math 
      eq_8.9n$eq_8.9n <- with(eq_8.9n, (p_ah_nh)*(x))


#not working but interesting..
      
      #rename col x in eq_8.9n to sum_c
      # names( X)[ names[ X] == "bob"]<-"sue"
      names(eq_8.9n)[names[eq_8.9n] == "x"] <- "sum_C" #Error in names[eq_8.9n] : object of type 'builtin' is not subsettable
      
 
      
#####################################################################################
      
      
#rename eq_8.9n$x to sum_c
      
     eq_8.9n$sum_c <- eq_8.9n$x
      
     eq_8.9n$x <- NULL 

#eq_8.9d
      
      eq_8.9d <- Zrd
      
        eq_8.9d
      
 #now need number of intersects per stratum... n_hd to be attached to eq_8.9n
 
 strata_data$occr <- (1)
 strata_data$occr <- as.numeric(strata_data$occr)
 
 n_hd = NULL

 for(j in 1:length(levels(strata_data$o_st))){
  x <- strata_data[strata_data$o_st==levels(strata_data$o_st)[j],c("p_st", "occr", "o_st")]
   n_hd[[levels(strata_data$o_st)[j]]] <- aggregate(x[,c("occr")], by=list(y$p_st), FUN=sum)
 }
 
  
 n_hd <- do.call(rbind, n_hd)


###### #attach p_st
      
      Zrd <- NULL       
      
      for(j in 1:length(levels(strata_data$o_st))){
        x <- strata_data[strata_data$o_st==levels(strata_data$o_st)[j],c("p_st","pred_C","o_st")]
        Zrd[[levels(strata_data$o_st)[j]]] <- aggregate(x[,c("pred_C")], by=list(x$p_st), FUN=sum)
      }
      
      Zrd <- do.call(rbind, Zrd)
      Zrd$o_st <- sapply(strsplit(row.names(Zrd), "\\."), function(x){x[1]})
  # include p_st...
      Zrd$p_st <- sapply(strsplit(row.names(Zrd), "\\."), function(x){x[2]})
     
      
 ##
 
 
 grp = c(1,1,1, 1,2, 2,2) 
 val = c(2,1,5,NA,3,NA,1) 
 dta = data.frame(grp=grp, val=val) 
 ddply(dta,"grp",summarise,count=length(na.omit(val))) 
 
 
  
  aggregate(x[,c("p_st")], by=list(x$p_st), FUN=..)
   
   
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