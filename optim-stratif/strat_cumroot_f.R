PRED_OC <- read.csv("PRED_OC.csv")

library("stratification")

strata.cumrootf(x=PRED_OC$oc_kgm3, nclass=5055, n = 48, CV = NULL, Ls = 6, certain = NULL,
                alloc = list(q1 = 0.5, q2 = 0, q3 = 0.5), rh = rep(1, Ls=6),
                model = c("none"),
                model.control = list())

#Output# 

############################ DO NOT RUN #########################

Strata information:
          rh |    bh anticip. mean anticip. var   Nh nh   fh
stratum 1  1 | 14.99         13.78         0.51 1054  9 0.01
stratum 2  1 | 18.02         16.28         0.81  786  8 0.01
stratum 3  1 | 20.95         19.55         0.72  782  8 0.01
stratum 4  1 | 23.66         22.26         0.58  849  7 0.01
stratum 5  1 | 26.60         25.20         0.71  798  8 0.01
stratum 6  1 | 35.74         27.86         0.68  786  8 0.01
Total                                           5055 48 0.01

Total sample size: 48 
Anticipated population mean: 20.47737 
Anticipated CV: 0.005677275

#####Output of Stratif_optim #####

Strata  StD_i      Var_i	      rel_area_i	alloc_i	bound_i
1	    1	  0.3638064	0.13235511	0.02769535	    2	    0.1666667
2	    2	  0.2594449	0.06731165	0.08308605	    3	    0.3333333
3	    3	  0.4977115	0.24771671	0.13808111	    4	    0.5000000
4	    4	  1.2532261	1.57057576	0.19505440	    10	  0.6666667
5	    5	  1.1684745	1.36533261	0.25044510	    12	  0.8333333
6	    6	  1.5263276	2.32967595	0.30563798	    17	  1.0000000

############################ DO NOT RUN #########################


