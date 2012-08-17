

#----------------------------------
# STEP 1: Data prep - all farms & C prediction versions (C maps)  import, project & compress
#----------------------------------


#---------------
# Farm 1: Spring
#---------------
# import the predicted values:
PRED_OC <- read.csv("PRED_OC.csv")
str(PRED_OC)
# sort data in ascending order
PRED_OC <- PRED_OC[order(PRED_OC$oc_kgm3),]
#project, compress and save
xsp <- PRED_OC
gridded(xsp) <- ~x+y
spplot(xsp["strata_name"])
xsp$strata_name_i <- as.integer(xsp$strata_name)
writeGDAL(xsp["strata_name_i"], "strata_name.sdat", "SAGA", mvFlag=-99999)
springf <- xsp["oc_kgm3"]
names(springf) <- "SOC"
proj4string(springf) <- CRS("+init=epsg:28355")
springf <- data.frame(springf)
save(springf, file="springf.rda", compress="xz")


#---------------
# Farm 2: Inver
#---------------

# import the predicted values
Inver_OC <- read.csv("Inver_OC.csv")
str(Inver_OC)
# sort data in ascending order
Inver_OC <- Inver_OC[order(Inver_OC$C_pct_1m),]
# project and save
xsp <- Inver_OC
gridded(xsp) <- ~x+y
spplot(xsp["C_pct_1m"])
xsp$C_pct_1m_i <- as.integer(xsp$C_pct_1m)
writeGDAL(xsp["C_pct_1m_i"], "C_pct_1m.sdat", "SAGA", mvFlag=-99999)
inverf <- xsp["C_pct_1m"]
names(inverf) <- "C_pct_1m"
proj4string(inverf) <- CRS("+init=epsg:28355")
inverf <- data.frame(inverf)
save(inverf, file="inverf.rda", compress="xz")

#---------------
# Farm 3: Wanga
#---------------

Wanga_OC <- read.csv("Wanga_OC.csv")
str(Wanga_OC)
# sort data in ascending order
Wanga_OC <- Wanga_OC[order(Wanga_OC$C_pct_1m),]
# project and save
xsp <- Wanga_OC
gridded(xsp) <- ~x+y
spplot(xsp["C_pct_1m"])
xsp$C_pct_1m_i <- as.integer(xsp$C_pct_1m)
writeGDAL(xsp["C_pct_1m_i"], "C_pct_1m.sdat", "SAGA", mvFlag=-99999)
Wangaf <- xsp["C_pct_1m"]
names(Wangaf) <- "C_pct_1m"
proj4string(Wangaf) <- CRS("+init=epsg:28355")
Wangaf <- data.frame(Wangaf)
save(Wangaf, file="Wangaf.rda", compress="xz")

#---------------
# Farm 4: Winona
#---------------

Winon_OC <- read.csv("Winona_OC.csv")
str(Winon_OC)
# sort data in ascending order
Winon_OC <- Winon_OC[order(Winon_OC$C_pct_1m),]
# project and save
xsp <- Winon_OC
gridded(xsp) <- ~X+Y
spplot(xsp["C_pct_1m"])
xsp$C_pct_1m_i <- as.integer(xsp$C_pct_1m)
writeGDAL(xsp["C_pct_1m_i"], "C_pct_1m.sdat", "SAGA", mvFlag=-99999)
Winonf <- xsp["C_pct_1m"]
names(Winonf) <- "C_pct_1m"
proj4string(Winonf) <- CRS("+init=epsg:28355")
Winonf <- data.frame(Winonf)
save(Winonf, file="Winonf.rda", compress="xz")

