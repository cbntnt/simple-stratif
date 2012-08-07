
PRED_OC <- read.csv("PRED_OC.csv")

Ls = 5

initbh <- min (PRED_OC$oc_kgm3) + (1:(Ls-1)) * (max((PRED_OC$oc_kgm3) - min (PRED_OC$oc_kgm3)) / Ls)
initbh

x <- strata.LH(PRED_OC$oc_kgm3, initbh, n = 48, CV = NULL, Ls, certain = NULL,
alloc = list(q1 = 0.5, q2 = 0, q3 = 0.5), takenone = 0,
bias.penalty = 1, takeall = 0, rh = rep(1, Ls),
model = c("none"),
model.control = list(), algo = c("Kozak"),
algo.control = list(method="modified", minNh=3)
)

outpt <- data.frame(strata = seq(along=x$Nh), Nh = x$Nh, Ah = x$Nh/sum(x$Nh), nh = x$nh, initbh = c((min(PRED_OC$oc_kgm3)), x$initbh), bh = c((min(PRED_OC$oc_kgm3)), x$bh), p_var = x$varh, smpvar = x$varh/x$nh, desvar = (x$Nh/(sum(x$Nh)))^2 * (x$varh/x$nh))

write.table(outpt, "outpt.txt", row.names=FALSE, col.names=TRUE, sep = " ")  

