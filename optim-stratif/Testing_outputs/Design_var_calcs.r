
initbh <- min (PRED_OC$oc_kgm3) + (1:(Ls-1)) * (max((PRED_OC$oc_kgm3) - min (PRED_OC$oc_kgm3)) / Ls)
initbh

Ls = 5


x <- strata.LH(PRED_OC$oc_kgm3, initbh, n = 48, CV = NULL, Ls, certain = NULL,
alloc = list(q1 = 0.5, q2 = 0, q3 = 0.5), takenone = 0,
bias.penalty = 1, takeall = 0, rh = rep(1, Ls),
model = c("none"),
model.control = list(), algo = c("Kozak"),
algo.control = list(method="modified"))

outpt <- table(strata = c(1,2,3,4,5), Ah = (x$Nh/sum(x$Nh)), nh = x$nh, bh = c((min(PRED_OC$oc_kgm3)), x$bh), varh = x$varh)