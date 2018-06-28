index = 141
Ret = readRDS("Ret.RDS")

nObs = nrow(Ret)
# nObs = 2620
RetData = Ret[,-1]
nVar = ncol(RetData)-1
Z = unlist(RetData[index])[1:nObs]
# library(xts)
# Z = xts(Z,order.by = Ret$date)

library(wavelets)
wt = dwt(Z)
wt = align(wt)
p = plot(wt)
