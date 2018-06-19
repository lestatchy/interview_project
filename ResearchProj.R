Ret = test[[1]][,c(1,8)]
colnames(Ret)[2] = Stocks[1]
for (i in 2:L) {
  # i = 2
  r = test[[i]][,c(1,8)]
  colnames(r)[2] = Stocks[i]
  Ret = merge(Ret, r, by = "date",all = TRUE)
}
Ret = Ret[-1,]
Sp500ret = cbind.data.frame(date = Ret$date, gain = rowMeans(Ret[,-1],na.rm = TRUE))

library(glmnet)

nObs = nrow(Ret)
win = 262

which.min(is.na(Ret[,index+1]))
start = which.min(is.na(Ret[,index+1]))+win-1


for(i in start:nObs){
  i = start
  y = matrix(Ret[i:(i-win+1),index+1])
  x = as.matrix(as.data.frame(lapply(Ret[i:(i-win+1),-c(1,index+1)],as.numeric)))
  lasso = glmnet(x, y, alpha = 1)
}
