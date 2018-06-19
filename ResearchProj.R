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

# which.min(is.na(Ret[,index+1]))
start = which.min(is.na(Ret[,index+1]))+win-1
crossthres = Ret[,1:3]
coefFlag = Ret
coefFlag[,2:(L+1)][] = 0
crossthres[,2:3][] = 0
colnames(crossthres)[2:3] = c("log_Lambda_MSE","log_Lambda_1se")
for(i in start:nObs){
  # i = start
  y = as.matrix(Ret[(i-win+1):i,index+1])
  x = as.matrix(Ret[(i-win+1):i,-c(1,index+1)])
  # x = as.matrix(Ret[i:(i-win+1),2:235])
  x = x[ , colSums(is.na(x)) == 0]
  
  # lasso = glmnet(x, y, alpha = 1)
  crossval = cv.glmnet(y = y, x = x)
  crossthres$log_Lambda_MSE[i] = crossval$lambda.min
  crossthres$log_Lambda_1se[i] = crossval$lambda.1se
  coefs <- coef(crossval, s = crossval$lambda.min)
  coefFlag[i,which(names(coefFlag) %in% coefs@Dimnames[[1]][coefs@i[-1]])] = coefs@x[-1]
}
saveRDS(crossthres,"Lambda.RDS")
saveRDS(coefFlag,"LassoCoef.RDS")

coefdata = cbind.data.frame(date = coefFlag$date, ncoef = rowSums(coefFlag[,-1]!=0))
coefdata = coefdata[which(coefdata$ncoef!=0),]
mean(coefdata$ncoef)
sd(coefdata$ncoef)

for(i in (start+1):nObs){
  i = start
  y = Ret[i,-1]
  x = Ret[(i-win):(i-1),-1]
  # x = as.matrix(Ret[i:(i-win+1),2:235])
  x = x[,which(colSums(is.na(x)) == 0)]
  y = as.numeric(y[,which(colSums(is.na(x)) == 0)])
  x = colMeans(x)

  coefs <- coef(crossval, s = crossval$lambda.min)
  coefFlag[i,which(names(coefFlag) %in% coefs@Dimnames[[1]][coefs@i[-1]])] = coefs@x[-1]
}
