####################### Descriptions and Comments #################################################
# LASSO model
# See details in ResearchProject.pdf or ResearchProject.Rmd
###################################################################################################

library(reshape2)
library(ggplot2)
test = readRDS('Data.rds')
Ret = test[[1]][,c(1,8)]
Stocks = names(test)
colnames(Ret)[2] = Stocks[1]
L = length(test)
index = 141
for (i in 2:L) {
  # i = 2
  r = test[[i]][,c(1,8)]
  colnames(r)[2] = Stocks[i]
  r$date = as.Date(r$date)
  Ret = merge(Ret, r, by = "date",all = TRUE)
}
Ret = Ret[-1,]
#### the original value here is 406.9999, should be discarded
Ret[4925,205] = 0

saveRDS(Ret,"Ret.RDS")
# Sp500ret = cbind.data.frame(date = Ret$date, gain = rowMeans(Ret[,-1],na.rm = TRUE))
Dates = readRDS('dates.RDS')

library(glmnet)
win = 262


Ret = Ret[Ret$date>=Dates[1] & Ret$date<=Dates[2],]
# nObs = nrow(Ret)
nObs = 2000
start = which.min(is.na(Ret[,index+1]))+win-1
crossthres = Ret[1:nObs,1:3]
coefFlag = Ret[1:nObs,]
coefFlag[,2:(L+1)][] = 0
crossthres[,2:3][] = 0
colnames(crossthres)[2:3] = c("log_Lambda_MSE","log_Lambda_1se")

#### Perform Lasso every day on historical 1 year data
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
  #### set cutoff at 1se
  coefs <- coef(crossval, s = crossval$lambda.1se)
  #### ignore the case that no stock is picked
  if(length(coefs@i)>1){
  reg = lm(y~.,as.data.frame(x[,coefs@i[-1]]))
  coefFlag[i,which(names(coefFlag) %in% coefs@Dimnames[[1]][coefs@i[-1]])] = reg$coefficients[-1]
  }
}
######################### save the results #####################################################
saveRDS(crossthres,"Lambda.RDS")
saveRDS(coefFlag,"LassoCoef.RDS")
################################################################################################

crossthres = readRDS('Lambda.RDS')
coefFlag = readRDS('LassoCoef.RDS')
# coefdata = cbind.data.frame(date = coefFlag$date, ncoef = rowSums(coefFlag[,-1]!=0))

# LassoResult = cbind.data.frame(crossthres,Ret[1:nObs,index+1])
# LassoResult = LassoResult[which(coefdata$ncoef!=0),]
# LassoResult = LassoResult[which(LassoResult$`Ret[, index + 1]`!=0),]
# x = LassoResult$log_Lambda_MSE/LassoResult$`Ret[, index + 1]`
# coefdata = coefdata[which(coefdata$ncoef!=0),]

coefs = coefFlag[1:nObs,-1]
s = Ret[1:nObs,-1]
s[is.na(s)] = 0
explained = rowSums(coefs*s)
r = data.frame(date = Ret$date[1:nObs], explained = explained, actual = Ret$MMM[1:nObs], diff = abs(Ret$MMM[1:nObs] - explained))
r = r[-(1:start),]
r_stat = colMeans(r[,c(2,3,4)])
r_melted = melt(r[,1:3],id = 'date')

ggplot(r_melted,
      aes(x=date, y=value, colour=variable)) +
  geom_line(alpha=0.4)

ggplot(r, aes(x=date,y=diff)) +
  geom_line()

