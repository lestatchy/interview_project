
####################### Descriptions and Comments #################################################
# factor model
# See details in ResearchProject.pdf or ResearchProject.Rmd
###################################################################################################

library(TSA)
library(data.table)

######################### 1-Factor Model ###############################################################
#### data cleaning
test = readRDS('Data.rds')
Ret = test[[1]][,c(1,8)]
Stocks = names(test)
colnames(Ret)[2] = Stocks[1]
L = length(test)
index = 141
for (i in 2:L) {
  # i = 141
  r = test[[i]][,c(1,8)]
  colnames(r)[2] = Stocks[i]
  r$date = as.Date(r$date)
  Ret = merge(Ret, r, by = "date",all.x = TRUE)
}
Ret = Ret[-1,]



#### market return
Sp500ret = cbind.data.frame(date = Ret$date, gain = rowMeans(Ret[,-1],na.rm = TRUE))

#### stock return
MMM = data.frame(date = Ret$date, gain = Ret$MMM)
# Dates = readRDS('dates.RDS')
# Ret_ex = Ret
# Ret_ex[,-1] = Ret[,-1]-Sp500ret$gain
# MMM_TS = MMM
# rownames(MMM_TS) = MMM$date
# MMM_TS = xts(MMM$gain, order.by=MMM$date)

#### rolling window
win = 262
betas = data.frame(date = NULL, intercept = NULL, beta = NULL, unexplained = NULL)
for (i in win:nrow(MMM)) {
  # i = 263
  y = MMM$gain[(i-win+1):i]
  x = Sp500ret$gain[(i-win+1):i]
  reg = lm(y~x)
  tmp = data.frame(date = MMM$date[i], intercept = reg$coefficients[1], 
                   beta = reg$coefficients[2], unexplained = reg$residuals[win])
  betas =rbind.data.frame(betas,tmp)
}
f1 = merge(betas,MMM, by='date')
f1 = data.frame(explained = f1$gain-f1$unexplained, actual = f1$gain, deviation = abs(f1$unexplained))
f_stat = data.frame(AverageExplainedReturn = mean(f1$explained),
                    AverageActualReturn = mean(f1$actual),
                    AverageDeviation = mean(f1$deviation))

ggplot(betas, aes(x = date, y = beta)) +
  geom_line() +
  labs(title="Market Betas")

######################### FF-3-Factor Model ###############################################################
#### download the factors from KF website
FF3URL <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip"
temp1 <- tempfile()
download.file(FF3URL,temp1)
FF3Model <- fread(unzip(temp1),skip=4)
rm(temp1)

#### data preparation
FF3Model[,1] = transform(FF3Model[,1], V1 = as.Date(as.character(V1), "%Y%m%d"))
colnames(FF3Model)[1] = "date"
FF3Model[,2:5] = FF3Model[,2:5]/100
ret_3f = merge(MMM,FF3Model,by = "date")

#### rolling window
win = 262
betas_3f = data.frame(date = NULL, intercept = NULL, mkt = NULL, smb = NULL, hml = NULL, unexplained = NULL)
for (i in win:nrow(ret_3f)) {
  # i = 262
  y = ret_3f$gain[(i-win+1):i] - ret_3f$RF[(i-win+1):i]
  x = ret_3f[(i-win+1):i,3:5]
  reg = lm(y~x$`Mkt-RF`+x$SMB+x$HML)
  tmp = data.frame(date = MMM$date[i], intercept = reg$coefficients[1], 
                   mkt = reg$coefficients[2], smb = reg$coefficients[3], 
                   hml = reg$coefficients[4], unexplained = reg$residuals[win])
  rownames(tmp) = NULL
  betas_3f = rbind.data.frame(betas_3f,tmp)
}


betas_3f$date = as.Date(betas_3f$date)
saveRDS(betas_3f,"betas_3f.RDS")
colMeans(betas_3f[,2:6])
apply(betas_3f[,2:6], 2, sd)

f3 = merge(betas_3f,MMM, by='date')
f3 = data.frame(explained = f3$gain-f3$unexplained, actual = f3$gain, deviation = abs(f3$unexplained))
f3_stat = data.frame(AverageExplainedReturn = mean(f3$explained),
                    AverageActualReturn = mean(f3$actual),
                    AverageDeviation = mean(f3$deviation))

r_melted = melt(r[,1:3],id = 'date')
betas_3f_melted = melt(betas_3f[,c(1,3,4,5)], id = 'date')

ggplot(r_melted,
       aes(x=date, y=value, colour=variable)) +
  geom_line(alpha=0.4)

ggplot(betas_3f_melted,aes(x = date, y = value, colour=variable)) +
  geom_line(alpha = 0.4) +
  ylab("beta") + 
  theme(legend.title=element_blank())



###### some research on periodicity and seasonality (Commented off, NOT A PART OF THE MODELS) #################

# periodogram(MMM_TS)

# ar = arima(MMM_TS,order = c(0,0,11))
# x = MMM$gain[1:3000]
# s = dolpc(x)
# s =abs(s)
# s[s<0.707*max(s)] = 0
# freq = which(s>0)
# period = unique(round(length(s)/freq))
# plot(freq)
# show(period)
# library(signal)
# N = 40
# dolpc = function(x, N = 60){
#   m = length(x)
#   d = c(x,rev(x)[-1])
#   r = Re(ifft(d))
#   r = r[1:m]
# }


# for(i in (start+1):nObs){
#   i = start
#   y = Ret[i,-1]
#   x = Ret[(i-win):(i-1),-1]
#   # x = as.matrix(Ret[i:(i-win+1),2:235])
#   x = x[,which(colSums(is.na(x)) == 0)]
#   y = as.numeric(y[,which(colSums(is.na(x)) == 0)])
#   x = colMeans(x)
# 
#   coefs <- coef(crossval, s = crossval$lambda.min)
#   coefFlag[i,which(names(coefFlag) %in% coefs@Dimnames[[1]][coefs@i[-1]])] = coefs@x[-1]
# }
