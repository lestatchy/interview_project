index = 141
Ret = readRDS("Ret.RDS")

nObs = nrow(Ret)
# nObs = 2620
RetData = Ret[,-1]
nVar = ncol(RetData)-1
inSample = round(nObs/2)
outSample = nObs - inSample

r = RetData[1:inSample,]
r = r[ , colSums(is.na(r)) == 0]

#### PCA
r = as.matrix(r)
m = colMeans(r)
r = sweep(r,2,m)
c = cov(r)
e = eigen(c)
#### loadings
loadings = sweep(e$vectors,2,sqrt(e$values),"*")
#### principle components
PCs = r%*%loadings
w = solve(loadings)
#### pcs' cumulative returns
PCs_tr = apply(PCs,2, function(x) cumprod(1+x))
PCs_tr = as.data.frame(PCs_tr)
PCs_tr = cbind.data.frame(Ret$date[1:inSample],PCs_tr)
colnames(PCs_tr)[1] = "date"

# pca = prcomp(r)
#### PCs' explanation power
ExpPwr = e$values/sum(e$values)
cumExpPwr = cumsum(ExpPwr)

PCA_stat = data.frame(PCs = 1:length(e$values),
                      "Marginal Explaining Power" = ExpPwr, 
                      "Cumulative Explaining Power" = cumExpPwr)

#### Visualization
library(reshape2)
PCA_stat_scaled = PCA_stat
PCA_stat_scaled$Marginal.Explaining.Power = PCA_stat_scaled$Marginal.Explaining.Power*4
PCA_stat_melted = melt(PCA_stat_scaled, id = "PCs")

ggplot(PCA_stat_scaled,aes(x = PCs)) +
  geom_bar(aes(y = Marginal.Explaining.Power, colour = "Marginal Explaining Power"),
           stat = "identity",alpha = 0.4) +
  geom_line(aes(y = Cumulative.Explaining.Power, colour = "Cumulative Explaining Power")) +
  scale_y_continuous("Cumulative", sec.axis = sec_axis(~./4, name = "Marginal"))

PC5 = PCs_tr[,1:6]
colnames(PC5)[2:6] = c("PC1","PC2","PC3","PC4","PC5")

PC5_melted = melt(PC5, id = "date")
ggplot(data = PC5_melted,
       aes(x = date, y = value, colour = variable)) +
  geom_line()

w5 = w[1:5,]
r_est = PCs[,1:5]%*%w5
e = abs(r[inSample,] - r_est[inSample,])
PCA_pred = data.frame(stock = 1:ncol(r),
                      "return at t" = r[inSample,], 
                      "estimate at t" = r_est[inSample,], 
                      "error at t" = e)
PCA_pred_melted = melt(PCA_pred[,1:3],id = "stock")

ggplot(data = PCA_pred_melted,
       aes(x = stock, y = value, colour = variable)) +
  geom_line()

ggplot(data=PCA_pred_melted, aes(x=stock, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge()) + theme_bw()

ggplot(PCA_pred, aes(x=error.at.t)) + 
  geom_histogram(color="black", fill="white",bins=40) + theme_bw()+
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(error.at.t)), color="blue", linetype="dashed", size=1)

####ICA

library(fastICA)

#### Calculate correlations
corr = rep(NA,ncol(RetData))

for (i in 1:ncol(RetData)) {
  df = merge(x = Ret[1:inSample,c(1,index+1)], y = Ret[1:inSample,c(1,i+1)], by = "date")
  df = df[complete.cases(df),]
  scaler = nrow(df)/inSample
  if (scaler>0) {
    corr[i] = cor(df[,2],df[,3])*scaler
  }
}

picked = order(corr)[2]
Sp500ret = cbind.data.frame(date = Ret$date, gain = rowMeans(Ret[,-1],na.rm = TRUE))

#### find market beta
reg_1 = lm(r[,index]~Sp500ret$gain[1:inSample])
reg_2 = lm(r[,picked]~Sp500ret$gain[1:inSample])
#### use residues for analysis
ica = fastICA(cbind(reg_1$residuals,reg_2$residuals),2)
#### scale manually
S = ica$S/30
A = ica$A*30
r_est_ica = S%*%A
STr = apply(S, 2, function(x) cumprod(1+x))
Signal = data.frame(date = Ret$date[1:inSample], Signal_1 = STr[,1], Signal_2 = STr[,2])
Signal_melted = melt(Signal, id = "date")
ggplot(data = Signal_melted,
       aes(x = date, y = value, colour = variable)) +
  geom_line()+theme_bw()


# e_ica = abs(r - r_est_ica)
# e_ica = abs(r[inSample,] - r_est_ica[inSample,])
# ICA_pred = data.frame(stock = 1:ncol(r),
#                       "return at t" = r[inSample,], 
#                       "estimate at t" = r_est_ica[inSample,], 
#                       "error at t" = e_ica)
# 
# ICA_pred_melted = melt(ICA_pred[,1:3],id = "stock")
# 
# ggplot(data = ICA_pred_melted,
#        aes(x = stock, y = value, colour = variable)) +
#   geom_line()
# 
# ggplot(data=ICA_pred_melted, aes(x=stock, y=value, fill=variable)) +
#   geom_bar(stat="identity", position=position_dodge()) + theme_bw()
# 
# ggplot(ICA_pred, aes(x=error.at.t)) + 
#   geom_histogram(color="black", fill="white",bins=40) + theme_bw()+
#   geom_density(alpha=.2, fill="#FF6666") +
#   geom_vline(aes(xintercept=mean(error.at.t)), color="blue", linetype="dashed", size=1)

