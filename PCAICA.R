index = 141
Ret = readRDS("Ret.RDS")

nObs = nrow(Ret)
# nObs = 2620
RetData = Ret[,-1]
nVar = ncol(RetData)-1
inSample = round(nObs/2)
outSample = nObs - inSample
# hc = hclust(dist(t(RetData)))
corr = rep(NA,ncol(RetData))
for (i in 1:ncol(RetData)) {
  df = merge(x = Ret[1:inSample,c(1,index+1)], y = Ret[1:inSample,c(1,i+1)], by = "date")
  df = df[complete.cases(df),]
  scaler = nrow(df)/inSample
  if (scaler>0) {
    corr[i] = cor(df[,2],df[,3])*scaler
  }
}
# picked = order(-corr)[2:11]
r = RetData[1:inSample,]
r = r[ , colSums(is.na(r)) == 0]
r = as.matrix(r)
m = colMeans(r)
r = sweep(r,2,m)
c = cov(r)
e = eigen(c)
# RetData[is.na(RetData)] = 0
# e0 = eigen(cov(RetData))
loadings = sweep(e$vectors,2,sqrt(e$values),"*")
PCs = r%*%loadings
w = solve(loadings)
# PCs = sweep(PCs,2,sqrt(e$values),"*")
# w = solve(sweep(e$vectors,2,sqrt(e$values),'*'))
PCs_tr = apply(PCs,2, function(x) cumprod(1+x))
PCs_tr = as.data.frame(PCs_tr)
PCs_tr = cbind.data.frame(Ret$date[1:inSample],PCs_tr)
colnames(PCs_tr)[1] = "date"



# pca = prcomp(r)
ExpPwr = e$values/sum(e$values)
cumExpPwr = cumsum(ExpPwr)
PCA_stat = data.frame(PCs = 1:length(e$values),
                      "Marginal Explaining Power" = ExpPwr, 
                      "Cumulative Explaining Power" = cumExpPwr)

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

library(fastICA)
ica = fastICA(r,371)
r_est_ica = ica$S[,1:5]%*%ica$A[1:5,]
# e_ica = abs(r - r_est_ica)
e_ica = abs(r[inSample,] - r_est_ica[inSample,])
ICA_pred = data.frame(stock = 1:ncol(r),
                      "return at t" = r[inSample,], 
                      "estimate at t" = r_est_ica[inSample,], 
                      "error at t" = e_ica)

ICA_pred_melted = melt(ICA_pred[,1:3],id = "stock")

ggplot(data = ICA_pred_melted,
       aes(x = stock, y = value, colour = variable)) +
  geom_line()

ggplot(data=ICA_pred_melted, aes(x=stock, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge()) + theme_bw()

ggplot(ICA_pred, aes(x=error.at.t)) + 
  geom_histogram(color="black", fill="white",bins=40) + theme_bw()+
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(error.at.t)), color="blue", linetype="dashed", size=1)

