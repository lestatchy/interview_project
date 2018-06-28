
index = 141
Ret = readRDS("Ret.RDS")

nObs = nrow(Ret)
# nObs = 2620
RetData = Ret[,-1]
nVar = ncol(RetData)-1
Z = unlist(RetData[index])[1:nObs]
Z_prior = data.frame(date = Ret$date[1:nObs], r_prior = rep(0,nObs))
Z_post = data.frame(date = Ret$date[1:nObs], r_post = rep(0,nObs))
E_prior = data.frame(date = Ret$date[1:nObs], err_prior = rep(0,nObs))
E_post = data.frame(date = Ret$date[1:nObs], err_post = rep(0,nObs))
Parameters = data.frame(date = Ret$date[1:nObs], Kalman_gain = rep(0,nObs), R = rep(0,nObs), Q = rep(0,nObs))
################################# Kalman Filter ##############################################################
#### Step 1
T = diag(nVar)
x0 = matrix(rep(1/nVar,nVar),ncol = 1)
P0 = diag(nVar)
R0 = 0.01
R = R0
Q = diag(nVar)*0.03^2
m = 90
V = matrix(rep(0,m),nrow = 1)
# H = RetData[,-index]
for (i in 1:nObs) {
  #### Step 2
  x1 = T%*%x0
  #### Step 3: Predict P
  P1 = T%*%P0%*%t(T) + Q
  #### Step 4: Innovation
  y = Z[i]
  H = matrix(unlist(RetData[i,-index]), nrow = 1)
  H[is.na(H)] = 0
  Hx1 = H%*%x1
  v = y - Hx1
  V = cbind(V,v)
  V = matrix(V[,-1],nrow = 1)
  #### Step 5: Innovation covariance
  HPH = H%*%P1%*%t(H)
  F = HPH + R
  #### Step 6: Update Kalman gain
  K = P1%*%t(H)/as.numeric(F)
  Parameters$Kalman_gain[i] = sum(K)
  #### Step 6: Update state estimate
  x0 = x1 + K*as.numeric(v)
  #### Step 7: Update state covariance
  P0 = P1 - P1%*%t(H)%*%H%*%P1/as.numeric(F)
  #### Step 8: Update Q
  VV = V%*%t(V)/m
  lambda = (VV-R)%*%solve(HPH)
  if(lambda<0) lambda = 1
  Q = Q*sqrt(lambda)
  Parameters$Q[i] = sum(diag(Q))
  #### Step 9: Update R
  R = VV - HPH
  if(det(R)<0) R = R0
  Parameters$R[i] = R
  #### Store data for current step
  Z_prior[i,2] = Hx1
  Z_post[i,2] = H%*%x0
  E_prior[i,2] = v
  E_post[i,2] = y-Z_post[i,2]
}     
results = merge(Z_prior,Z_post,by = "date")
results = cbind.data.frame(Z,results)
results = merge(results,E_prior,by = "date")
results = merge(results,E_post,by = "date")
results = merge(results,Parameters,by = "date")
colnames(results)[2] = "r_actual"
saveRDS(results,"KF.RDS")


################################# Result Presentation ########################################################
library(ggplot2)
library("reshape2")
r_rpost <- melt(results[,c(1,2,4)], id="date")  # convert to long format
r_rprior <- melt(results[,c(1,2,3)], id="date")  # convert to long format
e <- melt(results[,c(1,5,6)], id="date")  # convert to long format
ggplot(data=r_rpost,
       aes(x=date, y=value, colour=variable)) +
  geom_line(alpha = 0.5)

ggplot(data=r_rprior,
       aes(x=date, y=value, colour=variable)) +
  geom_line(alpha = 0.5)

ggplot(data=e,
       aes(x=date, y=value, colour=variable)) +
  geom_line(alpha = 0.5)

