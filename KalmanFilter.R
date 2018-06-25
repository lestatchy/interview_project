
index = 141
Ret = readRDS("Ret.RDS")

nObs = nrow(Ret)
nObs = 2000
RetData = Ret[,-1]
nVar = ncol(RetData)-1
Z = unlist(RetData[index])[1:nObs]
Z_prior = data.frame(date = Ret$date[1:nObs], r_prior = rep(0,nObs))
Z_post = data.frame(date = Ret$date[1:nObs], r_post = rep(0,nObs))
E_prior = data.frame(date = Ret$date[1:nObs], err_prior = rep(0,nObs))
E_post = data.frame(date = Ret$date[1:nObs], err_post = rep(0,nObs))
H = RetData[,-index]
loadings = Ret[,-index]
loadings[,-1][] = 0
#### Initial states
#### Initial state of x, y
x = rep(c(1/nVar,0.1/nVar),nVar)
#### x_t = Ax_{t-1} + w_t
A = diag(nVar) %x% matrix(c(1,0,1,1),nrow = 2)
#### Initial state of the covariance matrix
# P = diag(nVar*2)*cov(x)
P = x%*%t(x)
Q = 0
R = Z[1]^2
I = diag(nVar*2)
F = diag(nVar*2)
# A = F
for (i in 1:nObs) {
  #### x_{k|k-1}
  x_prior = A%*%x
  #### P_{k|k-1}
  P_prior = A%*%P%*%t(A) + Q
  #### H_k
  t = unlist(RetData[i,-index])
  t[is.na(t)] = 0
  H = t(as.matrix(kronecker(t,c(1,0))))
  Z_prior[i,2] = H%*%x_prior
  #### y_k = z_k - H_k * x_{k|k-1}
  y_tilda = Z[i] - H%*%x_prior
  E_prior[i,2] = y_tilda
  #### S_k
  S = H%*%P_prior%*%t(H) + R
  #### K_k: Kalman Gain
  K = P_prior%*%t(H)%*%solve(S)
  #### x_{k|k}: estimated loadings
  x = x_prior + K%*%y_tilda
  #### P_{k|k}
  P = (I-K%*%H)%*%P%*%t(I-K%*%H) + K%*%R%*%t(K)
  #### post fiting error
  E_post[i,2] = Z[i] - H%*%x
  loadings[i,-1] = x
  Z_post[i,2] = H%*%x
  # R = y_tilda^2
}                   
ss = merge(Z_post,Z_prior,by = "date")
ss = cbind.data.frame(ss,Z)
ss = ss[1:50,]

library(ggplot2)
library("reshape2")
test_data_long <- melt(ss[,c(1,2,4)], id="date")  # convert to long format

ggplot(data=test_data_long,
       aes(x=date, y=value, colour=variable)) +
  geom_line()
