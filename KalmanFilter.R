
index = 141


nObs = nrow(Ret)
RetData = Ret[,-1]
nVar = ncol(RetData)-1
Z = unlist(RetData[index])
Z_prior = data.frame(date = Ret$date, r_prior = rep(0,nObs))
Z_post = data.frame(date = Ret$date, r_post = rep(0,nObs))
E_prior = data.frame(date = Ret$date, err_prior = rep(0,nObs))
E_post = data.frame(date = Ret$date, err_post = rep(0,nObs))
H = RetData[,-index]
loadings = Ret[,-index]
loadings[,-1][] = 0
#### Initial states
#### Initial state of x, y
x = rep(c(1/nVar,-0.1/nVar),nVar)
#### x_t = Ax_{t-1} + w_t
A = diag(nVar) %x% matrix(c(1,0,1,1),nrow = 2)
#### Initial state of the covariance matrix
P = diag(nVar*2)*0.0004
Q = 0
R = 0
F = diag(nVar*2)
# A = F
for (i in 1:49) {
  #### x_{k|k-1}
  x = A%*%x
  #### P_{k|k-1}
  P = A%*%P%*%t(A) + Q
  #### H_k
  t = unlist(RetData[i,-index])
  t[is.na(t)] = 0
  H = t(as.matrix(kronecker(t,c(1,0))))
  Z_prior[i,2] = H%*%x
  #### y_k = z_k - H_k * x_{k|k-1}
  y_tilda = Z[i] - H%*%x
  E_prior[i,2] = y_tilda
  #### S_k
  S = H%*%P%*%t(H)
  #### K_k: Kalman Gain
  K = P%*%t(H)%*%solve(S)
  #### x_{k|k}: estimated loadings
  x = x + K%*%y_tilda
  #### P_{k|k}
  P = (1-K%*%H)%*%P%*%t(1-K%*%H) + K%*%R%*%t(K)
  #### post fiting error
  E_post[i,2] = Z[i] - H%*%x
  loadings[i,-1] = x
  Z_post[i,2] = H%*%x
}                   
ss = merge(Z_post,Z_prior,by = "date")
ss = cbind.data.frame(ss,Z)
ss = ss[1:50,]

library(ggplot2)
library("reshape2")
test_data_long <- melt(ss, id="date")  # convert to long format

ggplot(data=test_data_long,
       aes(x=date, y=value, colour=variable)) +
  geom_line()
