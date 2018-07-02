index = 141
Ret = readRDS("Ret.RDS")

nObs = nrow(Ret)
nObs = 2^12
RetData = Ret[,-1]
nVar = ncol(RetData)-1
Z = unlist(RetData[index])[1:nObs]

library(wavelets)
library(ggplot2)
library(gridExtra)
library(reshape2)
days = Ret$date[1:nObs]
#### wavelet transform
wt =  dwt(Z, n.levels=12,boundary="reflection", fast = FALSE)
Z_rec = data.frame(date = days)

#### reconstruction
p <- list()
for (i in 1:12) {
  # i = 2
  
  tmp = idwt(wt)
  Z_rec = cbind.data.frame(Z_rec,tmp)
  colnames(Z_rec)[i+1] = paste("resolution at",i)
  tmp = Z_rec[,c(1,i+1)]
  colnames(tmp) = c("date","signal")
  p[[i]] = ggplot(data = tmp)+geom_line(aes(x = date, y = signal))+ylab(paste("scale at",i))
  wt@W[[i]][] = 0
}
grid.arrange(grobs = p)

#### this part is for visualize each component
wt =  dwt(Z, n.levels=12,boundary="reflection", fast = FALSE)
wt_origin = wt
wtcoef = list()
for (i in 1:12) {
  wtcoef[[i]] = wt@W[[i]]
  wt@W[[i]][] = 0
}


decomposed = data.frame(date = days)
p1 <- list()
for (i in 1:12) {
  # i = 1
  wtmp = wt
  # wtmp
  wtmp@W[[i]] = wtcoef[[i]]
  decomposed = cbind.data.frame(decomposed,idwt(wtmp))
  colnames(decomposed)[i+1] = paste("component at level",i)
  tmp = decomposed[,c(1,i+1)]
  colnames(tmp) = c("date","signal")
  p1[[i]] = ggplot(data = tmp) +
    geom_line(aes(x = date, y = signal)) +
    ylab(paste("scale at",i)) +
    theme(axis.title.x=element_blank())
}
grid.arrange(grobs = p1)
decomposed_melted = melt(decomposed,id = "date")
ggplot(data = decomposed_melted,aes(x = date,y=value,colour = variable)) +
  geom_line(alpha = 0.4) +
  theme_bw()
