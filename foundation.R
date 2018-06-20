library(htmltab)
library(quantmod)
# library(tidyverse)
library(tidyquant)
library(ggplot2)
# match("MMM",names(test))
index = 141
url = 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
SP500 = htmltab(url, which = '//*[@id="mw-content-text"]/div/table[1]')
SP500.list = SP500$`Ticker symbol`
SP500.list = sapply(SP500.list, function(x) gsub("\\.", "-",x[1]))
DATA = new.env()
# ss = SP500.list[1:2]
L = length(SP500.list)
for(i in 1:L){
  try(
    getSymbols(SP500.list[i],DATA,from = "1998-06-18",to = "2018-06-18",return.class="data.frame")
    )
}
Dates = as.Date(c("1998-06-18","2018-06-18"))

GetData <- function(Dates){
  url = 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
  SP500 = htmltab(url, which = '//*[@id="mw-content-text"]/div/table[1]')
  SP500.list = SP500$`Ticker symbol`
  SP500.list = sapply(SP500.list, function(x) gsub("\\.", "-",x[1]))
  for(i in 1:L){
    new_DATA = new.env()
    try(
      getSymbols(SP500.list[i],new_DATA,from = Dates[1],to = Dates[2],return.class="data.frame")
    )
  }
  Data <- eapply(new_DATA, "[")
  L = length(Data)
  for (i in 1:L) {
    # i = 1
    lst = Data[[i]]
    colnames(lst) = c("open","high","low","close","volume","adjusted")
    p = lst$adjusted
    r = diff(p)/p[-length(p)]
    r = c(NA,r)
    lst = cbind.data.frame(lst,r)
    lst = cbind(as.Date(rownames(lst)),lst)
    colnames(lst)[1] = "date"
    colnames(lst)[8] = "gain"
    Data[[i]] = lst
  }
  saveRDS(Data,file = 'Data.rds')
  return(Data)
}



saveRDS(Dates,"dates.RDS")
Data <- eapply(DATA, "[")
s = SP500.list[1]

# 
# MMM <- getSymbols("MMM", from="2018-01-01", auto.assign=FALSE,return.class="data.frame")
# MMM = cbind(as.Date(rownames(MMM)),MMM)
# colnames(MMM) = c("date","open","high","low","close","volume","adjusted")
# 
# ggplot(MMM, aes(x = date, y = close)) +
#   geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
#   # labs(title = "AAPL Candlestick Chart", y = "Closing Price") +
#   scale_x_date(breaks = scales::pretty_breaks(10))+
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90))
test = Data
L = length(Data)
for (i in 1:L) {
  # i = 1
  lst = test[[i]]
  colnames(lst) = c("open","high","low","close","volume","adjusted")
  p = lst$adjusted
  r = diff(p)/p[-length(p)]
  r = c(NA,r)
  lst = cbind.data.frame(lst,r)
  lst = cbind(as.Date(rownames(lst)),lst)
  colnames(lst)[1] = "date"
  colnames(lst)[8] = "gain"
  test[[i]] = lst
}
saveRDS(test,file = 'Data.rds')
test = readRDS('Data.rds')
lst = test[[index]]
l = nrow(lst)
ggplot(lst, aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  # labs(title = "AAPL Candlestick Chart", y = "Closing Price") +
  scale_x_date(breaks = scales::pretty_breaks(10))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

corr = rep(NA,L)

for (i in 1:L) {
  # i = 1
  tmp = test[[i]][,c(1,8)]
  r = lst[,c(1,8)]
  df = merge(x = r, y = tmp, by = "date")
  if (nrow(df)==l) {
    df = df[-1,]
    corr[i] = cor(df$gain.x,df$gain.y)
  }
}
Stocks = names(test)
Corr = data.frame(stock = Stocks, correlation = corr)
t = Corr[order(Corr$correlation,decreasing = TRUE),]
H5 = t[2:6,]
ggplot(data=H5, aes(x=stock, y=correlation)) +
  geom_bar(stat="identity") +
  theme_bw()

Ret = test[[1]][,c(1,8)]
Stocks = names(test)
colnames(Ret)[2] = Stocks[1]
L = length(SP500.list)
# index = 141

for (i in 2:L) {
  # i = 2
  r = test[[i]][,c(1,8)]
  colnames(r)[2] = Stocks[i]
  Ret = merge(Ret, r, by = "date",all = TRUE)
}
Ret = Ret[-1,]


Corr_pairwise =cor(Ret[,as.numeric(row.names(t[1:6,]))+1])

library(ggcorrplot)

corr <- round(Corr_pairwise, 3)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3,
           method="circle",
           colors = c("tomato2", "white", "springgreen3"),
           title="Correlogram of the ", 
           ggtheme=theme_bw) +theme(plot.background = element_rect(fill = "darkblue"))
# 
# 
# names(MMM) <- gsub("^.+\\.","",names(MMM))  # remove "MMM." from column names
# 
# MMM <- data.frame(Date=as.POSIXct(index(MMM)), MMM[,1:4])
# 
# MMM$chg <- ifelse(Cl(MMM) > Op(MMM), "up", "dn")
# MMM$width <- as.numeric(periodicity(MMM)[1])
# MMM$flat_bar <- MMM[, "High"] == MMM[, "Low"]
# 
# 
# ggplot(MMM, aes(x=Date))+
#   geom_linerange(aes(ymin=Low, ymax=High)) +
#   theme_bw() +
#   labs(title="MMM") +
#   geom_rect(aes(xmin = Date - width/2 * 0.9, xmax = Date + width/2 * 0.9, ymin = pmin(Open, Close), ymax = pmax(Open, Close), fill = chg)) + guides(fill = FALSE, colour = FALSE) + scale_fill_manual(values = c("dn" = "darkred", "up" = "darkgreen"))
# # Handle special case of drawing a flat bar where OHLC = Open:
# if (any(MMM$flat_bar)) pl <- pl + geom_segment(data = MMM[MMM$flat_bar,], aes(x = Date - width / 2 * 0.9, y = Close, yend = Close, xend = Date + width / 2 * 0.9))
# 
# print(pl)
# 
# 
# 
# 
# 
# 
# 
# x = getSymbols(SP500.list[1:2], from = "2005-01-05",to = "2016-01-05",return.class="data.frame")
# # s = lapply(ss,function(x) try(getSymbols(x,from = "2005-01-05",to = "2016-01-05",return.class="data.frame")))
# test = tq_index("SP500") %>% tq_get(get = "stock.prices")
# s = SP500.list[1]
# # startMon = 12
# # startDay = 30
# # startYr = 2009
# # endMon = 12
# # endDay  = 20
# # endYr = 2010
# # freq = "d"
# # queryURL = c("https://ichart.yahoo.com/table.csv?s=",
# #              s,"&a=",startMon,"&b=",startDay,"&c=",
# #              startYr,"&d=",endMon,"&e=",endDay,"&f=",
# #              endYr,"&g=",freq,"&ignore=.csv")
