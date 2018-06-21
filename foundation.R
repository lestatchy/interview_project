library(htmltab)
library(quantmod)
# library(tidyverse)
library(tidyquant)
library(ggplot2)
# match("MMM",names(test))

# In data preparation, I use Wikipedia as the data source for the list of S&P 500 components and Yahoo Finance
# as the data source for daily prices. The process in downloading the data can be very time-consuming because
# of the restrictions from the server side. So rather than request the data every time, I download the data with
# long enough horizon and store it locally. And new request will be performed only when extra data needed. Indeed,
# it would be much better if I can build a database locally.
######################## Download data #################################################################
url = 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
SP500 = htmltab(url, which = '//*[@id="mw-content-text"]/div/table[1]')
SP500.list = SP500$`Ticker symbol`
SP500.list = sapply(SP500.list, function(x) gsub("\\.", "-",x[1]))

#### Store the stock data in new environment DATA
DATA = new.env()
# ss = SP500.list[1:2]
L = length(SP500.list)
Dates = as.Date(c("1998-06-18","2018-06-18"))


for(i in 1:L){
  try(
    getSymbols(SP500.list[i],DATA,from = Dates[1],to = Dates[2], return.class="data.frame")
    )
}
######################## Process data ###################################################################
#### get the data from the environment
Data <- eapply(DATA, "[")
s = SP500.list[1]
test = Data
L = length(Data)
#### preprocess the data and calculate daily returns
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

######################## Save data #####################################################################
# 
# saveRDS(Dates,"dates.RDS")
# saveRDS(test,file = 'Data.rds')

