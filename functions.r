library(htmltab)
library(quantmod)

dat = readRDS('Data.rds')
Dates = readRDS('dates.RDS')


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



