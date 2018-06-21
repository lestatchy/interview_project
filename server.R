#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

#### libraries, data and function loading in functions.r
source("functions.r")

shinyServer(function(input, output) {
   
  #### update stock data with the given time period. New request will established if
  #### the given period exceeds the stored period. In that circumstance, it would take
  #### much longer time to download data from the server.
  DataTable <- reactive({
    
    index = match(input$ticker,names(dat))
    t1 = as.Date(input$period[1])
    t2 = as.Date(input$period[2])
    
    #### give the data of picked stock
    if (t1>=Dates[1] && t2<=Dates[2]) {
      lst = dat[[index]]
      lst = lst[lst$date>=t1 & lst$date<=t2,]
    }else{
      #### if period exceeds the preset ones
      dat = GetData(c(t1,t2))
      lst = dat[[index]]
      lst = lst[lst$date>=t1 & lst$date<=t2,]
    }
    
    #### build a dataframe for all return data 
    Ret = dat[[1]][,c(1,8)]
    Stocks = names(dat)
    colnames(Ret)[2] = Stocks[1]
    L = length(dat)
    for (i in 2:L) {
      r = dat[[i]][,c(1,8)]
      colnames(r)[2] = Stocks[i]
      Ret = merge(Ret, r, by = "date",all = TRUE)
    }
    Ret = Ret[-1,]
    Ret = Ret[Ret$date>=t1 & Ret$date<=t2,]
    
    #### return the price of picked stock and daily gains of all other stocks
    return(list(lst,Ret))
  })
  
  #### find all correlations with picked stock
  t <- reactive({
    
    lst = DataTable()[[1]]
    
    L = length(dat)
    l = nrow(lst)
    
    corr = rep(NA,L)
    for (i in 1:L) {
      tmp = dat[[i]][,c(1,8)]
      r = lst[,c(1,8)]
      df = merge(x = r, y = tmp, by = "date")
      #### some of stocks may not be public listed at that time, ignore them, corresponding 
      #### entries will be left as NA
      if (nrow(df)==l) {
        df = df[-1,]
        corr[i] = cor(df$gain.x,df$gain.y)
      }
    }
    Stocks = names(dat)
    Corr = data.frame(stock = Stocks, correlation = corr)
    #### sort the correlations from high to low
    t = Corr[order(Corr$correlation,decreasing = TRUE),]
    t =na.omit(t)
    return(t)
  })
  
  #### candlestick plot
  output$candlePlot <- renderPlot({
    
    lst = DataTable()[[1]]
    # draw candlestick chart for picked stock
    ggplot(lst, aes(x = date, y = close)) +
      geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
      scale_x_date(breaks = scales::pretty_breaks(10))+
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90),axis.title.y = element_blank())
  })
  
  #### correlation chart
  output$corr <- renderPlot({
    #### t() returns the sorted correlations, we just need the top 6.
    t = t()
    #### DataTable() returns the daily return data, we need it to calculate correlations.
    Ret = DataTable()[[2]]
    
    #### calculate the correlations among top 6
    Corr_pairwise =cor(Ret[,as.numeric(row.names(t[1:6,]))+1])
    
    #### round for display
    corr <- round(Corr_pairwise, 3)
    
    # correlation heatmap
    ggcorrplot(corr, hc.order = TRUE, 
               type = "lower", 
               lab = TRUE, 
               lab_size = 3,
               method="circle",
               colors = c("tomato2", "white", "springgreen3"),
               title="Correlation", 
               ggtheme=theme_bw)
    })
  
  #### render the correlation table
  output$table <- renderDataTable({
    t = t()
    
    #### adjust for display
    t[,2] = round(t[,2],3)
    datatable(t, options = list(pageLength = 8),rownames = FALSE)
  })
  
})
