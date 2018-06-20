#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

source("functions.r")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  DataTable <- reactive({
    index = match(input$ticker,names(dat))
    
    t1 = as.Date(input$period[1])
    t2 = as.Date(input$period[2])
    if (t1>=Dates[1] && t2<=Dates[2]) {
      lst = dat[[index]]
      lst = lst[lst$date>=t1 & lst$date<=t2,]
    }else{
      dat = GetData(c(t1,t2))
      lst = dat[[index]]
      lst = lst[lst$date>=t1 & lst$date<=t2,]
    }
    
    Ret = dat[[1]][,c(1,8)]
    Stocks = names(dat)
    colnames(Ret)[2] = Stocks[1]
    L = length(SP500.list)
    # index = 141
    
    for (i in 2:L) {
      # i = 2
      r = dat[[i]][,c(1,8)]
      colnames(r)[2] = Stocks[i]
      Ret = merge(Ret, r, by = "date",all = TRUE)
    }
    Ret = Ret[-1,]
    Ret = Ret[Ret$date>=t1 & Ret$date<=t2,]
    return(list(lst,Ret))
  })
  t <- reactive({
    lst = DataTable()[[1]]
    L = length(dat)
    l = nrow(lst)
    corr = rep(NA,L)
    for (i in 1:L) {
      tmp = dat[[i]][,c(1,8)]
      r = lst[,c(1,8)]
      df = merge(x = r, y = tmp, by = "date")
      if (nrow(df)==l) {
        df = df[-1,]
        corr[i] = cor(df$gain.x,df$gain.y)
      }
    }
    Stocks = names(dat)
    Corr = data.frame(stock = Stocks, correlation = corr)
    t = Corr[order(Corr$correlation,decreasing = TRUE),]
    t =na.omit(t)
    return(t)
  })
  
  
  output$distPlot <- renderPlot({
    
    # index = match(input$ticker,names(dat))
    # 
    # t1 = as.Date(input$period[1])
    # t2 = as.Date(input$period[2])
    # if (t1>=Dates[1] && t2<=Dates[2]) {
    #   lst = dat[[index]]
    #   lst = lst[lst$date>=t1 & lst$date<=t2,]
    # }else{
    #   dat = GetData(c(t1,t2))
    #   lst = dat[[index]]
    #   lst = lst[lst$date>=t1 & lst$date<=t2,]
    # }
    lst = DataTable()[[1]]
    # draw the histogram with the specified number of bins
    ggplot(lst, aes(x = date, y = close)) +
      geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
      # labs(title = "AAPL Candlestick Chart", y = "Closing Price") +
      scale_x_date(breaks = scales::pretty_breaks(10))+
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90),axis.title.y = element_blank())
    
  })
  output$corr <- renderPlot({
    t = t()
    Ret = DataTable()[[2]]
    Corr_pairwise =cor(Ret[,as.numeric(row.names(t[1:6,]))+1])
    corr <- round(Corr_pairwise, 3)
    
    # Plot
    ggcorrplot(corr, hc.order = TRUE, 
               type = "lower", 
               lab = TRUE, 
               lab_size = 3,
               method="circle",
               colors = c("tomato2", "white", "springgreen3"),
               title="Correlation", 
               ggtheme=theme_bw)
    })
  
  output$table <- renderDataTable({
    t = t()
    t[,2] = round(t[,2],3)
    datatable(t, options = list(pageLength = 8),rownames = FALSE)
  })
  
})
