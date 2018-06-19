#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    index = match(input$ticker,names(test))
    lst = test[[index]]
    # draw the histogram with the specified number of bins
    ggplot(lst, aes(x = date, y = close)) +
      geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
      # labs(title = "AAPL Candlestick Chart", y = "Closing Price") +
      scale_x_date(breaks = scales::pretty_breaks(10))+
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90))
    
  })
  
})
