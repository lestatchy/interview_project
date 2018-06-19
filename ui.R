#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


shinyUI(fluidPage(
  
  fluidPage(
    verticalLayout(
      wellPanel(
      textInput(inputId = "ticker", 
                label = "Ticker: ", 
                value = "MMM"),
      dateRangeInput(inputId = "period",
                     label = "Period: ",
                     start = '2017-01-06',
                     end = '2017-06-25',
                     min = '2009-01-06',
                     max = '2017-07-25'),

       plotOutput("distPlot")
      ),
       dataTableOutput("table")
    )
  )
))
