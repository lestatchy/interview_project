#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "ticker", 
                label = "Text input", 
                value = "MMM"),
      dateRangeInput(inputId = "period",
                     label = "Period",
                     start = '2009-01-06',
                     end = '2017-06-25',
                     min = '2009-01-06',
                     max = '2017-07-25')
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
