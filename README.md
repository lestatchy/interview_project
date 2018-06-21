# Invesco QRTradingTest

### Data preparation: 
##### foundation.R

In data preparation, I use Wikipedia as the data source for the list of S&P 500 components and Yahoo Finance as the data source for daily prices. The process in downloading the data can be very time-consuming because of the restrictions from the server side. So rather than request the data every time, I download the data with long enough horizon and store it locally. And new request will be performed only when extra data needed. Indeed, it would be much better if I can build a database locally.

### ShinyApp: 
##### ui.R, server.R, function.r

### Research Project: 
##### ResearchProj.R, Model2.R, ResearchProject.Rmd, ResearchProject.pdf

A individual return can be attributed to firm-specific characteristics, like financial status, management team capability, etc., and market factors such as market betas, value, growth, etc.. 

Regardless of market factors, some firm specific characteristics can be shared by similar firms. So maybe we can explain one firm’s return by its peers. I think two method can be used based on my assumption. One is that we first calculate the historical correlations and picked stocks with top ranks according some threshold or limited number to pick and regress on contemporaneous returns of these stocks. The other method is that we apply LASSO on contemporaneous returns of all other stocks and use cross validation to find the best number to pick. I implemented the LASSO method because it is more data-driven.

The second model is quite simple. First I tried to use S&P 500 equal weight return as the factor to explain the firm return. Secondly, I used a Fama-French 3 Factor model to decompose the firm return. Both methods are done by time series regressions.

