library(dplyr)
library(tidyverse)
library(readr)
library(tsibble)
library(ggplot2)
library(fpp3)
library(lubridate)
library(fable)
library(feasts)
library(fabletools)
library(slider)
library(imputeTS)
library(shiny)
library(anytime)


ui <- fluidPage(
  titlePanel("Stock price Prediction"),
  sidebarPanel(
    selectInput("stock", label = "Select Stock",choices = c("MA","AAPL","AMZN","TSLA")),
    
    selectInput("forecast_input", label = "Select Number of Days for Forecast",choices = c("1 week","30 days","3 months","6 months", "1 year", "2 years")),
    
    dateRangeInput("dates", "Choose a date range:",
                   start = "2012-05-02",
                   end = "2022-04-29"
    )
    
    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Visualization", plotOutput("Daily_trend_chart")),
      tabPanel("Decomposition", plotOutput("daily_decomposition")),
      tabPanel("Moving Average", plotOutput("daily_moving_average")),
      tabPanel("Forecast", plotOutput("forecast"), tableOutput("accuracy")),
      tabPanel("Summary", tableOutput("summary"))
      #tabPanel("Articles")
    )
  )
)
