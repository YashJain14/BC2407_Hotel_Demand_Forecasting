setwd("~/Desktop/BA Year 2 Sem 2/BC2407/Project Docs")
bookings <- read.csv('Expedia.csv')

library(dplyr)
library(lubridate)

# Data cleaning ----------------------------------------------------------------------------------------------------------------------
# Include specified columns from data frame
bookings <- subset(bookings, select = c(srch_id, date_time))

#convert character to date_time 
bookings$date_time <- as.POSIXct(bookings$date_time)
bookings$date <- as.Date(bookings$date_time)

# Create a new dataset with counts of searches per day
search_counts <- bookings %>%
  group_by(date) %>%
  summarise(search_count = n())

#------------------------------------------------------------------------------------------------------------------------------------
#Forecasting 

library("TTR")
library("forecast")


search.ts <- ts(search_counts$search_count, frequency = 12, start = c(2012,10), end = c(2014, 10))

search.ts

plot.ts(data.ts, ylab = "search counts", xlab = "Months-Year",
        main = "Number of search counts")
## constant fluctuations. multiplicative Time Series.

# Moving Average -------------------------------------------------------------
m.ma3 <- SMA(search.ts, n = 3)
plot(m.ma3, main = "Moving Avg Span 3", ylab = "MA3 Forecast")

m.ma7 <- SMA(search.ts, n = 7)
plot(m.ma7, main = "Moving Avg Span 7", ylab = "MA7 Forecast")

# Classical Seasonal Decomposition by Moving Averages
m.ma.mul <- decompose(search.ts, type = "multiplicative")
plot(m.ma.mul)


# Simple Exponential Smoothing ---------------------------------------------
m.ses <- HoltWinters(search.ts, seasonal = "multiplicative", beta=F, gamma=F)

m.ses
## Optimal value of alpha = 0.7960363

plot(m.ses, main = "Simple Exp Smoothing with optimized alpha = 0.7960363")

m.ses$fitted

m.ses$alpha*14630 + (1-m.ses$alpha)*19626.88
# 15649.18

RMSE.ses <- round(sqrt(m.ses$SSE/nrow(m.ses$fitted)))
## RMSE = 5758


# Holt's method  --------------------------------------------------------------
m.holt <- HoltWinters(search.ts, seasonal = "multiplicative", gamma=F)

m.holt
## Optimal value of alpha = 1, beta = 0.6119526

plot(m.holt, main = "Holt Smoothing with optimized alpha = 1, beta = 0.6119526")

RMSE.holt <- round(sqrt(m.holt$SSE/nrow(m.holt$fitted)))
# RMSE = 7314


# Winter's method -------------------------------------------------------------
m.winters <- HoltWinters(search.ts, seasonal = "multiplicative")

m.winters
## Optimal value of alpha = 0.03173754, beta = 1, gamma = 1

plot(m.winters, main = "Winters Smoothing with optimized alpha = 0.03173754, beta = 1, gamma = 1.")

RMSE.winters <- round(sqrt(m.winters$SSE/nrow(m.winters$fitted)))
# RMSE = 3002


# Forecast 110 periods ahead using winters as it has lower RMSE -------------------------------------
m.winters.forecasts <- forecast(m.winters, h = 4)

m.winters.forecasts

plot(m.winters.forecasts, main = "4 Period Ahead Forecasts based on Winters Method")


# ARIMA -------------------------------------------------------------
arima_model <- auto.arima(data.ts)

# Make forecasts for future time periods
forecast_arima <- forecast(arima_model, h = 110)  # Forecasting for the next 12 months

# Plot the forecasts
plot(forecast_arima)




