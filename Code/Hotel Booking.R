setwd("~/Desktop/BA Year 2 Sem 2/BC2407/Project Docs/Project Script/Datasets")
data1 <- read.csv('Expedia.csv')

# Data cleaning ----------------------------------------------------------------------------------------------------------------------
# Include specified columns from data frame
data1 <- subset(data1, select = c(srch_id, date_time,srch_booking_window))
data1 <- data1[, c("srch_id", "date_time", "srch_booking_window")]

library(dplyr)
data1 <- select(data1, srch_id, date_time, srch_booking_window)

#convert character to date_time 
data1$date_time <- as.POSIXct(data1$date_time)

#Date time + booking windows = actual check-in days 
library(lubridate)
data1$date_time <- data1$date_time + days(data1$srch_booking_window)

#Create a new dataset from the new date_time column
data2 <- data1 %>%
  mutate(check_in_date = as.Date(date_time)) %>%
  group_by(check_in_date) %>%
  summarize(num_check_ins = n())

#------------------------------------------------------------------------------------------------------------------------------------
#Forecasting 

library("TTR")
library("forecast")


data.ts <- ts(data2$num_check_ins, frequency = 12, start = c(2012,10), end = c(2014, 10))

data.ts

plot.ts(data.ts, ylab = "Check-in", xlab = "Day-Year",
        main = "Number of check-in")
## Non-constant fluctuations. Additive Time Series.


# Moving Average -------------------------------------------------------------
m.ma3 <- SMA(data.ts, n = 3)
plot(m.ma3, main = "Moving Avg Span 3", ylab = "MA3 Forecast")

m.ma7 <- SMA(data.ts, n = 7)
plot(m.ma7, main = "Moving Avg Span 7", ylab = "MA7 Forecast")

# Classical Seasonal Decomposition by Moving Averages
m.ma.mul <- decompose(data.ts, type = "additive")
plot(m.ma.mul)


# Simple Exponential Smoothing ---------------------------------------------
m.ses <- HoltWinters(data.ts, seasonal = "additive", beta=F, gamma=F)

m.ses
## Optimal value of alpha = 0.9999339

plot(m.ses, main = "Simple Exp Smoothing with optimized alpha = 0.9999339")

m.ses$fitted

m.ses$alpha*14781 + (1-m.ses$alpha)*21854.758
# 14781.47

RMSE.ses <- round(sqrt(m.ses$SSE/nrow(m.ses$fitted)))
## RMSE = 3777


# Holt's method  --------------------------------------------------------------
m.holt <- HoltWinters(data.ts, seasonal = "additive", gamma=F)

m.holt
## Optimal value of alpha = 0.2281362, beta = 0.2354069

plot(m.holt, main = "Holt Smoothing with optimized alpha = 0.2281362, beta = 0.2354069")

RMSE.holt <- round(sqrt(m.holt$SSE/nrow(m.holt$fitted)))
# RMSE = 4042



# Winter's method -------------------------------------------------------------
m.winters <- HoltWinters(data.ts, seasonal = "additive")

m.winters
## Optimal value of alpha = 0.131568, beta = 0.09932213, gamma = 0.2025372.

plot(m.winters, main = "Winters Smoothing with optimized alpha = 0.131568, beta = 0.09932213, gamma = 0.2025372.")

RMSE.winters <- round(sqrt(m.winters$SSE/nrow(m.winters$fitted)))
# RMSE = 3640


# Forecast 110 periods ahead using winters as it has lower RMSE -------------------------------------
m.winters.forecasts <- forecast(m.winters, h = 110)

m.winters.forecasts

plot(m.winters.forecasts, main = "110 Period Ahead Forecasts based on Winters Method")






