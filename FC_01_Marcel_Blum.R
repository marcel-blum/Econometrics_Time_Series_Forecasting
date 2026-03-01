# student name: Marcel Blum
# matriculation number: 1096233
# course: Time Series Econometrics
# study program: International Business and Economics (Master)

### Disclaimer: ###
# I have used ChatGPT to support the development of this R script, and have used it primarily for debugging purposes.
# All final decisions regarding model selection, implementation, and interpretation were made independently.

# +++ Code +++ #
rm(list = ls())

forecaster <- function(data) {
  
  # import necessary packages
  library(lubridate)
  library(forecast)
  
  # split dataset into training and test data
  N <- dim(data)[1] # get no. of observations (rows)
  T <- dim(data)[2] # get no. of variables (columns)
  x_is <- data[1:(N/2), ]  # in-sample data (first half)
  x_oos <- data[(N/2 + 1):N, ]  # out-of-sample data (second half)
  
  # winsorize in-sample data to account for outliers
  lower <- quantile(x_is$GDP.Change, 0.01)
  upper <- quantile(x_is$GDP.Change, 0.99)
  x_is$GDP.Change.Win <- pmin(pmax(x_is$GDP.Change, lower), upper)
  
  # create time series object from winsorized in-sample data
  ts_data_win <- ts(x_is$GDP.Change.Win, start = c(2012, 1), frequency = 12)
  
  # model tuning: AIC-based lag selection (on in-sample data)
  max_lag <- 10
  aic_values <- rep(Inf, max_lag) # create numeric vector as a placeholder for AIC values
  for (p in 1:max_lag) { # determine optimal lag order p
    fit <- tryCatch(Arima(ts_data_win, order = c(p, 0, 0)), error = function(e) NULL)
    if (!is.null(fit)) {
      aic_values[p] <- AIC(fit)
    }
  }
  optimal_p <- which.min(aic_values) # set optimal lag length p
  
  # fit model on the in-sample data using AR(optimal_p)
  fit_m1 <- arima(x_is$GDP.Change.Win, order = c(optimal_p, 0, 0))
  
  # forecast 4 steps ahead for out-of-sample data
  fcst <- predict(fit_m1, n.ahead = 4)  # Forecast for 4 steps ahead
  
  # generate time series for forecasted values
  start_date <- as.Date("2032-01-01")  # start date of the forecast period
  forecast_dates <- seq(start_date, by = "month", length.out = 4)
  
  # combine forecast values with dates and display
  forecast_ts <- ts(fcst$pred, start = c(2032, 1), frequency = 12)
  
  # return forecast results with proper formatting
  forecast_df <- data.frame(
    Date = forecast_dates,
    GDP.Change = forecast_ts
  )
  
  # print forecast
  print(forecast_df)
}

# sample data
fc_data <- read.csv('/Users/marcel/R projects/IBE_Master/Time Series Econometrics/Time_Series_Econometrics/Raw Data/FC_01_Data.txt')

# run forecaster function
result <- forecaster(fc_data)