# Title: Univariate AR(p) Forecasting Model
# Author: Marcel Blum
# Description: Implements AR(p) forecasting for macroeconomic variables.
#              Used as the first forecasting stage (Forecast 1).

library(lubridate)
library(forecast)

#' Forecast Macroeconomic Variable using AR(p)
#' 
#' @param data A data frame containing 'GDP.Change'.
#' @return A data frame with forecast dates and predicted GDP values.
#' @details Implements outlier-robust winsorization followed by AIC-based lag selection.

# function
forecaster <- function(data) {
  
  # get dataset dimensions
  N <- dim(data)[1]
  T <- dim(data)[2]
  
  # split dataset into training (in-sample) and test (out-of-sample)
  x_is <- data[1:(N/2), ]
  x_oos <- data[(N/2 + 1):N, ]
  
  # winsorize in-sample data to mitigate outlier impact
  lower <- quantile(x_is$GDP.Change, 0.01)
  upper <- quantile(x_is$GDP.Change, 0.99)
  x_is$GDP.Change.Win <- pmin(pmax(x_is$GDP.Change, lower), upper)
  
  # create time series object from winsorized in-sample data
  ts_data_win <- ts(x_is$GDP.Change.Win, start = c(2012, 1), frequency = 12)
  
  # AIC-based lag selection on in-sample data
  max_lag <- 10
  aic_values <- rep(Inf, max_lag)
  for (p in 1:max_lag) {
    fit <- tryCatch(Arima(ts_data_win, order = c(p, 0, 0)), error = function(e) NULL)
    if (!is.null(fit)) {
      aic_values[p] <- AIC(fit)
    }
  }
  optimal_p <- which.min(aic_values)
  
  # model training using AR(optimal_p)
  fit_m1 <- arima(x_is$GDP.Change.Win, order = c(optimal_p, 0, 0))
  
  # generate forecast
  fcst <- predict(fit_m1, n.ahead = 4)
  
  # date generation for forecast output
  start_date <- as.Date("2032-01-01")
  forecast_dates <- seq(start_date, by = "month", length.out = 4)
  
  # create time series and data frame for output
  forecast_ts <- ts(fcst$pred, start = c(2032, 1), frequency = 12)
  forecast_df <- data.frame(
    Date = forecast_dates,
    GDP.Change = as.numeric(forecast_ts)
  )
  
  return(forecast_df)
}

# execution
# fc_data <- read.csv('path/to/your/data.txt')
# result <- forecaster(fc_data)
