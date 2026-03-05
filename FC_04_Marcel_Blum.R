# Title: Factor-Augmented VAR (FAVAR) Forecasting Model
# Author: Marcel Blum
# Description: Implements FAVAR forecasting using PCA for dimensionality reduction.
#              Used as the final forecasting stage (Forecast 4).

library(vars)
library(doParallel)
library(foreach)

#' Forecast Macroeconomic Variable using FAVAR
#' 
#' @param data A data frame containing 'Date', 'gdp', and other variables.
#' @return A numeric vector of 4-step-ahead forecasts.
#' @details Performs PCA-based dimensionality reduction, selects factors using 
#'          HQIC, and performs nested parallel cross-validation to tune 
#'          window size and lag length for the final VAR model.
forecaster <- function(data) {
  
  # define parameters
  forecast_horizon <- 4
  max_lag <- 10
  max_factors <- 20
  cv_sizes_window <- seq(0.5, 0.95, by = 0.05)
  
  # convert data
  data$Date <- as.Date(data$Date)
  y <- data$gdp
  X <- as.matrix(data[, !(names(data) %in% c("Date", "gdp"))])
  n <- nrow(X)
  
  # set up parallelization
  cores <- max(1, floor(detectCores() * 0.5))
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  on.exit({
    stopCluster(cl)
    registerDoSEQ()
  })
  
  # PCA with HQIC
  pca <- prcomp(X, center = TRUE, scale. = FALSE)
  T <- nrow(X)
  IC_vals <- sapply(1:max_factors, function(k) {
    F_hat <- pca$x[, 1:k]
    X_hat <- F_hat %*% t(pca$rotation[, 1:k])
    err <- X - X_hat
    sigma2 <- mean(err^2)
    hqic_penalty <- 2 * k * log(log(T)) / T
    log(sigma2) + hqic_penalty
  })
  best_k <- which.min(IC_vals)
  factors <- pca$x[, 1:best_k, drop = FALSE]
  colnames(factors) <- paste0("F", 1:best_k)
  
  # VAR input
  df_var <- data.frame(gdp = y, factors)
  
  # rolling cross-validation for lag and window size
  results_window <- foreach(size = cv_sizes_window, .combine = rbind, .packages = "vars") %:%
    foreach(p = 1:max_lag, .combine = rbind) %dopar% {
      train_size_cv <- floor(n * size)
      errors <- c()
      for (i in (train_size_cv + p):(n - forecast_horizon)) {
        start_idx <- i - train_size_cv - p + 1
        end_idx <- i
        if (start_idx <= 0 || end_idx > nrow(df_var)) next
        train_data <- df_var[start_idx:end_idx, ]
        
        model <- tryCatch(VAR(train_data, p = p, type = "const"), error = function(e) NULL)
        if (is.null(model)) next
        fcst <- tryCatch(predict(model, n.ahead = forecast_horizon), error = function(e) NULL)
        if (is.null(fcst)) next
        
        y_pred <- sapply(fcst$fcst$gdp[1:forecast_horizon], function(x) x[1])
        y_true <- df_var$gdp[(i + 1):(i + forecast_horizon)]
        if (length(y_pred) == forecast_horizon && length(y_true) == forecast_horizon) {
          errors <- c(errors, (y_true - y_pred)^2)
        }
      }
      data.frame(size = size, lag = p, msfe = mean(errors, na.rm = TRUE))
    }
  
  # fit final VAR model
  best_row <- results_window[which.min(results_window$msfe), ]
  best_size <- best_row$size
  best_lag <- best_row$lag
  final_train <- tail(df_var, floor(n * best_size) + best_lag)
  var_final <- VAR(final_train, p = best_lag, type = "const")
  fcst_list <- predict(var_final, n.ahead = forecast_horizon)$fcst$gdp
  fcst <- sapply(fcst_list[1:forecast_horizon], function(x) x[1])
  
  return(fcst)
}
