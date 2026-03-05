# Title: Hybrid Elastic Net + VAR Forecasting Model
# Author: Marcel Blum
# Description: Implements a hybrid framework utilizing Elastic Net for variable 
#              selection and VAR for time series forecasting.
#              Used as the third forecasting stage (Forecast 3).

library(glmnet)
library(vars)
library(dplyr)
library(doParallel)
library(foreach)

#' Forecast Macroeconomic Variable using Hybrid Elastic Net + VAR
#' 
#' @param data A data frame containing 'Date', 'gdp', and other variables.
#' @return A numeric vector of 4-step-ahead forecasts.
#' @details Uses parallelized grid search to optimize window size, alpha, and lambda.
#'          Elastic Net filters predictors, which are then modeled via VAR.
forecaster <- function(data) {
  
  # format data
  data <- data[order(data$Date), ]
  data$Date <- as.Date(data$Date)
  start_year <- as.numeric(format(min(data$Date), "%Y"))
  start_month <- as.numeric(format(min(data$Date), "%m"))
  
  # hardcoded parameters for run-time optimization; optimal set of parameters was found using CV
  window_sizes <- 0.7
  alpha_values <- 0.1
  lambda_values <- 0.7587
  
  # define forecast horizon
  forecast_horizon <- 4
  
  # create feature matrix X and target vector y
  X_full <- as.matrix(data[, !(names(data) %in% c("Date", "gdp"))])
  y_full <- data$gdp
  
  # observation count and loop limits
  n <- nrow(data)
  max_start <- n - forecast_horizon
  
  # create parameter grid
  param_grid <- expand.grid(window = window_sizes, alpha = alpha_values, lambda = lambda_values)
  
  # set up parallel backend
  cores <- parallel::detectCores() - 1
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  # execute parallel processing
  results <- foreach(i = 1:nrow(param_grid), .combine = rbind,
                     .packages = c("glmnet", "vars", "dplyr")) %dopar% {
                       
                       window_start_frac <- param_grid$window[i]
                       alpha_val <- param_grid$alpha[i]
                       lambda_val <- param_grid$lambda[i]
                       
                       min_train_size <- floor(window_start_frac * n)
                       
                       all_forecasts <- numeric()
                       all_actuals <- numeric()
                       last_fcst <- NULL
                       selected_vars_accum <- list()
                       lag_orders <- numeric()
                       
                       for (train_end in min_train_size:max_start) {
                         
                         X_train <- X_full[1:train_end, , drop = FALSE]
                         y_train <- y_full[1:train_end]
                         y_test <- y_full[(train_end + 1):(train_end + forecast_horizon)]
                         
                         fit <- glmnet(X_train, y_train, alpha = alpha_val, lambda = lambda_val, standardize = FALSE)
                         
                         selected_coefs <- coef(fit)
                         selected_vars <- rownames(selected_coefs)[which(selected_coefs != 0)]
                         selected_vars <- setdiff(selected_vars, "(Intercept)")
                         
                         if (length(selected_vars) == 0) next
                         
                         selected_vars_accum[[length(selected_vars_accum) + 1]] <- selected_vars
                         
                         gdp_train <- y_full[1:train_end]
                         X_selected <- data[1:train_end, selected_vars, drop = FALSE]
                         ts_data <- ts(cbind(gdp = gdp_train, X_selected),
                                       start = c(start_year, start_month),
                                       frequency = 12)
                         
                         candidate_lags <- 1:6
                         best_msfe <- Inf
                         best_p <- NA
                         
                         for (p in candidate_lags) {
                           if ((train_end - p) < forecast_horizon) next
                           
                           tryCatch({
                             model <- VAR(ts_data, p = p, type = "const")
                             forecast_result <- predict(model, n.ahead = forecast_horizon)
                             fcst_vals <- as.numeric(forecast_result$fcst$gdp[, "fcst"])
                             msfe <- mean((fcst_vals - y_test)^2, na.rm = TRUE)
                             
                             if (msfe < best_msfe) {
                               best_msfe <- msfe
                               best_p <- p
                             }
                           }, error = function(e) {})
                         }
                         
                         if (!is.na(best_p)) {
                           lag_order <- best_p
                         }
                         lag_orders <- c(lag_orders, lag_order)
                         
                         if (!is.na(lag_order) && (train_end - lag_order) > forecast_horizon) {
                           tryCatch({
                             var_model <- VAR(ts_data, p = lag_order, type = "const")
                             forecast_result <- predict(var_model, n.ahead = forecast_horizon)
                             fcst <- as.numeric(forecast_result$fcst$gdp[, "fcst"])
                             
                             k <- min(length(fcst), length(y_test))
                             all_forecasts <- c(all_forecasts, fcst[1:k])
                             all_actuals <- c(all_actuals, y_test[1:k])
                             last_fcst <- fcst
                           }, error = function(e) {})
                         }
                       }
                       
                       rmse <- sqrt(mean((all_actuals - all_forecasts)^2, na.rm = TRUE))
                       
                       list_df <- data.frame(
                         window = window_start_frac,
                         alpha = alpha_val,
                         lambda = lambda_val,
                         rmse = rmse,
                         forecast = I(list(last_fcst)),
                         predictors = I(list(unlist(selected_vars_accum))),
                         lag_orders = I(list(lag_orders))
                       )
                       return(list_df)
                     }
  
  best_result <- results[which.min(results$rmse), ]
  
  if (exists("cl")) {
    suppressWarnings(stopCluster(cl))
    registerDoSEQ()
    rm(cl)
  }
  
  fcst <- unlist(best_result$forecast)
  return(fcst)
}
