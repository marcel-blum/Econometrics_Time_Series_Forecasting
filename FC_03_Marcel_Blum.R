# student name: Marcel Blum
# course: Time Series Econometrics
# study program: International Business and Economics (Master)
# University of Hohenheim

### Disclaimer: ###
# I have used ChatGPT to support the development of this R script, and have used it primarily for debugging purposes.
# All final decisions regarding model selection, implementation, and interpretation were made independently.

# Note: Please use read.csv and not read.table when reading the raw data file "Data_FC_02.txt".
# In case the code does not run properly, these are my forecast values:
# [1] 0.8797638 0.6197801 0.5576986 0.5403387

# +++ Code +++ #
rm(list=ls())

data <- read.csv("~/R projects/IBE_Master/Time Series Econometrics/Time_Series_Econometrics/Raw Data/Data_FC_03.txt", header = TRUE)

forecaster <- function(data) {
  # load necessary libraries
  library(glmnet) # for regression model
  library(vars) # for VAR
  library(dplyr) # for data manipulation
  library(doParallel) # for parallel processing 
  library(foreach) # for looping in parallel
  
  # format data
  data <- data[order(data$Date), ]
  data$Date <- as.Date(data$Date)
  start_year <- as.numeric(format(min(data$Date), "%Y")) # extract start year from date
  start_month <- as.numeric(format(min(data$Date), "%m")) # extract start month from date
  
  # hardcoded values to reduce runtime
  window_sizes <- 0.7      # optimal window size
  alpha_values <- 0.1      # optimal alpha
  lambda_values <- 0.7587  # optimal lambda
  
  # Uncomment below to retest combinations:
  #window_sizes <- seq(0.3, 0.7, by = 0.05)
  #alpha_values <- seq(0, 1, by = 0.1)
  #lambda_values <- seq(0.01, 1, by = 0.05)
  
  # define forecast horizon
  forecast_horizon <- 4
  
  # create feature matrix X and target vector y (exclude 'Date' and 'gdp' from X)
  X_full <- as.matrix(data[, !(names(data) %in% c("Date", "gdp"))])
  y_full <- data$gdp
  
  # total number of observations
  n <- nrow(data)
  
  # latest possible training endpoint
  max_start <- n - forecast_horizon
  
  # create grid of parameter combinations to try out
  param_grid <- expand.grid(window = window_sizes, alpha = alpha_values, lambda = lambda_values)
  
  # set up parallel backend
  cores <- parallel::detectCores() - 1
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  # store selected predictors across parameter sets
  selected_predictors_list <- list()
  
  # parallel processing for each parameter combination
  results <- foreach(i = 1:nrow(param_grid), .combine = rbind,
                     .packages = c("glmnet", "vars", "dplyr")) %dopar% {
    window_start_frac <- param_grid$window[i]
    alpha_val <- param_grid$alpha[i]
    lambda_val <- param_grid$lambda[i]
    
    # compute min train size
    min_train_size <- floor(window_start_frac * n)
    
    # initialize vectors for storing forecasts and actuals
    all_forecasts <- numeric()
    all_actuals <- numeric()
    last_fcst <- NULL
    selected_vars_accum <- list()
    lag_orders <- numeric()
    
    # iterate over different training endpoints
    for (train_end in min_train_size:max_start) {
      
      # extract training and test sets
      X_train <- X_full[1:train_end, , drop = FALSE]
      y_train <- y_full[1:train_end]
      y_test <- y_full[(train_end + 1):(train_end + forecast_horizon)]
      
      # fit Elastic Net model with specified alpha and lambda
      fit <- glmnet(X_train, y_train, alpha = alpha_val, lambda = lambda_val, standardize = FALSE)
      
      # extract non-zero coefficients (selected predictors)
      selected_coefs <- coef(fit)
      selected_vars <- rownames(selected_coefs)[which(selected_coefs != 0)]
      selected_vars <- setdiff(selected_vars, "(Intercept)")
      
      # skip iteration if no predictors were selected
      if (length(selected_vars) == 0) next
      
      # save selected predictors
      selected_vars_accum[[length(selected_vars_accum) + 1]] <- selected_vars
      
      # prepare VAR input data (gdp + selected predictors)
      gdp_train <- y_full[1:train_end]
      X_selected <- data[1:train_end, selected_vars, drop = FALSE]
      ts_data <- ts(cbind(gdp = gdp_train, X_selected),
                    start = c(start_year, start_month),
                    frequency = 12)
      
      # select lag length for VAR based on lowest MSFE
      candidate_lags <- 1:6
      best_msfe <- Inf
      best_p <- NA
      
      for (p in candidate_lags) {
        # Ensure enough observations
        if ((train_end - p) < forecast_horizon) next # skip if too few observations
        
        tryCatch({
          model <- VAR(ts_data, p = p, type = "const") # fit VAR
          forecast_result <- predict(model, n.ahead = forecast_horizon) # do forecast 
          fcst_vals <- as.numeric(forecast_result$fcst$gdp[, "fcst"]) # extract GDP forecast
          msfe <- mean((fcst_vals - y_test)^2, na.rm = TRUE) # compute MSFE
          
          # update best lag if MSFE is lower
          if (msfe < best_msfe) {
            best_msfe <- msfe
            best_p <- p
          }
        }, error = function(e) {}) # handle errors silently
      }
      
      if (!is.na(best_p)) {
        lag_order <- best_p
        # Then continue to use it for final forecast
      }
      lag_orders <- c(lag_orders, lag_order)
      
      # final VAR forecasting using the best lag order
      if (!is.na(lag_order) && (train_end - lag_order) > forecast_horizon) {
        tryCatch({
          var_model <- VAR(ts_data, p = lag_order, type = "const")
          forecast_result <- predict(var_model, n.ahead = forecast_horizon)
          fcst <- as.numeric(forecast_result$fcst$gdp[, "fcst"])
          
          k <- min(length(fcst), length(y_test)) # match forecast to test size
          all_forecasts <- c(all_forecasts, fcst[1:k])
          all_actuals <- c(all_actuals, y_test[1:k])
          last_fcst <- fcst
        }, error = function(e) {}) # handle errors silently
      }
    }
    
    # compute RMSE for this parameter combo
    rmse <- sqrt(mean((all_actuals - all_forecasts)^2, na.rm = TRUE))
    
    # save results in a data frame
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
  
  # select best parameter combination (lowest RMSE)
  best_result <- results[which.min(results$rmse), ]
  
  # output the best hyperparameters and RMSE
  #print(table(unlist(best_result$lag_orders)))
  #cat("Best parameters — Window:", best_result$window,
      #"| Alpha:", best_result$alpha,
      #"| Lambda:", signif(best_result$lambda, 4),
      #"| RMSE:", round(best_result$rmse, 4), "\n")
  
  # output the most commonly selected lag
  most_common_lag <- names(which.max(table(unlist(best_result$lag_orders))))
  #cat("Most frequently selected lag order:", most_common_lag, "\n")
  
  # analyze and rank selected predictors
  all_selected <- unlist(best_result$predictors)
  predictor_freq <- sort(table(all_selected), decreasing = TRUE)
  #cat("\nMost frequently selected predictors:\n")
  #print(head(predictor_freq, 20))
  #cat("\nAll selected predictors (sorted by frequency):\n")
  #print(predictor_freq)
  
  # clean up parallel backend
  if (exists("cl")) {
    suppressWarnings(stopCluster(cl))
    registerDoSEQ()
    rm(cl)
  }
  
  # return the best forecast
  fcst <- unlist(best_result$forecast)
  return(fcst)
}

forecaster(data)
