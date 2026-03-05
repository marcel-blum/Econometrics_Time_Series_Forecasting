# Title: Multivariate VAR Forecasting Model
# Author: Marcel Blum
# Description: Implements Multivariate VAR with Granger-causality filtering and VIF-based multicollinearity checks.
#              Used as the second forecasting stage (Forecast 2).

library(vars)
library(car)
library(lmtest)
library(ggplot2)
library(dplyr)

#' Forecast Macroeconomic Variable using Multivariate VAR
#' 
#' @param data A data frame containing 'Date' and macroeconomic variables.
#' @return A numeric vector of 4-step-ahead forecasts.
#' @details Utilizes Granger-causality filtering, VIF for multicollinearity, 
#'          and expanding window cross-validation for lag selection.
forecaster <- function(data) {
  
  # prepare time series data
  data$Date <- as.Date(data$Date) 
  ts_data <- ts(data[, -1], start = c(1989, 1), frequency = 12) 
  
  # determine optimal lag for Granger-causality test
  lag_selection <- VARselect(ts_data, lag.max = 15, type = "const") 
  optimal_lag <- lag_selection$selection["AIC(n)"] 
  
  # conduct Granger-causality tests
  other_vars <- setdiff(colnames(ts_data), "gdp") 
  granger_results <- lapply(other_vars, function(var) { 
    test <- grangertest(ts_data[, "gdp"] ~ ts_data[, var], order = optimal_lag) 
    data.frame( 
      Variable = var,
      F_statistic = test$F[2],
      p_value = test$`Pr(>F)`[2]
    )
  })
  granger_df <- do.call(rbind, granger_results) 
  granger_df <- granger_df[order(granger_df$p_value), ] 
  signif_vars <- granger_df$Variable[granger_df$p_value < 0.05] 
  
  # conduct Variance Inflation Factor (VIF) filtering (test for multicollinearity)
  if (length(signif_vars) > 1) { 
    lm_formula <- as.formula(paste("gdp ~", paste(signif_vars, collapse = " + "))) 
    lm_model <- lm(lm_formula, data = ts_data) 
    vif_vals <- vif(lm_model) 
    selected_vars <- names(vif_vals[vif_vals < 10]) 
  } else {
    selected_vars <- signif_vars
  }
  
  # weighting according to F-statistic
  filtered_granger_df <- granger_df[granger_df$Variable %in% selected_vars, ] 
  filtered_granger_df$weight <- filtered_granger_df$F_statistic / sum(filtered_granger_df$F_statistic) 
  
  # build weighted index
  log_data <- ts_data[, filtered_granger_df$Variable] 
  weighted_index <- log_data %*% filtered_granger_df$weight 
  ts_index <- ts(cbind(gdp = ts_data[, "gdp"], index = weighted_index), start = c(1989, 1), frequency = 12) 
  ts_selected <- ts_index 
  
  # fixed split ratio and min_train_fraction
  split_ratio <- 0.68 
  split_idx <- floor(split_ratio * nrow(ts_selected)) 
  train_ts <- ts_selected[1:split_idx, ] 
  test_ts <- ts_selected[(split_idx + 1):nrow(ts_selected), ] 
  min_train_fraction <- 0.35 
  min_train_size <- floor(min_train_fraction * nrow(train_ts)) 
  val_size <- nrow(train_ts) - min_train_size 
  
  compute_cv_rmse <- function(lag) { 
    errors <- c() 
    for (i in 1:(val_size - 4)) { 
      train_end <- min_train_size + i - 1 
      train_cv <- train_ts[1:train_end, ] 
      test_cv <- train_ts[(train_end + 1):(train_end + 4), "gdp"] 
      tryCatch({ 
        model <- VAR(train_cv, p = lag, type = "const") 
        pred <- predict(model, n.ahead = 4)$fcst$gdp[, "fcst"] 
        errors <- c(errors, (test_cv - pred)^2) 
      }, error = function(e) { 
        errors <- c(errors, rep(NA, 4)) 
      })
    }
    sqrt(mean(errors, na.rm = TRUE)) 
  }
  
  lag_rmses <- sapply(1:15, compute_cv_rmse) 
  best_lag <- which.min(lag_rmses) 
  
  # final model training and forecast
  final_model <- VAR(train_ts, p = best_lag, type = "const") 
  forecast <- predict(final_model, n.ahead = 4) 
  fcst <- as.numeric(forecast$fcst$gdp[, "fcst"]) 
  actual_gdp <- test_ts[1:4, "gdp"] 
  rmse <- sqrt(mean((actual_gdp - fcst)^2, na.rm = TRUE)) 
  
  return(fcst)
}
