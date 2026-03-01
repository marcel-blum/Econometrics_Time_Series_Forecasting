# student name: Marcel Blum
# course: Time Series Econometrics
# study program: International Business and Economics (Master)
# University of Hohenheim

### Disclaimer: ###
# I have used ChatGPT to support the development of this R script, and have used it primarily for debugging purposes.
# All final decisions regarding model selection, implementation, and interpretation were made independently.

# Note: Please use read.csv and not read.table when reading the raw data file "Data_FC_02.txt".
# In case the code does not run properly, these are my forecast values:
#[1]  0.2175388 -0.4183024  0.2614759 -0.2993796

# +++ Code +++ #
forecaster <- function(data) {
  library(vars)
  library(car)
  library(lmtest)
  library(ggplot2)
  library(dplyr)
  
  # Prepare time series data
  data$Date <- as.Date(data$Date) # Convert dates into date format
  ts_data <- ts(data[, -1], start = c(1989, 1), frequency = 12) # Convert variables into ts_data and exclude date
  
  # Determine optimal lag for Granger-causality test
  lag_selection <- VARselect(ts_data, lag.max = 15, type = "const") # Determine optimal lag number needed for Granger-causality test
  optimal_lag <- lag_selection$selection["AIC(n)"] # Choose optimal lag based on AIC
  
  # Conduct Granger-causality tests
  other_vars <- setdiff(colnames(ts_data), "gdp") # Extract all variables except GDP and store them in other variables
  granger_results <- lapply(other_vars, function(var) { # Conduct Granger-causality test for the extracted variables
    test <- grangertest(ts_data[, "gdp"] ~ ts_data[, var], order = optimal_lag) # Test whether other variables Granger-cause GDP
    data.frame( # Exctract F-statistics and p-values from Granger-causality tests
      Variable = var,
      F_statistic = test$F[2],
      p_value = test$`Pr(>F)`[2]
    )
  })
  granger_df <- do.call(rbind, granger_results) # Combine all test results into a data frame
  granger_df <- granger_df[order(granger_df$p_value), ] # Sort results according to p-value
  signif_vars <- granger_df$Variable[granger_df$p_value < 0.05] # Select only the variables which are statistically significant
  
  # Conduct Variance Inflation Factor (VIF) filtering (test for multicollinearity)
  if (length(signif_vars) > 1) { # Check if more than one variable was found to be Granger-causal with GDP, if not skip
    lm_formula <- as.formula(paste("gdp ~", paste(signif_vars, collapse = " + "))) # Fit linear model of GDP against the significant variables
    lm_model <- lm(lm_formula, data = ts_data) # Fit linear regression model with GDP as the dependent variable and the significant variable as predictors
    vif_vals <- vif(lm_model) # Compute VIF for each predictor
    selected_vars <- names(vif_vals[vif_vals < 10]) # Keep only variables which are not strongly collinear
  } else {
    selected_vars <- signif_vars
  }
  
  # Weighting according to F-statistic to determine the level of influence of the other variables on GDP forecast
  filtered_granger_df <- granger_df[granger_df$Variable %in% selected_vars, ] # Filter original granger dataframe to keep only variables that demonstrate Granger-causality AND no multicollinearity
  filtered_granger_df$weight <- filtered_granger_df$F_statistic / sum(filtered_granger_df$F_statistic) # Compute weight for each predictor based on F-statistic in Granger-causality test
  
  # Build weighted index
  log_data <- ts_data[, filtered_granger_df$Variable] # Extract the ts data of the variables which are Granger-causal significant AND demonstrate no multicollinearity
  weighted_index <- log_data %*% filtered_granger_df$weight # Build weight index using selected variables
  ts_index <- ts(cbind(gdp = ts_data[, "gdp"], index = weighted_index), start = c(1989, 1), frequency = 12) # Combine GDP values with selected variables
  ts_selected <- ts_index # Assign ts index to new variable for testing purposes (which are not used anymore, but it is easier to keep it than replace the following ts_selected arguments with ts_index)
  
  # Fixed split ratio and min_train_fraction
  # Note: the optimal ratios are determined with a loop. This loop is commented out due to time-restraining reasons
  split_ratio <- 0.68 # Training-test-ratio
  split_idx <- floor(split_ratio * nrow(ts_selected)) # Calculate split index
  train_ts <- ts_selected[1:split_idx, ] # Build training set
  test_ts <- ts_selected[(split_idx + 1):nrow(ts_selected), ] # Build test set
  min_train_fraction <- 0.35 # Expanding window starting size
  min_train_size <- floor(min_train_fraction * nrow(train_ts)) # Determine minimum training size for expanding window
  val_size <- nrow(train_ts) - min_train_size # Determine how many validation steps can be performed by rolling the training window forward
  
  compute_cv_rmse <- function(lag) { # Function to compute CV RMSE for optimal lag, returns RMSE of 4-step-ahead forecasts using rolling CV
    errors <- c() # Store squared errors from all rolling validation windows, initialize it first
    for (i in 1:(val_size - 4)) { # Loop to run over rolling 4-step windows
      train_end <- min_train_size + i - 1 # Minimum training size grows by 1 in each iteration
      train_cv <- train_ts[1:train_end, ] # Training dataset for iterations
      test_cv <- train_ts[(train_end + 1):(train_end + 4), "gdp"] # The "actual" 4-steps ahead used for validation purposes
      tryCatch({ # R will not crash if model fitting/prediction fails
        model <- VAR(train_cv, p = lag, type = "const") # Fit VAR model with optimal lag
        pred <- predict(model, n.ahead = 4)$fcst$gdp[, "fcst"] # forecast 4-step ahead
        errors <- c(errors, (test_cv - pred)^2) # Store SE between forecast and actual GDP values
      }, error = function(e) { # Define what to do if error occurs
        errors <- c(errors, rep(NA, 4)) # Store NA to maintain sequence length if model fails
      })
    }
    sqrt(mean(errors, na.rm = TRUE)) # Compute RMSE over all rolling windows
  }
  
  lag_rmses <- sapply(1:15, compute_cv_rmse) # Calculate CV RMSE for lag 1 - 15
  best_lag <- which.min(lag_rmses) # Determine optimal lag with lowest RMSE
  
  final_model <- VAR(train_ts, p = best_lag, type = "const") # Train final model on training data
  forecast <- predict(final_model, n.ahead = 4) # Conduct 4-step ahead forecast
  fcst <- as.numeric(forecast$fcst$gdp[, "fcst"]) # Format forecast properly and extract first four steps from forecast output
  actual_gdp <- test_ts[1:4, "gdp"] # Get "actual" GDP values from test set for validation purposes
  rmse <- sqrt(mean((actual_gdp - fcst)^2, na.rm = TRUE)) # Compute RMSE
  
  return(fcst)
}