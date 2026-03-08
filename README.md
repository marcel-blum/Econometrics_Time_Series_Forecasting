## Visualizations
This directory contains the standardized diagnostic and performance plots for the forecasting models. These visualizations are designed to validate the models' statistical robustness and forecasting performance.

## Interpretation Guide
* **Forecast Performance:** The red line represents the out-of-sample forecast. The black line represents the historical GDP data. A continuous, smooth transition indicates a stable model fit.
* **Residual Diagnostics:** The goal is to see the autocorrelation bars staying within the blue dashed confidence intervals (representing white noise). If bars consistently exceed these bounds, it suggests the model may require further adjustment.
* **Model Tuning & Optimization:** Provides visual evidence that the model's core parameters were statistically optimized rather than arbitrarily chosen. Depending on the pipeline stage, this visualizes AIC/RMSE lag selection `optimal_p`, regularization penalty tuning `lambda`, or PCA factor selection `HQIC`.
