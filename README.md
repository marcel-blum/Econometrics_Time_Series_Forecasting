## Visualizations
This directory contains the standardized diagnostic and performance plots for the forecasting models. These visualizations are designed to validate the models' statistical robustness and forecasting performance.

## Interpretation Guide
* **Forecast Performance:** The red line represents the out-of-sample forecast. The black line represents the historical GDP data. A continuous, smooth transition indicates a stable model fit.
* **Residual Diagnostics:** The goal is to see the autocorrelation bars staying within the blue dashed confidence intervals (representing white noise). If bars consistently exceed these bounds, it suggests the model may require further adjustment.
* **Lag Selection:** Determination of `optimal_p`. Plots provide visual evidence that the lag selection was statistically optimized rather than arbitrary.
