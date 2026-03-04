# Econometrics_Time_Series_Forecasting
This repository contains the **end-to-end forecasting** pipeline developed for a four-stage macroeconomic forecasting competition. The framework demonstrates the progression from foundational univariate econometrics to high-dimensional factor models, specifically optimized for **Mean Squared Forecast Error (MSFE)**.


# Technical Methodology
The project implements four distinct, optimized model architectures:

1. **Univariate AR(p):** Robust modeling featuring outlier-robust winsorization and AIC-based multicollinearity checks.
2. **Multivariate VAR:** Utilized Granger-causality filtering and VIF-based multicollinearity checks with expanding window CV for optimal lag selection.
3. **Hybrid Elastic Net + VAR:** Implemented a parallelized grid search for regularization and lag optimization, tuned via rolling window CV.
4. **Factor-Augmented VAR (FAVAR):** Leveraged Principal Component Analysis (PCA) for dimensionality reduction and HQIC-optimized factor selection.

# Repository Structure
- **`/forecast_functions`**: Contains the production-ready `forecaster` functions. Each script encapsulates the full logic required for the respective forecast stage, optimized for performance and modularity.
- **`/visualizations`**: Contains performance plots and MSFE benchmarks to demonstrate model efficacy.

# Data Availability
Due to data privacy and non-disclosure agreements regarding the competition datasets, raw data is not provided in this repository. The code is designed to be plug-and-play; if you possess an equivalent macroeconomic dataset with similar structure, the `forecaster` functions will execute as intended.
