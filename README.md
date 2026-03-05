# Forecast Functions API Reference

This directory contains the four production-ready forecasting scripts developed for the University of Hohenheim Forecasting Competition. These scripts are designed to be modular and can be integrated into larger econometric pipelines.


## Environment & Reproducibility
This project was developed and tested using **R version 4.4.1**.

* **Dependencies:**
  All scripts require a standard R environment with the following packages installed:

```R
install.packages(c("vars", "forecast", "glmnet", "doParallel", "foreach", "lubridate", "dplyr", "car", "lmtest"))
* **Full Environment Log:** To view a complete snapshot of the development environment (including loaded namespaces), refer to the `session_info.txt` file in the root directory.
