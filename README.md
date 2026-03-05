# Forecast Functions API Reference

This directory contains the four production-ready forecasting scripts developed for the University of Hohenheim Forecasting Competition. These scripts are designed to be modular and can be integrated into larger econometric pipelines.


## Dependencies
All scripts require a standard R environment with the following packages installed:

```R
install.packages(c("vars", "forecast", "glmnet", "doParallel", "foreach", "lubridate", "dplyr", "car", "lmtest"))
