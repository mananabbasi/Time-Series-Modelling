Overview
This project forecasts UK birth rates (1887–2022) using ARIMA and Exponential Smoothing models. The R script (Birth_time_series.R) ensures models meet key assumptions like stationarity.

Files
Script: Birth_time_series.R

Dataset: uk_births_data.csv
Setup & Run
Download Files: Save Birth_time_series.R and uk_births_data.csv in the same folder.

Open Script: Open Birth_time_series.R in RStudio.

Set Working Directory:
setwd("path/to/your/directory")
Install Packages:
install.packages(c("forecast", "tseries", "ggplot2", "readr"))

Features
EDA: Visualizes UK birth data trends.

Model Testing: Compares ARIMA and Exponential Smoothing.

Forecasting: Predicts future birth rates.

Validation: Ensures stationarity and model accuracy.

