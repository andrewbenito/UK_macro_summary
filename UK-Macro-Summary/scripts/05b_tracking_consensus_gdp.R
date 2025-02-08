# ==============================================================================
# Initializing environment and loading functions
# ==============================================================================

rm(list=ls())
# Load standard libraries
lapply(c('here','tidyverse','data.table', 'readxl', 'openxlsx', 'broom', 'janitor', 'stringi',
         'MacrobondAPI','Rblpapi','ggridges', 'ggsci', 'parallel','glue', 'stringr', 'paletteer',
         'tseries', 'seasonal', 'forecast', 'tsibble', 'xts', 'zoo', 'gtsummary', 'gtExtras',
         'latex2exp', 'stargazer', 'sysfonts', 'showtext', 'patchwork'), 
       require, character.only = TRUE)

here::here()
blpConnect()
source('functions/functionsConsensusGDP.R')

# ==============================================================================
# Parameter descriptions
# ==============================================================================

# Description:
# Function plots how economic forecast evolved over time, together with preliminary and final prints. 
# Function is can only be applied to metrics which have quarterly prints.
# This is because print and forecast frequency has to match. 
# For this reason it can be applied primarily to GDP data in countries where it comes quarterly.
# I.e., Europe, China, NZ, ...

# Function parameters:
# fcast_series - 1st part of the BBG ticker for economist forecasts (from BBG ECFC)
# outturn - BBG ticker for outturn (from BBG ECFC)
# start_year - prefered start year for analysis and plotting
# P (default FALSE) - does the metric have a preliminary print
# series_name - plot title
# file_name - how to name a png output file

# ==============================================================================
# Running analysis for different countries
# ==============================================================================

# Select parameters
years <- c("2022", "2023") 
y_label <- "%"

# Select parameters for different countries
plt <- list()

for (start_year in years) {
  print(paste("updating charts starting in", start_year))
  
  # UK real GDP qoq
  fcast_series <- c('ECGQGB')
  outturn <- c("UKGRABIQ Index")
  series_name <- "UK Real GDP Growth (%qoq)"
  file_name <- "uk"
  P=TRUE
  l <- forecast.prep(fcast_series, outturn, start_year, P)
  # Unpacked dataframes - can override forecasts
  fcast <- l[[1]]
  outturns_p <- l[[2]]
  outturns_f <- l[[3]]
  l <- list(fcast, outturns_p, outturns_f)
  plt <- forecast.plot(l, P, series_name, y_label, file_name)
}

print("done")
