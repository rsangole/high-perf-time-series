# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: NEW FEATURES - MODELTIME 0.1.0

# GOAL ----
# Showcase Modeltime's Flexibility 

# OBJECTIVES ----
# - Expedited Forecasting - Skip Calibrating / Refitting
# - Working with In-Sample & Out-of-Sample Data
# - NEW Residual Diagnositics

# LIBRARIES ----

# Time Series ML
library(tidymodels)
library(modeltime)

# Core
library(tidyverse)
library(timetk)
library(lubridate)

# DATA & ARTIFACTS ----

feature_engineering_artifacts_list <- read_rds("00_models/feature_engineering_artifacts_list.rds")

data_prepared_tbl    <- feature_engineering_artifacts_list$data$data_prepared_tbl
forecast_tbl         <- feature_engineering_artifacts_list$data$forecast_tbl
recipe_spec_1_spline <- feature_engineering_artifacts_list$recipes$recipe_spec_1
recipe_spec_2_lag    <- feature_engineering_artifacts_list$recipes$recipe_spec_2

# TRAIN / TEST ----

splits <- data_prepared_tbl %>%
    time_series_split(assess = "8 weeks", cumulative = TRUE)


# 1.0 EXPEDITED FORECASTING ----

# * Model Time Table ----
#   - Fitted on Full Dataset (No Train/Test)



# * Make a Forecast ----
#   - No confidence intervals


# * Visualize a Fitted Model ----




# 2.0 CALIBRATION (In-Sample vs Out-of-Sample) ----

# * Refitting for Train/Test ----


# * Accuracy ----

# Out-of-Sample 


# In-Sample


# 3.0 RESIDUALS ----

# * Time Plot ----

# Out-of-Sample 


# In-Sample



# * ACF Plot ----

# Out-of-Sample 


# In-Sample



# * Seasonality ----

# Out-of-Sample 


# In-Sample



