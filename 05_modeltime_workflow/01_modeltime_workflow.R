# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: MODELTIME WORKFLOW FOR FORECASTING

# GOAL ----
# Deep-dive into modeltime

# OBJECTIVES ----
# - Understand the Modeltime Workflow
# - Understand Accuracy Measurements
# - Understand the Forecast Horizon & Confidence Intervals
# - Why refit?

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

recipe_spec_2_lag    <- feature_engineering_artifacts_list$recipes$recipe_spec_2

# TRAIN / TEST ----

splits <- data_prepared_tbl %>%
    time_series_split(assess = "8 weeks",
                      # initial = "1 year 1 months"
                      cumulative = TRUE)
splits

splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(optin_time, optins_trans)

data_prepared_tbl %>%
  time_series_cv(assess = "20 weeks",skip = "1 month",
                 initial = "1 years",
                 cumulative = F) -> splits_cv
tk_time_series_cv_plan(splits_cv) %>%
  plot_time_series_cv_plan(optin_time,optins_trans)

# Modeltime only works with objects already fitted with parsnip/workflows

# 1.0 MAKING MODELS ----
# - Works with Parnsip & Workflows
# - Models must be fit (trained)

# * Parsnip Model (ARIMA)  ----
model_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(optins_trans~optin_time,data=training(splits))
model_fit_arima
class(model_fit_arima)

?set_engine

# * Workflow (ARIMA + Date Features) ----
# separate the model from the preprocessing
model_spec_arima <- arima_reg() %>%
  set_engine("auto_arima")

recipe(optins_trans~optin_time, data = training(splits)) %>%
  prep() %>%
  juice() %>%
  glimpse(80)

recipe_spec_fourier <- recipe(optins_trans~optin_time, data = training(splits)) %>%
  step_fourier(optin_time, period = c(7,14,30,90), K = 1)

workflow_fit_arima <- workflow() %>%
  add_recipe(recipe_spec_fourier) %>%
  add_model(model_spec_arima) %>%
  fit(data = training(splits)) #here formula not needed, since it's in the recipe
workflow_fit_arima

# * Workflow (GLMNET + XREGS) ----

recipe_spec_2_lag
recipe_spec_2_lag %>% prep()
recipe_spec_2_lag %>% prep() %>% juice() %>% glimpse(80)

model_spec_glmnet <- linear_reg(penalty = 0.1, mixture = 0.5) %>%
  set_engine("glmnet")

workflow_fit_glmnet <- workflow() %>%
  add_recipe(recipe_spec_2_lag) %>%
  add_model(model_spec_glmnet) %>%
  fit(training(splits))
workflow_fit_glmnet

# 2.0 MODELTIME TABLE ----
# - Organize
model_tbl <- modeltime_table(
  model_fit_arima,
  workflow_fit_arima,
  workflow_fit_glmnet
) %>%
  update_model_description(3, "GLMNET - Lag Recipe")
model_tbl

# 3.0 CALIBRATION ----
# - Calculates residual model errors on test set
# - Gives us a true prediction error estimate when we model with confidence intervals
model_calib <- model_tbl %>%
  modeltime_calibrate(new_data = testing(splits))
model_calib

model_calib %>%
  slice(1) %>%
  unnest(.calibration_data)

# 4.0 TEST ACCURACY ----
# - Calculates common accuracy measures
# - MAE, MAPE, MASE, SMAPE, RMSE, R-SQUARED
model_calib %>%
  modeltime_accuracy()

default_forecast_accuracy_metric_set()

model_calib %>%
  modeltime_accuracy(metric_set = default_forecast_accuracy_metric_set())

model_calib %>%
  modeltime_accuracy(metric_set = default_forecast_accuracy_metric_set()) %>%
  table_modeltime_accuracy()

model_calib %>%
  modeltime_accuracy(metric_set = rmse)

metric_set(mae, rmse, iic)

model_calib %>%
  modeltime_accuracy(metric_set = metric_set(mae, rmse, iic))

# 5.0 TEST FORECAST ----
# - Visualize the out-of-sample forecast

model_calib %>%
  modeltime_forecast(actual_data = data_prepared_tbl,
                     conf_interval = 0.80)
# error because .calib-data is being used for the forecast
# which is missing the xregs for the glmnet model
# not a model for the reg_arima since the fouriers are based on optin-time
model_calib %>%
  modeltime_forecast(actual_data = data_prepared_tbl,
                     conf_interval = 0.80) %>%
  plot_modeltime_forecast()

# fix
model_calib %>%
  modeltime_forecast(actual_data = data_prepared_tbl,
                   new_data = testing(splits),
                   conf_interval = 0.80) %>%
  plot_modeltime_forecast()

model_calib %>%
  modeltime_forecast(actual_data = data_prepared_tbl,
                     new_data = testing(splits),
                     conf_interval = 0.80) %>%
  plot_modeltime_forecast(
    .legend_show = F,
    .conf_interval_show = T,
    .conf_interval_alpha = 0.05
  )

# 6.0 REFITTING ----

# * Refit ----

refit_tbl <- model_calib %>%
  modeltime_refit(data = data_prepared_tbl)
refit_tbl

# * Final Forecast ----
# - 'new_data' vs 'h'
# - 'actual_data'
# - Preprocessing

# fails since xregs are missing
refit_tbl %>%
  modeltime_forecast(
    h = 10
  )
refit_tbl %>%
  modeltime_forecast(
    h = 10
  ) %>%
  plot_modeltime_forecast(.legend_show = F)
# fix = provide forecast tbl
refit_tbl %>%
  modeltime_forecast(
    new_data = forecast_tbl
  )
refit_tbl %>%
  modeltime_forecast(
    new_data = forecast_tbl,
    actual_data = data_prepared_tbl %>% tail(100)
  )  %>%
  plot_modeltime_forecast(.legend_max_width = 25,
                          .conf_interval_fill = "lightblue")
