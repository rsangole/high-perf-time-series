# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: ADVANCED TIME SERIES FORECASTING FOR BUSINESS
# MODULE: TIME SERIES JUMPSTART

# GOAL: Forecast Daily Email Users - Next 8-WEEKS

# OBJECTIVES ----
# - Dive into a time-series analysis project
# - Experience Frameworks: modeltime
# - Experience 2 Algorithms:
#   1. Prophet
#   2. LM w/ Engineered Features

# LIBRARIES ----
library(tidymodels)
library(modeltime)

library(DataExplorer)
library(tidyverse)
library(timetk)
library(lubridate)

# DATA -----
mailchimp_users_tbl <- read_rds("00_data/mailchimp_users.rds")
mailchimp_users_tbl

# 1.0 EDA & DATA PREP ----
# * DAILY SUBSCRIBERS INCREASES
mailchimp_users_tbl %>% glimpse()

# * COUNT OF OPTINS BY DAY ----
optins_day_tbl <- mailchimp_users_tbl %>%
  summarise_by_time(.date_var = optin_time,
                    .by = "day",
                    optins = n())

optins_day_tbl

# * Summary diagnostics ----
optins_day_tbl %>% tk_summary_diagnostics()

# * Pad Time series ----
optins_day_prepared_tbl <- optins_day_tbl %>% pad_by_time(.date_var = optin_time,
                               .pad_value  = 0)
optins_day_prepared_tbl

# * Visualize ----
optins_day_prepared_tbl %>%
  plot_time_series(
    .date_var = optin_time,
    optins)

# 2.0 EVALUATION PERIOD ----

# * Filtering ----
eval_tbl <- optins_day_prepared_tbl %>%
  filter_by_time(optin_time,
                 .start_date = "2018-11-20",
                 .end_date = "end")
eval_tbl %>% plot_time_series(optin_time, optins)

# * Train Test ----
splits <- eval_tbl %>%
  time_series_split(
    date_var = optin_time,
    assess = "8 week",
    cumulative = TRUE
  )
splits

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(
    .date_var = optin_time,
    optins
  )

# 3.0 PROPHET FORECASTING ----

# * Prophet Model using Modeltime/Parsnip ----
model_prophet_fit <- prophet_reg() %>%
  set_engine("prophet") %>%
  fit(optins ~ optin_time, data = training(splits))
model_prophet_fit

# * modeltime process
model_tbl <- modeltime_table(
  model_prophet_fit
)

# * Calibration ----
calib_tbl <- model_tbl %>%
  modeltime_calibrate(
    new_data = testing(splits)
  )

# * Visualize Forecast ----
calib_tbl %>%
  modeltime_forecast(
    actual_data = eval_tbl
  ) %>%
  plot_modeltime_forecast()

# * Visualize FC ----
calib_tbl %>%
  modeltime_accuracy()


# 4.0 FORECASTING WITH FEATURE ENGINEERING ----

# * Identify Features ----
eval_tbl %>%
  plot_seasonal_diagnostics(
    .date_var = optin_time,
    log(optins)
  )

# * REcipes Spec ----
rec_spec <- recipe(formula = optins ~ .,
       data = training(splits)) %>%
  # TS Signature
  step_timeseries_signature(optin_time) %>%
  step_rm(ends_with("iso"),
          ends_with("xts"),
          contains("hour"),
          contains("minute"),
          contains("second"),
          contains("am.pm")) %>%
  step_normalize(optin_time_index.num,
                 optin_time_year) %>%
  step_dummy(all_nominal())

rec_spec %>% prep() %>% juice() %>% glimpse()

# * ML Model - Specs ----

model_spec <- linear_reg() %>%
  set_engine("lm")

workflow_fit_lm <- workflow() %>%
  add_model(model_spec) %>%
  add_recipe(rec_spec) %>%
  fit(training(splits))


# * Modeltime Process ----

cal_tbl <- modeltime_table(
  model_prophet_fit,
  workflow_fit_lm
) %>%
  modeltime_calibrate(
    new_data = testing(splits)
  )
cal_tbl

# * Perf Metrics ----
cal_tbl %>%
  modeltime_accuracy()

cal_tbl %>%
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = eval_tbl
  ) %>%
  plot_modeltime_forecast()

# 5.0 SUMMARY & NEXT STEPS ----

# * What you've learned ----
# - You've been exposed to:
#   - Tidymodels / Modeltime Framework
# - You've seen 2 modeling approaches:
#   - Prophet - Univariate, Automatic
#   - Linear Regression Model - Many recipe steps
# - You've experienced Feature Engineering
#   - Visualizations: ACF, Seasonality
#   - Feature Engineering from Date Variables
#
# * Where you are going! ----
# - You still need to learn:
#   - New algorithms
#   - Machine Learning - How to tune parameters
#   - Feature Engineering Strategies
#   - Ensembling - Competition winning strategy
#   - and a lot more!

