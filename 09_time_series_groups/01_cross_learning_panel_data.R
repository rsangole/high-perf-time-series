# BUSINESS SCIENCE UNIVERSITY ----
# DS4B 203-R: TIME SERIES FORECASTING  ----
# MODULE: SCALABLE TIME SERIES - CROSS-SECTIONAL LEARNING ----

# GOAL: Forecast Grouped Daily Google Analytics Page Views - Next 28-days

# OBJECTIVES ----
# - Cross-Sectional Learning - Forecast Grouped Data using Cross-Sections
# - Panel Data - Become comfortable with Overlapping Time Stamps
# - Time Series Resampling - Evaluating Model Stability Over Time
# - Ensembling - Multiple Cross-Sectional Models

# IMPORTANT ----
# - These techniques must only be used with non-sequential models (e.g. machine learning)

# LIBRARIES & SETUP ----

# Time Series ML
library(tidymodels)
library(modeltime)
library(modeltime.ensemble)

# Timing & Parallel Processing
library(tictoc)
library(future)
library(doFuture)

# Core
library(tidyquant)
library(tidyverse)
library(timetk)

# Plotting
library(plotly)

# * Parallel Processing ----

registerDoFuture()
n_cores <- parallel::detectCores()
plan(
    strategy = cluster,
    workers  = parallel::makeCluster(n_cores)
)

# plan(sequential)

# 1.0 DATA ----

# * GA Data ----
ga_page_raw_tbl <- read_rds("00_data/google_analytics_by_page_daily.rds")
ga_page_raw_tbl

ga_page_raw_tbl %>%
  group_by(pagePath) %>%
  plot_time_series(date,
                   pageViews,
                   .facet_ncol = 4,
                   .smooth = F,
                   .interactive = F)

# * Full Data ----

full_data_tbl <- ga_page_raw_tbl %>%

  # Fix data issues
  select(date, pagePath, pageViews) %>%
  group_by(pagePath) %>%
  pad_by_time(date,
              .by = 'day') %>%
  ungroup() %>%
  replace_na(list('pageViews' = 0)) %>%

  # Global Features / Xformations
  mutate(pageViews = log1p(pageViews)) %>%

  # Group-wise feature transformations
  group_by(pagePath) %>%
  future_frame(date,
               .length_out = 28,
               .bind_data = TRUE) %>%
  ungroup() %>%

  # Lags & Rolling Features, Fourier
  mutate(pagePath = as_factor(pagePath)) %>%
  group_split(pagePath) %>%
  map(.f = function(df){
    df %>%
      arrange(date) %>%
      tk_augment_fourier(date,
                         .periods = c(14, 28)) %>%
      tk_augment_lags(pageViews, .lags = 28) %>%
      tk_augment_slidify(pageViews_lag28,
                         .f       = ~ mean(.x, na.rm = TRUE),
                         .period  = c(7, 28, 28*2),
                         .partial = TRUE,
                         .align   = 'center')
  }) %>%
  bind_rows() %>%
  rowid_to_column(var = "rowid")

# * Data Prepared ----

data_prepared_tbl <- full_data_tbl %>%
  filter(!is.na(pageViews)) %>%
  drop_na()
data_prepared_tbl

# * Future Data ----

future_tbl <- full_data_tbl %>%
  filter(is.na(pageViews))

future_tbl %>% skimr::skim()

future_tbl <- future_tbl %>%
  mutate(across(.cols = contains('_lag'),
                .fns = ~ ifelse(is.nan(.x), NA, .x)),
         across(.cols = contains('_lag'),
                .fns = ~ replace_na(.x, 0))
  )

# 2.0 TIME SPLIT ----
splits <- data_prepared_tbl %>%
  time_series_split(date, assess = 28, cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, pageViews)

# 3.0 RECIPE ----

# * Clean Training Set ----
# - With Panel Data, need to do this outside of a recipe
# - Transformation happens by group

train_cleaned <- training(splits) %>%
  group_by(pagePath) %>%
  mutate(pageViews_clean = ts_clean_vec(pageViews, period = 7))

# training(splits) %>%
train_cleaned %>%
  group_by(pagePath) %>%
  plot_time_series(
    date, pageViews,
    .facet_ncol = 4, .interactive = F,  .smooth = FALSE
  )

ggplot() +
  geom_line(data = training(splits),
            aes(date, pageViews, color = "dirty")) +
  geom_line(data = train_cleaned,
            aes(date, pageViews_clean, color = "cleaned")) +
  facet_wrap(~pagePath, ncol = 4)

train_cleaned <- training(splits) %>%
  group_by(pagePath) %>%
  mutate(pageViews = ts_clean_vec(pageViews, period = 7))


#* Recipe Specification ----
train_cleaned

recipe_spec <- recipe(pageViews ~ .,
       data = train_cleaned) %>%
  update_role(rowid, new_role = "indicator") %>%
  step_timeseries_signature(date) %>%
  step_rm(contains("(iso)|(xts)|(hour)|(am.pm)|(minute)|(second)")) %>%
  step_normalize(date_index.num, date_year) %>%
  step_other(pagePath) %>% #TODO : understand this better
  step_dummy(all_nominal(), one_hot = TRUE)

recipe_spec %>%
  prep() %>%
  juice() %>%
  glimpse(80)


# 4.0 MODELS ----
# - !!! REMINDER: Cannot use sequential models !!!

# * PROPHET ----
workflow_fit_prophet <- workflow() %>%
  add_model(
    spec = prophet_reg() %>% set_engine("prophet")
  ) %>%
  add_recipe(recipe_spec) %>%
  fit(train_cleaned)


# * XGBOOST ----

workflow_fit_xgboost <- workflow() %>%
  add_model(
    spec = parsnip::boost_tree(mode = "regression") %>% set_engine("xgboost")
  ) %>%
  add_recipe(recipe_spec %>%
               update_role(date, new_role = "indicator")) %>%
  fit(train_cleaned)
workflow_fit_xgboost

# * PROPHET BOOST ----
workflow_fit_prophet_xgboost <- workflow() %>%
  add_model(
    spec = modeltime::prophet_boost(
      seasonality_daily = F,
      seasonality_weekly = F,
      seasonality_yearly = F
    ) %>%
      set_engine("prophet_xgboost")
  ) %>%
  add_recipe(recipe_spec) %>%
  fit(train_cleaned)


# * SVM ----
workflow_fit_svm <- workflow() %>%
  add_model(
    spec = svm_rbf(mode = "regression") %>% set_engine("kernlab")
  ) %>%
  add_recipe(
    recipe_spec %>% update_role(date, new_role = "indicator")
  ) %>%
  fit(train_cleaned)


# * RANDOM FOREST ----
workflow_fit_rf <- workflow() %>%
  add_model(
    spec = parsnip::rand_forest(mode = "regression") %>% set_engine("ranger")
  ) %>%
  add_recipe(
    recipe_spec %>% update_role(date, new_role = "indicator")
  ) %>%
  fit(train_cleaned)



# * NNET ----
workflow_fit_nnet <- workflow() %>%
  add_model(
    spec = parsnip::mlp(mode = "regression") %>% set_engine('nnet')
  ) %>%
  add_recipe(
    recipe_spec %>% update_role(date, new_role = "indicator")
  ) %>%
  fit(train_cleaned)

workflow_fit_nnetar <- workflow() %>%
  add_model(
    spec = modeltime::nnetar_reg(mode = "regression") %>% set_engine("nnetar")
  ) %>%
  add_recipe(
    recipe_spec
  ) %>%
  fit(train_cleaned)

# * MARS ----
workflow_fit_mars <- workflow() %>%
  add_model(
    spec = mars(mode = "regression") %>% set_engine("earth")
  ) %>%
  add_recipe(
    recipe_spec %>% update_role(date, new_role = "indicator")
  ) %>%
  fit(train_cleaned)


# * ACCURACY CHECK ----

submodels_1_tbl <- modeltime_table(
  workflow_fit_prophet,
  workflow_fit_prophet_xgboost,
  workflow_fit_xgboost,
  workflow_fit_rf,
  workflow_fit_svm,
  workflow_fit_nnet,
  workflow_fit_nnetar,
  workflow_fit_mars
)
submodels_1_tbl

submodels_1_tbl %>%
  modeltime_accuracy(testing(splits)) %>%
  arrange(rmse)

# 5.0 HYPER PARAMETER TUNING ----

# * RESAMPLES - K-FOLD -----
set.seed(42)
resamples_kfold <- train_cleaned %>% rsample::vfold_cv(v = 5)
resamples_kfold

resamples_kfold %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date,
                           pageViews,
                           .facet_ncol = 2)

# * XGBOOST TUNE ----

# ** Tunable Specification
model_spec_xgboost_tune <- boost_tree(
  mode = "regression",
  mtry = tune(),
  trees = tune(),
  tree_depth = tune(),
  min_n = tune(),
  learn_rate = tune(),
  loss_reduction = tune()
) %>%
  set_engine("xgboost")

workflow_spec_xgboost_tune <- workflow() %>%
  add_model(model_spec_xgboost_tune) %>%
  add_recipe(recipe_spec %>%
               update_role(date, new_role = "indicator"))
workflow_spec_xgboost_tune

# ** Tuning

parameters(workflow_spec_xgboost_tune)
tic()
set.seed(42)
tune_results_xgboost <- workflow_spec_xgboost_tune %>%
  tune_grid(
    resamples = resamples_kfold,
    param_info = parameters(workflow_spec_xgboost_tune) %>%
      update(learn_rate = learn_rate(range = c(0.001, 0.4), trans = NULL)),
    grid = 10,
    control = control_grid(verbose = TRUE, allow_par = TRUE)
  )
toc()
tune_results_xgboost

# ** Results
tune_results_xgboost %>% show_best("rmse", n = Inf)

# ** Finalize
workflow_fit_xgboost_tuned <- workflow_spec_xgboost_tune %>%
  finalize_workflow(select_best(tune_results_xgboost, "rmse")) %>%
  fit(train_cleaned)
workflow_fit_xgboost_tuned

# * RANGER TUNE ----

# ** Tunable Specification
model_spec_rf_tune <- rand_forest(
  mode = "regression",
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_engine("ranger")

workflow_spec_rf_tune <- workflow() %>%
  add_model(model_spec_rf_tune) %>%
  add_recipe(recipe_spec %>%
               update_role(date, new_role = "indicator"))
workflow_spec_rf_tune

# ** Tuning
parameters(workflow_spec_rf_tune)
tic()
set.seed(42)
tune_results_rf <- workflow_spec_rf_tune %>%
  tune_grid(
    resamples = resamples_kfold,
    grid = 10,
    control = control_grid(verbose = TRUE, allow_par = TRUE)
  )
toc()

# ** Results
tune_results_rf
tune_results_rf %>% show_best("rmse", n = Inf)

# ** Finalize
workflow_fit_rf_tuned <-workflow_fit_rf %>%
  finalize_workflow(select_best(tune_results_rf, "rmse")) %>%
  fit(train_cleaned)
workflow_fit_rf_tuned

# * EARTH TUNE ----

# ** Tunable Specification
model_spec_mars_tune <- parsnip::mars(
  mode = "regression",
  num_terms = tune(),
  prod_degree = tune()
) %>%
  set_engine("earth")

workflow_spec_mars_tune <- workflow() %>%
  add_model(model_spec_mars_tune) %>%
  add_recipe(recipe_spec %>% update_role(date, new_role = "indicator"))

# ** Tuning
tic()
set.seed(42)
tune_results_mars <- workflow_spec_mars_tune %>%
  tune_grid(
    resamples = resamples_kfold,
    grid = 10,
    control = control_grid(verbose = TRUE, allow_par = TRUE)
  )
toc()

# ** Results
tune_results_mars
tune_results_mars %>% show_best("rmse")

# ** Finalize
workflow_fit_mars_tuned <- workflow_spec_mars_tune %>%
  finalize_workflow(select_best(tune_results_mars, "rmse")) %>%
  fit(train_cleaned)
workflow_fit_mars_tuned

# 6.0 EVALUATE PANEL FORECEASTS  -----

# * Model Table ----
modeltime_table(workflow_fit_mars_tuned,
                workflow_fit_rf_tuned,
                workflow_fit_xgboost_tuned) %>%
  update_model_description(1, "EARTH - Tuned") %>%
  update_model_description(2, "RANGER - Tuned") %>%
  update_model_description(3, "XGBOOST - Tuned") %>%
  combine_modeltime_tables(submodels_1_tbl) -> submodels_2_tbl

submodels_2_tbl

# * Calibration ----
calib_tbl <- submodels_2_tbl %>%
  modeltime_calibrate(testing(splits))

# * Accuracy ----
calib_tbl
calib_tbl %>% modeltime_accuracy() %>% arrange(rmse)

# * Forecast Test ----
modeltime_forecast(calib_tbl,
                   new_data = testing(splits),
                   actual_data = data_prepared_tbl,
                   keep_data = TRUE) -> fcst_tbl

fcst_tbl %>%
  ggplot(aes(x = .index)) +
  geom_line(aes(y=.value,color=.key))+
  facet_wrap(~pagePath,ncol = 4)

fcst_tbl %>%
  filter(pagePath=="/learn.html") %>%
  ggplot(aes(x = .index)) +
  geom_line(aes(y=.value,color=.model_desc))+
  facet_wrap(~pagePath,ncol = 4) -> p
ggplotly(p)


# 5.0 RESAMPLING ----

# - Assess the stability of our models over time
# - Helps us strategize an ensemble approach

# * Time Series CV ----
train_cleaned %>%
  time_series_cv(
    assess = 28,
    skip = 28,
    cumulative = TRUE,
    slice_limit = 5
  ) -> resamples_tscv
resamples_tscv

resamples_tscv %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, pageViews)

# * Fitting Resamples ----
model_tbl_tuned_resamples <- submodels_2_tbl %>%
  modeltime_fit_resamples(
    resamples = resamples_tscv,
    control = control_resamples(
      verbose = TRUE,
      allow_par = TRUE
    )
  )

# * Resampling Accuracy Table ----
model_tbl_tuned_resamples

model_tbl_tuned_resamples %>%
  modeltime_resample_accuracy(metric_set = metric_set(rmse, rsq) ,
                              summary_fns = list(mean = mean, sd = sd)) %>%
  arrange(rmse_mean)

# * Resampling Accuracy Plot ----
model_tbl_tuned_resamples %>%
  plot_modeltime_resamples(
    .metric_set = metric_set(mae, rmse, rsq),
    .point_size = 4,
    .point_alpha = 0.8,
    .facet_ncol = 1
  )

# 8.0 ENSEMBLE PANEL MODELS -----

# * Average Ensemble ----

submodels_2_ids_to_keep <- c(3, 6, 10, 5, 2)

ensemble_fit <- submodels_2_tbl %>%
  filter(.model_id %in% submodels_2_ids_to_keep) %>%
  ensemble_average()
ensemble_fit

model_ensemble_tbl <- modeltime_table(ensemble_fit)
model_ensemble_tbl


# * Accuracy ----
model_ensemble_tbl %>%
  modeltime_accuracy(testing(splits))

# * Forecast ----
model_ensemble_tbl

forecasts_ensemble_test_tbl <- model_ensemble_tbl %>%
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = data_prepared_tbl,
    keep_data = TRUE
  )
forecasts_ensemble_test_tbl
forecasts_ensemble_test_tbl <- forecasts_ensemble_test_tbl %>%
  mutate(across(.cols=c(.value, pageViews), .fns = expm1))
forecasts_ensemble_test_tbl

forecasts_ensemble_test_tbl %>%
  group_by(pagePath) %>%
  plot_modeltime_forecast(
    .facet_ncol = 4,
    .legend_show = F
  )

forecasts_ensemble_test_tbl %>%
  filter(.key == "prediction") %>%
  group_by(pagePath) %>%
  select(pagePath, .value, pageViews) %>%
  summarize_accuracy_metrics(truth = pageViews,
                             estimate = .value,
                             metric_set = default_forecast_accuracy_metric_set())

# * Refit ----

data_prepared_tbl_clean <- data_prepared_tbl %>%
  group_by(pagePath) %>%
  mutate(pageViews = ts_clean_vec(pageViews, period = 7)) %>%
  ungroup()

model_ensemble_refit_tbl <- model_ensemble_tbl %>%
  modeltime_refit(data_prepared_tbl_clean)
model_ensemble_refit_tbl

model_ensemble_refit_tbl %>%
  modeltime_forecast(new_data = future_tbl,
                     actual_data = data_prepared_tbl,
                     keep_data = TRUE) %>%
  mutate(
    .value = expm1(.value),
    pageViews = expm1(pageViews)
  ) %>%
  group_by(pagePath) %>%
  plot_modeltime_forecast(.facet_ncol = 4,
                          .y_intercept = 0)

# * Turn OFF Parallel Backend
plan(sequential)

# 9.0 RECAP ----
# - You:
#     1. Prepared 20 Time Series Groups
#     2. Modeled Panel Data
#     3. Hyper Parameter Tuned
#     4. Resampled & Evaluated Accuracy Over Time
#     5. Ensembled the models using a strategy based on resample stability
#     6. RMSE 143, MAE 46, RSQ 0.40
# - This code can work for 10,000 Time Series.
#     1. Only expense is Hyper Parameter Tuning
#     2. Watch Saving Ensembles & Models - Memory Size


