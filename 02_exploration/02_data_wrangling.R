# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: TIME SERIES DATA WRANGLING ----


# GOAL ----
# - Gain exposure to timetk data wrangling functionality

# OBJECTIVES ----
# - Summarize/Pad - Manipulate Data to different periodicities (scales, intervals)
# - Filter - Zoom in & Slice Time Series
# - Mutate - Apply mutations by time groups
# - Joining Time Series
# - Index Operations
# - Future Frame - Forecasting Exposure



# LIBRARIES ----

library(tidyverse)
library(timetk)
library(lubridate)
library(DataExplorer)

# DATA ----

google_analytics_summary_tbl <- read_rds("00_data/google_analytics_summary_hourly.rds")
google_analytics_summary_tbl

mailchimp_users_tbl  <- read_rds("00_data/mailchimp_users.rds")
mailchimp_users_tbl

transactions_tbl  <- read_rds("00_data/transactions_weekly.rds")
transactions_tbl


# 1.0 SUMMARIZE BY TIME  ----
# - APPLY COMMON AGGREGATIONS
# - HIGH TO LOW FREQ

# * To Daily - Subscribers ----
subscribers_daily_tbl <- mailchimp_users_tbl %>%
  summarise_by_time(optin_time,
                    .by = "day",
                    optins = n())
subscribers_daily_tbl

# * To Daily - GA Summary ----



# * To Weekly - Subscribers ----



# * To Monthly - Transactions ----



# 2.0 PAD BY TIME ----
# - Filling in Gaps
# - Going from Low to High Frequency (un-aggregating)

# * Fill Daily Gaps ----




# * Weekly to Daily ----





# 3.0 FILTER BY TIME ----
# - Pare data down before modeling

# * Slicing - Everything after the BIG anomaly ----


# * Zooming In - Just December 2018 ----


# * Offsetting - Using plus-time and minus-time offsets to get things just right ----


# 4.0 MUTATING BY TIME -----
# - Get change from beginning/end of period

# * First, Last, Mean, Median by Period ----





# 5.0 JOINING BY TIME ----
# - Investigating Relationships
# - Identify External Regressors

# * Subscribers + GA Summary Web Traffic ----
subscribers_daily_padded_tbl <- subscribers_daily_tbl %>%
  pad_by_time(.date_var = optin_time,
              .pad_value = 0,
              .start_date = "2018-06")

google_analytics_daily_summary_tbl <- google_analytics_summary_tbl %>%
  mutate(dateHour = ymd_h(dateHour)) %>%
  summarise_by_time(.date_var = dateHour,
                    .by = "day",
                    across(pageViews:sessions,
                           sum))

sub_goog_joined_tbl <- subscribers_daily_padded_tbl %>%
  left_join(google_analytics_daily_summary_tbl,
            by = c("optin_time" = "dateHour"))
sub_goog_joined_tbl


# * Inspect Join -----
sub_goog_joined_tbl %>% plot_missing()
google_analytics_daily_summary_tbl %>% tk_summary_diagnostics()
sub_goog_joined_tbl %>% tk_summary_diagnostics()

sub_goog_joined_tbl %>%
  pivot_longer(-optin_time) %>%
  plot_time_series(optin_time,value,.color_var=name, .smooth = F)

# * Visualization Techniques (Relationships) ----
sub_goog_joined_tbl %>%
  drop_na() %>%
  mutate(across(optins:sessions, .fns = log1p)) %>%
  pivot_longer(-optin_time) %>%
  plot_time_series(optin_time,value,.color_var=name, .smooth = F)

log_std_sub_joined_tbl <- sub_goog_joined_tbl %>%
  drop_na() %>%
  mutate(across(optins:sessions, .fns = log1p)) %>%
  mutate(across(optins:sessions, .fns = standardize_vec))

log_std_sub_joined_tbl %>%
  pivot_longer(-optin_time) %>%
  plot_time_series(optin_time,value,.color_var=name, .smooth = F)




# 6.0 WORKING WITH THE INDEX ----
# - Index Manipulations

subscribers_daily_tbl %>% tk_index() %>% str()

# * Making an index ----
tibble(
  values = 1:100,
  dates = tk_make_timeseries(start_date = "2011",
                     by = "month",
                     length_out = 100)
)


# * Holiday Sequence ----
tk_make_holiday_sequence(start_date = "2011")
tk_make_holiday_sequence(start_date = "2011", calendar = "NYSE") %>%
  tk_get_holiday_signature() %>%
  glimpse()

# * Offsetting time ----

"2011-01-01" %+time% "1 day"
"2011-01-01" %+time% "4 hour"

# * Extending an index ----




# 7.0 FUTURE FRAME ----
# - Forecasting helper



# * Future Frame ----



# * Modeling ----



# * Visualizing ----



