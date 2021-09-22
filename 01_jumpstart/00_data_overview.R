# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: ADVANCED TIME SERIES FORECASTING FOR BUSINESS
# MODULE: INSPECT COURSE DATASETS


# LIBRARIES ----
library(tidyverse)


# DATA -----

# * Establish Relationships ----
#   - Website traffic (Page Views, Sessions, Organic Traffic)
#   - Top 20 Pages

# GA - Summary Hourly
read_rds(file = "00_data/google_analytics_summary_hourly.rds")

# GA - Top 20 Pages, Daily
read_rds("00_data/google_analytics_by_page_daily.rds")

# * Build Relationships ----
#   - Collect emails
#   - Host Events

# Mailchimp Data, Daily
read_rds("00_data/mailchimp_users.rds")

# Learning Labs
read_rds("00_data/learning_labs.rds")


# * Generate Course Revenue ----
#   - Revenue data (aggregated at weekly interval)
#   - Product Events

# Transactions, weekly
read_rds("00_data/transactions_weekly.rds")

# Events, daily
read_rds("00_data/product_events.rds")
