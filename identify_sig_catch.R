library(tidyverse)
library(lubridate)
library(readxl)
library(sf)
library(ozmaps)
source("./config/parameters.R")
source("./utils/get_season.R")
source("./utils/load_trap_data.R")

# options to specify a "significant catch"


d <- load_trap_data()

sig_catch <- d %>%
    mutate(season = get_season(date)) %>%
    mutate(season_year = year(date)) %>%
    group_by(loc, season, season_year) %>%
    mutate(mean_seasonal_daily_count = mean(daily_count)) %>%
    ungroup() %>%
    mutate(
        daily_count_thresh = DAILY_COUNT_THRESH,
        seasonal_count_thresh = SEASONAL_COUNT_THRESH
    ) %>%
    filter(
        daily_count > SEASONAL_COUNT_THRESH * mean_seasonal_daily_count &
            daily_count > DAILY_COUNT_THRESH
    )
write_csv(sig_catch, "./data/sig_catch.csv")

# expand all dates within trapping window to be used in simulations
sim_settings <- sig_catch %>%
    mutate(date_sampled = as.Date(date)) %>%
    rowwise() %>%
    mutate(date = list(seq.Date(date_sampled - timespan + 1, date_sampled, by = 1))) %>%
    ungroup() %>%
    unnest(date) %>%
    distinct()
write_csv(sim_settings, "./config/sim_loc_date_settings.csv")
