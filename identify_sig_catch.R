library(tidyverse)
library(lubridate)
library(readxl)
library(sf)
library(ozmaps)

d <- read_excel("./data/All trap raw data_GPS_ok.xlsx") %>%
    mutate(Lat = as.numeric(Lat)) %>%
    rename(
        date = `Date of catch`,
        loc = Location,
        timespan = `Days of catch`,
        count = `Ai (Total)`,
        lon = Long,
        lat = Lat
    ) %>%
    mutate(daily_count = count / timespan) %>%
    drop_na()


# Plot site location
aus <- ozmap_data() %>%
    st_transform(4326)

dsum <- d %>%
    group_by(loc, lon, lat) %>%
    summarise(n = n()) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

ggplot(dsum) +
    geom_sf(data = aus) +
    geom_point(aes(geometry = geometry), stat = "sf_coordinates")
ggsave("./plots/site_locations.png")


# Find significant catches
get_season <- function(input.date) {
    m <- month(input.date)
    cuts <- base::cut(m, breaks = c(0, 2, 5, 8, 11, 12))
    # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
    levels(cuts) <- c("Summer", "Autumn", "Winter", "Spring", "Summer")
    return(cuts)
}

get_seasonal_year <- function(date) {
    return(
        factor(if_else(month(date) == 12, year(date) + 1, year(date)))
    )
}
daily_count_thresh <- 10
seasonal_count_thresh <- 2

sig_catch <- d %>%
    mutate(season = get_season(date)) %>%
    mutate(season_year = get_seasonal_year(date)) %>%
    group_by(loc, season, season_year) %>%
    mutate(mean_seasonal_daily_count = mean(daily_count)) %>%
    ungroup() %>%
    filter(
        daily_count > seasonal_count_thresh * mean_seasonal_daily_count &
            daily_count > daily_count_thresh
    )
write_csv(sig_catch, "./data/sig_catch.csv")
# number of months spanned by sig catches
sig_catch %>%
    mutate(year_month = format(date, "%Y %b")) %>%
    distinct(year_month)


# no sig catches for stoney rise
sig_catch %>%
    filter(loc == "STONEY RISE")

# OPTIONAL - Plot trap counts on log scale with significant flights marked
if (FALSE) {
    day_to_date <- function(day) {
        as.Date("2020-01-01") + day - 1
    }

    for (iloc in unique(d$loc)) {
        isig_catch <- sig_catch %>%
            filter(loc == iloc)
        d %>%
            st_set_geometry(NULL) %>%
            filter(loc == iloc) %>%
            # filter(daily_count != 0) %>%
            mutate(season_year = get_seasonal_year(date)) %>%
            ggplot(aes(day_to_date(yday(date)), daily_count, color = season_year)) +
            geom_line() +
            geom_point(data = isig_catch, shape = 21, size = 2) +
            geom_hline(yintercept = daily_count_thresh, linetype = 2, color = "grey") +
            # scale_y_log10() +
            scale_x_date(date_break = "1 month", date_labels = "%b") +
            # facet_grid(loc ~ .) +
            xlab("") +
            guides(color = "none") +
            theme_bw() +
            ggtitle(iloc)

        ggsave(sprintf("./plots/sig_catch/%s.png", iloc))
    }
}
