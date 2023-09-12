## commands for first run to install dependencies
# install.packages(c("devtools", "here","tidyverse", "lubridate", "sf", "ozmaps"))
# devtools::install_github("rich-iannone/splitr")

library(splitr)
library(here)
library(tidyverse)
library(lubridate)
library(sf)
library(ozmaps)

# to do
# incorporate the climatic data at destination
# incorporate multi-night trajectory assuming consecutive nights

# simulation options
HEIGHT <- 1000 # the starting height in m
START_TIME <- 6 # start time of simulation in AEDT
DURATION <- 12 # assume flight start 12 hours prior (6pm previous day)
RAINFALL_THRESH <- 99999 # min rainfall (mm) threshold for migration (summed across flight path)
TEMPERATURE_THRESH <- 4 # min drop in temperature (C) threshold for migration (from start to end of flight path)
PRESSURE_THRESH <- 6 # min drop in pressure (mb) threshold for migration (from start to end of flight path)
NSTEPS <- 2 # number of steps to run

#  [1] "Maffra"               "HORSHAM"              "HAMILTON"
#  [4] "BALLARAT"             "RUTHERGLEN"           "Yanakie"
#  [7] "SWANHILL"             "ARARAT"               "FOWLERS GAP"
# [10] "Turretfield"          "Mt. Dowe"             "Point Lookout"
# [13] "Laureldale, Armidale" "Newholme"             "Thora"
# Case sensitive!
LOCATIONS <- c("ARARAT", "Maffra")

DATES <- c() # leave empty to run all sig_catch dates for selected season/year. Dates overide season, year
MONTHS <- c(11, 12) # leave empty to run all sig_catch dates for selected season/year. Months overide season
SEASONS <- c() # "Summer", "Autumn", "Winter", "Spring", "Summer"
YEARS <- c(1980)

# useful constants
AEDT_TO_UTC_OFFSET <- -10 # offset to convert AEDT time to UTC

options(timeout = 5 * 60) # allow more time to download climatic files

# load significant catch data made by identify_sig_catch.R
# create a new date for each day in the sample window
d0 <- read_csv("./data/sig_catch.csv")

# useful functions
get_season <- function(input.date) {
    m <- month(input.date)
    cuts <- base::cut(m, breaks = c(0, 2, 5, 8, 11, 12))
    # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
    levels(cuts) <- c("Summer", "Autumn", "Winter", "Spring", "Summer")
    return(cuts)
}

d <- d0 %>%
    mutate(date_sampled = as.Date(date)) %>%
    filter(loc %in% LOCATIONS) %>%
    # head(n = 100) %>%
    rowwise() %>%
    mutate(date = list(seq.Date(date_sampled - timespan + 1, date_sampled, by = 1))) %>%
    ungroup() %>%
    unnest(date) %>%
    distinct() %>%
    mutate(year_month = format(date))

# filter by dates if DATES specified
if (length(DATES) == 0 && length(MONTHS) > 0) {
    d <- d %>%
        filter(month(date) %in% MONTHS) %>%
        filter(year(date) %in% YEARS)
} else if (length(DATES) == 0 && MONTHS > 0) {
    d <- d %>%
        filter(get_season(date) %in% SEASONS) %>%
        filter(year(date) %in% YEARS)
} else {
    d <- d %>%
        filter(as.Date(date) %in% as.Date(DATES))
}

if (nrow(d) == 0) {
    stop("no significant catch data found for selected locations or dates")
}

# create met and out dit for simulations
dir.create("met", showWarnings = FALSE)
dir.create("out", showWarnings = FALSE)
dir.create("sims", showWarnings = FALSE)

aus <- ozmap_data() %>%
    st_transform(4326)

# for each date run a backwards simulation from 6am AEDT at the trap site to 6pm the previous night

# recursively call hysplit_trajectory where the final point becomes the input to the next sim

sims <- list()
for (i in 1:nrow(d)) {
    # for (i in 1) {
    d_i <- d[i, ]
    cat(sprintf("running simulation %d of %d...\n", i, nrow(d)))
    run_name <- sprintf("date_%s_loc_%s", d_i$date, d_i$loc)
    try({
        trajectory <- list()
        for (j in 1:NSTEPS) {
            # 1 Run HYSPLIT backwards at 1000 m.  Is the endpoint above land?  Yes (3), No (2)
            trajectory1 <-
                hysplit_trajectory(
                    lat = d_i$lat,
                    lon = d_i$lon,
                    height = HEIGHT,
                    duration = DURATION,
                    days = d_i$date,
                    daily_hours = START_TIME + AEDT_TO_UTC_OFFSET,
                    direction = "backward",
                    met_type = "reanalysis",
                    extended_met = TRUE,
                    met_dir = here::here("met"),
                    exec_dir = here::here("out")
                ) %>%
                mutate(run = run_name) %>%
                mutate(timespan = d_i$timespan)
            # 2 Move to the point where the trajectory crosses the coast, note the location and time. Go to (3)
            trajectory2 <- st_as_sf(trajectory1, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
            trajectory2 <- st_intersection(aus, trajectory2)
            if (nrow(trajectory2) == 0) break
            trajectory3 <- trajectory2[nrow(trajectory2), ]

            # 3 Is the 1000 m temp above 10C? Yes (5) No (4)
            # if(trajectory2$air_temp - 273.15 < 10) # we may as well run 4

            # 3.5 Is the 1000 m temp at origin above 10C? Yes (4) No (6) (as per M Smith suggestion)
            if (trajectory3$air_temp - 273.15 < 10) break

            # 4 Run HYSPLIT at 0 m from either the original or revised (coastal) starting point (run backwards or forwards, doesn’t matter, we just want the starting surface temp).  Is that temp above 10C? Yes (5), No (6)
            trajectory4 <-
                hysplit_trajectory(
                    lat = trajectory3$lat,
                    lon = trajectory3$lon,
                    height = 0,
                    duration = 0,
                    days = as.Date(trajectory3$traj_dt),
                    daily_hours = as.numeric(format(trajectory3$traj_dt, "%H")),
                    direction = "backward",
                    met_type = "reanalysis",
                    extended_met = TRUE,
                    met_dir = here::here("met"),
                    exec_dir = here::here("out")
                )
            if (trajectory4$air_temp - 273.15 < 10) break

            # 5 Accept the trajectory
            trajectory[[j]] <- st_set_geometry(trajectory2, NULL)
            d_i$date <- as.Date(trajectory3$traj_dt)
            d_i$lat <- trajectory3$lat
            d_i$lon <- trajectory3$lon
        }
        sims[[i]] <- bind_rows(trajectory)
    })
}


# bind data and calculate climatic variables
sims <- bind_rows(sims) %>%
    group_by(run) %>%
    mutate(rainfall = sum(rainfall)) %>%
    mutate(temp_change = air_temp[traj_dt == min(traj_dt)] - air_temp[traj_dt == max(traj_dt)]) %>%
    mutate(pressure_change = pressure[traj_dt == min(traj_dt)] - pressure[traj_dt == max(traj_dt)]) %>%
    mutate(season = get_season(as.Date(traj_dt))) %>%
    ungroup()

# check climates vars for each flight
sims %>%
    group_by(run) %>%
    mutate(minlat = min(lat)) %>%
    ungroup() %>%
    filter(minlat < -38) %>%
    distinct(run, rainfall, temp_change, pressure_change)

# # filter flight trajectories based on location
# sims_filtered <- sims %>%
#   filter(grepl("ARARAT", run)) %>%
#   filter(as.Date(traj_dt) == "1981-10-13")

# filter flight trajectories based on climatic conditions
sims_filtered <- sims %>%
    mutate(date = stringr::str_extract(run, "(?<=date_)\\d+-\\d+-\\d+")) %>%
    mutate(date = as.Date(date)) %>%
    mutate(loc = stringr::str_extract(run, "(?<=loc_).*+")) %>%
    left_join(distinct(d, date, loc, date_sampled, timespan)) %>%
    filter(
        timespan == 1 |
            rainfall > RAINFALL_THRESH |
            temp_change < -TEMPERATURE_THRESH |
            pressure_change < -PRESSURE_THRESH
    )

# ararat, hamilton, horsham, rutherglen, maffra,
# sims_filtered <- sims %>%
#     filter(grepl("HORSHAM", run)) %>%
#     filter(as.Date(traj_dt) == "1980-10-23")

# Plot trajectories


dsum <- d %>%
    group_by(loc, lon, lat) %>%
    summarise(n = n()) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

plot_trajectory <- function(sims, plot_name) {
    sims <- sims %>%
        mutate(date = factor(format(with_tz(traj_dt_i, "Australia/Sydney"), "%Y-%m-%d")))
    p <- ggplot(dsum) +
        geom_sf(data = aus) +
        geom_point(aes(geometry = geometry), stat = "sf_coordinates") +
        geom_path(data = sims, aes(lon, lat, group = run, color = date), alpha = 0.5) +
        coord_sf(xlim = c(135, 155), ylim = c(-25, -45)) +
        ggtitle(plot_name)

    if (length(DATES) == 0 && length(MONTHS) > 0) {
        p <- p +
            ggtitle(paste(paste(month.name[MONTHS], collapse = ", "), paste(YEARS, collapse = ", ")))
    } else if (length(DATES) == 0 && length(SEASONS) > 0) {
        p <- p +
            ggtitle(paste(paste(SEASONS, collapse = ", "), paste(YEARS, collapse = ", ")))
    }
    print(p)
    ggsave(paste0("./plots/", plot_name, ".png"))
}
plot_trajectory(sims, "trajectories_all")
plot_trajectory(sims_filtered, "trajectories_filtered")
for (season_i in c("Summer", "Autumn", "Winter", "Spring")) {
    sims_filtered %>%
        filter(season == season_i) %>%
        plot_trajectory(paste0("trajectories_filtered_", season_i))
}


# plot temperature profile
KelvinToCelsius <- function(TK) {
    TK - 273.15
}

plot_temperature <- function(sims, plot_name) {
    sims %>%
        # extract loc from run
        mutate(date = with_tz(traj_dt, "Australia/Sydney")) %>%
        mutate(site = str_extract(run, "(?<=loc_)\\w+")) %>%
        ggplot() +
        geom_line(aes(date, KelvinToCelsius(air_temp), group = run)) +
        geom_bar(stat = "identity", aes(date, rainfall * 24, group = run), fill = "blue", alpha = 0.4) +
        facet_wrap(~site) +
        scale_x_datetime(date_breaks = "1 day", date_labels = "%d %b %Y") +
        ylab("Temp (C) or daily rainfall (mm)") +
        xlab("Date") +
        ggtitle(plot_name)

    ggsave(paste0("./plots/", plot_name, ".png"), width = 8)
}
plot_temperature(sims, "temperature_all")

# Plot predicted origin of flight
plot_origin <- function(sims, plot_name) {
    grid_sum <- sims %>%
        filter(hour_along == -DURATION) %>%
        mutate(
            grid_x = round(lon, 0),
            grid_y = round(lat, 0)
        ) %>%
        group_by(grid_x, grid_y) %>%
        summarise(
            count = n(),
            .groups = "drop"
        ) %>%
        st_as_sf(coords = c("grid_x", "grid_y"), crs = 4326)


    ggplot() +
        geom_tile(data = grid_sum, aes(geometry = geometry, fill = count), stat = "sf_coordinates") +
        geom_sf(data = aus, color = "black", alpha = 0) +
        geom_point(data = dsum, aes(geometry = geometry), stat = "sf_coordinates") +
        coord_sf(xlim = c(135, 155), ylim = c(-25, -45)) +
        ggtitle(plot_name) +
        scale_fill_viridis_c(begin = 0.2, end = 0.9, option = "A")

    ggsave(paste0("./plots/", plot_name, ".png"))
}

plot_origin(sims_filtered, "origin_filtered")
plot_origin(sims, "origin_all")
for (season_i in c("Summer", "Autumn", "Winter", "Spring")) {
    sims_filtered %>%
        filter(season == season_i) %>%
        plot_origin(paste0("origin_filtered_", season_i))
}
