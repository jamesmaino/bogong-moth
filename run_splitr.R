## commands for first run to install dependencies
# install.packages(c("here","tidyverse", "lubridate", "sf", "ozmaps"))
# devtools::install_github("rich-iannone/splitr")

library(splitr)
library(here)
library(tidyverse)
library(lubridate)
library(sf)
library(ozmaps)

# simulation options
HEIGHT <- 50 # the starting height in m
START_TIME <- 6 # start time of simulation in AEDT
DURATION <- 12 # assume flight start 12 hours prior (6pm previous day)
RAINFALL_THRESH <- 99999 # min rainfall (mm) threshold for migration (summed across flight path)
TEMPERATURE_THRESH <- 4 # min drop in temperature (C) threshold for migration (from start to end of flight path)
PRESSURE_THRESH <- 6 # min drop in pressure (mb) threshold for migration (from start to end of flight path)

# useful constants
AEDT_TO_UTC_OFFSET <- -11 # offset to convert AEDT time to UTC

options(timeout = 5 * 60) # allow more time to download climatic files

# load significant catch data made by identify_sig_catch.R
# create a new date for each day in the sample window
d <- read_csv("./data/sig_catch.csv") %>%
    mutate(date_sampled = as.Date(date)) %>%
    # head(n = 100) %>%
    rowwise() %>%
    mutate(date = list(seq.Date(date_sampled - timespan + 1, date_sampled, by = 1))) %>%
    ungroup() %>%
    unnest(date) %>%
    distinct() %>%
    mutate(year_month = format(date))

# create met and out dit for simulations
dir.create("met", showWarnings = FALSE)
dir.create("out", showWarnings = FALSE)

# for each date run a backwards simulation from 6am AEDT at the trap site to 6pm the previous night
for (i in 101:nrow(d)) {
    d_i <- d[i, ]
    cat(sprintf("running simulation %d of %d...\n", i, nrow(d)))
    run_name <- sprintf("date_%s_loc_%s", d_i$date, d_i$loc)
    try({
        trajectory <-
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
            mutate(run = run_name)
        write_csv(trajectory, sprintf("./sims/%s.csv", run_name))
    })
}


sim_files <- list.files("sims", ".csv", full.names = TRUE)

# bind data and calculate climatic variables
get_season <- function(input.date) {
    m <- month(input.date)
    cuts <- base::cut(m, breaks = c(0, 2, 5, 8, 11, 12))
    # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
    levels(cuts) <- c("Summer", "Autumn", "Winter", "Spring", "Summer")
    return(cuts)
}

sims <- bind_rows(lapply(sim_files, read_csv, col_types = cols())) %>%
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

# filter flight trajectories based on climatic conditions
sims_filtered <- sims %>%
    filter(
        rainfall > RAINFALL_THRESH |
            temp_change < -TEMPERATURE_THRESH |
            pressure_change < -PRESSURE_THRESH
    )


# Plot trajectories
aus <- ozmap_data() %>%
    st_transform(4326)

dsum <- d %>%
    group_by(loc, lon, lat) %>%
    summarise(n = n()) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

plot_trajectory <- function(sims, plot_name) {
    p <- ggplot(dsum) +
        geom_sf(data = aus) +
        geom_point(aes(geometry = geometry), stat = "sf_coordinates") +
        geom_line(data = sims, aes(lon, lat, group = run), alpha = 0.5) +
        coord_sf(xlim = c(135, 155), ylim = c(-25, -45)) +
        ggtitle(plot_name)
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
