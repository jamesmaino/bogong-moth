## commands for first run to install dependencies
# install.packages(c("devtools", "here","tidyverse", "lubridate", "sf", "ozmaps"))
# devtools::install_github("rich-iannone/splitr")

# TO DO
# [x] extract parameters in config/parameters.R
# [x] Update season definitions
# [x] refactor scripts so that function are more modular
# [x] update sig catch plotting scripts so that they show parameters
# [ ] update main simulation script
# [ ] create get_climate_data function that downloads and locally stores climatic data
# [ ] run life cycle simulations from each sig catch peak

# Plots of seasonal abundance, with sig catches marked and generation times
# Quick catches vs temperature analysis
# Trajectory plots, origin and trajectory heatmaps for each year and season,
# filter trajectories based on 5, 10, 15C threshold


source("./setup.R")


source("./identify_sig_catch.R")

source("./config/parameters.R")

# sim_loc_and_date_settings <- read_csv("./config/sim_loc_date_settings.csv")
sim_loc_and_date_settings <- read_csv("./config/sim_loc_date_settings_manual.csv")

source("./utils/run_trajectory.R")
sims <- list()
for (i in 1:nrow(sim_loc_and_date_settings)) {
    # for (i in 9:11) {
    d_i <- sim_loc_and_date_settings[i, ]
    sim <- run_trajectory(
        lat = d_i$lat,
        lon = d_i$lon,
        height = HEIGHT,
        days_AEDT = d_i$date,
        daily_hours_AEDT = END_TIME,
        duration = DURATION,
        n_steps = N_STEPS,
        max_n_steps = MAX_N_STEPS,
        temp_thresh = TEMP_THRESH,
        ignore_cache = FALSE
    )

    if (all(is.na(sim))) next

    sims[[i]] <- sim %>%
        mutate(sim_name = paste(d_i$loc, d_i$date))
}
sims <- bind_rows(sims)

# plot trajectories
parameters <- list(
    height = HEIGHT,
    n_steps = N_STEPS,
    temp_thresh = TEMP_THRESH
)
param_string <- paste0(names(parameters), "", unlist(lapply(parameters, paste)), collapse = "_")


source("./utils/get_season.R")

source("./utils/plot_trajectory.R")
sims %>%
    filter(is.na(step))
plot_trajectory(sims, folder = "trajectories", plot_name = "all", param_string)
for (season_i in c("Summer", "Autumn", "Winter", "Spring")) {
    sims %>%
        mutate(season = get_season(traj_dt_i)) %>%
        filter(season == season_i) %>%
        plot_trajectory("trajectories", season_i, param_string)
}

# plot temperature profile
plot_temperature(sims, "temperature_all")

# Plot predicted origin of flight
plot_origin(sims_filtered, "origin_filtered")
plot_origin(sims, "origin_all")
for (season_i in c("Summer", "Autumn", "Winter", "Spring")) {
    sims_filtered %>%
        filter(season == season_i) %>%
        plot_origin(paste0("origin_filtered_", season_i))
}
