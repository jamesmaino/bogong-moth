## commands for first run to install dependencies
# install.packages(c("devtools", "here","tidyverse", "lubridate", "sf", "ozmaps"))
# devtools::install_github("rich-iannone/splitr")

# TO DO
# [x] extract parameters in config/parameters.R
# [x] Update season definitions
# [x] refactor scripts so that function are more modular
# [x] update sig catch plotting scripts so that they show parameters
# [x] update main simulation script
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
    print(i)
    sims[[i]] <- sim %>%
        mutate(sim_name = paste(d_i$loc, d_i$date))
}
sims <- bind_rows(sims)

source("./utils/plot_trajectory.R")
plot_trajectory(sims)

# plot temperature profile
source("./utils/plot_temperature.R")
plot_temperature(sims)

# Plot predicted origin of flight
source("./utils/plot_origin.R")
plot_origin(sims, 0.5)
