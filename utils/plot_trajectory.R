library(tidyverse)
library(sf)
library(ozmaps)

source("./utils/get_season.R")
source("./config/parameters.R")

# make australia shape file
aus <- ozmap_data() %>%
    st_transform(4326)

source("./utils/make_parameter_string.R")
param_string <- make_parameter_string()

# function for plotting
save_trajectory_plot <- function(sims, plot_name, plot_title = plot_name) {
    trap_sites <- sims %>%
        filter(step == 1) %>%
        group_by(lon_i, lat_i) %>%
        summarise(n = n()) %>%
        st_as_sf(coords = c("lon_i", "lat_i"), crs = 4326)

    sims <- sims %>%
        mutate(date = factor(traj_dt_i)) %>%
        mutate(step = factor(step))
    p <- ggplot() +
        geom_sf(data = aus, fill = "white") +
        geom_point(data = trap_sites, aes(geometry = geometry), stat = "sf_coordinates") +
        # geom_path(data = sims, aes(lon, lat, group = sim_name, color = step)) +
        geom_path(data = subset(sims, step != 3), aes(lon, lat, group = sim_name, color = step), alpha = 0.3, linetype = "solid") +
        geom_path(data = subset(sims, step == 3), aes(lon, lat, group = sim_name), color = "grey", alpha = 0.3, linetype = "dashed") +
        scale_color_manual(values = c("black", "grey", "black")) +
        scale_linetype_manual(values = c(1, 1, 4)) +
        guides(color = "none") +
        coord_sf(xlim = c(135, 155), ylim = c(-25, -45)) +
        ggtitle(plot_name, plot_title) +
        theme_bw() +
        xlab("") +
        ylab("")
    p
    plot_folder <- paste0("./results/plots/", "trajectories/")
    dir.create(plot_folder, recursive = TRUE, showWarnings = FALSE)
    ggsave(paste0(plot_folder, plot_name, ".png"), p, width = 8, height = 8)
}


# make plots
plot_trajectory <- function(sims) {
    save_trajectory_plot(sims, plot_name = "all", param_string)
    for (season_i in c("Summer", "Autumn", "Winter", "Spring")) {
        sims %>%
            mutate(season = get_season(traj_dt_i)) %>%
            filter(season == season_i) %>%
            save_trajectory_plot(season_i, param_string)
    }
}
