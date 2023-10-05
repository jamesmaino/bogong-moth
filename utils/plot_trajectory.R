library(tidyverse)
library(sf)
library(ozmaps)

aus <- ozmap_data() %>%
    st_transform(4326)


plot_trajectory <- function(sims, folder, plot_name, plot_title = plot_name) {
    trap_sites <- sims %>%
        filter(step == 1) %>%
        group_by(lon_i, lat_i) %>%
        summarise(n = n()) %>%
        st_as_sf(coords = c("lon_i", "lat_i"), crs = 4326)

    sims <- sims %>%
        mutate(date = factor(traj_dt_i)) %>%
        mutate(step = factor(step))
    p <- ggplot() +
        geom_sf(data = aus) +
        geom_point(data = trap_sites, aes(geometry = geometry), stat = "sf_coordinates") +
        geom_path(data = sims, aes(lon, lat, group = sim_name, color = step), alpha = 0.5) +
        # guides(color = "none") +
        coord_sf(xlim = c(135, 155), ylim = c(-25, -45)) +
        ggtitle(plot_title)


    plot_folder <- paste0("./results/plots/", folder)
    dir.create(plot_folder, recursive = TRUE, showWarnings = FALSE)
    ggsave(paste0(plot_folder, "/", plot_name, ".png"), p)
}
