round_to_increment <- function(x, increment = 0.05) {
    rounded <- round(x / increment) * increment
    return(rounded)
}


save_origin_plot <- function(sims, grid_size, plot_name, plot_title = plot_name) {
    trap_sites <- sims %>%
        filter(step == 1) %>%
        group_by(lon_i, lat_i) %>%
        summarise(n = n()) %>%
        st_as_sf(coords = c("lon_i", "lat_i"), crs = 4326)

    # grid_sum <- sims %>%
    #     filter(hour_along == -DURATION) %>%
    #     mutate(
    #         grid_x = round_to_increment(lon, grid_size),
    #         grid_y = round_to_increment(lat, grid_size)
    #     ) %>%
    #     group_by(grid_x, grid_y) %>%
    #     summarise(
    #         count = n(),
    #         .groups = "drop"
    #     ) %>%
    #     st_as_sf(coords = c("grid_x", "grid_y"), crs = 4326)

    bioreg_sum <- sims %>%
        filter(hour_along == -DURATION) %>%
        group_by(REG_NAME_7) %>%
        summarise(
            count = n(),
            .groups = "drop"
        ) %>%
        right_join(shp[, c("REG_NAME_7")]) %>%
        st_as_sf()

    bioreg_sum %>%
        st_set_geometry(NULL) %>%
        write_csv(paste0("./results/plots/origin/", plot_name, ".csv"))

    ggplot() +
        # geom_sf(data = aus, color = "black", alpha = 0) +
        geom_sf(data = bioreg_sum, aes(fill = count), na.rm = TRUE) +
        geom_point(color = "white", data = trap_sites, aes(geometry = geometry), stat = "sf_coordinates") +
        geom_point(shape = 21, data = trap_sites, aes(geometry = geometry), stat = "sf_coordinates") +
        coord_sf(xlim = c(135, 155), ylim = c(-25, -45)) +
        ggtitle(plot_name, plot_title) +
        theme_bw() +
        scale_fill_viridis_c(name = "Count", na.value = "white", begin = 0.2, end = 0.9, direction = -1, option = "A") +
        xlab("") +
        ylab("")

    # ggplot() +
    #     geom_tile(data = grid_sum, aes(geometry = geometry, fill = count, height = grid_size, width = grid_size), stat = "sf_coordinates") +
    #     geom_sf(data = aus, color = "black", alpha = 0) +
    #     geom_point(data = trap_sites, aes(geometry = geometry), stat = "sf_coordinates") +
    #     coord_sf(xlim = c(135, 155), ylim = c(-25, -45)) +
    #     ggtitle(plot_name, plot_title) +
    #     theme_bw() +
    #     scale_fill_viridis_c(begin = 0.2, end = 0.9, option = "A")

    ggsave(paste0("./results/plots/origin/", plot_name, ".png"), width = 8, height = 6)
}
# make plots
plot_origin <- function(sims, grid_size) {
    shp <- st_make_valid(st_read("./data/IBRA/IBRA7_regions/ibra7_regions.shp"))
    sims <- sims %>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
        st_transform(crs = st_crs(shp)) %>%
        st_make_valid() %>%
        st_join(shp) %>%
        st_set_geometry(NULL)

    save_origin_plot(sims, grid_size, plot_name = "all", param_string)
    for (season_i in c("Summer", "Autumn", "Winter", "Spring")) {
        try({
            sims %>%
                mutate(season = get_season(traj_dt_i)) %>%
                filter(season == season_i) %>%
                save_origin_plot(grid_size, season_i, param_string)
        })
    }
}
