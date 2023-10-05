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
