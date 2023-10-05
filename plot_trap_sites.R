source("setup.R")
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
