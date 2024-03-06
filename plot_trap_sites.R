source("setup.R")
source("./utils/load_trap_data.R")
source("./utils/get_season.R")

library(ggrepel)
library(ggspatial)
# Plot site location
aus <- ozmap_data() %>%
    st_transform(4326) %>%
    filter(!NAME %in% c("Other Territories", "Australian Capital Territory"))

d <- load_trap_data() %>%
    group_by(loc) %>%
    mutate(loc = paste0(loc, "\n", format(min(date), "%Y"), "-", format(max(date), "%y")))

dsum_season <- d %>%
    group_by(loc, season) %>%
    summarise(
        total = sum(count, na.rm = T)
    ) %>%
    pivot_wider(names_from = season, values_from = total)

dsum_total <- d %>%
    group_by(loc) %>%
    summarise(
        total = sum(count),
        total_no_winter = sum(count[season != "Winter"]),
        trap_days = sum(timespan),
        mean_catch = total_no_winter / trap_days,
    )

table3 <- dsum_season %>%
    left_join(dsum_total) %>%
    arrange(desc(mean_catch))
write_csv(table3, "./results/trap_count_summary.csv")


dsum_season <- d %>%
    group_by(loc, season) %>%
    summarise(
        total = sum(count, na.rm = T)
    ) %>%
    pivot_wider(names_from = season, values_from = total)

dsum_total <- d %>%
    group_by(loc) %>%
    summarise(
        total = sum(count),
        total_no_winter = sum(count[season != "Winter"]),
        trap_days = sum(timespan),
        mean_catch = total_no_winter / trap_days,
    )

trap_count_summary <- dsum_season %>%
    left_join(dsum_total) %>%
    arrange(desc(mean_catch))
write_csv(trap_count_summary, "./results/trap_count_summary.csv")

trap_rate_summary <- d %>%
    mutate(seasonal_year = ifelse(season %in% c("Autumn", "Winter"), year(date) - 1, year(date))) %>%
    group_by(loc, seasonal_year, season) %>%
    summarise(mean_catch_rate = sum(count) / sum(timespan)) %>%
    group_by(loc, seasonal_year) %>%
    pivot_wider(names_from = season, values_from = mean_catch_rate) %>%
    relocate("Summer", .before = "Autumn") %>%
    relocate("Spring", .before = "Summer") %>%
    mutate(seasonal_year = paste(seasonal_year, "-", seasonal_year + 1))
write_csv(trap_rate_summary, "./results/trap_rate_summary.csv")


dsum <- d %>%
    group_by(loc, lon, lat) %>%
    summarise(n = n()) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
    st_set_geometry(NULL) %>%
    select(name = loc, lon, lat) %>%
    mutate(loc = "site")

states <- aus %>%
    sf::st_centroid(aus) %>%
    mutate(NAME = dplyr::recode(NAME,
        `Western Australia` = "WA",
        `Northern Territory` = "NT",
        `Queensland` = "QLD",
        `South Australia` = "SA",
        `New South Wales` = "NSW",
        Victoria = "VIC",
        Tasmania = "TAS"
    ))
states[, c("lon", "lat")] <- sf::st_coordinates(states)
states[states$NAME == "NSW", ]$lat <- -32.9
states[states$NAME == "VIC", ]$lat <- -37
states[states$NAME == "VIC", ]$lon <- 145

p <- dsum %>%
    ggplot() +
    geom_sf(data = aus, fill = "white") +
    geom_point(aes(x = lon, y = lat), shape = 21) +
    geom_text_repel(aes(x = lon, y = lat, label = name), max.overlaps = 1000, seed = 123) +
    geom_text(data = states, aes(lon, lat, label = NAME), color = "grey") +
    annotation_scale(location = "bl", width_hint = 0.5, pad_y = unit(1, "cm")) +
    coord_sf(xlim = c(135, 155), ylim = c(-25, -45)) +
    theme_bw() +
    xlab("") +
    ylab("")
print(p)

ggsave("./results/plots/site_locations.png", p, width = 10, height = 10)
