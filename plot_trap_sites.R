source("setup.R")
source("./utils/load_trap_data.R")
source("./utils/get_season.R")

library(ggrepel)
# Plot site location
aus <- ozmap_data() %>%
    st_transform(4326)

d <- load_trap_data()

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
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

ggplot(dsum) +
    geom_sf(data = aus) +
    geom_point(aes(x = lon, y = lat)) +
    geom_text_repel(aes(x = lon, y = lat, label = stringr::str_to_title(loc)), max.overlaps = 100) +
    theme_void()
ggsave("./results/plots/site_locations.png")
