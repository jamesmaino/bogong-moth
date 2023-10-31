library(tidyverse)
source("./utils/get_silo_data.R")
source("./utils/load_trap_data.R")
source("./utils/calc_hourly_temp.R")


# load traps but only include SA and NSW traps
d <- load_trap_data() %>%
    filter(loc %in% c("Lareldale, Armidale", "Point Lookout", "Mt. Dowe", "Kingstown", "Newholme", "Thora", "Turretfield"))

dsum <- d %>%
    group_by(loc, lon, lat) %>%
    summarise(
        year_min = min(year(date)),
        year_max = max(year(date))
    ) %>%
    rowwise() %>%
    mutate(silo = list(get_silo_data(
        yearstart = year_min, yearfinish = year_max, longitude = lon, latitude = lat
    )))

dsum[1, ]$silo

climate <- dsum %>%
    transmute(loc, lon, lat, date = list(silo$`YYYY-MM-DD`), min_temp = list(silo$min_temp), max_temp = list(silo$max_temp)) %>%
    unnest(cols = c(date, min_temp, max_temp))

# offset date by one day to get previous day temp
climate <- climate %>%
    mutate(date = date - 1)

df <- d %>%
    left_join(climate) %>%
    select(loc, daily_count, min_temp, max_temp) %>%
    rowwise() %>%
    mutate(temp8pm = calc_hourly_temp(min_temp, max_temp, hour = 20)) %>%
    mutate(temp_round = round(temp8pm)) %>%
    group_by(loc, temp_round) %>%
    summarise(total_count = sum(daily_count, na.rm = TRUE)) %>%
    arrange(loc, temp_round) %>%
    group_by(loc) %>%
    mutate(total_count_cumulative = cumsum(total_count))

df %>%
    ggplot(aes(temp_round, total_count_cumulative, color = loc)) +
    geom_point() +
    geom_line(alpha = 0.5) +
    guides(color = "none") +
    facet_wrap(~loc, scales = "free")
ggsave("./results/temperature_vs_daily_catch.png", width = 10, height = 6)

m1 <- lm(log(daily_count) ~ max_temp * loc, data = filter(df, daily_count != 0))
summary(m1)
anova(m1)
