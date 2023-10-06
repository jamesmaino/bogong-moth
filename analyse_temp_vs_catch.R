library(tidyverse)
source("./utils/get_silo_data.R")
d <- load_trap_data()

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

df <- d %>%
    left_join(climate) %>%
    select(loc, daily_count, min_temp, max_temp)

df %>%
    ggplot() +
    geom_point(aes(max_temp, daily_count, color = loc)) +
    scale_y_log10() +
    guides(color = "none") +
    facet_wrap(~loc, scales = "free")
ggsave("./results/temperature_vs_daily_catch.png", width = 10, height = 6)

m1 <- lm(log(daily_count) ~ max_temp * loc, data = filter(df, daily_count != 0))
summary(m1)
anova(m1)
