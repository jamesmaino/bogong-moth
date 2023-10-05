library(tidyverse)
library(scales)
source("./utils/yday_to_date.R")
source("./utils/load_trap_data.R")

d <- load_trap_data() %>%
    mutate(season_year = factor(year(date)))

sig_catch <- read_csv("./data/sig_catch.csv") %>%
    mutate(season_year = factor(year(date)))

# number of months spanned by sig catches
sig_catch %>%
    mutate(year_month = format(date, "%Y %b")) %>%
    distinct(year_month)

# check if no sig catch identified
for (iloc in unique(d$loc)) {
    dsub <- sig_catch %>%
        filter(loc == iloc)
    if (nrow(dsub) == 0) warning(sprintf("no sig catches for %s", iloc))
}

# plot
for (iloc in unique(d$loc)) {
    isig_catch <- sig_catch %>%
        filter(loc == iloc)

    p <- d %>%
        filter(loc == iloc) %>%
        # filter(daily_count != 0) %>%
        ggplot(aes(yday_to_date(yday(date)), daily_count, color = season_year)) +
        geom_line() +
        geom_point(data = isig_catch, shape = 21, size = 2) +
        geom_hline(yintercept = isig_catch$daily_count_thresh[1], linetype = 2, color = "grey") +
        # scale_y_log10() +
        scale_x_date(date_break = "1 month", date_labels = "%b") +
        scale_color_viridis_d(name = "Year", option = "A", end = 0.9) +
        # facet_grid(loc ~ .) +
        xlab("") +
        ylab("Daily count") +
        # guides(color = "none") +
        theme_bw() +
        ggtitle(
            iloc,
            paste0(
                "SEASONAL COUNT THRESH = ", isig_catch$seasonal_count_thresh[1], "X\n",
                "DAILY COUNT THRESH = ", isig_catch$daily_count_thresh[1]
            )
        )

    ggsave(
        sprintf("./plots/sig_catch/%s.png", iloc),
        p,
        width = 8, height = 5
    )

    p_log <- p + scale_y_log10(labels = label_comma())
    ggsave(
        sprintf("./plots/sig_catch/%s_log.png", iloc),
        p_log,
        width = 8, height = 5
    )
}
