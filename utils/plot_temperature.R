library(tidyverse)
source("./utils/K_to_C.R")

plot_temperature <- function(sims, plot_name) {
    sims %>%
        # extract loc from run
        mutate(date = with_tz(traj_dt, "Australia/Sydney")) %>%
        mutate(site = str_extract(run, "(?<=loc_)\\w+")) %>%
        ggplot() +
        geom_line(aes(date, K_to_C(air_temp), group = run)) +
        geom_bar(stat = "identity", aes(date, rainfall * 24, group = run), fill = "blue", alpha = 0.4) +
        facet_wrap(~site) +
        scale_x_datetime(date_breaks = "1 day", date_labels = "%d %b %Y") +
        ylab("Temp (C) or daily rainfall (mm)") +
        xlab("Date") +
        ggtitle(plot_name)

    ggsave(paste0("./plots/", plot_name, ".png"), width = 8)
}
