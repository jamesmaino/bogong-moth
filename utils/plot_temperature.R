library(tidyverse)
source("./utils/K_to_C.R")

save_temperature_plot <- function(sims, plot_name, plot_title = plot_name) {
    u_sim_names <- unique(sims$sim_name)
    if (length(u_sim_names) > 12) {
        plot_title <- paste("Only first 12 simulations shown", plot_title)
        u_sim_names <- u_sim_names[1:12]
    }

    sims %>%
        filter(sim_name %in% u_sim_names) %>%
        # extract loc from run
        mutate(site = str_extract(run, "(?<=loc_)\\w+")) %>%
        ggplot() +
        geom_point(aes(traj_dt, K_to_C(air_temp), group = run)) +
        geom_bar(stat = "identity", aes(traj_dt, rainfall * 24, group = run), fill = "blue", alpha = 0.4) +
        facet_wrap(~site) +
        scale_x_datetime(date_breaks = "1 day", date_labels = "%d %b %Y") +
        ylab("Temp (C) or daily rainfall (mm)") +
        xlab("Date") +
        ggtitle(plot_title) +
        facet_wrap(~sim_name, scales = "free")

    folder_name <- "temperature/"
    ggsave(paste0("./results/plots/", folder_name, plot_name, ".png"), width = 12, height = 8)
}

source("./utils/make_parameter_string.R")
param_string <- make_parameter_string()

plot_temperature <- function(sims) {
    save_temperature_plot(sims, plot_name = "all", param_string)
    for (season_i in c("Summer", "Autumn", "Winter", "Spring")) {
        try({
            sims %>%
                mutate(season = get_season(traj_dt_i)) %>%
                filter(season == season_i) %>%
                save_temperature_plot(season_i, param_string)
        })
    }
}
