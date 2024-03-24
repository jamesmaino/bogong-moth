library(tidyverse)
library(scales)
source("./utils/yday_to_date.R")
source("./utils/load_trap_data.R")


# a function to convert a vector of years into a vector of groupings where consecutive years are grouped together as ranges (e.g., "1980-1981") and standalone years are left as is
createYearGroups <- function(years) {
  # Ensure the years are sorted and unique for grouping
  unique_years <- unique(sort(years))
  
  # Create a vector to identify breaks in consecutive years for unique years
  is_break <- c(TRUE, diff(unique_years) != 1)
  
  # Generate group IDs based on breaks (each TRUE in is_break starts a new group)
  group_id <- cumsum(is_break)
  
  # Prepare a named vector mapping each unique year to its group
  year_to_group_map <- setNames(rep(NA, length(unique_years)), unique_years)
  
  # Map each group ID to its range or single year for unique years
  for(gid in unique(group_id)) {
    current_years <- unique_years[group_id == gid]
    group_str <- if(length(current_years) > 1) {
      paste(min(current_years), max(current_years), sep = "-")
    } else {
      as.character(current_years)
    }
    year_to_group_map[names(year_to_group_map) %in% current_years] <- group_str
  }
  
  # Map back the group ranges to the original years using the prepared map
  year_groups <- year_to_group_map[as.character(years)]
  
  return(year_groups)
}

# Example usage with duplicates
years <- c(1980, 1980, 1981, 1983, 1985)
createYearGroups(years)

load_trap_data() %>%
    mutate(year = year(date)) %>% 
    group_by(loc, year) %>% 
    summarise(n = n())

# full data set
d <- load_trap_data() %>%
    mutate(year = year(date)) %>%
    mutate(season_year = factor(year(date))) %>%
    group_by(loc) %>% 
    mutate(year_group = createYearGroups(year))  %>% 
    ungroup()
    
distinct(d, loc, season_year, year_group)
# only sig catches
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

lifecycle <- sig_catch %>%
    rowwise() %>%
    mutate(
        lifecycle_span = list(c(
            yday_to_date(yday(date)),
            yday_to_date(yday(date) + lifecycle_duration)
        ))
    ) %>%
    unnest(lifecycle_span)

# plot
for (iloc in unique(d$loc)) {
    isig_catch <- sig_catch %>%
        filter(loc == iloc)

    i_lifecycle <- lifecycle %>%
        filter(loc == iloc)

    i_end_lifecycle <- i_lifecycle %>%
        group_by(season_year, loc, date) %>%
        summarise(
            daily_count = daily_count[1],
            date = date[1],
            lifecycle_duration = lifecycle_duration[1]
        )

    p <- d %>%
        filter(loc == iloc) %>%
        # filter(daily_count != 0) %>%
        ggplot(aes(yday_to_date(yday(date)), daily_count, color = loc)) +
        geom_line() +
        geom_point(data = isig_catch, shape = 21, size = 2) +
        geom_point(data = i_end_lifecycle, aes(x = yday_to_date(yday(date)) + lifecycle_duration), size = 1, alpha = 0.3) +
        geom_point(aes(y = rep(0, length(date))), color = "#9c9c9c", shape = 15, size = 2) +
        geom_hline(yintercept = isig_catch$daily_count_thresh[1], linetype = 2, color = "grey") +
        geom_line(data = i_lifecycle, aes(x = lifecycle_span, group = paste(loc, date)), linetype = 3) +
        # scale_y_log10() +
        scale_x_date(date_break = "1 month", date_labels = "%b") +
        scale_color_manual(values = c("#4f4f4f")) +
        facet_wrap(~season_year) +
        xlab("") +
        ylab("Daily count") +
        guides(color = "none") +
        theme_bw() +
        ggtitle(
            iloc,
            # paste0(
            #     "SEASONAL COUNT THRESH = ", isig_catch$seasonal_count_thresh[1], "X\n",
            #     "DAILY COUNT THRESH = ", isig_catch$daily_count_thresh[1]
            # )
        ) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    p
    ggsave(
        sprintf("./results/plots/sig_catch/%s.png", iloc),
        p,
        width = 8, height = 5
    )

    p_log <- p + scale_y_log10(labels = scales::label_comma(), 
    limits = c(0.1,max(d$daily_count))) 
    ggsave(
        sprintf("./results/plots/sig_catch/%s_log.png", iloc),
        p_log,
        width = 8, height = 5
    )

    if (iloc == "Turretfield") {
        ggsave(
            sprintf("./results/plots/sig_catch/%s_log_large.png", iloc),
            p_log,
            width = 13, height = 20
        )
    }
}
