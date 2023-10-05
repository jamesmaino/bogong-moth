library(tidyverse)
library(readxl)

load_trap_data <- function() {
    return(
        read_excel("./data/All trap raw data_GPS_ok.xlsx") %>%
            mutate(Lat = as.numeric(Lat)) %>%
            rename(
                date = `Date of catch`,
                loc = Location,
                timespan = `Days of catch`,
                count = `Ai (Total)`,
                lon = Long,
                lat = Lat
            ) %>%
            mutate(daily_count = count / timespan) %>%
            drop_na()
    )
}
