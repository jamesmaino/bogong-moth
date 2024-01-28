library(tidyverse)
library(lubridate)
library(readxl)

read_excel("./data/All trap raw data_GPS_ok.xlsx") %>%
    mutate(Date = as.Date(`Date of catch`)) %>%
    mutate(Year = year(Date), Week = lubridate::week(Date)) %>%
    group_by(Lat, Long, Location, Year, Week) %>%
    summarise(
        DateOfCatch = max(Date),
        DaysOfCatch = sum(`Days of catch`),
        TotalCatch = sum(`Ai (Total)`),
        NTrapPeriods = n(),
        Dates = paste(Date, collapse = "; "),
        Days = paste(`Days of catch`, collapse = "; "),
        Catches = paste(`Ai (Total)`, collapse = "; "),
    ) %>%
    ungroup() %>%
    write_csv("plots/data_weekly_aggregated.csv")
