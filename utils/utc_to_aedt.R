library(lubridate)

utc_to_aedt <- function(dates, hours) {
    # Convert vector of dates and hours to AEDT datetimes
    datetime_aedt <- mapply(function(date, hour) {
        datetime_utc <- ymd_h(date, hour, tz = "UTC")
        with_tz(datetime_utc, "Australia/Sydney")
    }, dates, hours, SIMPLIFY = TRUE)

    return(as.POSIXct(datetime_aedt, tz = "Australia/Sydney"))
}


# # Test
# dates <- c("1980-11-13", "1981-01-05")
# hours <- c(6, 12)

# result <- utc_to_aedt(dates, hours)
# print(result)
