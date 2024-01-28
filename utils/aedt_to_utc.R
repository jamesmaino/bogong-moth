library(lubridate)

aedt_to_utc <- function(dates, hours) {
    # Convert input to datetime in AEDT

    datetime_utc <- mapply(function(date, hour) {
        datetime_aedt <- ymd_h(paste(date, hour), tz = "Australia/Sydney")
        with_tz(datetime_aedt, "UTC")
    }, dates, hours, SIMPLIFY = TRUE)

    return(as.POSIXct(datetime_utc, tz = "UTC"))
}

# # Test
# dates <- c("1980-11-13", "1981-01-05")
# hours <- c(6, 12)

# result <- utc_to_aedt(dates, hours)
# print(result)
