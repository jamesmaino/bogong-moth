library(tidyverse)
get_silo_data <- function(
    yearstart, yearfinish, longitude, latitude) {
    filename <- paste0(
        "./cache/silo_",
        paste(yearstart, yearfinish, longitude, latitude, sep = "-"),
        ".csv"
    )
    if (file.exists(filename)) {
        silodata <- read_csv(filename, show_col_types = FALSE, progress = FALSE)
    } else {
        # get temp from silo
        params <- list(
            lat = sprintf("%1.6f", latitude),
            lon = sprintf("%1.6f", longitude),
            start = sprintf("%s0101", yearstart),
            finish = sprintf("%s1231", yearfinish),
            format = "csv",
            comment = "RXN",
            username = "john.doe@xyz.com.au",
            password = "silo"
        )
        res <- httr::GET("https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php", query = params)
        # browser()
        print(paste("downloading and caching silo data at ", filename))
        silodata <- readr::read_csv(httr::content(res, as = "text"), show_col_types = FALSE, progress = FALSE)
        silodata$jday <- format(silodata$`YYYY-MM-DD`, "%j")
        silodata <- silodata[silodata$jday != "366", ]
        write_csv(silodata, filename)
    }

    return(silodata)
}

# # test
# get_silo_data(yearstart = 2020, yearfinish = 2020, longitude = 143, latitude = -37)
