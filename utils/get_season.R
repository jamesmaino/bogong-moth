library(lubridate)

get_season <- function(input.date) {
    m <- month(input.date)
    cuts <- base::cut(m, breaks = c(0, 2, 5, 8, 12))
    levels(cuts) <- c("Summer", "Autumn", "Winter", "Spring", "Summer")
    return(cuts)
}
