source("./utils/bogong_moth_dev_model.R")
source("./utils/get_silo_data.R")
source("./utils/develop_bug.R")

bogong <- bogong_moth()

get_lifecycle_duration <- function(yearstart, yearfinish, start_day, longitude, latitude) {
    silo <- get_silo_data(yearstart, yearfinish, longitude, latitude)

    # run darabug2 script
    # https://github.com/cesaraustralia/darabug2
    # a bit of preprocessing because we are using the darabug functions developed for rasters
    Tmin <- silo$min_temp
    TMIN <- sapply(Tmin, function(x) list(x))
    Tmax <- silo$max_temp
    TMAX <- sapply(Tmax, function(x) list(x))

    data <- develop_bug(Tmax = TMAX, Tmin = TMAX, startDay = start_day, startStage = 1, insect = bogong, gens = 1)
    duration <- sum(data[, , "Stage_duration"])
    return(duration)
}

# test
# get_lifecycle_duration(yearstart = 2022, yearfinish = 2022, start_day = 1, longitude = 143, latitude = -37)
