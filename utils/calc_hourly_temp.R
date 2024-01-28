calc_hourly_temp <- function(temp_min, temp_max, hour, time_max = 14) {
    # Convert hour to a value between 0 (for 12am) and 1 (for 11:59pm)
    t <- hour / 24

    # If before time of maximum temperature
    if (hour <= time_max) {
        return(temp_min + (temp_max - temp_min) * (1 - cos(pi * t / (time_max / 24))) / 2)
    }
    # If after time of maximum temperature
    else {
        # Flip the function to make it decreasing after t_max
        return(temp_max + (temp_min - temp_max) * (1 - cos(pi * (t - time_max / 24) / (1 - time_max / 24))) / 2)
    }
}


# Test
temp_min <- 10
temp_max <- 25
hour <- 20 # 8pm
calc_hourly_temp(temp_min, temp_max, hour)
