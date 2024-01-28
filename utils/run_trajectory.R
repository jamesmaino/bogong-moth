library(tidyverse)
library(splitr)
library(sf)
library(ozmaps)

source("./utils/aedt_to_utc.R")
source("./utils/utc_to_aedt.R")
source("./utils/K_to_C.R")

aus <- ozmap_data() %>%
    st_transform(4326)

run_trajectory <- function(
    lat, lon, height, days_AEDT, daily_hours_AEDT, duration, n_steps, max_n_steps, temp_thresh, ignore_cache = FALSE) {
    # Extract parameters into a named list to append to sim output
    parameters <- list(
        lat = lat, lon = lon, height = height,
        days_AEDT = date(days_AEDT), daily_hours_AEDT = daily_hours_AEDT,
        duration = duration, max_n_steps = max_n_steps, temp_thresh = temp_thresh
    )
    parameters <- lapply(parameters, paste)
    params_tibble <- tibble(par_name = names(parameters), par_value = paste(unlist(parameters)))
    param_string <- paste0(names(parameters), "", unlist(lapply(parameters, paste)), collapse = "_")
    sim_filename <- paste0(
        "./cache/trajectory_sim",
        param_string,
        ".csv"
    )
    if (ignore_cache && file.exists(sim_filename)) file.remove(sim_filename)

    if (file.exists(sim_filename)) {
        sim <- read_csv(sim_filename, show_col_types = FALSE, progress = FALSE) %>%
            filter(step <= n_steps)
        return(sim)
    }

    cat(paste("running", param_string), "\n")

    tryCatch(
        {
            date <- aedt_to_utc(days_AEDT, daily_hours_AEDT)
            days <- date(date)
            daily_hours <- hour(date)
            trajectory <- list()
            step <- 1
            for (j in 1:max_n_steps) {
                # 1 Run HYSPLIT backwards at 1000 m.  Is the endpoint above land?  Yes (3), No (2)
                trajectory1 <-
                    splitr::hysplit_trajectory(
                        lat = lat,
                        lon = lon,
                        days = days,
                        duration = duration,
                        daily_hours = daily_hours,
                        height = height,
                        direction = "backward",
                        met_type = "reanalysis",
                        extended_met = TRUE,
                        met_dir = here::here("met"),
                        exec_dir = here::here("out")
                    )
                # 2 Move to the point where the trajectory crosses the coast, note the location and time. Go to (3)
                trajectory2 <- st_as_sf(trajectory1, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
                st_agr(aus) <- "constant"
                st_agr(trajectory2) <- "constant"
                trajectory2 <- st_intersection(aus, trajectory2)
                if (nrow(trajectory2) == 0) break
                trajectory3 <- trajectory2[nrow(trajectory2), ]

                # 3 Is the 1000 m temp above 10C? Yes (5) No (4)
                # if(trajectory2$air_temp - 273.15 < temp_thresh) # we may as well run 4

                # 3.5 Is the 1000 m temp at origin above minimum temp thresh? Yes (4) No (6) (as per M Smith suggestion)
                if (K_to_C(trajectory3$air_temp) < temp_thresh) {
                    trajectory6 <- st_set_geometry(trajectory2, NULL)[1, ]
                    trajectory6$hour_along <- NA
                    trajectory6$step <- step
                    trajectory[[j]] <- trajectory6
                    break
                }

                # 4 Run HYSPLIT at 0 m from either the original or revised (coastal) starting point (run backwards or forwards, doesn’t matter, we just want the starting surface temp).  Is that temp above 10C? Yes (5), No (6)
                trajectory4 <-
                    splitr::hysplit_trajectory(
                        lat = trajectory3$lat,
                        lon = trajectory3$lon,
                        height = 0,
                        duration = 0,
                        days = as.Date(trajectory3$traj_dt),
                        daily_hours = as.numeric(format(trajectory3$traj_dt, "%H")),
                        direction = "backward",
                        met_type = "reanalysis",
                        extended_met = TRUE,
                        met_dir = here::here("met"),
                        exec_dir = here::here("out")
                    )
                if (K_to_C(trajectory4$air_temp) < temp_thresh) break

                # 5 Accept the trajectory
                trajectory[[j]] <- st_set_geometry(trajectory2, NULL) %>%
                    mutate(step = step)
                # update starting conditions for next step
                cat(".")
                days <- days - lubridate::days(1)
                lat <- trajectory3$lat
                lon <- trajectory3$lon
                step <- step + 1
                # 6 Reject the trajectory
            }

            sims <- bind_rows(trajectory) %>%
                mutate(traj_dt = with_tz(traj_dt, "Australia/Sydney")) %>%
                filter(step <= n_steps)


            # save parameters as padded columns
            pad_size <- max(
                nrow(sims) - nrow(params_tibble),
                nrow(params_tibble) - nrow(sims)
            )
            padding <- tibble(par_name = rep(NA, pad_size), par_value = rep(NA, pad_size))
            padded_params_tibble <- bind_rows(params_tibble, padding)

            # cache sim
            sims %>%
                bind_cols(padded_params_tibble) %>%
                write_csv(sim_filename)
            return(sims)
        },
        error = function(err) {
            # Handle errors
            message_content <- conditionMessage(err)
            suspect_file <- aedt_to_utc(days_AEDT, daily_hours_AEDT) - lubridate::days(step - 1) - hours(duration)
            cat(paste("Caught an error:", "met data may be corrupted for ", date(suspect_file), "\n", message_content, "\n"))
            # print(paste("Caught an error:", err, date))
            return(NA) # Return a default value or do something else
        }
    )
}
