# identifying sig catch
DAILY_COUNT_THRESH <- 5 # daily catches must be higher than this
SEASONAL_COUNT_THRESH <- 2 # daily catches must be higher than the seasonal mean multiplied by this factor

# simulation options
HEIGHT <- 1000 # the starting height in m
END_TIME <- 6 # end time of backward simulation in AEDT
DURATION <- 12 # assume flight start 12 hours prior (6pm previous day)
N_STEPS <- 3 # number of steps to run
MAX_N_STEPS <- 3 # max number of steps to run (DON'T CHANGE THIS, AS IT WILL RERUN SIMULATIONS RATHER THAN SUBSET
TEMP_THRESH <- 12 # minimum temperature threshold for migration

if (N_STEPS > MAX_N_STEPS) stop("N_STEPS has exceeded MAX_N_STEPS")
options(timeout = 30 * 60) # allow more time to download climatic files
