make_parameter_string <- function() {
    source("./config/parameters.R")
    # get parameters and save into string for plot title
    parameters <- list(
        height = HEIGHT,
        n_steps = N_STEPS,
        temp_thresh = TEMP_THRESH
    )
    param_string <- paste0(names(parameters), "", unlist(lapply(parameters, paste)), collapse = "_")
    return(param_string)
}
