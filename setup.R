library(tidyverse)
library(lubridate)
library(readxl)
library(sf)
library(ozmaps)

source("./config/parameters.R")
# create met and out dir for simulations
dir.create("met", showWarnings = FALSE)
dir.create("out", showWarnings = FALSE)
dir.create("sims", showWarnings = FALSE)
dir.create("cache", showWarnings = FALSE)

dir.create("results/plots/sig_catch", recursive = TRUE, showWarnings = FALSE)
dir.create("results/plots/origin", recursive = TRUE, showWarnings = FALSE)
dir.create("results/plots/temperature", recursive = TRUE, showWarnings = FALSE)
dir.create("results/plots/trajectories", recursive = TRUE, showWarnings = FALSE)
