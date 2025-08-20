library(readr)
library(dplyr)
library(tidyr)
library(BayesianTools)
library(ggplot2)
library(patchwork)

source(here::here("R/calibration_helpers.R"))

timings <- readr::read_rds(here::here("timings_FB_2025-08-20_21h01.rds"))



# res_list <- lapply(here::here(timings$resultfile), readr::read_rds) # TODO: change for this
res_list <- lapply(timings$resultfile, readr::read_rds)


out_calib_s0 <- res_list[[which(timings$setup == 0)]]

# plot(out_calib_s0$mod)
plot_prior_posterior_density(out_calib_s0$mod)
