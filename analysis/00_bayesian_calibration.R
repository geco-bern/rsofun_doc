# Script running Bayesian calibration

# Load libraries
# renv::restore()
library(BayesianTools)
library(foreach)
library(doParallel)
library(tidyr)
library(dplyr)
library(readr)

source(here::here("R/test_mcmc_parallelization.R"), echo = TRUE)

# setup output function
timings_to_rds_csv <- function(timings, path = "timings_FB"){
  # to *.rds
  timings |> readr::write_rds(paste0(path,".rds"))

  # to *.csv
  timings |>
    mutate(across(
      where(is.difftime),
      ~sprintf("%8.1fmin", round(as.numeric(.x, "mins"),1))
    )) |>
    readr::write_csv(paste0(path,".csv"))
}

# run MCMC

burnin     <- 100#00
iterations <- 500#00
timings <- tibble()

## Scenario 3:
curr_timings <- test_mcmc_parallelization_rsofun(
  # calibration scenario:
  curr_calibration_setup    = 3,
  # mcmc setup:
  iterations = iterations, burnin = burnin,
  n_chains_independent      = 3,
  n_chains_within_sampler   = 3,
  # parallelization
  n_parallel_independent    = 3,     # now the 3 chains are run in parallel
  n_parallel_within_sampler = FALSE
)
timings <- bind_rows(timings, curr_timings); timings_to_rds_csv(timings)

## Scenario 2:
curr_timings <- test_mcmc_parallelization_rsofun(
  # calibration scenario:
  curr_calibration_setup    = 2,
  # mcmc setup:
  iterations = iterations, burnin = burnin,
  n_chains_independent      = 3,
  n_chains_within_sampler   = 3,
  # parallelization
  n_parallel_independent    = 3,     # now the 3 chains are run in parallel
  n_parallel_within_sampler = FALSE
)
timings <- bind_rows(timings, curr_timings); timings_to_rds_csv(timings)

## Scenario 1:
curr_timings <- test_mcmc_parallelization_rsofun(
  # calibration scenario:
  curr_calibration_setup    = 1,
  # mcmc setup:
  iterations = iterations, burnin = burnin,
  n_chains_independent      = 3,
  n_chains_within_sampler   = 3,
  # parallelization
  n_parallel_independent    = 3,     # now the 3 chains are run in parallel
  n_parallel_within_sampler = FALSE
)
timings <- bind_rows(timings, curr_timings)




## Scenario 0: ( is only FR-Pue like in initial submission)
curr_timings <- test_mcmc_parallelization_rsofun(
  # calibration scenario:
  curr_calibration_setup    = 0,
  # mcmc setup:
  iterations = iterations, burnin = burnin,
  n_chains_independent      = 3,
  n_chains_within_sampler   = 3,
  # parallelization
  n_parallel_independent    = 3,     # now the 3 chains are run in parallel
  n_parallel_within_sampler = FALSE
)
timings <- bind_rows(timings, curr_timings)
