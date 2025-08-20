library(BayesianTools)
library(foreach)
library(doParallel)
library(tidyr)
library(dplyr)

source(here::here("R/test_mcmc_parallelization.R"), echo = TRUE)

# test_mcmc_parallelization_dummyExample(
#   n_parallel_independent    = 1,
#   n_parallel_within_sampler = FALSE,
#   n_chains_independent      = 1,
#   n_chains_within_sampler   = 3,
#   burnin = 0, iterations = 100000
# )
# # M4:    7.7 seconds
# # WS02: 29.7 seconds
# # UBLX: 22.7 seconds
#
# test_mcmc_parallelization_dummyExample(
#   n_parallel_independent    = 1,
#   n_parallel_within_sampler = FALSE,
#   n_chains_independent      = 3,
#   n_chains_within_sampler   = 3,
#   burnin = 0, iterations = 100000
# )
# # M4:   23 seconds
# # WS02: 75 seconds
# # UBLX: 55 seconds
#
# test_mcmc_parallelization_dummyExample(
#   n_parallel_independent    = 3,     # now the 3 chains are run in parallel
#   n_parallel_within_sampler = FALSE,
#   n_chains_independent      = 3,
#   n_chains_within_sampler   = 3,
#   burnin = 0, iterations = 100000
# )
# # M4:    8.6 seconds
# # WS02: 40.4 seconds
# # UBLX: 31.3 seconds
#
# test_mcmc_parallelization(
#   n_parallel_independent    = 3,     # now the 3 chains are run in parallel
#   n_parallel_within_sampler = 3,     # now also the interal chains are parallel (might have too much communication overhead)
#   n_chains_independent      = 3,
#   n_chains_within_sampler   = 3,
#   burnin = 0, iterations = 100000
# )
# # M4:    1.7 minutes
# # WS02: XX.X minutes
# # UBLX: 26.7 minutes
#

# For massively parallel runs suggestion is to use SMC (sequential monte carlo) and not MCMC.

# Bottom line seems to be: #####################################################
# Keep 3 dependent chains on same CPU
# But parallelize independent chains

# Thus using 3 cores per calibration.

# Potentially using 9 cores when running all three scenarios simultaneously.
################################################################################

# for testing
timings_it <- tibble()
for(iter in c(5,15,50,100,200,400)){
  timings_it <- test_mcmc_parallelization_rsofun(
    iterations = iter, burnin = floor(iter/4),
    n_chains_independent      = 1,
    n_chains_within_sampler   = 3,
    # parallelization
    n_parallel_independent    = 1,
    n_parallel_within_sampler = FALSE
  ) |> mutate(config = "single-chain") |> bind_rows(timings_it)
  # M4:   XX.X seconds
  # WS02: XX.X seconds
  # UBLX: XX.X seconds

  timings_it <- test_mcmc_parallelization_rsofun(
    iterations = iter, burnin = floor(iter/4),
    n_chains_independent      = 3,
    n_chains_within_sampler   = 3,
    # parallelization
    n_parallel_independent    = 1,
    n_parallel_within_sampler = FALSE
  ) |> mutate(config = "3x-chain") |> bind_rows(timings_it)
  # M4:  XX.X seconds
  # WS02: XX.X seconds
  # UBLX: XX.X seconds

  timings_it <- test_mcmc_parallelization_rsofun(
    iterations = iter, burnin = floor(iter/4),
    n_chains_independent      = 3,
    n_chains_within_sampler   = 3,
    # parallelization
    n_parallel_independent    = 3,     # now the 3 chains are run in parallel
    n_parallel_within_sampler = FALSE
  ) |> mutate(config = "3x-chain-parallel") |> bind_rows(timings_it)
  # M4:   XX.X seconds
  # WS02: XX.X seconds
  # UBLX: XX.X seconds

  timings_it <- test_mcmc_parallelization_rsofun(
    iterations = iter, burnin = floor(iter/4),
    n_chains_independent      = 9,
    n_chains_within_sampler   = 3,
    # parallelization
    n_parallel_independent    = 9,     # now the 9 chains are run in parallel
    n_parallel_within_sampler = FALSE
  ) |> mutate(config = "9x-chain-parallel") |> bind_rows(timings_it)
  # M4:   XX.X seconds
  # WS02: XX.X seconds
  # UBLX: XX.X seconds


  # store
  hostname <- Sys.info()['nodename']
  timings_it <- timings_it |> mutate(machine = hostname)
  write_rds(timings_it, paste0("timings_",hostname,".rds"))
  write_csv(timings_it, paste0("timings_",hostname,".csv"))
}



iter <- 3750 # might this take 2h? If 50 iterations take 100seconds x 75 = 7500seconds
timings_it <- test_mcmc_parallelization_rsofun(
  iterations = iter, burnin = floor(iter/4),
  n_chains_independent      = 18,
  n_chains_within_sampler   = 3,
  # parallelization
  n_parallel_independent    = 18,     # now the 18 chains are run in parallel
  n_parallel_within_sampler = FALSE
) |> mutate(config = "9x-chain-parallel") |> bind_rows(timings_it)
# M4:   XX.X seconds
# WS02: XX.X seconds
# UBLX: XX.X seconds

hostname <- Sys.info()['nodename']
timings_it <- timings_it |> mutate(machine = hostname)
write_rds(timings_it, paste0("timings_",hostname,".rds"))
write_csv(timings_it, paste0("timings_",hostname,".csv"))


