#!/usr/bin/env Rscript

# Script running Bayesian calibration

# script is called with three arguments for sampling:
# 1. calibration scenario [0,1,2,3]
# 2. burnin iterations
# 3. total iterations (incl. burnin)

# Note that these arguments can be used to distribute over multiple nodes.
# Distribution over CPU cores of a single node is handled by multidplyr
# and argument ncores in the script.

# Example:
# Rscript -e 'renv::run("analysis/00_bayesian_calibration.R", project = "../rsofun_doc", args = c(3,11,51))'

# # When using this script directly from RStudio, not from the shell, specify
# args <- c("3", "11", "51")

# to receive arguments to script from the shell
args = commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==3)
args <- as.integer(args)
names(args) <- c("scenario","burnin","iterations")

stopifnot(length(args[["scenario"]])>=1)
stopifnot(length(args[["burnin"]])==1)
stopifnot(length(args[["iterations"]])==1)

print(sprintf(
  "Requested scenario #%d, for (%d-%d) iterations",
  args[["scenario"]],
  args[["iterations"]],
  args[["burnin"]]
))

# pak::pkg_install("geco-bern/rsofun@ebb6b208e72f83d7cb13c5802239b122f6853a52")

# Load libraries
renv::restore(prompt = FALSE)
library(BayesianTools)
library(foreach)
library(doParallel)
library(tidyr)
library(dplyr)
library(readr)
library(rsofun)

source(here::here("R/run_mcmc_rsofun.R"), echo = TRUE)

# setup output function
fname <- sprintf("timings_scen%s_%s",
                 paste0(args, collapse="-"),
                 format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"))
path <- here::here("data", "timings", fname)
dir.create(here::here("data", "timings"), showWarnings = FALSE)

timings_to_rds_csv <- function(timings, path = "timings_FB"){
  # to *.rds
  timings |> readr::write_rds(paste0(path,".rds"))

  # to *.csv
  timings |>
    mutate(across(
      where(lubridate::is.difftime),
      ~sprintf("%8.1fmin", round(as.numeric(.x, "mins"),1))
    )) |>
    readr::write_csv(paste0(path,".csv"))
}


# run MCMC
timings <- tibble(); timings_to_rds_csv(timings, path)

## Scenario 3:
if(3 %in% args["scenario"]){
  curr_timings <- run_mcmc_rsofun(
    # calibration scenario:
    curr_calibration_scenario    = 3,
    # mcmc setup:
    iterations = args[["iterations"]], burnin = args[["burnin"]],
    n_chains_independent      = 3,
    n_chains_within_sampler   = 3,
    # parallelization
    n_parallel_independent    = 1,     # now the 3 chains are run in sequence (set to 3 if you want in parallel)
    n_parallel_within_sampler = FALSE
  )
  timings <- bind_rows(timings, curr_timings); timings_to_rds_csv(timings, path)
}
## Scenario 2:
if(2 %in% args["scenario"]){
  curr_timings <- run_mcmc_rsofun(
    # calibration scenario:
    curr_calibration_scenario    = 2,
    # mcmc setup:
    iterations = args[["iterations"]], burnin = args[["burnin"]],
    n_chains_independent      = 3,
    n_chains_within_sampler   = 3,
    # parallelization
    n_parallel_independent    = 1,     # now the 3 chains are run in sequence (set to 3 if you want in parallel)
    n_parallel_within_sampler = FALSE
  )
  timings <- bind_rows(timings, curr_timings); timings_to_rds_csv(timings, path)
}
## Scenario 1:
if(1 %in% args["scenario"]){
  curr_timings <- run_mcmc_rsofun(
    # calibration scenario:
    curr_calibration_scenario    = 1,
    # mcmc setup:
    iterations = args[["iterations"]], burnin = args[["burnin"]],
    n_chains_independent      = 3,
    n_chains_within_sampler   = 3,
    # parallelization
    n_parallel_independent    = 1,     # now the 3 chains are run in sequence (set to 3 if you want in parallel)
    n_parallel_within_sampler = FALSE
  )
  timings <- bind_rows(timings, curr_timings); timings_to_rds_csv(timings, path)
}
## Scenario 0: ( is only FR-Pue like in initial submission)
if(0 %in% args["scenario"]){
  curr_timings <- run_mcmc_rsofun(
    # calibration scenario:
    curr_calibration_scenario    = 0,
    # mcmc setup:
    iterations = args[["iterations"]], burnin = args[["burnin"]],
    n_chains_independent      = 3,
    n_chains_within_sampler   = 3,
    # parallelization
    n_parallel_independent    = 1,     # now the 3 chains are run in sequence (set to 3 if you want in parallel)
    n_parallel_within_sampler = FALSE
  )
  timings <- bind_rows(timings, curr_timings); timings_to_rds_csv(timings, path)
}
