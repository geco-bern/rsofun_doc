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
# args <- c("3", "11", "51", "3")
# args <- c("0", "11", "51", "3")

# to receive arguments to script from the shell
args = commandArgs(trailingOnly=TRUE)
stopifnot(length(args) %in% c(3,4))
if(length(args)==3) {args <- c(args, "1")}
args <- as.integer(args)
names(args) <- c("scenario","burnin","iterations","parallel")

stopifnot(length(args[["scenario"]])>=1)
stopifnot(length(args[["burnin"]])==1)
stopifnot(length(args[["iterations"]])==1)
stopifnot(length(args[["parallel"]])==1)

print(sprintf(
  "Requested scenario #%d, for (%d-%d) iterations (on %d core(s))",
  args[["scenario"]],
  args[["iterations"]],
  args[["burnin"]],
  args[["parallel"]]
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
# outpath <- here::here("data")
outpath <- file.path("/data_2/scratch/fbernhard/rsofun_doc_outputs","data")
dir.create(dirname(outpath), showWarnings = FALSE, recursive = TRUE)

timings_to_rds_csv <- function(timings, filename = here::here("data","timings","timings_FB")){
  # to *.rds
  timings |> readr::write_rds(paste0(filename,".rds"))

  # to *.csv
  timings |>
    mutate(across(
      where(lubridate::is.difftime),
      ~sprintf("%8.1fmin", round(as.numeric(.x, "mins"),1))
    )) |>
    readr::write_csv(paste0(filename,".csv"))
}


# run MCMC
timings <- tibble(); timings_to_rds_csv(timings, file.path(outpath, "timings", fname))

## Scenario 3:
if(3 %in% args["scenario"]){
  curr_timings <- run_mcmc_rsofun(
    # calibration scenario:
    curr_calibration_scenario    = 3,
    # mcmc setup:
    iterations = args[["iterations"]], burnin = args[["burnin"]],
    n_chains_independent      = args[["parallel"]],
    n_chains_within_sampler   = 3,
    # parallelization
    n_parallel_independent    = args[["parallel"]],     # with '= 1' the 3 chains are run in sequence (set to 3 if you want in parallel)
    n_parallel_within_sampler = FALSE,
    outpath = outpath, logpath = file.path(outpath, "timings", paste0(fname,"_log.txt"))
  )
  timings <- bind_rows(timings, curr_timings); timings_to_rds_csv(timings, file.path(outpath, "timings", fname))
}
## Scenario 2:
if(2 %in% args["scenario"]){
  curr_timings <- run_mcmc_rsofun(
    # calibration scenario:
    curr_calibration_scenario    = 2,
    # mcmc setup:
    iterations = args[["iterations"]], burnin = args[["burnin"]],
    n_chains_independent      = args[["parallel"]],
    n_chains_within_sampler   = 3,
    # parallelization
    n_parallel_independent    = args[["parallel"]],     # with '= 1' the 3 chains are run in sequence (set to 3 if you want in parallel)
    n_parallel_within_sampler = FALSE,
    outpath = outpath, logpath = file.path(outpath, "timings", paste0(fname,"_log.txt"))
  )
  timings <- bind_rows(timings, curr_timings); timings_to_rds_csv(timings, file.path(outpath, "timings", fname))
}
## Scenario 1:
if(1 %in% args["scenario"]){
  curr_timings <- run_mcmc_rsofun(
    # calibration scenario:
    curr_calibration_scenario    = 1,
    # mcmc setup:
    iterations = args[["iterations"]], burnin = args[["burnin"]],
    n_chains_independent      = args[["parallel"]],
    n_chains_within_sampler   = 3,
    # parallelization
    n_parallel_independent    = args[["parallel"]],     # with '= 1' the 3 chains are run in sequence (set to 3 if you want in parallel)
    n_parallel_within_sampler = FALSE,
    outpath = outpath, logpath = file.path(outpath, "timings", paste0(fname,"_log.txt"))
  )
  timings <- bind_rows(timings, curr_timings); timings_to_rds_csv(timings, file.path(outpath, "timings", fname))
}
## Scenario 0: ( is only FR-Pue like in initial submission)
if(0 %in% args["scenario"]){
  curr_timings <- run_mcmc_rsofun(
    # calibration scenario:
    curr_calibration_scenario    = 0,
    # mcmc setup:
    iterations = args[["iterations"]], burnin = args[["burnin"]],
    n_chains_independent      = args[["parallel"]],
    n_chains_within_sampler   = 3,
    # parallelization
    n_parallel_independent    = args[["parallel"]],     # with '= 1' the 3 chains are run in sequence (set to 3 if you want in parallel)
    n_parallel_within_sampler = FALSE,
    outpath = outpath, logpath = file.path(outpath, "timings", paste0(fname,"_log.txt"))
  )
  timings <- bind_rows(timings, curr_timings); timings_to_rds_csv(timings, file.path(outpath, "timings", fname))
}
