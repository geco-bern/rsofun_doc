#!/usr/bin/env Rscript

# Script running Bayesian calibration

# script is called with two arguments for continuing sampling:
# 1. filename of previous mcmcSamplerList.rds
# 2. number of iterations to add

# Example:
# Rscript -e 'renv::run("analysis/03_continue_mcmc_rsofun.R", project = "../rsofun_doc", args = c("out_calib__scen4_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds", "10000"))'
# Rscript -e 'renv::run("analysis/03_continue_mcmc_rsofun.R", project = "../rsofun_doc", args = c("out_calib__scen14_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds", "10000"))'
# Rscript -e 'renv::run("analysis/03_continue_mcmc_rsofun.R", project = "../rsofun_doc", args = c("out_calib__scen14_DEzs-35000-0iter_8x3chains_on_CPU8x1_continued.rds", "15000"))'

# # When using this script directly from RStudio, not from the shell, specify
# args <- c("out_calib__scen14_DEzs-10000-0iter_8x3chains_on_CPU8x1.rds", "29")
# args <- c("out_calib__scen94_DREAMzs-20000-0iter_8x3chains_on_CPU8x1.rds", "29")

# to receive arguments to script from the shell
args = commandArgs(trailingOnly=TRUE)

source(here::here("analysis/00_define_setups.R")) # to define 'rsofun_doc_output_path'
# rds_input_path <- "rsofun_doc_output_path/data/calibrations/out_calib__scen14_DEzs-10000-0iter_8x3chains_on_CPU8x1.rds"
rds_input_path <- file.path(rsofun_doc_output_path, "data", "calibrations", args[[1]])
iterations_to_add <- as.integer(args[[2]])

print(sprintf("Requested to add %d samples to mcmc chain: %s", iterations_to_add, args[[1]]))
stopifnot(file.exists(rds_input_path))

source(here::here("R/calibration_helpers.R"))

library(BayesianTools)
library(foreach)
library(doParallel)
library(tidyr)
library(dplyr)
library(readr)
library(rsofun)
library(stringr)

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

continue_mcmc_rsofun <- function(
    rds_input_path,
    iterations_to_add
  ){
  burnin_to_add <-  0     # no burnin when continued

  # read previous mcmc chain(s):
  out_calib_to_continue <- readr::read_rds(rds_input_path)

  # parse input data from the previous mcmc chain(s):

  # parse the suffix_str:
  # sprintf("_scen%d_%s-%d-%diter_%dx%dchains_on_CPU%dx%d",
  #         scenario, sampler, iterations, burnin, n_chains, n_chains_inner, cores, cores_inner)
  pattern <- paste0("_scen(?<scenario>\\d+)_(?<sampler>\\w+)-(?<iterations>\\d+)-(?<burnin>\\d+)",
                    "iter_(?<nchains>\\d+)x(?<nchainsinner>\\d+)chains_on_CPU(?<cores>\\d+)x(?<coresinner>\\d+)",
                    "(?<continued>_continued)?")
  result <- str_match(out_calib_to_continue$name, pattern)

  curr_calibration_scenario<-as.integer(result[[1,"scenario"]])
  iterations_prev         <- as.integer(result[[1,"iterations"]]) # previous iterations
  burnin_prev             <- as.integer(result[[1,"burnin"]]) # previous burnin
  n_chains_independent    <- as.integer(result[[1,"nchains"]])
  n_chains_within_sampler <- as.integer(result[[1,"nchainsinner"]])
  n_parallel_independent  <- as.integer(result[[1,"cores"]])
  n_parallel_within_sampler <- FALSE; stopifnot(as.integer(result[[1,"coresinner"]])==1)

  stopifnot(out_calib_to_continue$mod[[1]]$settings$sampler %in% c("DEzs","DREAMzs")) # TODO: setup for DREAMzs
  stopifnot(result[[1,"sampler"]] %in% c("DEzs","DREAMzs"))                           # TODO: setup for DREAMzs

  # prepare new output:
  iterations_out <- iterations_prev + iterations_to_add
  suffix <- sprintf("_scen%d_%s-%d-%diter_%dx%dchains_on_CPU%dx%d_continued",
                      curr_calibration_scenario, result[[1,"sampler"]], iterations_out, burnin_prev, n_chains_independent, n_chains_within_sampler, n_parallel_independent, as.integer(result[[1,"coresinner"]]))

  outpath = dirname(rds_input_path)
  fname <- sprintf("timings_scen%d_%s", curr_calibration_scenario, format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"))
  logpath = file.path(outpath, "..", "timings", paste0(fname,"_log.txt"))

  ## Continue the MCMC sampler: ----
  require(BayesianTools)
  require(rsofun)
  mcmc_in <- out_calib_to_continue$mod # This is the mcmcSamplerList
  stopifnot(is(mcmc_in, "mcmcSamplerList"))

  print(paste0(Sys.time(),": continue sampling of ", basename(rds_input_path), " into ", suffix))
  start_time <- Sys.time()

  if (n_parallel_independent > 1){ # parallel MCMC sampler:

    if (logpath != "") {
      cl <- makeCluster(n_parallel_independent, outfile = logpath)
    } else {
      cl <- makeCluster(n_parallel_independent)
    }
    registerDoParallel(cl)

    indep_chains <- foreach(
      i = 1:n_parallel_independent,
      .packages=c('BayesianTools','rsofun','dplyr','tidyr','ReIns'),
      #.export = c('get_mod_obs_pmodel_bigD13C_vj_gpp'),
      .verbose = TRUE
    ) %dopar% {

      set.seed(1982 + i) # set a different seed on each worker
      BayesianTools::runMCMC(mcmc_in[[i]], settings = list(burnin=0, iterations=iterations_to_add))
    }
    stopCluster(cl)
    mcmc_out <- createMcmcSamplerList(indep_chains) # combine the independent chains

  } else { # sequential MCMC sampler:
    mcmc_out <- runMCMC(mcmc_in, settings = list(burnin=0, iterations=iterations_to_add))
  }

  ## Postprocess: ----
  # ensure return value 'mcmc_out' is a mcmcSamplerList even if n_chains_independent==1
  # (by default runMCMC returns only a mcmcSampler if n_chains_independent==1)
  if(is(mcmc_out, "mcmcSampler")){
    mcmc_out <- createMcmcSamplerList(list(mcmc_out)) # now mcmc_out is a mcmcSamplerList
  }
  end_time <- Sys.time()

  ## Build return object: 'return_value' ----
  # Extract MAP (maximum a posteriori value) of parameters
  bt_par <- BayesianTools::MAP(mcmc_out)$parametersMAP
  return_value <- list(
    par = bt_par,
    mod = mcmc_out,
    name = suffix)

  # append timing information
  additional_walltime <- (end_time - start_time)
  return_value$walltime <- out_calib_to_continue$walltime + additional_walltime
  return_value$runtime  <- get_runtime_numeric(return_value) # this automatically adds the previous runtime
  print(get_runtime_numeric(return_value))
  print(get_walltime(return_value))

  ## Store results to file: ----
  return_value$fpath <- file.path(outpath, paste0("out_calib_", suffix, ".rds"))
  write_rds(return_value, file = return_value$fpath, compress = "xz")

  print(paste0(Sys.time(),": end sampling continuation of ", basename(rds_input_path), " into ", suffix,
               ". Written *.rds-output to: ", return_value$fpath))

  ## Build return object: 'timings_rerun' ----
  timings_rerun <- tibble(
    #
    scenario       = curr_calibration_scenario,
    # sampling options:
    sampler        = result[[1,"sampler"]],
    burnin         = burnin_prev,
    iterations     = iterations_out,
    n_chains       = n_chains_independent,
    n_chains_inner = n_chains_within_sampler,
    # performance results:
    cores          = n_parallel_independent,
    cores_inner    = n_parallel_within_sampler,
    runtime        = NaN,
    walltime       = NaN,
    resultfile     = NaN
  )
  # append performance results to return object
  timings_rerun$runtime    <- return_value$runtime
  timings_rerun$walltime   <- return_value$walltime
  timings_rerun$resultfile <- return_value$fpath

  # save timings into csv and rds files:
  timings_to_rds_csv(timings_rerun, file.path(outpath, "..", "timings", fname))
}

# run the mcmc
continue_mcmc_rsofun(rds_input_path, iterations_to_add = iterations_to_add)
