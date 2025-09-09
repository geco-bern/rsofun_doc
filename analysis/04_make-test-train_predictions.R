#!/usr/bin/env Rscript

# Script making train/test predictions after Bayesian calibration

# script is called with two arguments for continuing sampling:
# 1. filename of previous mcmcSamplerList.rds
# 2. "test"/"train"/"both" data set to predict
# 3. number of burnin parameter samples to discard
# 4. number of parameter samples to run simulations for
# 5. number of cores to use for running simulations

# Example:
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen0_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds",            "8000", "200", "8"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen1_DEzs-100000-0iter_8x3chains_on_CPU8x1.rds",           "8000", "200", "8"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen2_DEzs-100000-0iter_10x3chains_on_CPU10x1.rds",         "8000", "200", "8"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen3_DEzs-100000-0iter_8x3chains_on_CPU8x1.rds",           "8000", "200", "8"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen14_DEzs-35000-0iter_8x3chains_on_CPU8x1_continued.rds", "8000", "200", "8"))'

# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen32_DEzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds", "8000", "200", "8"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen36_DEzs-50000-0iter_8x3chains_on_CPU8x1.rds", "8000", "200", "8"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen56_DEzs-15000-0iter_8x3chains_on_CPU8x1.rds", "4000", "200", "8"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen4_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds",  "8000", "200", "8"))'


# # When using this script directly from RStudio, not from the shell, specify
# args <- c("out_calib__scen14_DEzs-35000-0iter_8x3chains_on_CPU8x1_continued.rds", "8000", "200", "8")
# args <- c("out_calib__scen14_DEzs-35000-0iter_8x3chains_on_CPU8x1_continued.rds", "8000", "200", "8")
# args <- c("out_calib__scen0_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds",            "8000", "200", "8")
# args <- c("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen74_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds",            "8000", "200", "8")
# args <- c("out_calib__scen74_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds", "8000", "200", "8")
# args <- c("out_calib__scen94_DEzs-40000-0iter_8x3chains_on_CPU8x1_continued.rds", "8000", "5", "8")
# to receive arguments to script from the shell
args = commandArgs(trailingOnly=TRUE)
stopifnot(length(args) == 4)
names(args) <- c("mcmc","burnin","nsimulations","ncores")

stopifnot(length(args[["mcmc"]])>=1)
stopifnot(length(args[["burnin"]])==1)
stopifnot(length(args[["nsimulations"]])==1)
stopifnot(length(args[["ncores"]])==1)

args <- as.list(args)
args[c("burnin","nsimulations","ncores")] <- as.integer(args[c("burnin","nsimulations","ncores")])

print(sprintf(
  "Requested predictions for %s (burnin %d) for test and train data sets for %d simulations (on %d core(s))",
  args[["mcmc"]],
  args[["burnin"]],
  args[["nsimulations"]],
  args[["ncores"]]
))

stopifnot(args[["target_dataset"]] %in% c("test","train","both"))
stopifnot(args[["nsimulations"]] > 0)

source(here::here("analysis/00_define_scenarios.R")) # to define 'rsofun_doc_output_path'
rds_input_path <- file.path(rsofun_doc_output_path, "data", "calibrations", args[["mcmc"]])

stopifnot(file.exists(rds_input_path))

outfile <- sprintf(
  "out_predict_N%d_XXX_%dburnin__%s.rds",
  args[["nsimulations"]],
  args[["burnin"]],
  args[["mcmc"]])

outpath <- file.path(rsofun_doc_output_path, "data", "predictions", outfile)
if (!dir.exists(dirname(outpath))) {dir.create(dirname(outpath),  recursive=TRUE)}

# sample posteriors and run model for each sample parameter set
library(multidplyr)
source(here::here("R/run_prediction_rsofun.R"))

print("Posterior runs: Test")
out_calib <- readr::read_rds(rds_input_path)
df_predict_test <- run_prediction_rsofun(
  mcmc_posterior = out_calib,
  prediction     = "test",
  burnin_to_skip = args[["burnin"]],
  n_samples      = args[["nsimulations"]],
  n_cores        = args[["ncores"]])
readr::write_rds(df_predict_test, gsub("XXX","test",outpath), compress = "none")

print("Posterior runs: Train")
df_predict_train <- run_prediction_rsofun(
  mcmc_posterior = out_calib,
  prediction     = "train",
  burnin_to_skip = args[["burnin"]],
  n_samples      = args[["nsimulations"]],
  n_cores        = args[["ncores"]])
readr::write_rds(df_predict_train, gsub("XXX","train",outpath), compress = "none")

# get MAP and run model for MAP parameter set
outpathMAP <- paste0(gsub("out_predict_N[0-9]*_", "out_predict_MAP_", outpath), "_MAP.rds")
print("MAP run: Test")
df_predict_MAP_test <- run_prediction_rsofun(
  mcmc_posterior = out_calib,
  prediction     = "test",
  burnin_to_skip = args[["burnin"]],
  n_samples      = 1, # n_samples == 1, requests MAP
  n_cores        = args[["ncores"]])
readr::write_rds(df_predict_MAP_test, gsub("XXX","test",outpathMAP), compress = "none")

print("MAP run: Train")
df_predict_MAP_train <- run_prediction_rsofun(
  mcmc_posterior = out_calib,
  prediction     = "train",
  burnin_to_skip = args[["burnin"]],
  n_samples      = 1, # n_samples == 1, requests MAP
  n_cores        = args[["ncores"]])
readr::write_rds(df_predict_MAP_train, gsub("XXX","train",outpathMAP), compress = "none")


# apply bias-correction and simulate structural error
if (TRUE){
  print("Apply bias and sample error")
  # combine test and train predictions
  # apply bias-correction and simulate structural error
  library(dtplyr)
  library(data.table)

  # combine
  dat_to_plot_inclNAObs <- bind_rows(
    df_predict_train     |> select(mcmc_id, sim) |> mutate(dataset = "train") |> unnest(sim), # |> filter(!is.na(obs)) |> select(-obs_metadata), # TODO: append later Scenario = "1"
    df_predict_test      |> select(mcmc_id, sim) |> mutate(dataset = "test" ) |> unnest(sim), # |> filter(!is.na(obs)) |> select(-obs_metadata), # TODO: append later Scenario = "1"
    df_predict_MAP_train |> select(mcmc_id, sim) |> mutate(dataset = "train") |> unnest(sim), # |> filter(!is.na(obs)) |> select(-obs_metadata), # TODO: append later Scenario = "1"
    df_predict_MAP_test  |> select(mcmc_id, sim) |> mutate(dataset = "test" ) |> unnest(sim) # |> filter(!is.na(obs)) |> select(-obs_metadata), # TODO: append later Scenario = "1"
  ) |>
    lazy_dt()   # use lazy data.table and dtplyr for speed

  # dat_to_plot <- dat_to_plot_inclNAObs |> filter(!is.na(obs)) # remove days where to make data.frame smaller (but cannot be used to plot gpp time series due to these gaps)

  # define operation:
  apply_bias_correction_and_sample_error <- function(df_pred, N_sample_error){
    require(data.table)
    require(dtplyr)
    df_pred |>
      lazy_dt() |>   # use lazy data.table and dtplyr for speed
      group_by(err_par_sd) |>
      # following line is basically a cross_join: with   # previous solution was: tibble(sample_id = 1:N_sample_error)
      mutate(Nrow=n()) |> slice(rep(1:.N, each = N_sample_error)) |> mutate(sample_id = rep(1:N_sample_error, times = unique(Nrow))) |> select(-Nrow) |>
      # sample the error
      mutate(err_sample                 = rnorm(n(), sd = err_par_sd),
              mod_biasremoved_no_err    = mod_no_err - err_par_bias,
              mod_biasremoved_with_err  = mod_no_err - err_par_bias + err_sample) |>
      ungroup() |>

      # keep output light: i.e. refrain from computing deviations and remove unneded columns:
        # compute deviations
        # mutate(predBiasedNoErr_minus_obs     = mod_no_err - obs,
        #         predBiasRemovedNoErr_minus_obs   = mod_biasremoved_no_err - obs,
        #         predBiasRemovedWithErr_minus_obs = mod_biasremoved_with_err - obs) |>
      select(-run_model, -err_par_bias, -err_par_sd, -err_sample, -sample_id) |> #, -predBiasedNoErr_minus_obs, -predBiasRemovedNoErr_minus_obs, -predBiasRemovedWithErr_minus_obs)
      mutate(sitename = as.factor(sitename), dataset = as.factor(dataset)) |>
      as_tibble() # to access results of lazy computation
  }

  N_sample_error = 3

  # apply operation:

  # Obs-NAs removed: NOTE: this is actually not much smaller. Do not waster disk space for twice an almost same size data.frame
  # dat_to_plot_sampled <- dat_to_plot |> apply_bias_correction_and_sample_error(N_sample_error = N_sample_error)
  # readr::write_rds(dat_to_plot_sampled,
  #                  paste0(gsub("_XXX",paste0("parsxN",N_sample_error,"errors"), outpath),"_sampled_onlyObs.rds"),
  #                  compress = "none")

  # Obs-NAs kept:
  dat_to_plot_inclNAObs_sampled <- dat_to_plot_inclNAObs |> apply_bias_correction_and_sample_error(N_sample_error = N_sample_error)
  # readr::write_rds(dat_to_plot_inclNAObs_sampled,
  #                  paste0(gsub("_XXX",paste0("parsxN",N_sample_error,"errors"), outpath),"_sampled_continuousWithObsNA.rds"),
  #                  compress = "none")

  print("Save results")
  readr::write_rds(dat_to_plot_inclNAObs_sampled |> filter(dataset == "train"),
                   paste0(gsub("_XXX",paste0("parsxN",N_sample_error,"errors_train"), outpath),"_sampled_continuousWithObsNA.rds"),
                   compress = "none")
  readr::write_rds(dat_to_plot_inclNAObs_sampled |> filter(dataset == "test"),
                   paste0(gsub("_XXX",paste0("parsxN",N_sample_error,"errors_test"), outpath),"_sampled_continuousWithObsNA.rds"),
                   compress = "none")
}
