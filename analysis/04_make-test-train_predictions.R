#!/usr/bin/env Rscript

# Script making train/test predictions after Bayesian calibration

# script is called with two arguments for continuing sampling:
# 1. filename of previous mcmcSamplerList.rds
# 2. "test"/"train"/"both" data set to predict
# 3. number of burnin parameter samples to discard
# 4. number of parameter samples to run simulations for
# 5. number of cores to use for running simulations

# Example:
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen0_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds",            "8000", "200", "8", "3"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen1_DEzs-100000-0iter_8x3chains_on_CPU8x1.rds",           "8000", "200", "8", "3"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen2_DEzs-100000-0iter_10x3chains_on_CPU10x1.rds",         "8000", "200", "8", "3"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen3_DEzs-100000-0iter_8x3chains_on_CPU8x1.rds",           "8000", "200", "8", "3"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen14_DEzs-35000-0iter_8x3chains_on_CPU8x1_continued.rds", "8000", "200", "8", "3"))'

# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen32_DEzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds", "8000", "200", "8", "3"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen36_DEzs-50000-0iter_8x3chains_on_CPU8x1.rds", "8000", "200", "8", "3"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen56_DEzs-15000-0iter_8x3chains_on_CPU8x1.rds", "4000", "200", "8", "3"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen4_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds",  "8000", "200", "8", "3"))'


# # When using this script directly from RStudio, not from the shell, specify
# args <- c("out_calib__scen14_DEzs-35000-0iter_8x3chains_on_CPU8x1_continued.rds", "8000", "200", "8", "3")
# args <- c("out_calib__scen14_DEzs-35000-0iter_8x3chains_on_CPU8x1_continued.rds", "8000", "200", "8", "3")
# args <- c("out_calib__scen0_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds",            "8000", "200", "8", "3")
# args <- c("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen74_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds",            "8000", "200", "8", "3")
# args <- c("out_calib__scen74_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds", "8000", "200", "8", "3")
# args <- c("out_calib__scen94_DEzs-40000-0iter_8x3chains_on_CPU8x1_continued.rds", "8000", "5", "8", "3")
# to receive arguments to script from the shell
args = commandArgs(trailingOnly=TRUE)
stopifnot(length(args) == 5)
names(args) <- c("mcmc","burnin","nsimulations","ncores", "nerrorsamples")

stopifnot(length(args[["mcmc"]])>=1)
stopifnot(length(args[["burnin"]])==1)
stopifnot(length(args[["nsimulations"]])==1)
stopifnot(length(args[["ncores"]])==1)
stopifnot(length(args[["nerrorsamples"]])==1)

args <- as.list(args)
args[c("burnin","nsimulations","ncores","nerrorsamples")] <- as.integer(args[c("burnin","nsimulations","ncores","nerrorsamples")])

print(sprintf(
  "Requested predictions for %s (burnin %d) for test and train data sets for %d simulations sampled from posterior (+1 from MAP) (on %d core(s)) and sampling error distribution %d times",
  args[["mcmc"]],
  args[["burnin"]],
  args[["nsimulations"]],
  args[["ncores"]],
  args[["nerrorsamples"]]
))

stopifnot(args[["nsimulations"]]  > 0)
stopifnot(args[["ncores"]]        > 0)
stopifnot(args[["nerrorsamples"]] > 0)

source(here::here("analysis/00_define_setups.R")) # to define 'rsofun_doc_output_path'
rds_input_path <- file.path(rsofun_doc_output_path, "data", "calibrations", args[["mcmc"]])

stopifnot(file.exists(rds_input_path))

outfile <- sprintf(
  "out_predict_N%d+MAP_%dburnin__%s_XXX.rds",
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

print("Posterior runs: Train")
df_predict_train <- run_prediction_rsofun(
  mcmc_posterior = out_calib,
  prediction     = "train",
  burnin_to_skip = args[["burnin"]],
  n_samples      = args[["nsimulations"]],
  n_cores        = args[["ncores"]])

# get MAP and run model for MAP parameter set
print("MAP run: Test")
df_predict_MAP_test <- run_prediction_rsofun(
  mcmc_posterior = out_calib,
  prediction     = "test",
  burnin_to_skip = args[["burnin"]],
  n_samples      = 1, # n_samples == 1, requests MAP
  n_cores        = args[["ncores"]])

print("MAP run: Train")
df_predict_MAP_train <- run_prediction_rsofun(
  mcmc_posterior = out_calib,
  prediction     = "train",
  burnin_to_skip = args[["burnin"]],
  n_samples      = 1, # n_samples == 1, requests MAP
  n_cores        = args[["ncores"]])

# readr::write_rds(df_predict_test, gsub("XXX","test",outpath), compress = "none")
# readr::write_rds(df_predict_train, gsub("XXX","train",outpath), compress = "none")
# outpathMAP <- paste0(gsub("out_predict_N[0-9]*_", "out_predict_MAP_", outpath), "_MAP.rds")
# readr::write_rds(df_predict_MAP_test, gsub("XXX","test",outpathMAP), compress = "none")
# readr::write_rds(df_predict_MAP_train, gsub("XXX","train",outpathMAP), compress = "none")


# TO STORE MORE EFFICIENTLY, UNNEST THE NESTED DATA.FRAMES:
# combine test/train data.frames but split for different targets
df_predict <- bind_rows(
  df_predict_test      |> mutate(is_train0_test1 = 1L, is_MAP = FALSE),
  df_predict_train     |> mutate(is_train0_test1 = 0L, is_MAP = FALSE),
  df_predict_MAP_test  |> mutate(is_train0_test1 = 1L, is_MAP = TRUE),
  df_predict_MAP_train |> mutate(is_train0_test1 = 0L, is_MAP = TRUE)
)
rm(df_predict_test, df_predict_train, df_predict_MAP_test, df_predict_MAP_train)

# df_predict |> object.size() |> format("MB")
df_predict_params  <- df_predict |> select(posterior_sample_id, is_train0_test1, is_MAP, pars) |> unnest(pars)
df_predict_gpp     <- df_predict |> select(posterior_sample_id, is_train0_test1, is_MAP, sim)  |> unnest(sim) |> filter(target == "gpp")     |> unnest(obs_metadata)
df_predict_vj      <- df_predict |> select(posterior_sample_id, is_train0_test1, is_MAP, sim)  |> unnest(sim) |> filter(target == "vj")      |> unnest(obs_metadata)
df_predict_bigD13C <- df_predict |> select(posterior_sample_id, is_train0_test1, is_MAP, sim)  |> unnest(sim) |> filter(target == "bigD13C") |> unnest(obs_metadata)

readr::write_rds(df_predict_params,  gsub("XXX","params",  outpath), compress = "none")
readr::write_rds(df_predict_gpp,     gsub("XXX","gpp",     outpath), compress = "none")
readr::write_rds(df_predict_vj,      gsub("XXX","vj",      outpath), compress = "none")
readr::write_rds(df_predict_bigD13C, gsub("XXX","bigD13C", outpath), compress = "none")


# apply bias-correction and simulate structural error
if (TRUE){
  print("Apply bias and sample error")
  # combine test and train predictions
  # apply bias-correction and simulate structural error
  library(dtplyr)
  library(data.table)

  # df_predict_params
  df_predict_gpp     |> select(posterior_sample_id, err_par_sd, err_par_bias) |> distinct()
  df_predict_vj      |> select(posterior_sample_id, err_par_sd, err_par_bias) |> distinct()
  df_predict_bigD13C |> select(posterior_sample_id, err_par_sd, err_par_bias) |> distinct()

  # define operation:
  apply_bias_correction_and_sample_error <- function(df_pred, N_sample_error){
    require(data.table)
    require(dtplyr)
    set.seed(1982)
    df_pred |>
      lazy_dt() |>   # use lazy data.table and dtplyr for speed
      # a) repeat lines: once for each sampled error
      # following line is basically a cross_join: with
      slice(rep(1:n(), each = N_sample_error)) |>
      mutate(error_sample_id = as.integer(rep(1:N_sample_error, n()/N_sample_error))) |>
      arrange(posterior_sample_id) |>
      # b) sample the error:
      group_by(err_par_sd, err_par_bias) |>
      mutate(err_sample                = rnorm(n(),   sd = err_par_sd),
             mod_biasremoved_no_err    = mod_no_err - err_par_bias,
             mod_biasremoved_with_err  = mod_no_err - err_par_bias + err_sample) |>
      ungroup() |>
      as_tibble() |> # to access results of lazy dtplyr-computation as normal tibble()
      # keep output light: i.e. remove unneded columns:
      select(-run_model, -err_sample) |> # select(, -sample_id, -err_par_bias, -err_par_sd) |>
      mutate(across(where(is.character), as.factor)) # |> object.size() |> format("MB")
  }

  # apply operation:
  df_predict_gpp_sampled     <- df_predict_gpp     |> apply_bias_correction_and_sample_error(N_sample_error = args[["nerrorsamples"]])
  df_predict_vj_sampled      <- df_predict_vj      |> apply_bias_correction_and_sample_error(N_sample_error = args[["nerrorsamples"]])
  df_predict_bigD13C_sampled <- df_predict_bigD13C |> apply_bias_correction_and_sample_error(N_sample_error = args[["nerrorsamples"]])


  # FOR STORAGE REASONS unnest the obs_metadata and save different targets separately:
  fname_out_gpp     <- gsub("_XXX",paste0(    "_gpp_sampled_N", args[["nerrorsamples"]], "errors"), outpath)
  fname_out_vj      <- gsub("_XXX",paste0(     "_vj_sampled_N", args[["nerrorsamples"]], "errors"), outpath)
  fname_out_bigD13C <- gsub("_XXX",paste0("_bigD13C_sampled_N", args[["nerrorsamples"]], "errors"), outpath)

  df_predict_gpp_sampled     |> select(posterior_sample_id, error_sample_id, is_train0_test1, is_MAP, sitename, target, obs,                 date, err_par_bias, err_par_sd, mod_no_err, mod_biasremoved_no_err, mod_biasremoved_with_err) |> readr::write_rds(fname_out_gpp,     compress = "none")
  df_predict_vj_sampled      |> select(posterior_sample_id, error_sample_id, is_train0_test1, is_MAP, sitename, target, obs, genus, species, year, err_par_bias, err_par_sd, mod_no_err, mod_biasremoved_no_err, mod_biasremoved_with_err) |> readr::write_rds(fname_out_vj,      compress = "none")
  df_predict_bigD13C_sampled |> select(posterior_sample_id, error_sample_id, is_train0_test1, is_MAP, sitename, target, obs,        species, year, err_par_bias, err_par_sd, mod_no_err, mod_biasremoved_no_err, mod_biasremoved_with_err) |> readr::write_rds(fname_out_bigD13C, compress = "none")
}

