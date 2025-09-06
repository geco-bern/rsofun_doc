#!/usr/bin/env Rscript

# Script making train/test predictions after Bayesian calibration

# script is called with two arguments for continuing sampling:
# 1. filename of previous mcmcSamplerList.rds
# 2. "test"/"train"/"both" data set to predict
# 3. number of burnin parameter samples to discard
# 4. number of parameter samples to run simulations for
# 5. number of cores to use for running simulations

# Example:
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen0_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds",            "train", "8000", "200", "8"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen1_DEzs-100000-0iter_8x3chains_on_CPU8x1.rds",           "train", "8000", "200", "8"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen2_DEzs-100000-0iter_10x3chains_on_CPU10x1.rds",         "train", "8000", "200", "8"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen3_DEzs-100000-0iter_8x3chains_on_CPU8x1.rds",           "train", "8000", "200", "8"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen14_DEzs-35000-0iter_8x3chains_on_CPU8x1_continued.rds", "train", "8000", "200", "8"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen1_DEzs-100000-0iter_8x3chains_on_CPU8x1.rds",           "test", "8000", "200", "8"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen2_DEzs-100000-0iter_10x3chains_on_CPU10x1.rds",         "test", "8000", "200", "8"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen3_DEzs-100000-0iter_8x3chains_on_CPU8x1.rds",           "test", "8000", "200", "8"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen14_DEzs-35000-0iter_8x3chains_on_CPU8x1_continued.rds", "test", "8000", "200", "8"))'

# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen32_DEzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds", "train", "8000", "200", "8"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen36_DEzs-50000-0iter_8x3chains_on_CPU8x1.rds", "train", "8000", "200", "8"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen56_DEzs-15000-0iter_8x3chains_on_CPU8x1.rds", "train", "4000", "200", "8"))'
# Rscript -e 'renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen4_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds",  "train", "8000", "200", "8"))'


# # When using this script directly from RStudio, not from the shell, specify
# args <- c("out_calib__scen14_DEzs-35000-0iter_8x3chains_on_CPU8x1_continued.rds", "train", "8000", "200", "8")
# args <- c("out_calib__scen14_DEzs-35000-0iter_8x3chains_on_CPU8x1_continued.rds", "test", "8000", "200", "8")
# args <- c("out_calib__scen0_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds",            "test", "8000", "200", "8")
# to receive arguments to script from the shell
args = commandArgs(trailingOnly=TRUE)
stopifnot(length(args) == 5)
names(args) <- c("mcmc","target_dataset","burnin","nsimulations","ncores")

stopifnot(length(args[["mcmc"]])>=1)
stopifnot(length(args[["target_dataset"]])==1)
stopifnot(length(args[["burnin"]])==1)
stopifnot(length(args[["nsimulations"]])==1)
stopifnot(length(args[["ncores"]])==1)

args <- as.list(args)
args[c("burnin","nsimulations","ncores")] <- as.integer(args[c("burnin","nsimulations","ncores")])

print(sprintf(
  "Requested predictions for %s (burnin %d) for data set '%s' for %d simulations (on %d core(s))",
  args[["mcmc"]],
  args[["burnin"]],
  args[["target_dataset"]],
  args[["nsimulations"]],
  args[["ncores"]]
))

stopifnot(args[["target_dataset"]] %in% c("test","train","both"))
stopifnot(args[["nsimulations"]] > 0)

source(here::here("analysis/00_define_scenarios.R")) # to define 'rsofun_doc_output_path'
rds_input_path <- file.path(rsofun_doc_output_path, "data", "calibrations", args[["mcmc"]])

stopifnot(file.exists(rds_input_path))

outfile <- sprintf(
  "out_predict_N%d_%s_%dburnin__%s.rds",
  args[["nsimulations"]],
  args[["target_dataset"]],
  args[["burnin"]],
  args[["mcmc"]])

outpath <- file.path(rsofun_doc_output_path, "predictions", outfile)
if (!dir.exists(dirname(outpath))) {dir.create(dirname(outpath),  recursive=TRUE)}

# sample posteriors and run model for each sample parameter set
library(multidplyr)
source(here::here("R/run_prediction_rsofun.R"))

out_calib <- readr::read_rds(rds_input_path)
df_predict <- run_prediction_rsofun(
  mcmc_posterior = out_calib,
  prediction     = args[["target_dataset"]],
  burnin_to_skip = args[["burnin"]],
  n_samples      = args[["nsimulations"]],
  n_cores        = args[["ncores"]])

readr::write_rds(df_predict, outpath, compress = "gz")


