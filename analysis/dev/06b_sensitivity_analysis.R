#!/usr/bin/env Rscript

# Script running sensitivity analysis

# script is called with two arguments for sampling:
# 1. calibration scenario [0,1,2,3]
# 2. iterations of Morris sensitivity analysis

# Example:
# Rscript -e 'renv::run("analysis/06b_sensitivity_analysis.R", project = "../rsofun_doc", args = c("231", "train","all",     "10")'
# Rscript -e 'renv::run("analysis/06b_sensitivity_analysis.R", project = "../rsofun_doc", args = c("231", "train","gpp",     "10")'
# Rscript -e 'renv::run("analysis/06b_sensitivity_analysis.R", project = "../rsofun_doc", args = c("231", "train","vj",      "10")'
# Rscript -e 'renv::run("analysis/06b_sensitivity_analysis.R", project = "../rsofun_doc", args = c("231", "train","bigD13C", "10")'

# # When using this script directly from RStudio, not from the shell, specify
# args <- c("231", "train","all",     "10")
# args <- c("231", "train","gpp",     "10")
# args <- c("231", "train","vj",      "10")
# args <- c("231", "train","bigD13C", "3")

# to receive arguments to script from the shell
args = commandArgs(trailingOnly=TRUE)
stopifnot(length(args) == 4)

names(args) <- c("scenario","dataset","target","iterations")
stopifnot(args[["target"]]  %in% c("all","gpp","vj","bigD13C"))
stopifnot(args[["dataset"]] %in% c("train","test"))

print(sprintf(
  "Requested sensitivity analysis of likelihood of '%s' observations in '%s' dataset using parameter ranges of posterior of scenario '#%s', for '%s' iterations.",
  args[["target"]],
  args[["dataset"]],
  args[["scenario"]],
  args[["iterations"]]
))






# Load libraries
renv::restore(prompt = FALSE)
library(rsofun)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(sensitivity)
library(BayesianTools)
# library(foreach)
# library(doParallel)

# load definitions
source(here::here("R/sensitivity_sofun_serialized.R"))
source(here::here("R/run_sensitivity_rsofun.R"), echo = TRUE)

outpath <- file.path(rsofun_doc_output_path,"data")

# run sensitivitiy with requested arguments


# a) Sensitivity analysis can be run as screening of parameters prior to calibration ----
#    with the aim to detect parameters that are not identifiable
#    This allows to remove these parameters from the calibration.

#    In this case the range the parameters are varied in the sensitivity analysis
#    corresponds to the prior belief.

# define prior of calibration scenario as an indication of range of parameter variation:
# res <- setup_rsofun_calibration(args[["scenario"]])
# source(here::here("R/createMixedPrior.R"))
# priors <- createMixedPrior(res$par)
# suffix_str <- ""

# run sensitivity analysis
# res <- run_sensitivity_rsofun(
#   iterations                = as.integer(args[["iterations"]]),
#   outpath                   = outpath,
#   par_ranges_derived_from   = priors,
#   drivobs                   = res$drivobs_train, # use training data set for this type of SA
#   design                    = list(type = "oat", levels = 20, grid.jump = 3), # handed on to sensitivity::morris()
#   suffix_str                = suffix_str,
#   prior_par_definitions     = res$par
# )


# b) Sensitivity analysis can also be run after calibration ----
#    based on the posterior range, i.e. best guesses of parameter uncertainty
#    with the aim to analyze the model output.
#    This allows to order the parameters by importance with respect to a certain
#    and identify benefits of better constraining which parameter.

#    In this case the range the parameters are varied in the sensitivity analysis
#    corresponds to the posterior after a calibration.

# load the posterior
library(tibble)
library(tidyr)
library(dplyr)

# i) define the target function (through defining the underlying driver/observation dataset (train/test, all/gpp/vj/bigD13C))

res <- setup_rsofun_calibration(scenario = 123) # this must be scenario 3/93/113/123 since it contains all targets
drivobs_to_use_for_sensitivity <- dplyr::case_when(
  args[["target"]] == "all"     && args[["dataset"]] == "train" ~ list(res$drivobs_train |> unnest_wider(targets) |>                          nest(targets = c(vj, bigD13C, gpp))),
  args[["target"]] == "gpp"     && args[["dataset"]] == "train" ~ list(res$drivobs_train |> unnest_wider(targets) |> filter(gpp)           |> nest(targets = c(vj, bigD13C, gpp))),
  args[["target"]] == "vj"      && args[["dataset"]] == "train" ~ list(res$drivobs_train |> unnest_wider(targets) |> filter(vj & !bigD13C) |> nest(targets = c(vj, bigD13C, gpp))),
  args[["target"]] == "bigD13C" && args[["dataset"]] == "train" ~ list(res$drivobs_train |> unnest_wider(targets) |> filter(bigD13C & !vj) |> nest(targets = c(vj, bigD13C, gpp))),
  args[["target"]] == "all"     && args[["dataset"]] == "test"  ~ list(res$drivobs_test  |> unnest_wider(targets) |>                          nest(targets = c(vj, bigD13C, gpp))),
  args[["target"]] == "gpp"     && args[["dataset"]] == "test"  ~ list(res$drivobs_test  |> unnest_wider(targets) |> filter(gpp)           |> nest(targets = c(vj, bigD13C, gpp))),
  args[["target"]] == "vj"      && args[["dataset"]] == "test"  ~ list(res$drivobs_test  |> unnest_wider(targets) |> filter(vj & !bigD13C) |> nest(targets = c(vj, bigD13C, gpp))), # NOTE: filter(vj) would be enough, no double-sites in test ste
  args[["target"]] == "bigD13C" && args[["dataset"]] == "test"  ~ list(res$drivobs_test  |> unnest_wider(targets) |> filter(bigD13C & !vj) |> nest(targets = c(vj, bigD13C, gpp))), # NOTE: filter(bigD13c) would be enough, no double-sites in test ste
  TRUE ~ list(NULL)
)[[1]]
if (is.null(drivobs_to_use_for_sensitivity)) {stop(sprintf("Undefined target requested for sensitivity: %s/%s", args[["target"]], args[["dataset"]]))}

# ii) define the range of the prior (use posterior of a calibration)
posterior_path <- dplyr::case_when(
  args[["scenario"]] == "231" ~ "/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen231_DREAMzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds", # TODO: switch to 100k
  args[["scenario"]] == "113" ~ "/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen113_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds",
  args[["scenario"]] == "94" ~ "/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen94_DEzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds",
  args[["scenario"]] == "93" ~ "/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen93_DEzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds",
  TRUE ~ ""
)
burnins <- dplyr::case_when(
  args[["scenario"]] == "231" ~ 8000, # TODO: switch to 25k
  args[["scenario"]] == "113" ~ 25000,
  args[["scenario"]] == "94" ~ 25000,
  args[["scenario"]] == "93" ~ 25000,
  TRUE ~ NA_integer_
)
if (posterior_path == "") {stop(sprintf("Undefined posterior rds-file for scenario %s", args[["scenario"]]))}
posterior_to_use_for_sensitivity <- readr::read_rds(posterior_path)

# run sensitivity analysis
suffix_str <- sprintf(
    "_scen%s_%s_par-range-%s_target-%s-%s_%diter_", # %dx%dchains_on_CPU%dx%d
    args[["scenario"]], "morris",
    paste0("posterior_1.5IQR-", args[["scenario"]]),
    args[["dataset"]],
    args[["target"]],
    as.integer(args[["iterations"]])
    # TODO: include further needed options from settings. (e.g. parse 'design')
  )

# undebug(run_sensitivity_rsofun)
res <- run_sensitivity_rsofun_1point5IQR(
  iterations                = as.integer(args[["iterations"]]),
  outpath                   = outpath,
  par_ranges_derived_from   = posterior_to_use_for_sensitivity$mod,
  burnins                   = burnins,
  drivobs                   = drivobs_to_use_for_sensitivity,
  design                    = list(type = "oat", levels = 20, grid.jump = 3), # handed on to sensitivity::morris()
  suffix_str                = suffix_str
)

