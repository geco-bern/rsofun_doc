#!/usr/bin/env Rscript

# Script running sensitivity analysis

# script is called with two arguments for sampling:
# 1. calibration scenario [0,1,2,3]
# 2. iterations of Morris sensitivity analysis

# Example:
# Rscript -e 'renv::run("analysis/02_sensitivity_analysis.R", project = "../rsofun_doc", args = c(90, 10))'
# Rscript -e 'renv::run("analysis/02_sensitivity_analysis.R", project = "../rsofun_doc", args = c(90, 160))'
# Rscript -e 'renv::run("analysis/02_sensitivity_analysis.R", project = "../rsofun_doc", args = c(90, 210))'
# Rscript -e 'renv::run("analysis/02_sensitivity_analysis.R", project = "../rsofun_doc", args = c(90, 220))'
# Rscript -e 'renv::run("analysis/02_sensitivity_analysis.R", project = "../rsofun_doc", args = c(90, 500))'
# Rscript -e 'renv::run("analysis/02_sensitivity_analysis.R", project = "../rsofun_doc", args = c(90, 5000))'

# # When using this script directly from RStudio, not from the shell, specify
# args <- c("94", "1000")
# args <- c("94", "10")
# args <- c("90", "10")
# args <- c("92", "10") # this has all uniform priors except for tau... # par_to_estimate$tau_acclim  = list(mean = 14, sd = 8, lower = 0, upper = 60) # truncated normal
# args <- c("95", "10")
# args <- c("93", "10")

# to receive arguments to script from the shell
args = commandArgs(trailingOnly=TRUE)
stopifnot(length(args) == 2)
args <- as.integer(args)
names(args) <- c("scenario","iterations")

stopifnot(length(args[["scenario"]])>=1)
stopifnot(length(args[["iterations"]])==1)

print(sprintf(
  "Requested sensitivity analysis of scenario #%d, for %d iterations.",
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

# run sensitivity analysis
# res <- run_sensitivity_rsofun(
#   curr_calibration_scenario = args[["scenario"]],
#   iterations                = args[["iterations"]],
#   outpath                   = outpath,
#   par_ranges_derived_from   = "prior"
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
out_calib_s94 <- readr::read_rds(
  file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/",
            "out_calib__scen94_DEzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds")
)
out_calib_s93 <- readr::read_rds(
  file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/",
            "out_calib__scen94_DEzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds")
)

# define ranges as quantiles of posterior
# BayesianTools::getCredibleIntervals(out_calib_s94)
# BayesianTools::getPredictiveIntervals(out_calib_s94)

df_posterior <- BayesianTools::getSample(
    out_calib_s94$mod,
    parametersOnly = TRUE,
    start = 25000) |>
  as.data.frame()

par_ranges <- lapply(df_posterior, \(vec) quantile(vec, c(0.05, 0.95))) |>
  as.data.frame() |> as_tibble(rownames = "percentile") |>
  pivot_longer(-percentile, names_to = "parameter_name") |>
  mutate(percentile = if_else(percentile == "5%", "lower", "upper")) |>
  pivot_wider(names_from = percentile)

# run sensitivity analysis
res <- run_sensitivity_rsofun(
  curr_calibration_scenario = args[["scenario"]],   # TODO: comment in again
  iterations                = args[["iterations"]], # TODO: comment in again
  outpath                   = outpath,
  par_ranges_derived_from   = par_ranges
)

# TODO: we could of course make three sensitivity analyses of the three target types: "gpp","bigD13C", and "vj"
#       and equally with the test or train dataset.
#       - we could simply use par_ranges from scenario 94 with the likelihood function of other scenarios (i.e. 92,96,97) and the current code would use the training data set
#       - or we modify arguments to run_sensitivity_rsofun() to control in a more straightforward manner how we want it to be done.

