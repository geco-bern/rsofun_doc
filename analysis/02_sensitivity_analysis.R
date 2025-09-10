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
res <- run_sensitivity_rsofun(
  curr_calibration_scenario = args[["scenario"]],
  iterations                = args[["iterations"]],
  outpath                   = outpath
)
