#!/usr/bin/env Rscript

# Script running sensitivity analysis

# script is called with two arguments for sampling:
# 1. calibration scenario [0,1,2,3]
# 2. iterations of Morris sensitivity analysis

# Example:
# Rscript -e 'renv::run("analysis/92_sensitivity_analysis.R", project = "../rsofun_doc", args = c(90, 10))'
# Rscript -e 'renv::run("analysis/92_sensitivity_analysis.R", project = "../rsofun_doc", args = c(90, 100))'
# Rscript -e 'renv::run("analysis/92_sensitivity_analysis.R", project = "../rsofun_doc", args = c(90, 210))'
# Rscript -e 'renv::run("analysis/92_sensitivity_analysis.R", project = "../rsofun_doc", args = c(90, 220))'
# Rscript -e 'renv::run("analysis/92_sensitivity_analysis.R", project = "../rsofun_doc", args = c(90, 500))'
# Rscript -e 'renv::run("analysis/92_sensitivity_analysis.R", project = "../rsofun_doc", args = c(90, 5000))'

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

# run sensitivitiy with requested arguments
res <- run_sensitivity_rsofun(
  curr_calibration_scenario = args[["scenario"]],
  iterations = args[["iterations"]]
)




stop("Finished script")
res <- run_sensitivity_rsofun(curr_calibration_scenario = 90, iterations = 150) # for development only
res$morrisplot %+% filter(res$morrisplot$data, !grepl("^err_",parameter))

        #
        # morris_stats <- data.frame(
        #   parameter = res$raw_in$names,
        #   mu.star   = apply(abs(res$raw_out$ee), 2, mean, na.rm = T),
        #   sigma     = apply(res$raw_out$ee,      2, sd,   na.rm = T)
        #   ) |> arrange( mu.star )
        #
        # morris_stats |>
        #   tidyr::pivot_longer( -parameter, names_to = "variable", values_to = "value") |>
        #   ggplot(aes(
        #     reorder(parameter, value),
        #     value,
        #     fill = variable),
        #     color = NA) +
        #   geom_bar(position = position_dodge(), stat = 'identity') +
        #   # layout
        #   coord_flip() +     # make horizontal
        #   scale_fill_manual(
        #     "",
        #     labels = c('mu.star' = expression(mu * "*"),
        #                'sigma' = expression(sigma)),
        #     values = c('mu.star' = "#29a274ff",
        #                'sigma' = "#777055ff")) +
        #   theme_classic() +
        #   theme(
        #     axis.text = element_text(size = 6),
        #     axis.title = element_blank(),
        #     legend.position = "inside",
        #     legend.position.inside = c(0.95, 0.05),
        #     legend.justification = c(1.0, 0),
        #   )
