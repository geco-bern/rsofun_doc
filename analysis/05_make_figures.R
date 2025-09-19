# Script that generates figures for manuscript (and others)

library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(dtplyr)
library(BayesianTools)
library(ggplot2)
library(patchwork)
library(cowplot)
library(ggridges)
library(xtable)

source(here::here("R/figure_helpers.R"))
source(here::here("analysis/00_define_scenarios.R"))

source(here::here("R/calibration_helpers.R"))
source(here::here("R/prediction_helpers.R"))
source(here::here("R/run_prediction_rsofun.R"))


flag_plot_sampling_and_posteriors <- FALSE # possibility to switch this off
if (flag_plot_sampling_and_posteriors){
  # out_calib_s90 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen90_DEzs-50000-0iter_8x3chains_on_CPU8x1.rds"))
  out_calib_s90 <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen90_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s91 <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen91_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds")) # TODO change to 80k
  out_calib_s92 <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen92_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds")) # TODO change to 80k
  out_calib_s93 <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen93_DEzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s94 <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen94_DEzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s95 <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen95_DEzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds")) # TODO change to 100k
  out_calib_s96 <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen96_DEzs-80000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s97 <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen97_DEzs-80000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s98 <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen98_DEzs-80000-0iter_8x3chains_on_CPU8x1_continued.rds"))

  out_calib_s90DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen90_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s91DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen91_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s92DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen92_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s93DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen93_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s94DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen94_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))  # TODO change to 100k

  out_calib_s110DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen110_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s111DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen111_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s112DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen112_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s113DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen113_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))  # TODO change to 100k
  out_calib_s114DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen114_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))  # TODO change to 100k
  out_calib_s115DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen115_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))  # TODO change to 100k
  out_calib_s116DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen116_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s117DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen117_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s118DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen118_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))

  # out_calib_s104 <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen104_DEzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  (plot_prior_posterior_density(out_calib_s104$mod,burnin_to_skip = 1) + ggtitle("Scenario 104")+ ggtitle(out_calib_s104$fpath))
  # out_calib_s103 <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen103_DEzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  # (plot_prior_posterior_density(out_calib_s103$mod,burnin_to_skip = 1) + ggtitle("Scenario 103")+ ggtitle(out_calib_s103$fpath))


  ########## MCMC PLOTS: ########### -
  burnin_to_skip = 18000

  # Figure E: MCMC convergence diagnostics ----
  ## trace plots (of chains), correlation plots, Gelman-Rubin (r.1.1)
  (plot_mcmc_trace(out_calib_s110DREAMzs$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 25000)    + ggtitle(out_calib_s110DREAMzs$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s110DREAMZs.png")
  (plot_mcmc_trace(out_calib_s111DREAMzs$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 25000)    + ggtitle(out_calib_s111DREAMzs$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s111DREAMZs.png")
  (plot_mcmc_trace(out_calib_s112DREAMzs$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 25000)    + ggtitle(out_calib_s112DREAMzs$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s112DREAMZs.png")
  (plot_mcmc_trace(out_calib_s113DREAMzs$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 25000)    + ggtitle(out_calib_s113DREAMzs$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s113DREAMZs.png")
  (plot_mcmc_trace(out_calib_s114DREAMzs$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 25000)    + ggtitle(out_calib_s114DREAMzs$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s114DREAMZs.png")
  (plot_mcmc_trace(out_calib_s115DREAMzs$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 25000)    + ggtitle(out_calib_s115DREAMzs$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s115DREAMZs.png")
  (plot_mcmc_trace(out_calib_s116DREAMzs$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 25000)    + ggtitle(out_calib_s116DREAMzs$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s116DREAMZs.png")
  (plot_mcmc_trace(out_calib_s117DREAMzs$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 25000)    + ggtitle(out_calib_s117DREAMzs$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s117DREAMZs.png")
  (plot_mcmc_trace(out_calib_s118DREAMzs$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 25000)    + ggtitle(out_calib_s118DREAMzs$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s118DREAMZs.png")

  # (plot_mcmc_trace(out_calib_s103$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 25000)    + ggtitle(out_calib_s103$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s103.png")
  # (plot_mcmc_trace(out_calib_s104$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 25000)    + ggtitle(out_calib_s104$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s104.png")

  # Figure A: prior, posterior density plot ----
  ## for each scenario x params
  pl_post_s94DR<-(plot_prior_posterior_density(out_calib_s94DREAMzs$mod, burnin_to_skip = 8000)   + ggtitle("Scenario 94")+ ggtitle(out_calib_s94DREAMzs$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s94DREAMZs.png")

  pl_post_s110DR<-(plot_prior_posterior_density(out_calib_s110DREAMzs$mod, burnin_to_skip = 25000)   + ggtitle("Scenario 110")+ ggtitle(out_calib_s110DREAMzs$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s110DREAMZs.png")
  pl_post_s111DR<-(plot_prior_posterior_density(out_calib_s111DREAMzs$mod, burnin_to_skip = 25000)   + ggtitle("Scenario 111")+ ggtitle(out_calib_s111DREAMzs$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s111DREAMZs.png")
  pl_post_s112DR<-(plot_prior_posterior_density(out_calib_s112DREAMzs$mod, burnin_to_skip = 25000)   + ggtitle("Scenario 112")+ ggtitle(out_calib_s112DREAMzs$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s112DREAMZs.png")
  pl_post_s113DR<-(plot_prior_posterior_density(out_calib_s113DREAMzs$mod, burnin_to_skip = 25000)   + ggtitle("Scenario 113")+ ggtitle(out_calib_s113DREAMzs$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s113DREAMZs.png")
  pl_post_s114DR<-(plot_prior_posterior_density(out_calib_s114DREAMzs$mod, burnin_to_skip = 25000)   + ggtitle("Scenario 114")+ ggtitle(out_calib_s114DREAMzs$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s114DREAMZs.png")
  pl_post_s115DR<-(plot_prior_posterior_density(out_calib_s115DREAMzs$mod, burnin_to_skip = 25000)   + ggtitle("Scenario 115")+ ggtitle(out_calib_s115DREAMzs$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s115DREAMZs.png")
  pl_post_s116DR<-(plot_prior_posterior_density(out_calib_s116DREAMzs$mod, burnin_to_skip = 25000)   + ggtitle("Scenario 116")+ ggtitle(out_calib_s116DREAMzs$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s116DREAMZs.png")
  pl_post_s117DR<-(plot_prior_posterior_density(out_calib_s117DREAMzs$mod, burnin_to_skip = 25000)   + ggtitle("Scenario 117")+ ggtitle(out_calib_s117DREAMzs$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s117DREAMZs.png")
  pl_post_s118DR<-(plot_prior_posterior_density(out_calib_s118DREAMzs$mod, burnin_to_skip = 25000)   + ggtitle("Scenario 118")+ ggtitle(out_calib_s118DREAMzs$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s118DREAMZs.png")

  # pl_post_s103<-(plot_prior_posterior_density(out_calib_s103$mod, burnin_to_skip = 25000)   + ggtitle(out_calib_s103$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s103.png")
  # pl_post_s104<-(plot_prior_posterior_density(out_calib_s104$mod, burnin_to_skip = 25000)   + ggtitle(out_calib_s104$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s104.png")

  # compare them:
  # param_order <- out_calib_s94$mod[[1]]$setup$names
  param_order <- out_calib_s114DREAMzs$mod[[1]]$setup$names

  library(ggridges)
  # scenarios_to_compare3 <- list("Prior 1" = out_calib_s91$mod,
  #                               "Prior 2" = out_calib_s92$mod,
  #                               "Prior 3" = out_calib_s93$mod,
  #                               "Prior 4" = out_calib_s94$mod,
  #                               "1" = out_calib_s91$mod,
  #                               "2" = out_calib_s92$mod,
  #                               "3" = out_calib_s93$mod,
  #                               "4" = out_calib_s94$mod)
  # scenarios_to_compare3b <- list("Prior 1" = out_calib_s91DREAMzs$mod,
  #                               "Prior 2" = out_calib_s92DREAMzs$mod,
  #                               "Prior 3" = out_calib_s93DREAMzs$mod,
  #                               "Prior 4" = out_calib_s94DREAMzs$mod,
  #                               "1" = out_calib_s91DREAMzs$mod,
  #                               "2" = out_calib_s92DREAMzs$mod,
  #                               "3" = out_calib_s93DREAMzs$mod,
  #                               "4" = out_calib_s94DREAMzs$mod)
  # pl_post_comparison3 <- plot_prior_posterior_density_compare( named_list_scen =  scenarios_to_compare3, burnin_to_skip  = burnin_to_skip,
  #   ridges = TRUE, add_MAP = TRUE, param_order = param_order, params_not_to_plot = c("rd_to_vcmax", "soilm_betao"), correct_scenarios = correct_scenarios)
  # pl_post_comparison3b <- plot_prior_posterior_density_compare( named_list_scen =  scenarios_to_compare3b, burnin_to_skip  = 25000,
  #   ridges = TRUE, add_MAP = TRUE, param_order = param_order, params_not_to_plot = c("rd_to_vcmax", "soilm_betao"), correct_scenarios = correct_scenarios)

  scenarios_to_compare3c <- list("Prior 1" = out_calib_s111DREAMzs$mod,
                                "Prior 2" = out_calib_s112DREAMzs$mod,
                                "Prior 3" = out_calib_s113DREAMzs$mod,
                                "Prior 4" = out_calib_s114DREAMzs$mod,
                                "Prior 5" = out_calib_s115DREAMzs$mod,
                                "1" = out_calib_s111DREAMzs$mod,
                                "2" = out_calib_s112DREAMzs$mod,
                                "3" = out_calib_s113DREAMzs$mod,
                                "4" = out_calib_s114DREAMzs$mod,
                                "5" = out_calib_s115DREAMzs$mod)
  correct_scenarios <- c("5"=115, "4"=114, "3"=113, "2"=112, "1" = 111, "0" = 110) # this is for retrieval of correct scenario definition for fixed parameters, in spite of renaming
  pl_post_comparison3c <- plot_prior_posterior_density_compare( named_list_scen =  scenarios_to_compare3c[c("Prior 1","Prior 2","Prior 3","Prior 4", "1","2","3","4")], burnin_to_skip  = 25000,
    ridges = TRUE, add_MAP = TRUE, param_order = param_order, params_not_to_plot = c("rd_to_vcmax", "soilm_betao"), correct_scenarios = correct_scenarios)
  pl_post_comparison3d <- plot_prior_posterior_density_compare( named_list_scen =  scenarios_to_compare3c, burnin_to_skip  = 25000,
    ridges = TRUE, add_MAP = TRUE, param_order = param_order, params_not_to_plot = c("rd_to_vcmax", "soilm_betao"), correct_scenarios = correct_scenarios)
  pl_post_comparison3e <- plot_prior_posterior_density_compare( named_list_scen =  scenarios_to_compare3c[c("Prior 1","Prior 2","Prior 3", "1","2","3")], burnin_to_skip  = 25000,
    ridges = TRUE, add_MAP = TRUE, param_order = param_order, params_not_to_plot = c("rd_to_vcmax", "soilm_betao"), correct_scenarios = correct_scenarios)

  # ggsave_and_return(pl_post_comparison3, "fig_A_MCMCconvergence_posterior_s91_92_93_94.png", width = 7.2, height = 3.6)
  # ggsave_and_return(pl_post_comparison3b, "fig_A_MCMCconvergence_posterior_s91DR_92DR_93DR_94DR.png", width = 7.2, height = 3.6)
  ggsave_and_return(pl_post_comparison3c, "fig_A_MCMCconvergence_posterior_s111DR_112DR_113DR_114DR.png", width = 7.2, height = 3.6)
  ggsave_and_return(pl_post_comparison3d, "fig_A_MCMCconvergence_posterior_s111DR_112DR_113DR_114DR_115DR.png", width = 7.2, height = 3.6)
  ggsave_and_return(pl_post_comparison3e, "fig_A_MCMCconvergence_posterior_s111DR_112DR_113DR.png", width = 7.2, height = 3.6)

  scenarios_to_compare4 <- list("Prior 0" = out_calib_s110DREAMzs$mod,
                                "Prior 1" = out_calib_s111DREAMzs$mod,
                                "Prior 2" = out_calib_s112DREAMzs$mod,
                                "Prior 3" = out_calib_s113DREAMzs$mod,
                                "Prior 4" = out_calib_s114DREAMzs$mod,
                                "Prior 6" = out_calib_s116DREAMzs$mod,
                                "Prior 7" = out_calib_s117DREAMzs$mod,
                                "Prior 8" = out_calib_s118DREAMzs$mod,
                                "0" = out_calib_s110DREAMzs$mod,
                                "1" = out_calib_s111DREAMzs$mod,
                                "2" = out_calib_s112DREAMzs$mod,
                                "3" = out_calib_s113DREAMzs$mod,
                                "4" = out_calib_s114DREAMzs$mod,
                                "6" = out_calib_s116DREAMzs$mod,
                                "7" = out_calib_s117DREAMzs$mod,
                                "8" = out_calib_s118DREAMzs$mod
                                )
  pl_post_comparison4 <- plot_prior_posterior_density_compare(
    named_list_scen =  scenarios_to_compare4,
    burnin_to_skip  = 25000,
    ridges = TRUE, add_MAP = TRUE,
    correct_scenarios = c("8"=118, "7"=117, "6"=116, "4"=114, "3"=113, "2" = 111, "1" = 111, "0" = 110),  # this is for retrieval of correct scenario definition for fixed parameters, in spite of renaming
    param_order = param_order,
    params_not_to_plot = c("rd_to_vcmax", "soilm_betao")
  )

  ggsave_and_return(pl_post_comparison4,
                    "fig_A_MCMCconvergence_posterior_s110DR_111DR_112DR_113DR_114DR_115DR_116DR_117DR.png",
                    width = 7.2, height = 3.6)


  # Posterior parameter correlation analysis ----
  if (FALSE){ # This is quite a slow plot:

    save_corr_plot <- function(out_calib, thin, start, filename){
      png(filename, width = 7.2, height = 7.2, units = "in", res = 300)
      correlationPlot(out_calib$mod, thin = thin, start = start)
      dev.off()
    }

    # save_corr_plot(out_calib_s0, thin = 1, start = 0,             filename = here::here("fig/fig_E2_MCMCconvergence_corr_s0_burnin.png")) # the scatter plots with burnin do not make much sense
    # save_corr_plot(out_calib_s0, thin = 1, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s0.png"))
    # save_corr_plot(out_calib_s1, thin = 1, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s1.png"))
    # save_corr_plot(out_calib_s2, thin = 1, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s2.png"))
    # save_corr_plot(out_calib_s3, thin = 1, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s3.png"))
    # save_corr_plot(out_calib_s4, thin = 1, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s4.png"))
    save_corr_plot(out_calib_s90, thin = 5,  start = burnin_to_skip, filename = here::here("fig/fig_E2_MCMCconvergence_corr_s90.png"))
    save_corr_plot(out_calib_s91, thin = 5, start = burnin_to_skip, filename = here::here("fig/fig_E2_MCMCconvergence_corr_s91.png"))
    save_corr_plot(out_calib_s92, thin = 5, start = burnin_to_skip, filename = here::here("fig/fig_E2_MCMCconvergence_corr_s92.png"))
    save_corr_plot(out_calib_s93, thin = 5, start = 25000,          filename = here::here("fig/fig_E2_MCMCconvergence_corr_s93.png"))
    save_corr_plot(out_calib_s94, thin = 5, start = 25000,          filename = here::here("fig/fig_E2_MCMCconvergence_corr_s94.png"))
    save_corr_plot(out_calib_s95, thin = 5, start = 25000,          filename = here::here("fig/fig_E2_MCMCconvergence_corr_s95.png"))
    save_corr_plot(out_calib_s96, thin = 5, start = burnin_to_skip, filename = here::here("fig/fig_E2_MCMCconvergence_corr_s96.png"))
    save_corr_plot(out_calib_s97, thin = 5, start = burnin_to_skip, filename = here::here("fig/fig_E2_MCMCconvergence_corr_s97.png"))
    save_corr_plot(out_calib_s98, thin = 5, start = burnin_to_skip, filename = here::here("fig/fig_E2_MCMCconvergence_corr_s98.png"))
    save_corr_plot(out_calib_s94DREAMzs,thin=5,start=12000,         filename = here::here("fig/fig_E2_MCMCconvergence_corr_s94DREAMzs.png"))

    save_corr_plot(out_calib_s110DREAMzs,thin=5,start=25000,         filename = here::here("fig/fig_E2_MCMCconvergence_corr_s110DREAMzs_burnin25000.png"))
    save_corr_plot(out_calib_s111DREAMzs,thin=5,start=25000,         filename = here::here("fig/fig_E2_MCMCconvergence_corr_s111DREAMzs_burnin25000.png"))
    save_corr_plot(out_calib_s113DREAMzs,thin=5,start=25000,         filename = here::here("fig/fig_E2_MCMCconvergence_corr_s113DREAMzs_burnin25000.png"))
    save_corr_plot(out_calib_s114DREAMzs,thin=5,start=25000,         filename = here::here("fig/fig_E2_MCMCconvergence_corr_s114DREAMzs_burnin25000.png"))
    save_corr_plot(out_calib_s116DREAMzs,thin=5,start=25000,         filename = here::here("fig/fig_E2_MCMCconvergence_corr_s116DREAMzs_burnin25000.png"))
    save_corr_plot(out_calib_s117DREAMzs,thin=5,start=25000,         filename = here::here("fig/fig_E2_MCMCconvergence_corr_s117DREAMzs_burnin25000.png"))
    save_corr_plot(out_calib_s118DREAMzs,thin=5,start=25000,         filename = here::here("fig/fig_E2_MCMCconvergence_corr_s118DREAMzs_burnin25000.png"))

  }


          # # Check which parameters are most correlated
          # samples_s0 <- getSample(out_calib_s0_serial$mod, thin = 1, start = burnin_to_skip)
          # cor_matrix <- cor(samples_s0)
          # high_cor <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
          # if(nrow(high_cor) > 0){
          #   print("Highly correlated parameters (|r| > 0.7):")
          #   for(i in 1:nrow(high_cor)) {
          #     row_idx <- high_cor[i,1]
          #     col_idx <- high_cor[i,2]
          #     if(row_idx < col_idx) {  # avoid duplicates
          #       cat(sprintf("%s - %s: %.3f\n",
          #                   rownames(cor_matrix)[row_idx],
          #                   colnames(cor_matrix)[col_idx],
          #                   cor_matrix[row_idx, col_idx]))
          #     }
          #   }
          # }
          #
          # samples_s3 <- getSample(out_calib_s3$mod, thin = 1, start = burnin_to_skip)
          # cor_matrix <- cor(samples_s3)
          # high_cor <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
          # if(nrow(high_cor) > 0){
          #   print("Highly correlated parameters (|r| > 0.7):")
          #   for(i in 1:nrow(high_cor)) {
          #     row_idx <- high_cor[i,1]
          #     col_idx <- high_cor[i,2]
          #     if(row_idx < col_idx) {  # avoid duplicates
          #       cat(sprintf("%s - %s: %.3f\n",
          #                   rownames(cor_matrix)[row_idx],
          #                   colnames(cor_matrix)[col_idx],
          #                   cor_matrix[row_idx, col_idx]))
          #     }
          #   }
          # }

              ##### FROM THE PREVIOUS SUBMISSION
              # autocorrelation plots of chains
              # predictive plots (including error)

                # `BayesianTools` makes it easy to produce the trace plot of the MCMC chains and the posterior density plot for the parameters. Trace plots show the time series of the sampled chains, which should reach a stationary state. One can also choose a burnin visually, to discard the early iterations and keep only the samples from the stationary distribution to which they converge. We set \code{burnin = 3000} above from previous runs, and those iterations are not shown by the following trace plot. The samples after the burnin period should be used for inference.
                # ```{r fig.height = 10, fig.width = 7}
                # plot(par_calib$mod)
                # ```
                #
                # <!-- Internal recommendation: When you run the MCMC simulations with the DEzs sampler, there are two parameters that control the number of chains used: nrChains (documented for runMCMC) and startValue (documented for DEzs). The first dictates the number of independent chains to be run by the algorithm, while the second determines the number of internal chains to be run from a starting population (i.e. a population of initial parameter seeds). As Florian points out in this issue (https://github.com/florianhartig/BayesianTools/issues/224#issuecomment-877416919) the chains from within a population tend to be more correlated than those from independent "chains", and therefore internal chains should not be regarded as independent chains. This supports why for r3PG, they use nrChains=3 and startValue=3 (used by default), leading to 3*3=9 chains being plotted. For the example above, it doesn't make a big difference because the convergence is quite fast. -->
                #
                # The posterior density plots may be lumpy. In this case it's advisable to run the MCMC algorithm for more iterations, in order to get a better estimate of the parameters' posterior distributions. A good posterior should look more gaussian (although it can be skewed). A multimodal density indicates that the MCMC is still exploring the parameter space and hasn't converged yet. The posteriors can be plotted against the priors using `BayesianTools::marginalPlot()`.
                #
                # When convergence has been reached, the oscillation of the time series should look like white noise. It's normal that consecutive MCMC samples are correlated because of the sampling algorithm's nature, but the presence of a more general trend indicates that convergence hasn't been reached.
                #
                # <!-- Furthermore, trace plots can be deceiving and partial autocorrelation plots can throw some light. If autocorrelation is present, this can mean that the sampling is stuck in local maxima and the posterior parameter space may not be explored fully. Sometimes, thinning is used to deal with this autocorrelation. -->
                # ```{r fig.height = 10, fig.width = 7, eval = FALSE, echo = FALSE}
                # # Define function for plotting chains separately
                # plot_acf_mcmc <- function(chains, par_names){
                #   # chains: from the BayesianTools output
                #   n_chains <- length(chains)
                #   n_internal_chains <- length(chains[[1]]$chain)
                #   par(mfrow = c(length(par_names), n_chains))
                #   for(par_name in par_names){
                #     for(i in 1:n_chains){
                #       stopifnot(n_internal_chains<=3); color = c("blue", "red", "darkgreen")
                #       spacing = 0.5/n_internal_chains
                #       for(j in 1:n_internal_chains){
                #         autocorr_internal_chain <- pacf(getSample(chains[[i]]$chain[[j]])[, par_name], plot = FALSE)
                #         if(j==1){
                #           plot(autocorr_internal_chain, col = color[j],
                #                main = sprintf("Series of %s , chain (%i)", par_name, i))
                #         } else {
                #           lines(autocorr_internal_chain$lag + spacing*(j-1),
                #                 autocorr_internal_chain$acf,
                #                 col = color[j], type = "h")
                #         }
                #       }
                #     }
                #   }
                # }
                # plot_acf_mcmc(
                #   par_calib$mod,
                #   c("kphio", "kphio_par_a", "kphio_par_b", "soilm_thetastar", "soilm_betao",  "err_gpp")
                #   )
                # ```
                #
                # Looking at the correlation between chains for different parameters is also helpful because parameter correlation may slow down convergence, or the chains may oscillate in the multivariate posterior space. In this calibration we expect parameter samples to be somewhat correlated, especially `kphio_par_a` and `kphio_par_b` because they specify the shape of the temperature dependence of the quantum yield efficiency, $\varphi_o(T)$. We can also see that `err_gpp` is correlated with `kphio` (to which the P-model is very sensitive), since the error represents how good the model fits the observed GPP.
                #
                # ```{r fig.width=5, fig.height=5}
                # correlationPlot(par_calib$mod, thin = 1)   # use all samples, no thinning
                # ```
                #
                # In addition to visualizations, it's helpful to compute some convergence diagnostics, like the Gelman-Brooks-Rubin (GBR) potential scale factors. This diagnostic compares the variance within chains to that across chains and should progressively get closer to 1. It is common in the literature (Gelman, A., Carlin, J.B., Stern, H.S., Rubin, D.B.: Bayesian Data
                # Analysis, 2nd edn. Chapman & Hall, London (2004)) to accept convergence with a GBR between 1.05 and 1.1.
                # ```{r}
                # gelmanDiagnostics(par_calib$mod)
                # ```
                #
                # Finally, the parameter MAP estimates can be derived from the chains (that converged) after removing the burnin period. They can be seen, next to other statistics, using the `summary` function from the `BayesianTools` library.
                #
                # ```{r}
                # summary(par_calib$mod)
                # ```
                #
                # More details on diagnosing MCMC convergence can be found in [this vignette from BayesianTools](https://florianhartig.github.io/BayesianTools/articles/BayesianTools.html#running-mcmc-and-smc-functions) and [this blogpost](https://theoreticalecology.wordpress.com/2011/12/09/mcmc-chain-analysis-and-convergence-diagnostics-with-coda-in-r/).

  # Figure F: TBD: comparison of calibration vs GenSA?? ----
  ## or just using prior estimates from Stocker 2020? (r.1.14)

}



########## PREDICTION PLOTS: ########### -

# Figure B: error distribution density plot ----
## for each scenario x target x test+train

# Figure B2: error distribution predObs scatter plot ----
## for each scenario x target x test

flag_plot_predictions <- TRUE # possibility to switch this off
# define what data to load (and use this as suffix for output)
n_post <- "N20+MAP"
n_err <- "_N3errors"
# outfname_suffix <- paste0(n_post, n_err, "_s94-s91-s90")
outfname_suffix <- paste0(n_post, n_err, "_s113-s111")

if (flag_plot_predictions){
  # # Load sampled posterior params used for predictions
  # df_94_params  <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen94_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds")))
  # df_91_params  <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen91_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds")))
  # df_90_params  <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen90_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds")))
  # # Load predictions for plotting
  # df_94_vj      <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen94_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds")))
  # df_91_vj      <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen91_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds")))
  # df_90_vj      <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen90_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds")))
  # df_94_bigD13C <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen94_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds")))
  # df_91_bigD13C <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen91_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds")))
  # df_90_bigD13C <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen90_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds")))
  # df_94_gpp     <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen94_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds")))
  # df_91_gpp     <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen91_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds")))
  # df_90_gpp     <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen90_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds")))

  # Load sampled posterior params used for predictions
  df_113_params  <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_12000burnin__out_calib__scen113_DREAMzs-40000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds"))
  df_111_params  <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_25000burnin__out_calib__scen111_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds"))
  df_110_params  <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_25000burnin__out_calib__scen110_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds"))
  # Load predictions for plotting
  df_113_vj      <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_12000burnin__out_calib__scen113_DREAMzs-40000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds"))
  df_113_bigD13C <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_12000burnin__out_calib__scen113_DREAMzs-40000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds"))
  df_113_gpp     <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_12000burnin__out_calib__scen113_DREAMzs-40000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds"))
  df_111_vj      <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_25000burnin__out_calib__scen111_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds"))
  df_111_bigD13C <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_25000burnin__out_calib__scen111_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds"))
  df_111_gpp     <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_25000burnin__out_calib__scen111_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds"))
  df_110_vj      <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_25000burnin__out_calib__scen110_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds"))
  df_110_bigD13C <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_25000burnin__out_calib__scen110_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds"))
  df_110_gpp     <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_25000burnin__out_calib__scen110_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds"))

  # prepare plotting
  # # i) bind together, ii) mutate(Scenario = "0","1","3")    # for FigB: filter(!is.na(obs)) , for FigB3: filter(target == "gpp")
  dfwide_gpp_train <- bind_rows(
    df_113_gpp |> filter(is_train0_test1 == 0) |> mutate(Scenario = "3"),
    df_111_gpp |> filter(is_train0_test1 == 0) |> mutate(Scenario = "1"),
    df_110_gpp |> filter(is_train0_test1 == 0) |> mutate(Scenario = "0")
  )
  dfwide_gpp_test <- bind_rows(
    df_113_gpp |> filter(is_train0_test1 == 1) |> mutate(Scenario = "3"),
    df_111_gpp |> filter(is_train0_test1 == 1) |> mutate(Scenario = "1"),
    df_110_gpp |> filter(is_train0_test1 == 1) |> mutate(Scenario = "0")
  )
  dfwide_vj <- bind_rows(
    df_113_vj |> mutate(Scenario = "3"),
    df_111_vj |> mutate(Scenario = "1"),
    df_110_vj |> mutate(Scenario = "0")
  )
  dfwide_bigD13C <- bind_rows(
    df_113_bigD13C |> mutate(Scenario = "3"),
    df_111_bigD13C |> mutate(Scenario = "1"),
    df_110_bigD13C |> mutate(Scenario = "0")
  )
  dfwide_gpp_train |> select(              date, sitename, target) |> distinct() # 90k observations
  dfwide_gpp_test  |> select(              date, sitename, target) |> distinct() # 127k observations
  dfwide_vj        |> select(genus,species,year, sitename, target) |> distinct() # 590 observations
  dfwide_bigD13C   |> select(      species,year, sitename, target) |> distinct() # 2347 observations
  rm(df_113_vj)
  rm(df_111_vj)
  rm(df_110_vj)
  rm(df_113_bigD13C)
  rm(df_111_bigD13C)
  rm(df_110_bigD13C)
  rm(df_113_gpp)
  rm(df_111_gpp)
  rm(df_110_gpp)

  # make data sets long for plotting
  make_long <- function(df){
    df |>
      # pivot the model_output_types to long
      pivot_longer(c(mod_no_err, mod_biasremoved_no_err, mod_biasremoved_with_err),
                   names_to = "model_output_type", values_to = "modelled") |>
      mutate(model_output_type = factor(
        model_output_type,
        levels = c("mod_no_err","mod_biasremoved_no_err","mod_biasremoved_with_err"),
        labels = c("rsofun",    "bias-corrected",        "with struct. uncert."))) |>
      # derive column `parameters` ("MAP" or "Posterior") from `is_MAP`
      mutate(is_MAP = factor(ifelse(is_MAP, "MAP", "Posterior"))) |> rename(parameters = is_MAP) |>
      # derive column `dataset` ("train" or "test") from column `is_train0_test1`
      mutate(is_train0_test1 = factor(ifelse(is_train0_test1==1, "test", "train"))) |> rename(dataset = is_train0_test1)
  }
  dflong_gpp_train <- dfwide_gpp_train |>
    select(posterior_sample_id, Scenario, is_train0_test1, is_MAP, sitename, target,
           obs,                 date, # these are target specific observation_metadata
           mod_no_err, mod_biasremoved_no_err, mod_biasremoved_with_err) |>
    make_long()
  dflong_gpp_test <- dfwide_gpp_test |>
    select(posterior_sample_id, Scenario, is_train0_test1, is_MAP, sitename, target,
           obs,                 date, # these are target specific observation_metadata
           mod_no_err, mod_biasremoved_no_err, mod_biasremoved_with_err) |>
    make_long()
  dflong_vj <- dfwide_vj |>
    select(posterior_sample_id, Scenario, is_train0_test1, is_MAP, sitename, target,
           obs, genus, species, year, # these are target specific observation_metadata
           mod_no_err, mod_biasremoved_no_err, mod_biasremoved_with_err) |>
    make_long()
  dflong_bigD13C <- dfwide_bigD13C |>
    select(posterior_sample_id, Scenario, is_train0_test1, is_MAP, sitename, target,
           obs,        species, year, # these are target specific observation_metadata
           mod_no_err, mod_biasremoved_no_err, mod_biasremoved_with_err) |>
    make_long()


  # manually define what to show as output depending on the scenario:
  df_B1and2and3 <- list(
    # for gpp:
    gpp = bind_rows(dflong_gpp_test, dflong_gpp_train) |>
      # remove the bias-corrected values for gpp since we did not fit a bias
      filter(!(model_output_type %in% c("bias-corrected"))) |>
      # select what to plot and how to name it
      mutate(y_facet = case_when(
        Scenario == "4" & model_output_type == "rsofun" &               parameters == "MAP"       ~ "MAP",
        Scenario == "4" & model_output_type == "rsofun" &               parameters == "Posterior" ~ "Posterior",
        Scenario == "4" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error",
        Scenario == "3" & model_output_type == "rsofun" &               parameters == "MAP"       ~ "MAP",
        Scenario == "3" & model_output_type == "rsofun" &               parameters == "Posterior" ~ "Posterior",
        Scenario == "3" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error",
        Scenario == "1" & model_output_type == "rsofun" &               parameters == "MAP"       ~ "MAP",
        Scenario == "1" & model_output_type == "rsofun" &               parameters == "Posterior" ~ "Posterior",
        Scenario == "1" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error",
        # all else is not plotted
        TRUE ~ "remove") |> factor(levels = c("MAP","Posterior","Post.+Error"))) |>
      filter(y_facet != "remove"),
    # for vj:
    vj = dflong_vj |>
      # select what to plot and how to name it
      mutate(y_facet = case_when(
        Scenario == "4" & model_output_type == "bias-corrected" &       parameters == "MAP"       ~ "MAP",
        Scenario == "4" & model_output_type == "bias-corrected" &       parameters == "Posterior" ~ "Posterior",
        Scenario == "4" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error",
        Scenario == "3" & model_output_type == "bias-corrected" &       parameters == "MAP"       ~ "MAP",
        Scenario == "3" & model_output_type == "bias-corrected" &       parameters == "Posterior" ~ "Posterior",
        Scenario == "3" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error",
        Scenario == "1" & model_output_type == "rsofun" &               parameters == "MAP"       ~ "MAP",       # since we did not fit a bias, we don't have a bias correction
        Scenario == "1" & model_output_type == "rsofun" &               parameters == "Posterior" ~ "Posterior", # since we did not fit a bias, we don't have a bias correction
        # Scenario == "1" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error", # since vj was not fitted we don't have an error model
        # all else is not plotted
        TRUE ~ "remove") |> factor(levels = c("MAP","Posterior","Post.+Error"))) |>
      filter(y_facet != "remove"),
    # for bigD13C:
    bigD13C = dflong_bigD13C |>
      # select what to plot and how to name it
      mutate(y_facet = case_when(
        Scenario == "4" & model_output_type == "bias-corrected" &       parameters == "MAP"       ~ "MAP",
        Scenario == "4" & model_output_type == "bias-corrected" &       parameters == "Posterior" ~ "Posterior",
        Scenario == "4" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error",
        Scenario == "3" & model_output_type == "bias-corrected" &       parameters == "MAP"       ~ "MAP",
        Scenario == "3" & model_output_type == "bias-corrected" &       parameters == "Posterior" ~ "Posterior",
        Scenario == "3" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error",
        Scenario == "1" & model_output_type == "rsofun" &               parameters == "MAP"       ~ "MAP",       # since we did not fit a bias, we don't have a bias correction
        Scenario == "1" & model_output_type == "rsofun" &               parameters == "Posterior" ~ "Posterior", # since we did not fit a bias, we don't have a bias correction
        # Scenario == "1" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error", # since bigD13C was not fitted we don't have an error model
        # all else is not plotted
        TRUE ~ "remove") |> factor(levels = c("MAP","Posterior","Post.+Error"))) |>
      filter(y_facet != "remove")
  )

  df_B1_density    <- lapply(df_B1and2and3, \(df) df |> filter(!is.na(obs)))                    # remove NA observations
  df_B2_scatter    <- lapply(df_B1and2and3, \(df) df |> filter(!is.na(obs), dataset == "test")) # remove NA observations and test data set
  df_B3_timeseries <- df_B1and2and3["gpp"]                                                      # keep NA observations and test data set, only use gpp


  # derive alternative representation of observations as average observations (e.g. across species, samples, etc...)
  df_B2_scatter_avgObs <- list(
    gpp = df_B2_scatter$gpp |># no need to aggregate since each site and each day has only 1 gpp value
      mutate(obs_avg = obs,
             obs_sd  = NA,
             obs_n   = 1),
    vj  = df_B2_scatter$vj |>
    group_by(y_facet, modelled, target, sitename, parameters, dataset, Scenario, posterior_sample_id) |>
    summarise(obs_avg = mean(obs),
              obs_sd  = mean(obs),
              obs_n   = n()),
    bigD13C = df_B2_scatter$bigD13C |>
    group_by(y_facet, modelled, target, sitename, parameters, dataset, Scenario, posterior_sample_id) |>
    summarise(obs_avg = mean(obs),
              obs_sd  = mean(obs),
              obs_n   = n())
  )
  df_B1_density_avgObs <- list(
    gpp = df_B1_density$gpp |># no need to aggregate since each site and each day has only 1 gpp value
      mutate(obs_avg = obs,
             obs_sd  = NA,
             obs_n   = 1),
    vj  = df_B1_density$vj |>
    group_by(y_facet, modelled, target, sitename, parameters, dataset, Scenario, posterior_sample_id) |>
    summarise(obs_avg = mean(obs),
              obs_sd  = mean(obs),
              obs_n   = n()),
    bigD13C = df_B1_density$bigD13C |>
    group_by(y_facet, modelled, target, sitename, parameters, dataset, Scenario, posterior_sample_id) |>
    summarise(obs_avg = mean(obs),
              obs_sd  = mean(obs),
              obs_n   = n())
  )

  # df_B1_density$bigD13C |> filter(sitename == "lon_-111.80_lat_+040.77") |> filter(posterior_sample_id==1, y_facet == "Posterior", Scenario ==4) |> View()

  # df_B1and2and3_avgObs <- df_B1and2and3
  # df_B1and2and3_avgObs$gpp
  # df_B1and2and3_avgObs$vj |> filter()
  # df_B1and2and3_avgObs$bigD13C
  # df_B1_density_avgObs    <- lapply(df_B1and2and3_avgObs, \(df) df |> filter(!is.na(obs)))                    # remove NA observations
  # df_B2_scatter_avgObs    <- lapply(df_B1and2and3_avgObs, \(df) df |> filter(!is.na(obs), dataset == "test")) # remove NA observations and test data set
  # df_B3_timeseries_avgObs <- df_B1and2and3_avgObs["gpp"]                                                      # keep NA observations and test data set, only use gpp


  ## Figure B2: pred-vs-obs scatter plot ----
  ## for each scenario x target x test
  lims_bigD13C <- quantile(filter(df_B2_scatter$bigD13C, model_output_type == "rsofun")$modelled, c(0.01, 0.99))
  lims_bigD13C <- quantile(filter(df_B2_scatter$bigD13C, model_output_type == "bias-corrected")$modelled, c(0.01, 0.99))
  pl_scatter_bigD13C <- ggplot(df_B2_scatter$bigD13C, aes(x = modelled, y = obs)) +
    geom_hex(bins = 50, show.legend = FALSE) +
    # layout:
    # facet_grid(parameters+model_output_type ~ Scenario+dataset, labeller = labeller("Scenario" = label_both)) +
    facet_grid(y_facet ~ Scenario+dataset, labeller = labeller("Scenario" = label_both)) +
    khroma::scale_fill_batlowW(trans = "log", reverse = TRUE) +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
    labs(x=expression(paste("Predicted Δ13C (permil)")),
         y=expression(paste("Observed Δ13C (permil)"))) +
    # coord_fixed() +
    # coord_cartesian(xlim = lims_bigD13C) +
    # ylim(lims_bigD13C[[1]], lims_bigD13C[[2]]) +
    theme_classic() +
    theme(legend.position = "bottom")

  # lims_vj <- quantile(filter(df_B2_scatter$vj, target == "vj", model_output_type == "mod_biasremoved_no_err")$modelled, c(0.01, 0.99))
  pl_scatter_vj <- ggplot(data = df_B2_scatter$vj, aes(x = modelled, y = obs)) +
    geom_hex(bins = 50, show.legend = FALSE) +
    # layout:
    # facet_grid(parameters+model_output_type ~ Scenario+dataset, labeller = labeller("Scenario" = label_both)) +
    facet_grid(y_facet ~ Scenario+dataset, labeller = labeller("Scenario" = label_both)) +
    khroma::scale_fill_batlowW(trans = "log", reverse = TRUE) +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
    labs(x=expression(paste("Predicted Vcmax/Jmax (-)")),
         y=expression(paste("Observed Vcmax/Jmax (-)"))) +
    # coord_fixed() +
    # coord_cartesian(xlim = lims_vj) +
    # ylim(lims_vj[[1]], lims_vj[[2]]) +
    theme_classic() +
    theme(legend.position = "bottom")

  pl_scatter_gpp <- ggplot(data = df_B2_scatter$gpp, aes(x = modelled, y = obs)) +
    geom_hex(bins = 50, show.legend = FALSE) +
    # layout:
    # facet_grid(parameters+model_output_type ~ Scenario+dataset, labeller = labeller("Scenario" = label_both)) +
    facet_grid(y_facet ~ Scenario+dataset, labeller = labeller("Scenario" = label_both)) +
    khroma::scale_fill_batlowW(trans = "log", reverse = TRUE) +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
    labs(x=expression(paste("Predicted GPP (g C m"^-2, "s"^-1, ")")),
         y=expression(paste("Observed GPP (g C m"^-2, "s"^-1, ")"))) +
    coord_fixed() +
    # coord_cartesian(xlim = c(0, lims)) +
    # ylim(0, lims) +
    theme_classic() +
    theme(legend.position = "bottom")

  # combined
  pl_scatter_all <- cowplot::plot_grid(pl_scatter_gpp, pl_scatter_vj, pl_scatter_bigD13C, nrow = 1)
  ggsave_and_return(pl_scatter_all,
                    paste0("fig_B2_pred-vs-obs_s1_s4_pred",outfname_suffix,".png"),
                    width = 12, height = 6, units = "cm", scale = 2)
  pl_scatter_gpp_bysite_test <- pl_scatter_gpp %+% (pl_scatter_gpp$data |> filter(y_facet == "Posterior", Scenario %in% c("4","3"))) +
    facet_wrap(~sitename+dataset, ncol=13)
  pl_scatter_gpp_bysite_train<- pl_scatter_gpp %+% (df_B1and2and3$gpp |> filter(!is.na(obs), dataset =="train") |>
                        filter(y_facet == "Posterior", Scenario %in% c("4","3"))) +
    facet_wrap(~sitename+dataset, ncol=6)
  ggsave_and_return(pl_scatter_gpp_bysite_test,
                    paste0("fig_B2b_pred-vs-obs_s4_test-sites",outfname_suffix,".png"),
                    width = 12, height = 10, units = "cm", scale = 2)
  ggsave_and_return(pl_scatter_gpp_bysite_train,
                    paste0("fig_B2b_pred-vs-obs_s4_train-sites",outfname_suffix,".png"),
                    width = 12, height = 10, units = "cm", scale = 2)


            # individual
            # ggsave_and_return(pl_scatter_gpp,     "fig_B2_pred-vs-obs_s1-to-s4_gpp.png",     width = 12, height = 8, units = "cm", scale = 2)
            # ggsave_and_return(pl_scatter_vj,      "fig_B2_pred-vs-obs_s1-to-s4_vj.png",      width = 12, height = 8, units = "cm", scale = 2)
            # ggsave_and_return(pl_scatter_bigD13C, "fig_B2_pred-vs-obs_s1-to-s4_bigD13C.png", width = 12, height = 8, units = "cm", scale = 2)

            # ggsave_and_return(pl_scatter_gpp     %+% filter(pl_scatter_gpp$data,     dataset == "test", parameters == "MAP"), "fig_B2_pred-vs-obs_s1-to-s4_MAP_gpp.png",     width = 12, height = 8, units = "cm", scale = 2)
            # ggsave_and_return(pl_scatter_vj      %+% filter(pl_scatter_vj$data,      dataset == "test", parameters == "MAP"), "fig_B2_pred-vs-obs_s1-to-s4_MAP_vj.png",      width = 12, height = 8, units = "cm", scale = 2)
            # ggsave_and_return(pl_scatter_bigD13C %+% filter(pl_scatter_bigD13C$data, dataset == "test", parameters == "MAP"), "fig_B2_pred-vs-obs_s1-to-s4_MAP_bigD13C.png", width = 12, height = 8, units = "cm", scale = 2)

            # pl_scatter_S1S4_testonly <- cowplot::plot_grid(
            #   pl_scatter_gpp %+% filter(pl_scatter_gpp$data, dataset == "test", !(Scenario %in% c("2","3"))),
            #   pl_scatter_vj %+% filter(pl_scatter_vj$data, dataset == "test", !(Scenario %in% c("2","3"))),
            #   pl_scatter_bigD13C %+% filter(pl_scatter_bigD13C$data, dataset == "test", !(Scenario %in% c("2","3"))),
            #   nrow = 1)
            # ggsave_and_return(pl_scatter_S1S4_testonly,
            #                   "fig_B2_test_pred-vs-obs_s1_s4.png",   # here::here("fig","fig_B2_pred-vs-obs_s1_s4.png"),
            #                   width = 12, height = 6, units = "cm", scale = 3)
            #
            # pl_scatter_S1S4 <- cowplot::plot_grid(
            #   pl_scatter_gpp %+% filter(pl_scatter_gpp$data, !(Scenario %in% c("2","3"))),
            #   pl_scatter_vj  %+% filter(pl_scatter_vj$data, !(Scenario %in% c("2","3"))),
            #   pl_scatter_bigD13C %+% filter(pl_scatter_bigD13C$data, !(Scenario %in% c("2","3"))),
            #   nrow = 1)
            # ggsave_and_return(pl_scatter_S1S4,
            #                   "fig_B2_both_pred-vs-obs_s1_s4.png",   # here::here("fig","fig_B2_pred-vs-obs_s1_s4.png"),
            #                   width = 12, height = 6, units = "cm", scale = 3)


  ### REDO SCATTER FOR AVG OBS

  ## for each scenario x target x test
  pl_scatter_bigD13C <- ggplot(df_B2_scatter_avgObs$bigD13C, aes(x = modelled, y = obs_avg)) +
    geom_hex(bins = 50, show.legend = FALSE) +
    # layout:
    # facet_grid(parameters+model_output_type ~ Scenario+dataset, labeller = labeller("Scenario" = label_both)) +
    facet_grid(y_facet ~ Scenario+dataset, labeller = labeller("Scenario" = label_both)) +
    khroma::scale_fill_batlowW(trans = "log", reverse = TRUE) +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
    labs(x=expression(paste("Predicted Δ13C (permil)")),
         y=expression(paste("Avg. Observed Δ13C (permil)"))) +
    # coord_fixed() +
    # coord_cartesian(xlim = lims_bigD13C) +
    # ylim(lims_bigD13C[[1]], lims_bigD13C[[2]]) +
    theme_classic() +
    theme(legend.position = "bottom")

  pl_scatter_vj <- ggplot(data = df_B2_scatter_avgObs$vj, aes(x = modelled, y = obs_avg)) +
    geom_hex(bins = 50, show.legend = FALSE) +
    # layout:
    # facet_grid(parameters+model_output_type ~ Scenario+dataset, labeller = labeller("Scenario" = label_both)) +
    facet_grid(y_facet ~ Scenario+dataset, labeller = labeller("Scenario" = label_both)) +
    khroma::scale_fill_batlowW(trans = "log", reverse = TRUE) +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
    labs(x=expression(paste("Predicted Vcmax/Jmax (-)")),
         y=expression(paste("Avg. Observed Vcmax/Jmax (-)"))) +
    # coord_fixed() +
    # coord_cartesian(xlim = lims_vj) +
    # ylim(lims_vj[[1]], lims_vj[[2]]) +
    theme_classic() +
    theme(legend.position = "bottom")

  pl_scatter_gpp <- ggplot(data = df_B2_scatter_avgObs$gpp, aes(x = modelled, y = obs_avg)) +
    geom_hex(bins = 50, show.legend = FALSE) +
    # layout:
    # facet_grid(parameters+model_output_type ~ Scenario+dataset, labeller = labeller("Scenario" = label_both)) +
    facet_grid(y_facet ~ Scenario+dataset, labeller = labeller("Scenario" = label_both)) +
    khroma::scale_fill_batlowW(trans = "log", reverse = TRUE) +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
    labs(x=expression(paste("Predicted GPP (g C m"^-2, "s"^-1, ")")),
         y=expression(paste("Avg. Observed GPP (g C m"^-2, "s"^-1, ")"))) +
    coord_fixed() +
    # coord_cartesian(xlim = c(0, lims)) +
    # ylim(0, lims) +
    theme_classic() +
    theme(legend.position = "bottom")

  # combined
  pl_scatter_all <- cowplot::plot_grid(pl_scatter_gpp, pl_scatter_vj, pl_scatter_bigD13C, nrow = 1)
  ggsave_and_return(pl_scatter_all,
                    paste0("fig_B2_pred-vs-avgObs2_s1_s4_pred",outfname_suffix,".png"),
                    width = 12, height = 6, units = "cm", scale = 2)

  pl_scatter_gpp_bysite_test <- pl_scatter_gpp %+% (pl_scatter_gpp$data |> filter(y_facet == "Posterior", Scenario %in% c("4","3"))) +
    facet_wrap(~sitename+dataset, ncol=13)
  # pl_scatter_gpp_bysite_train<- pl_scatter_gpp %+% (df_B1and2and3$gpp |> filter(!is.na(obs_avg), dataset =="train") |>
  #                       filter(y_facet == "Posterior", Scenario %in% c("4","3"))) +
  #   facet_wrap(~sitename+dataset, ncol=6)
  ggsave_and_return(pl_scatter_gpp_bysite_test,
                    paste0("fig_B2b_pred-vs-avgObs2_s4_test-sites",outfname_suffix,".png"),
                    width = 12, height = 10, units = "cm", scale = 2)
  # ggsave_and_return(pl_scatter_gpp_bysite_train,
  #                   paste0("fig_B2b_pred-vs-avgObs2_s4_train-sites",outfname_suffix,".png"),
  #                   width = 12, height = 10, units = "cm", scale = 2)
  ### END REDO SCATTER FOR AVG OBS




  ## Figure B: error distribution density plot ----
  ## for each scenario x target x test+train
  # Plot error as as second density plots (no-fill, dashed lines)
  pl_density_alltargets <- ggplot(
      bind_rows(df_B1_density$vj, df_B1_density$bigD13C, df_B1_density$gpp) |>
        mutate(target = factor(target, levels = c("gpp","vj","bigD13C"))),
      aes(x = modelled - obs, y = interaction(dataset, Scenario))) +
    # add Posterior (fill):
    ggridges::geom_density_ridges( data = function(df) {df |> filter(y_facet == "Posterior")},
      mapping = aes(fill = dataset), # linetype = dataset
      scale = 0.8) +
    # add error (solid):
    ggridges::geom_density_ridges( data = function(df) {df |> filter(y_facet == "Post.+Error")},
      mapping = aes(color = dataset, linetype = "Post.+Error"),
      scale = 0.8, fill = NA, key_glyph = "timeseries") + # "polygon"
    # # add MAP (dashed):
    # ggridges::geom_density_ridges( data = function(df) {df |> filter(y_facet == "MAP")},
    #   mapping = aes(color = dataset, linetype = "MAP"),
    #   scale = 0.8, fill = NA, key_glyph = "timeseries") + # "polygon"
    # layout:
    facet_grid(y_facet ~ target+Scenario+dataset, labeller = labeller("Scenario" = label_both)) +
    labs(x=expression(paste("Predicted - Observed"))) +
    scale_fill_manual(NULL, aesthetics = c("fill", "colour"),
                      values = c("test"="#29a274ff",
                                 "train" = t_col("#777055ff"))) +
    scale_linetype_manual(NULL, values = c("Post.+Error" = "3313",
                                           "MAP"         = "3232",
                                           "fixed"       = "solid")) +
    theme_classic() +
    theme(legend.position        = "inside",
          legend.position.inside = c(0.02,0.02),
          legend.justification   = c(0,0),
          legend.direction       = "vertical",
          legend.box             = "horizontal",
          legend.background      = element_blank()) +
    theme(panel.grid.minor.x = element_line()) +
    scale_x_continuous(minor_breaks = 0.00001) + # 0 makes it disappear
    labs(linetype=NULL)
  # pl_density_alltargets
  # pl_density_alltargets + facet_grid(~Scenario+dataset)
  # pl_density_alltargets + facet_null()

  # pl_density_alltargets_v2 <- pl_density_alltargets +
  #   aes(y=dataset) + labs(y=NULL) +
  #   facet_grid(Scenario~target, labeller = labeller("Scenario" = label_both), scales = "free_x") +
  #   scale_y_discrete(limits = rev)

  dat_to_plot <- bind_rows(
      df_B1_density$vj,
      df_B1_density$bigD13C,
      df_B1_density$gpp
    ) |>
    mutate(target = factor(target, levels = c("gpp","vj","bigD13C")))

  dat_to_plot_avgObs <- bind_rows(
      df_B1_density_avgObs$gpp |> select(names(df_B1_density_avgObs$bigD13C)),
      df_B1_density_avgObs$vj,
      df_B1_density_avgObs$bigD13C
    ) |>
    mutate(target = factor(target, levels = c("gpp","vj","bigD13C")))

  pl_density_alltargets_v3 <- ggplot(dat_to_plot, aes(x = modelled - obs, y = Scenario)) +
    scale_y_discrete(limits = rev) +
    # add Posterior (fill):
    ggridges::geom_density_ridges( data = function(df) {df |> filter(y_facet == "Posterior")},
      mapping = aes(fill = dataset), # linetype = dataset
      scale = 0.8) +
    # add error (solid):
    ggridges::geom_density_ridges( data = function(df) {df |> filter(y_facet == "Post.+Error")},
      mapping = aes(color = dataset, linetype = "Post.+Error"),
      scale = 0.8, fill = NA, key_glyph = "timeseries") + # "polygon" or "timeseries"
    # # add MAP (dashed):
    # ggridges::geom_density_ridges( data = function(df) {df |> filter(y_facet == "MAP")},
    #   mapping = aes(color = dataset, linetype = "MAP"),
    #   scale = 0.8, fill = NA, key_glyph = "timeseries") + # "polygon" or "timeseries"
    # layout:
    scale_fill_manual(NULL, aesthetics = c("fill", "colour"), values = c("test"="#29a274ff", "train" = t_col("#777055ff"))) +
    scale_linetype_manual(NULL, values = c("Post.+Error" = "3313", "MAP"         = "3232", "fixed"       = "solid")) +
    # theme:
    theme_classic() +
    theme(legend.position        = "inside",
          legend.position.inside = c(0.02,0.02),
          legend.justification   = c(0,0),
          legend.direction       = "vertical",
          legend.box             = "horizontal",
          legend.background      = element_blank()) +
    # add line at 0:
    scale_x_continuous(minor_breaks = 0.00001) + # 0 makes it disappear
    theme(panel.grid.minor.x = element_line()) +
    # axis labels and facet grid labels
    labs(x = "Predicted - Observed", linetype = NULL) +
    facet_grid(
      ~target,
      scales = "free_x",
      labeller = as_labeller(c("gpp"     = "(a) GPP:",
                               "vj"      = "(b) Vcmax/Jmax:",
                               "bigD13C" = "(c) Δ13C:"))) +
    theme(strip.background = element_blank(),
          strip.text       = element_text(hjust = 0, size = 12, face = "bold"))


  ggsave(here::here(file.path("fig",paste0("fig_B_predObs_errorDensity_s1s4",outfname_suffix,".png"))),
         plot = pl_density_alltargets_v3, width=12, height=8, units = "cm", scale = 1.3)

  # redoc plot versus obs_avg instead of obs
  pl_density_alltargets_v3_avgObs <- (pl_density_alltargets_v3 %+% dat_to_plot_avgObs) +
    aes(x = modelled - obs_avg, y = Scenario) +
    labs(x = "Predicted - Avg. Observed")
  ggsave(here::here(file.path("fig",paste0("fig_Bb_predObs_errorDensity_s1s4",outfname_suffix,".png"))),
         plot = pl_density_alltargets_v3_avgObs, width=12, height=8, units = "cm", scale = 1.3)

  # and combine both, arranging axes:
  # Extend x-axis limits of pl_density_alltargets_v3_avgObs to be the same as pl_density_alltargets_v3
  # by using a geom_blank() layer (source: https://stackoverflow.com/a/21585521/3915004) :
  pl_density_alltargets_v3_build <- ggplot2::ggplot_build(pl_density_alltargets_v3)
  dummy <- data.frame(
    target = c("gpp", "gpp",
               "vj_obs__", "vj_obs__",
               "bigD13C_obs_permil", "bigD13C_obs_permil") |>
      factor(levels = c("gpp", "vj_obs__", "bigD13C_obs_permil")),
    x      = c(pl_density_alltargets_v3_build$layout$get_scales(1)$x$range$range,
               pl_density_alltargets_v3_build$layout$get_scales(2)$x$range$range,
               pl_density_alltargets_v3_build$layout$get_scales(3)$x$range$range),
    y = 1
  )
  pl_density_alltargets_v3_avgObs_xlimsExtended <- pl_density_alltargets_v3_avgObs + geom_blank(data = dummy, aes(x=x, y=y))
  pl_density_alltargets_v3_comparison <- cowplot::plot_grid(
    pl_density_alltargets_v3,
    pl_density_alltargets_v3_avgObs_xlimsExtended +
      facet_grid( ~target, scales = "free_x",
      labeller = as_labeller(c("gpp"     = "(d) GPP:",
                               "vj"      = "(e) Vcmax/Jmax:",
                               "bigD13C" = "(f) Δ13C:"))),
    ncol = 1, rel_heights = c(1,1))
  ggsave(here::here(file.path("fig",paste0("fig_Bbb_predObs_errorDensity_s1s4",outfname_suffix,".png"))),
       plot = pl_density_alltargets_v3_comparison, width=12, height=16, units = "cm", scale = 1.3)



            # pl_density_alltargets_v4 <- ggplot(data = bind_rows(df_B1_density$vj, df_B1_density$bigD13C, df_B1_density$gpp) |>
            #                     mutate(target = factor(target, levels = c("gpp","vj","bigD13C"))),
            #                   mapping = aes(y = Scenario)) +
            #   # add Posterior (fill):
            #   ggridges::geom_density_ridges(mapping = aes(x=modelled - obs, fill = dataset), data = function(df) {df |> filter(y_facet == "Posterior")},
            #                                 scale = 1.3) +
            #   ggridges::geom_density_ridges(mapping = aes(x=modelled - obs, color = dataset), data = function(df) {df |> filter(y_facet == "Post.+Error")},
            #                                 scale = 1.3, fill = NA, linetype = "dashed") +
            #   # layout:
            #   theme_classic() +
            #   # theme(legend.position = "bottom") +
            #   # theme(legend.position = "inside", legend.position.inside = c(0.02,0.98), legend.justification = c(0,1)) +
            #   theme(legend.position = "inside", legend.position.inside = c(0.02,0.02), legend.justification = c(0,0), legend.direction = "vertical", legend.box = "horizontal",
            #         legend.background = element_blank()) +
            #   labs(x="Predicted - Observed") +
            #   scale_y_discrete(limits = rev) +
            #   scale_fill_manual(NULL,aesthetics = c("colour","fill"), values = c(test = "#29a274ff", train = t_col("#777055ff"))) + # GECO colors
            #   facet_wrap( ~ target , nrow = 1, scales = "free_x",
            #               labeller = as_labeller(c("gpp"="(a) GPP:", "vj"="(b) Vcmax/Jmax:", "bigD13C"="(c) Δ13C:"))) +
            #   theme(strip.background = element_blank(), strip.text = element_text(hjust = 0, size = 12, face = "bold")) +
            #   theme(panel.grid.minor.x = element_line()) + scale_x_continuous(minor_breaks = 0.00001) # 0 makes it disappear
            #
            # ggsave(here::here(file.path("fig","fig_Ba_predObs_errorDensity_s1s4.png")),
            #        plot = pl_density_alltargets_v4, width=12, height=8, units = "cm", scale = 1.3)



  # Figure C2: plot of intra-site spread of observations ----
  # NOTE that this spread we won't be able to model
  res_s3 <- setup_rsofun_calibration(scenario = 3) # NOTE: this must remain scenario 3 since it has all the data...
  site_info <- bind_rows(
    res_s3$drivobs_train |> mutate(set = "train"),
    res_s3$drivobs_test |> mutate(set = "test")
  ) |> unnest(site_info) |>
    unnest_wider(targets)

  obs_df <- site_info |>
    filter(run_model == "onestep") |>
    select(-run_model, -params_siml, -forcing,
           -vj, -bigD13C, -gpp) |>
    select(sitename, lon, lat, elv, set, data) |> # TODO: remove this line again for covariates
    unnest(data)

  obs_df_bigD13C <- obs_df |> select(sitename, lon, lat, elv, set, bigD13C) |> unnest(bigD13C)
  obs_df_vj      <- obs_df |> select(sitename, lon, lat, elv, set, vj     ) |> unnest(vj     )
  obs_df_gpp <- site_info |>
    filter(run_model == "daily") |>
    select(-run_model, -params_siml, -forcing,
           -vj, -bigD13C, -gpp) |>
    select(sitename, lon, lat, elv, set, data) |> # TODO: remove this line again for covariates
    unnest(data) |> select(sitename, lon, lat, elv, set, date, gpp)

  # make a "model"-prediction by computing the average of each site's observation
  obs_df_bigD13C  <- obs_df_bigD13C |> group_by(sitename) |> mutate(avgObs = mean(bigD13C_obs_permil), sdObs = sd(bigD13C_obs_permil))
  obs_df_vj       <- obs_df_vj      |> group_by(sitename) |> mutate(avgObs = mean(vj_obs__)          , sdObs = sd(vj_obs__)          )
  obs_df_gpp      <- obs_df_gpp     |> group_by(sitename,date) |> mutate(avgObs = mean(gpp)          , sdObs = sd(gpp)               ) |>
    # this is just to reduce the number of points: (since anyway we only have one obs per day, avgObs==gpp
    group_by(sitename, lon, lat, elv, set) |> summarise(avgObs = mean(avgObs), gpp = mean(gpp))

  # obs_df_bigD13C |> arrange(-sdObs) |> mutate(sitename = forcats::as_factor(sitename)) |> ggplot(aes(y=as.numeric(sitename), x=bigD13C_obs_permil         )) + geom_point()
  # obs_df_vj      |> arrange(-sdObs) |> mutate(sitename = forcats::as_factor(sitename)) |> ggplot(aes(y=as.numeric(sitename), x=vj_obs__                   )) + geom_point()
  # obs_df_bigD13C |> arrange(sdObs) |> mutate(sitename = forcats::as_factor(sitename)) |> ggplot(aes(y=as.numeric(sitename), x=bigD13C_obs_permil - avgObs)) + geom_point() + labs("Site mean - Observed")
  # obs_df_vj      |> arrange(sdObs) |> mutate(sitename = forcats::as_factor(sitename)) |> ggplot(aes(y=as.numeric(sitename), x=vj_obs__           - avgObs)) + geom_point() + labs("Site mean - Observed")
  # obs_df_bigD13C |> arrange(-sdObs) |> mutate(sitename = forcats::as_factor(sitename)) |> ggplot(aes(x=bigD13C_obs_permil - avgObs)) + geom_density() + labs("Site mean - Observed")
  # obs_df_vj      |> arrange(-sdObs) |> mutate(sitename = forcats::as_factor(sitename)) |> ggplot(aes(x=vj_obs__           - avgObs)) + geom_density() + labs("Site mean - Observed")


  df_B1_density_obs <- bind_rows(
    obs_df_bigD13C |> ungroup() |> pivot_longer(bigD13C_obs_permil, names_to = "target", values_to = "obs") |> arrange(set, -lat) |> mutate(sitename = forcats::as_factor(sitename))|>mutate(site_nr_for_target = as.integer(sitename)),
    obs_df_vj      |> ungroup() |> pivot_longer(vj_obs__          , names_to = "target", values_to = "obs") |> arrange(set, -lat) |> mutate(sitename = forcats::as_factor(sitename))|>mutate(site_nr_for_target = as.integer(sitename)),
    obs_df_gpp     |> ungroup() |> pivot_longer(gpp               , names_to = "target", values_to = "obs") |> arrange(set, -lat) |> mutate(sitename = forcats::as_factor(sitename))|>mutate(site_nr_for_target = as.integer(sitename))
  ) |> mutate(target = factor(target, levels = c("gpp", "vj_obs__", "bigD13C_obs_permil")))

  pl_density_obs <- ggplot(df_B1_density_obs, aes(x=avgObs - obs, y = site_nr_for_target)) +
    geom_point(aes(color = set)) +
    # layout:
    theme_classic() +
    facet_grid(~target, scales = "free_x", drop = FALSE,
               labeller = as_labeller(c("gpp"="(d) GPP:", "vj_obs__"="(e) Vcmax/Jmax:", "bigD13C_obs_permil"="(f) Δ13C:"))) +
    theme(strip.background = element_blank(), strip.text = element_text(hjust = 0, size = 12, face = "bold")) +
    labs(x=expression(paste("Site mean - Observed")), y = "Site number") +
    scale_fill_manual(NULL, aesthetics = c("fill", "colour"),
                      values = c("test"  = t_col("#29a274ff"),
                                 "train" = t_col("#777055ff"))) +
    theme(legend.position        = "inside",
          legend.position.inside = c(0.02,0.02),
          legend.justification   = c(0,0),
          legend.direction       = "vertical",
          legend.box             = "horizontal",
          legend.background      = element_blank()) +
    theme(panel.grid.minor.x = element_line()) +
    scale_x_continuous(minor_breaks = 0.00001) + # 0 makes it disappear
    labs(linetype=NULL)
  pl_density_obs

  # Extend x-axis limits of pl_density_obs to be the same as pl_density_alltargets_v3:

  pl_density_alltargets_v3_build <- ggplot2::ggplot_build(pl_density_alltargets_v3)
  # pl_density_alltargets_v3_build$layout$panel_scales_x
  # pl_density_alltargets_v3_build$layout$get_scales(1)$x$range$range
  # pl_density_alltargets_v3_build$layout$get_scales(2)$x$range$range
  # pl_density_alltargets_v3_build$layout$get_scales(3)$x$range$range
  dummy <- data.frame( # approach:  https://stackoverflow.com/a/21585521/3915004
    target = c("gpp", "gpp", "vj_obs__", "vj_obs__", "bigD13C_obs_permil", "bigD13C_obs_permil") |> factor(levels = c("gpp", "vj_obs__", "bigD13C_obs_permil")),
    x      = c(pl_density_alltargets_v3_build$layout$get_scales(1)$x$range$range,
               pl_density_alltargets_v3_build$layout$get_scales(2)$x$range$range,
               pl_density_alltargets_v3_build$layout$get_scales(3)$x$range$range),
    y = 1
  )
  pl_density_obs_xlimsExtended <- pl_density_obs + geom_blank(data = dummy, aes(x=x, y=y))
  pl_density_alltargets_v3_withObs <- cowplot::plot_grid(
    pl_density_alltargets_v3,
    pl_density_obs_xlimsExtended,
    ncol = 1, rel_heights = c(2,1))
  ggsave(here::here(file.path("fig",paste0("fig_B1_predObs_errorDensity_s1s4",outfname_suffix,".png"))),
       plot = pl_density_alltargets_v3_withObs, width=12, height=12, units = "cm", scale = 1.3)




  ggsave(here::here(file.path("fig",paste0("fig_B1b_predObs_errorDensity_s1s4",outfname_suffix,".png"))),
       plot = pl_density_alltargets_v3_withObs, width=12, height=12, units = "cm", scale = 1.3)


  ## Figure B3: make a proper gpp time series plot ----
  plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario == "1",          dataset == "train"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s1_train",outfname_suffix,".png")))
  plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario %in% c("4","3"), dataset == "train"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s4or3_train",outfname_suffix,".png")))

  plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario == "1",          dataset == "test"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s1_test",outfname_suffix,".png")))
  plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario %in% c("4","3"), dataset == "test"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s4or3_test",outfname_suffix,".png")))

  plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario == "0", dataset == "train"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s0_train",outfname_suffix,".png")))
  plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario == "0", dataset == "test"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s0_test",outfname_suffix,".png")))
}

if (FALSE) { # alternative plotting codes (manual)
  ## Figure B2: pred-vs-obs scatter plot ----
  ## for each scenario x target x test
  # alternative manual approach
          # plot_predobs_gpp_scatter <- function(df_predict){
          #   df_hexplot_gpp <- df_predict |> unnest(sim) |> filter(!is.na(obs)) |> filter(target == "gpp")
          #
          #   lims <- round(max(quantile(df_hexplot_gpp$mod_no_err, 0.9999), quantile(df_hexplot_gpp$obs, 0.9999)))
          #   if (nrow(df_hexplot_gpp)>0){
          #     gg <- ggplot(df_hexplot_gpp, aes(x=mod_no_err, y=obs)) +
          #       geom_hex(bins = 50, show.legend = FALSE) +
          #       facet_wrap(~target) +
          #       geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
          #       coord_fixed() +
          #       xlim(0, lims) +
          #       ylim(0, lims) +
          #       theme_classic() +
          #       khroma::scale_fill_batlowW(trans = "log", reverse = TRUE) +
          #       facet_wrap(~sitename)
          #     # khroma::scale_fill_davos(trans = "log", reverse = TRUE)
          #   } else {
          #     gg <- ggplot(tibble(sitename = NA_character_, mod_no_err=NA,obs=NA), aes(x=mod_no_err,y=obs)) +
          #       facet_wrap(~sitename)
          #   }
          # }
          #
          # ## vj and D13C
          # plot_predobs_vj_D13C_scatter <- function(df_predict, target_selection = c("bigD13C","vj")){
          #   df_hexplot      <- df_predict |> unnest(sim) |> filter(!is.na(obs)) |> filter(target %in% target_selection)
          #   if (nrow(df_hexplot)>0){
          #     gg <- ggplot(df_hexplot, aes(x=mod_no_err, y=obs)) +
          #       geom_hex(bins = 50, show.legend = FALSE)
          #   } else {
          #     gg <- ggplot(tibble(target = target_selection, mod_no_err=NA,obs=NA), aes(x=mod_no_err,y=obs))
          #   }
          #   gg <- gg +
          #     geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
          #     theme_classic() +
          #     khroma::scale_fill_batlowW(trans = "log", reverse = TRUE)
          #
          #   if (length(target_selection) == 1){
          #     lims_max <- max(quantile(df_hexplot$mod_no_err, 0.9999), quantile(df_hexplot$obs, 0.9999))
          #     lims_min <- min(quantile(df_hexplot$mod_no_err, 0.0001), quantile(df_hexplot$obs, 0.0001))
          #     gg <- gg + facet_wrap(~target, ncol=1) +
          #       coord_fixed() + xlim(lims_min, lims_max) + ylim(lims_min, lims_max)
          #   } else {
          #     gg <- gg + facet_wrap(~target, scales = "free", ncol=1)
          #   }
          # }
          #
          # plot_all_predVsObs <- function(df_predict, rel_widths = c(5,2)){
          #   scatter_plot_gpp     <- plot_predobs_gpp_scatter(df_predict)
          #   scatter_plot_D13C    <- plot_predobs_vj_D13C_scatter(df_predict, target_selection = "bigD13C")
          #   scatter_plot_vj      <- plot_predobs_vj_D13C_scatter(df_predict, target_selection = "vj")
          #
          #   # arrange layouts:
          #   # testcode with dummy plots:
          #   # scatter_plot_gpp  <- ggplot(tibble(facet=rep(1:12,10)) |> mutate(x= runif(n()), y=runif(n())), aes(x=x,y=y)) + geom_point() + coord_fixed() + facet_wrap(~facet) + theme_classic()
          #   # plot_right <- ggplot(tibble(facet=rep(c("vj","bigD13C"),10)) |> mutate(x= runif(n(),10,12), y=runif(n(),25,30)), aes(x=x,y=y)) + geom_point() + facet_wrap(~facet, scales = "free", ncol=1) + theme_classic()
          #   # ggsave_and_return(cowplot::plot_grid(plot_left + facet_wrap(~facet, labeller = as_labeller(~paste0(.x, ", GPP (gCm-2s-1)"))),
          #   #                                      plot_right + facet_wrap(~facet, scales = "free", ncol=1, labeller = as_labeller(c("vj"="Vcmax/Jmax (-)","bigD13C" = "Δ13C (permil)"))) + labs(y=NULL),
          #   #                                      ncol = 2, rel_widths = c(5,2)),
          #   #                   "fig_B6_pred-vs-obs_s14.png", width = 7.2, height = 4.2)
          #   # scatter_plot_gpp     <- scatter_plot_gpp     + facet_wrap(~sitename,                        labeller = as_labeller(~paste0(.x, ", GPP (gCm-2s-1)")))
          #   # scatter_plot_D13C_vj <- plot_predobs_vj_D13C_scatter(df_predict)
          #   # scatter_plot_D13C_vj <- scatter_plot_D13C_vj + facet_wrap(~target, scales = "free", ncol=1, labeller = as_labeller(c("vj"="Vcmax/Jmax (-)","bigD13C" = "Δ13C (permil)"))) + labs(y=NULL)
          #   # cowplot::plot_grid(scatter_plot_gpp, scatter_plot_D13C_vj, ncol = 2, rel_widths = rel_widths)
          #
          #   scatter_plot_gpp  <- scatter_plot_gpp  + labs(x=expression(paste("Predicted (with param. unc.) (g C m"^-2, "s"^-1, ")")),
          #                                                 y=expression(paste("Observed (g C m"^-2, "s"^-1, ")"))) +
          #     theme(strip.background = element_blank())# facet_wrap(~sitename,       labeller = as_labeller(~paste0(.x, ", GPP")))  +
          #   scatter_plot_D13C <- scatter_plot_D13C + labs(x="Predicted (with param. unc.) (permil)",  y="Observed (permil)") +
          #     facet_wrap(~target, ncol=1, labeller = as_labeller(c("bigD13C" = "Δ13C"))) + theme(strip.background = element_blank(), strip.text = element_text(colour = NA))
          #   scatter_plot_vj   <- scatter_plot_vj   + labs(x="Predicted (with param. unc.) (-)",       y="Observed (-)")      +
          #     facet_wrap(~target, ncol=1, labeller = as_labeller(c("vj"="Vcmax/Jmax")))  + theme(strip.background = element_blank(), strip.text = element_text(colour = NA))
          #
          #   tg_list <- cowplot::align_plots(scatter_plot_D13C, scatter_plot_vj)
          #   cowplot::plot_grid(
          #     scatter_plot_gpp,
          #     cowplot::plot_grid(plotlist = tg_list, ncol=1, labels = c("(b) Δ13C:","(c) Vcmax/Jmax:"), hjust = 0),
          #     ncol = 2, rel_widths = rel_widths, labels = c("(a) GPP:"), hjust = 0)
          #   # this layout with rel_widths c(5,2) should work for training plot (3x4)+(2x1) of size 7.2, 4.2, 300, 1.6
          #   # this layout with rel_widths c(5,2.3) should work for testing  plot (6x7)+(2x1) of size 7.2, 4.2*1.3, 300, 1.6
          # }
  ## Figure B: error distribution density plot ----
  ## for each scenario x target x test+train
  # Plot error as as second density plots (no-fill, dashed lines)

          # # alternative manual approach
          # make_plot <- function(df, xlab = NULL, crop_percentiles = c(0.01, 0.99)){
          #   # lims <- quantile(df$predBiasRemovedNoErr_minus_obs, crop_percentiles)
          #   lims <- quantile(df$predBiasRemovedWithErr_minus_obs, crop_percentiles)
          #   gg <- ggplot(df,
          #                aes(#x=predBiasedNoErr_minus_obs,
          #                  y = Scenario,
          #                  fill = dataset)) +
          #     # geom_density() + theme_classic() + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
          #     ggridges::geom_density_ridges(mapping = aes(x=predBiasRemovedNoErr_minus_obs, fill = dataset)) +
          #     ggridges::geom_density_ridges(mapping = aes(x=predBiasRemovedWithErr_minus_obs, color = dataset), fill = NA, linetype = "dashed") +
          #     theme_classic() +
          #     facet_wrap( ~ target, ncol = 1, scales = "free_x") + theme(strip.background = element_blank(), strip.text = element_text(colour = NA)) +
          #     labs(y = NULL, x = xlab, color = "Scenario", linetype = NULL) +
          #     coord_cartesian(xlim = lims) +
          #     # scico::scale_color_scico_d(NULL, palette = "batlow")
          #     scale_fill_manual(NULL,aesthetics = c("colour","fill"), values = c(test = "#29a274ff", train = t_col("#777055ff"))) # GECO colors
          #   if (nrow(df)==0){gg <- NULL}
          #   return(gg)
          # }
          #
          # dat_to_plot_gpp <- dat_to_plot_sampled |> filter(posterior_sample_id == 1) |> filter(target == "gpp")
          # dat_to_plot_vj  <- dat_to_plot_sampled |> filter(posterior_sample_id == 1) |> filter(target == "vj")
          # dat_to_plot_D13 <- dat_to_plot_sampled |> filter(posterior_sample_id == 1) |> filter(target == "bigD13C")
          #
          # gg_gpp <- make_plot(as_tibble(dat_to_plot_gpp), xlab = expression(paste("(g C m"^-2, "s"^-1, ")")))
          # gg_vj  <- make_plot(as_tibble(dat_to_plot_vj),  xlab = expression(paste("(-)")))
          # gg_D13 <- make_plot(as_tibble(dat_to_plot_D13), xlab = expression(paste("(permil)")))
          #
          # plotlist <- list(gg_gpp,gg_vj,gg_D13) |> lapply(\(pl)pl + theme(legend.position = "none")) #|> purrr::compact()
          # plotlist <- lapply(plotlist, \(pl)pl + coord_cartesian(ylim = c(1,5))) # homogenize y axis
          # legend <- cowplot::get_legend(plotlist[[1]] + theme(legend.position = "bottom"))
          # # plots  <- cowplot::plot_grid(
          # #   plotlist = c(plotlist, list(legend)),
          # #   labels = c("(a) GPP:", "(b) Vcmax/Jmax:", "(c) Δ13C:"), hjust = 0,
          # #   ncol = 1,
          # #   rel_heights = c(rep(1, length(plotlist)), (length(plotlist))/(6-1))) # make legend 1/4
          #
          # plots_horizontal  <- cowplot::plot_grid(
          #   plotlist = plotlist,
          #   labels = c("(a) GPP:", "(b) Vcmax/Jmax:", "(c) Δ13C:"), hjust = 0,
          #   nrow = 1)
          # plots_horizontal
          # # ggsave(here::here(file.path("fig","figB_predObs_errorDensity_s1s2s3s14.png")),
          # #        plot = plots, width=3.6, height=7.2, units = "in", scale = 1)
          # # ggsave(here::here(file.path("fig","figB_predObs_errorDensity_s1s2s3s14_test.png")),
          # #        plot = pl_bias, width=12, height=8, units = "cm", scale = 1.3)
}

########## SENSITIVITY ANALYSIS: ########### -

# Figure D: sensitivity bar plot ----
# plots are directly created by 02_sensitivity_analysis.R


########## GENERAL PLOTS: ########### -
# s3_output_to_analyze <- out_calib_s3_serial$mod
# s2_output_to_analyze <- out_calib_s2_serial$mod
# s1_output_to_analyze <- out_calib_s1_serial$mod

flag_plot_general <- FALSE # possibility to switch this off
if (flag_plot_general){

  source(here::here("R/calibration_helpers.R"))
  source(here::here("R/run_mcmc_rsofun.R"), echo = TRUE)

  res_s113 <- setup_rsofun_calibration(scenario = 113)

  # Figure C: map of sites ----
  ## for each targets x test+train
  site_info <- bind_rows(
    res_s113$drivobs_train |> mutate(set = "train"),
    res_s113$drivobs_test |> mutate(set = "test")
  ) |> unnest(site_info) |>
    unnest_wider(targets)


  pl1_train <- rgeco:::plot_map_simpl(dir_ne = tempdir()) +
    geom_point(size=0.1,shape = 20, color = "red",
               data    = site_info |> filter(gpp) |> filter(set == "train"),
               mapping = aes(lon, lat)) + ggtitle("GPP flux sites") + labs(caption = sprintf("Training set (n=%d)", site_info |> filter(gpp) |> filter(set == "train") |> nrow()))
  pl2_train <- rgeco:::plot_map_simpl(dir_ne = tempdir()) +
    geom_point(size=0.1,shape = 20, color = "red",
               data    = site_info |> filter(vj) |> filter(set == "train"),
               mapping = aes(lon, lat)) + ggtitle("Vcmax/Jmax sites") + labs(caption = sprintf("Training set (n=%d)", site_info |> filter(vj) |> filter(set == "train") |> nrow()))
  pl3_train <- rgeco:::plot_map_simpl(dir_ne = tempdir()) +
    geom_point(size=0.1,shape = 20, color = "red",
               data    = site_info |> filter(bigD13C) |> filter(set == "train"),
               mapping = aes(lon, lat)) + ggtitle("Δ13C sites") + labs(caption = sprintf("Training set (n=%d)", site_info |> filter(bigD13C) |> filter(set == "train") |> nrow()))

  pl1_test <- rgeco:::plot_map_simpl(dir_ne = tempdir()) +
    geom_point(size=0.1,shape = 20, color = "red",
               data    = site_info |> filter(gpp) |> filter(set == "test"),
               mapping = aes(lon, lat)) + ggtitle(NULL) + labs(caption = #"GPP flux sites",
                                                  sprintf("Test set (n=%d)", site_info |> filter(gpp) |> filter(set == "test") |> nrow()))
  pl2_test <- rgeco:::plot_map_simpl(dir_ne = tempdir()) +
    geom_point(size=0.1,shape = 20, color = "red",
               data    = site_info |> filter(vj) |> filter(set == "test"),
               mapping = aes(lon, lat)) + ggtitle(NULL) + labs(caption = #"Vcmax/Jmax sites",
                                                  sprintf("Test set (n=%d)", site_info |> filter(vj) |> filter(set == "test") |> nrow()))
  pl3_test <- rgeco:::plot_map_simpl(dir_ne = tempdir()) +
    geom_point(size=0.1,shape = 20, color = "red",
               data    = site_info |> filter(bigD13C) |> filter(set == "test"),
               mapping = aes(lon, lat)) + ggtitle(NULL) + labs(caption = #"Δ13C sites",
                                                  sprintf("Test set (n=%d)", site_info |> filter(bigD13C) |> filter(set == "test") |> nrow()))

  library(cowplot)
  remove_labels <- theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    title = element_text(size=10), plot.caption = element_text(size=8)
    )
  pl_sitemap <- cowplot::plot_grid(
    pl1_train + remove_labels, pl2_train + remove_labels, pl3_train + remove_labels,
    pl1_test + remove_labels, pl2_test + remove_labels, pl3_test + remove_labels,
    rel_heights = c(1.2,1),
    # labels = c("(a)","(b)","(c)",NULL,NULL,NULL),
    ncol = 3)

  ggsave(
    here::here("fig/fig_C_append_climate_MapTargetTrainingSites.png"),
    pl_sitemap, width=12, height=5, units="cm", dpi=300, scale = 1.3)

  # Table a: site table [lon, lat, elv, climate, vegtype, train-or-test, targets, Nobs] ----
  Defourny_LCCS_to_IGBP_vegtype <- function(df){
    df |>
      mutate(Defourny_LCCS_acr = case_when(
        # Defourny_LCCS == "Bare areas"                                                                           ~ "BSV",     # filterd out,
        # Defourny_LCCS == "Water bodies"                                                                         ~ NA,        # filterd out,
        # Defourny_LCCS == "Urban areas"                                                                          ~ "URB",     # filterd out
        # Defourny_LCCS == "Cropland, rainfed"                                                                    ~ NA,#"CRO", # filterd out
        # Defourny_LCCS == "Cropland, irrigated or post-flooding"                                                 ~ NA,#"CRO", # filterd out
        # Defourny_LCCS == "Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)"   ~ NA,#"CRO", # filterd out
        Defourny_LCCS == "Tree cover, needleleaved, evergreen, closed to open (>15%)"                           ~ "ENF",
        Defourny_LCCS == "Tree cover, broadleaved, deciduous, closed to open (>15%)"                            ~ "DBF",
        Defourny_LCCS == "Tree cover, needleleaved, deciduous, closed to open (>15%)"                           ~ "DNF",
        Defourny_LCCS == "Tree cover, broadleaved, evergreen, closed to open (>15%)"                            ~ "EBF",
        Defourny_LCCS == "Tree cover, mixed leaf type (broadleaved and needleleaved)"                           ~ "MF",
        Defourny_LCCS == "Sparse vegetation (tree, shrub, herbaceous cover) (<15%)"                             ~ "BSV", # source: https://www.cen.uni-hamburg.de/en/icdc/data/land/docs-land/igbp-designations-of-surface-types.pdf
        Defourny_LCCS == "Mosaic tree and shrub (>50%) / herbaceous cover (<50%)"                               ~ "WSA", # source: https://www.cen.uni-hamburg.de/en/icdc/data/land/docs-land/igbp-designations-of-surface-types.pdf
        Defourny_LCCS == "Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) /cropland (<50%)"    ~ "WSA", # source: https://www.cen.uni-hamburg.de/en/icdc/data/land/docs-land/igbp-designations-of-surface-types.pdf
        Defourny_LCCS == "Grassland"                                                                            ~ "GRA",
        Defourny_LCCS == "Shrubland"                                                                            ~ "CSH", # source: https://www.cen.uni-hamburg.de/en/icdc/data/land/docs-land/igbp-designations-of-surface-types.pdf
        TRUE ~ Defourny_LCCS
      )
      )
  }

  site_info |>
    Defourny_LCCS_to_IGBP_vegtype() |>
    select(FDK_igbp_land_use, Defourny_LCCS_acr) |>
    mutate(land_use = case_when(is.na(Defourny_LCCS_acr) ~ FDK_igbp_land_use,
                                TRUE ~ Defourny_LCCS_acr)) |>
    group_by(land_use) |> summarise(n())


  site_info_for_table_a <- site_info |>
    # homogenize vegtype and climate
    # climate Koeppen-Geiger:
    mutate(climate = case_when(gpp     ~ FDK_koeppen_code, # for GPP sites use FluxDataKit
                               vj      ~ Beck_KG,          # for other sites use Beck reference
                               bigD13C ~ Beck_KG,
                               TRUE    ~ NA_character_)) |>      # for other sites use Beck reference
    mutate(climate = stringr::str_to_title(climate)) |>
    # vegtype:
    Defourny_LCCS_to_IGBP_vegtype() |>
    mutate(igbp_vegtype = case_when(gpp     ~ FDK_igbp_land_use,
                                    vj      ~ Defourny_LCCS_acr,
                                    bigD13C ~ Defourny_LCCS_acr,
                                    TRUE    ~ NA_character_)) |>
    # format table
    arrange(-gpp, -vj, -bigD13C) |>
    mutate(target = paste(vj, bigD13C, gpp)) |>
    mutate(target = case_when(
      target == "FALSE FALSE TRUE" ~ "gpp",
      target == "FALSE TRUE TRUE"  ~ "gpp+bigD13C",
      target == "TRUE TRUE TRUE"   ~ "gpp+bigD13C+vj",

      target == "FALSE TRUE FALSE" ~ "bigD13C",
      target == "TRUE TRUE FALSE"  ~ "bigD13C+vj",

      target == "TRUE FALSE FALSE" ~ "vj",
      target == "FALSE FALSE FALSE" ~ "none",
      TRUE ~ NA_character_))

  count_obs <- function(target, nested_data_df){
    if (target == "gpp"){
      N <- nrow(nested_data_df) # no need to unnest
    } else if (target == "vj"){
      N <- nrow(unnest(nested_data_df, vj))
    } else if (target == "bigD13C+vj"){
      N <- sprintf("(%d, %d)",nrow(unnest(nested_data_df, bigD13C)), nrow(unnest(nested_data_df, vj)))
    } else if (target == "bigD13C"){
      N <- nrow(unnest(nested_data_df, bigD13C))
    } else {
      stop("Error")
    }
    return(as.character(N))
  }

  table_a <- site_info_for_table_a |>
    rowwise() |> mutate(Nrows = nrow(data)) |> mutate(Nobs = count_obs(target, data)) |>
    # select columns and order rows:
    mutate(target = factor(target, levels = c("gpp","vj","bigD13C+vj", "bigD13C")),
           set    = factor(set,    levels = c("train","test"))) |>
    arrange(set, target, igbp_vegtype) |>
    select(set, target, Nobs, sitename, climate, igbp_vegtype, lon, lat, elv) |>
    rename(
      "Data set" = set,
      "Target variable" = target,
      "N obs." = Nobs,
      "Site name" = sitename,
      "Climate" = climate,
      "Vegetation type" = igbp_vegtype,
      "Lon." = lon,
      "Lat." = lat,
      "Elev." = elv,
    )

  table_a %>%
    xtable::xtable(
      x = .,
      caption = "Listing of sites in training and testing data sets.",
      tabular.environment = "supertabular", floating = FALSE,
      align = rep("l", (ncol(x = .) + 1))  # make all columns left-aligned
    ) %>%
    print(x = .,
      hline.after = c(-1,0,nrow(.)),  caption.placement = "top",
      file = here::here("fig/table-a_site_list.tex"),
      include.rownames = FALSE
    ) # this can be added to tex file as: \input{filename.tex})


  # Table b: prior ranges of estimated params ----
  rsofun_symbol_parname_description <- tribble(
    ~Parameter,             ~Symbol_tex,                              ~Units_tex,                     ~Symbol_R,           ~Description,
    # MODEL PARAMETER:
    "kphio",                "$\\varphi_0$",                           "\\unit{mol\\,mol^{-1}}",       expression("TODO"),     "Quantum yield at optimal temperature" ,
    "kphio_par_a",          "$a$",                                    "\\unit{°C^{-2}}",              expression("TODO"),     "Shape parameter for the temperature dependence of the quantum yield" ,
    "kphio_par_b",          "$b$",                                    "\\unit{°C}",                   expression("TODO"),     "Optimal temperature for the quantum yield" ,
    "soilm_thetastar",      "$\\theta^*$",                            "\\unit{mm}",                   expression("TODO"),     "Threshold plant-available soil water content in the soil moisture stress function" ,
    "soilm_betao",          "$\\beta_0$",                             "unitless",                     expression("TODO"),     "Stress factor at low soil moisture, intercept for the soil moisture stress function" ,
    "beta_unitcostratio",   "$\\beta$",                               "unitless",                     expression("TODO"),     "Unit cost ratio of carboxylation (maintenance of $V_{\\mathrm{cmax}}$) to transpiration" ,
    "rd_to_vcmax",          "$b_0$",                                  "unitless",                     expression("TODO"),     "Ratio of ($R_{\\mathrm{d25}}$) to the maximum carboxylation rate $V_{\\mathrm{cmax}}$ (both temperature-normalised dark respiration; eq. C8 in Stocker et al. 2020)" ,
    "tau_acclim",           "$\\tau$",                                "days",                         expression("TODO"),     "Acclimation time scale of photosynthesis" ,
    "kc_jmax",              "$c^{*}$",                                "unitless",                     expression("TODO"),     "Unit cost of electron transport (maintenance of $J_{\\mathrm{max}}$)" ,
    # ERROR PARAMETER:
    "err_gpp",              "$\\epsilon_{\\mathrm{gpp}}$",            "\\unit{gC\\,m^{-2}\\,s^{-1}}", expression("TODO"),     "Gaussian error standard deviation of GPP" ,
    "err_bigD13C",          "$\\epsilon_{\\mathrm{\\Delta^{13}C}}$",  "\\unit{\\permil}",             expression("TODO"),     "Gaussian error standard deviation of $\\Delta^{13}C$" ,
    "err_vj",               "$\\epsilon_{\\mathrm{vj}}$",             "unitless",                     expression("TODO"),     "Gaussian error standard deviation of $\\frac{V_{\\mathrm{cmax}}}{J_{\\mathrm{max}}}$" ,
    "errbias_bigD13C",      "$\\delta_{\\mathrm{\\Delta^{13}C}}$",    "\\unit{\\permil}",             expression("TODO"),     "Bias error term of $\\Delta^{13}C$ (= mod - obs)",
    "errbias_vj",           "$\\delta_{\\mathrm{vj}}$",               "unitless",                     expression("TODO"),     "Bias error term of $\\frac{V_{\\mathrm{cmax}}}{J_{\\mathrm{max}}}$ (= mod - obs)",
    "errscale_gpp",         "$\\kappa_{\\mathrm{gpp}}$",              "unitless",                     expression("TODO"),     "Multiplicative bias error term of GPP"
  ) |> mutate(Parameter = forcats::as_factor(Parameter),   # make factor to keep ordering
              Symbol_tex = forcats::as_factor(Symbol_tex)) # make factor to keep ordering

  caption <- paste(
    "Parameter listing including prior and Maximum A Posteriori (MAP) estimates.",
    "The bounds of uniform or truncated normal prior distributions are given in square brackets.",
    "Parameters that were held fixed for the calibration are marked with a single number in brackets and an asterisk (*)")

  res_s1 <- setup_rsofun_calibration(scenario = 111)
  res_s2 <- setup_rsofun_calibration(scenario = 112)
  res_s3 <- setup_rsofun_calibration(scenario = 113)
  out_calib_s111DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen111_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s112DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen112_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s113DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen113_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))


  par_prior_s1_3 <- bind_rows(
    bind_rows(lapply(res_s1$par, as.data.frame), .id = "Parameter") |> mutate(scenario = "Scenario 1"),
    bind_rows(lapply(res_s2$par, as.data.frame), .id = "Parameter") |> mutate(scenario = "Scenario 2"),
    bind_rows(lapply(res_s3$par, as.data.frame), .id = "Parameter") |> mutate(scenario = "Scenario 3")
  ) |>
    # format priors:
    mutate(format_decimals = case_when(
      Parameter %in% c()                        ~ "[%.0f to %.0f]",
      Parameter %in% c("kphio_par_b","soilm_thetastar","tau_acclim",
                       "beta_unitcostratio")    ~ "[%.1f to %.1f]",
      Parameter %in% c("kphio", "err_gpp", "rd_to_vcmax", "soilm_betao",
                       "err_bigD13C", "err_vj", "errbias_bigD13C", "errbias_vj","errscale_gpp",
                       "kc_jmax")               ~ "[%.2f to %.2f]",
      Parameter %in% c("kphio_par_a")           ~ "[%.3f to %.3f]",
      TRUE ~                                      "[%.3f to %.3f]")) |>
    mutate(prior_value = sprintf(format_decimals, lower, upper)) |> select(-format_decimals) |>
    # replace normal distributions: \mathcal{N}(\mu,\,\sigma^{2})
    rowwise() |> mutate(prior_value = ifelse(!is.na(sd), sprintf("$\\mathcal{N}(%.1f,\\,%.1f^{2})$\\tnote{a} %s",mean,sd,prior_value),prior_value))

  par_fix_s1_3 <- bind_rows(
    as.data.frame(res_s1$par_fixed) |> pivot_longer(everything(), names_to = "Parameter", values_to="fixed_value") |> mutate(scenario = "Scenario 1"),
    as.data.frame(res_s2$par_fixed) |> pivot_longer(everything(), names_to = "Parameter", values_to="fixed_value") |> mutate(scenario = "Scenario 2"),
    as.data.frame(res_s3$par_fixed) |> pivot_longer(everything(), names_to = "Parameter", values_to="fixed_value") |> mutate(scenario = "Scenario 3")
  ) |>
    # format fixed values
    mutate(format_decimals = case_when(
      Parameter %in% c()                        ~ "[%.0f]*",
      Parameter %in% c("kphio_par_b","soilm_thetastar","tau_acclim",
                       "beta_unitcostratio")    ~ "[%.1f]*",
      Parameter %in% c("kphio", "err_gpp", "rd_to_vcmax", "soilm_betao",
                       "err_bigD13C", "err_vj", "errbias_bigD13C", "errbias_vj","errscale_gpp",
                       "kc_jmax")               ~ "[%.2f]*",
      Parameter %in% c("kphio_par_a")           ~ "[%.3f]*",
      TRUE ~                                      "[%.3f]*")) |>
    mutate(fixed_value = sprintf(format_decimals, fixed_value)) |> select(-format_decimals)

  par_MAP_s1_3 <- bind_rows(
    data.frame(MAP = MAP(out_calib_s111DREAMzs$mod, start = 25000)$parametersMAP) |> tibble::rownames_to_column("Parameter") |> mutate(scenario = "Scenario 1"),
    data.frame(MAP = MAP(out_calib_s112DREAMzs$mod, start = 25000)$parametersMAP) |> tibble::rownames_to_column("Parameter") |> mutate(scenario = "Scenario 2"),
    data.frame(MAP = MAP(out_calib_s113DREAMzs$mod, start = 25000)$parametersMAP) |> tibble::rownames_to_column("Parameter") |> mutate(scenario = "Scenario 3")
  ) |>
    # format MAP values
    mutate(format_decimals = case_when(
      Parameter %in% c()                        ~ "%.0f",
      Parameter %in% c("kphio_par_b","soilm_thetastar","tau_acclim",
                       "beta_unitcostratio")    ~ "%.1f",
      Parameter %in% c("kphio", "err_gpp", "rd_to_vcmax", "soilm_betao",
                       "err_bigD13C", "err_vj", "errbias_bigD13C", "errbias_vj","errscale_gpp",
                       "kc_jmax")               ~ "%.2f",
      Parameter %in% c("kphio_par_a")           ~ "%.3f",
      TRUE ~                                      "%.3f")) |>
    mutate(MAP = sprintf(format_decimals, MAP)) |> select(-format_decimals)

  # RETRY FORMATTING:
  # Symbol|Parameter name|Description|S1,map,prior|S2,map,prior|S3,map,prior

  caption_v2 <- paste(
    "Parameter listing including Maximum A Posteriori (MAP) estimates and prior distributions.",
    "The bounds of uniform or truncated normal prior distributions are given in square brackets.",
    "Parameters that were held fixed for the calibration are marked with a single number in brackets and an asterisk (*)")

  table_b_v2 <- left_join(
      bind_rows(par_prior_s1_3,
                par_fix_s1_3 |> rename(prior_value = fixed_value)),
      par_MAP_s1_3,
      by = join_by(Parameter, scenario)) |>
    # format text
    mutate(cell_text = paste0(MAP, "\\newline", prior_value)) |>
    select(Parameter, scenario, cell_text) |># TODO instead of Parameter use Symbol
    pivot_wider(names_from = scenario, values_from = cell_text, names_glue = "{scenario}\\newline MAP\\newline [Prior]") |>
    # append Symbol and Description:
    mutate(Parameter = factor(Parameter, levels = levels(rsofun_symbol_parname_description$Parameter))) |>
    left_join(rsofun_symbol_parname_description,
              by = join_by(Parameter)) |>
    arrange(Parameter) |>
    select(Symbol_tex, Units_tex, Parameter,
           Description,
           `Scenario 1\\newline MAP\\newline [Prior]`,
           `Scenario 2\\newline MAP\\newline [Prior]`,
           `Scenario 3\\newline MAP\\newline [Prior]`)

  # remove some parameters:
  table_b_v2_reduced <- table_b_v2 |> filter(!(Parameter %in% c("rd_to_vcmax", "soilm_betao")))

  # export to LaTeX
  table_b_v2 %>%
    # mutate(Parameter = gsub("\\_","\\\\_",Parameter)) %>% # format for LaTeX
    select(-Parameter) %>% # use Symbol instead of Parameter(code)
    xtable::xtable(x = .,
                   caption = caption_v2,
                   align = rep("l", (ncol(x = .) + 1))  # make all columns left-aligned # align="rXXXXXX" # make use of tabularx "X"-column
                   # align = "p{0.7cm} p{1.4cm} p{5.5cm} X X X" # TODO: change later to:
    ) %>%
    print(x = .,
      file = here::here("fig/table-b_parameters.tex"),
      floating.environment = "threeparttable",
      caption.placement = "top", tabular.environment = "tabularx", width="\\textwidth",
      include.rownames = FALSE,
      sanitize.text.function=function(x){x} # override normal sanitizing function since we have defined tex
    ) # this can be added to tex file as: \input{filename.tex})

  table_b_v2_reduced %>%
    # mutate(Parameter = gsub("\\_","\\\\_",Parameter)) %>% # format for LaTeX
    select(-Parameter) %>% # use Symbol instead of Parameter(code)
    xtable::xtable(x = .,
                   caption = caption,
                   align = rep("l", (ncol(x = .) + 1))  # make all columns left-aligned # align="rXXXXXX" # make use of tabularx "X"-column
                   # align = "p{0.7cm} p{1.4cm} p{5.5cm} X X X" # TODO: change later to:
    ) %>%
    print(x = .,
      file = here::here("fig/table-b_parameters_reduced.tex"),
      floating.environment = "threeparttable",
      caption.placement = "top", tabular.environment = "tabularx", width="\\textwidth",
      include.rownames = FALSE,
      sanitize.text.function=function(x){x} # override normal sanitizing function since we have defined tex
    ) # this can be added to tex file as: \input{filename.tex})
}



# Plot runtimes of calibration ----

if (FALSE) {
              #
              #
              # # timings <- readr::read_rds(here::here("timings_FB_2025-08-20_21h25.rds"))
              # # timing_files <- list.files(here::here("data","timings"), pattern = "timings_scen.*_2025-08-.*.rds", full.names = T)
              # timing_files1 <- list.files(file.path(rsofun_doc_output_path, "data","timings"), pattern = "timings_scen.*_2025-08-.*.rds", full.names = T)
              # timings1 <- lapply(timing_files1, readr::read_rds) |> bind_rows()
              #
              # timing_files2 <- list.files(file.path("/home/fabian/GitHub/geco-bern/rsofun_doc/data","timings"), pattern = "timings_scen.*_2025-08-.*.rds", full.names = T)
              # timings2 <- lapply(timing_files2, readr::read_rds) |> bind_rows()
              #
              # timings <- bind_rows(timings1, timings2) |>
              #   mutate(scenario = factor(scenario),
              #          cores    = factor(cores))
              #
              # pl_timings <- ggplot(
              #   timings,
              #   aes(#x=(iterations-burnin)*n_chains*n_chains_inner,
              #     x=(iterations),
              #     y=as.numeric(walltime,"secs")/60,
              #     color = scenario,
              #     linetype = cores)) +
              #   geom_point() + geom_line() +
              #   geom_text(aes(label = sprintf("(%d,%d)",iterations,burnin)), vjust = 0, show.legend = F) +
              #   scale_x_log10(minor_breaks=scales::minor_breaks_n(10)) +
              #   scale_y_log10(minor_breaks=scales::minor_breaks_n(10)) +
              #   labs(y="walltime (minutes)") + theme_minimal()
              # pl_timings
              # pl_timings$data$scenario |> unique()
              # pl_timings %+% (pl_timings$data |> filter(scenario %in% c(0,1,2,3,4,14,86,87,88,89)))
              # pl_timings %+% (pl_timings$data |> filter(cores == 8, scenario %in% c(0,1,2,3,4,14)))
              # pl_timings %+% (pl_timings$data |> filter(cores == 8, scenario %in% c(0,4,14,15)))
              #
              #
              # longest_chains <- timings |>
              #   filter(grepl("/data_2/scratch/.*", resultfile)) |>
              #   mutate(nchains_x_iterations = n_chains * iterations) |>
              #   arrange(-nchains_x_iterations) |>
              #   group_by(scenario) |>
              #   slice(1)
              #
              # # out_calib_s0 <- longest_chains |> filter(scenario == 0, cores == 3) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              # # out_calib_s1 <- longest_chains |> filter(scenario == 1, cores == 3) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              # # out_calib_s2 <- longest_chains |> filter(scenario == 2, cores == 3) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              # # out_calib_s2b <- longest_chains |> filter(scenario == 2, cores == 10) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              # # out_calib_s3 <- longest_chains |> filter(scenario == 3, cores == 3) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              # # out_calib_s3b <- longest_chains |> filter(scenario == 3, cores == 3) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              # # out_calib_s3d <- longest_chains |> filter(scenario == 3, cores == 8) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              # # out_calib_s4d <- longest_chains |> filter(scenario == 4, cores == 8) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              #
              # # reload non-parallel versions, to see if there is an issue with parallelization (or if it is with the likelihood)
              # # out_calib_s0_serial <- longest_chains |> filter(scenario == 0, cores == 1) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              # # out_calib_s1_serial <- longest_chains |> filter(scenario == 1, cores == 1) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              # # out_calib_s2_serial <- longest_chains |> filter(scenario == 2, cores == 1) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              # # out_calib_s3_serial <- longest_chains |> filter(scenario == 3, cores == 1) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds() # TODO
              #
              # out_calib_s0 <- longest_chains |> filter(scenario == 0) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              # out_calib_s1 <- longest_chains |> filter(scenario == 1) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              # out_calib_s2 <- longest_chains |> filter(scenario == 2) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              # out_calib_s3 <- longest_chains |> filter(scenario == 3) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              # out_calib_s4 <- longest_chains |> filter(scenario == 4) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              # out_calib_s14 <- longest_chains |> filter(scenario == 14) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              # out_calib_s86 <- longest_chains |> filter(scenario == 86) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              # out_calib_s87 <- longest_chains |> filter(scenario == 87) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              # out_calib_s88 <- longest_chains |> filter(scenario == 88) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              # out_calib_s89 <- longest_chains |> filter(scenario == 89) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              #
              # out_calib_s14_base <- timings |> filter(scenario == 14, iterations == 10000) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
              # out_calib_s14_cont <- timings |> filter(scenario == 14, iterations == 10029, grepl("continued.rds",resultfile)) |> slice(1) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
}

