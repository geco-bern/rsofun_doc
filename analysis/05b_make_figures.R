# TO investigate gpp only fit per site

library(readr)
library(dplyr)
library(tidyr)
library(BayesianTools)
library(ggplot2)
library(patchwork)
library(cowplot)

source(here::here("R/calibration_helpers.R"))
source(here::here("R/prediction_helpers.R"))
source(here::here("R/run_prediction_rsofun.R"))

source(here::here("R/figure_helpers.R"))
source(here::here("analysis/00_define_scenarios.R"))


out_calib_s0 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen0_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s1 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen1_DEzs-100000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s2 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen2_DEzs-100000-0iter_10x3chains_on_CPU10x1.rds"))
out_calib_s3 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen3_DEzs-100000-0iter_8x3chains_on_CPU8x1.rds"))
# out_calib_s4 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen4_DEzs-35000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s14<- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen14_DEzs-50000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s15<- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen15_DEzs-15000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s16<- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen16_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s17<- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen17_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s18<- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen18_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds"))

out_calib_s31 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen31_DEzs-50000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s32 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen32_DEzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s33 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen33_DEzs-50000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s34 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen34_DEzs-50000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s35 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen35_DEzs-50000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s36 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen36_DEzs-50000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s37 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen37_DEzs-50000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s38 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen38_DEzs-50000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s39 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen39_DEzs-50000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s40 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen40_DEzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s41 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen41_DEzs-50000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s42 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen42_DEzs-50000-0iter_8x3chains_on_CPU8x1.rds"))

out_calib_s51 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen51_DEzs-15000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s52 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen52_DEzs-15000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s53 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen53_DEzs-15000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s54 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen54_DEzs-15000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s55 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen55_DEzs-15000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s56 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen56_DEzs-15000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s57 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen57_DEzs-15000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s58 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen58_DEzs-15000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s59 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen59_DEzs-15000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s60 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen60_DEzs-15000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s61 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen61_DEzs-15000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s62 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen62_DEzs-15000-0iter_8x3chains_on_CPU8x1.rds"))
#
burnin_to_skip = 8000
burnin_to_skip_50s = 2500

(plot_mcmc_trace(out_calib_s0$mod,  nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s0$fpath))  |> ggsave_and_return("fig_E_MCMCconvergence_trace_s0.png")
(plot_mcmc_trace(out_calib_s1$mod,  nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s1$fpath))  |> ggsave_and_return("fig_E_MCMCconvergence_trace_s1.png")
(plot_mcmc_trace(out_calib_s2$mod,  nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s2$fpath))  |> ggsave_and_return("fig_E_MCMCconvergence_trace_s2.png")
(plot_mcmc_trace(out_calib_s3$mod,  nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s3$fpath))  |> ggsave_and_return("fig_E_MCMCconvergence_trace_s3.png")
(plot_mcmc_trace(out_calib_s4$mod,  nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s4$fpath))  |> ggsave_and_return("fig_E_MCMCconvergence_trace_s4.png")
(plot_mcmc_trace(out_calib_s14$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s14$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s14.png")
(plot_mcmc_trace(out_calib_s15$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip_50s) + ggtitle(out_calib_s15$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s15.png")
(plot_mcmc_trace(out_calib_s16$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s16$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s16.png")
(plot_mcmc_trace(out_calib_s17$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s17$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s17.png")
(plot_mcmc_trace(out_calib_s18$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s18$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s18.png")

# plot_mcmc_trace(out_calib_s14$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 8000)
# plot_mcmc_trace(out_calib_s14$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 15000)

(plot_mcmc_trace(out_calib_s31$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s31$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s31.png")
(plot_mcmc_trace(out_calib_s32$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s32$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s32.png")
(plot_mcmc_trace(out_calib_s33$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s33$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s33.png")
(plot_mcmc_trace(out_calib_s34$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s34$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s34.png")
(plot_mcmc_trace(out_calib_s35$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s35$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s35.png")
(plot_mcmc_trace(out_calib_s36$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s36$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s36.png")
(plot_mcmc_trace(out_calib_s37$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s37$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s37.png")
(plot_mcmc_trace(out_calib_s38$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s38$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s38.png")
(plot_mcmc_trace(out_calib_s39$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s39$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s39.png")
(plot_mcmc_trace(out_calib_s40$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s40$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s40.png")
(plot_mcmc_trace(out_calib_s41$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s41$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s41.png")
(plot_mcmc_trace(out_calib_s42$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s42$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s42.png")

(plot_mcmc_trace(out_calib_s51$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip_50s) + ggtitle(out_calib_s51$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s51.png")
(plot_mcmc_trace(out_calib_s52$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip_50s) + ggtitle(out_calib_s52$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s52.png")
(plot_mcmc_trace(out_calib_s53$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip_50s) + ggtitle(out_calib_s53$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s53.png")
(plot_mcmc_trace(out_calib_s54$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip_50s) + ggtitle(out_calib_s54$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s54.png")
(plot_mcmc_trace(out_calib_s55$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip_50s) + ggtitle(out_calib_s55$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s55.png")
(plot_mcmc_trace(out_calib_s56$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip_50s) + ggtitle(out_calib_s56$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s56.png")
(plot_mcmc_trace(out_calib_s57$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip_50s) + ggtitle(out_calib_s57$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s57.png")
(plot_mcmc_trace(out_calib_s58$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip_50s) + ggtitle(out_calib_s58$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s58.png")
(plot_mcmc_trace(out_calib_s59$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip_50s) + ggtitle(out_calib_s59$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s59.png")
(plot_mcmc_trace(out_calib_s60$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip_50s) + ggtitle(out_calib_s60$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s60.png")
(plot_mcmc_trace(out_calib_s61$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip_50s) + ggtitle(out_calib_s61$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s61.png")
(plot_mcmc_trace(out_calib_s62$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip_50s) + ggtitle(out_calib_s62$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s62.png")



pl_post_s0  <- (plot_prior_posterior_density(out_calib_s0$mod,burnin_to_skip  = burnin_to_skip) + ggtitle("Scenario 0") + ggtitle(out_calib_s0$fpath))  |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s0.png")
pl_post_s1  <- (plot_prior_posterior_density(out_calib_s1$mod,burnin_to_skip  = burnin_to_skip) + ggtitle("Scenario 1") + ggtitle(out_calib_s1$fpath))  |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s1.png")
pl_post_s2  <- (plot_prior_posterior_density(out_calib_s2$mod,burnin_to_skip  = burnin_to_skip) + ggtitle("Scenario 2") + ggtitle(out_calib_s2$fpath))  |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s2.png")
pl_post_s3  <- (plot_prior_posterior_density(out_calib_s3$mod,burnin_to_skip  = burnin_to_skip) + ggtitle("Scenario 3") + ggtitle(out_calib_s3$fpath))  |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s3.png")
pl_post_s4  <- (plot_prior_posterior_density(out_calib_s4$mod,burnin_to_skip  = burnin_to_skip) + ggtitle("Scenario 4") + ggtitle(out_calib_s4$fpath))  |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s4.png")

pl_post_s14 <- (plot_prior_posterior_density(out_calib_s14$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 14")+ ggtitle(out_calib_s14$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s14.png")
pl_post_s15 <- (plot_prior_posterior_density(out_calib_s15$mod,burnin_to_skip = 4000) + ggtitle("Scenario 15")+ ggtitle(out_calib_s15$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s15.png")

pl_post_s16 <- (plot_prior_posterior_density(out_calib_s16$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 16")+ ggtitle(out_calib_s16$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s16.png")
pl_post_s17 <- (plot_prior_posterior_density(out_calib_s17$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 17")+ ggtitle(out_calib_s17$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s17.png")
pl_post_s18 <- (plot_prior_posterior_density(out_calib_s18$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 18")+ ggtitle(out_calib_s18$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s18.png")


pl_post_s31 <- (plot_prior_posterior_density(out_calib_s31$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 31")+ ggtitle(out_calib_s31$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s31.png")
pl_post_s32 <- (plot_prior_posterior_density(out_calib_s32$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 32")+ ggtitle(out_calib_s32$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s32.png")
pl_post_s33 <- (plot_prior_posterior_density(out_calib_s33$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 33")+ ggtitle(out_calib_s33$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s33.png")
pl_post_s34 <- (plot_prior_posterior_density(out_calib_s34$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 34")+ ggtitle(out_calib_s34$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s34.png")
pl_post_s35 <- (plot_prior_posterior_density(out_calib_s35$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 35")+ ggtitle(out_calib_s35$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s35.png")
pl_post_s36 <- (plot_prior_posterior_density(out_calib_s36$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 36")+ ggtitle(out_calib_s36$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s36.png")
pl_post_s37 <- (plot_prior_posterior_density(out_calib_s37$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 37")+ ggtitle(out_calib_s37$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s37.png")
pl_post_s38 <- (plot_prior_posterior_density(out_calib_s38$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 38")+ ggtitle(out_calib_s38$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s38.png")
pl_post_s39 <- (plot_prior_posterior_density(out_calib_s39$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 39")+ ggtitle(out_calib_s39$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s39.png")
pl_post_s40 <- (plot_prior_posterior_density(out_calib_s40$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 40")+ ggtitle(out_calib_s40$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s40.png")
pl_post_s41 <- (plot_prior_posterior_density(out_calib_s41$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 41")+ ggtitle(out_calib_s41$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s41.png")
pl_post_s42 <- (plot_prior_posterior_density(out_calib_s42$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 42")+ ggtitle(out_calib_s42$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s42.png")

pl_post_s51 <- (plot_prior_posterior_density(out_calib_s51$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 51")+ ggtitle(out_calib_s51$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s51.png")
pl_post_s52 <- (plot_prior_posterior_density(out_calib_s52$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 52")+ ggtitle(out_calib_s52$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s52.png")
pl_post_s53 <- (plot_prior_posterior_density(out_calib_s53$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 53")+ ggtitle(out_calib_s53$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s53.png")
pl_post_s54 <- (plot_prior_posterior_density(out_calib_s54$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 54")+ ggtitle(out_calib_s54$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s54.png")
pl_post_s55 <- (plot_prior_posterior_density(out_calib_s55$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 55")+ ggtitle(out_calib_s55$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s55.png")
pl_post_s56 <- (plot_prior_posterior_density(out_calib_s56$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 56")+ ggtitle(out_calib_s56$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s56.png")
pl_post_s57 <- (plot_prior_posterior_density(out_calib_s57$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 57")+ ggtitle(out_calib_s57$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s57.png")
pl_post_s58 <- (plot_prior_posterior_density(out_calib_s58$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 58")+ ggtitle(out_calib_s58$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s58.png")
pl_post_s59 <- (plot_prior_posterior_density(out_calib_s59$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 59")+ ggtitle(out_calib_s59$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s59.png")
pl_post_s60 <- (plot_prior_posterior_density(out_calib_s60$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 60")+ ggtitle(out_calib_s60$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s60.png")
pl_post_s61 <- (plot_prior_posterior_density(out_calib_s61$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 61")+ ggtitle(out_calib_s61$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s61.png")
pl_post_s62 <- (plot_prior_posterior_density(out_calib_s62$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 62")+ ggtitle(out_calib_s62$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s62.png")

# compare them:
# cowplot::plot_grid(
#   pl_post_s1 + theme(legend.position = "none"),
#   pl_post_s0 + theme(legend.position = "none"),
#   pl_post_s33 + theme(legend.position = "none"),
#   pl_post_s35 + theme(legend.position = "none"),
#   ncol=1)
scenarios_to_compare <- list("BE-Vie (s31)" = out_calib_s31$mod,
                             "CH-Dav (s32)" = out_calib_s32$mod,
                             "CZ-BK1 (s33)" = out_calib_s33$mod,
                             "DK-Sor (s34)" = out_calib_s34$mod,
                             "FI-Hyy (s35)" = out_calib_s35$mod,
                             "GF-Guy (s36)" = out_calib_s36$mod,
                             "IT-Lav (s37)" = out_calib_s37$mod,
                             "US-Ha1 (s38)" = out_calib_s38$mod,
                             "US-MMS (s39)" = out_calib_s39$mod,
                             "US-PFa (s40)" = out_calib_s40$mod,
                             "US-Var (s41)" = out_calib_s41$mod,
                             "US-Wkg (s42)" = out_calib_s42$mod)
scenarios_to_compare2 <- list("BE-Vie (s51)" = out_calib_s51$mod,
                             "CH-Dav (s52)" = out_calib_s52$mod,
                             "CZ-BK1 (s53)" = out_calib_s53$mod,
                             "DK-Sor (s54)" = out_calib_s54$mod,
                             "FI-Hyy (s55)" = out_calib_s55$mod,
                             "GF-Guy (s56)" = out_calib_s56$mod,
                             "IT-Lav (s57)" = out_calib_s57$mod,
                             "US-Ha1 (s58)" = out_calib_s58$mod,
                             "US-MMS (s59)" = out_calib_s59$mod,
                             "US-PFa (s60)" = out_calib_s60$mod,
                             "US-Var (s61)" = out_calib_s61$mod,
                             "US-Wkg (s62)" = out_calib_s62$mod)

scenarios_to_compare3 <- list("Prior 1" = out_calib_s1$mod,
                              "Prior 2" = out_calib_s2$mod,
                              "Prior 3" = out_calib_s3$mod,
                              "Prior 4" = out_calib_s14$mod,
                              "1" = out_calib_s1$mod,
                              "2" = out_calib_s2$mod,
                              "3" = out_calib_s3$mod,
                              "4" = out_calib_s14$mod)

# pl_post_comparison <- plot_prior_posterior_density_compare(
#   named_list_scen =  c(list("prior" = out_calib_s31$mod), scenarios_to_compare),
#   burnin_to_skip = burnin_to_skip)
# ggsave_and_return(pl_post_comparison, "fig_A_MCMCconvergence_posterior_all.png")
# ggsave_and_return(pl_post_comparison, "fig_A_MCMCconvergence_posterior_all_1_2_3_14.png")

pl_post_comparison <- plot_prior_posterior_density_compare(
  named_list_scen =  c(list("prior" = out_calib_s31$mod), scenarios_to_compare),
  burnin_to_skip  = burnin_to_skip)
ggsave_and_return(pl_post_comparison2, "fig_A_MCMCconvergence_posterior_all.png")

pl_post_comparison2 <- plot_prior_posterior_density_compare(
  named_list_scen =  c(list("prior" = out_calib_s51$mod), scenarios_to_compare2),
  burnin_to_skip  = burnin_to_skip_50s)
ggsave_and_return(pl_post_comparison2, "fig_A_MCMCconvergence_posterior_all2.png")


pl_post_comparison3 <- plot_prior_posterior_density_compare(
  named_list_scen =  scenarios_to_compare3,
  burnin_to_skip  = burnin_to_skip,
  ridges = TRUE)
# cols <- c(rep("grey",4), scico::scico_palette_data("batlow",categorical = TRUE)[1:4])
# # scales::show_col(cols)
# names(cols) <- names(scenarios_to_compare3)
# pl_post_comparison3 <- pl_post_comparison3 + scale_color_manual(NULL, values = cols)
# pl_post_comparison3 <- pl_post_comparison3 + aes(fill = distrib) + scale_color_manual(NULL, values = cols, aesthetics = c("colour","fill"))
ggsave_and_return(pl_post_comparison3, "fig_A_MCMCconvergence_posterior_s1_2_3_14.png",
                  width = 7.2, height = 3.6)

# Analyze estimated (MAP) of soilm_betastar vs whc:
res1 <- setup_rsofun_calibration(scenario = 1)
# get_MAP <- function(mod){
#   as.data.frame(BayesianTools::MAP(mod)$parametersMAP) |>
#     as_tibble(rownames = "param") |>
#     rename(c("param"=1,"MAP"=2))
# }
get_distr <- function(mod, N=1000, burnin_to_skip){
  as_tibble(BayesianTools::getSample(mod, start = burnin_to_skip, thin = 1, numSamples = N)) |>
    mutate(sample_id = 1:n()) |>
    select(sample_id, soilm_thetastar) |>
    pivot_longer(soilm_thetastar, names_to = "param", values_to = "param_sample")
}
# df_MAP <- lapply(scenarios_to_compare, get_MAP) |> bind_rows(.id = "scenario")
df_dis <- lapply(scenarios_to_compare, \(mod){get_distr(mod, burnin_to_skip = burnin_to_skip)}) |> bind_rows(.id = "scenario")
df_dis2<- lapply(scenarios_to_compare2,\(mod){get_distr(mod, burnin_to_skip = burnin_to_skip_50s)}) |> bind_rows(.id = "scenario")

dat_to_plot <- df_dis |> filter(param == "soilm_thetastar") |>
  pivot_wider(id_cols = c(sample_id, scenario), names_from = "param", values_from = "param_sample") |>
  mutate(sitename = substr(scenario,1,6)) |>
  left_join(res1$drivobs |> select(sitename, site_info) |> unnest(site_info))
pl_soilmthetastar_whc <- ggplot(dat_to_plot, aes(x=whc, y=soilm_thetastar, colour = scenario)) +
  geom_violin(scale = "width", width = 10) +
  geom_abline(slope = 0.2, linetype = "dashed")
ggsave_and_return(pl_soilmthetastar_whc, "fig_Z_MCMCpost_thetastar-vs-whc.png")

dat_to_plot2 <- df_dis2 |> filter(param == "soilm_thetastar") |>
  pivot_wider(id_cols = c(sample_id, scenario), names_from = "param", values_from = "param_sample") |>
  mutate(sitename = substr(scenario,1,6)) |>
  left_join(res1$drivobs |> select(sitename, site_info) |> unnest(site_info))
pl_soilmthetastar_whc2 <- ggplot(dat_to_plot2, aes(x=whc, y=soilm_thetastar, colour = scenario)) +
  geom_violin(scale = "width", width = 10) +
  geom_abline(slope = 0.2, linetype = "dashed")
ggsave_and_return(pl_soilmthetastar_whc2, "fig_Z_MCMCpost_thetastar-vs-whc2.png")



# Parameter correlation analysis
if (TRUE){ # This is quite a slow plot:

  save_corr_plot <- function(out_calib, thin, numSamples, start, filename){
    png(filename, width = 7.2, height = 7.2, units = "in", res = 300)
    correlationPlot(out_calib$mod, thin = thin, numSamples = numSamples, start = start)
    dev.off()
  }
  save_corr_plot(out_calib_s1, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s1.png"))
  save_corr_plot(out_calib_s2, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s2.png"))
  save_corr_plot(out_calib_s3, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s3.png"))
  save_corr_plot(out_calib_s14, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s14.png"))
  # save_corr_plot(out_calib_s15, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s15.png"))
  save_corr_plot(out_calib_s16, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s16.png"))
  save_corr_plot(out_calib_s17, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s17.png"))
  save_corr_plot(out_calib_s18, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s18.png"))
  # save_corr_plot(out_calib_s31, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s31.png"))
  # save_corr_plot(out_calib_s32, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s32.png"))
  # save_corr_plot(out_calib_s33, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s33.png"))
  # save_corr_plot(out_calib_s34, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s34.png"))
  # save_corr_plot(out_calib_s35, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s35.png"))
  # save_corr_plot(out_calib_s36, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s36.png"))
  # # save_corr_plot(out_calib_s37, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s37.png"))
  # save_corr_plot(out_calib_s38, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s38.png"))
  # save_corr_plot(out_calib_s39, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s39.png"))
  # save_corr_plot(out_calib_s40, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s40.png"))
  # save_corr_plot(out_calib_s41, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s41.png"))
  # save_corr_plot(out_calib_s42, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s42.png"))
}


