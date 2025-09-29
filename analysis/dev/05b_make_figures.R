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


out_calib_s14 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations_from_WS02/out_calib__scen14_DEzs-80000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s70 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen70_DEzs-50000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s71 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen71_DEzs-50000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s72 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen72_DEzs-50000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s73 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen73_DEzs-50000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s74 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen74_DEzs-50000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s75 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen75_DEzs-50000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s76 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen76_DEzs-40000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s77 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen77_DEzs-50000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s78 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen78_DEzs-50000-0iter_8x3chains_on_CPU8x1_continued.rds"))

out_calib_s90 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen90_DEzs-50000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s91 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen91_DEzs-50000-0iter_8x3chains_on_CPU8x1.rds"))
out_calib_s92 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen92_DEzs-50000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s93 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen93_DEzs-55000-0iter_8x3chains_on_CPU8x1_continued.rds")) # TODO change to 80k/100k
out_calib_s94 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen94_DEzs-80000-0iter_8x3chains_on_CPU8x1_continued.rds")) # TODO change to 80k/100k
out_calib_s95 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen95_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds")) # TODO change to 80k/100k
out_calib_s96 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen96_DEzs-80000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s97 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen97_DEzs-80000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s98 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen98_DEzs-80000-0iter_8x3chains_on_CPU8x1_continued.rds"))

out_calib_s94DREAMzs <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen94_DREAMzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds")) # TODO change to 80k/100k

# out_calib_s104 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen104_DEzs-60-0iter_8x3chains_on_CPU8x1_continued.rds"))
# (plot_prior_posterior_density(out_calib_s104$mod,burnin_to_skip = 1) + ggtitle("Scenario 104")+ ggtitle(out_calib_s104$fpath))
# out_calib_s103 <- readr::read_rds(file.path(rsofun_doc_output_path, "data/calibrations/out_calib__scen103_DEzs-60-0iter_8x3chains_on_CPU8x1_continued.rds"))
# (plot_prior_posterior_density(out_calib_s103$mod,burnin_to_skip = 1) + ggtitle("Scenario 103")+ ggtitle(out_calib_s103$fpath))

burnin_to_skip = 12000

# Figure E: trace plot
(plot_mcmc_trace(out_calib_s14$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s14$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s14.png")
(plot_mcmc_trace(out_calib_s70$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s70$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s70.png")
(plot_mcmc_trace(out_calib_s71$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s71$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s71.png")
(plot_mcmc_trace(out_calib_s72$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s72$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s72.png")
(plot_mcmc_trace(out_calib_s73$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s73$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s73.png")
(plot_mcmc_trace(out_calib_s74$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s74$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s74.png")
(plot_mcmc_trace(out_calib_s75$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s75$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s75.png")
(plot_mcmc_trace(out_calib_s76$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s76$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s76.png")
(plot_mcmc_trace(out_calib_s77$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s77$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s77.png")
(plot_mcmc_trace(out_calib_s78$mod, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = burnin_to_skip) + ggtitle(out_calib_s78$fpath)) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s78.png")

pl_post_s14 <- (plot_prior_posterior_density(out_calib_s14$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 14")+ ggtitle(out_calib_s14$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s14.png")
pl_post_s70 <- (plot_prior_posterior_density(out_calib_s70$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 70")+ ggtitle(out_calib_s70$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s70.png")
pl_post_s71 <- (plot_prior_posterior_density(out_calib_s71$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 71")+ ggtitle(out_calib_s71$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s71.png")
pl_post_s72 <- (plot_prior_posterior_density(out_calib_s72$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 72")+ ggtitle(out_calib_s72$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s72.png")
pl_post_s73 <- (plot_prior_posterior_density(out_calib_s73$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 73")+ ggtitle(out_calib_s73$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s73.png")
pl_post_s74 <- (plot_prior_posterior_density(out_calib_s74$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 74")+ ggtitle(out_calib_s74$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s74.png")
pl_post_s75 <- (plot_prior_posterior_density(out_calib_s75$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 75")+ ggtitle(out_calib_s75$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s75.png")
pl_post_s76 <- (plot_prior_posterior_density(out_calib_s76$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 76")+ ggtitle(out_calib_s76$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s76.png")
pl_post_s77 <- (plot_prior_posterior_density(out_calib_s77$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 77")+ ggtitle(out_calib_s77$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s77.png")
pl_post_s78 <- (plot_prior_posterior_density(out_calib_s78$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 78")+ ggtitle(out_calib_s78$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s78.png")

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

pl_post_comparison <- plot_prior_posterior_density_compare(
  named_list_scen =  c(list("prior" = out_calib_s31$mod), scenarios_to_compare),
  burnin_to_skip  = burnin_to_skip)
ggsave_and_return(pl_post_comparison2, "fig_A_MCMCconvergence_posterior_all.png")

burnin_to_skip_50s = 2500
pl_post_comparison2 <- plot_prior_posterior_density_compare(
  named_list_scen =  c(list("prior" = out_calib_s51$mod), scenarios_to_compare2),
  burnin_to_skip  = burnin_to_skip_50s)
ggsave_and_return(pl_post_comparison2, "fig_A_MCMCconvergence_posterior_all2.png")



# Analyze estimated (MAP) of soilm_betastar vs whc:
if (FALSE) { # This made sense for scenarios that fitted a single site only
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
    left_join(res1$drivobs_train |> select(sitename, site_info) |> unnest(site_info))
  pl_soilmthetastar_whc <- ggplot(dat_to_plot, aes(x=whc, y=soilm_thetastar, colour = scenario)) +
    geom_violin(scale = "width", width = 10) +
    geom_abline(slope = 0.2, linetype = "dashed")
  ggsave_and_return(pl_soilmthetastar_whc, "fig_Z_MCMCpost_thetastar-vs-whc.png")

  dat_to_plot2 <- df_dis2 |> filter(param == "soilm_thetastar") |>
    pivot_wider(id_cols = c(sample_id, scenario), names_from = "param", values_from = "param_sample") |>
    mutate(sitename = substr(scenario,1,6)) |>
    left_join(res1$drivobs_train |> select(sitename, site_info) |> unnest(site_info))
  pl_soilmthetastar_whc2 <- ggplot(dat_to_plot2, aes(x=whc, y=soilm_thetastar, colour = scenario)) +
    geom_violin(scale = "width", width = 10) +
    geom_abline(slope = 0.2, linetype = "dashed")
  ggsave_and_return(pl_soilmthetastar_whc2, "fig_Z_MCMCpost_thetastar-vs-whc2.png")
}


# Parameter correlation analysis
if (TRUE){ # This is quite a slow plot:

  save_corr_plot <- function(out_calib, thin, numSamples, start, filename){
    png(filename, width = 7.2, height = 7.2, units = "in", res = 300)
    correlationPlot(out_calib$mod, thin = thin, numSamples = numSamples, start = start)
    dev.off()
  }
  save_corr_plot(out_calib_s71, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s71.png"))
  save_corr_plot(out_calib_s72, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s72.png"))
  save_corr_plot(out_calib_s73, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s73.png"))
  save_corr_plot(out_calib_s74, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s74.png"))
  save_corr_plot(out_calib_s75, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s75.png"))
  save_corr_plot(out_calib_s76, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s76.png"))
  save_corr_plot(out_calib_s77, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s77.png"))
  save_corr_plot(out_calib_s78, thin = 1, numSamples = 5000, start = burnin_to_skip,filename = here::here("fig/fig_E2_MCMCconvergence_corr_s78.png"))

}

