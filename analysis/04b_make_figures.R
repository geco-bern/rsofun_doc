# TO investigate gpp only fit per site

library(readr)
library(dplyr)
library(tidyr)
library(BayesianTools)
library(ggplot2)
library(patchwork)

source(here::here("R/calibration_helpers.R"))
source(here::here("analysis/00_define_scenarios.R"))

out_calib_s31 <- readr::read_rds("/data_2/scratch/fbernhard/rsofun_doc_outputs/data/out_calib__scen31_DEzs-5000-0iter_8x3chains_on_CPU8x1.rds")
out_calib_s32 <- readr::read_rds("/data_2/scratch/fbernhard/rsofun_doc_outputs/data/out_calib__scen32_DEzs-5000-0iter_8x3chains_on_CPU8x1.rds")
# out_calib_s33 <- readr::read_rds("/data_2/scratch/fbernhard/rsofun_doc_outputs/data/out_calib__scen33_DEzs-5000-0iter_8x3chains_on_CPU8x1.rds")
# out_calib_s34 <- readr::read_rds("/data_2/scratch/fbernhard/rsofun_doc_outputs/data/out_calib__scen34_DEzs-5000-0iter_8x3chains_on_CPU8x1.rds")
# out_calib_s35 <- readr::read_rds("/data_2/scratch/fbernhard/rsofun_doc_outputs/data/out_calib__scen35_DEzs-5000-0iter_8x3chains_on_CPU8x1.rds")

#
burnin_to_skip = 500

plot_mcmc_trace(out_calib_s31$mod, nr_internal_chains = 3, burnin_to_skip = 0) + ggtitle(out_calib_s31$fpath) # + geom_vline(xintercept = burnin_to_skip, color="red", linetype="dash")
plot_mcmc_trace(out_calib_s32$mod, nr_internal_chains = 3, burnin_to_skip = 0) + ggtitle(out_calib_s32$fpath) # + geom_vline(xintercept = burnin_to_skip, color="red", linetype="dash")
# plot_mcmc_trace(out_calib_s33$mod, nr_internal_chains = 3, burnin_to_skip = 0) + ggtitle(out_calib_s33$fpath)

plot_prior_posterior_density(out_calib_s31$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 31")+ ggtitle(out_calib_s31$fpath)
plot_prior_posterior_density(out_calib_s32$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 32")+ ggtitle(out_calib_s32$fpath)
# plot_prior_posterior_density(out_calib_s33$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 33")+ ggtitle(out_calib_s33$fpath)
# plot_prior_posterior_density(out_calib_s34$mod,burnin_to_skip = burnin_to_skip) + ggtitle("Scenario 34")+ ggtitle(out_calib_s34$fpath)



# sample posteriors and run model for each sample parameter set
source(here::here("R/run_prediction_rsofun.R"))
t0 <- Sys.time()
# df_predict_s1  <- readRDS("df_predict_s1.RDS")
df_predict_s31 <- run_prediction_rsofun(out_calib_s31, "train", burnin_to_skip, n_samples = 500,n_cores = 12) # 500 samples on 12 cores: X minutes
# df_predict_s32 <- run_prediction_rsofun(out_calib_s32, "train", burnin_to_skip,n_samples = 500,n_cores = 12) # 500 samples on 12 cores: X minutes
# df_predict_s33 <- run_prediction_rsofun(out_calib_s33, "train", burnin_to_skip,n_samples = 500,n_cores = 12) # 500 samples on 12 cores: X minutes
t1 <- Sys.time()
print(t1-t0)
# NOTE: no error term has (yet) been added

# Plot raw predictions
## gpp:
df_hexplot_gpp_s31 <- df_predict_s31 |> unnest(sim) |> filter(!is.na(obs)) |> filter(target == "gpp")
# df_hexplot_gpp_s32 <- df_predict_s32 |> unnest(sim) |> filter(!is.na(obs)) |> filter(target == "gpp")

df_hexplot_gpp <- df_hexplot_gpp_s31

lims <- round(max(quantile(df_hexplot_gpp$mod_no_err, 0.9999), quantile(df_hexplot_gpp$obs, 0.9999)))
ggplot(df_hexplot_gpp, aes(x=mod_no_err, y=obs)) +
  geom_hex(bins = 50, show.legend = FALSE) +
  facet_wrap(~target) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  coord_fixed() +
  xlim(0, lims) +
  ylim(0, lims) +
  theme_classic() +
  khroma::scale_fill_batlowW(trans = "log", reverse = TRUE) +
  facet_wrap(~sitename)
# khroma::scale_fill_davos(trans = "log", reverse = TRUE)
