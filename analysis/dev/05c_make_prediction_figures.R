# TO do prediction plots (which are very RAM intensive)
# renv::restore()
# renv::status()

library(readr)
library(dplyr)
library(data.table)
library(dtplyr)
library(tidyr)
library(BayesianTools)
library(ggplot2)
library(patchwork)
library(cowplot)
library(ggridges)


source(here::here("R/figure_helpers.R"))
source(here::here("analysis/00_define_setups.R"))

source(here::here("R/calibration_helpers.R"))
source(here::here("R/prediction_helpers.R"))
source(here::here("R/run_prediction_rsofun.R"))


# sample posteriors and run model for each sample parameter set

# df_predict_s1old  <- read_rds("/home/fabian/GitHub/geco-bern/rsofun_doc/df_predict_s1.RDS") # THIS GIVES BIASED RESULTS
if (FALSE){
  ## s0 ----
  tryCatch({
    df_predict_s0  <- read_rds(file.path(rsofun_doc_output_path,"data","predictions_from_WS02/out_predict_N200_train_8000burnin__out_calib__scen0_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds.rds"))
    scatter_predObs_s0  <- plot_all_predVsObs(df_predict_s0)  |> ggsave_and_return("fig_B2b_train_scatter_pred-vs-obs_s0.png",  width = 7.2, height = 4.2);
    rm(scatter_predObs_s0, df_predict_s0); gc()
    # timeseries_s0  <- plot_predobs_gpp_timeseries(df_predict_s0)
    # ggsave_and_return(timeseries_s0,  "fig_B3_pred-vs-obs_s0.png",  width = 7.2, height = 7.2, units = "in", scale = 2.0)
    # rm(timeseries_s0, df_predict_s0); gc()
    }, error = function(e) e
  )

  ## s1 ----
  tryCatch({
    df_predict_s1  <- read_rds(file.path(rsofun_doc_output_path,"data","predictions_from_WS02/out_predict_N200_train_8000burnin__out_calib__scen1_DEzs-100000-0iter_8x3chains_on_CPU8x1.rds.rds"))
    scatter_predObs_s1  <- plot_all_predVsObs(df_predict_s1)  |> ggsave_and_return("fig_B2b_train_scatter_pred-vs-obs_s1.png",  width = 7.2, height = 4.2);
    rm(scatter_predObs_s1, df_predict_s1); gc()
    # timeseries_s1  <- plot_predobs_gpp_timeseries(df_predict_s1)
    # ggsave_and_return(timeseries_s1,  "fig_B3_pred-vs-obs_s1.png",  width = 7.2, height = 7.2, units = "in", scale = 2.0)
    # rm(timeseries_s1, df_predict_s1); gc()
    }, error = function(e) e
  )

  ## s2 ----
  tryCatch({
    df_predict_s2  <- read_rds(file.path(rsofun_doc_output_path,"data","predictions_from_WS02/out_predict_N200_train_8000burnin__out_calib__scen2_DEzs-100000-0iter_10x3chains_on_CPU10x1.rds.rds"))
    scatter_predObs_s2  <- plot_all_predVsObs(df_predict_s2)  |> ggsave_and_return("fig_B2b_train_scatter_pred-vs-obs_s2.png",  width = 7.2, height = 4.2);
    rm(scatter_predObs_s2, df_predict_s2); gc()
    # timeseries_s2  <- plot_predobs_gpp_timeseries(df_predict_s2)
    # ggsave_and_return(timeseries_s2,  "fig_B3_pred-vs-obs_s2.png",  width = 7.2, height = 7.2, units = "in", scale = 2.0)
    # rm(timeseries_s2, df_predict_s2); gc()
    }, error = function(e) e
  )

  ## s3 ----
  tryCatch({
    df_predict_s3  <- read_rds(file.path(rsofun_doc_output_path,"data","predictions_from_WS02/out_predict_N200_train_8000burnin__out_calib__scen3_DEzs-100000-0iter_8x3chains_on_CPU8x1.rds.rds"))
    scatter_predObs_s3  <- plot_all_predVsObs(df_predict_s3)  |> ggsave_and_return("fig_B2b_train_scatter_pred-vs-obs_s3.png",  width = 7.2, height = 4.2);
    rm(scatter_predObs_s3, df_predict_s3); gc()
    # timeseries_s3  <- plot_predobs_gpp_timeseries(df_predict_s3)
    # ggsave_and_return(timeseries_s3,  "fig_B3_pred-vs-obs_s3.png",  width = 7.2, height = 7.2, units = "in", scale = 2.0)
    # rm(timeseries_s3, df_predict_s3); gc()
    }, error = function(e) e
  )

  ## 14 ----
  tryCatch({
    df_predict_s14 <- read_rds(file.path(rsofun_doc_output_path,"data/predictions_from_WS02/out_predict_N200_train_8000burnin__out_calib__scen14_DEzs-50000-0iter_8x3chains_on_CPU8x1_continued.rds.rds"))
    scatter_predObs_s14 <- plot_all_predVsObs(df_predict_s14) |> ggsave_and_return("fig_B2b_train_scatter_pred-vs-obs_s14.png", width = 7.2, height = 4.2);
    rm(df_predict_s14, scatter_predObs_s14); gc()
    # timeseries_s14  <- plot_predobs_gpp_timeseries(df_predict_s14)
    # ggsave_and_return(timeseries_s14,  "fig_B3_pred-vs-obs_s14.png",  width = 7.2, height = 7.2, units = "in", scale = 2.0)
    # rm(timeseries_s14, scatter_predObs_s14); gc()
    }, error = function(e) e
  )

  ## 16 ----
  tryCatch({
    df_predict_s16 <- read_rds(file.path(rsofun_doc_output_path,"data/predictions_from_WS02/out_predict_N200_train_8000burnin__out_calib__scen16_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds.rds"))
    scatter_predObs_s16 <- plot_all_predVsObs(df_predict_s16) |> ggsave_and_return("fig_B2b_train_scatter_pred-vs-obs_s16.png", width = 7.2, height = 4.2);
    rm(df_predict_s16, scatter_predObs_s16); gc()
    # timeseries_s16  <- plot_predobs_gpp_timeseries(df_predict_s16)
    # ggsave_and_return(timeseries_s16,  "fig_B3_pred-vs-obs_s16.png",  width = 7.2, height = 7.2, units = "in", scale = 2.0)
    # rm(timeseries_s16, scatter_predObs_s16); gc()
    }, error = function(e) e
  )

  ## 17 ----
  tryCatch({
    df_predict_s17 <- read_rds(file.path(rsofun_doc_output_path,"data/predictions_from_WS02/out_predict_N200_train_8000burnin__out_calib__scen17_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds.rds"))
    scatter_predObs_s17 <- plot_all_predVsObs(df_predict_s17) |> ggsave_and_return("fig_B2b_train_scatter_pred-vs-obs_s17.png", width = 7.2, height = 4.2);
    rm(df_predict_s17, scatter_predObs_s17); gc()
    # timeseries_s17  <- plot_predobs_gpp_timeseries(df_predict_s17)
    # ggsave_and_return(timeseries_s17,  "fig_B3_pred-vs-obs_s17.png",  width = 7.2, height = 7.2, units = "in", scale = 2.0)
    # rm(timeseries_s17, scatter_predObs_s17); gc()
    }, error = function(e) e
  )

  ## 18 ----
  tryCatch({
    df_predict_s18 <- read_rds(file.path(rsofun_doc_output_path,"data/predictions_from_WS02/out_predict_N200_train_8000burnin__out_calib__scen18_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds.rds"))
    scatter_predObs_s18 <- plot_all_predVsObs(df_predict_s18) |> ggsave_and_return("fig_B2b_train_scatter_pred-vs-obs_s18.png", width = 7.2, height = 4.2);
    rm(df_predict_s18, scatter_predObs_s18); gc()
    # timeseries_s18  <- plot_predobs_gpp_timeseries(df_predict_s18)
    # ggsave_and_return(timeseries_s18,  "fig_B3_pred-vs-obs_s18.png",  width = 7.2, height = 7.2, units = "in", scale = 2.0)
    # rm(timeseries_s18, scatter_predObs_s18); gc()
    }, error = function(e) e
  )


  ## s1 ----
  tryCatch({
    df_predict_test_s1  <- read_rds(file.path(rsofun_doc_output_path,"data/predictions_from_WS02/out_predict_N200_test_8000burnin__out_calib__scen1_DEzs-100000-0iter_8x3chains_on_CPU8x1.rds.rds"))
    scatter_predObs_test_s1  <- plot_all_predVsObs(df_predict_test_s1, rel_widths = c(5,2.3))  |> ggsave_and_return("fig_B2a_test_scatter_pred-vs-obs_s1.png",  width = 7.2, height = 4.2*1.3);
    rm(scatter_predObs_test_s1, df_predict_test_s1); gc()
    # timeseries_test_s1  <- plot_predobs_gpp_timeseries(df_predict_test_s1)
    # ggsave_and_return(timeseries_test_s1,  "fig_B3_pred-vs-obs_test_s1.png",  width = 7.2, height = 7.2, units = "in", scale = 2.0)
    # rm(timeseries_test_s1, df_predict_test_s1); gc()
    }, error = function(e) e
  )

  ## s2 ----
  tryCatch({
    df_predict_test_s2  <- read_rds(file.path(rsofun_doc_output_path,"data/predictions_from_WS02/out_predict_N200_test_8000burnin__out_calib__scen2_DEzs-100000-0iter_10x3chains_on_CPU10x1.rds.rds"))
    scatter_predObs_test_s2  <- plot_all_predVsObs(df_predict_test_s2, rel_widths = c(5,2.3))  |> ggsave_and_return("fig_B2a_test_scatter_pred-vs-obs_s2.png",  width = 7.2, height = 4.2*1.3);
    rm(scatter_predObs_test_s2, df_predict_test_s2); gc()
    # timeseries_test_s2  <- plot_predobs_gpp_timeseries(df_predict_test_s2)
    # ggsave_and_return(timeseries_test_s2,  "fig_B3_pred-vs-obs_test_s2.png",  width = 7.2, height = 7.2, units = "in", scale = 2.0)
    # rm(timeseries_test_s2, df_predict_test_s2); gc()
    }, error = function(e) e
  )

  ## s3 ----
  tryCatch({
    df_predict_test_s3  <- read_rds(file.path(rsofun_doc_output_path,"data/predictions_from_WS02/out_predict_N200_test_8000burnin__out_calib__scen3_DEzs-100000-0iter_8x3chains_on_CPU8x1.rds.rds"))
    scatter_predObs_test_s3  <- plot_all_predVsObs(df_predict_test_s3, rel_widths = c(5,2.3))  |> ggsave_and_return("fig_B2a_test_scatter_pred-vs-obs_s3.png",  width = 7.2, height = 4.2*1.3);
    rm(scatter_predObs_test_s3, df_predict_test_s3); gc()
    # timeseries_test_s3  <- plot_predobs_gpp_timeseries(df_predict_test_s3)
    # ggsave_and_return(timeseries_test_s3,  "fig_B3_pred-vs-obs_test_s3.png",  width = 7.2, height = 7.2, units = "in", scale = 2.0)
    # rm(timeseries_test_s3, df_predict_test_s3); gc()
    }, error = function(e) e
  )

  ## 14 ----
  tryCatch({
    df_predict_test_s14 <- read_rds(file.path(rsofun_doc_output_path,"data/predictions_from_WS02/out_predict_N200_test_8000burnin__out_calib__scen14_DEzs-50000-0iter_8x3chains_on_CPU8x1_continued.rds.rds"))
    scatter_predObs_test_s14 <- plot_all_predVsObs(df_predict_test_s14,rel_widths = c(5,2.3))  |> ggsave_and_return("fig_B2a_test_scatter_pred-vs-obs_s14.png", width = 7.2, height = 4.2*1.3);
    rm(df_predict_test_s14, scatter_predObs_test_s14); gc()
    # timeseries_test_s14  <- plot_predobs_gpp_timeseries(df_predict_test_s14)
    # ggsave_and_return(timeseries_test_s14,  "fig_B3_pred-vs-obs_test_s14.png",  width = 7.2, height = 7.2, units = "in", scale = 2.0)
    # rm(timeseries_test_s14, scatter_predObs_test_s14); gc()
    }, error = function(e) e
  )

  ## 16 ----
  tryCatch({
    df_predict_test_s16 <- read_rds(file.path(rsofun_doc_output_path,"data/predictions_from_WS02/out_predict_N200_test_8000burnin__out_calib__scen16_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds.rds"))
    scatter_predObs_test_s16 <- plot_all_predVsObs(df_predict_test_s16,rel_widths = c(5,2.3))  |> ggsave_and_return("fig_B2a_test_scatter_pred-vs-obs_s16.png", width = 7.2, height = 4.2*1.3);
    rm(df_predict_test_s16, scatter_predObs_test_s16); gc()
    # timeseries_test_s16  <- plot_predobs_gpp_timeseries(df_predict_test_s16)
    # ggsave_and_return(timeseries_test_s16,  "fig_B3_pred-vs-obs_test_s16.png",  width = 7.2, height = 7.2, units = "in", scale = 2.0)
    # rm(timeseries_test_s16, scatter_predObs_test_s16); gc()
    }, error = function(e) e
  )

  ## 17 ----
  tryCatch({
    df_predict_test_s17 <- read_rds(file.path(rsofun_doc_output_path,"data/predictions_from_WS02/out_predict_N200_test_8000burnin__out_calib__scen17_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds.rds"))
    scatter_predObs_test_s17 <- plot_all_predVsObs(df_predict_test_s17,rel_widths = c(5,2.3))  |> ggsave_and_return("fig_B2a_test_scatter_pred-vs-obs_s17.png", width = 7.2, height = 4.2*1.3);
    rm(df_predict_test_s17, scatter_predObs_test_s17); gc()
    # timeseries_test_s17  <- plot_predobs_gpp_timeseries(df_predict_test_s17)
    # ggsave_and_return(timeseries_test_s17,  "fig_B3_pred-vs-obs_test_s17.png",  width = 7.2, height = 7.2, units = "in", scale = 2.0)
    # rm(timeseries_test_s17, scatter_predObs_test_s17); gc()
    }, error = function(e) e
  )

  ## 18 ----
  tryCatch({
    df_predict_test_s18 <- read_rds(file.path(rsofun_doc_output_path,"data/predictions_from_WS02/out_predict_N200_test_8000burnin__out_calib__scen18_DEzs-25000-0iter_8x3chains_on_CPU8x1.rds.rds"))
    scatter_predObs_test_s18 <- plot_all_predVsObs(df_predict_test_s18,rel_widths = c(5,2.3))  |> ggsave_and_return("fig_B2a_test_scatter_pred-vs-obs_s18.png", width = 7.2, height = 4.2*1.3);
    rm(df_predict_test_s18, scatter_predObs_test_s18); gc()
    # timeseries_test_s18  <- plot_predobs_gpp_timeseries(df_predict_test_s18)
    # ggsave_and_return(timeseries_test_s18,  "fig_B3_pred-vs-obs_test_s18.png",  width = 7.2, height = 7.2, units = "in", scale = 2.0)
    # rm(timeseries_test_s18, scatter_predObs_test_s18); gc()
    }, error = function(e) e
  )
}



# scatter_s31 <- plot_predobs_gpp_scatter(df_predict_s31)
# scatter_s32 <- plot_predobs_gpp_scatter(df_predict_s32)
# scatter_s33 <- plot_predobs_gpp_scatter(df_predict_s33)
# scatter_s34 <- plot_predobs_gpp_scatter(df_predict_s34)
# scatter_s35 <- plot_predobs_gpp_scatter(df_predict_s35)
# scatter_s36 <- plot_predobs_gpp_scatter(df_predict_s36)
# scatter_s37 <- plot_predobs_gpp_scatter(df_predict_s37)
# scatter_s38 <- plot_predobs_gpp_scatter(df_predict_s38)
# scatter_s39 <- plot_predobs_gpp_scatter(df_predict_s39)
# scatter_s40 <- plot_predobs_gpp_scatter(df_predict_s40)
# scatter_s41 <- plot_predobs_gpp_scatter(df_predict_s41)
# scatter_s42 <- plot_predobs_gpp_scatter(df_predict_s42)
#
# # cowplot::plot_grid(scatter_s31, scatter_s32)
# scatter_all <- cowplot::plot_grid(
#   scatter_s31,
#   scatter_s32,
#   scatter_s33,
#   scatter_s34,
#   scatter_s35,
#   scatter_s36,
#   scatter_s37,
#   scatter_s38,
#   scatter_s39,
#   scatter_s40,
#   scatter_s41,
#   scatter_s42)
#
# ggsave_and_return(scatter_all, "fig_B2_pred-vs-obs_allsingle.png", width = 7.2, height = 7.2)






  ########## SENSITIVITY ANALYSIS: ########### -

  # Figure D: sensitivity bar plot ----

  # TODO: make sensitivity analysis

  ########## MCMC PLOTS: ########### -
  # burnin_to_skip = 2000
  # Figure A: prior, posterior density plot ----

  # Figure E2: Parameter correlation analysis

  # Figure F: TBD: comparison of calibration vs G enSA?? ----
  ## or just using prior estimates from Stocker 2020? (r.1.14)






