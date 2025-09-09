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


source(here::here("R/calibration_helpers.R"))
source(here::here("R/prediction_helpers.R"))
source(here::here("R/run_prediction_rsofun.R"))

source(here::here("R/figure_helpers.R"))
source(here::here("analysis/00_define_scenarios.R"))


# sample posteriors and run model for each sample parameter set
# renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen14_DEzs-35000-0iter_8x3chains_on_CPU8x1_continued.rds", "8000", "200", "8", "3"))
# renv::run("analysis/04_make-test-train_predictions.R", project = "../rsofun_doc", args = c("out_calib__scen15_DEzs-15000-0iter_8x3chains_on_CPU8x1_continued.rds", "4000", "200", "8", "3"))

# t0 <- Sys.time()
# # df_predict_s1  <- readRDS("df_predict_s1.RDS")
# df_predict_s31 <- run_prediction_rsofun(out_calib_s31, "train", burnin_to_skip,n_samples = 200, n_cores = 1); readr::write_rds(df_predict_s31,"df_predict_s31.RDS") # 500 samples on 12 cores: 5 minutes
# df_predict_s32 <- run_prediction_rsofun(out_calib_s32, "train", burnin_to_skip,n_samples = 200, n_cores = 1); readr::write_rds(df_predict_s32,"df_predict_s32.RDS") # 500 samples on 12 cores: 5 minutes
# df_predict_s33 <- run_prediction_rsofun(out_calib_s33, "train", burnin_to_skip,n_samples = 200, n_cores = 1); readr::write_rds(df_predict_s33,"df_predict_s33.RDS") # 500 samples on 12 cores: X minutes
# df_predict_s34 <- run_prediction_rsofun(out_calib_s34, "train", burnin_to_skip,n_samples = 200, n_cores = 1); readr::write_rds(df_predict_s34,"df_predict_s34.RDS") # 500 samples on 12 cores: X minutes
# df_predict_s35 <- run_prediction_rsofun(out_calib_s35, "train", burnin_to_skip,n_samples = 200, n_cores = 1); readr::write_rds(df_predict_s35,"df_predict_s35.RDS") # 500 samples on 12 cores: X minutes
# df_predict_s36 <- run_prediction_rsofun(out_calib_s36, "train", burnin_to_skip,n_samples = 200, n_cores = 1); readr::write_rds(df_predict_s36,"df_predict_s36.RDS") # 500 samples on 12 cores: X minutes
# # df_predict_s37 <- run_prediction_rsofun(out_calib_s37, "train", burnin_to_skip,n_samples = 200, n_cores = 1); readr::write_rds(df_predict_s37,"df_predict_s37.RDS") # 500 samples on 12 cores: X minutes
# df_predict_s38 <- run_prediction_rsofun(out_calib_s38, "train", burnin_to_skip,n_samples = 200, n_cores = 1); readr::write_rds(df_predict_s38,"df_predict_s38.RDS") # 500 samples on 12 cores: X minutes
# df_predict_s39 <- run_prediction_rsofun(out_calib_s39, "train", burnin_to_skip,n_samples = 200, n_cores = 1); readr::write_rds(df_predict_s39,"df_predict_s39.RDS") # 500 samples on 12 cores: X minutes
# df_predict_s40 <- run_prediction_rsofun(out_calib_s40, "train", burnin_to_skip,n_samples = 200, n_cores = 1); readr::write_rds(df_predict_s40,"df_predict_s40.RDS") # 500 samples on 12 cores: X minutes
# df_predict_s41 <- run_prediction_rsofun(out_calib_s41, "train", burnin_to_skip,n_samples = 200, n_cores = 1); readr::write_rds(df_predict_s41,"df_predict_s41.RDS") # 500 samples on 12 cores: X minutes
# df_predict_s42 <- run_prediction_rsofun(out_calib_s42, "train", burnin_to_skip,n_samples = 200, n_cores = 1); readr::write_rds(df_predict_s42,"df_predict_s42.RDS") # 500 samples on 12 cores: X minutes
# t1 <- Sys.time()
# print(t1-t0)
# NOTE: no error term has (yet) been added

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



if (TRUE){
  # This is to do some manual plots:

  ########## PREDICTION PLOTS: ########### -

  ### s1: ----
  df_predict_s91       <- read_rds(file.path(rsofun_doc_output_path,"data","predictions","out_predict_N100_train_8000burnin__out_calib__scen91_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds.rds"))
  df_predict_test_s91  <- read_rds(file.path(rsofun_doc_output_path,"data","predictions","out_predict_N100_test_8000burnin__out_calib__scen91_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds.rds"))
  df_predict_s91MAP       <- read_rds(file.path(rsofun_doc_output_path,"data","predictions","out_predict_MAP_train_8000burnin__out_calib__scen91_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds.rds_MAP.rds"))
  df_predict_test_s91MAP  <- read_rds(file.path(rsofun_doc_output_path,"data","predictions","out_predict_MAP_test_8000burnin__out_calib__scen91_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds.rds_MAP.rds"))
  ### s2: ----
  df_predict_s92       <- read_rds(file.path(rsofun_doc_output_path,"data","predictions","out_predict_N100_train_8000burnin__out_calib__scen92_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds.rds"))
  df_predict_test_s92  <- read_rds(file.path(rsofun_doc_output_path,"data","predictions","out_predict_N100_test_8000burnin__out_calib__scen92_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds.rds"))
  ### s3: ----
  df_predict_s93       <- read_rds(file.path(rsofun_doc_output_path,"data","predictions","out_predict_N100_train_8000burnin__out_calib__scen93_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds.rds"))
  df_predict_test_s93  <- read_rds(file.path(rsofun_doc_output_path,"data","predictions","out_predict_N100_test_8000burnin__out_calib__scen93_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds.rds"))
    ### s4: ----
  df_predict_s94       <- read_rds(file.path(rsofun_doc_output_path,"data","predictions","out_predict_N100_train_8000burnin__out_calib__scen94_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds.rds"))
  df_predict_test_s94  <- read_rds(file.path(rsofun_doc_output_path,"data","predictions","out_predict_N100_test_8000burnin__out_calib__scen94_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds.rds"))
  df_predict_s94MAP       <- read_rds(file.path(rsofun_doc_output_path,"data","predictions","out_predict_MAP_train_8000burnin__out_calib__scen94_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds.rds_MAP.rds"))
  df_predict_test_s94MAP  <- read_rds(file.path(rsofun_doc_output_path,"data","predictions","out_predict_MAP_test_8000burnin__out_calib__scen94_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds.rds_MAP.rds"))

  # df_predict_s96       <- read_rds(file.path(rsofun_doc_output_path,"data","predictions","out_predict_N200_train_8000burnin__out_calib__scen96_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds.rds"))
  # df_predict_test_s96  <- read_rds(file.path(rsofun_doc_output_path,"data","predictions","out_predict_N200_test_8000burnin__out_calib__scen96_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds.rds"))
  # df_predict_s97       <- read_rds(file.path(rsofun_doc_output_path,"data","predictions","out_predict_N200_train_8000burnin__out_calib__scen97_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds.rds"))
  # df_predict_test_s97  <- read_rds(file.path(rsofun_doc_output_path,"data","predictions","out_predict_N200_test_8000burnin__out_calib__scen97_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds.rds"))
  # df_predict_s98       <- read_rds(file.path(rsofun_doc_output_path,"data","predictions","out_predict_N200_train_8000burnin__out_calib__scen98_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds.rds"))
  # df_predict_test_s98  <- read_rds(file.path(rsofun_doc_output_path,"data","predictions","out_predict_N200_test_8000burnin__out_calib__scen98_DEzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds.rds"))

  ### combine: ----
  dat_to_plot <- bind_rows(
    df_predict_s91      |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "1", dataset = "train") |> unnest(sim) |> filter(!is.na(obs)),# |> select(-obs_metadata),
    df_predict_test_s91 |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "1", dataset = "test" ) |> unnest(sim) |> filter(!is.na(obs)),# |> select(-obs_metadata),
    df_predict_s91MAP                              |> select(mcmc_id, sim) |> mutate(Scenario = "1", dataset = "train") |> unnest(sim) |> filter(!is.na(obs)),
    df_predict_test_s91MAP                         |> select(mcmc_id, sim) |> mutate(Scenario = "1", dataset = "test")  |> unnest(sim) |> filter(!is.na(obs)),
    df_predict_s92      |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "2", dataset = "train") |> unnest(sim) |> filter(!is.na(obs)),# |> select(-obs_metadata),
    df_predict_test_s92 |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "2", dataset = "test" ) |> unnest(sim) |> filter(!is.na(obs)),# |> select(-obs_metadata),
    df_predict_s93      |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "3", dataset = "train") |> unnest(sim) |> filter(!is.na(obs)),# |> select(-obs_metadata),
    df_predict_test_s93 |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "3", dataset = "test" ) |> unnest(sim) |> filter(!is.na(obs)),# |> select(-obs_metadata),
    df_predict_s94      |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "4", dataset = "train") |> unnest(sim) |> filter(!is.na(obs)),# |> select(-obs_metadata),
    df_predict_test_s94 |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "4", dataset = "test" ) |> unnest(sim) |> filter(!is.na(obs)),# |> select(-obs_metadata)
    df_predict_s94MAP                              |> select(mcmc_id, sim) |> mutate(Scenario = "4", dataset = "train") |> unnest(sim) |> filter(!is.na(obs)),
    df_predict_test_s94MAP                         |> select(mcmc_id, sim) |> mutate(Scenario = "4", dataset = "test")  |> unnest(sim) |> filter(!is.na(obs))
    # df_predict_s96      |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "96", dataset = "train") |> unnest(sim) |> filter(!is.na(obs)),# |> select(-obs_metadata),
    # df_predict_test_s96 |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "96", dataset = "test" ) |> unnest(sim) |> filter(!is.na(obs)),# |> select(-obs_metadata),
    # df_predict_s97      |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "97", dataset = "train") |> unnest(sim) |> filter(!is.na(obs)),# |> select(-obs_metadata),
    # df_predict_test_s97 |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "97", dataset = "test" ) |> unnest(sim) |> filter(!is.na(obs)),# |> select(-obs_metadata),
    # df_predict_s98      |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "98", dataset = "train") |> unnest(sim) |> filter(!is.na(obs)),# |> select(-obs_metadata),
    # df_predict_test_s98 |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "98", dataset = "test" ) |> unnest(sim) |> filter(!is.na(obs))# |> select(-obs_metadata)
  ) |>
    lazy_dt()
          # readr::write_rds(x = dat_to_plot, file = here::here(file.path("fig","figB_predObs_errorDensity_s1s2s3s14.rds"))) # to save intermediate
          # readr::write_rds(x = dat_to_plot, file = here::here(file.path("fig","figB_predObs_errorDensity_s1s14.rds"))) # to save intermediate

  dat_to_plot_inclNAObs <- bind_rows( # keeping is.na(obs) is good for plotting e.g. continuous time series of gpp
    df_predict_s91      |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "1", dataset = "train") |> unnest(sim),# |> select(-obs_metadata),
    df_predict_test_s91 |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "1", dataset = "test" ) |> unnest(sim),# |> select(-obs_metadata),
    df_predict_s91MAP                              |> select(mcmc_id, sim) |> mutate(Scenario = "1", dataset = "train") |> unnest(sim),
    df_predict_test_s91MAP                         |> select(mcmc_id, sim) |> mutate(Scenario = "1", dataset = "test")  |> unnest(sim),
    # df_predict_s92      |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "2", dataset = "train") |> unnest(sim),# |> select(-obs_metadata),
    # df_predict_test_s92 |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "2", dataset = "test" ) |> unnest(sim),# |> select(-obs_metadata),
    # df_predict_s93      |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "3", dataset = "train") |> unnest(sim),# |> select(-obs_metadata),
    # df_predict_test_s93 |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "3", dataset = "test" ) |> unnest(sim),# |> select(-obs_metadata),
    df_predict_s94      |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "4", dataset = "train") |> unnest(sim),# |> select(-obs_metadata),
    df_predict_test_s94 |> filter(mcmc_id%in%1:10) |> select(mcmc_id, sim) |> mutate(Scenario = "4", dataset = "test" ) |> unnest(sim),# |> select(-obs_metadata)
    df_predict_s94MAP                              |> select(mcmc_id, sim) |> mutate(Scenario = "4", dataset = "train") |> unnest(sim),
    df_predict_test_s94MAP                         |> select(mcmc_id, sim) |> mutate(Scenario = "4", dataset = "test")  |> unnest(sim)
  ) |>
    lazy_dt()



  # TODO: B: make a proper comparison density plot facetting only by target

  # TODO: simulate error to plot as second density plots (no-fill, dashed lines)
  # add observational error:
  # readr::write_rds(dat_to_plot_inclNAObs, "dat_to_plot_inclNAObs_forDevelopment1.RDS")
  # readr::write_rds(dat_to_plot, "dat_to_plot_forDevelopment4.RDS")
  # dat_to_plot <- readr::read_rds("dat_to_plot_forDevelopment3.RDS")
  N_sample_error <- 3 # TODO: increase to 10
  # N_samples <- tibble(sample_id = 1:N_sample_error)
  dat_to_plot_sampled <- dat_to_plot |>
    lazy_dt() |>   # use lazy data.table for speed
    group_by(sitename, mcmc_id, target) |> slice(1:min(n(),720)) |> ungroup() |> # TODO: just for development. to lower computational requirements. TODO: remove this line again
    # filter(target != "gpp") |> # TODO: just for development. to lower computational requirements. TODO: remove this line again
    # TODO: just for development. to lower computational requirements. TODO: remove this line again
    filter(mcmc_id == 1) |> #select(Scenario, dataset, target, obs, err_par, predNoErr_minus_obs) |> # err_par_sd
    group_by(err_par_sd) |>
    # following line is basically a cross_join:
    mutate(Nrow=n()) |> slice(rep(1:.N, each = N_sample_error)) |> mutate(sample_id = rep(1:N_sample_error, times = unique(Nrow))) |> select(-Nrow) |>
    # sample the error
    rename(mod_biased_no_err      = mod_no_err) |>
    mutate(err_sample             = rnorm(n(), sd = err_par_sd),
           mod_unbiased_no_err    = mod_biased_no_err - err_par_bias,
           mod_unbiased_with_err  = mod_biased_no_err - err_par_bias + err_sample) |>
    # compute deviations
    mutate(predBiasedNoErr_minus_obs     = mod_biased_no_err - obs,
           predUnbiasedNoErr_minus_obs   = mod_unbiased_no_err - obs,
           predUnbiasedWithErr_minus_obs = mod_unbiased_with_err - obs) |>
    as_tibble() # to access results of lazy computation

  ## Figure B: error distribution density plot ----
  ## for each scenario x target x test+train
  pl_bias <- ggplot(data = dat_to_plot_sampled, mapping = aes(y = Scenario)) +
    #x=predBiasedNoErr_minus_obs,
    ggridges::geom_density_ridges(mapping = aes(x=predUnbiasedNoErr_minus_obs, fill = dataset)) +
    ggridges::geom_density_ridges(mapping = aes(x=predUnbiasedWithErr_minus_obs, color = dataset), fill = NA, linetype = "dashed") +
    # layout:
    facet_wrap( ~ target , nrow = 1, scales = "free_x") +
    theme_classic() +
    theme(legend.position = "bottom") + labs(x=NULL) +
    scale_fill_manual(NULL,aesthetics = c("colour","fill"), values = c(test = "#29a274ff", train = t_col("#777055ff"))) # GECO colors
  pl_bias2 <- pl_bias +
    facet_wrap( ~ target , nrow = 1, scales = "free_x",
                labeller = as_labeller(c("gpp"="(a) GPP:", "vj"="(b) Vcmax/Jmax:", "bigD13C"="(c) Δ13C:"))) +
    theme(strip.background = element_blank(), strip.text = element_text(hjust = 0, size = 16, face = "bold"))
  # pl_bias + cowplot::draw_plot_label(c("(a)"), x = 0, y = 1)
  ggsave(here::here(file.path("fig","figB_predObs_errorDensity_s71s72s73s74_test.png")),
         plot = pl_bias2, width=12, height=8, units = "cm", scale = 1.3)

  # alternative approach
  make_plot <- function(df, xlab = NULL, crop_percentiles = c(0.01, 0.99)){
    # lims <- quantile(df$predUnbiasedNoErr_minus_obs, crop_percentiles)
    lims <- quantile(df$predUnbiasedWithErr_minus_obs, crop_percentiles)
    gg <- ggplot(df,
                 aes(#x=predBiasedNoErr_minus_obs,
                     y = Scenario,
                     fill = dataset)) +
      # geom_density() + theme_classic() + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
      ggridges::geom_density_ridges(mapping = aes(x=predUnbiasedNoErr_minus_obs, fill = dataset)) +
      ggridges::geom_density_ridges(mapping = aes(x=predUnbiasedWithErr_minus_obs, color = dataset), fill = NA, linetype = "dashed") +
      theme_classic() +
      facet_wrap( ~ target, ncol = 1, scales = "free_x") + theme(strip.background = element_blank(), strip.text = element_text(colour = NA)) +
      labs(y = NULL, x = xlab, color = "Scenario", linetype = NULL) +
      coord_cartesian(xlim = lims) +
      # scico::scale_color_scico_d(NULL, palette = "batlow")
      scale_fill_manual(NULL,aesthetics = c("colour","fill"), values = c(test = "#29a274ff", train = t_col("#777055ff"))) # GECO colors
    if (nrow(df)==0){gg <- NULL}
    return(gg)
  }

  dat_to_plot_gpp <- dat_to_plot_sampled |> filter(mcmc_id == 1) |> filter(target == "gpp")
  dat_to_plot_vj  <- dat_to_plot_sampled |> filter(mcmc_id == 1) |> filter(target == "vj")
  dat_to_plot_D13 <- dat_to_plot_sampled |> filter(mcmc_id == 1) |> filter(target == "bigD13C")

  gg_gpp <- make_plot(as_tibble(dat_to_plot_gpp), xlab = expression(paste("(g C m"^-2, "s"^-1, ")")))
  gg_vj  <- make_plot(as_tibble(dat_to_plot_vj),  xlab = expression(paste("(-)")))
  gg_D13 <- make_plot(as_tibble(dat_to_plot_D13), xlab = expression(paste("(permil)")))

  plotlist <- list(gg_gpp,gg_vj,gg_D13) |> lapply(\(pl)pl + theme(legend.position = "none")) #|> purrr::compact()
  legend <- cowplot::get_legend(plotlist[[1]] + theme(legend.position = "bottom"))
  # plots  <- cowplot::plot_grid(
  #   plotlist = c(plotlist, list(legend)),
  #   labels = c("(a) GPP:", "(b) Vcmax/Jmax:", "(c) Δ13C:"), hjust = 0,
  #   ncol = 1,
  #   rel_heights = c(rep(1, length(plotlist)), (length(plotlist))/(6-1))) # make legend 1/4

  plots_horizontal  <- cowplot::plot_grid(
    plotlist = plotlist,
    labels = c("(a) GPP:", "(b) Vcmax/Jmax:", "(c) Δ13C:"), hjust = 0,
    nrow = 1)

  # ggsave(here::here(file.path("fig","figB_predObs_errorDensity_s1s2s3s14.png")),
  #        plot = plots, width=3.6, height=7.2, units = "in", scale = 1)
  # ggsave(here::here(file.path("fig","figB_predObs_errorDensity_s1s2s3s14_test.png")),
  #        plot = pl_bias2, width=12, height=8, units = "cm", scale = 1.3)



  # TODO: B2: make a proper comparison scatter plot facetting only by target

  # TODO: B3: make a proper gpp time series plot
  ts_plot_s1 <- plot_predobs_gpp_timeseries(
    df_predict_s1, N_sample_error = 10,
    fpath = here::here(file.path("fig","fig_BXX_predObs_gppTimeSeries_s1.png")))
  ts_plot_test_s1 <- plot_predobs_gpp_timeseries(
    df_predict_test_s1, N_sample_error = 10,
    fpath = here::here(file.path("fig","fig_BXX_predObs_gppTimeSeries_test_s1.png")))

  ts_plot_s74 <- plot_predobs_gpp_timeseries(
    df_predict_s74, N_sample_error = 10,
    fpath = here::here(file.path("fig","fig_BXX_predObs_gppTimeSeries_s74.png")))
  ts_plot_test_s74 <- plot_predobs_gpp_timeseries(
    df_predict_test_s74, N_sample_error = 10,
    fpath = here::here(file.path("fig","fig_BXX_predObs_gppTimeSeries_test_s74.png")))

  # readr::write_rds(x = dat_to_plot, file = here::here(file.path("fig","figB_predObs_errorDensity_s1s2s3s14.rds"))) # to save intermediate
  # TODO:
  # add here a mod_with_err and then compute the predWithErr_minus_obs distributions and plot both

  # TODO: B2: make a proper comparison plot facetting only by target

  # TODO: start sensitivity


  ### make plot: ----


  ########## SENSITIVITY ANALYSIS: ########### -

  # Figure D: sensitivity bar plot ----

  # TODO: make sensitivity analysis

  ########## MCMC PLOTS: ########### -
  # burnin_to_skip = 2000
  # Figure A: prior, posterior density plot ----

  # Figure E2: Parameter correlation analysis

  # Figure F: TBD: comparison of calibration vs G enSA?? ----
  ## or just using prior estimates from Stocker 2020? (r.1.14)

}






# scatter_s1  <- plot_predobs_gpp_scatter(df_predict_s1)
# scatter_s14 <- plot_predobs_gpp_scatter(df_predict_s14)
# scatter_s32 <- plot_predobs_gpp_scatter(df_predict_s32)
# scatter_s56 <- plot_predobs_gpp_scatter(df_predict_s56)
#
#
# ggsave_and_return(scatter_s1, "fig_B2_pred-vs-obs_s1.png", width = 7.2, height = 7.2)
# ggsave_and_return(scatter_s14, "fig_B2_pred-vs-obs_s14.png", width = 7.2, height = 7.2)
#
# timeseries_s1  <- plot_predobs_gpp_timeseries(df_predict_s1)
# ggsave_and_return(timeseries_s1,  "fig_B3_pred-vs-obs_s1.png",  width = 7.2, height = 7.2, units = "in", scale = 2.0)
#
# timeseries_s14 <- plot_predobs_gpp_timeseries(df_predict_s14)
# ggsave_and_return(timeseries_s14, "fig_B3_pred-vs-obs_s14.png", width = 7.2, height = 7.2, units = "in", scale = 2.0)
#
#
#
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
