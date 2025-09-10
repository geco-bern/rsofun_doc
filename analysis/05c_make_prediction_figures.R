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
source(here::here("analysis/00_define_scenarios.R"))

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



if (TRUE){
  # define what data to load (and use this as suffix for output)
  n_post <- "N20+MAP"
  n_err <- "_N3errors"
  outfname_suffix <- paste0(n_post, n_err, "_s94-s91-s90")

  # Load sampled posterior params used for predictions
  df_94_params  <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen94_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds")))
  df_91_params  <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen91_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds")))
  df_90_params  <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen90_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds")))
  # Load predictions for plotting
  df_94_vj      <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen94_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds")))
  df_91_vj      <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen91_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds")))
  df_90_vj      <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen90_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds")))
  df_94_bigD13C <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen94_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds")))
  df_91_bigD13C <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen91_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds")))
  df_90_bigD13C <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen90_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds")))
  df_94_gpp     <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen94_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds")))
  df_91_gpp     <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen91_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds")))
  df_90_gpp     <- readr::read_rds(file.path(rsofun_doc_output_path, "data", "predictions", paste0("out_predict_",n_post,"_18000burnin__out_calib__scen90_DEzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds")))

  # prepare plotting
  # # i) bind together, ii) mutate(Scenario = "0","1","4")    # for FigB: filter(!is.na(obs)) , for FigB3: filter(target == "gpp")
  dfwide_gpp_train <- bind_rows(
    df_94_gpp |> filter(is_train0_test1 == 0) |> mutate(Scenario = "4"),
    df_91_gpp |> filter(is_train0_test1 == 0) |> mutate(Scenario = "1"),
    df_90_gpp |> filter(is_train0_test1 == 0) |> mutate(Scenario = "0")
  )
  dfwide_gpp_test <- bind_rows(
    df_94_gpp |> filter(is_train0_test1 == 1) |> mutate(Scenario = "4"),
    df_91_gpp |> filter(is_train0_test1 == 1) |> mutate(Scenario = "1"),
    df_90_gpp |> filter(is_train0_test1 == 1) |> mutate(Scenario = "0")
  )
  dfwide_vj <- bind_rows(
    df_94_vj |> mutate(Scenario = "4"),
    df_91_vj |> mutate(Scenario = "1"),
    df_90_vj |> mutate(Scenario = "0")
  )
  dfwide_bigD13C <- bind_rows(
    df_94_bigD13C |> mutate(Scenario = "4"),
    df_91_bigD13C |> mutate(Scenario = "1"),
    df_90_bigD13C |> mutate(Scenario = "0")
  )
  dfwide_gpp_train |> select(              date, sitename, target) |> distinct() # 90k observations
  dfwide_gpp_test  |> select(              date, sitename, target) |> distinct() # 127k observations
  dfwide_vj        |> select(genus,species,year, sitename, target) |> distinct() # 590 observations
  dfwide_bigD13C   |> select(      species,year, sitename, target) |> distinct() # 2347 observations
  rm(df_94_vj)
  rm(df_91_vj)
  rm(df_90_vj)
  rm(df_94_bigD13C)
  rm(df_91_bigD13C)
  rm(df_90_bigD13C)
  rm(df_94_gpp)
  rm(df_91_gpp)
  rm(df_90_gpp)

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


  # define what to show as output depending on the scenario:
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
        Scenario == "1" & model_output_type == "rsofun" &               parameters == "MAP"       ~ "MAP",       # since we did not fit a bias, we don't have a bias correction
        Scenario == "1" & model_output_type == "rsofun" &               parameters == "Posterior" ~ "Posterior", # since we did not fit a bias, we don't have a bias correction
        # Scenario == "1" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error", # since bigD13C was not fitted we don't have an error model
        # all else is not plotted
        TRUE ~ "remove") |> factor(levels = c("MAP","Posterior","Post.+Error"))) |>
      filter(y_facet != "remove")
  )

  df_B1_density    <- lapply(df_B1and2and3, \(df) df |> filter(!is.na(obs)))       # remove NA observations
  df_B2_scatter    <- lapply(df_B1and2and3, \(df) df |> filter(!is.na(obs),
                                                               dataset == "test")) # remove NA observations and test data set
  df_B3_timeseries <- df_B1and2and3["gpp"] # keep NA observations and test data set, only use gpp

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




  # individual
  # ggsave_and_return(pl_scatter_gpp,     "fig_B2_pred-vs-obs_s1-to-s4_gpp.png",     width = 12, height = 8, units = "cm", scale = 2)
  # ggsave_and_return(pl_scatter_vj,      "fig_B2_pred-vs-obs_s1-to-s4_vj.png",      width = 12, height = 8, units = "cm", scale = 2)
  # ggsave_and_return(pl_scatter_bigD13C, "fig_B2_pred-vs-obs_s1-to-s4_bigD13C.png", width = 12, height = 8, units = "cm", scale = 2)

  # ggsave_and_return(pl_scatter_gpp     %+% filter(pl_scatter_gpp$data,     dataset == "test", parameters == "MAP"), "fig_B2_pred-vs-obs_s1-to-s4_MAP_gpp.png",     width = 12, height = 8, units = "cm", scale = 2)
  # ggsave_and_return(pl_scatter_vj      %+% filter(pl_scatter_vj$data,      dataset == "test", parameters == "MAP"), "fig_B2_pred-vs-obs_s1-to-s4_MAP_vj.png",      width = 12, height = 8, units = "cm", scale = 2)
  # ggsave_and_return(pl_scatter_bigD13C %+% filter(pl_scatter_bigD13C$data, dataset == "test", parameters == "MAP"), "fig_B2_pred-vs-obs_s1-to-s4_MAP_bigD13C.png", width = 12, height = 8, units = "cm", scale = 2)

  # combined
  pl_scatter_all <- cowplot::plot_grid(pl_scatter_gpp, pl_scatter_vj, pl_scatter_bigD13C, nrow = 1)
  ggsave_and_return(pl_scatter_all,
                    paste0("fig_B2_pred-vs-obs_s1_s4_pred",outfname_suffix,".png"),
                    width = 12, height = 6, units = "cm", scale = 2)
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
  pl_scatter_gpp_bysite_test <- pl_scatter_gpp %+% (pl_scatter_gpp$data |> filter(y_facet == "Posterior", Scenario == "4")) +
    facet_wrap(~sitename+dataset, ncol=13)
  pl_scatter_gpp_bysite_train<- pl_scatter_gpp %+% (df_B1and2and3$gpp |> filter(!is.na(obs), dataset =="train") |>
                        filter(y_facet == "Posterior", Scenario == "4")) +
    facet_wrap(~sitename+dataset, ncol=6)
  ggsave_and_return(pl_scatter_gpp_bysite_test,
                    paste0("fig_B2b_pred-vs-obs_s4_test-sites",outfname_suffix,".png"),
                    width = 12, height = 10, units = "cm", scale = 2)
  ggsave_and_return(pl_scatter_gpp_bysite_train,
                    paste0("fig_B2b_pred-vs-obs_s4_train-sites",outfname_suffix,".png"),
                    width = 12, height = 10, units = "cm", scale = 2)

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
    # add MAP (dashed):
    ggridges::geom_density_ridges( data = function(df) {df |> filter(y_facet == "MAP")},
      mapping = aes(color = dataset, linetype = "MAP"),
      scale = 0.8, fill = NA, key_glyph = "timeseries") + # "polygon"
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
  pl_density_alltargets_v3 <- pl_density_alltargets +
    aes(y=Scenario) + scale_y_discrete(limits = rev) +
    facet_grid(~target, scales = "free_x",
               labeller = as_labeller(c("gpp"="(a) GPP:", "vj"="(b) Vcmax/Jmax:", "bigD13C"="(c) Δ13C:"))) +
    theme(strip.background = element_blank(), strip.text = element_text(hjust = 0, size = 12, face = "bold"))
  ggsave(here::here(file.path("fig",paste0("fig_B_predObs_errorDensity_s1s4",outfname_suffix,".png"))),
         plot = pl_density_alltargets_v3, width=12, height=8, units = "cm", scale = 1.3)
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






  ## Figure B3: make a proper gpp time series plot ----
  plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario == "1", dataset == "train"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s1_train",outfname_suffix,".png")))
  plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario == "4", dataset == "train"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s4_train",outfname_suffix,".png")))

  plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario == "1", dataset == "test"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s1_test",outfname_suffix,".png")))
  plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario == "4", dataset == "test"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s4_test",outfname_suffix,".png")))

  plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario == "0", dataset == "train"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s0_train",outfname_suffix,".png")))
  plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario == "0", dataset == "test"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s0_test",outfname_suffix,".png")))

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

  # TODO: make sensitivity analysis

  ########## MCMC PLOTS: ########### -
  # burnin_to_skip = 2000
  # Figure A: prior, posterior density plot ----

  # Figure E2: Parameter correlation analysis

  # Figure F: TBD: comparison of calibration vs G enSA?? ----
  ## or just using prior estimates from Stocker 2020? (r.1.14)

}





