# Script that generates prediction figures for scenario 111 and 113

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

source(here::here("R/figure_helpers.R"))
source(here::here("analysis/00_define_scenarios.R"))

source(here::here("R/calibration_helpers.R"))
source(here::here("R/prediction_helpers.R"))
source(here::here("R/run_prediction_rsofun.R"))


# Figure B2: error distribution predObs scatter plot ----
## for each scenario x target x test

flag_plot_predictions <- TRUE # possibility to switch this off
# define what data to load (and use this as suffix for output)
n_post <- "N20+MAP"
n_err <- "_N3errors"
# outfname_suffix <- paste0(n_post, n_err, "_s94-s91-s90")
outfname_suffix <- paste0(n_post, n_err, "_s113-s111")

if (flag_plot_predictions){
  # Load sampled posterior params used for predictions
  df_113_params  <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_12000burnin__out_calib__scen113_DREAMzs-40000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds"))
  df_111_params  <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_25000burnin__out_calib__scen111_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds"))

  # Load predictions for plotting
  df_113_vj      <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_12000burnin__out_calib__scen113_DREAMzs-40000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds"))
  df_113_bigD13C <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_12000burnin__out_calib__scen113_DREAMzs-40000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds"))
  df_113_gpp     <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_12000burnin__out_calib__scen113_DREAMzs-40000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds"))
  df_111_vj      <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_25000burnin__out_calib__scen111_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds"))
  df_111_bigD13C <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_25000burnin__out_calib__scen111_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds"))
  df_111_gpp     <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_25000burnin__out_calib__scen111_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds"))

  # prepare plotting
  # # i) bind together, ii) mutate(Scenario = "0","1","3")    # for FigB: filter(!is.na(obs)) , for FigB3: filter(target == "gpp")
  dfwide_gpp_train <- bind_rows(
    df_113_gpp |> filter(is_train0_test1 == 0) |> mutate(Scenario = "3"),
    df_111_gpp |> filter(is_train0_test1 == 0) |> mutate(Scenario = "1")
  )
  dfwide_gpp_test <- bind_rows(
    df_113_gpp |> filter(is_train0_test1 == 1) |> mutate(Scenario = "3"),
    df_111_gpp |> filter(is_train0_test1 == 1) |> mutate(Scenario = "1")
  )
  dfwide_vj <- bind_rows(
    df_113_vj |> mutate(Scenario = "3"),
    df_111_vj |> mutate(Scenario = "1")
  )
  dfwide_bigD13C <- bind_rows(
    df_113_bigD13C |> mutate(Scenario = "3"),
    df_111_bigD13C |> mutate(Scenario = "1")
  )
  dfwide_gpp_train |> select(              date, sitename, target) |> distinct() # 90k observations
  dfwide_gpp_test  |> select(              date, sitename, target) |> distinct() # 127k observations
  dfwide_vj        |> select(genus,species,year, sitename, target) |> distinct() # 590 observations
  dfwide_bigD13C   |> select(      species,year, sitename, target) |> distinct() # 2347 observations
  rm(df_113_vj)
  rm(df_111_vj)
  rm(df_113_bigD13C)
  rm(df_111_bigD13C)
  rm(df_113_gpp)
  rm(df_111_gpp)

  # make data sets long for plotting
  make_long <- function(dfwide){
    dfwide |>
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
    select(posterior_sample_id, error_sample_id, Scenario, is_train0_test1, is_MAP, sitename, target,
           obs,                 date, # these are target specific observation_metadata
           mod_no_err, mod_biasremoved_no_err, mod_biasremoved_with_err) |>
    make_long()
  dflong_gpp_test <- dfwide_gpp_test |>
    select(posterior_sample_id, error_sample_id, Scenario, is_train0_test1, is_MAP, sitename, target,
           obs,                 date, # these are target specific observation_metadata
           mod_no_err, mod_biasremoved_no_err, mod_biasremoved_with_err) |>
    make_long()
  dflong_vj <- dfwide_vj |>
    select(posterior_sample_id, error_sample_id, Scenario, is_train0_test1, is_MAP, sitename, target,
           obs, genus, species, year, # these are target specific observation_metadata
           mod_no_err, mod_biasremoved_no_err, mod_biasremoved_with_err) |>
    make_long()
  dflong_bigD13C <- dfwide_bigD13C |>
    select(posterior_sample_id, error_sample_id, Scenario, is_train0_test1, is_MAP, sitename, target,
           obs,        species, year, # these are target specific observation_metadata
           mod_no_err, mod_biasremoved_no_err, mod_biasremoved_with_err) |>
    make_long()
  rm(dfwide_gpp_train)
  rm(dfwide_gpp_test)
  rm(dfwide_vj)
  rm(dfwide_bigD13C)

  # manually define what to show as output depending on the scenario:
  df_B1and2and3 <- list(
    # for gpp:
    gpp = bind_rows(dflong_gpp_test, dflong_gpp_train) |>
      # remove the bias-corrected values for gpp since we did not fit a bias
      filter(!(model_output_type %in% c("bias-corrected"))) |>
      # select what to plot and how to name it
      mutate(y_facet = case_when(
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
  df_B1and2and3_avgObs <- list(
    gpp = df_B1and2and3$gpp |> rename(obs_avg = obs) |> mutate(obs_sd  = NA, obs_n   = 1),
    vj  = df_B1and2and3$vj |>
      group_by(posterior_sample_id, error_sample_id, Scenario, dataset, parameters, sitename, target, model_output_type, y_facet) |>
      # group_by(-c('genus', 'species', 'year')) |>
      summarise(obs_avg  = mean(obs), obs_sd  = sd(obs), obs_n   = n(),
                # for the sampled error model the error was sampled independently for each observations. Taking mean would be wrong: just take first sample
                # mod_mean  = mean(modelled), mod_sd  = sd(modelled), mod_n   = length(unique(modelled))
                modelled = first(modelled)
                ),
    bigD13C = df_B1and2and3$bigD13C |>
    group_by(posterior_sample_id, error_sample_id, Scenario, dataset, parameters, sitename, target, model_output_type, y_facet) |>
    # group_by(-c('genus', 'species', 'year')) |>
    summarise(obs_avg  = mean(obs), obs_sd  = sd(obs), obs_n   = n(),
              # for the sampled error model the error was sampled independently for each observations. Taking mean would be wrong: just take first sample
              # mod_mean  = mean(modelled), mod_sd  = sd(modelled), mod_n   = length(unique(modelled))
                modelled = first(modelled)
              )
  )
  # df_B1and2and3_avgObs$gpp
  # df_B1and2and3_avgObs$bigD13C
  # df_B1and2and3_avgObs$vj
  df_B1_density_avgObs <- lapply(df_B1and2and3_avgObs, \(df) df |> filter(!is.na(obs_avg)))                    # remove NA observations
  df_B2_scatter_avgObs <- lapply(df_B1and2and3_avgObs, \(df) df |> filter(!is.na(obs_avg), dataset == "test")) # remove NA observations and test data set
  # df_B1_density$bigD13C |> filter(sitename == "lon_-111.80_lat_+040.77") |> filter(posterior_sample_id==1, error_sample_id==1, y_facet == "Posterior", Scenario %in% c(3,4))
  # df_B1_density_avgObs$bigD13C |> filter(sitename == "lon_-111.80_lat_+040.77") |> filter(posterior_sample_id==1, error_sample_id==1, y_facet == "Posterior", Scenario %in% c(3,4))



  ## Figure B2: pred-vs-obs scatter plot ----
  ## for each scenario x target x test
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
                    paste0("fig_B2_pred-vs-avgObs_s1_s4_pred",outfname_suffix,".png"),
                    width = 12, height = 6, units = "cm", scale = 2)
  # alternative for train dataset:
  pl_scatter_all_train <- cowplot::plot_grid(
    pl_scatter_gpp     %+% filter(df_B1_density_avgObs$gpp,     dataset == "train"),
    pl_scatter_vj      %+% filter(df_B1_density_avgObs$vj,      dataset == "train"),
    pl_scatter_bigD13C %+% filter(df_B1_density_avgObs$bigD13C, dataset == "train"),
    nrow = 1
  )
  ggsave_and_return(pl_scatter_all_train,
                    paste0("fig_B2_pred-vs-avgObs_s1_s4_pred",outfname_suffix,"_train.png"),
                    width = 12, height = 6, units = "cm", scale = 2)





  pl_scatter_gpp_bysite_test <- pl_scatter_gpp %+% (pl_scatter_gpp$data |> filter(y_facet == "Posterior", Scenario %in% c("4","3"))) +
    facet_wrap(~sitename+dataset, ncol=13)
  # pl_scatter_gpp_bysite_train<- pl_scatter_gpp %+% (df_B1and2and3$gpp |> filter(!is.na(obs_avg), dataset =="train") |>
  #                       filter(y_facet == "Posterior", Scenario %in% c("4","3"))) +
  #   facet_wrap(~sitename+dataset, ncol=6)
  # ggsave_and_return(pl_scatter_gpp_bysite_test,
  #                   paste0("fig_B2b_pred-vs-avgObs_s4_test-sites",outfname_suffix,".png"),
  #                   width = 12, height = 10, units = "cm", scale = 2)
  # ggsave_and_return(pl_scatter_gpp_bysite_train,
  #                   paste0("fig_B2b_pred-vs-avgObs_s4_train-sites",outfname_suffix,".png"),
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
  ggsave(here::here(file.path("fig",paste0("fig_B1b_predObs_errorDensity_s1s4",outfname_suffix,".png"))),
         plot = pl_density_alltargets_v3_avgObs, width=12, height=8, units = "cm", scale = 1.3)

  # and combine both, arranging axes:
  # Extend x-axis limits of pl_density_alltargets_v3_avgObs to be the same as pl_density_alltargets_v3
  # by using a geom_blank() layer (source: https://stackoverflow.com/a/21585521/3915004) :
  pl_density_alltargets_v3_build <- ggplot2::ggplot_build(pl_density_alltargets_v3)
  dummy <- data.frame(
    target = c("gpp", "gpp",
               # "vj_obs__", "vj_obs__",
               # "bigD13C_obs_permil", "bigD13C_obs_permil") |>
               "vj", "vj",
               "bigD13C", "bigD13C") |>
      factor(levels = c("gpp", "vj", "bigD13C")),
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
  ggsave(here::here(file.path("fig",paste0("fig_B1c_predObs_errorDensity_s1s4",outfname_suffix,".png"))),
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




  ## Figure B3: make a proper gpp time series plot ----
  plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario == "1",          dataset == "train"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s1_train",outfname_suffix,".png")))
  plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario %in% c("4","3"), dataset == "train"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s4or3_train",outfname_suffix,".png")))

  plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario == "1",          dataset == "test"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s1_test",outfname_suffix,".png")))
  plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario %in% c("4","3"), dataset == "test"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s4or3_test",outfname_suffix,".png")))

  plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario == "0", dataset == "train"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s0_train",outfname_suffix,".png")))
  plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario == "0", dataset == "test"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s0_test",outfname_suffix,".png")))
}
