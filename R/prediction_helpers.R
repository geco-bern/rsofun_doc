# Plot raw predictions
## gpp:
#ts_to_plot <- df_B3_timeseries$gpp |> filter(Scenario == "1", dataset == "train"); fpath = here::here("fig","fig_BXY_predObs_gppTimeSeries_s1_train.png")
plot_predobs_gpp_timeseries3 <- function(ts_to_plot, fpath){

  # separate obs
  df_tsplot_gpp_obs <- ts_to_plot |>
    select(sitename, target, date, obs, Scenario, dataset) |>
    distinct()

  # compute stats of sampled distributions before plotting them
  tibble_to_plot <- ts_to_plot |>
    filter(target == "gpp") |>
    lazy_dt() |>
    group_by(Scenario, dataset, parameters, sitename, target, date, model_output_type, y_facet) |>
    summarise(#mod_no_err_p50 = quantile(mod_no_err, 0.5),
      modelled_p50 = quantile(modelled, 0.5),
      modelled_p95 = quantile(modelled, 0.95),
      modelled_p05 = quantile(modelled, 0.05)
    ) |>
    as_tibble() |>
    # and bind back obsevations
    left_join(
      df_tsplot_gpp_obs,
      by = join_by(sitename, target, date, Scenario, dataset))

  n_sites <- tibble_to_plot$sitename |> unique() |> length()

  pl_timeseries_gpp <- ggplot(
    data = tibble_to_plot,
    mapping = aes(x=date, y=modelled_p50)) +
    # Observations underneath (following Cameron 2022)
    geom_point(
      data = function(df) df |> select(-parameters, -model_output_type, -y_facet, -starts_with("modelled")) |> distinct(),
      # data = df_tsplot_gpp_obs, # variant 1, but does not allow %+%-replacement of underlying data
      mapping = aes(y=obs), color = "black", shape = 4, alpha= 0.5, size=0.5) +
    # Structural uncertainty (including error model), a.k.a prediction band
    geom_ribbon(
      alpha=0.5,
      data = function(df) df |> filter(model_output_type == "with struct. uncert."),
      mapping = aes(ymin = modelled_p05, ymax = modelled_p95, fill = "Post.+Error")) +
    # Parametric uncertainty (without error model, only parameter sampling), a.k.a confidence band
    geom_ribbon(
      alpha=0.5,
      data = function(df) df |> filter(model_output_type == "rsofun"),
      mapping = aes(ymin = modelled_p05, ymax = modelled_p95, fill = "Posterior")) +
    geom_line(
      data = function(df) df |> filter(model_output_type == "rsofun"),
      mapping = aes(y = modelled_p50, color = "Posterior")) +
    # layout
    facet_wrap(~sitename, scales = "free_x") +
    theme_classic() + theme(legend.position = "bottom") +
    labs(
      x = NULL, #'Date',
      y = expression(paste("GPP (g C m"^-2, "s"^-1, ")"))
    ) +
    scale_fill_manual(NULL,aesthetics = c("colour", "fill"),
                      breaks = c("Post.+Error",
                                 "Posterior"),
                      # values = c("moccasin",  # colors from Cameron 2022
                      #            "#99333380") # colors from Cameron 2022
                      values = c("Posterior"=t_col("#29a274ff"),
                                 "Post.+Error" = t_col("#777055ff"))
                      # values = c(t_col("tomato", 50),
                      #            t_col("#1b9e77", 0))
    ) +
    scale_x_date(date_breaks = "12 months", date_labels = "%Y-%m")

  # save some variants of this plot:
  # TODO: make this conditional on n_sites
  ggsave(gsub("fig_BXY", "fig_B3a", fpath),
         plot = pl_timeseries_gpp + facet_wrap(~sitename, scales = "free_x", ncol = 2),
         width=7.2, height=7.2*1.5, units = "in", scale = 1)
  ggsave(gsub("fig_BXY", "fig_B3b", fpath),
         plot = pl_timeseries_gpp %+% (pl_timeseries_gpp$data |> group_by(sitename) |> slice(1:(4*365))) +
           facet_wrap(~sitename, scales = "free_x", ncol = 2),
         width=7.2, height=7.2*1.5, units = "in", scale = 1)
  ggsave(gsub("fig_BXY", "fig_B3c", fpath),
         plot = pl_timeseries_gpp %+% (pl_timeseries_gpp$data |> filter(date >= lubridate::ymd("2002-01-01"),
                                                                        date <= lubridate::ymd("2005-12-31"))) +
           facet_wrap(~sitename, scales = "free_x", ncol = 2),
         width=7.2, height=7.2*1.5, units = "in", scale = 1)
  ggsave(gsub("fig_BXY", "fig_B3d", fpath),
         plot = pl_timeseries_gpp %+% (pl_timeseries_gpp$data |> filter(date >= lubridate::ymd("2012-01-01"),
                                                                        date <= lubridate::ymd("2015-12-31"))) +
           facet_wrap(~sitename, scales = "free_x", ncol = 2),
         width=7.2, height=7.2*1.5, units = "in", scale = 1)
}

plot_predobs_gpp_scatter <- function(df_predict){
  df_hexplot_gpp <- df_predict |> unnest(sim) |> filter(!is.na(obs)) |> filter(target == "gpp")

  lims <- round(max(quantile(df_hexplot_gpp$mod_no_err, 0.9999), quantile(df_hexplot_gpp$obs, 0.9999)))
  if (nrow(df_hexplot_gpp)>0){
    gg <- ggplot(df_hexplot_gpp, aes(x=mod_no_err, y=obs)) +
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
  } else {
    gg <- ggplot(tibble(sitename = NA_character_, mod_no_err=NA,obs=NA), aes(x=mod_no_err,y=obs)) +
      facet_wrap(~sitename)
  }
}

## vj and D13C
plot_predobs_vj_D13C_scatter <- function(df_predict, target_selection = c("bigD13C","vj")){
  df_hexplot      <- df_predict |> unnest(sim) |> filter(!is.na(obs)) |> filter(target %in% target_selection)
  if (nrow(df_hexplot)>0){
    gg <- ggplot(df_hexplot, aes(x=mod_no_err, y=obs)) +
      geom_hex(bins = 50, show.legend = FALSE)
  } else {
    gg <- ggplot(tibble(target = target_selection, mod_no_err=NA,obs=NA), aes(x=mod_no_err,y=obs))
  }
  gg <- gg +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
    theme_classic() +
    khroma::scale_fill_batlowW(trans = "log", reverse = TRUE)

  if (length(target_selection) == 1){
    lims_max <- max(quantile(df_hexplot$mod_no_err, 0.9999), quantile(df_hexplot$obs, 0.9999))
    lims_min <- min(quantile(df_hexplot$mod_no_err, 0.0001), quantile(df_hexplot$obs, 0.0001))
    gg <- gg + facet_wrap(~target, ncol=1) +
      coord_fixed() + xlim(lims_min, lims_max) + ylim(lims_min, lims_max)
  } else {
    gg <- gg + facet_wrap(~target, scales = "free", ncol=1)
  }
}

plot_all_predVsObs <- function(df_predict, rel_widths = c(5,2)){
  scatter_plot_gpp     <- plot_predobs_gpp_scatter(df_predict)
  scatter_plot_D13C    <- plot_predobs_vj_D13C_scatter(df_predict, target_selection = "bigD13C")
  scatter_plot_vj      <- plot_predobs_vj_D13C_scatter(df_predict, target_selection = "vj")

  # arrange layouts:
  # testcode with dummy plots:
  # scatter_plot_gpp  <- ggplot(tibble(facet=rep(1:12,10)) |> mutate(x= runif(n()), y=runif(n())), aes(x=x,y=y)) + geom_point() + coord_fixed() + facet_wrap(~facet) + theme_classic()
  # plot_right <- ggplot(tibble(facet=rep(c("vj","bigD13C"),10)) |> mutate(x= runif(n(),10,12), y=runif(n(),25,30)), aes(x=x,y=y)) + geom_point() + facet_wrap(~facet, scales = "free", ncol=1) + theme_classic()
  # ggsave_and_return(cowplot::plot_grid(plot_left + facet_wrap(~facet, labeller = as_labeller(~paste0(.x, ", GPP (gCm-2s-1)"))),
  #                                      plot_right + facet_wrap(~facet, scales = "free", ncol=1, labeller = as_labeller(c("vj"="Vcmax/Jmax (-)","bigD13C" = "Δ13C (permil)"))) + labs(y=NULL),
  #                                      ncol = 2, rel_widths = c(5,2)),
  #                   "fig_B6_pred-vs-obs_s14.png", width = 7.2, height = 4.2)
  # scatter_plot_gpp     <- scatter_plot_gpp     + facet_wrap(~sitename,                        labeller = as_labeller(~paste0(.x, ", GPP (gCm-2s-1)")))
  # scatter_plot_D13C_vj <- plot_predobs_vj_D13C_scatter(df_predict)
  # scatter_plot_D13C_vj <- scatter_plot_D13C_vj + facet_wrap(~target, scales = "free", ncol=1, labeller = as_labeller(c("vj"="Vcmax/Jmax (-)","bigD13C" = "Δ13C (permil)"))) + labs(y=NULL)
  # cowplot::plot_grid(scatter_plot_gpp, scatter_plot_D13C_vj, ncol = 2, rel_widths = rel_widths)

  scatter_plot_gpp  <- scatter_plot_gpp  + labs(x=expression(paste("Predicted (with param. unc.) (g C m"^-2, "s"^-1, ")")),
                                                y=expression(paste("Observed (g C m"^-2, "s"^-1, ")"))) +
    theme(strip.background = element_blank())# facet_wrap(~sitename,       labeller = as_labeller(~paste0(.x, ", GPP")))  +
  scatter_plot_D13C <- scatter_plot_D13C + labs(x="Predicted (with param. unc.) (permil)",  y="Observed (permil)") +
    facet_wrap(~target, ncol=1, labeller = as_labeller(c("bigD13C" = "Δ13C"))) + theme(strip.background = element_blank(), strip.text = element_text(colour = NA))
  scatter_plot_vj   <- scatter_plot_vj   + labs(x="Predicted (with param. unc.) (-)",       y="Observed (-)")      +
    facet_wrap(~target, ncol=1, labeller = as_labeller(c("vj"="Vcmax/Jmax")))  + theme(strip.background = element_blank(), strip.text = element_text(colour = NA))

  tg_list <- cowplot::align_plots(scatter_plot_D13C, scatter_plot_vj)
  cowplot::plot_grid(
    scatter_plot_gpp,
    cowplot::plot_grid(plotlist = tg_list, ncol=1, labels = c("(b) Δ13C:","(c) Vcmax/Jmax:"), hjust = 0),
    ncol = 2, rel_widths = rel_widths, labels = c("(a) GPP:"), hjust = 0)
  # this layout with rel_widths c(5,2) should work for training plot (3x4)+(2x1) of size 7.2, 4.2, 300, 1.6
  # this layout with rel_widths c(5,2.3) should work for testing  plot (6x7)+(2x1) of size 7.2, 4.2*1.3, 300, 1.6
}


