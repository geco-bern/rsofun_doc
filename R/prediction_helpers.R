# Plot raw predictions
## gpp:
plot_predobs_gpp_timeseries <- function(df_predict, N_sample_error = 5){
  df_tsplot_gpp <- df_predict |> unnest(sim) |> filter(target == "gpp")

  df_tsplot_gpp_obs <- df_tsplot_gpp |> select(sitename, target, obs_metadata, obs) |> distinct() |> unnest(obs_metadata)
  df_tsplot_gpp_mod <- df_tsplot_gpp |> select(mcmc_id, sitename, obs_metadata, mod_no_err, err_par_sd) |> unnest(obs_metadata)

  # add observational error:
  N_samples <- tibble(sample_id = 1:N_sample_error)
  df_tsplot_gpp_mod_sampled <- df_tsplot_gpp_mod |>
    dplyr::cross_join(N_samples) |>
    mutate(mod_with_err = mod_no_err + rnorm(n(), sd = err_par_sd))

  df_tsplot_gpp_mod_stat <- df_tsplot_gpp_mod_sampled |> group_by(sitename, date) |>
    summarise(mod_no_err_med = median(mod_no_err),
              mod_no_err_p50 = quantile(mod_no_err, 0.5),
              mod_no_err_p95 = quantile(mod_no_err, 0.95),
              mod_no_err_p05 = quantile(mod_no_err, 0.05),
              mod_with_err_p95 = quantile(mod_with_err, 0.95),
              mod_with_err_p05 = quantile(mod_with_err, 0.05)
    )

  # ggplot(df_tsplot_gpp_obs, aes(x=date, y=obs)) + geom_point() +
  #   facet_wrap(~sitename, scales = "free_x")
  gg <- ggplot(df_tsplot_gpp_mod_stat, aes(x=date, y=mod_no_err_p50)) +
    geom_ribbon(aes(ymin = mod_with_err_p05, ymax = mod_with_err_p95, fill = "Structural uncertainty")) +#, alpha = 0.3) +
    geom_ribbon(aes(ymin = mod_no_err_p05,   ymax = mod_no_err_p95,   fill = "Parameter uncertainty")) +#,   alpha = 0.3) +
    # geom_line() +
    geom_point(data = df_tsplot_gpp_obs, aes(y=obs), color = "black", shape = 4, alpha= 0.5, size=0.5) +
    facet_wrap(~sitename, scales = "free_x") + theme_classic() +
    labs(
      x = 'Date',
      y = expression(paste("GPP (g C m"^-2, "s"^-1, ")"))
    ) +
    scale_fill_manual(NULL,
                      breaks = c("Structural uncertainty",
                                 "Parameter uncertainty"),
                      values = c(t_col("tomato", 50),
                                 t_col("#1b9e77", 0)))
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


