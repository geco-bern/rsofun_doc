## copied from sofunCalVal package (and adapted to analyse_modobs3)
analyse_modobs3 <- function(df,
                            mod,
                            obs,
                            type = "points",
                            filnam = NA,
                            relative = FALSE,
                            lower_xlim = 0,
                            use_factor = NULL,
                            shortsubtitle = FALSE,
                            plot_subtitle = TRUE,
                            plot_linmod = TRUE,
                            pal = "batlowW",
                            ...) {
  require(ggplot2)
  require(dplyr)
  # require(LSD)
  require(ggthemes)
  # require(RColorBrewer)

  # if (identical(filnam, NA)) filnam <- "analyse_modobs.pdf"

  ## rename to 'mod' and 'obs' and remove rows with NA in mod or obs
  df <- df %>%
    as_tibble() %>%
    ungroup() %>%
    dplyr::select(dplyr::all_of(c(mod = mod, obs = obs))) %>%
    tidyr::drop_na(mod, obs)

  ## get linear regression (coefficients)
  linmod <- lm(obs ~ mod, data = df)

  ## construct metrics table using the 'yardstick' library
  df_metrics <- df %>%
    yardstick::metrics(obs, mod) %>%
    dplyr::bind_rows(tibble(.metric = "n", .estimator = "standard", .estimate = summarise(df, numb = n()) %>% unlist())) %>%
    dplyr::bind_rows(tibble(.metric = "slope", .estimator = "standard", .estimate = coef(linmod)[2])) %>%
    # dplyr::bind_rows( tibble( .metric = "nse",      .estimator = "standard", .estimate = hydroGOF::NSE( obs, mod, na.rm=TRUE ) ) ) %>%
    dplyr::bind_rows(tibble(.metric = "mean_obs", .estimator = "standard", .estimate = summarise(df, mean = mean(obs, na.rm = TRUE)) %>% unlist())) %>%
    dplyr::bind_rows(tibble(
      .metric = "prmse", .estimator = "standard",
      .estimate = dplyr::filter(., .metric == "rmse") %>% dplyr::select(.estimate) %>% unlist() /
        dplyr::filter(., .metric == "mean_obs") %>%
        dplyr::select(.estimate) %>%
        unlist()
    )) %>%
    dplyr::bind_rows(tibble(
      .metric = "pmae", .estimator = "standard",
      .estimate = dplyr::filter(., .metric == "mae") %>% dplyr::select(.estimate) %>% unlist() /
        dplyr::filter(., .metric == "mean_obs") %>%
        dplyr::select(.estimate) %>%
        unlist()
    )) %>%
    dplyr::bind_rows(tibble(.metric = "bias", .estimator = "standard", .estimate = dplyr::summarise(df, mean((mod - obs), na.rm = TRUE)) %>% unlist())) %>%
    dplyr::bind_rows(tibble(.metric = "pbias", .estimator = "standard", .estimate = dplyr::summarise(df, mean((mod - obs) / obs, na.rm = TRUE)) %>% unlist()))

  rsq_val <- df_metrics %>%
    dplyr::filter(.metric == "rsq") %>%
    dplyr::select(.estimate) %>%
    unlist() %>%
    unname()
  rmse_val <- df_metrics %>%
    dplyr::filter(.metric == "rmse") %>%
    dplyr::select(.estimate) %>%
    unlist() %>%
    unname()
  mae_val <- df_metrics %>%
    dplyr::filter(.metric == "mae") %>%
    dplyr::select(.estimate) %>%
    unlist() %>%
    unname()
  bias_val <- df_metrics %>%
    dplyr::filter(.metric == "bias") %>%
    dplyr::select(.estimate) %>%
    unlist() %>%
    unname()
  slope_val <- df_metrics %>%
    dplyr::filter(.metric == "slope") %>%
    dplyr::select(.estimate) %>%
    unlist() %>%
    unname()
  n_val <- df_metrics %>%
    dplyr::filter(.metric == "n") %>%
    dplyr::select(.estimate) %>%
    unlist() %>%
    unname()

  if (relative) {
    rmse_val <- rmse_val / mean(df$obs, na.rm = TRUE)
    bias_val <- bias_val / mean(df$obs, na.rm = TRUE)
  }

  rsq_lab <- format(rsq_val, digits = 2)
  rmse_lab <- format(rmse_val, digits = 3)
  mae_lab <- format(mae_val, digits = 3)
  bias_lab <- format(bias_val, digits = 3)
  slope_lab <- format(slope_val, digits = 3)
  n_lab <- format(n_val, digits = 3)

  results <- tibble(rsq = rsq_val, rmse = rmse_val, mae = mae_val, bias = bias_val, slope = slope_val, n = n_val)

  if (shortsubtitle) {
    subtitle <- bquote(italic(R)^2 == .(rsq_lab) ~ ~
                         RMSE == .(rmse_lab))
  } else {
    subtitle <- bquote(italic(R)^2 == .(rsq_lab) ~ ~
                         RMSE == .(rmse_lab) ~ ~
                         bias == .(bias_lab) ~ ~
                         slope == .(slope_lab) ~ ~
                         italic(N) == .(n_lab))
  }

  if (type == "hex") {

    upper_xlim <- round(max(quantile(df$mod, 0.9999), quantile(df$obs, 0.9999)))
    stopifnot(is.null(lower_xlim) || (is.numeric(lower_xlim) && length(lower_xlim)==1))
    lower_xlim = ifelse(is.null(lower_xlim),
                        round(min(quantile(df$mod, 0.0001), quantile(df$obs, 0.0001))),
                        lower_xlim)

    ## ggplot hexbin
    gg <- df %>%
      ggplot2::ggplot(aes(x = mod, y = obs)) +
      geom_hex(bins = 50, show.legend = FALSE) +
      geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
      # geom_hline(yintercept = 0, linetype = "dotted") +
      # geom_vline(xintercept = 0, linetype = "dotted") +
      coord_fixed() +
      xlim(lower_xlim, upper_xlim) +
      ylim(lower_xlim, upper_xlim) +
      theme_classic() +
      labs(x = mod, y = obs)

    if (pal == "batlowW"){
      gg <- gg + khroma::scale_fill_batlowW(trans = "log", reverse = TRUE)
    } else if (pal == "davos"){
      gg <- gg + khroma::scale_fill_davos(trans = "log", reverse = TRUE)
    }

    if (plot_subtitle) gg <- gg + labs(subtitle = subtitle)
    if (plot_linmod) gg <- gg + geom_smooth(method = "lm", color = "red", linewidth = 0.5, se = FALSE)

    if (!identical(filnam, NA)) {
      ggsave(filnam, width = 5, height = 5)
    }
  }

  return(list(df_metrics = df_metrics, gg = gg, linmod = linmod, results = results))
}
