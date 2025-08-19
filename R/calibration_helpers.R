get_calibration_settings_str <- function(out_calib) {
  # defines a filename-compatible description of calibration settings
  # from 'out_calib', i.e. an output object of calib_sofun()
  stopifnot(is(out_calib$mod, "mcmcSamplerList"))

  # explore what's in a mcmcSamplerList:
  # summary(out_calib$mod)
  # plot(out_calib$mod)
  individual_chains <- out_calib$mod
  nrChains <- length(individual_chains) # number of chains

  # plot(individual_chains[[1]]) # chain 1
  # plot(individual_chains[[2]]) # chain 2
  # plot(individual_chains[[3]]) # chain 3
  # class(individual_chains[[1]]$setup); individual_chains[[1]]$setup # Bayesian Setup
  # individual_chains[[1]]$chain
  # individual_chains[[1]]$X
  # individual_chains[[1]]$Z

  nrInternalChains <- lapply(
    individual_chains,
    function(curr_chain){curr_chain$settings$startValue})  |>
    # function(curr_chain){length(curr_chain$chain)})  |>
    unlist() |>
    unique() |>
    paste0(collapse = "-")

  nrIterations <- lapply(
    individual_chains,
    function(curr_chain){curr_chain$settings$iterations})|>
    unlist() |>
    unique() |>
    paste0(collapse = "-")

  nrBurnin <- lapply(
    individual_chains,
    function(curr_chain){curr_chain$settings$burnin})    |>
    unlist() |>
    unique() |>
    paste0(collapse = "-")

  sampler_name <- lapply(
    individual_chains,
    function(curr_chain){curr_chain$settings$sampler})   |>
    unlist() |>
    unique() |>
    paste0(collapse = "-")

  # create descriptive string of settings for filename
  return(
    sprintf(
      "%s_%s_%s-%siter_%sx%schains",
      out_calib$name,
      sampler_name,
      nrIterations,
      nrBurnin,
      nrChains,
      nrInternalChains
    )
  )
}



# Bayesian calibration output
getSetup <- function(x) {
  classes <- class(x)
  if (any(c('mcmcSampler', 'smcSampler') %in% classes)) x$setup
  else if (any(c('mcmcSamplerList', 'smcSamplerList') %in% classes)) x[[1]]$setup
  else stop('Can not get setup from x')
}
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color

  ## Get RGB values for named color
  rgb.val <- col2rgb(color)

  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)

  ## Save the color
  invisible(t.col)
}
plot_prior_posterior_density <- function(x){
  require(BayesianTools)

  # Get matrices of prior and posterior samples
  posteriorMat <- getSample(x, parametersOnly = TRUE)
  priorMat <-  getSetup(x)$prior$sampler(10000) # nPriorDraws = 10000

  # Parameter names
  parNames <- colnames(posteriorMat)
  # rename columns priorMat
  colnames(priorMat) <- parNames

  # Create data frame for plotting
  df_plot <- rbind(
    data.frame(posteriorMat, distrib = "posterior"),
    data.frame(priorMat, distrib = "prior")
  )
  df_plot$distrib <- as.factor(df_plot$distrib)

  # Plot with facet wrap
  gg <- df_plot |> tibble() |>
    pivot_longer(-c(distrib), names_to = "variable") |>
    mutate(variable = forcats::fct_inorder(variable)) |> # order by appearance
    ggplot(
      aes(x = value, fill = distrib)
    ) +
    geom_density() +
    theme_classic() +
    facet_wrap( ~ variable , nrow = 2, scales = "free") +
    theme(
      legend.position = "bottom",
      axis.title.x = element_text("")
    ) +
    scale_fill_manual(NULL, values = c("#29a274ff", t_col("#777055ff"))) # GECO colors

  return(gg)
}
