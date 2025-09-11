# Runs the requested model sensitivity analysis, stores to data-folder and returns results
sensitivity_sofun_serialized <- function(
    drivers,
    obs,
    settings,
    suffix = "",        # for storing results (rds and plot)
    outpath             # for storing results (rds only; plots are hardcoded to './fig')
    # ...
){
  print(paste0(Sys.time(),": start sensitivity analyisis of ", suffix))
  #--- Bayesiantools ----

  ## Preprocess: ----
  # parse prior distributions of parameters
  parnames <- names(settings$par)
  source(here::here("R/createMixedPrior.R"))
  priors  <- createMixedPrior(settings$par)

  # Your external data
  # drivers
  # obs

  # sampler needs a function ll(random_par) for the likelihood,
  # since data is provided as a closure (drivers, obs) we need a function factory to be able
  # create this function on each worker

  # make available get_mod_obs_pmodel_bigD13C_vj_gpp so we can export it to workers
  source(here::here("R/cost_likelihood_pmodel_bigD13C_vj_gpp.R"))

  ll_factory <- function(obs, drivers, parnames, get_mod_obs, ...){
    function(random_par){
      eval(settings$metric)(par = as.list(setNames(random_par, parnames)), # NOTE: for morris we need as.list() to transform the vector to a named list
                            obs = obs,
                            drivers = drivers,
                            get_mod_obs = get_mod_obs,
                            ...)
    }
  }

  ## Run the sensitivity: ----
  start_time <- Sys.time()

  # if (settings$control$n_parallel_independent > 1){ # parallel sensitivity analysis:
  #   stop("Parallel sensitivity not implemented")
  # } else { # sequential sensitivity analysis:

  # setup the bayesian sampling
  morrisSetup <- createBayesianSetup(
    likelihood = ll_factory(obs, drivers, parnames, get_mod_obs = get_mod_obs_pmodel_bigD13C_vj_gpp),# , ...),
    prior      = priors,
    names      = parnames,
    parallel   = FALSE) # TODO...

  # define lower and upper limit of parameters: 'binf_arg' and 'bsup_arg'
  #    this behavior is defined by the value of `settings$control$par_ranges`
  #    it can either be the string "prior" or then a data.frame with column names:
  #    c("parameter_name", "lower", "upper") that contains the ranges to all parameters
  stopifnot(
    (is.character(settings$control$par_ranges) && settings$control$par_ranges == "prior") ||
    (is.data.frame(settings$control$par_ranges) && all(names(settings$control$par_ranges) %in% c("parameter_name", "lower", "upper")))
  )

  if (is.character(settings$control$par_ranges) && settings$control$par_ranges == "prior"){
    # get range from prior
    binf_bsup <- getPriorMinMaxRanges(morrisSetup$prior, settings$par)
    stopifnot(length(binf_bsup$inflim) == length(settings$par))
    stopifnot(all(names(binf_bsup$inflim) == names(settings$par)))
    binf_arg <- binf_bsup$inflim
    bsup_arg <- binf_bsup$suplim

  } else if (is.data.frame(settings$control$par_ranges) &&
             all(names(settings$control$par_ranges) %in% c("parameter_name", "lower", "upper"))) {
    # get range from specified data.frame (e.g. derived from posterior)
    par_ranges <- settings$control$par_ranges
    stopifnot(length(par_ranges$lower) == length(settings$par))
    stopifnot(all(par_ranges$parameter_name == names(settings$par))) # this expects same ordering
    binf_arg <- par_ranges$lower
    bsup_arg <- par_ranges$upper

  } else {

    stop(
      "Recieved unknown argument in 'settings$control$par_ranges'.",
      "\n",
      paste0(capture.output(print(settings$control$par_ranges)), collapse = "\n"))
  }

  # run morris sensitivity
  # morrisSetup$posterior$density(binf_arg) # run this to test whether likelihood will work with a named vector of parameter values
  # morrisSetup$posterior$density(bsup_arg) # run this to test whether likelihood will work with a named vector of parameter values
  set.seed(432)
  morrisOut <- sensitivity::morris(
    model   = morrisSetup$posterior$density,
    factors = morrisSetup$names,
    r       = settings$control$settings$iterations,
    design  = settings$control$settings$design,
    binf    = binf_arg,
    bsup    = bsup_arg,
    scale   = TRUE
  )

  ## Postprocess: ----
  end_time <- Sys.time()
  print(end_time - start_time)

  ## Build return object: 'return_value' ----
  # Summarise the morris output into statistics
  morris_stats <- data.frame(
    parameter = morrisSetup$names,
    mu.star   = apply(abs(morrisOut$ee), 2, mean, na.rm = T),
    sigma     = apply(morrisOut$ee,      2, sd,   na.rm = T)
  ) |>
    arrange( mu.star )


  # Create barplot to show sensitivity analysis output
  gg <- morris_stats |>
    tidyr::pivot_longer( -parameter, names_to = "variable", values_to = "value") |>
    ggplot(aes(
      reorder(parameter, value),
      value,
      fill = variable),
      color = NA) +
    geom_bar(position = position_dodge(), stat = 'identity') +
    # layout
    coord_flip() +     # make horizontal
    scale_fill_manual(
      "",
      labels = c('mu.star' = expression(mu * "*"),
                 'sigma' = expression(sigma)),
      values = c('mu.star' = "#29a274ff",
                 'sigma' = "#777055ff")) +
    theme_classic() +
    theme(
      axis.text = element_text(size = 6),
      axis.title = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.95, 0.05),
      legend.justification = c(1.0, 0),
    )

  # Save plot
  ggsave(here::here("fig", paste0("fig_D_sensitivity_",suffix,".pdf")), plot = gg, width = 5, height = 3, units = "in")
  ggsave(here::here("fig", paste0("fig_D_sensitivity_",suffix,".png")), plot = gg, width = 5, height = 3, units = "in")
  ggsave(here::here("fig", paste0("fig_D2_sensitivity_",suffix,".png")), plot = gg, width = 8, height = 4.8, units = "cm")
  ggsave(here::here("fig", paste0("fig_D3_sensitivity_",suffix,".png")),
         plot = gg %+% filter(gg$data, !grepl("^((err)|(errbias))_",parameter)), # remove the error parameters from the plot
         width = 8, height = 4.8, units = "cm")

  # Save RDS and return value:
  return_value <- list(stats   = morris_stats,
                       raw_out = morrisOut,
                       raw_in  = morrisSetup,
                       morrisplot = gg)

  rds_fname <- file.path(outpath, "sensitivity", paste0("fig_D_morris_",suffix,".rds"))
  if(!dir.exists(dirname(rds_fname))){dir.create(dirname(rds_fname))}
  readr::write_rds(return_value, file = rds_fname)

  return(return_value)
}
