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
          # debug(cost_likelihood_pmodel_bigD13C_vj_gpp)
          # settings <- list(
          #   method = "BayesianTools",
          #   metric = cost_likelihood_pmodel_bigD13C_vj_gpp,
          #   control = list(
          #     sampler = "DEzs",
          #     settings = list(
          #       burnin     = burnin,                 # 10000,
          #       iterations = iterations,             # 50000,
          #       nrChains   = NA,                     # number of independent chains
          #       startValue = n_chains_within_sampler # number of internal chains to be sampled
          #     ),
          #     n_chains_independent      = 1,
          #     n_parallel_independent    = 1,
          #     n_parallel_within_sampler = 1
          #   ),
          #   par = res$par
          # )
  ll_factory <- function(obs, drivers, parnames, get_mod_obs, ...){
    function(random_par){
      eval(settings$metric)(par = as.list(setNames(random_par, parnames)), # TODO: for morris we need as.list()
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
  # bayesianSetup
  morrisSetup <- createBayesianSetup(
    likelihood = ll_factory(obs, drivers, parnames, get_mod_obs = get_mod_obs_pmodel_bigD13C_vj_gpp),# , ...),
    prior      = priors,
    names      = parnames,
    parallel   = FALSE) # TODO...

  set.seed(432)
  # define lower and upper limit of parameters
  binf_bsup <- getPriorMinMaxRanges(morrisSetup$prior, settings$par)
  stopifnot(length(binf_bsup$inflim) == length(settings$par))
  stopifnot(all(names(binf_bsup$inflim) == names(settings$par)))

  # arguments to morris sensitivity analysis:
  targetFunction <- morrisSetup$posterior$density
  factors        <- morrisSetup$names
  r              <- settings$control$settings$iterations
  design         <- list(type = "oat", levels = 20, grid.jump = 3) # settings$control$settings$design # TODO
  binf           <- binf_bsup$inflim
  bsup           <- binf_bsup$suplim

  # targetFunction(binf) # to test whether likelihood will work with a named vector
  # targetFunction(bsup) # to test whether likelihood will work with a named vector

  morrisOut <- sensitivity::morris(
    model   = targetFunction,
    factors = factors,
    r       = r,
    design  = design,
    binf    = binf,
    bsup    = bsup,
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
