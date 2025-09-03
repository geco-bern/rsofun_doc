# Script running sensitivity analysis

# Load libraries
library(rsofun)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sensitivity)
library(BayesianTools)

# Read data produced with 01_sample_sites.R
res <- setup_rsofun_calibration(scenario = 1)

drivers  <- select(res$drivobs, sitename, run_model, params_siml, site_info, forcing)
obs      <- select(res$drivobs, sitename, run_model, targets, data)
settings <- list(
  method = "BayesianTools",
  metric = cost_likelihood_pmodel_bigD13C_vj_gpp,
  control = list(
    sampler = "DEzs",
    settings = list(
      burnin     = burnin,                 # 10000,
      iterations = iterations,             # 50000,
      nrChains   = NA,                     # number of independent chains
      startValue = n_chains_within_sampler # number of internal chains to be sampled
    ),
    n_chains_independent      = n_chains_independent,
    n_parallel_independent    = n_parallel_independent,
    n_parallel_within_sampler = n_parallel_within_sampler
  ),
  par = res$par
)
# suffix = ""
sensitivity_sofun <- function(
    drivers,
    obs,
    settings#,
    # optim_out = TRUE, # whether to return chains
    # suffix = "", # for storing rds
    # outpath = here::here("data"), logpath = "",
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

  if (settings$control$n_parallel_independent > 1){ # parallel sensitivity analysis:
    stop("Parallel sensitivity not implemented")
  } else { # sequential sensitivity analysis:

    # setup the bayesian sampling
    # bayesianSetup
    morrisSetup <- createBayesianSetup(
      likelihood = ll_factory(obs, drivers, parnames, get_mod_obs = get_mod_obs_pmodel_bigD13C_vj_gpp),# , ...),
      prior      = priors,
      names      = parnames,
      parallel   = FALSE) # TODO...

    # # since sequential sampling, let runMCMC handle the actual number of chains
    # settings$control$settings$nrChains <- settings$control$n_chains_independent
    # # calculate the runs
    # mcmc_out <- BayesianTools::runMCMC(
    #   bayesianSetup = bayesianSetup,
    #   sampler       = settings$control$sampler,
    #   settings      = settings$control$settings
    # )

    set.seed(432)
    morrisOut <- tidyr::tibble()
    morrisOut <- sensitivity::morris(
      model   = morrisSetup$posterior$density,
      factors = morrisSetup$names,
      # r = 2,
      # design = list(type = "oat", levels = 3, grid.jump = 1),
      r = 1000,
      design = list(type = "oat", levels = 20, grid.jump = 3),
      binf = morrisSetup$prior$lower,
      bsup = morrisSetup$prior$upper,
      scale = TRUE
      )
    #
    # # Summarise the morris output into statistics
    # morrisOut.df <- data.frame(
    #   parameter = names(par_cal_best),
    #   mu.star = apply(abs(morrisOut$ee), 2, mean, na.rm = T),
    #   sigma = apply(morrisOut$ee, 2, sd, na.rm = T)
    # ) %>%
    #   arrange( mu.star )
    #
    # # Create barplot to show sensitivity analysis output
    # gg <- morrisOut.df |>
    #   tidyr::pivot_longer( -parameter, names_to = "variable", values_to = "value") |>
    #   ggplot(aes(
    #     reorder(parameter, value),
    #     value,
    #     fill = variable),
    #     color = NA) +
    #   geom_bar(position = position_dodge(), stat = 'identity') +
    #   scale_fill_manual("",
    #                     labels = c('mu.star' = expression(mu * "*"),
    #                                'sigma' = expression(sigma)),
    #                     values = c('mu.star' = "#29a274ff",
    #                                'sigma' = "#777055ff")) +
    #   theme_classic() +
    #   theme(
    #     axis.text = element_text(size = 6),
    #     axis.title = element_blank(),
    #     legend.position = c(0.9, 0.1), legend.justification = c(0.95, 0.05)
    #   ) +
    #   coord_flip()    # make horizontal
    #
    # ggsave(here::here("fig/morris.pdf"), plot = gg, width = 5, height = 3)
  }

  ## Postprocess: ----
  end_time <- Sys.time()

  ## Build return object: 'return_value' ----

}
