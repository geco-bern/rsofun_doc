calib_sofun_parallelized <- function(
    drivers,
    obs,
    settings,
    optim_out = TRUE, # whether to return chains
    suffix = "", # for storing rds
    outpath = here::here("data"), logpath = "",
    ...
){
  print(paste0(Sys.time(),": start sampling of ", suffix))

  # backwards compatibility: set default values of parallelization options
  # by default do three chains
  if(is.null(settings$control$n_chains_independent)){    settings$control$n_chains_independent <- 3}
  # by default activate parallelization of independent chains, but deactivate within-sampler paralellization
  if(is.null(settings$control$n_parallel_independent)){  settings$control$n_parallel_independent <- settings$control$n_chains_independent}
  if(is.null(settings$control$n_parallel_within_sampler)){settings$control$n_parallel_within_sampler <- 1}

  if(settings$control$n_parallel_within_sampler==1){
    settings$control$n_parallel_within_sampler <- FALSE
  } # When set to 1 we want to deactivate parallel running.
  #   Unfortunately runMCMC interprets 1 as TRUE (leading parallelization to n_cores - 1)
  #   Thus we need to set it manually to FALSE.


  #--- Bayesiantools ----
  if (tolower(settings$method) == "bayesiantools"){

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
        eval(settings$metric)(par = setNames(random_par, parnames),
                              obs = obs,
                              drivers = drivers,
                              get_mod_obs = get_mod_obs,
                              ...)
      }
    }


    ## Run the MCMC sampler: ----

    require(BayesianTools)
    start_time <- Sys.time()

    if (settings$control$n_parallel_independent > 1){ # parallel MCMC sampler:

      if (logpath != "") {
        cl <- makeCluster(settings$control$n_parallel_independent, outfile = logpath)
      } else {
        cl <- makeCluster(settings$control$n_parallel_independent)
      }
      registerDoParallel(cl)

      if (settings$control$n_parallel_independent != settings$control$n_chains_independent){
        warning(sprintf(
          "Requested %d indep. chains, but ran %d indep. chains as `n_parallel_independent` takes precedence.",
          settings$control$n_chains_independent, settings$control$n_parallel_independent)
        )
      }
      # since parallel sampling, fix the number of chains of runMCMC to 1, but call it multiple times
      settings$control$settings$nrChains <- 1

      indep_chains <- foreach(
        i = 1:settings$control$n_parallel_independent,
        .packages=c('BayesianTools','rsofun','dplyr','tidyr','lubridate'),
        .export = c('get_mod_obs_pmodel_bigD13C_vj_gpp'),
        .verbose = TRUE
      ) %dopar% {

        set.seed(1982 + i) # set a different seed on each worker
        bayesianSetup <- createBayesianSetup(

          # inside worker: rebuild the closure so it picks up 'obs', 'drivers', 'parnames', 'get_mod_obs_pmodel_bigD13C_vj_gpp'
          likelihood = ll_factory(obs, drivers, parnames, get_mod_obs = get_mod_obs_pmodel_bigD13C_vj_gpp, ...),
          prior      = priors,
          names      = parnames,
          parallel   = settings$control$n_parallel_within_sampler)
        BayesianTools::runMCMC(
          bayesianSetup = bayesianSetup,
          sampler       = settings$control$sampler,
          settings      = settings$control$settings
        )
      }
      stopCluster(cl)
      mcmc_out <- createMcmcSamplerList(indep_chains) # combine the independent chains

    } else { # sequential MCMC sampler:

      # setup the bayesian sampling
      bayesianSetup <- createBayesianSetup(
        likelihood = ll_factory(obs, drivers, parnames, get_mod_obs = get_mod_obs_pmodel_bigD13C_vj_gpp, ...),
        prior      = priors,
        names      = parnames,
        parallel   = settings$control$n_parallel_within_sampler)

      # since sequential sampling, let runMCMC handle the actual number of chains
      settings$control$settings$nrChains <- settings$control$n_chains_independent
      # calculate the runs
      mcmc_out <- BayesianTools::runMCMC(
        bayesianSetup = bayesianSetup,
        sampler       = settings$control$sampler,
        settings      = settings$control$settings
      )
    }


    ## Postprocess: ----

    # ensure return value 'mcmc_out' is a mcmcSamplerList even if n_chains_independent==1
    # (by default runMCMC returns only a mcmcSampler if n_chains_independent==1)
    if(is(mcmc_out, "mcmcSampler")){
      mcmc_out <- createMcmcSamplerList(list(mcmc_out)) # now mcmc_out is a mcmcSamplerList
    }

    end_time <- Sys.time()


    ## Build return object: 'return_value' ----

    # Extract MAP (maximum a posteriori value) of parameters
    bt_par <- BayesianTools::MAP(mcmc_out)$parametersMAP

    return_value <- list(par = bt_par)

    if (optim_out){ # append raw MCMC chains
      return_value <- c(return_value, list(mod = mcmc_out))
    }
    # if (input_out){ # append MCMC input
    #   return_value <- c(return_value,
    #                     list(bayesianSetup = bayesianSetup,             # unneded: return_value$mod[[1]]$setup
    #                          sampler       = settings$control$sampler,  # unneded: return_value$mod[[1]]$sampler
    #                          settings      = settings$control$settings))# unneded: return_value$mod[[1]]$settings
    # }

    # append timing information
    return_value$walltime <- end_time - start_time
    return_value$runtime  <- get_runtime_numeric(return_value)
    # summary(return_value)
    # plot(return_value$mod)
    print(get_runtime_numeric(return_value))
    print(get_walltime(return_value))

    return_value$name <- suffix

    ## Store results to file: ----
    return_value$fpath <- file.path(outpath, "calibrations", paste0("out_calib_", suffix, ".rds"))
    write_rds(return_value, file = return_value$fpath, compress = "xz")


  } else {
    stop("Unknown method passed to calib_sofun().")
  }

  print(paste0(Sys.time(),": end sampling of ", suffix,
               ". Written *.rds-output to: ", return_value$fpath))

  return(return_value)
}
