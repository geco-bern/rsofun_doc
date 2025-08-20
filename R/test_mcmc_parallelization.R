test_mcmc_parallelization_dummyExample <- function(
    n_parallel_independent  = 3,        # number of cores for parallelization of independent chains     https://cran.r-project.org/web/packages/BayesianTools/vignettes/InterfacingAModel.html#running-several-mcmcs-in-parallel
    n_parallel_within_sampler = FALSE,  # number of cores for parallelization of within-sampler chains  https://cran.r-project.org/web/packages/BayesianTools/vignettes/InterfacingAModel.html#within-sampler-parallelization as well as https://cran.r-project.org/web/packages/BayesianTools/vignettes/BayesianTools.html#reference-on-creating-likelihoods

    n_chains_independent    = 3, # number of independent chains
    n_chains_within_sampler = 3, # number of internal chains to be sampled (at least 2 for DEzs)

    burnin = 0,
    iterations = 1000
){
  if(n_parallel_within_sampler==1){n_parallel_within_sampler <- FALSE} # since 1 means no parallel running. Should not be interpreted as TRUE which would lead to all_cores-1

  require(BayesianTools)
  start_time <- Sys.time()

  # Your external data
  x <- c(1, 2, 3)

  if (n_parallel_independent > 1){
    cl <- makeCluster(n_parallel_independent)
    registerDoParallel(cl)

    if (n_parallel_independent != n_chains_independent){
      warning(sprintf(
        "Requested %d indep. chains, but ran %d indep. chains as `n_parallel_independent` takes precedence.",
        n_chains_independent, n_parallel_independent)
      )
    }

    ll_factory <- function(my_x) {function(param) {sum(dnorm(param - my_x, log=TRUE))}}
    indep_chains <- foreach(i = 1:n_parallel_independent, .packages='BayesianTools') %dopar% {
      rebuilt_ll <- ll_factory(x) # inside worker: rebuild the closure so it picks up 'x'
      bayesianSetup <- createBayesianSetup(
        likelihood = rebuilt_ll,
        lower = c(-10, -10, -10), upper = c(10, 10, 10),
        parallel = n_parallel_within_sampler)
      runMCMC(
        bayesianSetup,
        sampler = 'DEzs',
        settings = list(
          burnin = burnin, iterations = iterations,
          nrChains = 1,                        # number of independent chains (kept here at 1 to parallelize with foreach and combining as a McmcSamplerList())
          startValue = n_chains_within_sampler # number of internal chains to be sampled
        )
      )
    }
    stopCluster(cl)

    out <- createMcmcSamplerList(indep_chains) # combine the independent chains

  } else {
    # sequential run of
    ll <- function(param) {sum(dnorm(param - x, log=TRUE))} # this picks up 'x' as a closure
    bayesianSetup <- createBayesianSetup(
      likelihood = ll,
      lower = c(-10, -10, -10), upper = c(10, 10, 10),
      parallel = n_parallel_within_sampler)
    out <- runMCMC(
      bayesianSetup,
      sampler = 'DEzs',
      settings = list(
        burnin = burnin, iterations = iterations,
        nrChains = n_chains_independent,     # number of independent chains
        startValue = n_chains_within_sampler # number of internal chains to be sampled
      )
    )
  }

  end_time <- Sys.time()
  out$walltime <- end_time - start_time
  # print(walltime)
  # summary(out)
  print(get_runtime(out))
  print(get_walltime(out))
}


get_runtime <- function(out_calib) {# function(settings_calib){
  total_time_secs <- sum(unlist(lapply(
    out_calib$mod,
    function(curr_chain){curr_chain$settings$runtime[["elapsed"]]})))
  return(sprintf("Total runtime: %.0f secs", total_time_secs))
}
get_runtime_numeric <- function(out_calib) {# function(settings_calib){
  total_time_secs <- sum(unlist(lapply(
    out_calib$mod,
    function(curr_chain){curr_chain$settings$runtime[["elapsed"]]})))
  return(structure(total_time_secs, class = "difftime", units = "secs"))
}
get_walltime <- function(out_calib){out_calib$walltime}











################################################################################ -
################################################################################ -
################################################################################ -

#### SETUP RSOFUN
setup_rsofun_calibration <- function(setup = 3){
  # FROM THE REVISION PLAN:
  # Setup 1: global, reduced parameter set (as in initial manuscript version), only GPP as target
  # Setup 2: global, full parameter set, only GPP as target
  # Setup 3: global, full parameter set, GPP and traits as target
  # We expect Setup 2 to yield wider posteriors than from Setup 1, and that posterior distributions will be narrowed again by Setup 3. This experimental design will allow us to demonstrate the robustness (or absence thereof) of the MCMC and the usefulness of using traits for simultaneously calibrating with fluxes.

  require(tidyverse)
  require(rpmodel)
  require(rgeco) # pak::pkg_install("geco-bern/rgeco")
  require(dplyr)
  require(purrr)
  require(rsofun)  # install from branch simple_pmodel_v2
  require(ingestr)
  require(BayesianTools)

  ## Load forcing and targets data ----
  bigD13C_vj_gpp_drivers <- read_rds(here::here("data/01_bigD13C-vj-gpp_calibsofun_drivers.rds"))
  bigD13C_vj_gpp_obs     <- read_rds(here::here("data/01_bigD13C-vj-gpp_calibsofun_obs.rds"))

  ## Read test-train split ----
  df_test_train_split <- read_csv(here::here("data/01_test_train_split.csv"))

  sites_train <- df_test_train_split |> filter(dataset == "train")
  sites_test  <- df_test_train_split |> filter(dataset == "test")

  ## Append test-train split ----
  bigD13C_vj_gpp_drivers <- bigD13C_vj_gpp_drivers |>
    inner_join(
      select(bind_rows(sites_train,sites_test), sitename, run_model, dataset),
      by = join_by(sitename, run_model))

  bigD13C_vj_gpp_obs <- bigD13C_vj_gpp_obs |>
    inner_join(
      select(bind_rows(sites_train,sites_test), sitename, run_model, dataset),
      by = join_by(sitename, run_model))

  ## Preprocess observation data (gpp) ----

  ### Verify issues visually: ----
  # TODO: remove quality check filter here (and add to gpp_data.R:67)
  # some observations of gpp are NA
  pl_issue_gpp_NA <- bigD13C_vj_gpp_obs |> filter(run_model == "daily") |>
    unnest(data) |>
    group_by(sitename) |> filter(any(is.na(gpp))) |>
    ggplot(aes(x=date,y=sitename, color = is.na(gpp))) + geom_point() + # TODO: discuss issue
    theme_classic() +
    facet_grid(dataset~., scales = "free_y", space = "free")

  # some observations of gpp are negative (keep them)
  plot_issue_gpp_value <- bigD13C_vj_gpp_obs |> filter(run_model == "daily") |>
    unnest(data) |>
    ggplot(aes(x=gpp, color = sitename)) + geom_density() + facet_grid(dataset~.) +
    theme_classic()
  plot_issue_gpp_value %+% filter(plot_issue_gpp_value$data, dataset == "train") /
  plot_issue_gpp_value %+% filter(plot_issue_gpp_value$data, dataset == "test")


  # remove lower quality gpp and NA
  bigD13C_vj_gpp_obs <- bind_rows(

    # for gpp keep only high-quality
    bigD13C_vj_gpp_obs |>
      filter(run_model == "daily") |>
      mutate(data = purrr::map(data, \(nstdf){
        nstdf |>
          # keep only high quality gpp
          filter(gpp_qc >= 0.8) |>
          # and non-NA:
          filter(!is.na(gpp))
        }
      )
    ),

    # for non-gpp keep all:
    bigD13C_vj_gpp_obs |>
      filter(run_model != "daily")
  )

  ### Verify issues visually: ----
  pl_issue_gpp_all_afterQC <- bigD13C_vj_gpp_obs |> filter(run_model == "daily") |>
    unnest(data) |>
    ggplot(aes(x=date,y=sitename, color = is.na(gpp))) + geom_point() + # TODO: discuss issue
    theme_classic() +
    facet_grid(dataset~., scales = "free_y", space = "free")


  ## Apply test-train split to data ----
  train_drivers <- bigD13C_vj_gpp_drivers |> filter(dataset == "train") |> select(-dataset)
  train_obs     <- bigD13C_vj_gpp_obs     |> filter(dataset == "train") |> select(-dataset)

  test_drivers <- bigD13C_vj_gpp_drivers |> filter(dataset == "test") |> select(-dataset)
  test_obs     <- bigD13C_vj_gpp_obs     |> filter(dataset == "test") |> select(-dataset)

  ## Setup the settings for the three calibration setups ----
  ## Define parameter
  default_par_fixed <- list(# fix parameter value from previous calibration
    kphio              = 0.04998,
    kphio_par_a        = 0.0,
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover paper setup with soil moisture stress
    soilm_betao        = 0.0,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 20.0,
    kc_jmax            = 0.41
  )
  if (setup %in% c(1)){
    par_to_estimate <- list(
      kphio           = list(lower = 0.02, upper = 0.15, init = 0.05),
      kphio_par_a     = list(lower = -0.004, upper = -0.001, init = -0.0025),
      kphio_par_b     = list(lower = 10, upper = 30, init = 20),
      soilm_thetastar = list(lower = 1, upper = 250, init = 40),
      soilm_betao     = list(lower = 0.0, upper = 1.0, init = 0.0),
      err_gpp         = list(lower = 0.1, upper = 3, init = 0.8),
      err_bigD13C     = list(lower = 0.1, upper = 3, init = 0.8), # TODO: without err_bigD13C and err_vj this errors
      err_vj          = list(lower = 0.1, upper = 3, init = 0.8)  # TODO: without err_bigD13C and err_vj this errors
    )
    par_to_fix <- default_par_fixed[!(names(default_par_fixed) %in% names(par_to_estimate))]

  } else if (setup %in% c(2,3)) {
    par_to_estimate <- list(
      kphio           = list(lower = 0.02, upper = 0.15, init = 0.05),
      kphio_par_a     = list(lower = -0.004, upper = -0.001, init = -0.0025),
      kphio_par_b     = list(lower = 10, upper = 30, init = 20),
      soilm_thetastar = list(lower = 1, upper = 250, init = 40),
      soilm_betao     = list(lower = 0.0, upper = 1.0, init = 0.0),
      beta_unitcostratio = as.list(c(lower = 0.1, upper = 3.0, init = 1.0)*146.0),
      rd_to_vcmax        = as.list(c(lower = 0.1, upper = 3.0, init = 1.0)*0.014),      # 0.014 value from Atkin et al. 2015 for C3 herbaceous
      tau_acclim         = as.list(c(lower = 0.1, upper = 3.0, init = 1.0)*20.0),
      kc_jmax            = as.list(c(lower = 0.1, upper = 3.0, init = 1.0)*0.41),
      err_gpp         = list(lower = 0.01, upper = 3, init = 0.8),
      err_bigD13C     = list(lower = 0.01, upper = 3, init = 0.8), # TODO: without err_bigD13C and err_vj this errors
      err_vj          = list(lower = 0.01, upper = 3, init = 0.8)  # TODO: without err_bigD13C and err_vj this errors
    )
    par_to_fix <- default_par_fixed[!(names(default_par_fixed) %in% names(par_to_estimate))]

  } else {
    warning(sprintf("Unsupported setup: %d", setup))
  }

  ## Setup the data (drivers and obs) for the three calibration setups ----

  # subset different combination of target variables
  # for easier handling do this in combined drivobs-object
  drivobs_bigD13C_vj_gpp <- dplyr::inner_join(
    train_drivers,
    train_obs,
    by = join_by(sitename, run_model))

  # TODO: check if passing combined drivobs is computationally more efficient for calib_sofun()
  if (setup %in% c(1,2)){
    drivobs <- drivobs_bigD13C_vj_gpp |>
      unnest_wider(targets) |>
      filter(gpp) |>
      nest(targets = c(vj, bigD13C, gpp))
  } else if (setup %in% c(3)) {
    drivobs <- drivobs_bigD13C_vj_gpp
  } else {
    warning(sprintf("Unsupported setup: %d", setup))
  }

  ## return ---
  return(list(
    drivobs = drivobs,
    # driver    = tibble(),
    # obs       = tibble(),
    par_fixed = par_to_fix,
    par = par_to_estimate
    )
  )
}

calib_sofun_parallelized <- function(
    drivers,
    obs,
    settings,
    optim_out = TRUE,
    suffix = "", # for storing rds
    ...
){
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

    # reformat parameters
    pars <- as.data.frame(do.call("rbind", settings$par))
    parnames <- rownames(pars)

    priors  <- BayesianTools::createUniformPrior(
      lower = unlist(pars$lower),
      upper = unlist(pars$upper),
      best  = unlist(pars$init)
    )

    # Your external data
    # drivers
    # obs

    # sampler needs a function ll(random_par) for the likelihood,
    # since data is provided as a closure (drivers, obs) we need a function factory to be able
    # create this function on each worker
    ll_factory <- function(obs, drivers, parnames, ...){
      function(random_par){
        eval(settings$metric)(par = setNames(random_par, parnames),
                              obs = obs,
                              drivers = drivers,
                              ...)
      }
    }

    ## Run the MCMC sampler: ----

    require(BayesianTools)
    start_time <- Sys.time()

    if (settings$control$n_parallel_independent > 1){ # parallel MCMC sampler:

      cl <- makeCluster(settings$control$n_parallel_independent)
      registerDoParallel(cl)

      if (settings$control$n_parallel_independent != settings$control$n_chains_independent){
        warning(sprintf(
          "Requested %d indep. chains, but ran %d indep. chains as `n_parallel_independent` takes precedence.",
          settings$control$n_chains_independent, settings$control$n_parallel_independent)
        )
      }
      # since parallel sampling, fix the number of chains of runMCMC to 1, but call it multiple times
      settings$control$settings$nrChains <- 1

      indep_chains <- foreach(i = 1:settings$control$n_parallel_independent, .packages=c('BayesianTools','rsofun','dplyr','tidyr')) %dopar% { #%dopar% {
        bayesianSetup <- createBayesianSetup(
          likelihood = ll_factory(obs, drivers, parnames, ...), # inside worker: rebuild the closure so it picks up 'obs', 'drivers', 'parnames'
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
      out <- createMcmcSamplerList(indep_chains) # combine the independent chains

    } else { # sequential MCMC sampler:

      # setup the bayesian sampling
      bayesianSetup <- createBayesianSetup(
        likelihood = ll_factory(obs, drivers, parnames, ...),
        prior      = priors,
        names      = parnames,
        parallel   = settings$control$n_parallel_within_sampler)

      # since sequential sampling, let runMCMC handle the actual number of chains
      settings$control$settings$nrChains <- settings$control$n_chains_independent
      # calculate the runs
      out <- BayesianTools::runMCMC(
        bayesianSetup = bayesianSetup,
        sampler       = settings$control$sampler,
        settings      = settings$control$settings
      )
    }

    ## Postprocess: ----

    # ensure return value 'out' is a mcmcSamplerList even if n_chains_independent==1
    # by default runMCMC returns only a mcmcSampler if n_chains_independent==1
    if(is(out, "mcmcSampler")){
      out <- createMcmcSamplerList(list(out)) # now out is a mcmcSamplerList
    }

    # drop last value
    bt_par <- BayesianTools::MAP(out)$parametersMAP
    bt_par <- bt_par[1:(length(bt_par))]

    if (optim_out){
      out_optim <- list(par = bt_par, mod = out)
    } else {
      out_optim <- list(par = bt_par)
    }

    names(out_optim$par) <- names(settings$par)

    end_time <- Sys.time()
    out_optim$walltime <- end_time - start_time
    out_optim$runtime <- get_runtime_numeric(out_optim)
    # print(walltime)
    # summary(out_optim)
    # plot(out_optim$mod)
    print(get_runtime_numeric(out_optim))
    print(get_walltime(out_optim))

    # Store intermediate results
    out_optim$name <- suffix
    out_optim$fpath <- here::here(paste0("data/out_calib_", suffix, ".rds"))
    write_rds(out_optim, file = out_optim$fpath, compress = "xz")

  } else {
    stop("Unknown method passed to calib_sofun().")
  }

  return(out_optim)
}

# #### MAKE TEST FUNCTION
test_mcmc_parallelization_rsofun <- function(
    # MCMC setup:
    iterations = 3,
    burnin = 0,
    n_chains_independent    = 3, # number of independent chains
    n_chains_within_sampler = 3, # number of internal chains to be sampled (at least 2 for DEzs)

    # parallelization:
    n_parallel_independent  = 3,      # number of cores for parallelization of independent chains     https://cran.r-project.org/web/packages/BayesianTools/vignettes/InterfacingAModel.html#running-several-mcmcs-in-parallel
    n_parallel_within_sampler = FALSE # number of cores for parallelization of within-sampler chains  https://cran.r-project.org/web/packages/BayesianTools/vignettes/InterfacingAModel.html#within-sampler-parallelization as well as https://cran.r-project.org/web/packages/BayesianTools/vignettes/BayesianTools.html#reference-on-creating-likelihoods
){

  # Setup simulation model
  curr_setup <- 3
  res <- setup_rsofun_calibration(setup = curr_setup)
  # res$drivobs
  # res$par_fixed
  # res$par

  # Load loglikelihood
  source(here::here("R/calibration_helpers.R"), echo = TRUE)
  source(here::here("R/cost_likelihood_pmodel_bigD13C_vj_gpp.R"), echo = TRUE)

  # cost_likelihood_pmodel_bigD13C_vj_gpp
  # cost_likelihood_pmodel_bigD13C_vj_gpp_v2
  # cost_likelihood_pmodel_bigD13C_vj_gpp_v3

  # Setup MCMC
  calib_sofun_settings <- list(
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

  # Run calibration in parallel
  timings <- tibble(
    #
    setup          = curr_setup,
    # sampling options:
    sampler        = calib_sofun_settings$control$sampler,
    burnin         = burnin,
    iterations     = iterations,
    n_chains       = n_chains_independent,
    n_chains_inner = n_chains_within_sampler,
    # performance results:
    cores          = n_parallel_independent,
    cores_inner    = n_parallel_within_sampler,
    runtime        = NaN,
    walltime       = NaN,
    resultfile     = NaN
  )
  suffix_str <- with(
    timings,
    sprintf(
      "_setup%d_%s-%d-%diter_%dx%dchains_on_CPU%dx%d",
      setup, sampler, iterations, burnin, n_chains, n_chains_inner, cores, cores_inner)
  )

  out_calib <- calib_sofun_parallelized(
    drivers   = select(res$drivobs, sitename, run_model, params_siml, site_info, forcing),
    obs       = select(res$drivobs, sitename, run_model, targets, data),
    settings  = calib_sofun_settings,
    # other arguments for the cost function
    par_fixed = res$par_fixed,
    suffix = suffix_str
  )

  # store performance results
  timings$runtime    <- out_calib$runtime
  timings$walltime   <- out_calib$walltime
  timings$resultfile <- out_calib$fpath

  # return performance results
  return(timings)
}

