source(here::here("R/calibration_helpers.R"))
source(here::here("R/calib_sofun_parallelized.R"))
source(here::here("analysis/00_define_scenarios.R"))

run_mcmc_rsofun <- function(
    curr_calibration_scenario,
    # MCMC setup:
    iterations = 3,
    burnin = 0,
    n_chains_independent    = 3, # number of independent chains
    n_chains_within_sampler = 3, # number of internal chains to be sampled (at least 2 for DEzs)

    # parallelization:
    n_parallel_independent  = 3,      # number of cores for parallelization of independent chains     https://cran.r-project.org/web/packages/BayesianTools/vignettes/InterfacingAModel.html#running-several-mcmcs-in-parallel
    n_parallel_within_sampler = FALSE,# number of cores for parallelization of within-sampler chains  https://cran.r-project.org/web/packages/BayesianTools/vignettes/InterfacingAModel.html#within-sampler-parallelization as well as https://cran.r-project.org/web/packages/BayesianTools/vignettes/BayesianTools.html#reference-on-creating-likelihoods
    outpath = here::here("data"), logpath = ""
){
  # Setup simulation model
  res <- setup_rsofun_calibration(scenario = curr_calibration_scenario)
  # res$drivobs
  # res$drivobs_test
  # res$par_fixed
  # res$par

  # Load loglikelihood
  source(here::here("R/calibration_helpers.R"), echo = FALSE)
  source(here::here("R/cost_likelihood_pmodel_bigD13C_vj_gpp.R"), echo = FALSE)
  # loads:
  #   cost_likelihood_pmodel_bigD13C_vj_gpp
  #   cost_likelihood_pmodel_bigD13C_vj_gpp_v2
  #   cost_likelihood_pmodel_bigD13C_vj_gpp_v3

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
    scenario       = curr_calibration_scenario,
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
      "_scen%d_%s-%d-%diter_%dx%dchains_on_CPU%dx%d",
      scenario, sampler, iterations, burnin, n_chains, n_chains_inner, cores,
      ifelse(cores_inner, cores_inner, 1))
  )

  out_calib <- calib_sofun_parallelized(
    drivers   = select(res$drivobs, sitename, run_model, params_siml, site_info, forcing),
    obs       = select(res$drivobs, sitename, run_model, targets, data),
    settings  = calib_sofun_settings,
    # other arguments for the cost function
    par_fixed = res$par_fixed,
    suffix    = suffix_str,
    outpath = outpath, logpath = logpath
  ) # this stores the whole out_calib in an rds object identified by "suffix_str" into outpath

  # append performance results to return object
  timings$runtime    <- out_calib$runtime
  timings$walltime   <- out_calib$walltime
  timings$resultfile <- out_calib$fpath

  # return performance results
  return(timings)
}

