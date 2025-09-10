source(here::here("R/calibration_helpers.R"))
source(here::here("R/sensitivity_sofun_serialized.R"))
source(here::here("analysis/00_define_scenarios.R"))

run_sensitivity_rsofun <- function(
    curr_calibration_scenario,
    # morris sensitivity setup
    iterations = 3,
    outpath = outpath
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
  #   get_mod_obs_pmodel_bigD13C_vj_gpp

  # Setup sensitivity analysis
  sensitivity_sofun_settings <- list(
    # method = "BayesianTools",
    metric = cost_likelihood_pmodel_bigD13C_vj_gpp,
    control = list(
      settings = list(
        iterations = iterations                # 50000,
        # burnin     = burnin,                 # 10000,
        # nrChains   = NA,                     # number of independent chains
        # startValue = n_chains_within_sampler # number of internal chains to be sampled
      )
      # sampler = "DEzs",
      # n_chains_independent      = n_chains_independent,
      # n_parallel_independent    = n_parallel_independent,
      # n_parallel_within_sampler = n_parallel_within_sampler
    ),
    par = res$par
  )


  # # Run sensitivity in parallel # TODO:;;; define function below and then run here
  # # Run sensitivity
  suffix_str <- sprintf(
    "_scen%d_%s-%diter", # %dx%dchains_on_CPU%dx%d
    curr_calibration_scenario, "morris", iterations #n_chains, n_chains_inner, cores, cores_inner
    # TODO: include further needed options from settings. (e.g. parse 'design')
  )

  out_morris <- sensitivity_sofun_serialized(
    drivers  = select(res$drivobs, sitename, run_model, params_siml, site_info, forcing),
    obs      = select(res$drivobs, sitename, run_model, targets, data),
    settings = sensitivity_sofun_settings,
    suffix   = suffix_str,       # for storing results
    outpath  = outpath           # for storing results
  ) # this stores the whole out_morris in an rds object identified by "suffix_str" (into outpath)

  return(out_morris)
}
