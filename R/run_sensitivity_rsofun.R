source(here::here("R/calibration_helpers.R"))
source(here::here("R/sensitivity_sofun_serialized.R"))
source(here::here("analysis/00_define_scenarios.R"))

run_sensitivity_rsofun <- function(
    curr_calibration_scenario,
    # morris sensitivity setup
    iterations = 3,
    outpath = outpath,
    par_ranges_derived_from = "prior" # this could either be "prior" or a data.frame(parameter_name=..., lower=..., upper=...)
  ){
  # Setup simulation model
  res <- setup_rsofun_calibration(scenario = curr_calibration_scenario)

  # Load loglikelihood
  source(here::here("R/calibration_helpers.R"), echo = FALSE)
  source(here::here("R/cost_likelihood_pmodel_bigD13C_vj_gpp.R"), echo = FALSE)

  # Setup Morris sensitivity analysis
  design <- list(type = "oat", levels = 20, grid.jump = 3)
  sensitivity_sofun_settings <- list(
    # method = "BayesianTools",
    metric = cost_likelihood_pmodel_bigD13C_vj_gpp,
    control = list(
      settings = list(
        iterations = iterations,
        design     = design
      ),
    par_ranges = par_ranges_derived_from
    ),
    par = res$par
  )


  # # Run sensitivity in parallel # TODO:;;; define function below and then run here
  # # Run sensitivity
  suffix_str <- sprintf(
    "_scen%d_%s-%diter_par-range-%s", # %dx%dchains_on_CPU%dx%d
    curr_calibration_scenario, "morris", iterations,
    ifelse(is.character(par_ranges_derived_from), par_ranges_derived_from, "prespecified")
    # TODO: include further needed options from settings. (e.g. parse 'design')
    #       #n_chains, n_chains_inner, cores, cores_inner
  )

  out_morris <- sensitivity_sofun_serialized(
    drivers  = select(res$drivobs_train, sitename, run_model, params_siml, site_info, forcing),
    obs      = select(res$drivobs_train, sitename, run_model, targets, data),
    settings = sensitivity_sofun_settings,
    suffix   = suffix_str,       # for storing results
    outpath  = outpath           # for storing results
  ) # this stores the whole out_morris in an rds object identified by "suffix_str" (into outpath)

  return(out_morris)
}
