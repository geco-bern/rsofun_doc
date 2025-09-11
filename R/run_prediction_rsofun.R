source(here::here("R/predict_sofun_parallelized.R"), echo = FALSE)
source(here::here("analysis/00_define_scenarios.R"))

run_prediction_rsofun <- function(
    mcmc_posterior,
    prediction = c("both","test","train"),
    burnin_to_skip = 0,
    n_samples = 100,      # if n_samples == 1, use MAP
    n_cores = NULL){

  if (length(prediction) == 1 && (prediction %in% c("both","test","train"))){
    # as expected
  } else {
    stop("Provide prediction as either: 'both', 'test' or 'train'")
  }
  n_cores <- ifelse(is.null(n_cores), 1, n_cores)
  stopifnot(is(mcmc_posterior$mod, "mcmcSamplerList"))

  curr_calibration_scenario <- as.integer(gsub(
    ".*_scen([0-9]*)_.*",  # NOTE: hardcoded expected format of filename
    "\\1",
    mcmc_posterior$name))
  stopifnot(!is.na(curr_calibration_scenario)) # Catches if expected format is inaccurate

  # Setup simulation model
  res <- setup_rsofun_calibration(scenario = curr_calibration_scenario)

  # Load loglikelihood/prediction model
  source(here::here("R/predict_sofun_parallelized.R"), echo = FALSE)

  # Set random seed for reproducibility
  set.seed(2023)

  # Sample parameters from MCMC posterior
  # Evaluation of the uncertainty coming from the model parameters' uncertainty
  if (n_samples > 1){
    samples_par <- getSample(
        mcmc_posterior$mod,
        thin = 1,
        start = burnin_to_skip, numSamples = n_samples
      ) |>
        as.data.frame() |>
        # Add sample IDs
        dplyr::mutate(posterior_sample_id = 1:n()) |>
        tidyr::nest(.by = posterior_sample_id, .key = "pars")
  } else {
    # mcmc_posterior$par # these are already precomputed...
    # but more robust to recompute:
    samples_par <- BayesianTools::MAP(mcmc_posterior$mod)$parametersMAP |>
      as.list() |> as_tibble() |>
      # Add sample IDs
      dplyr::mutate(posterior_sample_id = 0L) |> # posterior_sample_id == 0 means MAP
      tidyr::nest(.by = posterior_sample_id, .key = "pars")
  }

  # Setup prediction
  predict_sofun_settings <- list(n_cores=n_cores)

  if (prediction == "both"){
    curr_driver <- bind_rows(res$drivobs_train, res$drivobs_test) |>
      select(sitename, run_model, params_siml, site_info, forcing)
    curr_obs    <- bind_rows(res$drivobs_train, res$drivobs_test) |>
      select(sitename, run_model, targets, data)
  } else if (prediction == "train"){
    curr_driver <- select(res$drivobs_train, sitename, run_model, params_siml, site_info, forcing)
    curr_obs    <- select(res$drivobs_train, sitename, run_model, targets, data)
  } else if (prediction == "test"){
    curr_driver <- select(res$drivobs_test, sitename, run_model, params_siml, site_info, forcing)
    curr_obs    <- select(res$drivobs_test, sitename, run_model, targets, data)
    stopifnot(nrow(curr_obs)>0)
  }

  # Run prediction
  df_pred_vs_obs <- predict_sofun_parallelized(
    drivers     = curr_driver,
    obs         = curr_obs,
    settings    = predict_sofun_settings,
    par         = samples_par,
    par_fixed   = res$par_fixed
  )

  return(df_pred_vs_obs)
}

