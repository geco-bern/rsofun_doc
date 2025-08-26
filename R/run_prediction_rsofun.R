source(here::here("R/predict_sofun_parallelized.R"), echo = FALSE)
source(here::here("analysis/00_define_scenarios.R"))

run_prediction_rsofun <- function(
    mcmc_posterior,
    prediction = c("both","test","train"),
    burnin_to_skip = 0,
    n_samples = 100,
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
  samples_par <- getSample(
    mcmc_posterior$mod,
    thin = 1,
    start = burnin_to_skip, numSamples = n_samples
  ) |>
    as.data.frame() |>
    # Add sample IDs
    dplyr::mutate(mcmc_id = 1:n()) |>
    tidyr::nest(.by = mcmc_id, .key = "pars")

  # Setup prediction
  predict_sofun_settings <- list(n_cores=n_cores)

  if (prediction == "both"){
    curr_driver <- bind_rows(res$drivobs, res$drivobs_test) |>
      select(sitename, run_model, params_siml, site_info, forcing)
    curr_obs    <- bind_rows(res$drivobs, res$drivobs_test) |>
      select(sitename, run_model, targets, data)
  } else if (prediction == "train"){
    curr_driver <- select(res$drivobs, sitename, run_model, params_siml, site_info, forcing)
    curr_obs    <- select(res$drivobs, sitename, run_model, targets, data)
  } else if (prediction == "test"){
    curr_driver <- select(res$drivobs_test, sitename, run_model, params_siml, site_info, forcing)
    curr_obs    <- select(res$drivobs_test, sitename, run_model, targets, data)
  }

  # Generate paths for output files
  outpath <- file.path(
    dirname(mcmc_posterior$fpath),
    "predictions",
    paste0("out_predict_",prediction,"_", gsub("out_calib_","",basename(mcmc_posterior$fpath)))
  )
  logpath <- ifelse(
    is.null(n_cores),
    NULL,
    file.path(dirname(outpath), paste0("log__",basename(outpath)))
  )

  # Create output directories if they don't exist
  dir.create(dirname(outpath), showWarnings = FALSE, recursive = TRUE)
  if (!dir.exists(dirname(outpath))) dir.create(dirname(outpath), recursive = TRUE)
  if (!dir.exists(dirname(logpath))) dir.create(dirname(logpath), recursive = TRUE)

  # Run prediction
  df_pred_vs_obs <- predict_sofun_parallelized(
    drivers     = curr_driver,
    obs         = curr_obs,
    settings    = predict_sofun_settings,
    par         = samples_par,
    par_fixed   = res$par_fixed,
    outpath     = outpath,
    logpath     = logpath
  )

  return(df_pred_vs_obs)
}

