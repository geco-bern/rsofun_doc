source(here::here("R/cost_likelihood_pmodel_bigD13C_vj_gpp.R"))

predict_sofun_parallelized <- function(
    drivers,
    obs,
    settings,
    par_fixed,
    par_df = NULL,
    outpath = NULL,
    logpath = NULL) {

  # Set number of cores if not specified
  if (is.null(settings$n_cores)) {
    settings$n_cores <- min(30, min(detectCores() - 1, nrow(par %||% data.frame(x = 1))))
  }

  # Function to run prediction for a single parameter set
  run_pmodel_single_prediction <- function(par) {
    # Function that runs the P-model for a sample of parameters
    # but does not add the observation error

    # tryCatch({

      # Taken from cost_likelihood_pmodel_bigD13C_vj_gpp()
      stopifnot(nrow(obs) > 0)     # ensure some observation data are provided
      stopifnot(nrow(drivers) > 0) # ensure some driver data are provided

      # A) Include current parameters ----
      stopifnot(length(intersect(names(par), names(par_fixed))) == 0) # no overlap
      params_modl <- c(par, par_fixed)

      # B,C) Run model and bring together with observed ----
      ## run the time series model for gpp/et/... time series
      ## run the onestep model for traits
      df_pred_vs_obs <- get_mod_obs_pmodel_bigD13C_vj_gpp(drivers, obs, params_modl, parallel=FALSE, ncores=1, return_continuous_timeseries = TRUE)

      # D) (DON'T) Sample error model ----
      # NOTE: sampling is not done here, but can optionally be done before plotting
      #       Here we rename to clarify that no error model has yet been applied.
      df_pred_vs_obs <- df_pred_vs_obs |>
        # clarify name of model output (containing not yet any error model term)
        rename(mod_no_err = mod) |>
        relocate(c(mod_no_err, err_par), .after = last_col())

      return(df_pred_vs_obs)

    # }, error = function(e) {
    #   warning(paste("Error in sample", i, ":", e$message))
    #   return(NULL)
    # })
  }


  # Run the P-model predictions for each set of parameters
  if (settings$n_cores > 1 && nrow(par_df) > 1) {
    # TODO: reactivate
    # cl <- makeCluster(settings$n_cores) # TODO use: logpath
    # clusterEvalQ(cl, library(dplyr))
    # clusterEvalQ(cl, library(rsofun))
    #
    # results <- parLapply(cl, 1:nrow(par_df), run_pmodel_single_prediction)
    # stopCluster(cl)
    stop("Not set up.")
    # TODO: setup with mutlidplyr::
    # df_model_predictions <- par_df |>
    #   dplyr::mutate(sim = purrr::map(pars, ~run_pmodel_single_prediction(.x)))

  } else {
    df_model_predictions <- par_df |>
      dplyr::mutate(sim = purrr::map(pars, ~run_pmodel_single_prediction(.x)))
  }

  # Save results
  if (!is.null(outpath)) {
    write_rds(df_model_predictions, outpath, compress = "xz")
  }

  return(df_model_predictions)
}
