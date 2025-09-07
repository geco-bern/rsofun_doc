source(here::here("R/cost_likelihood_pmodel_bigD13C_vj_gpp.R"))

predict_sofun_parallelized <- function(
    drivers,
    obs,
    settings,
    par_fixed,
    par_df = NULL) {

  # Set number of cores if not specified
  if (is.null(settings$n_cores)) {
    settings$n_cores <- min(detectCores() - 1, 20) # at most 20
  }
  settings$n_cores <- min(nrow(par_df), settings$n_cores) # ensure not more than needed

  # Function to run prediction for a single parameter set
  run_pmodel_single_prediction <- function(par, par_fixed, drivers, obs) {
    # Function that runs the P-model for a sample of parameters
    # but does not add the observation error

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
      relocate(c(mod_no_err, err_par_sd), .after = last_col())

    return(df_pred_vs_obs)
  }

  # Run the P-model predictions for each set of parameters
  if (settings$n_cores > 1 && nrow(par_df) > 1) { # parallel version

    cl <- multidplyr::new_cluster(settings$n_cores) |>  # TODO use: logpath for logging messages. NOTE: actually not needed
      multidplyr::cluster_assign(
        get_mod_obs_pmodel_bigD13C_vj_gpp = get_mod_obs_pmodel_bigD13C_vj_gpp
      ) |>
      multidplyr::cluster_library(packages = c("dplyr", "tidyr", "purrr", "rsofun"))

    df_model_predictions <- par_df |>
      multidplyr::partition(cl) |>
      dplyr::mutate('sim' = purrr::map(
        pars,
        ~run_pmodel_single_prediction(
          par = .x,
          par_fixed,
          drivers,
          obs
        )
      )) |>
      dplyr::collect()

  } else { # sequential version

    df_model_predictions <- par_df |>
      dplyr::mutate('sim' = purrr::map(
        pars,
        ~run_pmodel_single_prediction(
          par = .x,
          par_fixed,
          drivers,
          obs
        )
      ))

  }

  # # Save results
  # if (!is.null(outpath)) {
  #   write_rds(df_model_predictions, outpath, compress = "xz")
  # }

  return(df_model_predictions)
}
