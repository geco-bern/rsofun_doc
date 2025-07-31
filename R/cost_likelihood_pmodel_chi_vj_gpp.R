#' Cost function computing a log-likelihood for calibration of P-model
#' parameters
#'
#' The cost function performs a P-model run for the input drivers and model parameter
#' values, and computes the outcome's normal log-likelihood centered at the input
#' observed values and with standard deviation given as an input parameter
#' (calibratable).
#'
#' @param par A vector of values for the parameters to be calibrated, including
#' a subset of model parameters (described in \code{\link{runread_pmodel_f}}),
#' in order, and error terms
#' for each target variable (for example \code{'gpp_err'}), in the same order as
#' the targets appear in \code{targets}.
#' @param obs A nested data.frame of observations, with columns \code{'sitename'}
#' and \code{'data'} (see \code{\link{p_model_validation}} or \code{\link{p_model_validation_vcmax25}}
#' to check their structure).
#' @param drivers A nested data.frame of driver data. See \code{\link{p_model_drivers}}
#' for a description of the data structure.
#' @param targets A character vector indicating the target variables for which the
#' optimization will be done and the RMSE computed. This string must be a column
#' name of the \code{data} data.frame belonging to the validation nested data.frame
#' (for example 'gpp').
#' @param par_fixed A named list of model parameter values to keep fixed during the
#' calibration. These should complement the input \code{par} such that all model
#' parameters are passed on to \code{\link{runread_pmodel_f}}.
#' @param parallel A logical specifying whether simulations are to be parallelised
#' (sending data from a certain number of sites to each core). Defaults to
#' \code{FALSE}.
#' @param ncores An integer specifying the number of cores used for parallel
#' computing. Defaults to 2.
#'
#' @return The log-likelihood of the observed target values, assuming that they
#' are independent, normally distributed and centered on the predictions
#' made by the P-model run with standard deviation given as input (via `par` because
#' the error terms are estimated through the calibration with `BayesianTools`,
#' as shown in the "Parameter calibration and cost functions" vignette).
#'
#' @details To run the P-model, all model parameters must be given. The cost
#' function uses arguments \code{par} and \code{par_fixed} such that, in the
#' calibration routine, \code{par} can be updated by the optimizer and
#' \code{par_fixed} are kept unchanged throughout calibration.
#'
#' If the validation data contains a "date" column (fluxes), the simulated target time series
#' is compared to the observed values on those same dates (e.g. for GPP). Otherwise,
#' there should only be one observed value per site (leaf traits), and the outputs
#' (averaged over the growing season, weighted by predicted GPP) will be
#' compared to this single value representative of the site (e.g. Vcmax25). As an exception,
#' when the date of a trait measurement is available, it will be compared to the
#' trait value predicted on that date.
#'
#' @export
#'
#' @examples
#' # Compute the likelihood for a set of
#' # model parameter values involved in the
#' # temperature dependence of kphio
#' # and example data
#' cost_likelihood_pmodel(
#'  par = c(0.05, -0.01, 1,     # model parameters
#'          2),                # err_gpp
#'  obs = p_model_validation,
#'  drivers = p_model_drivers,
#'  targets = c('gpp'),
#'  par_fixed = list(
#'   soilm_thetastar    = 0.6 * 240,  # old setup with soil moisture stress
#'   soilm_betao        = 0.0,
#'   beta_unitcostratio = 146.0,
#'   rd_to_vcmax        = 0.014,      # from Atkin et al. 2015 for C3 herbaceous
#'   tau_acclim         = 30.0,
#'   kc_jmax            = 0.41
#'  )
#' )

cost_likelihood_pmodel_chi_vj_gpp <- function(
    par,   # model parameters & error terms for each target
    obs,
    drivers,
    par_fixed = NULL,   # non-calibrated model parameters
    parallel  = FALSE,
    ncores    = 2
){

  stopifnot(nrow(obs) > 0)     # ensure some observation data are provided
  stopifnot(nrow(drivers) > 0) # ensure some driver data are provided

  # A) Include current parameters ----
  stopifnot(length(intersect(names(par), names(par_fixed))) == 0) # no overlap
  params_modl <- c(par, par_fixed)


  # B) Run model ----

  ## run the time series model for gpp/et/... time series
  df_daily <- drivers |>
    filter(run_model == "daily") |> # NOTE: this works gracefully even when no simulations are requested
    runread_pmodel_f(
      drivers   = _,
      par       = params_modl,
      makecheck = TRUE,        # TODO: disable check
      parallel  = parallel,
      ncores    = ncores
    )

  ## run the onestep model for traits
  df_onestep <- drivers |>
    # TODO: from here on unneeded computational overhead
    filter(run_model == "onestep") |>
    # filter(FALSE) |>
    group_by(sitename) |>
    unnest(c(params_siml, forcing))
    # TODO: up until here: unneeded computational overhead
  # NOTE: this is to make this work gracefully even when no simulations are requested
  if (nrow(df_onestep) == 0){ # no onestep simulations requested, generate dummy output:
    df_onestep <- tibble(
      sitename                 = character(),
      vcmax_mod_molm2s         = numeric(),
      jmax_mod_molm2s          = numeric(),
      vcmax25_mod_molm2s       = numeric(),
      jmax25_mod_molm2s        = numeric(),
      gs_accl_mod_molCmolPhPa  = numeric(),
      wscal_mod__              = numeric(),
      chi_mod__                = numeric(),
      iwue_mod__               = numeric(),
      rd_mod_gCm2s             = numeric(),
      vj_mod__                 = numeric())
  } else { # only run onestep simulations if requested, generate simulation output:
    df_onestep <- df_onestep |>
      group_modify(~run_pmodel_onestep_f_bysite(
        lc4 = FALSE,
        # select what forcing columns to use:
        forcing =  data.frame(temp = .x$temp,
                              vpd  = .x$vpd,
                              ppfd = .x$ppfd,
                              co2  = .x$co2,
                              patm = .x$patm),
        params_modl = params_modl,
        makecheck   = TRUE)) |> # TODO: disable check
      rename(vcmax_mod_molm2s   = vcmax,
             jmax_mod_molm2s    = jmax,
             vcmax25_mod_molm2s = vcmax25,
             jmax25_mod_molm2s  = jmax25,
             gs_accl_mod_molCmolPhPa = gs_accl,
             wscal_mod__        = wscal,
             chi_mod__          = chi,
             iwue_mod__         = iwue,
             rd_mod_gCm2s       = rd) |>
      mutate(vj_mod__ = vcmax_mod_molm2s/jmax_mod_molm2s)
  }



  # C) Bring together modelled and observed ----

  # NOTE: calibration targets can be controlled by providing no forcing and
  #       observation data for certain targets
  # NOTE: in that case some of the tibbles() will be empty (i.e. nrow() == 0)
  # NOTE: the unnesting operation does not work in case, since there is no
  #       nested element to infer the column names and column types
  # NOTE: below function makes unnest() work gracefully even in case nrow=0
  ensure_cols_defined <- function(df, expected_columns){
    if(nrow(df)==0){
      df |>
        # replace the unspecified column with expected columns
        select(!where(\(cl){class(cl)=="vctrs_unspecified"})) |>
        cross_join(expected_columns)
      } else {df}
  }

  df_mod_obs_daily <- obs |>
    filter(run_model == "daily") |>
    select(sitename, run_model, targets, data) |>
    unnest(c(data)) |>
    # make this work gracefully in case nrow=0
    ensure_cols_defined(tibble(date = as.Date(character()))) |>
    # join the modelled data
    left_join(
      df_daily |>
        unnest(data) |>
        # make this work gracefully in case nrow=0
        ensure_cols_defined(tibble(date = as.Date(character()),gpp = numeric(),le = numeric())) |>
        select(sitename, date, gpp_mod = gpp, le_mod = le),
      by = join_by(sitename, date)) |>
    # nest again
    nest(modobs = -c(sitename, run_model, targets))

  df_mod_obs_onestep <- obs |>
    filter(run_model == "onestep") |>
    select(sitename, run_model, targets, data) |>
    unnest(data) |>
    # make this work gracefully in case nrow=0
    ensure_cols_defined(tibble(chi = list(), vj = list())) |>
    # join the modelled data
    left_join(
      df_onestep |>
        select(sitename, vcmax_mod_molm2s, jmax_mod_molm2s, chi_mod__, vj_mod__),
      by = join_by(sitename)) |>
    # nest again
    nest(modobs = -c(sitename, run_model, targets))

  # combine into single data.frame
  targets <- grep("^err_", names(par), value = TRUE)

  # for (curr_target in targets){
  #   print(curr_target)
  # } # or alternativel lapply
  # or hardcode:
  df_mod_obs <- bind_rows(
    df_mod_obs_daily |> unnest(modobs) |>
      # make this work gracefully in case nrow=0
      ensure_cols_defined(tibble(gpp_mod = numeric(), gpp = numeric())) |>
      rename(all_of(c(mod = "gpp_mod", obs = "gpp"))) |>
      mutate(target  = "gpp",#curr_target,
             err_par = par[["err_gpp"]]) |> #par[[paste0("err_,"curr_target]]) |>
      select(sitename, run_model, target, mod, obs, err_par),

    df_mod_obs_onestep |>
      unnest(modobs) |>
      # make this work gracefully in case nrow=0
      ensure_cols_defined(tibble(chi = list(), chi_mod__ = numeric())) |>
      unnest(chi) |>
      # make this work gracefully in case nrow=0
      ensure_cols_defined(tibble(chi_obs__ = numeric())) |>
      rename(all_of(c(mod = "chi_mod__", obs = "chi_obs__"))) |>
      mutate(target  = "chi",#curr_target,
             err_par = par[["err_chi"]]) |> #par[[paste0("err_,"curr_target]]) |>
      select(sitename, run_model, target, mod, obs, err_par),

    df_mod_obs_onestep |>
      unnest(modobs) |>
      # make this work gracefully in case nrow=0
      ensure_cols_defined(tibble(vj = list(), vj_mod__ = numeric())) |>
      unnest(vj) |>
      # make this work gracefully in case nrow=0
      ensure_cols_defined(tibble(vj_obs__ = numeric())) |>
      rename(all_of(c(mod = "vj_mod__", obs = "vj_obs__"))) |>
      mutate(target  = "vj",#curr_target,
             err_par = par[["err_vj"]]) |> #par[[paste0("err_,"curr_target]]) |>
      select(sitename, run_model, target, mod, obs, err_par)
  )
  stopifnot(all(targets %in% c("err_gpp", "err_chi", "err_vj"))) # above hardcoded snippet is wrong if this is not the case

  # NOTE: below was used to solve issue with NA in modeled or observed values
  # df_mod_obs |> filter(is.na(obs)) # OK
  # df_mod_obs |> filter(is.na(mod)) # OK
  # df_mod_obs_daily   |> unnest(modobs) |> filter(is.na(gpp_mod))                   # OK
  # df_mod_obs_daily   |> unnest(modobs) |> filter(is.na(gpp))                       # OK
  # df_mod_obs_onestep |> unnest(modobs) |> unnest(vj) |> filter(is.na(vj_mod__))    # OK
  # df_mod_obs_onestep |> unnest(modobs) |> unnest(vj) |> filter(is.na(vj_obs__))    # OK
  # df_mod_obs_onestep |> unnest(modobs) |> unnest(chi) |> filter(is.na(chi_mod__))  # OK
  # df_mod_obs_onestep |> unnest(modobs) |> unnest(chi) |> filter(is.na(chi_obs__))  # OK

  # D) Compute likelihood ----

  # compute ll
  df_ll <- df_mod_obs |>
    filter(!is.na(obs)) |>
    mutate(ll = stats::dnorm( # compute loglikelihood
      x    = obs - mod,
      mean = 0,
      sd   = err_par,
      log  = TRUE
    )) |>
    select(sitename, run_model, target, mod, obs, err_par, ll)

  # df_ll
  ll <- sum(df_ll$ll)

  # trap boundary conditions
  if(is.nan(ll) | is.na(ll) | ll == 0){ll <- -Inf}

  return(ll)
}

