cost_likelihood_pmodel_bigD13C_vj_gpp_v2 <- function(
    par,   # model parameters & error terms for each target
    obs,     # UNUSED
    drivers, # UNUSED
    par_fixed = NULL,   # non-calibrated model parameters
    parallel  = FALSE,
    ncores    = 1,
    daily_drivers,    # daily_drivers <- filter(drivers, run_model == "daily")
    onestep_drivers,  # onestep_drivers <- drivers |>
    # filter(run_model == "onestep") |>
    #   # filter(FALSE) |>
    #   group_by(sitename) |>
    #   unnest(c(params_siml, forcing))
    daily_obs,        # daily_obs <- obs |>
    # filter(run_model == "daily") |>
    # select(sitename, run_model, targets, data) |>
    # unnest(c(data)) |>
    # # make this work gracefully in case nrow=0
    # ensure_cols_defined(tibble(date = as.Date(character())))
    onestep_obs       # onestep_obs <- obs |>
    # filter(run_model == "onestep") |>
    # select(sitename, run_model, targets, data) |>
    # unnest(data) |>
    # # make this work gracefully in case nrow=0
    # ensure_cols_defined(tibble(bigD13C = list(), vj = list()))
){

  # stopifnot(nrow(obs) > 0)     # ensure some observation data are provided
  # stopifnot(nrow(drivers) > 0) # ensure some driver data are provided

  # A) Include current parameters ----
  stopifnot(length(intersect(names(par), names(par_fixed))) == 0) # no overlap
  params_modl <- c(par, par_fixed)


  # B) Run model ----

  ## run the time series model for gpp/et/... time series
  df_daily <- runread_pmodel_f(
    drivers   = daily_drivers,
    par       = params_modl,
    makecheck = FALSE,        # TODO: disable check
    parallel  = parallel,
    ncores    = ncores
  ) |>
    unnest(data) |>
    # make this work gracefully in case nrow=0
    # ensure_cols_defined(tibble(date = as.Date(character()),gpp = numeric(),le = numeric())) |>
    select(sitename, date, gpp_mod = gpp, le_mod = le)

  ## run the onestep model for traits
  # NOTE: this is to make this work gracefully even when no simulations are requested
  if (nrow(onestep_drivers) == 0){ # no onestep simulations requested, generate dummy output:
    df_onestep <- tibble(
      sitename                 = character(),
      vcmax_mod_molm2s         = numeric(),
      jmax_mod_molm2s          = numeric(),
      vcmax25_mod_molm2s       = numeric(),
      jmax25_mod_molm2s        = numeric(),
      gs_accl_mod_molCmolPhPa  = numeric(),
      wscal_mod__              = numeric(),
      bigD13C_mod_permil       = numeric(),
      iwue_mod__               = numeric(),
      rd_mod_gCm2s             = numeric(),
      vj_mod__                 = numeric())
  } else { # only run onestep simulations if requested, generate simulation output:
    df_onestep <- onestep_drivers |>
      group_modify(~run_pmodel_onestep_f_bysite(
        lc4 = FALSE,
        # select what forcing columns to use:
        forcing =  data.frame(temp = .x$temp,
                              vpd  = .x$vpd,
                              ppfd = .x$ppfd,
                              co2  = .x$co2,
                              patm = .x$patm),
        params_modl = params_modl,
        makecheck   = FALSE)) |> # TODO: disable check
      # rename(vcmax_mod_molm2s   = vcmax,
      #        jmax_mod_molm2s    = jmax,
      #        vcmax25_mod_molm2s = vcmax25,
      #        jmax25_mod_molm2s  = jmax25,
      #        gs_accl_mod_molCmolPhPa = gs_accl,
      #        bigD13C_mod_permil      = bigdelta,
      #        iwue_mod__         = iwue,
      #        rd_mod_gCm2s       = rd) |>
      # mutate(vj_mod__ = vcmax_mod_molm2s/jmax_mod_molm2s)
      mutate(vj_mod__ = vcmax/jmax) |> rename(bigD13C_mod_permil = bigdelta)
  }
  df_onestep <- df_onestep |> select(sitename, #vcmax_mod_molm2s, jmax_mod_molm2s,
                                     bigD13C_mod_permil,
                                     vj_mod__)

  # C) Bring together modelled and observed ----

  # NOTE: calibration targets can be controlled by providing no forcing and
  #       observation data for certain targets
  # NOTE: in that case some of the tibbles() will be empty (i.e. nrow() == 0)
  # NOTE: the unnesting operation does not work in case, since there is no
  #       nested element to infer the column names and column types
  # NOTE: below function makes unnest() work gracefully even in case nrow=0
  # ensure_cols_defined <- function(df, expected_columns){
  #   if(nrow(df)==0){
  #     df |>
  #       # replace the unspecified column with expected columns
  #       select(!where(\(cl){class(cl)=="vctrs_unspecified"})) |>
  #       cross_join(expected_columns)
  #   } else {df}
  # }

  df_mod_obs_daily <- daily_obs |>
    # join the modelled data
    left_join(df_daily, by = join_by(sitename, date)) #|>
  # nest again
  # nest(modobs = -c(sitename, run_model, targets))

  df_mod_obs_onestep <- onestep_obs |>
    # join the modelled data
    left_join(df_onestep, by = join_by(sitename)) #|>
  # nest again
  # nest(modobs = -c(sitename, run_model, targets))

  # combine into single data.frame
  targets <- grep("^err_", names(par), value = TRUE)

  # for (curr_target in targets){
  #   print(curr_target)
  # } # or alternativel lapply
  # or hardcode:
  df_mod_obs <- bind_rows(
    df_mod_obs_daily |> # unnest(modobs) |>
      # make this work gracefully in case nrow=0
      # ensure_cols_defined(tibble(gpp_mod = numeric(), gpp = numeric())) |>
      rename(all_of(c(mod = "gpp_mod", obs = "gpp"))) |>
      mutate(#target  = "gpp",#curr_target,
        err_par_sd = par[["err_gpp"]]), #|> #par[[paste0("err_,"curr_target]]) |>
    # select(sitename, run_model, target, mod, obs, err_par_sd),

    df_mod_obs_onestep |> # unnest(modobs) |>
      # make this work gracefully in case nrow=0
      # ensure_cols_defined(tibble(bigD13C = list(), bigD13C_mod_permil = numeric())) |>
      unnest(bigD13C) |>
      # make this work gracefully in case nrow=0
      # ensure_cols_defined(tibble(bigD13C_obs_permil = numeric())) |>
      rename(all_of(c(mod = "bigD13C_mod_permil", obs = "bigD13C_obs_permil"))) |>
      mutate(#target  = "bigD13C",#curr_target,
        err_par_sd = par[["err_bigD13C"]]), #|> #par[[paste0("err_,"curr_target]]) |>
    # select(sitename, run_model, target, mod, obs, err_par_sd),

    df_mod_obs_onestep |> # unnest(modobs) |>
      # make this work gracefully in case nrow=0
      # ensure_cols_defined(tibble(vj = list(), vj_mod__ = numeric())) |>
      unnest(vj) |>
      # make this work gracefully in case nrow=0
      # ensure_cols_defined(tibble(vj_obs__ = numeric())) |>
      rename(all_of(c(mod = "vj_mod__", obs = "vj_obs__"))) |>
      mutate(#target  = "vj",#curr_target,
        err_par_sd = par[["err_vj"]]), #|> #par[[paste0("err_,"curr_target]]) |>
    # select(sitename, run_model, target, mod, obs, err_par_sd)
  )
  stopifnot(all(targets %in% c("err_gpp", "err_bigD13C", "err_vj"))) # above hardcoded snippet is wrong if this is not the case

  # D) Compute likelihood ----
  ll_normal    <- function(obs,mod,sd){stats::dnorm( x=obs, mean = mod,                sd    = sd, log = TRUE)} # TODO: err_par_sd must be positive
  # ll_lognormal <- function(obs,mod,sd){stats::dlnorm(x=obs, meanlog = mod,             sdlog = sd, log = TRUE)} # TODO: err_par_sd must be positive
  # ll_lognormal2<- function(obs,mod,sd){stats::dlnorm(x=obs, meanlog = log(mod) + sd^2, sdlog = sd, log = TRUE)}
  # ll_proportional<-function(obs,mod,sd){stats::dnorm(x=obs, mean = mod,                sd = abs(mod)*sd, log = TRUE)} # proportional: https://docs.pumas.ai/stable/model_components/error_models/
  # ll_userdefined <- function(obs,mod,err_par1, err_par2, err_par3){}

  # compute ll
  df_ll <- df_mod_obs |> # group_by(target, err_par_sd) |>
    # compute loglikelihoods
    # rowwise() |> # not needed and slowing things down
    mutate(ll = ll_normal(obs,mod,err_par_sd)) #|>
  # select(sitename, run_model, target, mod, obs, err_par_sd, ll)

  ll <- sum(df_ll$ll)

  # trap boundary conditions
  if(is.nan(ll) | is.na(ll) | ll == 0){ll <- -Inf}

  return(ll)
}













cost_likelihood_pmodel_bigD13C_vj_gpp_v3 <- function(
    par,   # model parameters & error terms for each target
    obs,     # UNUSED
    drivers, # UNUSED
    par_fixed = NULL,   # non-calibrated model parameters
    parallel  = FALSE,
    ncores    = 1,
    daily_drivers,
    onestep_drivers,
    daily_obs,
    onestep_obs
){
  # A) Include current parameters ----
  params_modl <- c(par, par_fixed)

  # B) Run model ----
  ## run the time series model for gpp/et/... time series
  df_daily <- runread_pmodel_f(
    drivers   = daily_drivers,
    par       = params_modl,
    makecheck = FALSE,
    parallel  = parallel,
    ncores    = ncores
  ) |>
    unnest(data) |>
    select(sitename, date, gpp_mod = gpp, le_mod = le)

  ## run the onestep model for traits
  df_onestep <- onestep_drivers |>
    group_modify(~run_pmodel_onestep_f_bysite(
      lc4 = FALSE,
      # select what forcing columns to use:
      forcing =  data.frame(temp = .x$temp,
                            vpd  = .x$vpd,
                            ppfd = .x$ppfd,
                            co2  = .x$co2,
                            patm = .x$patm),
      params_modl = params_modl,
      makecheck   = FALSE)) |>
    mutate(vj_mod__ = vcmax/jmax) |> rename(bigD13C_mod_permil = bigdelta)

  # C) Bring together modelled and observed ----
  df_mod_obs_daily <-
    left_join(daily_obs, df_daily, by = join_by(sitename, date))
  df_mod_obs_onestep <-
    left_join(onestep_obs, df_onestep, by = join_by(sitename))
  #
  #   # combine into single data.frame
  #   df_mod_obs <- bind_rows(
  #     df_mod_obs_daily |>
  #       rename(all_of(c(mod = "gpp_mod", obs = "gpp"))) |>
  #       mutate(err_par_sd = par[["err_gpp"]]),
  #     df_mod_obs_onestep |>
  #       unnest(bigD13C) |>
  #       rename(all_of(c(mod = "bigD13C_mod_permil", obs = "bigD13C_obs_permil"))) |>
  #       mutate(err_par_sd = par[["err_bigD13C"]]), #|> #par[[paste0("err_,"curr_target]]) |>
  #     df_mod_obs_onestep |>
  #       unnest(vj) |>
  #       rename(all_of(c(mod = "vj_mod__", obs = "vj_obs__"))) |>
  #       mutate(#target  = "vj",#curr_target,
  #         err_par_sd = par[["err_vj"]]), #|> #par[[paste0("err_,"curr_target]]) |>
  #     # select(sitename, run_model, target, mod, obs, err_par_sd)
  #   )
  #
  #   # D) Compute log-likelihood ----
  #   ll <- sum(stats::dnorm( x=df_mod_obs$obs, mean = df_mod_obs$mod, sd = df_mod_obs$err_par_sd, log = TRUE))
  #   browser()
  #   # trap boundary conditions
  #   if(is.nan(ll) | is.na(ll) | ll == 0){ll <- -Inf}
  #
  #   unnest(df_mod_obs_onestep, vj) |> filter(is.na(vj_mod__))
  ll2 <-
    sum(stats::dnorm( x=df_mod_obs_daily$gpp,                                   mean = df_mod_obs_daily$gpp_mod,                               sd = par[["err_gpp"]], log = TRUE)) +
    sum(stats::dnorm( x=unnest(df_mod_obs_onestep, bigD13C)$bigD13C_obs_permil, mean = unnest(df_mod_obs_onestep, bigD13C)$bigD13C_mod_permil, sd = par[["err_bigD13C"]], log = TRUE)) +
    sum(stats::dnorm( x=unnest(df_mod_obs_onestep, vj)$vj_obs__,                mean = unnest(df_mod_obs_onestep, vj)$vj_mod__,                sd = par[["err_vj"]], log = TRUE))

  return(ll2)
}
