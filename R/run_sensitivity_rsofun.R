source(here::here("R/calibration_helpers.R"))
source(here::here("R/sensitivity_sofun_serialized.R"))
source(here::here("analysis/00_define_setups.R"))

run_sensitivity_rsofun <- function(
    iterations = 3,
    par_ranges_derived_from, # this could either be of class "prior" or of class "mcmcSamplerList" i.e. posterior
    drivobs,
    design, # to be handed to morris::sensitivity()
    outpath,
    suffix_str,
    prior_par_definitions = NULL # default is NULL, only needed if is(par_ranges_derived_from, "prior")
  ){
  stopifnot(
    (is(par_ranges_derived_from, "mcmcSamplerList") && is.null(prior_par_definitions)) ||
      (is(par_ranges_derived_from, "prior")         && is.list(prior_par_definitions))
  )

  # Load loglikelihood
  source(here::here("R/calibration_helpers.R"), echo = FALSE)
  source(here::here("R/cost_likelihood_pmodel_bigD13C_vj_gpp.R"), echo = FALSE)

  # Define parameters to vary and their range based on either prior or posterior
  # define data.frame 'par_ranges' := data.frame(parameter_name=..., lower=..., upper=...)
  if (is(par_ranges_derived_from, "mcmcSamplerList")) {

    # define ranges as quantiles of posterior
    df_posterior <- BayesianTools::getSample(
        par_ranges_derived_from,
        parametersOnly = TRUE,
        start = 25000) |>
      as.data.frame()
    # BayesianTools::getCredibleIntervals(par_ranges_derived_from)
    # BayesianTools::getPredictiveIntervals(par_ranges_derived_from)

    par_ranges <- lapply(df_posterior, \(vec) {
        quantile(vec, c(0.05, 0.95)) |> purrr::set_names(c("lower","upper"))
      }) |>
      as.data.frame() |> as_tibble(rownames = "percentile") |>
      pivot_longer(-percentile, names_to = "parameter_name") |>
      pivot_wider(names_from = percentile)

  } else if (is(par_ranges_derived_from, "prior")) {

    binf_bsup <- getPriorMinMaxRanges(par_ranges_derived_from, prior_par_definitions)
    stopifnot(length(binf_bsup$inflim) == length(prior_par_definitions))
    stopifnot(all(names(binf_bsup$inflim) == names(prior_par_definitions)))

    par_ranges = data.frame(
      parameter_names = names(prior_par_definitions),
      lower           = unname(binf_bsup$inflim),
      upper           = unname(binf_bsup$suplim)
    )

    # alternatively we could also use quantiles of prior:
    # define ranges as quantiles of posterior
    # df_prior <- par_ranges_derived_from$sampler(25000) |>
    #   as.data.frame()
    # names(df_prior) <- names(prior_par_definitions)
    #
    # par_ranges <- lapply(df_prior, \(vec) {
    #     quantile(vec, c(0.05, 0.95)) |> purrr::set_names(c("lower","upper"))
    #   }) |>
    #   as.data.frame() |> as_tibble(rownames = "percentile") |>
    #   pivot_longer(-percentile, names_to = "parameter_name") |>
    #   pivot_wider(names_from = percentile)

  } else {
    stop("Error received `par_ranges_derived_from` that is neither of class 'prior' nor 'mcmcSamplerList'")
  }

  # Setup Morris sensitivity analysis
  sensitivity_sofun_settings <- list(
    metric = cost_likelihood_pmodel_bigD13C_vj_gpp,
    par_ranges = par_ranges,
    control = list(
      settings = list(
        iterations = iterations,
        design     = design
      )
    )
  )

  # # Run sensitivity
  out_morris <- sensitivity_sofun_serialized(
    drivers  = select(drivobs, sitename, run_model, params_siml, site_info, forcing),
    obs      = select(drivobs, sitename, run_model, targets, data),
    settings = sensitivity_sofun_settings,
    suffix   = suffix_str,       # for storing results
    outpath  = outpath           # for storing results
  ) # this stores the whole out_morris in an rds object identified by "suffix_str" (into outpath)

  return(out_morris)
}







run_sensitivity_rsofun_1point5IQR <- function(
    iterations = 3,
    par_ranges_derived_from, # this could either be of class "prior" or of class "mcmcSamplerList" i.e. posterior
    burnins,
    drivobs,
    design, # to be handed to morris::sensitivity()
    outpath,
    suffix_str,
    prior_par_definitions = NULL # default is NULL, only needed if is(par_ranges_derived_from, "prior")
  ){
  stopifnot(
    (is(par_ranges_derived_from, "mcmcSamplerList") && is.null(prior_par_definitions)) ||
      (is(par_ranges_derived_from, "prior")         && is.list(prior_par_definitions))
  )

  # Load loglikelihood
  source(here::here("R/calibration_helpers.R"), echo = FALSE)
  source(here::here("R/cost_likelihood_pmodel_bigD13C_vj_gpp.R"), echo = FALSE)

  # Define parameters to vary and their range based on either prior or posterior
  # define data.frame 'par_ranges' := data.frame(parameter_name=..., lower=..., upper=...)
  if (is(par_ranges_derived_from, "mcmcSamplerList")) {

    # define ranges as quantiles of posterior
    df_posterior <- BayesianTools::getSample(
        par_ranges_derived_from,
        parametersOnly = TRUE,
        start = burnins) |>
      as.data.frame()
    # BayesianTools::getCredibleIntervals(par_ranges_derived_from)
    # BayesianTools::getPredictiveIntervals(par_ranges_derived_from)

    par_ranges <- lapply(df_posterior, \(vec) {
        quantile(vec, c(0.25, 0.75)) |> purrr::set_names(c("lower_quartile","upper_quartile"))
      }) |>
      as.data.frame() |> as_tibble(rownames = "percentile") |>
      pivot_longer(-percentile, names_to = "parameter_name") |>
      pivot_wider(names_from = percentile)

    # compute lower and upper ranges as 1.5*IQR added to 25%- and 75%-quartile.
    par_ranges <- par_ranges |>
      mutate(IQR = upper_quartile - lower_quartile) |>
      mutate(lower = lower_quartile -  1.5*IQR,
             upper = upper_quartile +  1.5*IQR)

    # print(par_ranges)
    par_ranges <- par_ranges |> select(parameter_name, lower, upper)

  } else if (is(par_ranges_derived_from, "prior")) {

    binf_bsup <- getPriorMinMaxRanges(par_ranges_derived_from, prior_par_definitions)
    stopifnot(length(binf_bsup$inflim) == length(prior_par_definitions))
    stopifnot(all(names(binf_bsup$inflim) == names(prior_par_definitions)))

    par_ranges = data.frame(
      parameter_names = names(prior_par_definitions),
      lower           = unname(binf_bsup$inflim),
      upper           = unname(binf_bsup$suplim)
    )

    # alternatively we could also use quantiles of prior:
    # define ranges as quantiles of posterior
    # df_prior <- par_ranges_derived_from$sampler(25000) |>
    #   as.data.frame()
    # names(df_prior) <- names(prior_par_definitions)
    #
    # par_ranges <- lapply(df_prior, \(vec) {
    #     quantile(vec, c(0.05, 0.95)) |> purrr::set_names(c("lower","upper"))
    #   }) |>
    #   as.data.frame() |> as_tibble(rownames = "percentile") |>
    #   pivot_longer(-percentile, names_to = "parameter_name") |>
    #   pivot_wider(names_from = percentile)

  } else {
    stop("Error received `par_ranges_derived_from` that is neither of class 'prior' nor 'mcmcSamplerList'")
  }

  # NEW: remove error model parameters from sensitivity analysis since they are interacting extremely with the other parameters
  par_ranges_no_errors <- par_ranges |>
    filter(!grepl("^err",parameter_name)) # catches err_gpp, errscale_gpp, errbias_gpp, etc...
  par_fixed_df <- par_ranges |>
    filter(grepl("^err",parameter_name)) |> # catches err_gpp, errscale_gpp, errbias_gpp, etc...
    mutate(mean = (lower + upper)/2)
  par_fixed <- par_fixed_df$mean
  names(par_fixed) <- par_fixed_df$parameter_name

  # also ensure that we have the scale and bias parameters set
  par_fixed2 <- c(par_fixed, list(errscale_gpp = 1, errbias_vj = 0, errbias_bigD13C=0)) # TODO: this might introduce an error if they are now double
  stopifnot(length(names(par_fixed2)) == length(unique(names(par_fixed2))))

  # Setup Morris sensitivity analysis
  sensitivity_sofun_settings <- list(
    metric = cost_likelihood_pmodel_bigD13C_vj_gpp,
    par_ranges = par_ranges_no_errors,
    par_fixed = par_fixed2,
    control = list(
      settings = list(
        iterations = iterations,
        design     = design
      )
    )
  )

  # # Run sensitivity
  out_morris <- sensitivity_sofun_serialized(
    drivers  = select(drivobs, sitename, run_model, params_siml, site_info, forcing),
    obs      = select(drivobs, sitename, run_model, targets, data),
    settings = sensitivity_sofun_settings,
    suffix   = suffix_str,       # for storing results
    outpath  = outpath           # for storing results
  ) # this stores the whole out_morris in an rds object identified by "suffix_str" (into outpath)

  return(out_morris)
}
