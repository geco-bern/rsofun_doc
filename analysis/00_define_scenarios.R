setup_rsofun_calibration <- function(scenario = 3){
  # FROM THE REVISION PLAN:
  # Setup 1: global, reduced parameter set (as in initial manuscript version), only GPP as target
  # Setup 2: global, full parameter set, only GPP as target
  # Setup 3: global, full parameter set, GPP and traits as target
  # We expect Setup 2 to yield wider posteriors than from Setup 1, and that posterior distributions will be narrowed again by Setup 3. This experimental design will allow us to demonstrate the robustness (or absence thereof) of the MCMC and the usefulness of using traits for simultaneously calibrating with fluxes.

  require(tidyverse)
  require(rpmodel)
  require(rgeco) # pak::pkg_install("geco-bern/rgeco")
  require(dplyr)
  require(purrr)
  require(rsofun)  # install from branch simple_pmodel_v2
  require(ingestr)
  require(BayesianTools)

  ## Load forcing and targets data ----
  bigD13C_vj_gpp_drivers <- read_rds(here::here("data/01_bigD13C-vj-gpp_calibsofun_drivers.rds"))
  bigD13C_vj_gpp_obs     <- read_rds(here::here("data/01_bigD13C-vj-gpp_calibsofun_obs.rds"))

  ## Read test-train split ----
  df_test_train_split <- read_csv(
    here::here("data/01_test_train_split.csv"),
    col_types = cols(
      sitename = col_character(),
      run_model = col_character(),
      targets_vj = col_logical(),
      targets_bigD13C = col_logical(),
      targets_gpp = col_logical(),
      dataset = col_character()
    ))

  sites_train <- df_test_train_split |> filter(dataset == "train")
  sites_test  <- df_test_train_split |> filter(dataset == "test")

  ## Append test-train split thereby subsetting to only test and train sites ----
  bigD13C_vj_gpp_drivers <- bigD13C_vj_gpp_drivers |>
    inner_join(
      select(bind_rows(sites_train,sites_test), sitename, run_model, dataset),
      by = join_by(sitename, run_model))

  bigD13C_vj_gpp_obs <- bigD13C_vj_gpp_obs |>
    inner_join(
      select(bind_rows(sites_train,sites_test), sitename, run_model, dataset),
      by = join_by(sitename, run_model))

  ## Preprocess observation data (gpp) ----
  ## # no additinal QC needed

  ## Apply test-train split to data ----
  train_drivers <- bigD13C_vj_gpp_drivers |> filter(dataset == "train") |> select(-dataset)
  train_obs     <- bigD13C_vj_gpp_obs     |> filter(dataset == "train") |> select(-dataset)

  test_drivers <- bigD13C_vj_gpp_drivers |> filter(dataset == "test") |> select(-dataset)
  test_obs     <- bigD13C_vj_gpp_obs     |> filter(dataset == "test") |> select(-dataset)

  ## Setup the settings for the three calibration scenarios ----
  ## Define parameter
  default_par_fixed <- list(# fix parameter value from previous calibration
    kphio              = 0.04998,
    kphio_par_a        = 0.0,
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover paper setup with soil moisture stress
    soilm_betao        = 0.0,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 20.0,
    kc_jmax            = 0.41
  )
  if (scenario %in% c(1,4)){
    par_to_estimate <- list(
      kphio           = list(lower = 0.02, upper = 0.15, init = 0.05),
      kphio_par_a     = list(lower = -0.004, upper = -0.001, init = -0.0025),
      kphio_par_b     = list(lower = 10, upper = 30, init = 20),
      soilm_thetastar = list(lower = 1, upper = 250, init = 40),
      soilm_betao     = list(lower = 0.0, upper = 1.0, init = 0.0),
      err_gpp         = list(lower = 0.1, upper = 3, init = 0.8),
      err_bigD13C     = list(lower = 0.1, upper = 3, init = 0.8), # TODO: without err_bigD13C and err_vj this errors
      err_vj          = list(lower = 0.1, upper = 3, init = 0.8)  # TODO: without err_bigD13C and err_vj this errors
    )
    par_to_fix <- default_par_fixed[!(names(default_par_fixed) %in% names(par_to_estimate))]

  } else if (scenario %in% c(2,3)) {
    par_to_estimate <- list(
      kphio           = list(lower = 0.02, upper = 0.15, init = 0.05),
      kphio_par_a     = list(lower = -0.004, upper = -0.001, init = -0.0025),
      kphio_par_b     = list(lower = 10, upper = 30, init = 20),
      soilm_thetastar = list(lower = 1, upper = 250, init = 40),
      soilm_betao     = list(lower = 0.0, upper = 1.0, init = 0.0),
      beta_unitcostratio = as.list(c(lower = 0.1, upper = 3.0, init = 1.0)*146.0),
      rd_to_vcmax        = as.list(c(lower = 0.1, upper = 3.0, init = 1.0)*0.014),      # 0.014 value from Atkin et al. 2015 for C3 herbaceous
      tau_acclim         = as.list(c(lower = 0.1, upper = 3.0, init = 1.0)*20.0),
      kc_jmax            = as.list(c(lower = 0.1, upper = 3.0, init = 1.0)*0.41),
      err_gpp         = list(lower = 0.01, upper = 3, init = 0.8),
      err_bigD13C     = list(lower = 0.01, upper = 3, init = 0.8), # TODO: without err_bigD13C and err_vj this errors
      err_vj          = list(lower = 0.01, upper = 3, init = 0.8)  # TODO: without err_bigD13C and err_vj this errors
    )
    par_to_fix <- default_par_fixed[!(names(default_par_fixed) %in% names(par_to_estimate))]

  } else if (scenario %in% c(0)) {
    par_to_estimate <- list(
      kphio           = list(lower = 0.02, upper = 0.15, init = 0.05),
      kphio_par_a     = list(lower = -0.004, upper = -0.001, init = -0.0025),
      kphio_par_b     = list(lower = 10, upper = 30, init = 20),
      soilm_thetastar = list(lower = 1, upper = 250, init = 40),   # 4.32375, 259.425, 432.375
      soilm_betao     = list(lower = 0.0, upper = 1.0, init = 0.0),
      err_gpp         = list(lower = 0.01, upper = 3, init = 0.8), # 0.1, 0.8, 3.0
      err_bigD13C     = list(lower = 0.01, upper = 3, init = 0.8), # TODO: without err_bigD13C and err_vj this errors
      err_vj          = list(lower = 0.01, upper = 3, init = 0.8)  # TODO: without err_bigD13C and err_vj this errors
    )
    par_to_fix <- default_par_fixed[!(names(default_par_fixed) %in% names(par_to_estimate))]
  } else {
    stop(sprintf("Unsupported scenario: %d", scenario))
  }

  ## Setup the data (drivers and obs) for the three calibration scenarios ----

  # subset different combination of target variables
  # for easier handling do this in combined drivobs-object
  drivobs_train_bigD13C_vj_gpp <- dplyr::inner_join(
    train_drivers,
    train_obs,
    by = join_by(sitename, run_model))

  drivobs_test_bigD13C_vj_gpp <- dplyr::inner_join(
    test_drivers,
    test_obs,
    by = join_by(sitename, run_model))

  if (scenario %in% c(1,2,4)){
    drivobs <- drivobs_train_bigD13C_vj_gpp |>
      unnest_wider(targets) |>
      filter(gpp) |>
      nest(targets = c(vj, bigD13C, gpp))
    drivobs_test <- drivobs_test_bigD13C_vj_gpp |>
      unnest_wider(targets) |>
      filter(gpp) |>
      nest(targets = c(vj, bigD13C, gpp))

  } else if (scenario %in% c(3)) {
    drivobs <- drivobs_train_bigD13C_vj_gpp
    drivobs_test <- drivobs_test_bigD13C_vj_gpp

  } else if (scenario %in% c(0)) {
    drivobs <- tibble( # load it based on FR-Pue data:
      sitename    = rsofun::p_model_drivers$sitename,
      run_model   = "daily",
      params_siml = rsofun::p_model_drivers$params_siml,
      site_info   = rsofun::p_model_drivers$site_info,
      forcing     = rsofun::p_model_drivers$forcing,
      targets     = list(list(vj = FALSE, bigD13C = FALSE, gpp = TRUE)),
      data        = list(rsofun::p_model_validation$data[[1]] |>
                           mutate(gpp_qc = 0.90) |> # assume bigger than 0.8
                           filter(!is.na(gpp)))
    )

    drivobs_test <- drivobs |> dplyr::slice(0)

  } else {
    browser()
    stop(sprintf("Unsupported scenario: %d", scenario))
  }

  ## return ---
  return(list(
    drivobs = drivobs,
    drivobs_test = drivobs_test,
    # driver    = tibble(),
    # obs       = tibble(),
    # # TODO: check if passing combined drivobs is computationally more efficient for calib_sofun()
    par_fixed = par_to_fix,
    par = par_to_estimate
  )
  )
}
