# Script to be sourced: defines where to store outputs and what details of the calibration scenarios

rsofun_doc_output_path <- if (grepl("node", Sys.info()["nodename"])) {
  # on UBELIX (think about rsyncing this to a permanent storage)
  "/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs"
} else if (Sys.info()["nodename"] == "dash") {
  # on WS-02
  "/data_2/scratch/fbernhard/rsofun_doc_outputs"
} else {
  stop("Please specify where to store output as 'rsofun_doc_output_path'.")
}


# FROM THE REVISION PLAN:
# Setup 1: global, reduced parameter set (as in initial manuscript version), only GPP as target
# Setup 2: global, full parameter set, only GPP as target
# Setup 3: global, full parameter set, GPP and traits as target
# We expect Setup 2 to yield wider posteriors than from Setup 1, and that posterior distributions will be narrowed again by Setup 3. This experimental design will allow us to demonstrate the robustness (or absence thereof) of the MCMC and the usefulness of using traits for simultaneously calibrating with fluxes.
# NOTE:
# Setups that were used were:
#    228: Setup a)      Delta^'13 C             
#    227: Setup b)      VJ                        
#    226: Setup c)      Delta^'13 C*',VJ       
#    222: Setup d)      GPP                      
#    223: Setup e)      Delta^'13 C*',VJ, GPP  
#    231: Setup h)      Delta^'13 C*',VJ, GPP  

#    220: Setup X)      GPP (of single site FR-Pue),  and less parameters (fixing: beta_unitcostratio, rd_to_vcmax, tau_acclim, kc_jmax)
#    221: Setup XX)     GPP                           and less parameters (fixing: beta_unitcostratio, rd_to_vcmax, tau_acclim, kc_jmax)

setup_rsofun_calibration <- function(scenario){
  require(tidyverse)
  require(rpmodel)
  require(rgeco) # pak::pkg_install("geco-bern/rgeco")
  require(dplyr)
  require(purrr)
  require(rsofun)  # install from branch simple_pmodel_v2
  # require(ingestr)
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

  ## Setup the settings for the different calibration scenarios ----
  ## Define default parameter
  default_par_fixed <- list(# fix parameter value from previous calibration
    kphio              = 0.04998,    # value from Stocker et al. 2020
    kphio_par_a        = 0.0,        # 0 corresponds to no temperature dependency of kphio (ORG setup in Stocker et al. 2020)
    kphio_par_b        = 1.0,        #
    soilm_thetastar    = 0.6 * 240,  # to recover paper setup with soil moisture stress
    soilm_betao        = 0.0,        # 1 corresponds to no reduction, 0 to full reduction at theta==0
    beta_unitcostratio = 146.0,      # value from Stocker et al. 2020
    rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 14.0,       # value from Liu et al. 2024
    kc_jmax            = 0.41,       # value from Stocker et al. 2024 (citing Wang et al. 2017)
    errbias_bigD13C    = 0.0,
    errbias_vj         = 0.0,
    errscale_gpp       = 1.0
  )

  ## Define parameters to estimate and their priors
  if (scenario %in% c(220,221,222,223,224,225,226,227,228,           # the 220s are reruns from 2025-09-19 that remove again the bias))
                      229,230,231)){                                 # 229,230,231 are like 223 but using posteriors from 226
    par_to_estimate <- list(
      kphio           = list(lower = 0.02, upper = 0.15, init = 0.05),
      kphio_par_a     = list(lower = -0.004, upper = -0.001, init = -0.0025),
      kphio_par_b     = list(lower = 10, upper = 30, init = 20),
      soilm_thetastar = list(lower = 1, upper = 250, init = 40),
      # soilm_betao        = list(lower = 0.0, upper = 1.0, init = 0.0),
      # rd_to_vcmax        = as.list(c(lower = 0.1, upper = 3.0, init = 1.0)*0.014), # use a fixed rd_to_vcmax      # 0.014 value from Atkin et al. 2015 for C3 herbaceous
      beta_unitcostratio = as.list(c(lower = 0.1, upper = 3.0, init = 1.0)*146.0),
      tau_acclim         = list(mean = 14, sd = 8, lower = 0.01, upper = 40),           # truncated normal, with ~14 days as mean, taken from Liu et al. 2024, Nat.Plants and Mäkelä et al. 2004, Tree Phys.
      kc_jmax            = as.list(c(lower = 0.1, upper = 3.0, init = 1.0)*0.41),
      err_gpp         = list(lower = 0.01, upper = 3, init = 0.8),
      err_bigD13C     = list(lower = 0.01, upper = 3, init = 0.8),
      err_vj          = list(lower = 0.01, upper = 3, init = 0.8),
      errbias_bigD13C = list(lower =   -8, upper = 8, init = 0.0),
      errbias_vj      = list(lower =   -1, upper = 1, init = 0.0)
    )
    if (scenario %in% c(220,221)){
      # reduce number of parameters to estimate:
      par_to_estimate$beta_unitcostratio <- NULL
      par_to_estimate$rd_to_vcmax        <- NULL
      par_to_estimate$tau_acclim         <- NULL
      par_to_estimate$kc_jmax            <- NULL
    }
    if (scenario %in% c(224)){
      par_to_estimate$kphio           <- list(mean    = 0.0479684950570567,   sd    = 9.75104729575593e-05)
      par_to_estimate$kphio_par_a     <- list(mean    = -0.00179211384220008, sd    = 2.98616456930556e-05)
      par_to_estimate$kphio_par_b     <- list(mean    = 18.4293950588911,     sd    = 0.102867468875224)
      par_to_estimate$soilm_thetastar <- list(mean    = 27.0859346061886,     sd    = 0.762249191490997)
    }
    if (scenario %in% c(225)){
      par_to_estimate$kphio           <- NULL
      par_to_estimate$kphio_par_a     <- NULL
      par_to_estimate$kphio_par_b     <- NULL
      par_to_estimate$soilm_thetastar <- NULL
    }
    if (scenario %in% c(220,221,222,223,224,225,226,227,228,           # the 220s are reruns from 2025-09-19 that remove again the bias
                        229,230,231)){                                 # 229,230,231 are like 223 but using posteriors from 226
      par_to_estimate$err_bigD13C     <- list(lower = 0.01, upper = 15, init = 0.8)
      par_to_estimate$errbias_bigD13C <- NULL
      par_to_estimate$errbias_vj      <- NULL
    }
  } else {
    stop(sprintf("Unsupported scenario: %d", scenario))
  }

  if (scenario %in% c(229,231)){   # 229 is like 223 but using posteriors from 226 for beta_unitcostratio and kc_jmax as priors
                # read in posteriors from scenario 1 as prior for 14
                calib_scen226 <- readr::read_rds(file.path(rsofun_doc_output_path, "data","calibrations","out_calib__scen226_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
                # "/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen226_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"

                # i) extract samples as a data.frame
                burnins_scen226 <- 30000
                samples_scen226 <- getSample(calib_scen226$mod, thin = 1, start = burnins_scen226) %>% as.data.frame()

                # ii) fit normal and lognormal distributions for each parameter
                param_normals_scen226 <- lapply(setNames(names(samples_scen226), names(samples_scen226)), function(p) {
                  list(mean = mean(samples_scen226[[p]]),
                      sd   = sd(  samples_scen226[[p]]))
                })[c('beta_unitcostratio', 'kc_jmax')] # only keep these
                # param_lognormals_scen226 <- lapply(list(soilm_betao = 'soilm_betao'), function(p) { # only for soilm_betao
                #   list(meanlog = mean(log(samples_scen226[[p]])),
                #       sdlog   = sd(  log(samples_scen226[[p]])))
                # })

                # then pass on these as prior for these
                par_to_estimate$beta_unitcostratio <- param_normals_scen226$beta_unitcostratio
                par_to_estimate$kc_jmax            <- param_normals_scen226$kc_jmax

    par_to_estimate$beta_unitcostratio <- list(mean    = 207.86, sd    = 6.79)
    par_to_estimate$kc_jmax            <- list(mean    = 0.4244, sd    = 0.0217)
    if (scenario %in% c(231)){
      par_to_estimate$beta_unitcostratio <- list(mean    = 207.86, sd    = 6.79,    lower = 207.86 - 3*6.79, upper = 207.86 + 3*6.79)   # truncated normal
      par_to_estimate$kc_jmax            <- list(mean    = 0.4244, sd    = 0.0217)
    }
  }
  if (scenario %in% c(230)){   # 230 is like 223 but using posteriors from 226 for beta_unitcostratio and kc_jmax as fixed
    par_to_estimate$beta_unitcostratio <- NULL
    par_to_estimate$kc_jmax            <- NULL
  }

  # Remove parameters that are defined to be estimated from default_par_fixed
  par_to_fix <- default_par_fixed[!(names(default_par_fixed) %in% names(par_to_estimate))]
  if (scenario %in% c(225)) {
    par_to_fix$kphio           <- 0.0479684950570567
    par_to_fix$kphio_par_a     <- -0.00179211384220008
    par_to_fix$kphio_par_b     <- 18.4293950588911
    par_to_fix$soilm_thetastar <- 27.0859346061886
    par_to_fix$soilm_betao <- 0
  }
  if (scenario %in% c(230)){   # 230 is like 223 but using posteriors from 226 for beta_unitcostratio and kc_jmax as fixed
    par_to_fix$beta_unitcostratio <- 207.86
    par_to_fix$kc_jmax            <- 0.4244
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

  if (scenario %in% c(221,222)){ # only GPP data

    drivobs_train <- drivobs_train_bigD13C_vj_gpp |>
      unnest_wider(targets) |>
      filter(gpp) |>
      nest(targets = c(vj, bigD13C, gpp))
    drivobs_test <- drivobs_test_bigD13C_vj_gpp ## for the test data set keep all

  } else if (scenario %in% c(223,224,225, 229,230,231)) { # GPP and traits data
    drivobs_train<- drivobs_train_bigD13C_vj_gpp
    drivobs_test <- drivobs_test_bigD13C_vj_gpp
  } else if (scenario %in% c(226,227,228)) {              # only traits data, either both, or vj only, or bigD13C only
    drivobs_train <- drivobs_train_bigD13C_vj_gpp |>
      unnest_wider(targets) |>
      filter(case_when(scenario %in% c(226) ~ vj | bigD13C,
                       scenario %in% c(227) ~ vj & !bigD13C,  # NOTE: this removes the three sites that have vj AND bigD13C
                       scenario %in% c(228) ~ bigD13C & !vj,  # NOTE: this removes the three sites that have vj AND bigD13C
                       TRUE ~ TRUE)) |>
      nest(targets = c(vj, bigD13C, gpp))
    drivobs_test <- drivobs_test_bigD13C_vj_gpp ## for the test data set keep all
  } else if (scenario %in% c(220)) {  # only GPP data from FR-Pue

    drivobs_train <- tibble( # load it based on FR-Pue data:
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

    # drivobs_test <- drivobs_train |> dplyr::slice(0) # No test dataset
    drivobs_test <- drivobs_test_bigD13C_vj_gpp ## for the test data set keep all

  } else {
    stop(sprintf("Unsupported scenario: %d", scenario))
  }

  ## return ---
  return(list(
    drivobs_train = drivobs_train,
    drivobs_test = drivobs_test,
    par_fixed = par_to_fix,
    par = par_to_estimate
  ))
}
