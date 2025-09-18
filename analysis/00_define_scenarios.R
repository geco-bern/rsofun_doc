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


setup_rsofun_calibration <- function(scenario){
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
    kc_jmax            = 0.41        # value from Stocker et al. 2024 (citing Wang et al. 2017)
  )

  ## Define parameters to estimate and their priors
  if (scenario %in% c(120,121,122,123,124,125,126,127,128)){                # the 120s are reruns from 2025-09-17 that lower the prior bound of beta_unitcostratio to 0.01))
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
    if (scenario %in% c(120,121)){
      # reduce number of parameters to estimate:
      par_to_estimate$beta_unitcostratio <- NULL
      par_to_estimate$rd_to_vcmax        <- NULL
      par_to_estimate$tau_acclim         <- NULL
      par_to_estimate$tau_acclim         <- NULL
    }
  } else if (scenario %in% c(70,71, 80,81, 90,91, 0,1,4, 11, 31,32,33,34,35,36,37,38,39,40,41,42,     # the 70s (70,71,72,73,74,75,76,77,78) are reruns from 2025-09-03
                      110,111, # the 110s are reruns from 2025-09-11 that fix soilm_betao to 0.0
                      120,121, # the 120s are reruns from 2025-09-17 that lower the prior bound of beta_unitcostratio to 0.01
                                 51,52,53,54,55,56,57,58,59,60,61,62      )) {   # the 50s and 60s are the multi-site gpp only with a fixed beta0==0
    par_to_estimate <- list(
      kphio           = list(lower = 0.02, upper = 0.15, init = 0.05),
      kphio_par_a     = list(lower = -0.004, upper = -0.001, init = -0.0025),
      kphio_par_b     = list(lower = 10, upper = 30, init = 20),
      soilm_thetastar = list(lower = 1, upper = 250, init = 40), # NOTE: scenario 0 was previously: 4.32375, 259.425, 432.375
      soilm_betao     = list(lower = 0.0, upper = 1.0, init = 0.0),
      # no beta_unitcostratio,
      # no rd_to_vcmax,
      # no tau_acclim,
      # no kc_jmax,
      err_gpp         = list(lower = 0.1, upper = 3, init = 0.8), # NOTE: scenario 0 was previously: 0.1, 0.8, 3.0
      err_bigD13C     = list(lower = 0.1, upper = 3, init = 0.8), # TODO: without err_bigD13C and err_vj this errors
      err_vj          = list(lower = 0.1, upper = 3, init = 0.8), # TODO: without err_bigD13C and err_vj this errors
      errbias_bigD13C = list(lower =  -2, upper = 2, init = 0.0), # TODO: without bias_bigD13C and bias_vj this errors
      errbias_vj      = list(lower =  -2, upper = 2, init = 0.0)  # TODO: without err_bigD13C and err_vj this errors
    )
    if (scenario %in% c(90, 91, 110, 111, 120, 121)){
      par_to_estimate$errbias_bigD13C = list(lower =   -8, upper = 8, init = 0.0)
      par_to_estimate$errbias_vj      = list(lower =   -1, upper = 1, init = 0.0)
    }
    if (scenario %in% c(110, 111, 120, 121)){
      par_to_estimate$soilm_betao = NULL
    }
    if (scenario %in% c(51,52,53,54,55,56,57,58,59,60,61,62)){
      par_to_estimate$soilm_betao = NULL
    }
    if (scenario %in% c(120,121)){
      par_to_estimate$err_gpp         = list(lower = 0.01, upper = 3, init = 0.8)
      par_to_estimate$err_bigD13C     = list(lower = 0.01, upper = 3, init = 0.8) # TODO: without err_bigD13C and err_vj this errors
      par_to_estimate$err_vj          = list(lower = 0.01, upper = 3, init = 0.8) # TODO: without err_bigD13C and err_vj this errors
      par_to_estimate$errbias_bigD13C = list(lower =   -8, upper = 8, init = 0.0)
      par_to_estimate$errbias_vj      = list(lower =   -1, upper = 1, init = 0.0)
    }
    if (scenario %in% c(124)){ # use priors from posterior of scenario 1 for kphio, kphio_par_a, kphio_par_b, soilm_thetastar, soilm_betao
      par_to_estimate$kphio           <- list(mean    = 0.0479684950570567,   sd    = 9.75104729575593e-05)
      par_to_estimate$kphio_par_a     <- list(mean    = -0.00179211384220008, sd    = 2.98616456930556e-05)
      par_to_estimate$kphio_par_b     <- list(mean    = 18.4293950588911,     sd    = 0.102867468875224)
      par_to_estimate$soilm_thetastar <- list(mean    = 27.0859346061886,     sd    = 0.762249191490997)
    }
    if (scenario %in% c(125)) { # use fixed mean from posterior of scenario 1 for kphio, kphio_par_a, kphio_par_b, soilm_thetastar, soilm_betao
      par_to_estimate$kphio           <- NULL
      par_to_estimate$kphio_par_a     <- NULL
      par_to_estimate$kphio_par_b     <- NULL
      par_to_estimate$soilm_thetastar <- NULL
      par_to_estimate$soilm_betao     <- NULL
    }
  } else if (scenario %in% c(122,123,124,125,126,127,128,                # the 120s are reruns from 2025-09-17 that lower the prior bound of beta_unitcostratio to 0.01
                             112,113,114,115,116,117,118,                # the 110s are reruns from 2025-09-11 that fix soilm_betao to 0.0
                             92,93,94,95,96,97,98,                       # the 90s are reruns from 2025-09-06
                             82,83,84,85,86,87,88,                       # the 80s (80,81,82,83,84,85,86,87,88) are reruns from 2025-09-05
                             72,73,74,75,76,77,78,                       # the 70s (70,71,72,73,74,75,76,77,78) are reruns from 2025-09-03
                             2,3, 12,13, 14,15, 16,17,18)) {
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
      err_vj          = list(lower = 0.01, upper = 3, init = 0.8), # TODO: without err_bigD13C and err_vj this errors
      errbias_bigD13C = list(lower =   -2, upper = 2, init = 0.0), # TODO: without bias_bigD13C and bias_vj this errors
      errbias_vj      = list(lower =   -2, upper = 2, init = 0.0)  # TODO: without err_bigD13C and err_vj this errors
    )
    if (scenario %in% c(122,123,124,125,126,127,128,
                        112,113,114,115,116,117,118,
                        92,93,94,95,96,97,98,
                        82,83,84,85,86,87,88,
                        72,73,74,75,76,77,78)){ # use fixed rd_to_vcmax and use prior for tau_acclim
      par_to_estimate$rd_to_vcmax = NULL
      par_to_estimate$tau_acclim  = list(mean = 14, sd = 8, lower = 0, upper = 60) # truncated normal
      if (scenario %in% c(82,83,84,85,86,87,88)){
        par_to_estimate$errbias_bigD13C = list(lower =   -4, upper = 4, init = 0.0)
        par_to_estimate$errbias_vj      = list(lower =   -4, upper = 4, init = 0.0)
      } else if (scenario %in% c(122,123,124,125,126,127,128,
                                 112,113,114,115,116,117,118,
                                 92,93,94,95,96,97,98)){
        par_to_estimate$errbias_bigD13C = list(lower =   -8, upper = 8, init = 0.0)
        par_to_estimate$errbias_vj      = list(lower =   -1, upper = 1, init = 0.0)
      }
    }
    if (scenario %in% c(122,123,124,125,126,127,128,
                        112,113,114,115,116,117,118)){
      par_to_estimate$soilm_betao = NULL
    }

    if (scenario %in% c(74, 84, 94, 114, 124,
                        14)) { # use priors from posterior of scenario 1 for kphio, kphio_par_a, kphio_par_b, soilm_thetastar, soilm_betao

      # read in posteriors from scenario 1 as prior for 14
      calib_scen1 <- readr::read_rds(file.path(rsofun_doc_output_path, "data","calibrations","out_calib__scen1_DEzs-100000-0iter_8x3chains_on_CPU8x1.rds"))

      # fit a normal    distribution to: kphio, kphio_par_a, kphio_par_b, soilm_thetastar
      # fit a lognormal distribution to: soilm_betao

      # i) extract samples as a data.frame
      burnins_scen1 <- 4000
      samples_scen1 <- getSample(calib_scen1$mod, thin = 1, start = burnins_scen1) %>% as.data.frame()

      # ii) fit normal and lognormal distributions for each parameter
      param_normals_scen1 <- lapply(setNames(names(samples_scen1), names(samples_scen1)), function(p) {
        list(mean = mean(samples_scen1[[p]]),
             sd   = sd(  samples_scen1[[p]]))
      })[c('kphio', 'kphio_par_a', 'kphio_par_b', 'soilm_thetastar')] # only keep these
      param_lognormals_scen1 <- lapply(list(soilm_betao = 'soilm_betao'), function(p) { # only for soilm_betao
        list(meanlog = mean(log(samples_scen1[[p]])),
             sdlog   = sd(  log(samples_scen1[[p]])))
      })

              # fit other types of distributions
              # library(fitdistrplus) # can fit:
              #   # dbeta,  pbeta,  qbeta
              #   # dlnorm, plnorm, qlnorm
              #   # dnorm , pnorm , qnorm
              # fit <- fitdist(samples_scen1[["soilm_betao"]], "beta", method = "mle")
              # param_beta_scen1 <- list(soilm_betao = list(
              #   shape1 = fit$estimate[["shape1"]],
              #   shape2 = fit$estimate[["shape2"]])
              # )
              # fit <- fitdist(samples_scen1[["soilm_betao"]], "norm", method = "mle")
              # param_norm_scen1 <- list(soilm_betao = list(
              #   mean = fit$estimate[["mean"]],
              #   sd = fit$estimate[["sd"]])
              # )

              # # visual check of normal parameters:
              # pl_check <- plot_prior_posterior_density(calib_scen1$mod, burnin_to_skip = burnins_scen1) + ggtitle(calib_scen1$fpath)
              # normals_to_plot_across_facets <- param_normals_scen1 |> as.data.frame() |>
              #   pivot_longer(everything()) |> separate(name, into=c("variable","measure"), sep = "\\.") |>
              #   pivot_wider(names_from = measure, values_from = value)
              # pl_prior_check1 <- pl_check +
              #   geom_vline(data = normals_to_plot_across_facets, mapping=aes(xintercept=mean), color = "red") +
              #   geom_vline(data = normals_to_plot_across_facets, mapping=aes(xintercept=mean+sd, linestyle = "dashed"), color = "red") +
              #   geom_vline(data = normals_to_plot_across_facets, mapping=aes(xintercept=mean-sd, linestyle = "dashed"), color = "red")
              #
              # # visual check of soilmbetao:
              # pl_prior_check2 <- (pl_check %+% filter(pl_check$data, variable == "soilm_betao")) +
              #   geom_function(fun = function(x) 50/30 * dlnorm(x, mean = param_lognormals_scen1$soilm_betao$meanlog, sd = param_lognormals_scen1$soilm_betao$sdlog), n = 101,
              #                 mapping = aes(color = "dlnorm")) +
              #   geom_function(fun = function(x) 20/1500 * dbeta(x, shape1 = param_beta_scen1$soilm_betao$shape1, shape2 = param_beta_scen1$soilm_betao$shape2), n = 500,
              #                 mapping = aes(color = "beta")) +
              #   geom_function(fun = function(x) 1/1 * dnorm(x, mean = param_norm_scen1$soilm_betao$mean, sd = param_norm_scen1$soilm_betao$sd), n = 500,
              #                 mapping = aes(color = "norm"))
              # pl_prior_check1 / pl_prior_check2

      # then pass on these as prior for these
      par_to_estimate$kphio           <- param_normals_scen1$kphio
      par_to_estimate$kphio_par_a     <- param_normals_scen1$kphio_par_a
      par_to_estimate$kphio_par_b     <- param_normals_scen1$kphio_par_b
      par_to_estimate$soilm_thetastar <- param_normals_scen1$soilm_thetastar
      # par_to_estimate$soilm_betao     <- param_lognormals_scen1$soilm_betao # NOTE: use lognormal!
      par_to_estimate$soilm_betao     <- param_lognormals_scen1$soilm_betao # NOTE: use truncated lognormal!

      par_to_estimate$kphio           <- list(mean    = 0.0479684950570567,   sd    = 9.75104729575593e-05)
      par_to_estimate$kphio_par_a     <- list(mean    = -0.00179211384220008, sd    = 2.98616456930556e-05)
      par_to_estimate$kphio_par_b     <- list(mean    = 18.4293950588911,     sd    = 0.102867468875224)
      par_to_estimate$soilm_thetastar <- list(mean    = 27.0859346061886,     sd    = 0.762249191490997)
      par_to_estimate$soilm_betao     <- list(meanlog = -4.65845041863264,    sdlog = 1.31209247435319) # NOTE: use lognormal!
      if (scenario %in% c(74, 84, 94)){
        par_to_estimate$soilm_betao     <- list(meanlog = -4.65845041863264,    sdlog = 1.31209247435319, endpoint = 1.0) # NOTE: use truncated lognormal!
      } else if (scenario %in% c(114, 124)){
        par_to_estimate$soilm_betao = NULL
      }
    }
    if (scenario %in% c(15, 75, 85, 95, 115, 125)) { # use fixed mean from posterior of scenario 1 for kphio, kphio_par_a, kphio_par_b, soilm_thetastar, soilm_betao
      par_to_estimate$kphio <- NULL
      par_to_estimate$kphio_par_a <- NULL
      par_to_estimate$kphio_par_b <- NULL
      par_to_estimate$soilm_thetastar <- NULL
      par_to_estimate$soilm_betao <- NULL
    }
  } else if (scenario %in% c(104,103)){
    par_to_estimate <- list(
      kphio              = list(mean    = 0.0479684950570567,   sd    = 9.75104729575593e-05),
      kphio_par_a        = list(mean    = -0.00179211384220008, sd    = 2.98616456930556e-05),
      kphio_par_b        = list(mean    = 18.4293950588911,     sd    = 0.102867468875224),
      soilm_thetastar    = list(mean    = 27.0859346061886,     sd    = 0.762249191490997),
      soilm_betao        = list(meanlog = -4.65845041863264,    sdlog = 1.31209247435319, endpoint = 1.0), # NOTE: use truncated lognormal!
      beta_unitcostratio = list(mean = 146.0, sd = 50,  lower = 0.1, upper = 400), # truncated normal
      tau_acclim         = list(mean = 14,    sd = 8,   lower = 0.1, upper = 60),  # truncated normal
      kc_jmax            = list(mean = 0.41,  sd = 0.2, lower = 0.1, upper = 1.5), # truncated normal
      err_gpp            = list(mean = 0.8, sd = 0.5, lower = 0.01, upper = 5), # truncated normal
      err_bigD13C        = list(mean = 0.8, sd = 0.5, lower = 0.01, upper = 5), # truncated normal
      err_vj             = list(mean = 0.8, sd = 0.5, lower = 0.01, upper = 5), # truncated normal
      errbias_bigD13C    = list(mean = 0, sd = 5),
      errbias_vj         = list(mean = 0, sd = 1)
    )
    if(scenario == 103){
      par_to_estimate$kphio           = list(mean = 0.05,    sd = 0.05,  lower = 0.02, upper = 0.15)
      par_to_estimate$kphio_par_a     = list(mean = -0.0025, sd = 0.001, lower = -0.004, upper = -0.001)
      par_to_estimate$kphio_par_b     = list(mean = 20,      sd = 5,    lower = 10, upper = 30)
      par_to_estimate$soilm_thetastar = list(mean = 40,      sd = 20,   lower = 1, upper = 250) # NOTE: scenario 0 was previously: 4.32375, 259.425, 432.375
      par_to_estimate$soilm_betao     = list(meanlog = -2, sdlog = 1, endpoint = 1.0) # NOTE: use truncated lognormal!
    }
  } else {
    stop(sprintf("Unsupported scenario: %d", scenario))
  }


  # Remove parameters that are defined to be estimated from default_par_fixed
  par_to_fix <- default_par_fixed[!(names(default_par_fixed) %in% names(par_to_estimate))]
  if (scenario %in% c(15, 75, 85, 95, 115, 125)) {
    par_to_fix$kphio           <- 0.0479684950570567
    par_to_fix$kphio_par_a     <- -0.00179211384220008
    par_to_fix$kphio_par_b     <- 18.4293950588911
    par_to_fix$soilm_thetastar <- 27.0859346061886
    par_to_fix$soilm_betao     <- exp(-4.65845041863264)
  }
  if (scenario %in% c(115, 125)) {par_to_fix$soilm_betao <- 0}


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

  if (scenario %in% c(1,2,11,12, 71,72, 81,82, 91,92, 111,112, 121,122)){ # only GPP data

    drivobs_train <- drivobs_train_bigD13C_vj_gpp |>
      unnest_wider(targets) |>
      filter(gpp) |>
      nest(targets = c(vj, bigD13C, gpp))
    drivobs_test <- drivobs_test_bigD13C_vj_gpp ## for the test data set keep all

  } else if (scenario %in% c(123,124,125,      # the 120s are reruns from 2025-09-17 that lower the prior bound of beta_unitcostratio to 0.01
                             113,114,115,      # the 110s are reruns from 2025-09-11 that fix soilm_betao to 0.0
                             103,104,93,94,95, # the 90s are reruns from 2025-09-06
                             83,84,85, # the 80s (80,81,82,83,84,85,86,87,88) are reruns from 2025-09-05
                             73,74,75, # the 70s (70,71,72,73,74,75,76,77,78) are reruns from 2025-09-03
                             3,4,13,14,15)) { # GPP and traits data

    drivobs_train<- drivobs_train_bigD13C_vj_gpp
    drivobs_test <- drivobs_test_bigD13C_vj_gpp
  } else if (scenario %in% c(126,127,128,      # the 120s are reruns from 2025-09-17 that lower the prior bound of beta_unitcostratio to 0.01
                             116,117,118,      # the 110s are reruns from 2025-09-11 that fix soilm_betao to 0.0
                             96,97,98, # the 90s are reruns from 2025-09-06
                             86,87,88, # the 80s (80,81,82,83,84,85,86,87,88) are reruns from 2025-09-05
                             76,77,78, # the 70s (70,71,72,73,74,75,76,77,78) are reruns from 2025-09-03
                             16,17,18)) { # only traits data, either both, or vj only, or bigD13C only
    drivobs_train <- drivobs_train_bigD13C_vj_gpp |>
      unnest_wider(targets) |>
      filter(case_when(scenario %in% c(16,76,86,96,116, 126) ~ vj | bigD13C,
                       scenario %in% c(17,77,87,97,117     ) ~ vj,             # NOTE: this lets through three sites with bigD13C information
                       scenario %in% c(18,78,88,98,118     ) ~ bigD13C,        # NOTE: this lets through three sites with vj       information
                       scenario %in% c(                 127) ~ vj & !bigD13C,  # NOTE: this removes the three sites that have vj AND bigD13C
                       scenario %in% c(                 128) ~ bigD13C & !vj,  # NOTE: this removes the three sites that have vj AND bigD13C
                       TRUE ~ TRUE)) |>
      nest(targets = c(vj, bigD13C, gpp))
    drivobs_test <- drivobs_test_bigD13C_vj_gpp ## for the test data set keep all
  } else if (scenario %in% c(0, 70, 80, 90, 110, 120)) {  # only GPP data from FR-Pue

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

  } else if (scenario %in% c(31,32,33,34,35,36,37,38,39,40,41,42,
                             51,52,53,54,55,56,57,58,59,60,61,62 )) { # the 30s and 40s are the multi-site gpp only)){ # the 30s and 40s are the multi-site gpp only, the 130s and 140s are preparation for site-specific whc/soilm_theatstar
    drivobs_train <- drivobs_train_bigD13C_vj_gpp |>
      filter(sitename == case_when(
        scenario %in% c(31,131,51) ~ "BE-Vie",
        scenario %in% c(32,132,52) ~ "CH-Dav",
        scenario %in% c(33,133,53) ~ "CZ-BK1",
        scenario %in% c(34,134,54) ~ "DK-Sor",
        scenario %in% c(35,135,55) ~ "FI-Hyy",
        scenario %in% c(36,136,56) ~ "GF-Guy",
        scenario %in% c(37,137,57) ~ "IT-Lav",
        scenario %in% c(38,138,58) ~ "US-Ha1",
        scenario %in% c(39,139,59) ~ "US-MMS",
        scenario %in% c(40,140,60) ~ "US-PFa",
        scenario %in% c(41,141,61) ~ "US-Var",
        scenario %in% c(42,142,62) ~ "US-Wkg",
        TRUE~"donotuseany"
      ))
    if (nrow(drivobs_train) == 0){stop(sprintf("Unsupported scenario %d", scenario))}

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
