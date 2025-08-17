library(tidyverse)
library(rpmodel)
library(rgeco) # pak::pkg_install("geco-bern/rgeco")
library(dplyr)
library(purrr)
# pak::pkg_install("geco-bern/rsofun@simple_pmodel_v2")
library(rsofun)  # install from branch simple_pmodel_v2
# pak::pkg_install("geco-bern/ingestr")
library(ingestr)
library(BayesianTools)

## Load forcing and targets data ----
bigD13C_vj_gpp_drivers <- read_rds(here::here("data/01_bigD13C-vj-gpp_calibsofun_drivers.rds"))
bigD13C_vj_gpp_obs     <- read_rds(here::here("data/01_bigD13C-vj-gpp_calibsofun_obs.rds"))

## Read test-train split ----
df_test_train_split <- read_csv(here::here("data/01_test_train_split.csv"))

sites_train <- df_test_train_split |> filter(dataset == "train")
sites_test  <- df_test_train_split |> filter(dataset == "test")


        # Test run pmodel --------------------------------------------------------------
        # ## Apply one-step P-model function on each row ----
        # ### Define constant model parameters
        # params_modl <- list(
        #   kphio              = 0.04998,
        #   kphio_par_a        = 0.0,
        #   kphio_par_b        = 1.0,
        #   beta_unitcostratio = 146.0,
        #   rd_to_vcmax        = 0.014,
        #   kc_jmax            = 0.41
        # )
        #
        # ### Apply the trait model function row-wise and bind results
        # trait_modeled <- bigD13C_vj_gpp_drivers |>
        #   filter(run_model == "onestep") |>
        #   ungroup() |>
        #   unnest(c(forcing)) |>
        #
        #   group_by(sitename) |> #, lon, lat) |>
        #   group_modify(~run_pmodel_onestep_f_bysite(
        #     lc4 = FALSE,
        #     # select what forcing columns to use:
        #     forcing =  data.frame(temp = .x$temp,
        #                           vpd  = .x$vpd,
        #                           ppfd = .x$ppfd,
        #                           co2  = .x$co2,
        #                           patm = .x$patm),
        #     params_modl = params_modl, # list(kphio = 0.04998, kphio_par_a = 0, kphio_par_b = 1, beta_unitcostratio = 146, rd_to_vcmax = 0.014, kc_jmax = 0.41)
        #     makecheck = FALSE)) |>
        #   rename(vcmax_mod_molm2s   = vcmax,
        #          jmax_mod_molm2s    = jmax,
        #          vcmax25_mod_molm2s = vcmax25,
        #          jmax25_mod_molm2s  = jmax25,
        #          gs_accl_mod_molCmolPhPa = gs_accl, # mol C (mol photons)\eqn{^{-1}} Pa\eqn{^{-1}
        #          wscal_mod__        = wscal,
        #          bigD13C_mod_permil          = bigD13C,
        #          iwue_mod__         = iwue,
        #          rd_mod_gCm2s       = rd) |>
        #   mutate(vj_mod__ = vcmax_mod_molm2s/jmax_mod_molm2s)
        #
        #
        # ## Combine modelled and observed ----
        # trait_targets <- bigD13C_vj_gpp_obs |> filter(run_model=="onestep") |> unnest(data)
        # # trait_targets |> unnest(bigD13C)
        # # trait_targets |> unnest(vj)
        # # # trait_targets |> unnest(gpp)
        #
        # traits_mod_obs <- dplyr::inner_join(
        #   trait_modeled |> nest(model = -c(sitename)),
        #   trait_targets,
        #   by = join_by(sitename))
        #
        # traits_mod_obs_vj <- traits_mod_obs |>
        #   # get obs
        #   unnest(vj) |>
        #   select(sitename, model, genus, species, vcmax_obs_molm2s, jmax_obs_molm2s, vj_obs__) |>
        #   # get model
        #   unnest(model) |>
        #   select(sitename, genus, species,
        #          vcmax_obs_molm2s, jmax_obs_molm2s, vj_obs__,
        #          vcmax_mod_molm2s, jmax_mod_molm2s, vj_mod__)
        #
        # traits_mod_obs_bigD13C <- traits_mod_obs |>
        #   # get obs
        #   unnest(bigD13C) |>
        #   select(sitename, model, species, bigD13C_obs_permil) |>
        #   # get model
        #   unnest(model) |>
        #   select(sitename, species,
        #          bigD13C_obs_permil,
        #          bigD13C_mod_permil)
        #
        # # traits_mod_obs_gpp <- traits_mod_obs |> unnest(gpp)
        #
        #
        # ## Plot modelled vs observed ----
        # ### bigD13C
        # ggplot(traits_mod_obs_bigD13C, aes(bigD13C_mod_permil, bigD13C_obs_permil)) +
        #   geom_point() +
        #   geom_abline(slope = 1, intercept = 0, linetype = "dotted") #+
        # # labs(
        # #   x = "Modelled bigD13C (-)",
        # #   y = "Observed bigD13C (-)"
        # # )
        #
        # ### Vcmax, Jmax, V/J-ratio
        # ggplot(traits_mod_obs_vj, aes(vcmax_mod_molm2s, vcmax_obs_molm2s)) +
        #   geom_point() +
        #   geom_abline(slope = 1, intercept = 0, linetype = "dotted") #+
        # ggplot(traits_mod_obs_vj, aes(jmax_mod_molm2s, jmax_obs_molm2s)) +
        #   geom_point() +
        #   geom_abline(slope = 1, intercept = 0, linetype = "dotted") #+
        # ggplot(traits_mod_obs_vj, aes(vj_mod__, vj_obs__)) +
        #   geom_point() +
        #   geom_abline(slope = 1, intercept = 0, linetype = "dotted") #+



## Preprocess observation data (gpp)

# some observations of gpp are negative TODO: filter them out
## for training sites
bigD13C_vj_gpp_obs |> filter(sitename %in% sites_train$sitename) |>
  filter(run_model == "daily") |> unnest(data) |>
  ggplot(aes(x=gpp, color = sitename)) + geom_density()# + facet_wrap(~sitename)
## for testing sites
bigD13C_vj_gpp_obs |> filter(sitename %in% sites_test$sitename) |>
  filter(run_model == "daily") |> unnest(data) |>
  ggplot(aes(x=gpp, color = sitename)) + geom_density()# + facet_wrap(~sitename)

# some observations of gpp are NA, filter them out:
# some observations of gpp are negative, filter those below -2 out
# TODO: document this
bigD13C_vj_gpp_obs <- bind_rows(
  # filter out NAs in gpp observations
  bigD13C_vj_gpp_obs |> filter(run_model == "daily") |>
    unnest(data) |>
    filter(!is.na(gpp)) |>
    filter(gpp > -2) |>
    nest(data = -c(sitename, run_model, targets)),
  # do not filter out anything from the other observations
  bigD13C_vj_gpp_obs |> filter(run_model != "daily")
)

# some model input leads to NA in modeled gpp, filter them out:
# bigD13C_vj_gpp_drivers <- bigD13C_vj_gpp_drivers |> filter(sitename != "US-Bar")
bigD13C_vj_gpp_drivers2 <- bind_rows(
  # correct missing ccov
  bigD13C_vj_gpp_drivers |> filter(sitename == "US-Bar") |>
    unnest(forcing) |>
    mutate(ccov = if_else(is.na(ccov), 0, ccov)) |>
    nest(forcing = -c(sitename, run_model, params_siml, site_info)),
  # keep other unchanged
  bigD13C_vj_gpp_drivers |> filter(sitename != "US-Bar")
)
# NOTE: this appeared to be related only to site "US-Bar"
# TODO: document this
# TODO: find out why. Are there some wrong units in input data? NO, it appears all to be caused by ccov=NA
bigD13C_vj_gpp_drivers |>
  group_by(sitename) |>
  filter(sitename %in% c("US-Bar", "US-Ton")) |>
  unnest(forcing) |> slice(1:10)
# bigD13C_vj_gpp_drivers |> filter(sitename == "US-Bar") |> unnest(forcing) |> filter(is.na(ccov)) # this is only 355 rows
# bigD13C_vj_gpp_drivers |> filter(sitename == "US-Bar") |> unnest(forcing) |> filter(!is.na(ccov))



## Apply test-train split to data ----
train_drivers <- bigD13C_vj_gpp_drivers2 |> filter(sitename %in% sites_train$sitename)
train_obs     <- bigD13C_vj_gpp_obs     |> filter(sitename %in% sites_train$sitename)

test_drivers <- bigD13C_vj_gpp_drivers2 |> filter(sitename %in% sites_test$sitename)
test_obs     <- bigD13C_vj_gpp_obs     |> filter(sitename %in% sites_test$sitename)





## Compute loglikelihood ----
source(here::here("R/cost_likelihood_pmodel_bigD13C_vj_gpp.R"))
## Calibrate parameters ----

# Define calibration settings for three targets
settings_joint_likelihood_bigD13C_vj_gpp <- list(
  method = "BayesianTools",
  metric = cost_likelihood_pmodel_bigD13C_vj_gpp,
  control = list(
    sampler = "DEzs",
    settings = list(
      burnin = 1,    # kept artificially low
      iterations = 5 # kept artificially low
    )),
  par = list(kc_jmax = list(lower = 0.2,  upper = 0.6, init = 0.41),  # uniform priors
             # TODO: add further parameters
             # TODO: error parameters must come last
             err_gpp = list(lower = 0.001, upper = 0.6, init = 0.3),
             err_bigD13C = list(lower = 0.001, upper = 0.6, init = 0.3),
             err_vj  = list(lower = 0.001, upper = 0.6, init = 0.3))
)

# Run the calibration on all data:
par_calib_join_bigD13C_vj_gpp <- calib_sofun(
  drivers  = train_drivers,
  obs      = train_obs,
  settings = settings_joint_likelihood_bigD13C_vj_gpp,
  # arguments for the cost function
  par_fixed = list(         # fix parameter value from previous calibration
    kphio              = 0.04998,
    kphio_par_a        = 0.0,
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover paper setup with soil moisture stress
    soilm_betao        = 0.0,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 30.0
    # kc_jmax            = 0.41
  )
)
#plot(par_calib_join_bigD13C_vj_gpp$mod)





# subset different combination of target variables
# for easier handling do this in combined drivobs-object
drivobs_bigD13C_vj_gpp <- dplyr::inner_join(
  # TODO: check if combined drivobs is computationally more efficient for calib_sofun()
  train_drivers,
  train_obs,
  by = join_by(sitename, run_model))

# case 1: no gpp provided, only bigD13C,vj
drivobs_bigD13CZZZZZ13ZZZZZc_vj <- drivobs_bigD13C_vj_gpp |>
  unnest_wider(targets) |>
  filter(bigD13C | vj) |>
  nest(targets = c(vj, bigD13C, gpp))

# case 2: only bigD13C provided
drivobs_bigD13C <- drivobs_bigD13C_vj_gpp |>
  unnest_wider(targets) |>
  filter(bigD13C) |>
  nest(targets = c(vj, bigD13C, gpp))

# case 3: only vj provided
drivobs_vj <- drivobs_bigD13C_vj_gpp |>
  unnest_wider(targets) |>
  filter(vj) |>
  nest(targets = c(vj, bigD13C, gpp))

# case 4: only gpp provided
drivobs_gpp <- drivobs_bigD13C_vj_gpp |>
  unnest_wider(targets) |>
  filter(gpp) |>
  nest(targets = c(vj, bigD13C, gpp))


# case 1:
par_calib_join_bigD13C_vj_gpp2 <- calib_sofun(
  drivers  = select(drivobs_bigD13C_vj_gpp, sitename, run_model, params_siml, site_info, forcing),
  obs      = select(drivobs_bigD13C_vj_gpp, sitename, run_model, targets, data),
  settings = settings_joint_likelihood_bigD13C_vj_gpp,
  # arguments for the cost function
  par_fixed = list(         # fix parameter value from previous calibration
    kphio              = 0.04998,
    kphio_par_a        = 0.0,
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover paper setup with soil moisture stress
    soilm_betao        = 0.0,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 30.0
    # kc_jmax            = 0.41
  )
)
# plot(par_calib_join_bigD13C_vj_gpp2$mod)
par_calib_join_bigD13C_vj_gpp2$mod |> summary() # DEzs(1,5) takes ~ 41.0s

# case 1:
par_calib_join_bigD13C_vj <- calib_sofun(
  drivers  = select(drivobs_bigD13C_vj, sitename, run_model, params_siml, site_info, forcing),
  obs      = select(drivobs_bigD13C_vj, sitename, run_model, targets, data),
  settings = settings_joint_likelihood_bigD13C_vj_gpp,
  # arguments for the cost function
  par_fixed = list(         # fix parameter value from previous calibration
    kphio              = 0.04998,
    kphio_par_a        = 0.0,
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover paper setup with soil moisture stress
    soilm_betao        = 0.0,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 30.0
    # kc_jmax            = 0.41
  )
)
# plot(par_calib_join_bigD13C_vj$mod)
par_calib_join_bigD13C_vj$mod |> summary() # DEzs(1,5) takes ~ 19.8s

# case 2:
par_calib_join_bigD13C <- calib_sofun(
  drivers  = select(drivobs_bigD13C, sitename, run_model, params_siml, site_info, forcing),
  obs      = select(drivobs_bigD13C, sitename, run_model, targets, data),
  settings = settings_joint_likelihood_bigD13C_vj_gpp,
  # arguments for the cost function
  par_fixed = list(         # fix parameter value from previous calibration
    kphio              = 0.04998,
    kphio_par_a        = 0.0,
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover paper setup with soil moisture stress
    soilm_betao        = 0.0,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 30.0
    # kc_jmax            = 0.41
  )
)
# plot(par_calib_join_bigD13C$mod)
par_calib_join_bigD13C$mod |> summary() # DEzs(1,5) takes ~ 3.5s

# case 3:
par_calib_join_vj <- calib_sofun(
  drivers  = select(drivobs_vj, sitename, run_model, params_siml, site_info, forcing),
  obs      = select(drivobs_vj, sitename, run_model, targets, data),
  settings = settings_joint_likelihood_bigD13C_vj_gpp,
  # arguments for the cost function
  par_fixed = list(         # fix parameter value from previous calibration
    kphio              = 0.04998,
    kphio_par_a        = 0.0,
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover paper setup with soil moisture stress
    soilm_betao        = 0.0,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 30.0
    # kc_jmax            = 0.41
  )
)
# plot(par_calib_join_vj$mod)
par_calib_join_vj$mod |> summary() # DEzs(1,5) takes ~ 19.7s

# case 4:
par_calib_join_gpp <- calib_sofun(
  drivers  = select(drivobs_gpp, sitename, run_model, params_siml, site_info, forcing),
  obs      = select(drivobs_gpp, sitename, run_model, targets, data),
  settings = settings_joint_likelihood_bigD13C_vj_gpp,
  # arguments for the cost function
  par_fixed = list(         # fix parameter value from previous calibration
    kphio              = 0.04998,
    kphio_par_a        = 0.0,
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover paper setup with soil moisture stress
    soilm_betao        = 0.0,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 30.0
    # kc_jmax            = 0.41
  )
)
# plot(par_calib_join_gpp$mod)
par_calib_join_gpp$mod |> summary() # DEzs(1,5) takes ~ 6.9s






























# Setup actual calibration:
set.seed(1982)
source(here::here("R/calibration_helpers.R"))

# FROM THE REVISION PLAN:
# Setup 1: global, reduced parameter set (as in initial manuscript version), only GPP as target
# Setup 2: global, full parameter set, only GPP as target
# Setup 3: global, full parameter set, GPP and traits as target
# We expect Setup 2 to yield wider posteriors than from Setup 1, and that posterior distributions will be narrowed again by Setup 3. This experimental design will allow us to demonstrate the robustness (or absence thereof) of the MCMC and the usefulness of using traits for simultaneously calibrating with fluxes.


## Load loglikelihood ----
source(here::here("R/cost_likelihood_pmodel_bigD13C_vj_gpp.R"))

## Setup the settings for the three calibration setups ----
par_setup1 <- list(
  kphio           = list(lower = 0.02, upper = 0.15, init = 0.05),
  kphio_par_a     = list(lower = -0.004, upper = -0.001, init = -0.0025),
  kphio_par_b     = list(lower = 10, upper = 30, init = 20),
  soilm_thetastar = list(lower = 1, upper = 250, init = 40),
  soilm_betao     = list(lower = 0.0, upper = 1.0, init = 0.0),
  err_gpp         = list(lower = 0.1, upper = 3, init = 0.8),
  err_bigD13C         = list(lower = 0.1, upper = 3, init = 0.8), # TODO: without err_bigD13C and err_vj this errors
  err_vj          = list(lower = 0.1, upper = 3, init = 0.8)  # TODO: without err_bigD13C and err_vj this errors
)

#TODO: define ranges for new parameters
par_setup23 <- list(
  kphio           = list(lower = 0.02, upper = 0.15, init = 0.05),
  kphio_par_a     = list(lower = -0.004, upper = -0.001, init = -0.0025),
  kphio_par_b     = list(lower = 10, upper = 30, init = 20),
  soilm_thetastar = list(lower = 1, upper = 250, init = 40),
  soilm_betao     = list(lower = 0.0, upper = 1.0, init = 0.0),
  beta_unitcostratio = as.list(c(lower = 0.1, upper = 3.0, init = 1.0)*146.0),
  rd_to_vcmax        = as.list(c(lower = 0.1, upper = 3.0, init = 1.0)*0.014),      # 0.014 value from Atkin et al. 2015 for C3 herbaceous
  tau_acclim         = as.list(c(lower = 0.1, upper = 3.0, init = 1.0)*20.0),
  kc_jmax            = as.list(c(lower = 0.1, upper = 3.0, init = 1.0)*0.41),
  err_gpp         = list(lower = 0.1, upper = 3, init = 0.8),
  err_bigD13C         = list(lower = 0.1, upper = 3, init = 0.8), # TODO: without err_bigD13C and err_vj this errors
  err_vj          = list(lower = 0.1, upper = 3, init = 0.8)  # TODO: without err_bigD13C and err_vj this errors
)
        # From 02_sensitivity_analysis.R:
        # # best parameter values (initial values)
        # par_cal_best <- c(
        #   kphio              = list(lower = 0.03,   upper = 0.15,  init = 0.09423773 ),
        #   kphio_par_a        = list(lower = -0.004, upper = 0.001, init = 0.0025     ),
        #   kphio_par_b        = list(lower = 10,     upper = 30,    init = 20         ),
        #   soilm_thetastar    = list(lower = 0,      upper = 240,   init = 0.6*240    ),
        #   soilm_betao        = list(lower = 0,      upper = 1,     init = 0.2        ),
        #   beta_unitcostratio = list(lower = 50.0,   upper = 200.0, init = 146.0      ),
        #   rd_to_vcmax        = list(lower = 0.01,   upper = 0.1,   init = 0.014      ),
        #   tau_acclim         = list(lower = 7.0,    upper = 60.0,  init = 30.0       ),
        #   kc_jmax            = list(lower = 0.2,    upper = 0.8,   init = 0.41       ),
        #   error_gpp          = list(lower = 0.01    upper = 4,     init = 1          ),
        # )


create_settings_and_par_fixed <- function(par, burnin=1, iterations=5){
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

  list(
    settings = list(
      method = "BayesianTools",
      metric = cost_likelihood_pmodel_bigD13C_vj_gpp,
      control = list(
        sampler = "DEzs",
        settings = list(
          burnin = burnin,     #10000,
          iterations = iterations, #50000,
          nrChains = 3,       # number of independent chains
          startValue = 3      # number of internal chains to be sampled
        )),
      par = par
    ),
    # only keep par_fixed that are not set as par
    par_fixed = default_par_fixed[
      !(names(default_par_fixed) %in% names(par))
      ]
  )
}
#create_settings_and_par_fixed(par_setup1)$settings
#create_settings_and_par_fixed(par_setup1)$par_fixed
#create_settings_and_par_fixed(par_setup23)$settings
#create_settings_and_par_fixed(par_setup23)$par_fixed

## Setup the data (drivers and obs) for the three calibration setups ----

# subset different combination of target variables
# for easier handling do this in combined drivobs-object
drivobs_bigD13C_vj_gpp <- dplyr::inner_join(
  train_drivers,
  train_obs,
  by = join_by(sitename, run_model))

# TODO: check if passing combined drivobs is computationally more efficient for calib_sofun()
drivobs_setup12 <- drivobs_bigD13C_vj_gpp |>
  unnest_wider(targets) |>
  filter(gpp) |>
  nest(targets = c(vj, bigD13C, gpp))

drivobs_setup3 <- drivobs_bigD13C_vj_gpp


## Calibrate parameters ----

in_calib_setup1  <- create_settings_and_par_fixed(par_setup1, burnin=10000, iterations=50000)
in_calib_setup23 <- create_settings_and_par_fixed(par_setup23, burnin=10000/3, iterations=50000/3) # TODO: remove /3

# Run setup1:
out_calib_setup1 <- calib_sofun(
  drivers   = select(drivobs_setup12, sitename, run_model, params_siml, site_info, forcing),
  obs       = select(drivobs_setup12, sitename, run_model, targets, data),
  settings  = in_calib_setup1$settings,
  # arguments for the cost function
  par_fixed = in_calib_setup1$par_fixed
)
plot(out_calib_setup1$mod)
summary(out_calib_setup1$mod)
# setup1: DEzs(1,5) takes ~ 20.9 seconds
# setup1: DEzs(10000,50000) takes ~ 150'000 seconds = 42h


# Store intermediate results
out_calib_setup1$name <- "s1"
settings_string <- get_calibration_settings_str(out_calib_setup1)
write_rds(out_calib_setup1,
        file = here::here(paste0("data/out_calib_", settings_string, ".rds")),
        compress = "xz")


# Run setup2:
out_calib_setup2 <- calib_sofun(
  drivers   = select(drivobs_setup12, sitename, run_model, params_siml, site_info, forcing),
  obs       = select(drivobs_setup12, sitename, run_model, targets, data),
  settings  = in_calib_setup23$settings,
  # arguments for the cost function
  par_fixed = in_calib_setup23$par_fixed
)
#plot(out_calib_setup2$mod)
summary(out_calib_setup2$mod)
# setup2: DEzs(1,5) takes ~ 22.7 seconds
# setup2: DEzs(10000/3, 50000/3) takes ~ 43000 seconds = 12 hours

# Store intermediate results
out_calib_setup2$name <- "s2"
settings_string <- get_calibration_settings_str(out_calib_setup2)
write_rds(out_calib_setup2,
        file = here::here(paste0("data/out_calib_", settings_string, ".rds")),
        compress = "xz")


# Run setup3:
in_calib_setup23 <- create_settings_and_par_fixed(par_setup23, burnin=1000, iterations=5000) # TODO: remove /3
out_calib_setup3 <- calib_sofun(
  drivers   = select(drivobs_setup3, sitename, run_model, params_siml, site_info, forcing),
  obs       = select(drivobs_setup3, sitename, run_model, targets, data),
  settings  = in_calib_setup23$settings,
  # arguments for the cost function
  par_fixed = in_calib_setup23$par_fixed
)
#plot(out_calib_setup3$mod)
summary(out_calib_setup3$mod)
# setup3: DEzs(1,5) takes ~ 100 seconds (makecheck=TRUE) and 100 seconds (makecheck=FALSE)
# setup3: DEzs(10000/10, 50000/10) takes ~ 80'000 seconds = 22 hours

# Store intermediate results
out_calib_setup3$name <- "s3"
settings_string <- get_calibration_settings_str(out_calib_setup3)
write_rds(out_calib_setup3,
        file = here::here(paste0("data/out_calib_", settings_string, ".rds")),
        compress = "xz")






# Make some comparison plots:

pl1 <- plot_prior_posterior_density(out_calib_setup1$mod) + ggtitle("Setup 1 10k/50k")
pl2 <- plot_prior_posterior_density(out_calib_setup2$mod) + ggtitle("Setup 2 3k/13k")
pl3 <- plot_prior_posterior_density(out_calib_setup3$mod) + ggtitle("Setup 3 1k/5k")

# pl1
# pl2
# pl3

library(patchwork)
(pl1 + theme(legend.position = "none"))/
  (pl2 + theme(legend.position = "none"))/
  pl3
