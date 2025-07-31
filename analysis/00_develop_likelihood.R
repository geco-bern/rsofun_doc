rm(list = ls())
library(tidyverse)
library(rpmodel)
library(rgeco) # pak::pkg_install("geco-bern/rgeco")
library(dplyr)
library(purrr)
# pak::pkg_install("geco-bern/rsofun@simple_pmodel_v2")
library(rsofun)  # install from branch simple_pmodel_v2
# pak::pkg_install("geco-bern/ingestr")
library(ingestr)

## Load forcing and targets data ----
chi_vj_gpp_drivers <- readRDS(here::here("data/chi-vj-gpp_calibsofun_drivers.rds"))
chi_vj_gpp_obs     <- readRDS(here::here("data/chi-vj-gpp_calibsofun_obs.rds"))


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
        # trait_modeled <- chi_vj_gpp_drivers |>
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
        #          chi_mod__          = chi,
        #          iwue_mod__         = iwue,
        #          rd_mod_gCm2s       = rd) |>
        #   mutate(vj_mod__ = vcmax_mod_molm2s/jmax_mod_molm2s)
        #
        #
        # ## Combine modelled and observed ----
        # trait_targets <- chi_vj_gpp_obs |> filter(run_model=="onestep") |> unnest(data)
        # # trait_targets |> unnest(chi)
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
        # traits_mod_obs_chi <- traits_mod_obs |>
        #   # get obs
        #   unnest(chi) |>
        #   select(sitename, model, species, chi_obs__) |>
        #   # get model
        #   unnest(model) |>
        #   select(sitename, species,
        #          chi_obs__,
        #          chi_mod__)
        #
        # # traits_mod_obs_gpp <- traits_mod_obs |> unnest(gpp)
        #
        #
        # ## Plot modelled vs observed ----
        # ### Chi
        # ggplot(traits_mod_obs_chi, aes(chi_mod__, chi_obs__)) +
        #   geom_point() +
        #   geom_abline(slope = 1, intercept = 0, linetype = "dotted") #+
        # # labs(
        # #   x = "Modelled Chi (-)",
        # #   y = "Observed Chi (-)"
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

# some observations of gpp are NA, filter them out:
# TODO: document this
chi_vj_gpp_obs <- bind_rows(
  # filter out NAs in gpp observations
  chi_vj_gpp_obs |> filter(run_model == "daily") |>
    unnest(data) |>
    filter(!is.na(gpp)) |>
    nest(data = -c(sitename, run_model, targets)),
  # do not filter out anything from the other observations
  chi_vj_gpp_obs |> filter(run_model != "daily")
)

# some model input leads to NA in modeled gpp, filter them out:
# chi_vj_gpp_drivers <- chi_vj_gpp_drivers |> filter(sitename != "US-Bar")
chi_vj_gpp_drivers2 <- bind_rows(
  # correct missing ccov
  chi_vj_gpp_drivers |> filter(sitename == "US-Bar") |>
    unnest(forcing) |>
    mutate(ccov = if_else(is.na(ccov), 0, ccov)) |>
    nest(forcing = -c(sitename, run_model, params_siml, site_info)),
  # keep other unchanged
  chi_vj_gpp_drivers |> filter(sitename != "US-Bar")
)
# NOTE: this appeared to be related only to site "US-Bar"
# TODO: document this
# TODO: find out why. Are there some wrong units in input data? NO, it appears all to be caused by ccov=NA
chi_vj_gpp_drivers |>
  group_by(sitename) |>
  filter(sitename %in% c("US-Bar", "US-Ton")) |>
  unnest(forcing) |> slice(1:10)
# chi_vj_gpp_drivers |> filter(sitename == "US-Bar") |> unnest(forcing) |> filter(is.na(ccov)) # this is only 355 rows
# chi_vj_gpp_drivers |> filter(sitename == "US-Bar") |> unnest(forcing) |> filter(!is.na(ccov))






## Compute loglikelihood ----
source(here::here("R/cost_likelihood_pmodel_chi_vj_gpp.R"))

## Calibrate parameters ----

# Define calibration settings for three targets
settings_joint_likelihood_chi_vj_gpp <- list(
  method = "BayesianTools",
  metric = cost_likelihood_pmodel_chi_vj_gpp,
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
             err_chi = list(lower = 0.001, upper = 0.6, init = 0.3),
             err_vj  = list(lower = 0.001, upper = 0.6, init = 0.3))
)

# Run the calibration on all data:
par_calib_join_chi_vj_gpp <- calib_sofun(
  drivers  = chi_vj_gpp_drivers2,
  obs      = chi_vj_gpp_obs,
  settings = settings_joint_likelihood_chi_vj_gpp,
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
#plot(par_calib_join_chi_vj_gpp$mod)





# subset different combination of target variables
# for easier handling do this in combined drivobs-object
drivobs_chi_vj_gpp <- dplyr::inner_join(
  # TODO: check if combined drivobs is computationally more efficient for calib_sofun()
  chi_vj_gpp_drivers2,
  chi_vj_gpp_obs,
  by = join_by(sitename, run_model))

# case 1: no gpp provided, only chi,vj
drivobs_chi_vj <- drivobs_chi_vj_gpp |>
  unnest_wider(targets) |>
  filter(chi | vj) |>
  nest(targets = c(vj, chi, gpp))

# case 2: only chi provided
drivobs_chi <- drivobs_chi_vj_gpp |>
  unnest_wider(targets) |>
  filter(chi) |>
  nest(targets = c(vj, chi, gpp))

# case 3: only vj provided
drivobs_vj <- drivobs_chi_vj_gpp |>
  unnest_wider(targets) |>
  filter(vj) |>
  nest(targets = c(vj, chi, gpp))

# case 4: only gpp provided
drivobs_gpp <- drivobs_chi_vj_gpp |>
  unnest_wider(targets) |>
  filter(gpp) |>
  nest(targets = c(vj, chi, gpp))


# case 1:
par_calib_join_chi_vj_gpp2 <- calib_sofun(
  drivers  = select(drivobs_chi_vj_gpp, sitename, run_model, params_siml, site_info, forcing),
  obs      = select(drivobs_chi_vj_gpp, sitename, run_model, targets, data),
  settings = settings_joint_likelihood_chi_vj_gpp,
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
# plot(par_calib_join_chi_vj_gpp2$mod)
par_calib_join_chi_vj_gpp2$mod |> summary() # DEzs(1,5) takes ~ 41.0s

# case 1:
par_calib_join_chi_vj <- calib_sofun(
  drivers  = select(drivobs_chi_vj, sitename, run_model, params_siml, site_info, forcing),
  obs      = select(drivobs_chi_vj, sitename, run_model, targets, data),
  settings = settings_joint_likelihood_chi_vj_gpp,
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
# plot(par_calib_join_chi_vj$mod)
par_calib_join_chi_vj$mod |> summary() # DEzs(1,5) takes ~ 19.8s

# case 2:
par_calib_join_chi <- calib_sofun(
  drivers  = select(drivobs_chi, sitename, run_model, params_siml, site_info, forcing),
  obs      = select(drivobs_chi, sitename, run_model, targets, data),
  settings = settings_joint_likelihood_chi_vj_gpp,
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
# plot(par_calib_join_chi$mod)
par_calib_join_chi$mod |> summary() # DEzs(1,5) takes ~ 3.5s

# case 3:
par_calib_join_vj <- calib_sofun(
  drivers  = select(drivobs_vj, sitename, run_model, params_siml, site_info, forcing),
  obs      = select(drivobs_vj, sitename, run_model, targets, data),
  settings = settings_joint_likelihood_chi_vj_gpp,
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
  settings = settings_joint_likelihood_chi_vj_gpp,
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


