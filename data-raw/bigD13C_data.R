# This script prepares the input forcing tibble ('df_bigD13C_forcing') and
# output target tibble ('df_bigD13C_target') needed for simulating bigD13C = Ci/Ca
# with P-model.
#
# The input forcing is first filled with NA.
# In a second step the input forcing needs to be replaced with data from ingestr.
#
# These two objects are stored as *.rds files in subfolder data/

library(tidyverse)
library(rgeco)   # pak::pkg_install("https://github.com/geco-bern/rgeco")
library(leaf13C) # pak::pkg_install("traitecoevo/datastorr")
                 # pak::pkg_install("traitecoevo/leaf13C")

# Please cite
# Cornwell, William K., et al. “Climate and soils together regulate photosynthetic carbon isotope discrimination within C3 plants worldwide.” Global Ecology and Biogeography 27.9 (2018): 1056-1067.

# see https://github.com/traitecoevo/leaf13C/blob/master/leaf13C_metadata.txt for column description
df_bigD13C_allobs <- leaf13C::get_data(version = "0.2.2") |>
  tibble() |>
  rename(bigD13C = big.D13.merged) |>
  mutate(sitename = sprintf("lon_%+07.2f_lat_%+07.2f", longitude, latitude))

df_bigD13C <- df_bigD13C_allobs |>
  # dropping observations that are missing either of the targets
  drop_na(bigD13C)


# aggregate model inputs and model targets
#   inputs:  by site only         (reducing number of simulations)
#   targets: by site and species  (allowing for species differences in error term)
#
#   For the GMD paper, not aggregating across species requires to specify
#   likelihood as a function of mismatch wrt all species individually for a given site.
df_bigD13C_forcing <- df_bigD13C |>
  group_by(sitename) |>
  summarise(.groups = "keep",
            lon = mean(longitude),
            lat = mean(latitude),
            year         = NA_real_,
            temp_degC    = NA_real_, #mean(temperature_gs), # deg C,     growing season value (growing season, where monthly mean T>0)
            vpd_Pa       = NA_real_, #mean(vpd_gs),         # Pa,        growing season value (growing season, where monthly mean T>0)
            par_molm2s   = NA_real_, #mean(par_gs),         # mol/m2/s, growing season value (growing season, where monthly mean T>0)
            elv_masl     = NA_real_, #mean(z),              # m asl
            co2_ppm      = NA_real_, #mean(ca)              # ppm
            Nobs   = NA_integer_,
            Nyears = NA_integer_,
            Ndates = NA_integer_
            )  |>
  mutate(
    patm_Pa = rpmodel::calc_patm(elv_masl)
  )
df_bigD13C_target <- df_bigD13C |>
  mutate(collection.date = lubridate::make_date(collection.year, collection.month)) |>
  group_by(sitename, species) |>
  summarise(
    .groups = "keep",
    lon = mean(longitude),
    lat = mean(latitude),
    year   = mean(collection.year),
    bigD13C_obs__ = mean(bigD13C), # unitless
    Nobs = n(),
    Nyears = length(unique(collection.year)),
    Ndates = length(unique(collection.date)),
  )

df_bigD13C_target |>
  ggplot(aes(x = bigD13C_obs__)) +
  geom_histogram(bins = 15)

rgeco:::plot_map_simpl() +
  geom_point(data = df_bigD13C_forcing, aes(lon, lat))
# df_bigD13C_target |> filter(Nobs>1)
# df_bigD13C_target |> filter(Nyears>1) |> print(n=100)
# df_bigD13C_target |> filter(Ndates>1) |> print(n=100)

write_rds(df_bigD13C_forcing, here::here("data/00_bigD13C_forcing.rds"))
write_rds(df_bigD13C_target, here::here("data/00_bigD13C_target.rds"))
rm(df_bigD13C)
rm(df_bigD13C_allobs)








# TODO: below is just a test.




#### EXAMPLE WITH DEMO DATA
# library(rsofun)
# # read in demo data
# df_drivers <- p_model_drivers
# # load parameters (valid ones)
# params_modl <- list(
#   kphio              = 0.04998, # setup ORG in Stocker et al. 2020 GMD
#   kphio_par_a        = 0.01,  # set to zero to disable temperature-dependence of kphio, setup ORG in Stocker et al. 2020 GMD
#   kphio_par_b        = 1.0,
#   soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
#   soilm_betao        = 0.01,
#   beta_unitcostratio = 146.0,
#   rd_to_vcmax        = 0.014, # value from Atkin et al. 2015 for C3 herbaceous
#   tau_acclim         = 30.0,
#   kc_jmax            = 0.41
# )
# # run the SOFUN Fortran P-model
# mod <- run_pmodel_f_bysite(
#   sitename    = "empty",
#   params_siml = df_drivers$params_siml[[1]],
#   site_info   = df_drivers$site_info[[1]],
#   forcing     = df_drivers$forcing[[1]],
#   params_modl = params_modl,
#   makecheck = FALSE
# )
# plot(mod$bigD13C)
#### END EXAMPLE

#### DEVELOPMENT
df_bigD13C_forcing_dummy <- df_bigD13C_forcing |>
  mutate(
    temp_degC   = 17.7,    # deg C,     growing season value (growing season, where T>0)
    vpd_Pa      = 0.736,   # Pa,        growing season value (growing season, where T>0)
    par_molm2s  = 849/10^6,# mol/m2/s, growing season value (growing season, where T>0)
    co2_ppm     = 360,     # ppm
    elv_masl    = 68,      # m asl
    patm_Pa     = rpmodel::calc_patm(elv_masl)
  )
df_bigD13C_forcing_dummy

# Apply one-step P-model function on each row of df_sites
library(dplyr)
library(purrr)
library(rsofun)  # install from branch simple_pmodel

# Define constant model parameters
params_modl <- list(
  kphio              = 0.04998,
  kphio_par_a        = 0.0,
  kphio_par_b        = 1.0,
  beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014,
  kc_jmax            = 0.41
)


# Apply the model function row-wise and bind results
df_bigD13C_modeled <- df_bigD13C_forcing_dummy |>
  group_by(sitename) |> #, lon, lat) |>
  group_modify(~run_pmodel_onestep_f_bysite(
    lc4 = FALSE,
    forcing =  data.frame(temp = .x$temp_degC, # TODO: with ingestr this could be daytime average
                          vpd  = .x$vpd_Pa,    # TODO: with ingestr this could be daytime average
                          ppfd = .x$par_molm2s,
                          co2  = .x$co2_ppm,
                          patm = .x$patm_Pa),
    params_modl = params_modl,
    makecheck = FALSE)) |>
    rename(vcmax_mod_molm2s   = vcmax,
           jmax_mod_molm2s    = jmax,
           vcmax25_mod_molm2s = vcmax25,
           jmax25_mod_molm2s  = jmax25,
           gs_accl_mod_molCmolPhPa = gs_accl, # mol C (mol photons)\eqn{^{-1}} Pa\eqn{^{-1}
           bigD13C_mod__      = bigdelta,
           iwue_mod__         = iwue,
           rd_mod_gCm2s       = rd)
  # transform to same units as targets:
  # # unneeded for bigD13C


# Plot modelled vs observed

# Combine modelled and observed
df_bigD13C_with_outputs <- dplyr::inner_join(df_bigD13C_modeled, df_bigD13C_target, by = join_by(sitename))

# Vcmax
df_bigD13C_with_outputs |>
  ggplot(aes(bigD13C_mod__, bigD13C_obs__)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") #+
  # labs(
  #   x = "Modelled bigD13C (-)",
  #   y = "Observed bigD13C (-)"
  # )

#### END DEVELOPMENT

