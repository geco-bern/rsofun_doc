# This script prepares the input forcing tibble ('df_chi_forcing') and
# output target tibble ('df_chi_target') needed for simulating Chi = Ci/Ca
# with P-model.
#
# The input forcing is first filled with NA.
# In a second step the input forcing needs to be replaced with data from ingestr.
#
# These two objects are stored as *.rds files in subfolder data/

rm(list = ls())
library(tidyverse)
library(rgeco)   # remotes::install_github("https://github.com/geco-bern/rgeco")
library(leaf13C) # remotes::install_github("traitecoevo/datastorr")
                 # remotes::install_github("traitecoevo/leaf13C")

# Please cite
# Cornwell, William K., et al. “Climate and soils together regulate photosynthetic carbon isotope discrimination within C3 plants worldwide.” Global Ecology and Biogeography 27.9 (2018): 1056-1067.

# see https://github.com/traitecoevo/leaf13C/blob/master/leaf13C_metadata.txt for column description
df_chi_allobs <- leaf13C::get_data(version = "0.2.2")

# Calculate chi (ci:ca) from big-Delta data.
calc_chi_bigdelta <- function(bigdelta, ca, mgdd0 = NA){
  # source Lavergne 2020, New Phytol, https://doi.org/10.1111/nph.16314:

  # bigdelta # (permil) observed discrimination in plant material relative to atmospheric CO2
  # ca       # (Pa) ambient (ca) partial CO2 pressure
  # mgdd0    # ???
  # chi      # (-) ratio of leaf internal (ci) to ambient (ca) partial CO2 pressure

  # Parameters
  a <- 4.4  # isotope fractionation from CO2 diffusion in air (4.4 permil; Craig, 1953)
  b <- 27.0 # isotope fractionation from effective Rubisco carboxylation (26–30 permil)

  if (!is.na(mgdd0)){ # internal note: Method Wang Hang with mgdd0 being the temperature depencey of gammastar, getting beta=146.0 (unitcost ratio)

    # account for co2 compensation point and its temperature dependency
    f <- 8.0    # isotope fractionation from photorespiration (8–16 permil; Ubierna & Farquhar, 2014)
    k <- 0.0512 # ???
    gammastar25 <- 42.75                              # (Pa) photorespiratory compensation point at 25°C
    gammastar <- gammastar25*exp(k*(mgdd0 - 25.0))    # (Pa) the CO2 compensation point
    chi <- (bigdelta - a + f*gammastar/ca)/(b - a)    # Eqn 2, Lavergne 2020, New Phytol

  } else {

    chi <- (bigdelta - a)/(b - a)

  }

  return(chi)
}

df_chi_allobs <- df_chi_allobs |>
  rowwise() |>
  mutate(chi = calc_chi_bigdelta(big.D13.merged, ca = 400)) |>
  ungroup() |>
  mutate(site = sprintf("lon_%+07.2f_lat_%+07.2f", longitude, latitude))

# drop NAs
df_chi <- df_chi_allobs |>
  # dropping observations that are missing either of the targets
  drop_na(chi)

# aggregate model inputs and model targets
#   inputs:  by site only         (reducing number of simulations)
#   targets: by site and species  (allowing for species differences in error term)
#
#   For the GMD paper, not aggregating across species requires to specify
#   likelihood as a function of mismatch wrt all species individually for a given site.
df_chi_forcing <- df_chi |>
  group_by(site) |>
  summarise(.groups = "keep",
            lon = mean(longitude),
            lat = mean(latitude),
            temp_degC    = NA_real_, #mean(temperature_gs),  # deg C,     growing season value (growing season, where monthly mean T>0)
            vpd_Pa       = NA_real_, #mean(vpd_gs),           # Pa,        growing season value (growing season, where monthly mean T>0)
            par_umolm2s  = NA_real_, #mean(par_gs),           # umol/m2/s, growing season value (growing season, where monthly mean T>0)
            elv_masl     = NA_real_, #mean(z),                # m asl
            co2_ppm      = NA_real_, #mean(ca)
            )  |>           # ppm
  mutate(
    patm_Pa = rpmodel::calc_patm(elv_masl)
  )
# TODO: get the meteo conditions through ingestr

df_chi_target <- df_chi |>
  group_by(site, species) |>
  summarise(.groups = "keep",
            lon = mean(longitude),
            lat = mean(latitude),
            chi_obs__ = mean(chi)   # unitless
  )

rm(df_chi)
rm(df_chi_allobs)

df_chi_target |>
  ggplot(aes(x = chi_obs__)) +
  geom_histogram(bins = 15)

rgeco:::plot_map_simpl() +
  geom_point(data = df_chi_forcing, aes(lon, lat))


saveRDS(df_chi_forcing, here::here("data/chi_forcing.rds"))
saveRDS(df_chi_target, here::here("data/chi_target.rds"))
















# TODO: replace forcing data with data from ingestr

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
# plot(mod$chi)
#### END EXAMPLE

#### DEVELOPMENT
df_chi_forcing_dummy <- df_chi_forcing[1,] |>
  mutate(
    temp_degC   = 17.7,    # deg C,     growing season value (growing season, where T>0)
    vpd_Pa      = 0.736,   # Pa,        growing season value (growing season, where T>0)
    par_umolm2s = 849,     # umol/m2/s, growing season value (growing season, where T>0)
    co2_ppm     = 360,     # ppm
    elv_masl    = 68,      # m asl
    patm_Pa     = rpmodel::calc_patm(elv_masl)
  )
df_chi_forcing_dummy

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
df_chi_modeled <- df_chi_forcing_dummy |>
  group_by(site) |> #, lon, lat) |>
  group_modify(~run_pmodel_onestep_f_bysite(
    lc4 = FALSE,
    forcing =  data.frame(temp = .x$temp_degC,        # TODO: with ingestr this could be daytime average
                          vpd  = .x$vpd_Pa,           # TODO: with ingestr this could be daytime average
                          ppfd = .x$par_umolm2s/1000, # TODO: rsofun needs ppfd in mol/m2/s
                          co2  = .x$co2_ppm,
                          patm = .x$patm_Pa),
    params_modl = params_modl,
    makecheck = FALSE)) |>
    rename(vcmax_mod_molm2s   = vcmax,
           jmax_mod_molm2s    = jmax,
           vcmax25_mod_molm2s = vcmax25,
           jmax25_mod_molm2s  = jmax25,
           gs_accl_mod_molCmolPhPa = gs_accl, # mol C (mol photons)\eqn{^{-1}} Pa\eqn{^{-1}
           wscal_mod__        = wscal,
           chi_mod__          = chi,
           iwue_mod__         = iwue,
           rd_mod_gCm2s       = rd)
  # transform to same units as targets:
  # # unneeded for chi


# Plot modelled vs observed

# Combine modelled and observed
df_chi_with_outputs <- dplyr::inner_join(df_chi_modeled, df_chi_target, by = join_by(site))

# Vcmax
df_chi_with_outputs |>
  ggplot(aes(chi_mod__, chi_obs__)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") #+
  # labs(
  #   x = "Modelled Chi (-)",
  #   y = "Observed Chi (-)"
  # )

#### END DEVELOPMENT


# TODO: (source: https://github.com/geco-bern/rsofun/issues/134, May 2023)
# notneeded:     In rsofun, introduce a non-structural C pool (NSC) with prescribed decay constant.
# notneeded:     Add GPP to NSC and treat isotopic signature explicitly (calculating d13C from simulated ci:ca,
# notneeded:     using Farquhar et al. (1989), see also Wang et al., 2017). NSC pool and d13C implementation
# since we can use:     output(7) = dble(out_pmodel%chi)


# TODO: colin: As far as I know the most extensive compilation of leaf-level
# δ13C data is in Lavergne et al. (2020) GCB. I suggest using Eq (2) in Lavergne
# et al. (2020) New Phytologist for the relationship between Δ13C and χ.



# TODO: Beni:
# @Fabian, the d13C data will have to be treated and modelled in a similar way as the vj data
#
# Here we need the below input forcing data for each site.
# This can be modelled in a much simplified version based on "geco-bern/get_Vcmax_data", which used daily values


# XXX todo: use worldclim through ingestr to get one-step forcing for daytime growing season values:
# - temperature
# - vpd
# - ppfd

# temp:
# - growing season: mean across months for which monthly tmean > 0 deg C
# - daytime temperature: derived as a function of tmin and tmax, see equation 5 in Peng et al., 2023 (https://onlinelibrary.wiley.com/doi/abs/10.1111/1365-2745.14208)

# vpd:
# - vpd abgeleitet aus vapour pressure (Worldclim), gemäss code in ingestr für watch-wfdei
# - (vpd(tmin) + vpd(tmax))/2
# - average only over months with tmean > 0

# ppfd:
#   - aus solar radiation, multiplikation mit faktor (2....) gemäss anderen datenprodukten in ingestr


# XXX todo: use ingestr to extract elevation given lon/lat, and calculate patm using standard atmospheric pressure (e.g., calc_patm() in rpmodel or ingestr)

# XXX todo: use ingestr to read CO2 value given column `collection.year`

