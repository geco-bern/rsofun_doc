library(tidyverse)
library(rgeco)   # remotes::install_github("https://github.com/geco-bern/rgeco")
library(leaf13C) # remotes::install_github("traitecoevo/datastorr")
                 # remotes::install_github("traitecoevo/leaf13C")


# Please cite
# Cornwell, William K., et al. “Climate and soils together regulate photosynthetic carbon isotope discrimination within C3 plants worldwide.” Global Ecology and Biogeography 27.9 (2018): 1056-1067.

# see https://github.com/traitecoevo/leaf13C/blob/master/leaf13C_metadata.txt for column description
df <- leaf13C::get_data(version = "0.2.2")

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

  if (!is.na(mgdd0)){

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

df <- df |>
  rowwise() |>
  mutate(chi = calc_chi_bigdelta(big.D13.merged, ca = 400))

df_sites <- df |>
  ungroup() |>
  mutate(site = sprintf("lon_%+08.3f_lat_%+08.3f", longitude, latitude)) |>
  group_by(site) |>
  summarise(
    lon = mean(longitude),
    lat = mean(latitude),
    year = mean(year),
    chi = mean(chi)
  )

df_sites |>
  ggplot(aes(x = chi)) +
  geom_histogram(bins = 15)

rgeco:::plot_map_simpl() +
  geom_point(data = df_sites, aes(lon, lat))


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
