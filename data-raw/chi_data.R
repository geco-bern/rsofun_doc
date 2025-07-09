remotes::install_github("traitecoevo/datastorr")
remotes::install_github("traitecoevo/leaf13C")

library(leaf13C)

# Please cite
# Cornwell, William K., et al. “Climate and soils together regulate photosynthetic carbon isotope discrimination within C3 plants worldwide.” Global Ecology and Biogeography 27.9 (2018): 1056-1067.

# see https://github.com/traitecoevo/leaf13C/blob/master/leaf13C_metadata.txt for column description
df <- leaf13C::get_data()

# Calculate chi (ci:ca) from big-Delta data.
calc_chi_bigdelta <- function(bigdelta, ca, mgdd0 = NA){

  # Parameters
  a <- 4.4
  b <- 27.0

  if (!is.na(mgdd0)){

    # account for co2 compensation point and its temperature dependency
    f <- 8.0
    k <- 0.0512
    gammastar25 <- 42.75
    gammastar <- gammastar25*exp(k*(mgdd0 - 25.0))
    chi <- (bigdelta - a + f*gammastar/ca)/(b - a)

  } else {

    chi <- (bigdelta - a)/(b - a)

  }

  return(chi)
}

df <- df |>
  rowwise() |>
  mutate(chi = calc_chi_bigdelta(big.D13.merged, 400))  # using [co2] = 400 here, but should use value corresponding to data given observation year

df_sites <- df |>
  ungroup() |>
  mutate(site = paste0("lon_", as.character(format(longitude, digits = 3)), "_lat_", as.character(format(latitude, digits = 3)))) |>
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

rgeco::plot_map_simpl() +
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
