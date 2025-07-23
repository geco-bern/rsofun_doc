# This script appends climate data to the the input forcings for
# 'df_chi_forcing' and 'df_vj_forcing' by using the {ingestr} package.
#
# It needs access to worldclim data set.
#
# The appended forcing data sets are stored as *.rds files in subfolder data/

rm(list = ls())
library(tidyverse)
library(rpmodel)
library(rgeco) # pak::pkg_install("geco-bern/rgeco")
library(dplyr)
library(purrr)
# pak::pkg_install("geco-bern/rsofun@simple_pmodel_v2")
# library(rsofun)  # install from branch simple_pmodel_v2
# pak::pkg_install("geco-bern/ingestr")
library(ingestr)



# Load data --------------------------------------------------------------------
df_chi_forcing <- readRDS(here::here("data/chi_forcing.rds"))
df_vj_forcing  <- readRDS(here::here("data/vj_forcing.rds"))

df_chi_target <- readRDS(here::here("data/chi_target.rds"))
df_vj_target  <- readRDS(here::here("data/vj_target.rds"))


# Prepare ingestr --------------------------------------------------------------
siteinfo_all <- bind_rows(df_chi_forcing, df_vj_forcing) |>
  # ensure no duplicated sites
  select(sitename, year) |> # drop lon, lat (derive from sitename)
  ungroup() |>
  distinct() |>
  # derive (rounded) lon, lat from sitename
  separate(
    sitename, into=c(NA, "lon", NA, "lat"),
    sep = "_", remove = FALSE, convert = TRUE)

# siteinfo_all <- siteinfo_all %>% slice(c(32,34)) # for development
# siteinfo |> filter(is.na(year))
# siteinfo |> filter(!is.na(year))

siteinfo <- siteinfo_all |>
  # derive columns date_start and date_end
  mutate(year_to_use = ifelse(is.na(year), 2000, year)) |> # use year 2000 if no information provided
  mutate(date_start = lubridate::make_date(year_to_use, 1, 1),
         date_end   = lubridate::make_date(year_to_use, 12, 31)) |>
  # only subset needed columns
  select(sitename, date_start, date_end, lon, lat) #,Nobs, Nyears


# Run ingestr to get elv, co2, tavg,tmin,tmax,vapr,srad ------------------------
## Get elevation data
df_etopo <- ingest(
  siteinfo,
  source = "etopo1",
  dir = "/data/archive/etopo_NA_NA/data/"
) |>
  # rename with units
  unnest(data) |>
  rename(elv_masl = elv) |>
  nest(data = -c(sitename))

## Get daily CO2 data
df_co2 <- ingest(
  siteinfo,
  source  = "co2_mlo",
  verbose = FALSE
) |>
  # rename with units
  unnest(data) |>
  rename(co2_ppm = co2) |>
  nest(data = -c(sitename))

## Get climate data (12 values for avg conditions of each month between 1970-2000)
settings_worldclim <- list(varnam = c("tavg","tmin","tmax","vapr","srad"))
# Units as described: https://geco-bern.github.io/ingestr/articles/example.html#worldclim-1
# vapr, kPa
# srad, kJ m-2 day-1
# tmin, tmax, tavg, degC
# WorldClim is derived from CRU, thus in CRU:
#           tmp, tmn, tmx are: monthly average daily (mean,min,max) temperature
df_worldclim <- ingest(
  siteinfo,
  source    = "worldclim",
  settings  = settings_worldclim,
  dir = "/data/archive/worldclim_fick_2017/data/"
) |>
  # rename with units
  unnest(data) |>
  rename(vapr_kPa   = vapr,
         srad_kJm2d = srad,
         tavg_degC  = tavg,
         tmin_degC  = tmin,
         tmax_degC  = tmax) |>
  nest(data = -c(sitename))

# TODO: instead of 1970-2000 average, we might use the actual values of the actual years?


# Derive single values of co2, patm, ppfd, tgrowth, vpd (representing average growing conditions) ------------------------
## Select CO2 from 1990: ----
df_co2 |>     # provides daily co2 values
  unnest(data) |>
  # do only keep a instance of each distinct date
  group_by(year, date) |> slice(1) |>
  ggplot(aes(x=date, y=co2_ppm, group=sitename)) + geom_line()

df_co2_agg <- df_co2 |>
  unnest(data) |>
  # only keep january first value
  filter(lubridate::month(date) == 1,
         lubridate::day(date)   == 1) |>
  select(-date, -year)

## Derive patm from elevation: ----
df_etopo_agg <- df_etopo |>
  unnest(data) |>
  mutate(patm_Pa = ingestr::calc_patm(elv_masl))

## Derive growing average ppfd, tgrowth, vpd from monthly climate data: ----

### for ppfd:
# kfFEC <- 2.04          # from-flux-to-energy, umol/J (Meek et al., 1984)
kfFECmol <- 2.04 * 1e-6  # from-flux-to-energy, mol/J (Meek et al., 1984)

# NOTE: 2.04 is a combination of 40.8% of visible light is in PAR range [400, 700 nm]
#                             and average energy per mol photon of 200 kJ
#                             given E = h*c/lambda, 300 kJ/mol at 700 nm and 170 kJ/mol at 400
#                             assuming a uniform spectrum between [400, 700 nm] gives 200 kJ/mol
# 1                         # J visible light /s/m2
# 1 * 0.408                 # J PAR/s/m2
# 1 * 0.408 / 200000 * 10^6 # umol PAR/s/m2 # with 200000 J/(mol photons) average


### for Tgrowth:
# just use ingestr::calc_tgrowth
# ingestr::calc_tgrowth(10,20,lat=45,doy=180)
    # calc_tgrowth_FB <- function(Tmin, Tmax, lat_deg, doy){# eq.5 in Peng et al., 2023 (https://onlinelibrary.wiley.com/doi/abs/10.1111/1365-2745.14208)
    #   # delta: deg,  monthly average solar declination (angle between equatorial plane and sun's rays)
    #   delta = 23.45 * sin(2*pi * (284 + doy) / 365.25) # TODO: find source
    #
    #   # eq. A7.2, Jones, Plants and Microclimate, 2014, https://doi.org/10.1017/CBO9780511845727
    #   xx <- -tanpi(lat_deg/180) * tanpi(delta/180) # cosine of the hour angle of the sun at sunrise or sunset
    #   # daylength <- acos(xx) * 2 / 15
    #
    #   summand <- 0.5*sqrt(1 - xx^2) / acos(xx)  # units of inverse radians ??
    #   Tg <- Tmax * (0.5 + summand)
    #       + Tmin * (0.5 - summand)
    #   return(Tg) # same units as Tmax and Tmin
    # }
    # calc_tgrowth_FB(      10,20,lat_deg=45,doy=180)

###
df_worldclim2 <- df_worldclim |>
  # derive (rounded) lon, lat from sitename
  separate(
    sitename, into=c(NA, "lon", NA, "lat"),
    sep = "_", remove = FALSE, convert = TRUE) |> # re-append lon, lat dplyr::left_join(siteinfo, by = join_by(sitename)) |>
  # postprocess climate data
  unnest(data) |>
  rowwise() |>
  # add flag for growing season, add ppfd, add tgrowth,
  mutate(
    growing_season  = tavg_degC > 0,

    # ppfd:
    ppfd_molm2s     = 1e3 * srad_kJm2d /3600/24 * kfFECmol,    # kJ m-2 day-1 -> J m-2 s-1 -> mol m-2 s-1

    # tgrowth:
    # (i.e. average temperature during daytime, considering daylength, assuming sinusoidal temp profile):
    # eq.5 in Peng et al., 2023 (https://onlinelibrary.wiley.com/doi/abs/10.1111/1365-2745.14208)
    doy             = lubridate::make_date(1970, month, 15) |> lubridate::yday(),
    tgrowth_degC    = ingestr::calc_tgrowth(tmin_degC,tmax_degC,lat=lat,doy=doy),

    # vpd:
    # variant 1:
    VPD_TminTmaxAVG_Pa = ingestr::calc_vpd(eact = vapr_kPa*1e3, tmin = tmin_degC, tmax = tmax_degC),
    # variant 2: (Peng)
    VPD_Tgrowth_Pa     = ingestr::calc_vpd(eact = vapr_kPa*1e3, tc = tgrowth_degC),
  )


stopifnot(0 == nrow(df_worldclim2 |> filter(is.na(tgrowth_degC)))) # TODO: here we're loosing some sites
df_worldclim2 |> filter(is.na(tgrowth_degC))  |> group_by(sitename) |> slice(1)   # 41 sites have NA in tgrowth
df_worldclim2 |> filter(!is.na(tgrowth_degC)) |> group_by(sitename) |> slice(1)   # 585 siteas have at least 1 non-NA tgrowth

pl_grow_seasonNA <-
  rgeco:::plot_map_simpl() + # 41 sites have NA in tgrowth # TODO: can we keep these sites for the analysis?
  geom_point(
    data    = df_worldclim2 |> filter(is.na(tgrowth_degC)),
    mapping = aes(lon, lat))
ggsave(here::here("fig/fig_00_append_climate.png"), pl_grow_seasonNA, width=7.2, height=3.6, units="in")



# temp:
# - growing season: mean across months for which monthly tmean > 0 deg C
# - daytime temperature: derived as a function of tmin and tmax, see equation 5 in Peng et al., 2023 (https://onlinelibrary.wiley.com/doi/abs/10.1111/1365-2745.14208)

# vpd:
# - vpd abgeleitet aus vapour pressure (Worldclim), gemäss code in ingestr für watch-wfdei
# - (vpd(tmin) + vpd(tmax))/2
# - average only over months with tmean > 0

# ppfd:
#   - aus solar radiation, multiplikation mit faktor (2....) gemäss anderen datenprodukten in ingestr

df_worldclim_agg <-
  df_worldclim2 |>
  group_by(sitename) |>
  # compute means across growing season (i.e. across months for which monthly tmean > 0 deg C)
  filter(growing_season) |>
  summarise(
    growing_season_months = paste0(month,collapse = ","),
    growing_season_length = length(month),
    growing_season_length2= n(), # TODO: this is for double checking

    temp = mean(tgrowth_degC),
    temp2 = mean(tgrowth_degC, na.rm=T), # TODO: this is for double checking

    vpd = mean(VPD_TminTmaxAVG_Pa), # TODO: remove one of the two
    vpd2= mean(VPD_Tgrowth_Pa),

    ppfd=mean(ppfd_molm2s)
  )




# Combine and output
df_trait_forcing_filled <-
  dplyr::inner_join(df_etopo_agg |> nest(patm = c(elv_masl, patm_Pa)),
                    df_co2_agg   |> nest(co2  = c(co2_ppm)),
                    by = join_by(sitename)) |>
  dplyr::inner_join(df_worldclim_agg |> nest(clim = c(growing_season_months,growing_season_length,growing_season_length2,temp,temp2,vpd,vpd2,ppfd)),
                    by = join_by(sitename)) |>
  # re-append lon, lat
  dplyr::left_join(siteinfo, by = join_by(sitename)) |>
  select(sitename, lon, lat, patm, co2, clim)

saveRDS(df_trait_forcing_filled, file = here::here("data/chi-vj_forcing.rds"))

df_trait_targets <-
  bind_rows(df_chi_target |> nest(chi = -c(sitename, lon, lat)),
            df_vj_target  |> nest(vj  = -c(sitename, lon, lat))) |>
  # ensure no duplicated sites
  select(sitename, lon, lat, chi, vj) |>
  ungroup() |>
  distinct()

saveRDS(df_trait_targets, file = here::here("data/chi-vj_targets.rds"))





# Check growing season lengths
pl_grow_season <- rgeco:::plot_map_simpl() +
  geom_point(
    data    = unnest(df_trait_forcing_filled, clim),
    mapping = aes(lon, lat, color = growing_season_length)) +
  labs(color = "Length of\ngrowing\nseason\n(months)")
pl_grow_season
ggsave(here::here("fig/fig_01_append_climate.png"), pl_grow_season, width=7.2, height=3.6, units="in")

# Check all input data
plot_clim <- function(colname_to_plot, label = NULL){
  rgeco:::plot_map_simpl() +
    {if(!is.null(label)) labs(color = label) } +
    geom_point(
      data    = unnest(df_trait_forcing_filled, clim),
      mapping = aes(lon, lat, color = .data[[colname_to_plot]]))
}
library(patchwork)
p1 <- plot_clim("growing_season_length", "Length of\ngrowing\nseason\n(months)")
p2 <- plot_clim("ppfd", "PPFD\n(mol/m2/s)")
p3 <- plot_clim("temp", "Mean\ngrowth\ntemp\n(deg C)")
p4 <- plot_clim("temp2","Mean\ngrowth\ntemp\n(na.rm)\n(deg C)")
p5 <- plot_clim("vpd",  "VPD\n(f(Tmin,Tmax))\n(Pa)")
p6 <- plot_clim("vpd2", "VPD\n(f(Tgrowth))\n(Pa)")
pl_all_climate <-
  (p1+p2)/
  (p3+p4)/
  (p5+p6)
ggsave(here::here("fig/fig_02_append_climate.png"), pl_all_climate, width=7.2*2, height=3.6*3, units="in")



######### TODO ###################
##################################
##################################
rgeco:::plot_map_simpl() +
  geom_point(data = df_trait_forcing_filled, aes(lon, lat))

# Issue 1: (some sites have no values in worldclim data)
worldclim_missing_sites <- c(
  "lon_+158.80_lat_-054.50", "lon_-041.75_lat_-022.38", "lon_-063.80_lat_-064.80",
  "lon_-063.82_lat_-064.82", "lon_-069.83_lat_+011.61", "lon_-072.38_lat_+078.53",
  "lon_-074.60_lat_+078.58", "lon_-075.92_lat_+078.88", "lon_-079.38_lat_+008.97",
  "lon_-086.70_lat_+076.35", "lon_+113.83_lat_+004.18")
# missing_sites <- anti_join(siteinfo_all, df_trait_forcing_filled, by = join_by(sitename))
# df_etopo |> filter(sitename %in% missing_sites$sitename) # OK
# df_co2_agg |> filter(sitename %in% missing_sites$sitename) # OK
# df_worldclim |> filter(sitename %in% missing_sites$sitename) |> unnest(data) # got NA
# df_worldclim2 |> filter(sitename %in% missing_sites$sitename) # got NA
# df_worldclim_agg |> filter(sitename %in% missing_sites$sitename) # not ok

df_worldclim |> filter(sitename %in% worldclim_missing_sites) |> unnest(data) |> print(n=100)# got NA

# Issue 2: (one site has tavg_degC == 0 for all months, and thus no growing season)
df_etopo |> filter(sitename == "lon_-074.60_lat_+078.58")
df_co2_agg |> filter(sitename == "lon_-074.60_lat_+078.58")
df_worldclim2 |> filter(sitename == "lon_-074.60_lat_+078.58") # OK
df_worldclim_agg |> filter(sitename == "lon_-074.60_lat_+078.58") # not ok
######### TODO ###################
##################################
##################################



# VJ:
  # NOTE: potentially replace forcing data with data from ingestr to homogenize with chi data.
  #       however, we do not have the time information
  # TODO: compare the climate data, derived here with the one provided in vj data set

df_compare_forcing <- dplyr::inner_join(
  # a) climate data derive above
  df_trait_forcing_filled |>
    unnest(c(patm, co2, clim)) |>
    select(sitename,
           elv_masl   = elv_masl,
           patm_Pa    = patm_Pa,
           co2_ppm    = co2_ppm,
           temp_degC  = temp,
           temp2_degC_ingestr = temp2,
           vpd_Pa     = vpd,
           vpd2_Pa_ingestr = vpd2,
           ppfd_molm2s= ppfd),
  # b) climate data provided in vj data set
  df_vj_forcing |>
    mutate(ppfd_molm2s = par_molm2s) |>
    select(sitename, elv_masl, patm_Pa, co2_ppm, temp_degC, vpd_Pa, ppfd_molm2s),
  by = join_by(sitename),
  suffix = c("_ingestr", "_vjdata")
  )

library(ggplot2)
plot_comparison <- df_compare_forcing |>
  pivot_longer(-sitename) |>
  separate(name, into = c("name", "source"), sep = "_(?=[^_]+$)") |>
  pivot_wider(names_from = source, values_from = value) |>
  # filter(name %in% c("temp_degC","temp2_degC","vpd_Pa","vpd2_Pa")) |>
  # filter(is.na(vjdata))
  tidyr::fill(vjdata) |> # fills up the alternative versions of vpd2 and temp2
  ggplot(aes(x=vjdata, y=ingestr)) + geom_point() + facet_wrap(~name, scales = "free") +
  geom_abline() + ggtitle("For VJ dataset:\nComparing ingestr vs vj climate")
plot_comparison
ggsave(here::here("fig/fig_03_append_climate.png"), plot_comparison, width=7.2, height=7.2, units="in")


# TODO: future work:
# - assess differences between ppfd_vj and ppfd_ingestr, there appears to be a factor of 2.0
# - assess impact of two different VPD computations (f(tmax)+f(tmin))/2 vs f(tgrowth)
# - could be using CO2_ppm from VJ data set to derive collection.year of VJ measurements
# - could be using monthly (CRU or Worldclim) meteo data for the given year, instead of 1970-2000 climate average
# - reformat 'df_trait_forcing_filled' similarly to read_rds("/data_2/FluxDataKit/v3.4/zenodo_upload/rsofun_driver_data_v3.4.2.rds")






#### DO THE SAME FOR TIME SERIES FORCING DATA
# TODO: combine this with the
# df_timeseries_forcing <- tibble()

#   Download FluxDataKit data from Zenodo:
#   sudo apt install librdf0-dev
#   install.packages("zen4R")
# library(zen4R)
# download_path <- tempdir(check = TRUE)
# download_zenodo(path = download_path, "10.5281/zenodo.14808331", files = "rsofun_driver_data_v3.4.2.rds") # v3.4.2
# df_timeseries_forcing_and_ <- readRDS(file.path(download_path, "rsofun_driver_data_v3.4.2.rds"))

