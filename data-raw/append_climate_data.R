# This script appends climate data to the the input forcings for
# 'df_bigD13C_forcing' and 'df_vj_forcing' by using the {ingestr} package.
# And then combines it to
#
# It needs access to worldclim data set in the form of *.tif files
# located at '/data/archive/worldclim_fick_2017/data/*.tif'.
#
# The appended forcing data sets are stored as *.rds files in subfolder data/

library(tidyverse)
library(rpmodel)
library(rgeco) # pak::pkg_install("geco-bern/rgeco")
library(dplyr)
library(purrr)
library(ingestr)
library(terra)
library(patchwork)
library(ggplot2)


# Load data --------------------------------------------------------------------
df_bigD13C_forcing <- read_rds(here::here("data/00_bigD13C_forcing.rds"))
df_vj_forcing  <- read_rds(here::here("data/00_vj_forcing.rds"))

df_bigD13C_target <- read_rds(here::here("data/00_bigD13C_target.rds"))
df_vj_target  <- read_rds(here::here("data/00_vj_target.rds"))

      # # TODO: just checking for gpp sites: if ingestr-derived ppfd agrees with "rsofun_driver_data_v3.4.2.rds"
      #   NOTE: we normally do this later when combining the forcing data for the bigD13C and vj data with the gpp data
      # #   Download FluxDataKit data from Zenodo:
      # #   sudo apt install librdf0-dev
      # #   install.packages("zen4R")
      # library(zen4R)
      # download_path <- tempdir(check = TRUE)
      # download_zenodo(path = download_path, "10.5281/zenodo.14808331", files = "fdk_site_info.csv")
      # download_zenodo(path = download_path, "10.5281/zenodo.14808331", files = "fdk_site_fullyearsequence.csv")
      # download_zenodo(path = download_path, "10.5281/zenodo.14808331", files = "rsofun_driver_data_v3.4.2.rds")
      # fdk_site_info             <- readr::read_csv(file.path(download_path, "fdk_site_info.csv"))
      # fdk_site_fullyearsequence <- readr::read_csv(file.path(download_path, "fdk_site_fullyearsequence.csv"))
      # drivers <- read_rds(file.path(download_path, "rsofun_driver_data_v3.4.2.rds"))
      #
      # # testthat::expect_equal(fdk_site_info,FluxDataKit::fdk_site_info) # THis fails
      # # testthat::expect_equal(   # This succeeds
      # #   fdk_site_info |>
      # #     select(-koeppen_code),
      # #   FluxDataKit::fdk_site_info |>
      # #     mutate(reference_height = unname(reference_height)) |>
      # #     select(-koeppen_code))
      #
      # gpp_sites_to_use <- fdk_site_info |>
      #   filter(!(igbp_land_use %in% c("CRO", "WET"))) |>
      #   left_join(
      #     fdk_site_fullyearsequence,
      #     by = "sitename"
      #   ) |>
      #   filter(nyears_gpp > 10)
      #
      # df_gpp_forcingtarget <- drivers |> filter(sitename %in% gpp_sites_to_use$sitename)
      # # # END TODO


# Prepare ingestr --------------------------------------------------------------
siteinfo_all <- bind_rows(df_bigD13C_forcing, df_vj_forcing) |>
  # ensure no duplicated sites
  select(sitename, year) |> # drop lon, lat (derive from sitename)
  ungroup() |>
  distinct() |>
  # derive (rounded) lon, lat from sitename
  separate(
    sitename, into=c(NA, "lon", NA, "lat"),
    sep = "_", remove = FALSE, convert = TRUE)

      # # TODO: just checking for gpp sites: if ingestr-derived ppfd agrees with "rsofun_driver_data_v3.4.2.rds"
      # # append gpp sites to siteinfo_all
      # siteinfo_all <- bind_rows(
      #   siteinfo_all,
      #   df_gpp_forcingtarget |>
      #     # get year from forcing
      #     group_by(sitename) |> unnest(forcing) |> slice(1) |> mutate(year = lubridate::year(date)) |>
      #     # get lon, lat from site_info
      #     select(sitename, site_info, year) |> unnest(site_info) |>
      #     select(sitename, lon, lat, year)
      # )
      # # END TODO

siteinfo <- siteinfo_all |>
  # derive columns date_start and date_end
  mutate(year_to_use = ifelse(is.na(year), 2000, year)) |> # use year 2000 if no information provided
  mutate(date_start = lubridate::make_date(year_to_use, 1, 1),
         date_end   = lubridate::make_date(year_to_use, 12, 31)) |>
  # only subset needed columns
  select(sitename, date_start, date_end, lon, lat) #,Nobs, Nyears

# Get metadata on landcover and climate
## Add Koeppen-Geiger
Beck_KG_metadata <- tribble(
    ~Beck_KG_code, ~Beck_KG, ~Beck_KG_description, ~Beck_KG_colorcode,
    # source: https://doi.org/10.6084/m9.figshare.6396959 (legend.txt)
    1,   "Af",   "Tropical, rainforest",                   "[0 0 255]",
    2,   "Am",   "Tropical, monsoon",                      "[0 120 255]",
    3,   "Aw",   "Tropical, savannah",                     "[70 170 250]",
    4,   "BWh",  "Arid, desert, hot",                      "[255 0 0]",
    5,   "BWk",  "Arid, desert, cold",                     "[255 150 150]",
    6,   "BSh",  "Arid, steppe, hot",                      "[245 165 0]",
    7,   "BSk",  "Arid, steppe, cold",                     "[255 220 100]",
    8,   "Csa",  "Temperate, dry summer, hot summer",      "[255 255 0]",
    9,   "Csb",  "Temperate, dry summer, warm summer",     "[200 200 0]",
    10,  "Csc",  "Temperate, dry summer, cold summer",     "[150 150 0]",
    11,  "Cwa",  "Temperate, dry winter, hot summer",      "[150 255 150]",
    12,  "Cwb",  "Temperate, dry winter, warm summer",     "[100 200 100]",
    13,  "Cwc",  "Temperate, dry winter, cold summer",     "[50 150 50]",
    14,  "Cfa",  "Temperate, no dry season, hot summer",   "[200 255 80]",
    15,  "Cfb",  "Temperate, no dry season, warm summer",  "[100 255 80]",
    16,  "Cfc",  "Temperate, no dry season, cold summer",  "[50 200 0]",
    17,  "Dsa",  "Cold, dry summer, hot summer",           "[255 0 255]",
    18,  "Dsb",  "Cold, dry summer, warm summer",          "[200 0 200]",
    19,  "Dsc",  "Cold, dry summer, cold summer",          "[150 50 150]",
    20,  "Dsd",  "Cold, dry summer, very cold winter",     "[150 100 150]",
    21,  "Dwa",  "Cold, dry winter, hot summer",           "[170 175 255]",
    22,  "Dwb",  "Cold, dry winter, warm summer",          "[90 120 220]",
    23,  "Dwc",  "Cold, dry winter, cold summer",          "[75 80 180]",
    24,  "Dwd",  "Cold, dry winter, very cold winter",     "[50 0 135]",
    25,  "Dfa",  "Cold, no dry season, hot summer",        "[0 255 255]",
    26,  "Dfb",  "Cold, no dry season, warm summer",       "[55 200 255]",
    27,  "Dfc",  "Cold, no dry season, cold summer",       "[0 125 125]",
    28,  "Dfd",  "Cold, no dry season, very cold winter",  "[0 70 95]",
    29,  "ET",   "Polar, tundra",                          "[178 178 178]",
    30,  "EF",   "Polar, frost",                           "[102 102 102]")
r <- rast("/data/archive/koeppengeiger_beck_2018/data/Beck_KG_V1_present_0p5.tif"); # plot(r)
siteinfo$Beck_KG_code = extract(r, select(siteinfo, c(lon, lat)), ID = FALSE)[["Beck_KG_V1_present_0p5"]]
siteinfo <- siteinfo |>
  left_join(Beck_KG_metadata, by = join_by(Beck_KG_code)) |>
  select(-Beck_KG_code, -Beck_KG_description, -Beck_KG_colorcode)
siteinfo <- siteinfo |>
  mutate(Beck_KG = if_else(sitename == "lon_-079.38_lat_+008.97","EF",Beck_KG))
stopifnot(all(!is.na(siteinfo$Beck_KG)))

## Add LCCS landcover
lccs_metadata <- tribble(
  ~lccs_code, ~lccs, ~lccs_colorcode,
  # source: Table 1-2 https://dast.copernicus-climate.eu/documents/satellite-land-cover/WP2-FDDP-LC-2021-2022-SENTINEL3-300m-v2.1.1_PUGS_v1.1_final.pdf
  0, "No Data", "0, 0, 0",
  10, "Cropland, rainfed", "255, 255, 100",
  20, "Cropland, irrigated or post-flooding", "170, 240, 240",
  30, "Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)", "220, 240, 100",
  40, "Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) /cropland (<50%)", "200, 200, 100",
  50, "Tree cover, broadleaved, evergreen, closed to open (>15%)", "0, 100, 0",
  60, "Tree cover, broadleaved, deciduous, closed to open (>15%)", "0, 160, 0",
  70, "Tree cover, needleleaved, evergreen, closed to open (>15%)", "0, 60, 0",
  80, "Tree cover, needleleaved, deciduous, closed to open (>15%)", "40, 80, 0",
  90, "Tree cover, mixed leaf type (broadleaved and needleleaved)", "120, 130, 0",
  100, "Mosaic tree and shrub (>50%) / herbaceous cover (<50%)", "140, 160, 0",
  110, "Mosaic herbaceous cover (>50%) / tree and shrub (<50%)", "190, 150, 0",
  120, "Shrubland", "150, 100, 0",
  130, "Grassland", "255, 180, 50",
  140, "Lichens and mosses", "255, 220, 210",
  150, "Sparse vegetation (tree, shrub, herbaceous cover) (<15%)", "255, 235, 175",
  160, "Tree cover, flooded, fresh or brackish water", "0, 120, 90",
  170, "Tree cover, flooded, saline water", "0, 150, 120",
  180, "Shrub or herbaceous cover, flooded, fresh/saline/brackish water", "0, 220, 130",
  190, "Urban areas", "195, 20, 0",
  200, "Bare areas", "255, 245, 215",
  210, "Water bodies", "0, 70, 200",
  220, "Permanent snow and ice", "255, 255, 255")
r2 <- rast("/data/archive/landcover_defourny_2023/data/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc"); # plot(r2[["lccs_class"]])
siteinfo$lccs_code = extract(r2[["lccs_class"]], select(siteinfo, c(lon, lat)), ID = FALSE)[["lccs_class"]]
siteinfo <- siteinfo |>
  mutate(lccs_code = floor(lccs_code/10)*10) |> # floor regional codes to global codes
  left_join(lccs_metadata, by = join_by(lccs_code)) |>
  select(-lccs_code, -lccs_colorcode)
stopifnot(all(!is.na(siteinfo$lccs)))
siteinfo <- siteinfo |> rename(Defourny_LCCS = lccs)


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
df_worldclim_raw <- ingest(
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

# NOTE: 10 sites are filtered out since they
# did not return worldclim data
# df_worldclim_raw |> unnest(c(data)) |>
#   filter(if_any(everything(), is.na)) |>
#   slice(1)
df_worldclim_monthly <- df_worldclim_raw |>
  unnest(c(data)) |>
  filter(!if_any(everything(), is.na)) |>
  nest(data = -c(sitename)) |>
  ungroup()

# Temporally disaggregate/downscale monthly climate to daily climate values  ------------------------
# COPYING/ADAPTING FUNCTIONS FROM INGESTR:

# Interpolates monthly data to daily data using polynomials or linear
# for a single year
expand_clim_worldclim_monthly <- function( mdf, worldclim_vars ){
  # define variables
  sitename <- year <- NULL

  ddf <- mdf |>
    # apply it separately for each site and each year
    group_split(sitename, year) |>
    purrr::map(\(df) expand_clim_worldclim_monthly_byyr(first(df$year), df, worldclim_vars) |>
                 mutate('sitename' = first(df$sitename)) #ensure to keep sitename
    ) |>
    bind_rows()

  return( ddf )

}
# Interpolates monthly data to daily data using polynomials or linear
# for a single year
expand_clim_worldclim_monthly_byyr <- function( yr, mdf, worldclim_vars ){
  mdf <- mdf |> rename( # TODO: just for in-script use, to be removed when integrating into {ingestr}
    vapr = vapr_kPa,    # TODO: just for in-script use, to be removed when integrating into {ingestr}
    srad = srad_kJm2d,  # TODO: just for in-script use, to be removed when integrating into {ingestr}
    tavg = tavg_degC,   # TODO: just for in-script use, to be removed when integrating into {ingestr}
    tmin = tmin_degC,   # TODO: just for in-script use, to be removed when integrating into {ingestr}
    tmax = tmax_degC)   # TODO: just for in-script use, to be removed when integrating into {ingestr}

  # define variables
  year <- ccov_int <- NULL
  nmonth <- 12

  startyr <- mdf$year %>% first()
  endyr   <- mdf$year %>% last()

  yr_pvy <- max(startyr, yr-1)
  yr_nxt <- min(endyr, yr+1)

  # add first and last year to head and tail of 'mdf'
  first <- mdf[1:12,] %>% mutate( year = year - 1)
  last  <- mdf[(nrow(mdf)-11):nrow(mdf),] %>% mutate( year = year + 1 )

  ddf <- ingestr:::init_dates_dataframe( yr, yr )


  # air temperature: interpolate using polynomial
  polynomial_interpolate <- function(mdf, yr, yr_pvy, yr_nxt, var = "tavg"){
    mval     <- dplyr::filter( mdf, year==yr     )[[var]]
    mval_pvy <- dplyr::filter( mdf, year==yr_pvy )[[var]]
    mval_nxt <- dplyr::filter( mdf, year==yr_nxt )[[var]]
    if (length(mval_pvy)==0){mval_pvy <- mval}
    if (length(mval_nxt)==0){mval_nxt <- mval}

    return(
      init_dates_dataframe( yr, yr ) %>%
        mutate(
          "{var}" := ingestr:::monthly2daily( mval, "polynom", mval_pvy[nmonth], mval_nxt[1], leapyear = lubridate::leap_year(yr) ) )
    )
  }
  if ("tavg" %in% worldclim_vars){
    ddf <- polynomial_interpolate( mdf, yr, yr_pvy, yr_nxt, var = "tavg") %>%
      right_join( ddf, by = c("date") )
  }

  # daily minimum and maximum air temperature: interpolate using polynomial
  if ("tmin" %in% worldclim_vars){
    ddf <- polynomial_interpolate( mdf, yr, yr_pvy, yr_nxt, var = "tmin") %>%
      right_join( ddf, by = c("date") )
  }
  if ("tmax" %in% worldclim_vars){
    ddf <- polynomial_interpolate( mdf, yr, yr_pvy, yr_nxt, var = "tmax") %>%
      right_join( ddf, by = c("date") )
  }

  # cloud cover: interpolate using polynomial
  if ("ccov" %in% worldclim_vars){
    ddf <- polynomial_interpolate( mdf, yr, yr_pvy, yr_nxt, var = "ccov") %>%
      rename(ccov_int = ccov) %>%
      # Reduce CCOV to a maximum 100%
      mutate( ccov = ifelse( ccov_int > 100, 100, ccov_int ) ) %>%
      right_join( ddf, by = c("date") ) %>%
      select(-ccov_int)
  }

  # solar radiation: interpolate using polynomial
  if ("srad" %in% worldclim_vars){
    ddf <- polynomial_interpolate( mdf, yr, yr_pvy, yr_nxt, var = "srad") %>%
      right_join( ddf, by = c("date") )
  }

  # VPD: interpolate vapor pressure 'vapr' using polynomial
  if ("vapr" %in% worldclim_vars){
    ddf <- polynomial_interpolate( mdf, yr, yr_pvy, yr_nxt, var = "vapr") %>%
      right_join( ddf, by = c("date") )
  }

  # precipitation: interpolate using weather generator

  if ("prec" %in% worldclim_vars){
    mprec <- dplyr::filter( mdf, year==yr )$prec
    mwetd <- dplyr::filter( mdf, year==yr )$wetd

    if (any(!is.na(mprec))&&any(!is.na(mwetd))){
      ddf <-  init_dates_dataframe( yr, yr ) %>%
        mutate( prec = get_daily_prec( mprec, mwetd, leapyear = lubridate::leap_year(yr) ) ) %>%
        right_join( ddf, by = c("date") )
    }
  }

  ddf <- ddf |> rename(  # TODO: just for in-script use, to be removed when integrating into {ingestr}
    vapr_kPa   = vapr,   # TODO: just for in-script use, to be removed when integrating into {ingestr}
    srad_kJm2d = srad,   # TODO: just for in-script use, to be removed when integrating into {ingestr}
    tavg_degC  = tavg,   # TODO: just for in-script use, to be removed when integrating into {ingestr}
    tmin_degC  = tmin,   # TODO: just for in-script use, to be removed when integrating into {ingestr}
    tmax_degC  = tmax)   # TODO: just for in-script use, to be removed when integrating into {ingestr}

  return( ddf )

}

mdf <- df_worldclim_monthly |>
  # slice_sample(n=10) |> # for development
  unnest(data) |>
  mutate(date = make_date(month = month, day = 1)) |>
  mutate(year = year(date)) |>
  group_by(sitename) |>
  ungroup()

# worldclim_vars <- c("tavg_degC", "tmin_degC", "tmax_degC", "vapr_kPa", "srad_kJm2d")
worldclim_vars <- c("tavg", "tmin", "tmax", "vapr", "srad")
df_worldclim <-
  expand_clim_worldclim_monthly( mdf, worldclim_vars ) |>
    group_by(sitename) |>
    select(sitename, date, tavg_degC, tmin_degC, tmax_degC, vapr_kPa, srad_kJm2d) |>
    nest(data = -c(sitename))

# Illustrate temporal disaggregation of monthly climate values
set.seed(1982)
df_for_plot_monthly <- df_worldclim_monthly |> slice_sample(n=10) |> unnest(data) |> mutate(date = make_date(month = month, day = 1))
df_for_plot_daily   <- df_worldclim |> unnest(data) |> filter(sitename %in% df_for_plot_monthly$sitename)

pl_disaggregation <- ggplot(df_for_plot_monthly, aes(x = date, y = tavg_degC, group = sitename)) + # color = sitename
  scale_x_date(date_breaks = "month", date_labels = "%b, %dst") +
  # monthly data:
  geom_step(direction = "hv") + # since we put the monthly avg at the first of each month
  # daily data:
  geom_line(data = df_for_plot_daily, color = "red")
pl_disagg <-
  ( # first row
    (pl_disaggregation + aes(y=tavg_degC)) +
      (pl_disaggregation + aes(y=tmin_degC)) +
      (pl_disaggregation + aes(y=tmax_degC))
  )/( # second row
    (pl_disaggregation + aes(y=vapr_kPa))
  )/( # third row
      (pl_disaggregation + aes(y=srad_kJm2d))
  )
ggsave(here::here("fig/00_fig_A_append_climate_disaggregate.png"), pl_disagg, width=7.2, height=7.2, units="in")


# Derive single values of co2, patm, ppfd, tgrowth, vpd (representing average growing conditions) ------------------------
## Select CO2: ----
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
    myingestr_calc_tgrowth <- function (tmin, tmax, lat, doy) {
      out_berger <- ingestr:::get_berger_tls(doy)
      xx <- (-1.0) * tan(lat * pi / 180) * tan(out_berger$decl_angle * pi / 180) # xx is -ru/rv in Davis (2017)
      # xx is in the range of [-2 to +2] for lat going -80 to +80
      if (xx <= -1){       # polar day (midnight sun)
        hs                                   <- pi       # radians, according to Davis (2017)
        #daylength_h                         <- 24
        tgrowth <- tmin + (tmax-tmin)/2 # 24h day: average T is average of Tmax and Tmin (since lim h=>pi sin(pi)/pi = 0)
      } else if (xx >= 1){ # polar night
        hs                                   <- 0        # radians, according to Davis (2017)
        hour_angle_of_sun_at_sunrise_sunset  <- 0        # degrees, according to Davis (2017)
        #daylength_h <- 0
        tgrowth <- tmax            # edge case: 1s day: average T is Tmax (since lim h=>0 sin(h)/h = 1)

      } else {
        # eq. A7.2, Jones (2014), Plants and Microclimate:
        hs <- acos(xx)                                            # radians
        # hour_angle_of_sun_at_sunrise_sunset <- acos(xx) *180 / pi # degrees
        # daylength_h <- 2*hour_angle_of_sun_at_sunrise_sunset*24/360

        # Version Fabian: assume subdaily temp: with T(h) = Tmin + dT/2 + dT/2*cos(h) (for h=[0,pi]=[midday,midnight], so that it is Tmax at h=0 and Tmin at h=pi)
        #                 with dT = Tmax - Tmin
        #                 Average T during day hours can be expressed as integral (similar to Davis (2017)):
        #                 tgrowth = 2/h_s \int_0^{h_s} T(h) dh
        #                 (with hs the hour angle of the sun, assuming h=0 at noon and h=pi at midnight)
        tgrowth <- tmin + (tmax-tmin)/2 + (tmax-tmin)/2/hs*sin(hs)
      }
      return(tgrowth)
    }
###
df_worldclim2 <- df_worldclim |>
  # get lon, lat
  # # derive (rounded) lon, lat from sitename
  # separate(
  #   sitename, into=c(NA, "lon", NA, "lat"),
  #   sep = "_", remove = FALSE, convert = TRUE) |> # re-append lon, lat dplyr::left_join(siteinfo, by = join_by(sitename)) |>
  left_join(select(siteinfo_all, sitename, lon, lat),
            by = join_by(sitename)) |>
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
    # doy             = lubridate::make_date(1970, month, 15) |> lubridate::yday(),
    doy             = lubridate::yday(date),
    # tgrowth_degC    = ingestr::calc_tgrowth(tmin_degC,tmax_degC,lat=lat,doy=doy),
    tgrowth_degC   = myingestr_calc_tgrowth(tmin_degC,tmax_degC,lat=lat,doy=doy),

    # vpd:
    # variant 1:
    VPD_TminTmaxAVG_Pa = ingestr::calc_vpd(eact = vapr_kPa*1e3, tmin = tmin_degC, tmax = tmax_degC)
    # # variant 2: (Peng)
    # VPD_Tgrowth_Pa     = ingestr::calc_vpd(eact = vapr_kPa*1e3, tc = tgrowth_degC),
  )
# ggplot(df_worldclim2, aes(x=doy, y=lat, color = is.na(tgrowth_degC))) + geom_point()
# ggplot(df_worldclim2, aes(x=doy, y=lat, color = tgrowth_degC)) + geom_point()
stopifnot(0 == nrow(df_worldclim2 |> filter(is.na(tgrowth_degC)))) # assert tgrowth_degC correctly computed

# df_worldclim2 |> filter(is.na(tgrowth_degC))  |> group_by(sitename) |> slice(1)   # 0 sites have NA in tgrowth
# df_worldclim2 |> filter(!is.na(tgrowth_degC)) |> group_by(sitename) |> slice(1)   # 571 sites have at least 1 non-NA tgrowth

# temp:
# - growing season: mean across days for which disaggregated daily tmean > 0 deg C
# - daytime temperature: derived as a function of tmin and tmax, see equation 5 in Peng et al., 2023 (https://onlinelibrary.wiley.com/doi/abs/10.1111/1365-2745.14208)

# vpd:
# - vpd abgeleitet aus vapour pressure (Worldclim), gemäss code in ingestr für watch-wfdei
# - (vpd(tmin) + vpd(tmax))/2
# - average only over days with tavg > 0

# ppfd:
#   - aus solar radiation, multiplikation mit faktor (2....) gemäss anderen datenprodukten in ingestr

df_worldclim_agg <-
  df_worldclim2 |>
  group_by(sitename) |>
  # compute means across growing season (i.e. across months for which monthly tmean > 0 deg C)
  filter(growing_season) |>
  summarise(
    # with monthly data:
    # growing_season_months = paste0(month,collapse = ","),
    # growing_season_length = length(month),
    # with daily data:
    # growing_season_doys = paste0(doy,collapse = ","),
    growing_season_length = length(doy),

    temp = mean(tgrowth_degC),

    vpd = mean(VPD_TminTmaxAVG_Pa),
    # vpd2= mean(VPD_Tgrowth_Pa),

    ppfd=mean(ppfd_molm2s)
  )

# NOTE: 1 site is filtered out since it
# did return worldclim tavg_degC of 0 for all 12 months. Thus no growing_season.
# df_worldclim2 |>
#   group_by(sitename) |> filter(sum(growing_season) < 1)


      # # TODO: just checking for gpp sites: if ingestr-derived ppfd agrees with "rsofun_driver_data_v3.4.2.rds"
      # library(ggplot2)
      # df_gpp_forcingtarget
      # df_compare_forcing_FDK <-
      #   left_join(
      #     df_gpp_forcingtarget |> unnest(forcing) |>
      #       mutate(day   = lubridate::day(date),
      #              month = lubridate::month(date),
      #              year  = lubridate::year(date)) |>
      #       select(
      #         sitename,
      #         year,
      #         month,
      #         day,
      #         temp_FDK = temp,
      #         tmin_FDK = tmin,
      #         tmax_FDK = tmax,
      #         ppfd_FDK = ppfd,
      #         vpd_FDK  = vpd),
      #     df_worldclim2 |>
      #       filter(sitename %in% df_gpp_forcingtarget$sitename) |>
      #       select(
      #         sitename,
      #         month,
      #         temp_INGESTR = tavg_degC,
      #         # temp_INGESTR = tgrowth_degC, # either temp is strongly correlated
      #         tmin_INGESTR = tmin_degC,
      #         tmax_INGESTR = tmax_degC,
      #         ppfd_INGESTR = ppfd_molm2s,
      #         vpd_INGESTR  = VPD_TminTmaxAVG_Pa),
      #     by = join_by(sitename, month)
      #   ) |>
      #   pivot_longer(-c(sitename, year, month, day))
      # df_compare_forcing_FDK3 <- df_compare_forcing_FDK |>
      #   filter(day==15) |>
      #   separate(name, into = c("name", "source"), sep = "_") |>
      #   pivot_wider(names_from = source, values_from = value)
      # plot_comparison <- ggplot(df_compare_forcing_FDK3, aes(x=FDK, y=INGESTR)) + geom_point() + facet_wrap(~name, scales = "free") +
      #   geom_abline() + ggtitle("For VJ dataset:\nComparing ingestr vs vj climate")
      # plot_comparison # THIS LOOKS GOOD
      # # END TODO




## Subset vj and bigD13C sites ----
# - do not use croplands and wetlands
# - do not use

classes_to_remove_from_vj_bigD13C <- c(
  "Bare areas",
  "Cropland, irrigated or post-flooding",
  "Cropland, rainfed",
  "Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)",
  "Lichens and mosses",
  "Permanent snow and ice",
  "Water bodies",
  "Urban areas",
  "Tree cover, flooded, fresh or brackish water", "Tree cover, flooded, saline water", "Shrub or herbaceous cover, flooded, fresh/saline/brackish water"
)
siteinfo |>
  mutate(to_remove = Defourny_LCCS %in% classes_to_remove_from_vj_bigD13C) |>
  group_by(to_remove, Defourny_LCCS) |> summarise(n()) |> print(n=100)

sites_to_remove2 <- siteinfo |>
  mutate(to_remove = Defourny_LCCS %in% classes_to_remove_from_vj_bigD13C) |>
  filter(to_remove) |>
  magrittr::extract2("sitename")

      # gpp_sites_to_use <- fdk_site_info |>
      #   filter(!(igbp_land_use %in% c("CRO", "WET"))) |>
      #   left_join(
      #     fdk_site_fullyearsequence,
      #     by = "sitename"
      #   ) |>
      #   filter(nyears_gpp > 5)
      #
      # df_gpp_forcingtarget <- gpp_sites_to_use |>
      #   select(sitename, nyears_gpp,
      #          FDK_koeppen_code = koeppen_code, FDK_igbp_land_use = igbp_land_use,
      #          year_start_gpp, year_end_gpp) |>
      #   left_join(drivers, by = join_by(sitename)) |>
      #   # nest the additional columns into site_info
      #   unnest(site_info) |>
      #   nest(site_info = c(
      #     lon, lat, elv, whc, canopy_height, reference_height,
      #     nyears_gpp, FDK_koeppen_code, FDK_igbp_land_use, year_start_gpp, year_end_gpp)) |>
      #   select(sitename, params_siml, site_info, forcing)
      #
      #

# Combine and output

# since we did not get worldclim data for 10 sites
# and only 0 temperature for another 1 sites:
# `df_trait_forcing_filled` is missing these 11 sites
# anti_join(df_co2_agg, df_worldclim_agg)$sitename |> dput()
sites_to_remove1 <- c(
  "lon_+158.80_lat_-054.50", "lon_-041.75_lat_-022.38", "lon_-063.80_lat_-064.80",
  "lon_-063.82_lat_-064.82", "lon_-069.83_lat_+011.61", "lon_-072.38_lat_+078.53",
  "lon_-074.60_lat_+078.58", "lon_-075.92_lat_+078.88", "lon_-079.38_lat_+008.97",
  "lon_-086.70_lat_+076.35", "lon_+113.83_lat_+004.18"
)

sites_to_remove <- c(sites_to_remove1, sites_to_remove2)


df_trait_forcing_filled <-
  dplyr::full_join(df_etopo_agg |> nest(patm = c(elv_masl, patm_Pa)) |> filter(!(sitename %in% sites_to_remove)),
                    df_co2_agg  |> nest(co2  = c(co2_ppm))           |> filter(!(sitename %in% sites_to_remove)),
                    by = join_by(sitename)) |>
  dplyr::full_join(df_worldclim_agg |> nest(clim = c(growing_season_length,temp,vpd,ppfd)) |> filter(!(sitename %in% sites_to_remove)),
                    by = join_by(sitename)) |>
  # re-append lon, lat
  dplyr::left_join(siteinfo, by = join_by(sitename)) |>
  select(sitename, lon, lat, patm, co2, clim, Defourny_LCCS, Beck_KG)


df_trait_targets <-
  dplyr::full_join(
    df_bigD13C_target |> select(-c(lon,lat)) |> nest(bigD13C = -sitename) |> filter(!(sitename %in% sites_to_remove)),
    df_vj_target      |> select(-c(lon,lat)) |> nest(vj  = -sitename) |> filter(!(sitename %in% sites_to_remove)),
    by = join_by(sitename)) |>
  select(sitename, bigD13C, vj) |>
  ungroup() |>
  distinct()

# NOTE: ensure we have the same sites in
check2 <- anti_join(df_trait_forcing_filled, df_trait_targets, by = join_by(sitename)); stopifnot(0 == nrow(check2))
check1 <- anti_join(df_trait_targets, df_trait_forcing_filled, by = join_by(sitename)); stopifnot(0 == nrow(check1))

stopifnot(all(df_trait_targets$sitename == df_trait_forcing_filled$sitename)) # ensure same ordering

# write_rds(df_trait_forcing_filled, file = here::here("data/01_bigD13C-vj_forcing.rds"),
#         compress = "xz")
# write_rds(df_trait_targets,        file = here::here("data/01_bigD13C-vj_targets.rds"),
#         compress = "xz")


# Quality check derived data --------------------------------------------------------------

# Check growing season lengths
pl_grow_season <- rgeco:::plot_map_simpl() +
  geom_point(
    data    = unnest(df_trait_forcing_filled, clim),
    mapping = aes(lon, lat, color = growing_season_length)) +
  labs(color = "Length of\ngrowing\nseason\n(days)")
pl_grow_season
ggsave(here::here("fig/00_fig_B_append_climate_MaplengthGrowSeason.png"), pl_grow_season, width=7.2, height=3.6, units="in")


# Check all input data
plot_clim <- function(colname_to_plot, label = NULL){
  rgeco:::plot_map_simpl() +
    {if(!is.null(label)) labs(color = label) } +
    geom_point(
      data    = unnest(df_trait_forcing_filled, clim),
      mapping = aes(lon, lat, color = .data[[colname_to_plot]]))
}
p1 <- plot_clim("growing_season_length", "Length of\ngrowing\nseason\n(days)")
p2 <- plot_clim("ppfd", "PPFD\n(mol/m2/s)")
p3 <- plot_clim("temp", "Mean\ngrowth\ntemp\n(deg C)")
p4 <- plot_clim("vpd",  "VPD\nmean(\n  f(Tmin),\n  f(Tmax)\n)\n(Pa)")
pl_all_climate <-
  (p1+p2)/
  (p3+p4)
ggsave(here::here("fig/00_fig_C_append_climate_MapClimateVars.png"), pl_all_climate, width=7.2*2, height=3.6*2, units="in")

plot_hist <- function(colname_to_plot, label = NULL){
  unnest(df_trait_forcing_filled, c(clim,patm,co2)) |>
    ggplot(aes(x = .data[[colname_to_plot]])) +
    {if(!is.null(label)) labs(x = label) } +
    geom_histogram(bins = 30) + theme_bw()
}
p1 <- plot_hist("growing_season_length", "Length of growing season (days)")
p2 <- plot_hist("ppfd", "PPFD (mol/m2/s)")
p3 <- plot_hist("temp", "Mean growth temp (deg C)")
p4 <- plot_hist("vpd",  "VPD=mean(f(Tmin),f(Tmax)), (Pa)")
p5 <- plot_hist("lat", "Latitude (deg)")
p6 <- plot_hist("co2_ppm", "CO2 (ppm)")
p7 <- plot_hist("elv_masl", "Elevation (masl)")
p8 <- plot_hist("patm_Pa", "Atmospheric. pressure (Pa)")

p9  <- ggplot(unnest(df_trait_targets, c("bigD13C")), aes(x=bigD13C_obs_permil)) + geom_histogram(bins=30, fill = "green4") + theme_bw() + labs(x = "Observed bigD13C = Ci/Ca")
p10 <- ggplot(unnest(df_trait_targets, c("vj")),  aes(x=vj_obs__))  + geom_histogram(bins=30, fill = "green4") + theme_bw() + labs(x = "Observed VJ = VCmax/Jmax")
p11 <- ggplot(unnest(df_trait_targets, c("vj")),  aes(x=vcmax_obs_molm2s*10^6))  + geom_histogram(bins=30, fill = "red3") + theme_bw() + labs(x = "Observed VCmax (umol/m2/s)")
p12 <- ggplot(unnest(df_trait_targets, c("vj")),  aes(x=jmax_obs_molm2s *10^6))  + geom_histogram(bins=30, fill = "red3") + theme_bw() + labs(x = "Observed Jmax (umol/m2/s)")

pl_all_climate_and_target_hist <-
  (p1+p2)/
  (p3+p4)/
  (p5+p6)/
  (p7+p8)/
  (p9+p10)/
  (p11+p12)
ggsave(
  here::here("fig/00_fig_D_append_climate_HistOnestepSites.png"),
  pl_all_climate_and_target_hist, width=3.6 * 2, height=1.8 * 5, units="in")





######### TODO ###################
##################################
##################################

# VJ:
  # TODO: compare the climate data, derived here with the one provided in vj data set

# TODO: remove df_compare_forcing <- dplyr::inner_join(
# TODO: remove   # a) climate data derive above
# TODO: remove   df_trait_forcing_filled |>
# TODO: remove     unnest(c(patm, co2, clim)) |>
# TODO: remove     select(sitename,
# TODO: remove            elv_masl   = elv_masl,
# TODO: remove            patm_Pa    = patm_Pa,
# TODO: remove            co2_ppm    = co2_ppm,
# TODO: remove            temp_degC  = temp,
# TODO: remove            # temp2_degC_ingestr = temp2,
# TODO: remove            vpd_Pa     = vpd,
# TODO: remove            # vpd2_Pa_ingestr = vpd2,
# TODO: remove            ppfd_molm2s= ppfd),
# TODO: remove   # b) climate data provided in vj data set
# TODO: remove   df_vj_forcing |>
# TODO: remove     mutate(ppfd_molm2s = par_molm2s) |>
# TODO: remove     select(sitename, elv_masl, patm_Pa, co2_ppm, temp_degC, vpd_Pa, ppfd_molm2s),
# TODO: remove   by = join_by(sitename),
# TODO: remove   suffix = c("_ingestr", "_vjdata")
# TODO: remove   )
# TODO: remove
# TODO: remove library(ggplot2)
# TODO: remove plot_comparison <- df_compare_forcing |>
# TODO: remove   pivot_longer(-sitename) |>
# TODO: remove   separate(name, into = c("name", "source"), sep = "_(?=[^_]+$)") |>
# TODO: remove   pivot_wider(names_from = source, values_from = value) |>
# TODO: remove   # filter(name %in% c("temp_degC","temp2_degC","vpd_Pa","vpd2_Pa")) |>
# TODO: remove   # filter(is.na(vjdata))
# TODO: remove   tidyr::fill(vjdata) |> # fills up the alternative versions of vpd2 and temp2
# TODO: remove   ggplot(aes(x=vjdata, y=ingestr)) + geom_point() + facet_wrap(~name, scales = "free") +
# TODO: remove   geom_abline() + ggtitle("For VJ dataset:\nComparing ingestr vs vj climate")
# TODO: remove plot_comparison
# TODO: remove ggsave(here::here("fig/fig_04_append_climate.png"), plot_comparison, width=7.2, height=4.8, units="in")
# TODO: remove This shows a mismatch between ppfd


# TODO: future work:
# - assess differences between ppfd_vj and ppfd_ingestr, there appears to be a factor of 2.0
# - assess impact of two different VPD computations (f(tmax)+f(tmin))/2 vs f(tgrowth)
# - could be using CO2_ppm from VJ data set to derive collection.year of VJ measurements
# - could be using monthly (CRU or Worldclim) meteo data for the given year, instead of 1970-2000 climate average
# - reformat 'df_trait_forcing_filled' similarly to read_rds("/data_2/FluxDataKit/v3.4/zenodo_upload/rsofun_driver_data_v3.4.2.rds")










# Combine with daily forcing data for gpp --------------------------------------------------------------

## Load bigD13C, vj data ----
# bigD13C_vj_forcing <- read_rds(file = here::here("data/01_bigD13C-vj_forcing.rds"))
# bigD13C_vj_targets <- read_rds(file = here::here("data/01_bigD13C-vj_targets.rds"))
bigD13C_vj_forcing <- df_trait_forcing_filled
bigD13C_vj_targets <- df_trait_targets

## Load gpp data ----
gpp_forcingtarget <- read_rds(file = here::here("data/00_gpp_forcingtarget.rds"))


## Prepare bigD13C, vj, gpp data (drivers and targets) ----
# format bigD13C_vj_forcing similarly to rsofun::p_model_drivers
bigD13C_vj_drivers <- bigD13C_vj_forcing |>
  unnest(c(patm, co2, clim)) |>
  ## 1) nest forcing
  select(sitename,
         lon, lat, elv_masl,
         temp, vpd, ppfd, co2 = co2_ppm, patm = patm_Pa,
         Defourny_LCCS, Beck_KG) |>
  nest(forcing = c(temp, vpd, ppfd, co2, patm)) |>
  ## 2) nest params_siml
  mutate(lc4 = FALSE) |>
  nest(params_siml = c(lc4)) |>
  ## 3) nest site_info
  rename(elv = elv_masl) |>
  mutate(whc = NA) |>
  nest(site_info = c(lon, lat, elv, whc, Defourny_LCCS, Beck_KG)) |>
  ## 4) add additional info
  mutate(run_model = "onestep") |> # either "onestep" or "daily"
  # order
  select(sitename, run_model, params_siml, site_info, forcing)

# format bigD13C_vj_targets similarly to bind_rows(rsofun::p_model_validation, rsofun::p_model_validation_vcmax25)
bigD13C_vj_obs <- bigD13C_vj_targets |>
  ## 1) add additional info
  rowwise() |> mutate(
    run_model   = "onestep",
    targets = list(list(
      vj      = !is.null(vj),
      bigD13C = !is.null(bigD13C),
      gpp     = FALSE
    ))
  ) |>
  ## 2) nest data
  ungroup() |>
  nest(data = c(bigD13C,vj)) |>
  # order
  select(sitename, run_model, targets, data)

# format gpp_forcingtarget similarly to rsofun::p_model_drivers
gpp_drivers <- gpp_forcingtarget |>
  mutate(run_model = "daily") |> # either "onestep" or "daily"
  # order
  select(sitename, run_model, params_siml, site_info, forcing) |>
  # reduce columns in forcing:
  mutate(forcing = purrr::map(forcing, \(nested_forcing_df){
    nested_forcing_df |>
      select(date, temp, vpd, ppfd, netrad, patm, snow, rain, tmin, tmax, vwind, fapar, co2, ccov)
      # thereby unselecting: select(-c(gpp, gpp_qc, nee, nee_qc, le, le_qc))
  }))

# format gpp_forcingtarget similarly to bind_rows(rsofun::p_model_validation, rsofun::p_model_validation_vcmax25)
gpp_obs_nonQC <- gpp_forcingtarget |>
  select(sitename, forcing) |> unnest(forcing) |>
  select(sitename, date, gpp, gpp_qc, nee, nee_qc, le, le_qc) |>
  nest(data = -c(sitename)) |>
  mutate(run_model = "daily", # either "onestep" or "daily"
         targets = list(list(
           vj      = FALSE,
           bigD13C = FALSE,
           gpp     = TRUE
         ))) |>
  # order
  select(sitename, run_model, targets, data)
gpp_obs <- gpp_obs_nonQC |>
  # remove rows corresponding to low quality gpp observations
  # previously low quality gpp were overwritten with NA
  # since we couldn't remove the rows due to the forcing
  # Now that this is split we can remove those rows
  mutate(data = purrr::map(data, \(nested_forcing_df){
    nested_forcing_df |> filter(!is.na(gpp))
  }))

# Visualize QC operation:
# pl_QC_a <- gpp_forcingtarget |> unnest(forcing) |>
#   group_by(sitename) |> filter(any(is.na(gpp))) |>
#   mutate(flag = case_when(gpp_qc<0.8~"low_quality",
#                           is.na(gpp)~"NA",
#                           TRUE~"good_quality")) |>
#   ggplot(aes(x=date,y=sitename, color = flag)) + geom_point() +
#   theme_classic()
pl_QC_b <- gpp_obs_nonQC |> unnest(data) |>
  group_by(sitename) |> filter(any(is.na(gpp))) |>
  mutate(flag = case_when(gpp_qc<0.8~"low_quality",
                          is.na(gpp)~"NA",
                          TRUE~"good_quality")) |>
  ggplot(aes(x=date,y=sitename, color = flag)) + geom_point() +
  theme_classic()
# gpp_obs |> unnest(data) |>
#   group_by(sitename) |> filter(any(is.na(gpp))) |>
#   mutate(flag = case_when(gpp_qc<0.8~"low_quality",
#                           is.na(gpp)~"NA",
#                           TRUE~"good_quality")) |>
#   ggplot(aes(x=date,y=sitename, color = flag)) + geom_point() +
#   theme_classic()
stopifnot(
  gpp_obs |>
    unnest(data) |>
    group_by(sitename) |>
    filter(any(is.na(gpp))) |>
    nrow() == 0)

ggsave(here::here("fig/00_fig_E_append_climate_GPP-QC.png"),
       pl_QC_b, width=7.2, height=7.2, units="in", scale = 1.5)




## Combine bigD13C, vj, gpp data (drivers and targets) ----
bigD13C_vj_gpp_drivers <- bind_rows(gpp_drivers, bigD13C_vj_drivers)
bigD13C_vj_gpp_obs     <- bind_rows(gpp_obs, bigD13C_vj_obs)
# bigD13C_vj_gpp_drivers_obs <- dplyr::inner_join( # TODO: check if this is computationally more efficient for calib_sofun()
#   bigD13C_vj_gpp_drivers,
#   bigD13C_vj_gpp_obs,
#   by = join_by(sitename, run_model))

write_rds(
  bigD13C_vj_gpp_drivers,
  file = here::here("data/01_bigD13C-vj-gpp_calibsofun_drivers.rds"),
  compress = "xz")

write_rds(
  bigD13C_vj_gpp_obs,
  file = here::here("data/01_bigD13C-vj-gpp_calibsofun_obs.rds"),
  compress = "xz")




## Plot the available data
bigD13C_vj_gpp_obs |> unnest_wider(targets) |> filter(vj)
bigD13C_vj_gpp_obs |> unnest_wider(targets) |> filter(bigD13C)
bigD13C_vj_gpp_obs |> unnest_wider(targets) |> filter(gpp)

coordinates <- bigD13C_vj_gpp_drivers |> unnest(site_info) |> select(sitename, lon,lat,elv)

pl1 <- rgeco:::plot_map_simpl() +
  geom_point(size=0.1,shape = 20,
    data    = bigD13C_vj_gpp_obs |> unnest_wider(targets) |>
      filter(vj) |>
      left_join(coordinates),
    mapping = aes(lon, lat)) + ggtitle("Vcmax/Jmax sites")
pl2 <- rgeco:::plot_map_simpl() +
  geom_point(size=0.1,shape = 20,
    data    = bigD13C_vj_gpp_obs |> unnest_wider(targets) |>
      filter(bigD13C) |>
      left_join(coordinates),
    mapping = aes(lon, lat)) + ggtitle("Δ13C sites")
pl3 <- rgeco:::plot_map_simpl() +
  geom_point(size=0.1,shape = 20,
    data    = bigD13C_vj_gpp_obs |> unnest_wider(targets) |>
      filter(gpp) |>
      left_join(coordinates),
    mapping = aes(lon, lat)) + ggtitle("GPP flux sites")

pl_targets <-
  (pl1 + theme(axis.text.x = element_blank()))/
  (pl2 + theme(axis.text.x = element_blank()))/
  pl3
ggsave(
  here::here("fig/00_fig_F_append_climate_MapTargetSites.png"),
  pl_targets, width=3.6, height=1.8 * 3, units="in")
