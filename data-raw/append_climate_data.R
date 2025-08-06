# This script appends climate data to the the input forcings for
# 'df_chi_forcing' and 'df_vj_forcing' by using the {ingestr} package.
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
# pak::pkg_install("geco-bern/rsofun@simple_pmodel_v2")
# library(rsofun)  # install from branch simple_pmodel_v2
# pak::pkg_install("geco-bern/ingestr")
library(ingestr)



# Load data --------------------------------------------------------------------
df_chi_forcing <- read_rds(here::here("data/00_chi_forcing.rds"))
df_vj_forcing  <- read_rds(here::here("data/00_vj_forcing.rds"))

df_chi_target <- read_rds(here::here("data/00_chi_target.rds"))
df_vj_target  <- read_rds(here::here("data/00_vj_target.rds"))

      # # TODO: just checking for gpp sites: if ingestr-derived ppfd agrees with "rsofun_driver_data_v3.4.2.rds"
      #   NOTE: we normally do this later when combining the forcing data for the chi and vj data with the gpp data
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
siteinfo_all <- bind_rows(df_chi_forcing, df_vj_forcing) |>
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
df_worldclim <- df_worldclim_raw |> unnest(c(data)) |>
  filter(!if_any(everything(), is.na)) |>
  nest(data = -c(sitename))


# TODO: instead of 1970-2000 average from worldclim, we might use the actual
#       values of the actual years from worldclim?


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
    doy             = lubridate::make_date(1970, month, 15) |> lubridate::yday(),
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
# df_worldclim2 |> filter(!is.na(tgrowth_degC)) |> group_by(sitename) |> slice(1)   # 585 sites have at least 1 non-NA tgrowth

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
    # growing_season_length2= n(), # NOTE: this was for double checking

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

# Combine and output

# since we did not get worldclim data for 10 sites
# and only 0 temperature for another 1 sites:
# `df_trait_forcing_filled` is missing these 11 sites
# anti_join(df_co2_agg, df_worldclim_agg)$sitename |> dput()
sites_to_remove <- c(
  "lon_+158.80_lat_-054.50", "lon_-041.75_lat_-022.38", "lon_-063.80_lat_-064.80",
  "lon_-063.82_lat_-064.82", "lon_-069.83_lat_+011.61", "lon_-072.38_lat_+078.53",
  "lon_-074.60_lat_+078.58", "lon_-075.92_lat_+078.88", "lon_-079.38_lat_+008.97",
  "lon_-086.70_lat_+076.35", "lon_+113.83_lat_+004.18"
)
sites_to_remove

df_trait_forcing_filled <-
  dplyr::full_join(df_etopo_agg |> nest(patm = c(elv_masl, patm_Pa)) |> filter(!(sitename %in% sites_to_remove)),
                    df_co2_agg  |> nest(co2  = c(co2_ppm))           |> filter(!(sitename %in% sites_to_remove)),
                    by = join_by(sitename)) |>
  dplyr::full_join(df_worldclim_agg |> nest(clim = c(growing_season_months,growing_season_length,temp,vpd,ppfd)),
                    by = join_by(sitename)) |>
  # re-append lon, lat
  dplyr::left_join(siteinfo, by = join_by(sitename)) |>
  select(sitename, lon, lat, patm, co2, clim)


df_trait_targets <-
  dplyr::full_join(
    df_chi_target |> select(-c(lon,lat)) |> nest(chi = -sitename) |> filter(!(sitename %in% sites_to_remove)),
    df_vj_target  |> select(-c(lon,lat)) |> nest(vj  = -sitename) |> filter(!(sitename %in% sites_to_remove)),
    by = join_by(sitename)) |>
  select(sitename, chi, vj) |>
  ungroup() |>
  distinct()

# NOTE: ensure we have the same sites in
check2 <- anti_join(df_trait_forcing_filled, df_trait_targets, by = join_by(sitename)); stopifnot(0 == nrow(check2))
check1 <- anti_join(df_trait_targets, df_trait_forcing_filled, by = join_by(sitename)); stopifnot(0 == nrow(check1))

stopifnot(all(df_trait_targets$sitename == df_trait_forcing_filled$sitename)) # ensure same ordering

# write_rds(df_trait_forcing_filled, file = here::here("data/01_chi-vj_forcing.rds"),
#         compress = "xz")
# write_rds(df_trait_targets,        file = here::here("data/01_chi-vj_targets.rds"),
#         compress = "xz")


# Quality check derived data --------------------------------------------------------------

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
p4 <- plot_clim("vpd",  "VPD\nmean(\n  f(Tmin),\n  f(Tmax)\n)\n(Pa)")
pl_all_climate <-
  (p1+p2)/
  (p3+p4)
ggsave(here::here("fig/fig_02_append_climate.png"), pl_all_climate, width=7.2*2, height=3.6*2, units="in")

plot_hist <- function(colname_to_plot, label = NULL){
  unnest(df_trait_forcing_filled, c(clim,patm,co2)) |>
    ggplot(aes(x = .data[[colname_to_plot]])) +
    {if(!is.null(label)) labs(x = label) } +
    geom_histogram(bins = 30) + theme_bw()
}
p1 <- plot_hist("growing_season_length", "Length of growing season (months)")
p2 <- plot_hist("ppfd", "PPFD (mol/m2/s)")
p3 <- plot_hist("temp", "Mean growth temp (deg C)")
p4 <- plot_hist("vpd",  "VPD=mean(f(Tmin),f(Tmax)), (Pa)")
p5 <- plot_hist("lat", "Latitude (deg)")
p6 <- plot_hist("co2_ppm", "CO2 (ppm)")
p7 <- plot_hist("elv_masl", "Elevation (masl)")
p8 <- plot_hist("patm_Pa", "Atmospheric. pressure (Pa)")

p9  <- ggplot(unnest(df_trait_targets, c("chi")), aes(x=chi_obs__)) + geom_histogram(bins=30, fill = "green4") + theme_bw() + labs(x = "Observed chi = Ci/Ca")
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
  here::here("fig/fig_03_append_climate.png"),
  pl_all_climate_and_target_hist, width=3.6 * 2, height=1.8 * 5, units="in")







######### TODO ###################
##################################
##################################

# VJ:
  # NOTE: potentially replace forcing data with data from ingestr to homogenize with chi data.
  #       however, we do not have the time information
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

## Load chi, vj data ----
# chi_vj_forcing <- read_rds(file = here::here("data/01_chi-vj_forcing.rds"))
# chi_vj_targets <- read_rds(file = here::here("data/01_chi-vj_targets.rds"))
chi_vj_forcing <- df_trait_forcing_filled
chi_vj_targets <- df_trait_targets

## Load gpp data ----
gpp_forcingtarget <- read_rds(file = here::here("data/00_gpp_forcingtarget.rds"))



## Prepare chi, vj, gpp data (drivers and targets) ----
# format chi_vj_forcing similarly to rsofun::p_model_drivers
chi_vj_drivers <- chi_vj_forcing |>
  unnest(c(patm, co2, clim)) |>
  ## 1) nest forcing
  select(sitename,
         lon, lat, elv_masl,
         temp, vpd, ppfd, co2 = co2_ppm, patm = patm_Pa) |>
  #nest(forcing = -c(sitename))
  nest(forcing = c(temp, vpd, ppfd, co2, patm)) |>
  ## 2) nest params_siml
  mutate(lc4 = FALSE) |>
  nest(params_siml = c(lc4)) |>
  ## 3) nest site_info
  rename(elv = elv_masl) |>
  mutate(whc = NA) |>
  nest(site_info = c(lon, lat, elv, whc)) |>
  ## 4) add additional info
  mutate(run_model = "onestep") |> # either "onestep" or "daily"
  # order
  select(sitename, run_model, params_siml, site_info, forcing)

# format chi_vj_targets similarly to bind_rows(rsofun::p_model_validation, rsofun::p_model_validation_vcmax25)
chi_vj_obs <- chi_vj_targets |>
  ## 1) add additional info
  rowwise() |> mutate(
    run_model   = "onestep",
    targets = list(list(
      vj  = is.null(vj),
      chi = is.null(chi),
      gpp = FALSE
    ))
  ) |>
  ## 2) nest data
  ungroup() |>
  nest(data = c(chi,vj)) |>
  # order
  select(sitename, run_model, targets, data)

# format gpp_forcingtarget similarly to rsofun::p_model_drivers
gpp_drivers <- gpp_forcingtarget |>
  mutate(run_model = "daily") |> # either "onestep" or "daily"
  # order
  select(sitename, run_model, params_siml, site_info, forcing)

# format gpp_forcingtarget similarly to bind_rows(rsofun::p_model_validation, rsofun::p_model_validation_vcmax25)
gpp_obs <- gpp_forcingtarget |>
  select(sitename, forcing) |> unnest(forcing) |>
  select(sitename, date, gpp, gpp_qc, nee, nee_qc, le, le_qc) |>
  nest(data = -c(sitename)) |>
  mutate(run_model = "daily", # either "onestep" or "daily"
         targets = list(list(
           vj  = FALSE,
           chi = FALSE,
           gpp = TRUE
         ))) |>
  # order
  select(sitename, run_model, targets, data)


## Combine chi, vj, gpp data (drivers and targets) ----
chi_vj_gpp_drivers <- bind_rows(gpp_drivers, chi_vj_drivers)
chi_vj_gpp_obs     <- bind_rows(gpp_obs, chi_vj_obs)
# chi_vj_gpp_drivers_obs <- dplyr::inner_join( # TODO: check if this is computationally more efficient for calib_sofun()
#   chi_vj_gpp_drivers,
#   chi_vj_gpp_obs,
#   by = join_by(sitename, run_model))

write_rds(
  chi_vj_gpp_drivers,
  file = here::here("data/01_chi-vj-gpp_calibsofun_drivers.rds"),
  compress = "xz")

write_rds(
  chi_vj_gpp_obs,
  file = here::here("data/01_chi-vj-gpp_calibsofun_obs.rds"),
  compress = "xz")




## Plot the available data
chi_vj_gpp_obs |> unnest_wider(targets) |> filter(vj)
chi_vj_gpp_obs |> unnest_wider(targets) |> filter(chi)
chi_vj_gpp_obs |> unnest_wider(targets) |> filter(gpp)

coordinates <- chi_vj_gpp_drivers |> unnest(site_info) |> select(sitename, lon,lat,elv)

pl1 <- rgeco:::plot_map_simpl() +
  geom_point(size=0.1,shape = 20,
    data    = chi_vj_gpp_obs |> unnest_wider(targets) |>
      filter(vj) |>
      left_join(coordinates),
    mapping = aes(lon, lat)) + ggtitle("V/J sites")
pl2 <- rgeco:::plot_map_simpl() +
  geom_point(size=0.1,shape = 20,
    data    = chi_vj_gpp_obs |> unnest_wider(targets) |>
      filter(chi) |>
      left_join(coordinates),
    mapping = aes(lon, lat)) + ggtitle("Ci/Ca sites")
pl3 <- rgeco:::plot_map_simpl() +
  geom_point(size=0.1,shape = 20,
    data    = chi_vj_gpp_obs |> unnest_wider(targets) |>
      filter(gpp) |>
      left_join(coordinates),
    mapping = aes(lon, lat)) + ggtitle("GPP flux sites")
pl_targets <-
  (pl1 + theme(axis.text.x = element_blank()))/
  (pl2 + theme(axis.text.x = element_blank()))/
  pl3

ggsave(
  here::here("fig/fig_05_append_climate.png"),
  pl_targets, width=3.6, height=1.8 * 3, units="in")
