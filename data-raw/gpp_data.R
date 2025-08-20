# This script prepares the input forcing and output target tibble
# ('df_gpp_forcingtarget') needed for simulating daily GPP with P-model.
#
# This object is stored as *.rds file in subfolder data/

library(tidyr)
library(dplyr)
library(zen4R)
library(readr)
library(here)
library(lubridate)

## Load gpp data ----
#   Download FluxDataKit data from Zenodo:
#   sudo apt install librdf0-dev
#   install.packages("zen4R")
download_path <- tempdir(check = TRUE)
download_zenodo(path = download_path, "10.5281/zenodo.14808331", files = "fdk_site_info.csv")
download_zenodo(path = download_path, "10.5281/zenodo.14808331", files = "fdk_site_fullyearsequence.csv")
download_zenodo(path = download_path, "10.5281/zenodo.14808331", files = "rsofun_driver_data_v3.4.2.rds")
fdk_site_info             <- readr::read_csv(file.path(download_path, "fdk_site_info.csv"))
fdk_site_fullyearsequence <- readr::read_csv(file.path(download_path, "fdk_site_fullyearsequence.csv"))
drivers <- read_rds(file.path(download_path, "rsofun_driver_data_v3.4.2.rds"))

# df_old <- read_rds("/data_2/FluxDataKit/v3.4/zenodo_upload/rsofun_driver_data_v3.4.2.rds")
# df_new <- read_rds(file.path(download_path, "rsofun_driver_data_v3.4.2.rds"))
# testthat::expect_equal(df_old,df_new)


## Subset gpp sites ----
# - do not use croplands and wetlands
# - do only use sites with at least 5 years for training and testing (see "analysis/01_subset_test_train_sites.R")

gpp_sites_to_use <- fdk_site_info |>
  filter(!(igbp_land_use %in% c("CRO", "WET"))) |>
  left_join(
    fdk_site_fullyearsequence,
    by = "sitename"
  ) |>
  filter(nyears_gpp > 5)

df_gpp_forcingtarget <- gpp_sites_to_use |>
  select(sitename, nyears_gpp,
         FDK_koeppen_code = koeppen_code, FDK_igbp_land_use = igbp_land_use,
         year_start_gpp, year_end_gpp) |>
  left_join(drivers, by = join_by(sitename)) |>
  # nest the additional columns into site_info
  unnest(site_info) |>
  nest(site_info = c(
    lon, lat, elv, whc, canopy_height, reference_height,
    nyears_gpp, FDK_koeppen_code, FDK_igbp_land_use, year_start_gpp, year_end_gpp)) |>
  select(sitename, params_siml, site_info, forcing)


## Quality check gpp data ----
# NOTE that based on: https://github.com/geco-bern/FluxDataKit/blob/43fb25847bb99ab8b02abf943fa82933c5a5315b/R/fdk_format_drivers.R#L118
#     gpp = GPP_NT_VUT_REF,    i.e. "Gross Primary Production, from Nighttime partition ing method, reference selected from GPP versions using model efficiency (MEF). The MEF analysis is repeated for each time aggregation" https://fluxnet.org/data/fluxnet2015-dataset/fullset-data-product/
#     gpp_qc = NEE_VUT_REF_QC, i.e. "fraction between 0-1, indicating percentage of me asured and good quality gapfill data" https://fluxnet.org/data/fluxnet2015-dataset/fullset-data-product/

# only keep years with good quality data
df_gpp_forcingtarget_cropped <- df_gpp_forcingtarget |>
  unnest(c(site_info, forcing)) |>
  mutate(year = year(date)) |>
  filter(year >= year_start_gpp & year <= year_end_gpp) |>
  select(-year_start_gpp, -year_end_gpp, -year) |>
  nest(forcing = 'date':'le_qc') |>
  nest(site_info = 'lon':'FDK_igbp_land_use') |>
  select(sitename, params_siml, site_info, forcing)


# overwrite remaining low quality gpp (QC < 0.8) with NAs
# NOTE: since forcing and target observations are combined, we cannot remove
#       the corresponding days from the data.frame(). Thus we overwrite with NA.
df_gpp_forcingtarget_cropped_qc <- df_gpp_forcingtarget_cropped |>
  mutate(forcing = purrr::map(forcing, \(nested_df){
    mutate(nested_df, gpp = ifelse(gpp_qc >= 0.8, gpp, NA_real_))
  }))



### Verify issues visually: ----
# # drivers |> filter(sitename == "GF-Guy") |> unnest(forcing)
# # df_old |> filter(sitename == "GF-Guy") |> unnest(forcing)
# df_gpp_forcingtarget |> unnest(forcing) |>
#   group_by(sitename) |> filter(any(is.na(gpp))) |>
#   ggplot(aes(x=date,y=sitename, color = is.na(gpp))) + geom_point() + # TODO: discuss issue
#   theme_classic()
# df_gpp_forcingtarget_cropped |> unnest(forcing) |>
#   group_by(sitename) |> filter(any(is.na(gpp))) |>
#   ggplot(aes(x=date,y=sitename, color = is.na(gpp))) + geom_point() + # TODO: discuss issue
#   theme_classic() # cropping helped a bit
# df_gpp_forcingtarget_cropped_qc |> unnest(forcing) |>
#   group_by(sitename) |> filter(any(is.na(gpp))) |>
#   ggplot(aes(x=date,y=sitename, color = is.na(gpp))) + geom_point() + # TODO: discuss issue
#   theme_classic()




write_rds(df_gpp_forcingtarget_cropped_qc,
          here::here("data/00_gpp_forcingtarget.rds"),
          compress = "xz")

