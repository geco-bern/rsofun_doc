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
# drivers |> filter(sitename == "GF-Guy") |> unnest(forcing)
# df_old |> filter(sitename == "GF-Guy") |> unnest(forcing)
# library(ggplot2)
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

      # pl_timeseries_gpp <- ggplot(
      #   data = df_gpp_forcingtarget_cropped_qc |> unnest(forcing),
      #   mapping = aes(x=date, y=gpp)) +
      #   # Observations underneath (following Cameron 2022)
      #   geom_point(shape = 4, alpha= 0.5, size=0.5) +
      #   # layout
      #   facet_wrap(~sitename, scales = "free_x", ncol=3) +
      #   theme_classic() + theme(legend.position = "bottom") +
      #   labs( x = NULL, y = expression(paste("GPP (g C m"^-2, "s"^-1, ")")) ) +
      #   scale_x_date(date_breaks = "12 months", date_labels = "%Y-%m")
      #
      # plot_group <- 15
      # df_to_plot_for_check <- df_gpp_forcingtarget_cropped_qc |> ungroup() |>
      #   mutate(plotting_group=(1:n()) %/% plot_group + 1) |>
      #   unnest(forcing) |>
      #   # add visual QC:
      #   mutate(visual_qc_flag = case_when(
      #     sitename == "US-Whs" & date >= ymd("2016-01-01") & date <= ymd("2017-12-31") ~ "drop",
      #     sitename == "US-Ho2" & date >= ymd("2007-01-01") & date <= ymd("2007-12-31") ~ "drop",
      #     sitename == "ES-LJu" & date >= ymd("2006-01-01") & date <= ymd("2006-12-31") ~ "drop",
      #     sitename == "CH-Dav" & date >= ymd("2010-01-01") & date <= ymd("2010-12-31") ~ "drop",
      #     TRUE ~ "keep"
      #   ))
      # df_to_plot_for_check |> filter("drop" == visual_qc_flag)
      # lst_df_for_check <- df_to_plot_for_check |> group_split(plotting_group)
      # for (it in seq_along(lst_df_for_check)){
      #   ggsave(
      #     sprintf("fig/00_gpp_forcingtarget_qc_%d.png", it),
      #     plot = pl_timeseries_gpp %+% lst_df_for_check[[it]] +
      #       aes(color = visual_qc_flag) + scale_color_manual(values = c("drop" = "red", "keep" = "black")),
      #     width = 36, height = 20, units = "cm", dpi = 300, scale = 2
      #     )
      # }

# overwrite visually detected gpp with NAs
df_gpp_forcingtarget_cropped_qc2 <- df_gpp_forcingtarget_cropped_qc |>
  unnest(forcing) |>
  mutate(gpp = case_when(
    sitename == "US-Whs" & date >= ymd("2016-01-01") & date <= ymd("2017-12-31") ~ NA_real_,
    sitename == "US-Ho2" & date >= ymd("2007-01-01") & date <= ymd("2007-12-31") ~ NA_real_,
    sitename == "ES-LJu" & date >= ymd("2006-01-01") & date <= ymd("2006-12-31") ~ NA_real_,
    sitename == "CH-Dav" & date >= ymd("2010-01-01") & date <= ymd("2010-12-31") ~ NA_real_,
    TRUE ~ gpp)) |>
  nest(forcing = 'date':'le_qc')

# check confirms correct removal:
# pl_timeseries_gpp %+% unnest(filter(df_gpp_forcingtarget_cropped_qc2, sitename %in% c("US-Whs","US-Ho2","ES-LJu","CH-Dav")), forcing)



write_rds(df_gpp_forcingtarget_cropped_qc2,
          here::here("data/00_gpp_forcingtarget.rds"),
          compress = "xz")

