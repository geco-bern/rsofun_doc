library(tidyverse)
library(readr)
library(FluxDataKit)
# remotes::install_github("geco-bern/rgeco")
library(rgeco)
# library(plotbiomes)

# Sample sites -----------------------------------------------------------------
df <- fdk_site_info |>
  left_join(
    fdk_site_fullyearsequence,
    by = "sitename"
  )

df |>
  ggplot(aes(nyears_gpp)) +
  geom_histogram()

df_train <- df |>
  filter(nyears_gpp > 15 & !(igbp_land_use %in% c("CRO", "WET"))) |>
  # mutate(p_over_pet = as.numeric(p_over_pet)) |>
  # mutate(
  #   whc_bin = cut(whc, breaks = quantile(whc, probs = seq(0, 1, 1/3)), include.lowest = TRUE),
  #   mat_bin = cut(mat, breaks = quantile(mat, probs = seq(0, 1, 1/3)), include.lowest = TRUE),
  #   mi_bin = cut(p_over_pet, breaks = quantile(p_over_pet, probs = seq(0, 1, 1/3)), include.lowest = TRUE)
  # ) |>
  # mutate(strata = interaction(whc_bin, mat_bin, mi_bin, igbp_land_use, drop = TRUE))
  mutate(strata = interaction(koeppen_code, igbp_land_use, drop = TRUE))

set.seed(1982)

df_sampled_train <- df_train |>
  group_by(strata) |>
  sample_n(size = 1, replace = FALSE) |>
  ungroup()

View(df_sampled_train)

write_rds(df_sampled_train, file = here::here("data/df_sampled_train.rds"))
write_csv(
  df_sampled_train |>
    mutate(
      lon = round(lon, 2),
      lat = round(lat, 2)
    ) |>
    select(
      Site = sitename,
      Long. = lon,
      Lat. = lat,
      Elevation = elv,
      `Climate zone` = koeppen_code,
      `Vegetation type` = igbp_land_use,
      `Year start` = year_start_gpp,
      `Year end` = year_end_gpp
    ),
  file = here::here("data/df_sampled_train.csv")
  )

## Test sites ------------------------------------------------------------------
df_test <- df |>
  filter(!(sitename %in% df_sampled_train$sitename)) |>
  filter(nyears_gpp > 5 & !(igbp_land_use %in% c("CRO", "WET"))) |>
  # mutate(p_over_pet = as.numeric(p_over_pet)) |>
  # mutate(
  #   whc_bin = cut(whc, breaks = quantile(whc, probs = seq(0, 1, 1/3)), include.lowest = TRUE),
  #   mat_bin = cut(mat, breaks = quantile(mat, probs = seq(0, 1, 1/3)), include.lowest = TRUE),
  #   mi_bin = cut(p_over_pet, breaks = quantile(p_over_pet, probs = seq(0, 1, 1/3)), include.lowest = TRUE)
  # ) |>
  # mutate(strata = interaction(whc_bin, mat_bin, mi_bin, igbp_land_use, drop = TRUE))
  mutate(strata = interaction(koeppen_code, igbp_land_use, drop = TRUE))

set.seed(1982)

df_sampled_test <- df_test |>
  group_by(strata) |>
  sample_n(size = 1, replace = FALSE) |>
  ungroup()

## Inspect sample --------------------------------------------------------------
View(df_sampled_train)
View(df_sampled_test)

# plot_map_simpl() +
#   geom_point(
#     aes(lon, lat, color = group),
#     data = bind_rows(
#       df_sampled_test |>
#         mutate(group = "test"),
#       df_sampled_train |>
#         mutate(group = "train")
#     )
#   )
#
# plotbiomes::whittaker_base_plot() +
#   geom_point(
#     aes(mat, map/10),
#     data = df_sampled
#   )

# Subset data ------------------------------------------------------------------
drivers <- read_rds("~/data_2/FluxDataKit/v3.4/zenodo_upload/rsofun_driver_data_v3.4.2.rds")

# only sampled sites
drivers_train <- drivers |>
  filter(sitename %in% df_sampled_train$sitename)

# only years with good quality data
forcing_train <- drivers_train |>
  select(sitename, forcing) |>
  unnest(forcing) |>
  left_join(
    df_sampled_train |>
      select(
        sitename,
        year_start = year_start_gpp,
        year_end = year_end_gpp),
    by = join_by(sitename)
  ) |>
  mutate(year = year(date)) |>
  filter(year >= year_start & year <= year_end) |>
  select(-year_start, -year_end, -year) |>
  group_by(sitename) |>
  nest() |>
  rename(forcing = data)

drivers_train <- drivers_train |>
  select(-forcing) |>
  left_join(
    forcing_train,
    by = "sitename"
  )

## Write to file ---------------------------------------------------------------
write_rds(drivers_train, file = here::here("data/drivers_train.rds"))




