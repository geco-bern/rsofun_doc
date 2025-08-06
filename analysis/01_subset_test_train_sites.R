# TODO: rewrite this to replace 01_sample_sites.R

gpp_forcingtarget <- read_rds(here::here("data/00_gpp_forcingtarget.rds"))

# # only keep years with good quality data
# drivers_train <- gpp_forcingtarget |>
#   unnest(forcing) |>
#   mutate(year = year(date)) |>
#   filter(year >= year_start_gpp & year <= year_end_gpp) |>
#   select(-year_start_gpp, -year_end_gpp, -year) |>
#   nest(forcing = 'date':'le_qc')










library(tidyverse)
library(readr)
# pak::pkg_install("geco-bern/FluxDataKit")
library(FluxDataKit)
# pak::pkg_install("geco-bern/rgeco")
library(rgeco)
# library(plotbiomes)

# Sample sites -----------------------------------------------------------------
df <- fdk_site_info |>
  left_join(
    fdk_site_fullyearsequence,
    by = "sitename"
  )

# TODO: replace df => gpp_forcingtarget

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

set.seed(1)

df_sampled_train <- df_train |>
  group_by(strata) |>
  sample_n(size = 1, replace = FALSE) |>
  ungroup()

View(df_sampled_train)

# write_rds(df_sampled_train, file = here::here("data/df_sampled_train.rds"))
# write_csv(
#   df_sampled_train |>
#     mutate(
#       lon = round(lon, 2),
#       lat = round(lat, 2)
#     ) |>
#     select(
#       Site = sitename,
#       Long. = lon,
#       Lat. = lat,
#       Elevation = elv,
#       `Climate zone` = koeppen_code,
#       `Vegetation type` = igbp_land_use,
#       `Year start` = year_start_gpp,
#       `Year end` = year_end_gpp
#     ),
#   file = here::here("data/df_sampled_train.csv")
# )

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

# plotbiomes::whittaker_base_plot() +
#   geom_point(
#     aes(mat, map/10),
#     data = df_sampled_train
#   )
