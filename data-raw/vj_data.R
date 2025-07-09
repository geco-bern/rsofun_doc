library(tidyverse)
library(rpmodel)
library(rgeco) # remotes::install_github("https://github.com/geco-bern/rgeco")

# Nick Smith's data,
df <- read_csv(here::here("data/GlobV_v2.1_env_open.csv"))

# xxx for GMD, do not aggregate across species, but specify likelihood as a function of mismatch wrt all species individually for a given site
df_sites <- df |>
  ungroup() |>
  mutate(vj = vcmax/jmax) |>
  mutate(site = paste0("lon_", as.character(format(longitude, digits = 2)), "_lat_", as.character(format(latitude, digits = 3)))) |>
  group_by(site) |>
  summarise(
    lon = mean(longitude),
    lat = mean(latitude),
    temp = mean(temperature_gs),
    vpd = mean(vpd_gs),
    par = mean(par_gs),
    elv = mean(z),
    co2 = mean(ca),
    vcmax = mean(vcmax),
    jmax = mean(jmax),
    vj = mean(vj)
  ) |>
  mutate(
    patm = rpmodel::calc_patm(elv)
  )

df_sites |>
  ggplot(aes(x = vj)) +
  geom_histogram(bins = 15)

rgeco:::plot_map_simpl() +
  geom_point(
    data = df_sites |>
      drop_na(vj),
    aes(lon, lat)
    )

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
df_sites_with_outputs <- df_sites |>
  mutate(row_id = row_number()) |>
  pmap_dfr(function(temp, vpd, patm, par, co2, row_id, ...) {
    result <- run_pmodel_onestep_f_bysite(
      lc4 = FALSE,
      forcing = data.frame(temp = temp, vpd = vpd, ppfd = par, co2 = co2, patm = patm),
      params_modl = params_modl,
      makecheck = FALSE
    ) |>
      rename(vcmax_mod = vcmax, jmax_mod = jmax)
    cbind(row_id = row_id, result)
  }) |>
  right_join(df_sites |> mutate(row_id = row_number()), by = "row_id") |>
  select(-row_id)

# Plot modelled vs observed
# Vcmax
df_sites_with_outputs |>
  ggplot(aes(vcmax_mod, vcmax)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  labs(
    x = "Modelled Vcmax",
    y = "Observed Vcmax"
  )

# Jmax
df_sites_with_outputs |>
  ggplot(aes(jmax_mod, jmax)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  labs(
    x = "Modelled Jmax",
    y = "Observed Jmax"
  )

# Vcmax:Jmax ratio
df_sites_with_outputs |>
  mutate(vj_mod = vcmax_mod / jmax_mod) |>
  ggplot(aes(vj_mod, vj)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  labs(
    x = "Modelled Vcmax:Jmax",
    y = "Observed Vcmax:Jmax"
  )

# # Kumarathunge data
# df <- read_csv("~/data/archive/acitglob_kumarathunge_2020/data/Kumarathunge-aci-tglob-8595d961d4c8/Data/PPC-TGlob_V1.0.csv")
# df <- read_csv("/data/archive/acitglob_kumarathunge_2020/data/Kumarathunge-aci-tglob-8595d961d4c8/Data/PPC-TGlob_V1.0.csv") # Workstation-02
#
# # filter data to field - native environment (seed source lon/lat can be interpreted as lon/lat for forcing data)
# df <- df |>
#   filter(Growth_condition == "Field (NE)")
#
# df_sites <- df |>
#   ungroup() |>
#   mutate(vj = Vcmax/Jmax) |>
#   group_by(Location) |>
#   summarise(
#     lon = mean(seed_source_longitude),
#     lat = mean(seed_source_latitude),
#     vcmax = mean(Vcmax),
#     jmax = mean(Jmax),
#     vj = mean(vj)
#   )
#
# df_sites |>
#   ggplot(aes(x = vj)) +
#   geom_histogram(bins = 9)
