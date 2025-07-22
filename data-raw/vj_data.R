# This script prepares the input forcing tibble ('df_vj_forcing') and
# output target tibble ('df_vj_target') needed for simulating vcmax, jmax and
# their ratio vj = vcmax/jmax with P-model.
#
# The input forcing is taken from the data set.
# In a second step this input forcing could be replaced with data from ingestr.
#
# These two objects are stored as *.rds files in subfolder data/

rm(list = ls())
library(tidyverse)
library(rpmodel)
library(rgeco) # pak::pkg_install("geco-bern/rgeco")
library(dplyr)
library(purrr)
library(rsofun)  # install from branch simple_pmodel_v2
                 # pak::pkg_install("geco-bern/rsofun@simple_pmodel_v2")

# Nick Smith's data,
df_vj_allobs <- read_csv(here::here("data-raw/GlobV_v2.1_env_open.csv"))

df_vj_allobs <- df_vj_allobs |>
  ungroup() |>
  mutate(vj = vcmax/jmax) |>
  mutate(site = sprintf("lon_%+07.2f_lat_%+07.2f", longitude, latitude))


#TODO: no time information contained. Do we need that? Is month important for vj ratio? Is year (CO2) important for vj ratio?


# drop NAs
df_vj <- df_vj_allobs |>
  # dropping observations that are missing either of the targets
  drop_na(vcmax, jmax)

# ggplot(df_vj_allobs, aes(x=genus, y=latitude)) + geom_point() + theme(axis.text.x = element_text(angle=90))
# df_vj_allobs$genus |> unique() |> sort()   #  824 different genus (??) TODO
# df_vj_allobs$species |> unique() |> sort() # 1118 different species
# paste(df_vj_allobs$genus, df_vj_allobs$species) |> unique() |> sort() # 1476 different genus+species

# aggregate model inputs and model targets
#   inputs:  by site only         (reducing number of simulations)
#   targets: by site and species  (allowing for species differences in error term)
#
#   For the GMD paper, not aggregating across species requires to specify
#   likelihood as a function of mismatch wrt all species individually for a given site.
df_vj_forcing <- df_vj |>
  group_by(site) |>
  summarise(.groups = "keep",
    lon = mean(longitude),
    lat = mean(latitude),
    temp_degC    = mean(temperature_gs),  # deg C,     growing season value (growing season, where monthly mean T>0)
    vpd_Pa       = mean(vpd_gs),           # Pa,        growing season value (growing season, where monthly mean T>0)
    par_umolm2s  = mean(par_gs),           # umol/m2/s, growing season value (growing season, where monthly mean T>0)
    elv_masl     = mean(z),                # m asl
    co2_ppm      = mean(ca))  |>           # ppm
  mutate(
    patm_Pa = rpmodel::calc_patm(elv_masl)
  )

df_vj_target <- df_vj |>
  group_by(site, genus, species) |>
  summarise(.groups = "keep",
    lon = mean(longitude),
    lat = mean(latitude),
    vcmax_obs_umolm2s = mean(vcmax),  # umol/m2/s
    jmax_obs_umolm2s  = mean(jmax),   # umol/m2/s
    vj_obs = mean(vj)                 # unitless ratio
  )

rm(df_vj)

# Show
df_vj_target  # 1031 site,genus,species combinations
df_vj_forcing # 106 sites
# df_vj |> group_by(site) |> summarise(lon = mean(longitude), lat = mean(latitude)) # 247 sites (containing NAs)

# Plot
df_vj_target |>
  ggplot(aes(x = vj_obs)) +
  geom_histogram(bins = 15)


# rgeco:::plot_map_simpl() +
#   geom_point(data = df_vj_allobs |>
#                group_by(site) |>
#                summarise(lon = mean(longitude),
#                          lat = mean(latitude)),
#              aes(lon, lat))

rgeco:::plot_map_simpl() +
  geom_point(data = df_vj_forcing, aes(lon, lat))

ggplot() +
  geom_point(data = df_vj_target, aes(genus, lat, color = vcmax_obs_umolm2s)) +
  labs(color = "Vcmax\n(umol/m2/s)") +
  theme_minimal() + theme(axis.text.x = element_text(angle=90), panel.grid.major.x = element_blank())
# Without aggregating across species below map plot doesn't make sense anymore:
# rgeco:::plot_map_simpl() +
#   geom_point(data = df_vj_target, aes(lon, lat, color = vcmax_umolm2s)) +
#   theme(legend.position = c(0,0),
#         legend.justification = c(0,0),
#         legend.background = element_rect(fill = NA)) +
#   labs(color = "Vcmax\n(umol/m2/s)")


saveRDS(df_vj_forcing, here::here("data/vj_forcing.rds"))
saveRDS(df_vj_target, here::here("data/vj_target.rds"))




# TODO: potentially replace forcing data with data from ingestr to homogenize with chi data.
#       however, we do not have the time information
# rm(df_vj_forcing)
# df_vj_forcing <- ingestr::...

















# TODO: below is just a test.

# Apply one-step P-model function on each row of df_vj
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
df_vj_modeled <- df_vj_forcing |>
  group_by(site) |> #, lon, lat) |>
  group_modify(~run_pmodel_onestep_f_bysite(
    lc4 = FALSE,
    forcing =  data.frame(temp = .x$temp_degC,        # TODO: with ingestr this could be daytime average
                          vpd  = .x$vpd_Pa,           # TODO: with ingestr this could be daytime average
                          ppfd = .x$par_umolm2s/1000, # TODO: rsofun needs ppfd in mol/m2/s
                          co2  = .x$co2_ppm,
                          patm = .x$patm_Pa),
    params_modl = params_modl,
    makecheck = FALSE)) |>
  rename(vcmax_mod_molm2s = vcmax,
         jmax_mod_molm2s  = jmax) |>
  # transform to same units as targets:
  mutate(vcmax_mod_umolm2s = vcmax_mod_molm2s*1000,
         jmax_mod_umolm2s  = jmax_mod_molm2s*1000) |>
  select(-vcmax_mod_molm2s, -jmax_mod_molm2s)

# Plot modelled vs observed

# Combine modelled and observed
df_vj_with_outputs <- dplyr::inner_join(df_vj_modeled, df_vj_target, by = join_by(site))

# Vcmax
df_vj_with_outputs |>
  ggplot(aes(vcmax_mod_umolm2s, vcmax_obs_umolm2s)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") #+
  # labs(
  #   x = "Modelled Vcmax",
  #   y = "Observed Vcmax"
  # )

# Jmax
df_vj_with_outputs |>
  ggplot(aes(jmax_mod_umolm2s, jmax_obs_umolm2s)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") #+
  # labs(
  #   x = "Modelled Jmax",
  #   y = "Observed Jmax"
  # )

# Vcmax:Jmax ratio
df_vj_with_outputs |>
  mutate(vj_mod = vcmax_mod_umolm2s / jmax_mod_umolm2s) |>
  ggplot(aes(vj_mod, vj_obs)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  labs(
    x = "Modelled Vcmax:Jmax",
    y = "Observed Vcmax:Jmax"
  )


# TODO: Using Vcmax data in a calibration exercise could be problematic because
# it is so sensitive to illumination level. Jmax likewise. But the ratio of
# Jmax25 to Vcmax25 is much more stable. I don't know what is the best data set
# for this ratio, but you coulc consider using the compilation of experimental
# measurements in Kumarathunge et al. (2019) New Phytologist?

# TODO Wang: Kattge et al. For estimating c*, I used the dataset of Jmax and Vcmax from Jens Kattge
# to generate Figure S5 in Wang et al. Nature Plants paper. There are quite a
# few datasets compiled for Vcmax, such as Yan et al. 2025 Fundemental Research,
# but I am not sure how many in-pair Jmax data available...

# TODO: Colin:
# Daytime temperature:
# $$ T_g = T_{max} \left[ \frac{1}{2} + \cos^{-1}(x)\frac{(1-x^2)^{1/2}}{2} \right] + T_{min}\left[ \frac{1}{2} - \cos^{-1}(x)\frac{(1-x^2)^{1/2}}{2} \right] $$
# $$ x = -\tan(\lambda)\tan(\delta) $$
# where $\lambda$ is latitude and $\delta$ is the monthly average solar declination.
# $\delta$ = TODO find this reference 100

# TODO: Beni:
# @Fabian: Note that I created a new function in rsofun that allows us to run
# the P-model in a very stripped-down and computationally efficient mode - as a
# single-step call instead of a time series simulation. I added this to a new
# branch `simple_pmodel`. The plots I included above are created with
# data-raw/vj_data.R in the repo rsofun_doc. For the model calibration with the
# traits data, we will have to create a new cost function that calls that
# one-step P-model call in much the same way as demonstrated in rsofun_doc:
# data-raw/vj_data.R.




# # Kumarathunge data
# df_vj <- read_csv("~/data/archive/acitglob_kumarathunge_2020/data/Kumarathunge-aci-tglob-8595d961d4c8/Data/PPC-TGlob_V1.0.csv")
# df_vj <- read_csv("/data/archive/acitglob_kumarathunge_2020/data/Kumarathunge-aci-tglob-8595d961d4c8/Data/PPC-TGlob_V1.0.csv") # Workstation-02
#
# # filter data to field - native environment (seed source lon/lat can be interpreted as lon/lat for forcing data)
# df_vj <- df_vj |>
#   filter(Growth_condition == "Field (NE)")
#
# df_vj <- df_vj |>
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
# df_vj |>
#   ggplot(aes(x = vj)) +
#   geom_histogram(bins = 9)
