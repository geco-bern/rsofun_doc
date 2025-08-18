library(tidyverse)
library(rgeco)
library(ggplot2)
library(sf)

# Load gpp, vj, bigD13C data -------------------------------------------------------
drivers <- read_rds(here::here("data/01_bigD13C-vj-gpp_calibsofun_drivers.rds"))
obs     <- read_rds(here::here("data/01_bigD13C-vj-gpp_calibsofun_obs.rds"))

# add information to driver vj or bigD13C is fitted
drivers <- drivers |>
  left_join(
    obs |> unnest_wider(targets,  names_sep = "_") |> select(sitename, starts_with("targets")),
    by = join_by(sitename)
  )



gpp_forcing        <- drivers |> filter(run_model == "daily")
vj_bigD13C_forcing <- drivers |> filter(run_model == "onestep")


# NOTE: 'gpp_forcing' was already filtered:
#       - crop- and wetland sites were removed as well as
#       - sites with 5 or less years were removed
gpp_forcing |> unnest(site_info) |>
  ggplot(aes(nyears_gpp)) +
  geom_histogram()

gpp_forcing |> unnest(site_info) |>
  ggplot(aes(x=nyears_gpp, color=FDK_igbp_land_use)) +
  geom_histogram()


      # # Remove sites without climate and landcover classification
      # gpp_forcing |> unnest(site_info) |> filter(is.na(FDK_koeppen_code) | is.na(FDK_igbp_land_use)) # nrow: 0, all gpp sites have koeppen or igbp information
      # vj_bigD13C_forcing |> unnest(site_info) |> filter(is.na(Defourny_LCCS) | is.na(Beck_KG)) # nrow: 3, three vj sites have no koeppen or igbp information



# Sample sites for training and testing ----------------------------------------

## ensure no data leakage if we make train-test split separately on gpp and vj_bigD13C data sets
stopifnot(length(intersect(gpp_forcing$sitename, vj_bigD13C_forcing$sitename)) == 0)

## Define strata for split for gpp as well as for bigD13C-vj datasets --------------
gpp_strata <- gpp_forcing |>
  unnest(site_info) |>
  # mutate(p_over_pet = as.numeric(p_over_pet)) |>
  # mutate(
  #   whc_bin = cut(whc, breaks = quantile(whc, probs = seq(0, 1, 1/3)), include.lowest = TRUE),
  #   mat_bin = cut(mat, breaks = quantile(mat, probs = seq(0, 1, 1/3)), include.lowest = TRUE),
  #   mi_bin = cut(p_over_pet, breaks = quantile(p_over_pet, probs = seq(0, 1, 1/3)), include.lowest = TRUE)
  # ) |>
  # mutate(strata = interaction(whc_bin, mat_bin, mi_bin, igbp_land_use, drop = TRUE))
  mutate(strata = interaction(FDK_koeppen_code, FDK_igbp_land_use, drop = TRUE))

vjbigD13C_strata <- vj_bigD13C_forcing |>
  # define strata
  unnest(site_info) |>
  mutate(strata = interaction(Beck_KG, Defourny_LCCS, targets_vj, targets_bigD13C, targets_gpp, drop = TRUE))


## gpp sites -------------------------------------------------------------------
## for simplicity only take 1 site per stratum for training and 1 site for testing
set.seed(1)

gpp_train <- gpp_strata |>
  filter(nyears_gpp > 15) |>
  group_by(strata) |>
  sample_n(size = 1, replace = FALSE) |>
  ungroup()

gpp_train

set.seed(1982)

gpp_test <- gpp_strata |>
  filter(!(sitename %in% gpp_train$sitename)) |>
  filter(nyears_gpp > 5) |>
  group_by(strata) |>
  sample_n(size = 1, replace = FALSE) |>
  ungroup()

# # check which sites are unuse:
# gpp_unused <- gpp_strata |>
#   anti_join(gpp_test) |>
#   anti_join(gpp_train)
# # stopifnot(nrow(gpp_unused) == 0) # TODO: reactivate.


## vj, bigD13C sites ---------------------------------------------------------------
set.seed(1982)

# determine test sites
vjbigD13C_train <- vjbigD13C_strata |>
  slice_sample(prop=0.5, by = c(strata, targets_vj, targets_bigD13C))

# determine test sites
vjbigD13C_test <- vjbigD13C_strata |>
  filter(strata %in% unique(vjbigD13C_train$strata)) |> # ensure test is in same strata as train
  filter(!(sitename %in% vjbigD13C_train$sitename))     # simply remove the 50% from train, no need to slice_sample again
  # slice_sample(prop=0.5, by = c(strata, targets_vj, targets_bigD13C))

vjbigD13C_strata |> group_by(targets_vj, targets_bigD13C) |> summarise(n())
vjbigD13C_train  |> group_by(targets_vj, targets_bigD13C) |> summarise(n())
vjbigD13C_test   |> group_by(targets_vj, targets_bigD13C) |> summarise(n())

## Write to file ---------------------------------------------------------------
df_test_train_split <- bind_rows(
  vjbigD13C_train |> select(sitename, run_model, targets_vj, targets_bigD13C, targets_gpp) |> mutate(dataset = "train"),
  vjbigD13C_test  |> select(sitename, run_model, targets_vj, targets_bigD13C, targets_gpp) |> mutate(dataset = "test"),
  gpp_train |> select(sitename, run_model, targets_vj, targets_bigD13C, targets_gpp) |> mutate(dataset = "train"),
  gpp_test  |> select(sitename, run_model, targets_vj, targets_bigD13C, targets_gpp) |> mutate(dataset = "test")
)

write_csv(df_test_train_split, file = here::here("data/01_test_train_split.csv"))


## Add samples to drivers ------------------------------------------------------
drivers2 <- drivers |>
  dplyr::left_join(
    df_test_train_split,
    by = join_by(sitename, run_model, targets_vj, targets_bigD13C, targets_gpp)
  ) |>
  # TODO
  mutate(dataset2 = if_else(is.na(dataset), "unused", dataset))


# Plot the distribution of training and testing sites --------------------------
dat_to_plot <- drivers2 |>
  unnest(site_info) |>
  mutate(target = paste(targets_vj, targets_bigD13C, targets_gpp)) |>
  mutate(target = case_when(
    target == "FALSE FALSE TRUE" ~ "gpp",
    target == "FALSE TRUE TRUE"  ~ "gpp+bigD13C",
    target == "TRUE TRUE TRUE"   ~ "gpp+bigD13C+vj",

    target == "FALSE TRUE FALSE" ~ "bigD13C",
    target == "TRUE TRUE FALSE"  ~ "bigD13C+vj",

    target == "TRUE FALSE FALSE" ~ "vj",
    target == "FALSE FALSE FALSE" ~ "none",
    TRUE ~ NA_character_)) |>
  select(target, everything())

filter(dat_to_plot, target == "none") |> magrittr::extract2("sitename") # "lon_+151.14_lat_-033.69" "lon_-079.10_lat_+035.97" "lon_-083.81_lat_+042.27"


## Fig A: Plot a map of test train data:

# read 110 m resolution coastline from NaturalEarth data (is a shapefile)
add_coast <- geom_sf(
  data = rnaturalearth::ne_coastline(scale = 110, returnclass = "sf"),
  colour = 'black',
  linewidth = 0.1)
# download oceans
add_ocean <- geom_sf(
  data = rnaturalearth::ne_download(scale = 110, returnclass = "sf",
                                    type = "ocean",
                                    category = "physical"),
  color = NA,
  fill = "white")

# some layout modifications
map_layout_modifications <- list(
  xlab(''),
  ylab(''),
  theme_bw(),
  theme(axis.ticks.y.right = element_line(),
        axis.ticks.x.top = element_line(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "grey70"),
        plot.background = element_rect(fill = "white")
  )
)

set_map_extent <- function(
    lonmin = -180,
    lonmax = 180,
    latmin = -60,
    latmax = 85
){
  # set extent in longitude and latitude
  coord_sf(xlim = c(lonmin, lonmax),
           ylim = c(latmin, latmax),
           expand = FALSE   # to draw map strictly bounded by the specified extent
  )
}

fig_A_test_train_map <- ggplot(dat_to_plot) +
  # define map layout
  add_ocean +
  add_coast +
  set_map_extent() +
  map_layout_modifications +
  # add data
  geom_point(aes(x=lon,y=lat)) +
  facet_grid(target~dataset2)


ggsave(
  here::here("fig/01_fig_A_test_train_map.png"),
  fig_A_test_train_map, width=7.2, height=4.8, units="in", scale = 2)



## Fig B: Plot histograms of covariates of of test train data:
dat_to_plot2a <- pivot_longer(dat_to_plot, c(lon, lat, elv, whc)) |> filter(target != "none")
dat_to_plot2b <- pivot_longer(dat_to_plot, c(FDK_koeppen_code, FDK_igbp_land_use, Defourny_LCCS, Beck_KG)) |> filter(target != "none")

# numerical covariates
fig_B_test_train_hist <- ggplot(dat_to_plot2a |> filter(!is.na(target))) +
  theme_bw() +
  # add data
  geom_density(aes(x=value, color = dataset2)) +
  facet_grid(target~name, scales = "free")

ggsave(
  here::here("fig/01_fig_B_test_train_hist.png"),
  fig_B_test_train_hist, width=7.2, height=4.8, units="in", scale = 2)

# categorical covariates
# fig_B2_test_train_hist <- ggplot(dat_to_plot2b |> filter(!is.na(target), target != "gpp") |> drop_na(value)) +
#   theme_bw() +
#   # add data
#   geom_histogram(aes(x=value, color = dataset2)) +
#   facet_grid(target~name, scales = "free")
# fig_B2_test_train_hist
# ggsave(
#   here::here("fig/01_fig_B2_test_train_hist.png"),
#   fig_B2_test_train_hist, width=7.2, height=4.8, units="in", scale = 2)
