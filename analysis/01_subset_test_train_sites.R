library(tidyverse)
library(rgeco)
library(ggplot2)
library(sf)

# Load gpp, vj, chi data -------------------------------------------------------
drivers <- read_rds(here::here("data/01_chi-vj-gpp_calibsofun_drivers.rds"))
obs     <- read_rds(here::here("data/01_chi-vj-gpp_calibsofun_obs.rds"))

# add information to driver vj or chi is fitted
drivers <- drivers |>
  left_join(
    obs |> unnest_wider(targets,  names_sep = "_") |> select(sitename, starts_with("targets")),
    by = join_by(sitename)
  )



gpp_forcing    <- drivers |> filter(run_model == "daily")
vj_chi_forcing <- drivers |> filter(run_model == "onestep")


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
      # vj_chi_forcing |> unnest(site_info) |> filter(is.na(Defourny_LCCS) | is.na(Beck_KG)) # nrow: 3, three vj sites have no koeppen or igbp information



# Sample sites for training and testing ----------------------------------------

## ensure no data leakage if we make train-test split separately on gpp and vj_chi data sets
stopifnot(length(intersect(gpp_forcing$sitename, vj_chi_forcing$sitename)) == 0)

## Define strata for split for gpp as well as for chi-vj datasets --------------
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

vjchi_strata <- vj_chi_forcing |>
  # define strata
  unnest(site_info) |>
  mutate(strata = interaction(Beck_KG, Defourny_LCCS, targets_vj, targets_chi, targets_gpp, drop = TRUE))


## gpp sites -------------------------------------------------------------------
set.seed(1)

gpp_train <- gpp_strata |>
  filter(nyears_gpp > 15) |>
  group_by(strata) |>
  sample_n(size = 1, replace = FALSE) |> # TODO: why do we only use 1 site per strata for training?
  ungroup()

gpp_train

set.seed(1982)

gpp_test <- gpp_strata |>
  filter(!(sitename %in% gpp_train$sitename)) |>
  filter(nyears_gpp > 5) |>
  group_by(strata) |>
  sample_n(size = 1, replace = FALSE) |> # TODO: why do we sample and don't use all the remaining sites?
  ungroup()

# TODO check:
gpp_unused <- gpp_strata |>
  anti_join(gpp_test) |>
  anti_join(gpp_train)
# stopifnot(nrow(gpp_unused) == 0) # TODO: reactivate.
# END TODO


## vj, chi sites ---------------------------------------------------------------
set.seed(1982)

# determine test sites
vjchi_test <- vjchi_strata |>
  group_by(strata) |>
  slice_sample(n=1) |>
  ungroup()

# all other are train sites
vjchi_train <- vjchi_strata |>
  filter(!(sitename %in% vjchi_test$sitename))

# check that all are used:
vjchi_unused <- vjchi_strata |>
  anti_join(vjchi_test) |>
  anti_join(vjchi_train)
stopifnot(nrow(vjchi_unused) == 0)


## Write to file ---------------------------------------------------------------
# TODO: write_rds(drivers_train, file = here::here("data/drivers_train.rds"))
# TODO: make this as a dataframe that specifies: sitename, run_model, targets_vj, targets_chi, targets_gpp, and dataset=["test","train"]

df_test_train_split <- bind_rows(
  vjchi_train |> select(sitename, run_model, targets_vj, targets_chi, targets_gpp) |> mutate(dataset = "train"),
  vjchi_test  |> select(sitename, run_model, targets_vj, targets_chi, targets_gpp) |> mutate(dataset = "test"),
  gpp_train |> select(sitename, run_model, targets_vj, targets_chi, targets_gpp) |> mutate(dataset = "train"),
  gpp_test  |> select(sitename, run_model, targets_vj, targets_chi, targets_gpp) |> mutate(dataset = "test")
)

write_csv(df_test_train_split, file = here::here("data/01_test_train_split.csv"))


## Add samples to drivers ------------------------------------------------------
drivers2 <- drivers |>
  dplyr::left_join(
    df_test_train_split,
    by = join_by(sitename, run_model, targets_vj, targets_chi, targets_gpp)
  ) |>
  # TODO
  mutate(dataset2 = if_else(is.na(dataset), "unused", dataset))
# END TODO


# Plot the distribution of training and testing sites --------------------------
dat_to_plot <- drivers2 |>
  unnest(site_info) |>
  mutate(target = paste(targets_vj, targets_chi, targets_gpp)) |>
  mutate(target = case_when(
    target == "FALSE FALSE TRUE" ~ "gpp",
    target == "FALSE TRUE TRUE"  ~ "gpp+chi",
    target == "TRUE TRUE TRUE"   ~ "gpp+chi+vj",

    target == "FALSE TRUE FALSE" ~ "chi",
    target == "TRUE TRUE FALSE"  ~ "chi+vj",

    target == "TRUE FALSE FALSE" ~ "vj",
    target == "TRUE FALSE FALSE" ~ "none",
    TRUE ~ NA_character_)) |>
  select(target, everything())

filter(dat_to_plot, is.na(target)) #TODO: where are these coming from??
filter(dat_to_plot, is.na(target)) |> magrittr::extract2("sitename") # "lon_+151.14_lat_-033.69" "lon_-079.10_lat_+035.97" "lon_-083.81_lat_+042.27"


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
dat_to_plot2a <- pivot_longer(dat_to_plot, c(lon, lat, elv, whc))
dat_to_plot2b <- pivot_longer(dat_to_plot, c(FDK_koeppen_code, FDK_igbp_land_use, Defourny_LCCS, Beck_KG))

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
