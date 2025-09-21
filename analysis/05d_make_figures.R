# Script that generates figures for manuscriptå

library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(dtplyr)
library(BayesianTools)
library(ggplot2)
library(patchwork)
library(cowplot)
library(ggridges)
library(xtable)

source(here::here("R/figure_helpers.R"))
source(here::here("analysis/00_define_scenarios.R"))

source(here::here("R/calibration_helpers.R"))
source(here::here("R/prediction_helpers.R"))
source(here::here("R/run_prediction_rsofun.R"))



# Setup re-labelling for final figures: ----

## Re-label Parameters ----
rsofun_symbol_parname_description <- tribble(
  ~Parameter,             ~Symbol_tex,                              ~Units_tex,                     ~Symbol_R,                       ~Description,
  # MODEL PARAMETER:
  "kphio",                "$\\varphi_0$",                           "\\unit{mol\\,mol^{-1}}",       "phi[0]",                 "Quantum yield at optimal temperature" ,
  "kphio_par_a",          "$\\varphi_a$",                           "\\unit{°C^{-2}}",              "phi[a]",                 "Shape parameter for the temperature dependence of the quantum yield" ,
  "kphio_par_b",          "$\\varphi_b$",                           "\\unit{°C}",                   "phi[b]",                 "Optimal temperature for the quantum yield" ,
  "soilm_thetastar",      "$\\theta^*$",                            "\\unit{mm}",                   "theta^'*'",              "Threshold plant-available soil water content in the soil moisture stress function" ,
  "soilm_betao",          "$\\beta_0$",                             "unitless",                     "beta[0]",                "Stress factor at low soil moisture, intercept for the soil moisture stress function" ,
  "beta_unitcostratio",   "$\\beta$",                               "unitless",                     "beta",                   "Unit cost ratio of carboxylation (maintenance of $V_{\\mathrm{cmax}}$) to transpiration" ,
  "rd_to_vcmax",          "$b_0$",                                  "unitless",                     "b[0]",                   "Ratio of ($R_{\\mathrm{d25}}$) to the maximum carboxylation rate $V_{\\mathrm{cmax}}$ (both temperature-normalised dark respiration; eq. C8 in Stocker et al. 2020)" ,
  "tau_acclim",           "$\\tau$",                                "days",                         "tau",                    "Acclimation time scale of photosynthesis" ,
  "kc_jmax",              "$c^{*}$",                                "unitless",                     "c^'*'",                  "Unit cost of electron transport (maintenance of $J_{\\mathrm{max}}$)" ,
  # ERROR PARAMETER:
  "err_gpp",              "$\\epsilon_{\\mathrm{gpp}}$",            "\\unit{gC\\,m^{-2}\\,s^{-1}}", "epsilon['gpp']",         "Gaussian error standard deviation of GPP" ,
  "err_bigD13C",          "$\\epsilon_{\\mathrm{\\Delta^{13}C}}$",  "\\unit{\\permil}",             "epsilon[Delta^'13'*C]",  "Gaussian error standard deviation of $\\Delta^{13}C$" ,
  "err_vj",               "$\\epsilon_{\\mathrm{vj}}$",             "unitless",                     "epsilon['vj']",          "Gaussian error standard deviation of $\\frac{V_{\\mathrm{cmax}}}{J_{\\mathrm{max}}}$" ,
  "errbias_bigD13C",      "$\\delta_{\\mathrm{\\Delta^{13}C}}$",    "\\unit{\\permil}",             "delta[Delta^'13'*C]",    "Bias error term of $\\Delta^{13}C$ (= mod - obs)",
  "errbias_vj",           "$\\delta_{\\mathrm{vj}}$",               "unitless",                     "delta['vj']",            "Bias error term of $\\frac{V_{\\mathrm{cmax}}}{J_{\\mathrm{max}}}$ (= mod - obs)",
  "errscale_gpp",         "$\\kappa_{\\mathrm{gpp}}$",              "unitless",                     "kappa['gpp']",           "Multiplicative bias error term of GPP"
) |> mutate(Parameter = forcats::as_factor(Parameter),   # make factor to keep ordering
            Symbol_tex = forcats::as_factor(Symbol_tex)) # make factor to keep ordering
label_vec <- setNames(rsofun_symbol_parname_description$Symbol_R, rsofun_symbol_parname_description$Parameter)
custom_labeller_variable <- function(labels, multi_line = TRUE) { # adapted from label_parsed
  replaced_labels <- left_join(select(labels, variable), # TODO: this has variable hardcoded
                               select(rsofun_symbol_parname_description, variable = Parameter, label = Symbol_R),
                               by = join_by(variable))
  # print(tibble(replaced_labels))
  replaced_labels <- replaced_labels |> select(-variable)
  replaced_labels <- label_value(replaced_labels, multi_line = multi_line)
  # print(replaced_labels)
  lapply(unname(replaced_labels), lapply, function(values) {
          c(parse(text = as.character(values)))
      })
}
## Re-label Scenarios ----
scenario_labels <-tribble(
  ~scenario, ~label, ~label_targets,
  228,       "a)",     "'a) '*Delta^'13'*C",
  227,       "b)",     "'b) '*frac(V[cmax], J[max])",
  226,       "c)",     "'c) '*Delta^'13'*C*','*frac(V[cmax], J[max])",
  222,       "d)",     "'d) '*GPP",
  223,       "e)",     "'e) '*Delta^'13'*C*','*frac(V[cmax], J[max])*','*GPP",
  229,       "f)",     "'f) '*Delta^'13'*C*','*frac(V[cmax], J[max])*','*GPP",   #  (priors)
  230,       "g)",     "'g) '*Delta^'13'*C*','*frac(V[cmax], J[max])*','*GPP"    #  (fixed)
  ) #|> mutate(label = factor(label))
scenario_label_vec <- setNames(scenario_labels$label_targets, scenario_labels$label)
custom_labeller_scenarios <- function(x) {
  sapply(x, function(level) scenario_label_vec[level])
}
# ggplot(scenario_labels, aes(x=scenario, y=label)) + geom_point() +
#   scale_y_discrete(NULL, labels = \(x) parse(text = custom_labeller_scenarios(x)), lim = rev) +
#   theme(axis.text.y = element_text(hjust=0))




# GENERAL PLOTS/TABLES: ----
source(here::here("R/calibration_helpers.R"))
source(here::here("R/run_mcmc_rsofun.R"), echo = TRUE)

flag_plot_general <- FALSE # possibility to switch this off

## Figure C: map of sites ----
## for each targets x test+train
if (flag_plot_general){
  res_s223 <- setup_rsofun_calibration(scenario = 223)
  site_info <- bind_rows(
    res_s223$drivobs_train |> mutate(set = "train"),
    res_s223$drivobs_test |> mutate(set = "test")
  ) |> unnest(site_info) |>
    unnest_wider(targets)


  pl1_train <- rgeco:::plot_map_simpl(dir_ne = tempdir()) +
    geom_point(size=0.1,shape = 20, color = "red",
               data    = site_info |> filter(gpp) |> filter(set == "train"),
               mapping = aes(lon, lat)) + ggtitle("GPP flux sites") + labs(caption = sprintf("Training set (n=%d)", site_info |> filter(gpp) |> filter(set == "train") |> nrow()))
  pl2_train <- rgeco:::plot_map_simpl(dir_ne = tempdir()) +
    geom_point(size=0.1,shape = 20, color = "red",
               data    = site_info |> filter(vj) |> filter(set == "train"),
               mapping = aes(lon, lat)) + ggtitle("Vcmax/Jmax sites") + labs(caption = sprintf("Training set (n=%d)", site_info |> filter(vj) |> filter(set == "train") |> nrow()))
  pl3_train <- rgeco:::plot_map_simpl(dir_ne = tempdir()) +
    geom_point(size=0.1,shape = 20, color = "red",
               data    = site_info |> filter(bigD13C) |> filter(set == "train"),
               mapping = aes(lon, lat)) + ggtitle("Δ13C sites") + labs(caption = sprintf("Training set (n=%d)", site_info |> filter(bigD13C) |> filter(set == "train") |> nrow()))

  pl1_test <- rgeco:::plot_map_simpl(dir_ne = tempdir()) +
    geom_point(size=0.1,shape = 20, color = "red",
               data    = site_info |> filter(gpp) |> filter(set == "test"),
               mapping = aes(lon, lat)) + ggtitle(NULL) + labs(caption = #"GPP flux sites",
                                                  sprintf("Test set (n=%d)", site_info |> filter(gpp) |> filter(set == "test") |> nrow()))
  pl2_test <- rgeco:::plot_map_simpl(dir_ne = tempdir()) +
    geom_point(size=0.1,shape = 20, color = "red",
               data    = site_info |> filter(vj) |> filter(set == "test"),
               mapping = aes(lon, lat)) + ggtitle(NULL) + labs(caption = #"Vcmax/Jmax sites",
                                                  sprintf("Test set (n=%d)", site_info |> filter(vj) |> filter(set == "test") |> nrow()))
  pl3_test <- rgeco:::plot_map_simpl(dir_ne = tempdir()) +
    geom_point(size=0.1,shape = 20, color = "red",
               data    = site_info |> filter(bigD13C) |> filter(set == "test"),
               mapping = aes(lon, lat)) + ggtitle(NULL) + labs(caption = #"Δ13C sites",
                                                  sprintf("Test set (n=%d)", site_info |> filter(bigD13C) |> filter(set == "test") |> nrow()))

  library(cowplot)
  remove_labels <- theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    title = element_text(size=10), plot.caption = element_text(size=8)
    )
  pl_sitemap <- cowplot::plot_grid(
    pl1_train + remove_labels, pl2_train + remove_labels, pl3_train + remove_labels,
    pl1_test + remove_labels, pl2_test + remove_labels, pl3_test + remove_labels,
    rel_heights = c(1.2,1),
    # labels = c("(a)","(b)","(c)",NULL,NULL,NULL),
    ncol = 3)

  ggsave(
    here::here("fig/fig_C_append_climate_MapTargetTrainingSites.png"),
    pl_sitemap, width=12, height=5, units="cm", dpi=300, scale = 1.3)
}

## Table a: site table [lon, lat, elv, climate, vegtype, train-or-test, targets, Nobs] ----
if (flag_plot_general){
  Defourny_LCCS_to_IGBP_vegtype <- function(df){
    df |>
      mutate(Defourny_LCCS_acr = case_when(
        # Defourny_LCCS == "Bare areas"                                                                           ~ "BSV",     # filterd out,
        # Defourny_LCCS == "Water bodies"                                                                         ~ NA,        # filterd out,
        # Defourny_LCCS == "Urban areas"                                                                          ~ "URB",     # filterd out
        # Defourny_LCCS == "Cropland, rainfed"                                                                    ~ NA,#"CRO", # filterd out
        # Defourny_LCCS == "Cropland, irrigated or post-flooding"                                                 ~ NA,#"CRO", # filterd out
        # Defourny_LCCS == "Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)"   ~ NA,#"CRO", # filterd out
        Defourny_LCCS == "Tree cover, needleleaved, evergreen, closed to open (>15%)"                           ~ "ENF",
        Defourny_LCCS == "Tree cover, broadleaved, deciduous, closed to open (>15%)"                            ~ "DBF",
        Defourny_LCCS == "Tree cover, needleleaved, deciduous, closed to open (>15%)"                           ~ "DNF",
        Defourny_LCCS == "Tree cover, broadleaved, evergreen, closed to open (>15%)"                            ~ "EBF",
        Defourny_LCCS == "Tree cover, mixed leaf type (broadleaved and needleleaved)"                           ~ "MF",
        Defourny_LCCS == "Sparse vegetation (tree, shrub, herbaceous cover) (<15%)"                             ~ "BSV", # source: https://www.cen.uni-hamburg.de/en/icdc/data/land/docs-land/igbp-designations-of-surface-types.pdf
        Defourny_LCCS == "Mosaic tree and shrub (>50%) / herbaceous cover (<50%)"                               ~ "WSA", # source: https://www.cen.uni-hamburg.de/en/icdc/data/land/docs-land/igbp-designations-of-surface-types.pdf
        Defourny_LCCS == "Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) /cropland (<50%)"    ~ "WSA", # source: https://www.cen.uni-hamburg.de/en/icdc/data/land/docs-land/igbp-designations-of-surface-types.pdf
        Defourny_LCCS == "Grassland"                                                                            ~ "GRA",
        Defourny_LCCS == "Shrubland"                                                                            ~ "CSH", # source: https://www.cen.uni-hamburg.de/en/icdc/data/land/docs-land/igbp-designations-of-surface-types.pdf
        TRUE ~ Defourny_LCCS
      )
      )
  }

  site_info |>
    Defourny_LCCS_to_IGBP_vegtype() |>
    select(FDK_igbp_land_use, Defourny_LCCS_acr) |>
    mutate(land_use = case_when(is.na(Defourny_LCCS_acr) ~ FDK_igbp_land_use,
                                TRUE ~ Defourny_LCCS_acr)) |>
    group_by(land_use) |> summarise(n())


  site_info_for_table_a <- site_info |>
    # homogenize vegtype and climate
    # climate Koeppen-Geiger:
    mutate(climate = case_when(gpp     ~ FDK_koeppen_code, # for GPP sites use FluxDataKit
                               vj      ~ Beck_KG,          # for other sites use Beck reference
                               bigD13C ~ Beck_KG,
                               TRUE    ~ NA_character_)) |>      # for other sites use Beck reference
    mutate(climate = stringr::str_to_title(climate)) |>
    # vegtype:
    Defourny_LCCS_to_IGBP_vegtype() |>
    mutate(igbp_vegtype = case_when(gpp     ~ FDK_igbp_land_use,
                                    vj      ~ Defourny_LCCS_acr,
                                    bigD13C ~ Defourny_LCCS_acr,
                                    TRUE    ~ NA_character_)) |>
    # format table
    arrange(-gpp, -vj, -bigD13C) |>
    mutate(target = paste(vj, bigD13C, gpp)) |>
    mutate(target = case_when(
      target == "FALSE FALSE TRUE" ~ "gpp",
      target == "FALSE TRUE TRUE"  ~ "gpp+bigD13C",
      target == "TRUE TRUE TRUE"   ~ "gpp+bigD13C+vj",

      target == "FALSE TRUE FALSE" ~ "bigD13C",
      target == "TRUE TRUE FALSE"  ~ "bigD13C+vj",

      target == "TRUE FALSE FALSE" ~ "vj",
      target == "FALSE FALSE FALSE" ~ "none",
      TRUE ~ NA_character_))

  count_obs <- function(target, nested_data_df){
    if (target == "gpp"){
      N <- nrow(nested_data_df) # no need to unnest
    } else if (target == "vj"){
      N <- nrow(unnest(nested_data_df, vj))
    } else if (target == "bigD13C+vj"){
      N <- sprintf("(%d, %d)",nrow(unnest(nested_data_df, bigD13C)), nrow(unnest(nested_data_df, vj)))
    } else if (target == "bigD13C"){
      N <- nrow(unnest(nested_data_df, bigD13C))
    } else {
      stop("Error")
    }
    return(as.character(N))
  }

  table_a <- site_info_for_table_a |>
    rowwise() |> mutate(Nrows = nrow(data)) |> mutate(Nobs = count_obs(target, data)) |>
    # select columns and order rows:
    mutate(target = factor(target, levels = c("gpp","vj","bigD13C+vj", "bigD13C")),
           set    = factor(set,    levels = c("train","test"))) |>
    arrange(set, target, igbp_vegtype) |>
    select(set, target, Nobs, sitename, climate, igbp_vegtype, lon, lat, elv) |>
    rename(
      "Data set" = set,
      "Target variable" = target,
      "N obs." = Nobs,
      "Site name" = sitename,
      "Climate" = climate,
      "Vegetation type" = igbp_vegtype,
      "Lon." = lon,
      "Lat." = lat,
      "Elev." = elv,
    )

  table_a %>%
    xtable::xtable(
      x = .,
      caption = "Listing of sites in training and testing data sets.",
      tabular.environment = "supertabular", floating = FALSE,
      align = rep("l", (ncol(x = .) + 1))  # make all columns left-aligned
    ) %>%
    print(x = .,
      hline.after = c(-1,0,nrow(.)),  caption.placement = "top",
      file = here::here("fig/table-a_site_list.tex"),
      include.rownames = FALSE
    ) # this can be added to tex file as: \input{filename.tex})

}

# MCMC PLOTS ----

## Load MCMC sampling----
out_calib_s220DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen220_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s221DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen221_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s222DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen222_DREAMzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds")) # TODO: change to 100k
out_calib_s223DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen223_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s224DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen224_DREAMzs-8000-0iter_8x3chains_on_CPU8x1_continued.rds")) # TODO: change to 100k
out_calib_s225DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen225_DREAMzs-8000-0iter_8x3chains_on_CPU8x1_continued.rds")) # TODO: change to 100k
out_calib_s226DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen226_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s227DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen227_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s228DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen228_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
out_calib_s229DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen229_DREAMzs-10000-0iter_8x3chains_on_CPU8x1_continued.rds")) # TODO: change to 100k
out_calib_s230DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen230_DREAMzs-10000-0iter_8x3chains_on_CPU8x1_continued.rds")) # TODO: change to 100k



## Figure E: MCMC convergence diagnostics ----
## trace plots (of chains), correlation plots, Gelman-Rubin (r.1.1)
plot_mcmc_trace(out_calib_s220DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 30000) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s220DREAMzs.png")
plot_mcmc_trace(out_calib_s221DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 30000) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s221DREAMzs.png")
plot_mcmc_trace(out_calib_s222DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 30000) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s222DREAMzs.png")
plot_mcmc_trace(out_calib_s223DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 30000) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s223DREAMzs.png")
plot_mcmc_trace(out_calib_s224DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 2000) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s224DREAMzs.png")
plot_mcmc_trace(out_calib_s225DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 2000) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s225DREAMzs.png")
plot_mcmc_trace(out_calib_s226DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 30000) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s226DREAMzs.png")
plot_mcmc_trace(out_calib_s227DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 30000) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s227DREAMzs.png")
plot_mcmc_trace(out_calib_s228DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 30000) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s228DREAMzs.png")
plot_mcmc_trace(out_calib_s229DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 3000) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s229DREAMzs.png")
plot_mcmc_trace(out_calib_s230DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 3000) |> ggsave_and_return("fig_E_MCMCconvergence_trace_s230DREAMzs.png")


## Figure E2: Posterior parameter correlation analysis ----
## correlation plots
save_corr_plot <- function(out_calib, thin, start, filename){
  png(filename, width = 18, height = 18, units = "cm", res = 400)
  correlationPlot(out_calib$mod, thin = thin, start = start)
  dev.off()
}

save_corr_plot(out_calib_s220DREAMzs,thin=5,start=30000,         filename = here::here("fig/fig_E2_MCMCconvergence_corr_s220DREAMzs_burnin30000.png"))
save_corr_plot(out_calib_s221DREAMzs,thin=5,start=30000,         filename = here::here("fig/fig_E2_MCMCconvergence_corr_s221DREAMzs_burnin30000.png"))
save_corr_plot(out_calib_s222DREAMzs,thin=5,start=18000,         filename = here::here("fig/fig_E2_MCMCconvergence_corr_s222DREAMzs_burnin18000.png"))
save_corr_plot(out_calib_s223DREAMzs,thin=5,start=30000,         filename = here::here("fig/fig_E2_MCMCconvergence_corr_s223DREAMzs_burnin30000.png"))
save_corr_plot(out_calib_s224DREAMzs,thin=5,start=2000,         filename = here::here("fig/fig_E2_MCMCconvergence_corr_s224DREAMzs_burnin2000.png"))
save_corr_plot(out_calib_s225DREAMzs,thin=5,start=2000,         filename = here::here("fig/fig_E2_MCMCconvergence_corr_s225DREAMzs_burnin2000.png"))
save_corr_plot(out_calib_s226DREAMzs,thin=5,start=30000,         filename = here::here("fig/fig_E2_MCMCconvergence_corr_s226DREAMzs_burnin30000.png"))
save_corr_plot(out_calib_s227DREAMzs,thin=5,start=30000,         filename = here::here("fig/fig_E2_MCMCconvergence_corr_s227DREAMzs_burnin30000.png"))
save_corr_plot(out_calib_s228DREAMzs,thin=5,start=30000,         filename = here::here("fig/fig_E2_MCMCconvergence_corr_s228DREAMzs_burnin30000.png"))
save_corr_plot(out_calib_s229DREAMzs,thin=5,start=3000,         filename = here::here("fig/fig_E2_MCMCconvergence_corr_s229DREAMzs_burnin1800.png"))
save_corr_plot(out_calib_s230DREAMzs,thin=5,start=3000,         filename = here::here("fig/fig_E2_MCMCconvergence_corr_s230DREAMzs_burnin1800.png"))



## Figure A: prior, posterior density plot ----
## for each scenario x params

### indivdiual plots: ----
pl_post_s220DR<-(plot_prior_posterior_density(out_calib_s220DREAMzs$mod, burnin_to_skip = 30000)   + ggtitle("Scenario 220")+ ggtitle(out_calib_s220DREAMzs$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s220DREAMzs.png")
pl_post_s221DR<-(plot_prior_posterior_density(out_calib_s221DREAMzs$mod, burnin_to_skip = 30000)   + ggtitle("Scenario 221")+ ggtitle(out_calib_s221DREAMzs$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s221DREAMzs.png")
pl_post_s222DR<-(plot_prior_posterior_density(out_calib_s222DREAMzs$mod, burnin_to_skip = 18000)   + ggtitle("Scenario 222")+ ggtitle(out_calib_s222DREAMzs$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s222DREAMzs.png")
pl_post_s223DR<-(plot_prior_posterior_density(out_calib_s223DREAMzs$mod, burnin_to_skip = 30000)   + ggtitle("Scenario 223")+ ggtitle(out_calib_s223DREAMzs$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s223DREAMzs.png")
pl_post_s224DR<-(plot_prior_posterior_density(out_calib_s224DREAMzs$mod, burnin_to_skip = 3000)   + ggtitle("Scenario 224")+ ggtitle(out_calib_s224DREAMzs$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s224DREAMzs.png")
pl_post_s225DR<-(plot_prior_posterior_density(out_calib_s225DREAMzs$mod, burnin_to_skip = 3000)   + ggtitle("Scenario 225")+ ggtitle(out_calib_s225DREAMzs$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s225DREAMzs.png")
pl_post_s226DR<-(plot_prior_posterior_density(out_calib_s226DREAMzs$mod, burnin_to_skip = 30000)   + ggtitle("Scenario 226")+ ggtitle(out_calib_s226DREAMzs$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s226DREAMzs.png")
pl_post_s227DR<-(plot_prior_posterior_density(out_calib_s227DREAMzs$mod, burnin_to_skip = 30000)   + ggtitle("Scenario 227")+ ggtitle(out_calib_s227DREAMzs$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s227DREAMzs.png")
pl_post_s228DR<-(plot_prior_posterior_density(out_calib_s228DREAMzs$mod, burnin_to_skip = 30000)   + ggtitle("Scenario 228")+ ggtitle(out_calib_s228DREAMzs$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s228DREAMzs.png")
pl_post_s229DR<-(plot_prior_posterior_density(out_calib_s229DREAMzs$mod, burnin_to_skip = 3000)   + ggtitle("Scenario 229")+ ggtitle(out_calib_s229DREAMzs$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s229DREAMzs.png")
pl_post_s230DR<-(plot_prior_posterior_density(out_calib_s230DREAMzs$mod, burnin_to_skip = 3000)   + ggtitle("Scenario 230")+ ggtitle(out_calib_s230DREAMzs$fpath)) |> ggsave_and_return("fig_A_MCMCconvergence_posterior_s230DREAMzs.png")

### single comparison plot: ----
param_order6 <- out_calib_s223DREAMzs$mod[[1]]$setup$names
param_order6 <- c(param_order6,
                  # add fixed parameters (in 300s these should be fixed to 0)
                  "errscale_gpp","errbias_bigD13C", "errbias_vj"
                  ) |> unique()
scenarios_to_compare6 <- list("Prior 220" = out_calib_s220DREAMzs$mod,
                              "Prior 221" = out_calib_s221DREAMzs$mod,
                              "Prior 222" = out_calib_s222DREAMzs$mod,
                              "Prior 223" = out_calib_s223DREAMzs$mod,
                              # "Prior 224" = out_calib_s224DREAMzs$mod,
                              "Prior 226" = out_calib_s226DREAMzs$mod,
                              "Prior 227" = out_calib_s227DREAMzs$mod,
                              "Prior 228" = out_calib_s228DREAMzs$mod,
                              "Prior 229" = out_calib_s229DREAMzs$mod,
                              "Prior 230"= out_calib_s230DREAMzs$mod,
                              "220" = out_calib_s220DREAMzs$mod,
                              "221" = out_calib_s221DREAMzs$mod,
                              "222" = out_calib_s222DREAMzs$mod,
                              "223" = out_calib_s223DREAMzs$mod,
                              # "224" = out_calib_s224DREAMzs$mod,
                              "226" = out_calib_s226DREAMzs$mod,
                              "227" = out_calib_s227DREAMzs$mod,
                              "228" = out_calib_s228DREAMzs$mod,
                              "229" = out_calib_s229DREAMzs$mod,
                              "230"= out_calib_s230DREAMzs$mod
                              )
# this is for retrieval of correct scenario definition for fixed parameters, in spite of renaming
# correct_scenarios6 <- c("10"=230, "9"=229, "8"=228, "7"=227, "6"=226, "3"=223, "2"=222, "1"=221, "0"=220)
pl_post_comparison6c <- plot_prior_posterior_density_compare2(
  named_list_scen =  scenarios_to_compare6[c("Prior 221","Prior 222","Prior 223","Prior 227","Prior 228", "221","222","223","227","228")],
  burnin_to_skip  = 3000,
  add_MAP = TRUE, param_order = param_order6, params_not_to_plot = c("rd_to_vcmax", "soilm_betao", "errbias_bigD13C", "errbias_vj", "errscale_gpp"))

pl_post_comparison6d <- plot_prior_posterior_density_compare2(
  named_list_scen =  scenarios_to_compare6[c("Prior 221","Prior 222","Prior 223","Prior 226","Prior 227","Prior 228", "221","222","223","226","227","228")],
  burnin_to_skip  = 3000,
  add_MAP = TRUE, param_order = param_order6, params_not_to_plot = c("rd_to_vcmax", "soilm_betao", "errbias_bigD13C", "errbias_vj", "errscale_gpp"))

pl_post_comparison6e <- plot_prior_posterior_density_compare2(
  named_list_scen =  scenarios_to_compare6,
  burnin_to_skip  = 3000,
  add_MAP = TRUE, param_order = param_order6, params_not_to_plot = c("rd_to_vcmax", "soilm_betao", "errbias_bigD13C", "errbias_vj", "errscale_gpp"))

pl_post_comparison6f <- plot_prior_posterior_density_compare2(
  named_list_scen =  scenarios_to_compare6[c("Prior 222","Prior 223","Prior 226","Prior 227","Prior 228","Prior 230",
                                                   "222",      "223",      "226",      "227",      "228",      "230")],
  burnin_to_skip  = 3000,
  add_MAP = TRUE, param_order = param_order6, params_not_to_plot = c("rd_to_vcmax", "soilm_betao", "errbias_bigD13C", "errbias_vj", "errscale_gpp"))

ggsave_and_return(pl_post_comparison6c, "fig_A_MCMCconvergence_posterior_labelled_s221DR_222DR_223DR_227DR_228DR.png",         width = 7.2, height = 3.6)
ggsave_and_return(pl_post_comparison6d, "fig_A_MCMCconvergence_posterior_labelled_s221DR_222DR_223DR_226DR_227DR_228DR.png",   width = 7.2, height = 3.6)
ggsave_and_return(pl_post_comparison6e, "fig_A_MCMCconvergence_posterior_labelled_s220DR_222DR_..._228DR.png",                 width = 7.2, height = 3.6)
ggsave_and_return(pl_post_comparison6f, "fig_A_MCMCconvergence_posterior_labelled_s222DR_223DR_226DR_227DR_228DR_229DR.png",   width = 7.2, height = 3.6)




## Figure F: TBD: comparison of calibration vs GenSA?? ----
## or just using prior estimates from Stocker 2020? (r.1.14)

## Plot/output runtimes of MCMC samplings (calibrations) ----
# TODO


## Table b: prior ranges and MAP of estimated params ----
caption <- paste(
  "Parameter listing including prior and Maximum A Posteriori (MAP) estimates.",
  "The bounds of uniform or truncated normal prior distributions are given in square brackets.",
  "Parameters that were held fixed for the calibration are marked with a single number in brackets and an asterisk (*)")

scenarios_to_compare <- scenario_labels |> filter(scenario %in% c(228,227,226,222,223,230))

# get all priors as data.frame:
par_priors_df <- lapply(scenarios_to_compare$scenario, \(scen) {
  bind_rows(lapply(setup_rsofun_calibration(scen)$par, as.data.frame), .id = "Parameter") |> mutate(scenario = scen)
}) |> bind_rows()

# get all fixed values as data.frame()
par_fixed_df <- lapply(scenarios_to_compare$scenario, \(scen) {
  as.data.frame(setup_rsofun_calibration(scen)$par_fixed) |> pivot_longer(everything(), names_to = "Parameter", values_to="fixed_value") |> mutate(scenario = scen)
}) |> bind_rows()


# get all MAP as data.frame()
par_MAP_df <- bind_rows(
  data.frame(MAP = MAP(out_calib_s222DREAMzs$mod, start = 30000)$parametersMAP) |> tibble::rownames_to_column("Parameter") |> mutate(scenario = 222),
  data.frame(MAP = MAP(out_calib_s223DREAMzs$mod, start = 30000)$parametersMAP) |> tibble::rownames_to_column("Parameter") |> mutate(scenario = 223),
  data.frame(MAP = MAP(out_calib_s226DREAMzs$mod, start = 30000)$parametersMAP) |> tibble::rownames_to_column("Parameter") |> mutate(scenario = 226),
  data.frame(MAP = MAP(out_calib_s227DREAMzs$mod, start = 30000)$parametersMAP) |> tibble::rownames_to_column("Parameter") |> mutate(scenario = 227),
  data.frame(MAP = MAP(out_calib_s228DREAMzs$mod, start = 3000)$parametersMAP) |> tibble::rownames_to_column("Parameter") |> mutate(scenario = 228),
  data.frame(MAP = MAP(out_calib_s230DREAMzs$mod, start = 3000)$parametersMAP) |> tibble::rownames_to_column("Parameter") |> mutate(scenario = 230)
)
stopifnot(all(sort(unique(par_MAP_df$scenario)) == sort(unique(par_fixed_df$scenario))))


# format the priors:
par_priors_df |> left_join(scenario_labels)
par_prior_s1_3 <- par_priors_df |>
  # format priors:
  mutate(format_decimals = case_when(
    Parameter %in% c()                        ~ "[%.0f to %.0f]",
    Parameter %in% c("kphio_par_b","soilm_thetastar","tau_acclim",
                     "beta_unitcostratio")    ~ "[%.1f to %.1f]",
    Parameter %in% c("kphio", "err_gpp", "rd_to_vcmax", "soilm_betao",
                     "err_bigD13C", "err_vj", "errbias_bigD13C", "errbias_vj","errscale_gpp",
                     "kc_jmax")               ~ "[%.2f to %.2f]",
    Parameter %in% c("kphio_par_a")           ~ "[%.3f to %.3f]",
    TRUE ~                                      "[%.3f to %.3f]")) |>
  mutate(prior_value = sprintf(format_decimals, lower, upper)) |> select(-format_decimals) |>
  # replace normal distributions: \mathcal{N}(\mu,\,\sigma^{2})
  rowwise() |> mutate(prior_value = ifelse(!is.na(sd), sprintf("$\\mathcal{N}(%.1f,\\,%.1f^{2})$\\tnote{a} %s",mean,sd,prior_value),prior_value))

# format the fixed params:
par_fixed_df |> left_join(scenario_labels)
par_fix_s1_3 <- par_fixed_df |>
  # format fixed values
  mutate(format_decimals = case_when(
    Parameter %in% c()                        ~ "[%.0f]*",
    Parameter %in% c("kphio_par_b","soilm_thetastar","tau_acclim",
                     "beta_unitcostratio")    ~ "[%.1f]*",
    Parameter %in% c("kphio", "err_gpp", "rd_to_vcmax", "soilm_betao",
                     "err_bigD13C", "err_vj", "errbias_bigD13C", "errbias_vj","errscale_gpp",
                     "kc_jmax")               ~ "[%.2f]*",
    Parameter %in% c("kphio_par_a")           ~ "[%.3f]*",
    TRUE ~                                      "[%.3f]*")) |>
  mutate(fixed_value = sprintf(format_decimals, fixed_value)) |> select(-format_decimals)

# format the MAP params:
par_MAP_s1_3 <- par_MAP_df |>
  # format MAP values
  mutate(format_decimals = case_when(
    Parameter %in% c()                        ~ "%.0f",
    Parameter %in% c("kphio_par_b","soilm_thetastar","tau_acclim",
                     "beta_unitcostratio")    ~ "%.1f",
    Parameter %in% c("kphio", "err_gpp", "rd_to_vcmax", "soilm_betao",
                     "err_bigD13C", "err_vj", "errbias_bigD13C", "errbias_vj","errscale_gpp",
                     "kc_jmax")               ~ "%.2f",
    Parameter %in% c("kphio_par_a")           ~ "%.3f",
    TRUE ~                                      "%.3f")) |>
  mutate(MAP = sprintf(format_decimals, MAP)) |> select(-format_decimals)

# RETRY FORMATTING:
# Symbol|Parameter name|Description|S1,map,prior|S2,map,prior|S3,map,prior

caption_v2 <- paste(
  "Parameter listing including Maximum A Posteriori (MAP) estimates and prior distributions.",
  "The bounds of uniform or truncated normal prior distributions are given in square brackets.",
  "Parameters that were held fixed for the calibration are marked with a single number in brackets and an asterisk (*)")

table_b_v2 <-
  left_join(
    bind_rows(par_prior_s1_3,
              par_fix_s1_3 |> rename(prior_value = fixed_value)),
    par_MAP_s1_3,
    by = join_by(Parameter, scenario)) |>
  # format text
  mutate(cell_text = paste0(MAP, "\\newline", prior_value)) |>
  select(Parameter, scenario, cell_text) |>
  # get correct labels of scenarios and parameters:
    ## replace scenario number with label
    left_join(scenario_labels |> select(scenario, label),
              by = join_by(scenario)) |> select(-scenario) |> rename(scenario = label) |>
  pivot_wider(names_from = scenario, values_from = cell_text, names_glue = "Scen. {scenario}\\newline MAP\\newline [Prior]") |>
  # append Symbol and Description:
  mutate(Parameter = factor(Parameter, levels = levels(rsofun_symbol_parname_description$Parameter))) |>
  left_join(rsofun_symbol_parname_description,
            by = join_by(Parameter)) |>
  arrange(Parameter) |>
  select(Symbol_tex, Units_tex, Parameter,
         Description, starts_with("Scen. ") )

# remove some parameters:
table_b_v2_reduced <- table_b_v2 |> filter(!(Parameter %in% c("rd_to_vcmax", "soilm_betao")))

# export to LaTeX
fname  <- paste0("table-b_parameters_scen_", paste0(scenarios_to_compare$scenario, collapse = "-"), ".tex")
fname2 <- paste0("table-b_parameters_scen_", paste0(scenarios_to_compare$scenario, collapse = "-"), "_reduced.tex")

table_b_v2 %>%
  # mutate(Parameter = gsub("\\_","\\\\_",Parameter)) %>% # format for LaTeX
  select(-Parameter) %>% # use Symbol instead of Parameter(code)
  xtable::xtable(x = .,
                 caption = caption_v2,
                 align = rep("l", (ncol(x = .) + 1))  # make all columns left-aligned # align="rXXXXXX" # make use of tabularx "X"-column
                 # align = "p{0.7cm} p{1.4cm} p{5.5cm} X X X" # TODO: change later to:
  ) %>%
  print(x = .,
    file = here::here("fig", fname),
    floating.environment = "threeparttable",
    caption.placement = "top", tabular.environment = "tabularx", width="\\textwidth",
    include.rownames = FALSE,
    sanitize.text.function=function(x){x} # override normal sanitizing function since we have defined tex
  ) # this can be added to tex file as: \input{filename.tex})

table_b_v2_reduced %>%
  # mutate(Parameter = gsub("\\_","\\\\_",Parameter)) %>% # format for LaTeX
  select(-Parameter) %>% # use Symbol instead of Parameter(code)
  xtable::xtable(x = .,
                 caption = caption,
                 align = rep("l", (ncol(x = .) + 1))  # make all columns left-aligned # align="rXXXXXX" # make use of tabularx "X"-column
                 # align = "p{0.7cm} p{1.4cm} p{5.5cm} X X X" # TODO: change later to:
  ) %>%
  print(x = .,
    file = here::here("fig", fname2),
    floating.environment = "threeparttable",
    caption.placement = "top", tabular.environment = "tabularx", width="\\textwidth",
    include.rownames = FALSE,
    sanitize.text.function=function(x){x} # override normal sanitizing function since we have defined tex
  ) # this can be added to tex file as: \input{filename.tex})







# PREDICTION PLOTS ----
# TO RUN PREDICTINOS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions2.sh 220 100000 30000 100 1 "_continued.rds"
# TO RUN PREDICTINOS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions2.sh 221 100000 30000 100 1 "_continued.rds"
# TO RUN PREDICTINOS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions2.sh 222 60000  18000 100 1 "_continued.rds"
# TO RUN PREDICTINOS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions2.sh 223 100000 30000 100 1 "_continued.rds"
# TO RUN PREDICTINOS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions2.sh 224 8000   2000 100 1 "_continued.rds"
# TO RUN PREDICTINOS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions2.sh 225 8000   2000 100 1 "_continued.rds"
# TO RUN PREDICTINOS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions2.sh 226 100000 30000 100 1 "_continued.rds"
# TO RUN PREDICTINOS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions2.sh 227 100000 30000 100 1 "_continued.rds"
# TO RUN PREDICTINOS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions2.sh 228 100000 30000 100 1 "_continued.rds"
# TO RUN PREDICTINOS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions2.sh 229 10000  3000 100 1 "_continued.rds"
# TO RUN PREDICTINOS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions2.sh 230 10000  3000 100 1 "_continued.rds"


## Figure B2: error distribution predObs scatter plot ----
## for each scenario x target x test

flag_plot_predictions <- TRUE # possibility to switch this off

# define what data to load (and use this as suffix for output)
n_post <- "N20+MAP"
n_err <- "_N4errors"
outfname_suffix <- paste0(n_post, n_err, "_s222-s229")
if (flag_plot_predictions){
  # Load sampled posterior params used for predictions
  df_222_params  <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_18000burnin__out_calib__scen222_DREAMzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds"))
  df_223_params  <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_30000burnin__out_calib__scen223_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds"))
  df_226_params  <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_30000burnin__out_calib__scen226_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds"))
  df_227_params  <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_30000burnin__out_calib__scen227_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds"))
  df_228_params  <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_30000burnin__out_calib__scen228_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds"))

  # Load predictions for plotting
  df_222_vj      <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_18000burnin__out_calib__scen222_DREAMzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds"))
  df_222_bigD13C <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_18000burnin__out_calib__scen222_DREAMzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds"))
  df_222_gpp     <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_18000burnin__out_calib__scen222_DREAMzs-60000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds"))
  df_223_vj      <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_30000burnin__out_calib__scen223_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds"))
  df_223_bigD13C <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_30000burnin__out_calib__scen223_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds"))
  df_223_gpp     <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_30000burnin__out_calib__scen223_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds"))
  df_226_vj      <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_30000burnin__out_calib__scen226_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds"))
  df_226_bigD13C <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_30000burnin__out_calib__scen226_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds"))
  df_226_gpp     <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_30000burnin__out_calib__scen226_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds"))
  df_227_vj      <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_30000burnin__out_calib__scen227_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds"))
  df_227_bigD13C <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_30000burnin__out_calib__scen227_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds"))
  df_227_gpp     <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_30000burnin__out_calib__scen227_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds"))
  df_228_vj      <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_30000burnin__out_calib__scen228_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds"))
  df_228_bigD13C <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_30000burnin__out_calib__scen228_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds"))
  df_228_gpp     <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_30000burnin__out_calib__scen228_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds"))
  df_229_vj      <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_3000burnin__out_calib__scen229_DREAMzs-10000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds"))
  df_229_bigD13C <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_3000burnin__out_calib__scen229_DREAMzs-10000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds"))
  df_229_gpp     <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_3000burnin__out_calib__scen229_DREAMzs-10000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds"))
  df_230_vj      <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_3000burnin__out_calib__scen228_DREAMzs-10000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds"))
  df_230_bigD13C <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_3000burnin__out_calib__scen228_DREAMzs-10000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds"))
  df_230_gpp     <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_N20+MAP_3000burnin__out_calib__scen228_DREAMzs-10000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds"))
}

source(here::here("R/analyse_modobs2.R"))

my_own_scatter <- function(df, mod, ...){
  stopifnot(mod %in% c("mod_biasremoved_no_err", "mod_biasremoved_with_err"))
  stopifnot(nrow(df) > 0)

  # (MAP, Posterior, Posterior+Error)
  # MAP:             is posterior_sample_id==0
  #                  e.g. filter(df_gpp, is_train0_test1 == 1,     is_MAP, error_sample_id==1)
  # Posterior:       just take one error sampling, but all (~25 posterior samples)
  #                  e.g. filter(df_gpp, is_train0_test1 == 1,             error_sample_id==1),
  # Posterior+Error: take all errors
  #                  e.g. filter(df_gpp, is_train0_test1 == 1,             error_sample_id>=1)
  analyse_modobs2(df, mod = mod, obs = "obs", type = "hex", ...)
}

gpp_labs <- list(labs(x = expression(paste("Predicted GPP (g C m"^-2, "s"^-1, ")")),
                      y = expression(paste("Observed GPP (g C m"^-2, "s"^-1, ")"))))
vj_labs <- list(labs(x = paste("Predicted Vcmax/Jmax (-)"),
                     y = paste("Observed Vcmax/Jmax (-)")))
bigD13C_labs <- list(labs(x = paste("Predicted Δ13C (permil)"),
                          y = paste("Observed Δ13C (permil)")))

gpp_labs_xNULL     <- list(labs(x = " ", y = expression(paste("Observed GPP (g C m"^-2, "s"^-1, ")"))))
vj_labs_xNULL      <- list(labs(x = " ", y = paste("Observed Vcmax/Jmax (-)")))
bigD13C_labs_xNULL <- list(labs(x = " ", y = paste("Observed Δ13C (permil)")))


# compute scatters (and skills) for MAP of test set:
list_of_scenarios_to_loop_over <- list(
  s228 = list("bigD13C" = df_228_bigD13C, "vj" = df_228_vj, "gpp" = df_228_gpp),
  s227 = list("bigD13C" = df_227_bigD13C, "vj" = df_227_vj, "gpp" = df_227_gpp),
  s226 = list("bigD13C" = df_226_bigD13C, "vj" = df_226_vj, "gpp" = df_226_gpp),
  s222 = list("bigD13C" = df_222_bigD13C, "vj" = df_222_vj, "gpp" = df_222_gpp),
  s223 = list("bigD13C" = df_223_bigD13C, "vj" = df_223_vj, "gpp" = df_223_gpp),
  # s229 = list("bigD13C" = df_229_bigD13C, "vj" = df_229_vj, "gpp" = df_229_gpp)
  s230 = list("bigD13C" = df_230_bigD13C, "vj" = df_230_vj, "gpp" = df_230_gpp)
)


for (parameter_set in c("MAP", "Posterior")){
  list_of_scatters <- lapply(list_of_scenarios_to_loop_over, function(list_of_targets){
    lapply(list_of_targets, function(df_target_prediction){
      my_own_scatter(
        df_target_prediction %>% {
          if (parameter_set == "MAP") filter(., is_MAP) else .
          } %>% filter(error_sample_id==1, is_train0_test1 == 1),
        mod = "mod_biasremoved_no_err",
        shortsubtitle = TRUE)
    })
  })

  ### indivdiual plots for each scenario: ----
  pl_scatter_228 <- cowplot::plot_grid(
    nrow = 1,
    list_of_scatters$s228$bigD13C$gg + bigD13C_labs + labs(title = "Scenario 228"),
    list_of_scatters$s228$vj$gg      + vj_labs      + labs(title = "Scenario 228"),
    list_of_scatters$s228$gpp$gg     + gpp_labs     + labs(title = "Scenario 228"))
  ggsave(plot = pl_scatter_228, filename = here::here("fig", paste0("fig_B2c_pred-vs-obs_s228_pred",outfname_suffix,"_", parameter_set, ".png")),
         width = 12, height = 4, units = "cm", dpi = "print", scale = 2.0)
  # etc....

  ### single comparison plot: ----
  mark_as_target <- theme(panel.background = element_rect(fill = t_col("darkgreen", 80)))
  pl_scatter_comparison <- cowplot::plot_grid(
    ncol = 3, byrow = TRUE,
    list_of_scatters$s228$bigD13C$gg + mark_as_target + bigD13C_labs_xNULL ,#+ labs(title = "Scenario 228"),
    list_of_scatters$s228$vj$gg                       + vj_labs_xNULL      ,#+ labs(title = "Scenario 228"),
    list_of_scatters$s228$gpp$gg                      + gpp_labs_xNULL     ,#+ labs(title = "Scenario 228"),

    list_of_scatters$s227$bigD13C$gg                  + bigD13C_labs_xNULL ,#+ labs(title = "Scenario 227"),
    list_of_scatters$s227$vj$gg      + mark_as_target + vj_labs_xNULL      ,#+ labs(title = "Scenario 227"),
    list_of_scatters$s227$gpp$gg                      + gpp_labs_xNULL     ,#+ labs(title = "Scenario 227"),

    list_of_scatters$s226$bigD13C$gg + mark_as_target + bigD13C_labs_xNULL ,#+ labs(title = "Scenario 226"),
    list_of_scatters$s226$vj$gg      + mark_as_target + vj_labs_xNULL      ,#+ labs(title = "Scenario 226"),
    list_of_scatters$s226$gpp$gg                      + gpp_labs_xNULL     ,#+ labs(title = "Scenario 226"),

    list_of_scatters$s222$bigD13C$gg                  + bigD13C_labs_xNULL ,#+ labs(title = "Scenario 222"),
    list_of_scatters$s222$vj$gg                       + vj_labs_xNULL      ,#+ labs(title = "Scenario 222"),
    list_of_scatters$s222$gpp$gg     + mark_as_target + gpp_labs_xNULL     ,#+ labs(title = "Scenario 222"),

    list_of_scatters$s223$bigD13C$gg + mark_as_target + bigD13C_labs ,#+ labs(title = "Scenario 223"),
    list_of_scatters$s223$vj$gg      + mark_as_target + vj_labs      ,#+ labs(title = "Scenario 223"),
    list_of_scatters$s223$gpp$gg     + mark_as_target + gpp_labs      #+ labs(title = "Scenario 223")
    #
    # list_of_scatters$s229$bigD13C$gg + bigD13C_labs + labs(title = "Scenario 229"),
    # list_of_scatters$s229$vj$gg      + vj_labs      + labs(title = "Scenario 229"),
    # list_of_scatters$s229$gpp$gg     + gpp_labs     + labs(title = "Scenario 229"),
  )

  ggsave(plot = pl_scatter_comparison,
         filename = here::here("fig", paste0("fig_B2d_pred-vs-obs_four-scen_pred",outfname_suffix,"_", parameter_set, ".png")),
         width = 12, height = 12/3*5, units = "cm", dpi = "print", scale = 2.0)
}





# Alternative layout
pl_scatter_comparison2 <- cowplot::plot_grid(nrow = 3, byrow=FALSE, labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
    cowplot::plot_grid(ncol = 3, # labels = c("1", "2", "3"),
      list_of_scatters$s228$bigD13C$gg + mark_as_target + bigD13C_labs_xNULL ,
      list_of_scatters$s228$vj$gg                       + vj_labs_xNULL      ,
      list_of_scatters$s228$gpp$gg                      + gpp_labs_xNULL     ),
    cowplot::plot_grid(ncol = 3,
      list_of_scatters$s227$bigD13C$gg                  + bigD13C_labs_xNULL ,
      list_of_scatters$s227$vj$gg      + mark_as_target + vj_labs_xNULL      ,
      list_of_scatters$s227$gpp$gg                      + gpp_labs_xNULL     ),
    cowplot::plot_grid(ncol = 3,
      list_of_scatters$s222$bigD13C$gg                  + bigD13C_labs ,
      list_of_scatters$s222$vj$gg                       + vj_labs      ,
      list_of_scatters$s222$gpp$gg     + mark_as_target + gpp_labs     ),

    cowplot::plot_grid(ncol = 3,
      list_of_scatters$s226$bigD13C$gg + mark_as_target + bigD13C_labs_xNULL ,
      list_of_scatters$s226$vj$gg      + mark_as_target + vj_labs_xNULL      ,
      list_of_scatters$s226$gpp$gg                      + gpp_labs_xNULL     ),
    cowplot::plot_grid(ncol = 3,
      list_of_scatters$s223$bigD13C$gg + mark_as_target + bigD13C_labs_xNULL ,
      list_of_scatters$s223$vj$gg      + mark_as_target + vj_labs_xNULL      ,
      list_of_scatters$s223$gpp$gg     + mark_as_target + gpp_labs_xNULL     ),
    cowplot::plot_grid(ncol = 3,
      list_of_scatters$s223$bigD13C$gg + mark_as_target + bigD13C_labs ,
      list_of_scatters$s223$vj$gg      + mark_as_target + vj_labs      ,
      list_of_scatters$s223$gpp$gg     + mark_as_target + gpp_labs     )
    # TODO: replace with 229 or 230:
    # list_of_scatters$s229$bigD13C$gg + bigD13C_labs,
    # list_of_scatters$s229$vj$gg      + vj_labs     ,
    # list_of_scatters$s229$gpp$gg     + gpp_labs    ,
  )
pl_scatter_comparison2
ggsave(plot = pl_scatter_comparison2,
         filename = here::here("fig", paste0("fig_B2e_pred-vs-obs_four-scen_pred",outfname_suffix,"_", parameter_set, ".png")),
         width = 12, height = 6, units = "cm", dpi = "print", scale = 4.0)






## Figure B: error distribution density plot ----
## for each scenario x target x test+train
# Plot error as as second density plots (no-fill, dashed lines)
# pl_density_alltargets <- ggplot(
#     bind_rows(df_B1_density$vj, df_B1_density$bigD13C, df_B1_density$gpp) |>
#       mutate(target = factor(target, levels = c("gpp","vj","bigD13C"))),
#     aes(x = modelled - obs, y = interaction(dataset, Scenario))) +
#   # add Posterior (fill):
#   ggridges::geom_density_ridges( data = function(df) {df |> filter(y_facet == "Posterior")},
#     mapping = aes(fill = dataset), # linetype = dataset
#     scale = 0.8) +
#   # add error (solid):
#   ggridges::geom_density_ridges( data = function(df) {df |> filter(y_facet == "Post.+Error")},
#     mapping = aes(color = dataset, linetype = "Post.+Error"),
#     scale = 0.8, fill = NA, key_glyph = "timeseries") + # "polygon"
#   # # add MAP (dashed):
#   # ggridges::geom_density_ridges( data = function(df) {df |> filter(y_facet == "MAP")},
#   #   mapping = aes(color = dataset, linetype = "MAP"),
#   #   scale = 0.8, fill = NA, key_glyph = "timeseries") + # "polygon"
#   # layout:
#   facet_grid(y_facet ~ target+Scenario+dataset, labeller = labeller("Scenario" = label_both)) +
#   labs(x=expression(paste("Predicted - Observed"))) +
#   scale_fill_manual(NULL, aesthetics = c("fill", "colour"),
#                     values = c("test"="#29a274ff",
#                                "train" = t_col("#777055ff"))) +
#   scale_linetype_manual(NULL, values = c("Post.+Error" = "3313",
#                                          "MAP"         = "3232",
#                                          "fixed"       = "solid")) +
#   theme_classic() +
#   theme(legend.position        = "inside",
#         legend.position.inside = c(0.02,0.02),
#         legend.justification   = c(0,0),
#         legend.direction       = "vertical",
#         legend.box             = "horizontal",
#         legend.background      = element_blank()) +
#   theme(panel.grid.minor.x = element_line()) +
#   scale_x_continuous(minor_breaks = 0.00001) + # 0 makes it disappear
#   labs(linetype=NULL)
# pl_density_alltargets
# pl_density_alltargets + facet_grid(~Scenario+dataset)
# pl_density_alltargets + facet_null()

# pl_density_alltargets_v2 <- pl_density_alltargets +
#   aes(y=dataset) + labs(y=NULL) +
#   facet_grid(Scenario~target, labeller = labeller("Scenario" = label_both), scales = "free_x") +
#   scale_y_discrete(limits = rev)

dat_to_plot <- bind_rows(
    df_B1_density$vj,
    df_B1_density$bigD13C,
    df_B1_density$gpp
  ) |>
  mutate(target = factor(target, levels = c("gpp","vj","bigD13C")))

dat_to_plot_avgObs <- bind_rows(
    df_B1_density_avgObs$gpp |> select(names(df_B1_density_avgObs$bigD13C)),
    df_B1_density_avgObs$vj,
    df_B1_density_avgObs$bigD13C
  ) |>
  mutate(target = factor(target, levels = c("gpp","vj","bigD13C")))

pl_density_alltargets_v3 <- ggplot(dat_to_plot, aes(x = modelled - obs, y = Scenario)) +
  scale_y_discrete(limits = rev) +
  # add Posterior (fill):
  ggridges::geom_density_ridges( data = function(df) {df |> filter(y_facet == "Posterior")},
    mapping = aes(fill = dataset), # linetype = dataset
    scale = 0.8) +
  # add error (solid):
  ggridges::geom_density_ridges( data = function(df) {df |> filter(y_facet == "Post.+Error")},
    mapping = aes(color = dataset, linetype = "Post.+Error"),
    scale = 0.8, fill = NA, key_glyph = "timeseries") + # "polygon" or "timeseries"
  # # add MAP (dashed):
  # ggridges::geom_density_ridges( data = function(df) {df |> filter(y_facet == "MAP")},
  #   mapping = aes(color = dataset, linetype = "MAP"),
  #   scale = 0.8, fill = NA, key_glyph = "timeseries") + # "polygon" or "timeseries"
  # layout:
  scale_fill_manual(NULL, aesthetics = c("fill", "colour"), values = c("test"="#29a274ff", "train" = t_col("#777055ff"))) +
  # scale_linetype_manual(NULL, values = c("Post.+Error" = "3313", "MAP"         = "3232", "fixed"       = "solid")) +
  scale_linetype_manual(NULL, values = c("Post.+Error" = "solid", "MAP"         = "3232", "fixed"       = "solid")) +
  # theme:
  theme_classic() +
  theme(legend.position        = "inside",
        legend.position.inside = c(0.02,0.02),
        legend.justification   = c(0,0),
        legend.direction       = "vertical",
        legend.box             = "horizontal",
        legend.background      = element_blank()) +
  # add line at 0:
  scale_x_continuous(minor_breaks = 0.00001) + # 0 makes it disappear
  theme(panel.grid.minor.x = element_line()) +
  # axis labels and facet grid labels
  labs(x = "Predicted - Observed", linetype = NULL) +
  facet_grid(
    ~target,
    scales = "free_x",
    labeller = as_labeller(c("gpp"     = "(a) GPP:",
                             "vj"      = "(b) Vcmax/Jmax:",
                             "bigD13C" = "(c) Δ13C:"))) +
  theme(strip.background = element_blank(),
        strip.text       = element_text(hjust = 0, size = 12, face = "bold"))

ggsave(here::here(file.path("fig",paste0("fig_B_predObs_errorDensity_s1s4",outfname_suffix,".png"))),
       plot = pl_density_alltargets_v3, width=12, height=8, units = "cm", scale = 1.3)

# redoc plot versus obs_avg instead of obs
pl_density_alltargets_v3_avgObs <- (pl_density_alltargets_v3 %+% dat_to_plot_avgObs) +
  aes(x = modelled - obs_avg, y = Scenario) +
  labs(x = "Predicted - Avg. Observed")
ggsave(here::here(file.path("fig",paste0("fig_B1b_predObs_errorDensity_s1s4",outfname_suffix,".png"))),
       plot = pl_density_alltargets_v3_avgObs, width=12, height=8, units = "cm", scale = 1.3)

# and combine both, arranging axes:
# Extend x-axis limits of pl_density_alltargets_v3_avgObs to be the same as pl_density_alltargets_v3
# by using a geom_blank() layer (source: https://stackoverflow.com/a/21585521/3915004) :
pl_density_alltargets_v3_build <- ggplot2::ggplot_build(pl_density_alltargets_v3)
dummy <- data.frame(
  target = c("gpp", "gpp",
             # "vj_obs__", "vj_obs__",
             # "bigD13C_obs_permil", "bigD13C_obs_permil") |>
             "vj", "vj",
             "bigD13C", "bigD13C") |>
    factor(levels = c("gpp", "vj", "bigD13C")),
  x      = c(pl_density_alltargets_v3_build$layout$get_scales(1)$x$range$range,
             pl_density_alltargets_v3_build$layout$get_scales(2)$x$range$range,
             pl_density_alltargets_v3_build$layout$get_scales(3)$x$range$range),
  y = 1
)

pl_density_alltargets_v3_avgObs_xlimsExtended <- pl_density_alltargets_v3_avgObs + geom_blank(data = dummy, aes(x=x, y=y))
pl_density_alltargets_v3_comparison <- cowplot::plot_grid(
  pl_density_alltargets_v3,
  pl_density_alltargets_v3_avgObs_xlimsExtended +
    facet_grid( ~target, scales = "free_x",
    labeller = as_labeller(c("gpp"     = "(d) GPP:",
                             "vj"      = "(e) Vcmax/Jmax:",
                             "bigD13C" = "(f) Δ13C:"))),
  ncol = 1, rel_heights = c(1,1))
ggsave(here::here(file.path("fig",paste0("fig_B1c_predObs_errorDensity_s1s4",outfname_suffix,".png"))),
     plot = pl_density_alltargets_v3_comparison, width=12, height=16, units = "cm", scale = 1.3)


## Figure B (variant 2): error distribution density plot: ----
# make plots for different targets separately
pl_density_singleTarget_base <- ggplot(filter(dat_to_plot, target == "vj"), aes(x = modelled - obs, y = Scenario)) +
  scale_y_discrete(limits = rev) +
  # add Posterior (fill):
  ggridges::geom_density_ridges( data = function(df) {df |> filter(y_facet == "Posterior")},
    mapping = aes(fill = dataset), scale = 0.8) +
  # add error (solid):
  ggridges::geom_density_ridges( data = function(df) {df |> filter(y_facet == "Post.+Error")},
    mapping = aes(color = dataset, linetype = "Post.+Error"), scale = 0.8, fill = NA, key_glyph = "timeseries") +
  # layout:
  scale_fill_manual(NULL, aesthetics = c("fill", "colour"), values = c("test"="#29a274ff", "train" = t_col("#777055ff"))) +
  scale_linetype_manual(NULL, values = c("Post.+Error" = "solid", "MAP"         = "3232", "fixed"       = "solid")) +
  # theme:
  theme_classic() +
  theme(legend.position        = "inside",
        legend.position.inside = c(0.02,0.02),
        legend.justification   = c(0,0),
        legend.direction       = "vertical",
        legend.box             = "horizontal",
        legend.background      = element_blank()) +
  # add line at 0:
  scale_x_continuous(minor_breaks = 0.00001) + # 0 makes it disappear
  theme(panel.grid.minor.x = element_line()) +
  # axis labels and facet grid labels
  coord_cartesian(xlim = c(-1,1)) +
  labs(x = "Predicted - Observed", linetype = NULL) +
  facet_grid(
    ~target,
    scales = "free_x",
    labeller = as_labeller(c("gpp"     = "(a) GPP:",
                             "vj"      = "(b) Vcmax/Jmax:",
                             "bigD13C" = "(c) Δ13C:"))) +
  theme(strip.background = element_blank(),
        strip.text       = element_text(hjust = 0, size = 12, face = "bold"))

(pl_density_singleTarget_base %+% filter(dat_to_plot, target == "vj")) +
  coord_cartesian(xlim = c(-1,1)) +
  labs(x = "Predicted - Observed Vcmax/Jmax (-) ", linetype = NULL)
(pl_density_singleTarget_base %+% filter(dat_to_plot, target == "bigD13C")) +
  coord_cartesian(xlim = c(-10,10)) +
  labs(x = expression(paste("Predicted - Observed Δ"^13,"C (permil)")), linetype = NULL)
(pl_density_singleTarget_base %+% filter(dat_to_plot, target == "gpp")) +
  coord_cartesian(xlim = c(-5,5)) +
  labs(x = expression(paste("Predicted - Observed GPP (g C m"^-2, "s"^-1, ")")), linetype = NULL)







## Figure B3: make a proper gpp time series plot ----
# plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario == "1",          dataset == "train"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s1_train",outfname_suffix,".png")))
# plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario %in% c("4","3"), dataset == "train"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s4or3_train",outfname_suffix,".png")))
#
# plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario == "1",          dataset == "test"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s1_test",outfname_suffix,".png")))
# plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario %in% c("4","3"), dataset == "test"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s4or3_test",outfname_suffix,".png")))
#
# plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario == "0", dataset == "train"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s0_train",outfname_suffix,".png")))
# plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario == "0", dataset == "test"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s0_test",outfname_suffix,".png")))
#


# SENSITIVITY ANALYSIS: ----

## Figure D: sensitivity bar plot ----
## plots are directly created by 02_sensitivity_analysis.R

