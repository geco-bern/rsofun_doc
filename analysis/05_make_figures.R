# Update this to 1.0.2 on zenodo


# Script that generates figures for manuscript

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
source(here::here("analysis/00_define_setups.R"))

source(here::here("R/calibration_helpers.R"))
source(here::here("R/prediction_helpers.R"))
source(here::here("R/run_prediction_rsofun.R"))

dir.create(here::here("fig"), showWarnings = FALSE)

# Setup re-labelling for final figures: ----

## Re-label Parameters ----
rsofun_symbol_parname_description <- tribble(
  ~Parameter,             ~Symbol_tex,                              ~Units_tex,                     ~Symbol_R,                                                         ~Symbol_R2,                       ~Description,
  # MODEL PARAMETER:
  "kphio",                "$\\varphi_0^{*}$",                       "\\unit{mol\\,mol^{-1}}",       "italic(phi)[0]^'*'*' (mol mol\u207B\u00B9)'",                         "italic(phi)[0]^'*'",             "Quantum yield at optimal temperature" ,
  "kphio_par_a",          "$a_\\varphi$",                           "\\unit{°C^{-2}}",              "italic(a)[italic(phi)]*' (\u00B0C\u207B\u00B2)'",                   "italic(a)[italic(phi)]",         "Shape parameter for the temperature dependence of the quantum yield" ,
  "kphio_par_b",          "$b_\\varphi$",                           "\\unit{°C}",                   "italic(b)[italic(phi)]*' (\u00B0C)'",                        "italic(b)[italic(phi)]",         "Optimal temperature for the quantum yield" ,
  "soilm_thetastar",      "$\\theta^*$",                            "\\unit{mm}",                   "italic(theta)^'*'*' ('*'mm'*')'",                                  "italic(theta)^'*'",              "Soil moisture limitation threshold (eq.~\ref{eq:soilmoisturestress})" ,
  "soilm_betao",          "$\\beta_0$",                             "unitless",                     "italic(beta)[0]*' (-)'",                                           "italic(beta)[0]",                "Stress factor at low soil moisture, intercept for the soil moisture stress function" ,
  "beta_unitcostratio",   "$\\beta$",                               "unitless",                     "italic(beta)*' (-)'",                                              "italic(beta)",                   "Unit cost ratio of carboxylation to transpiration" , # (maintenance of $V_{\\mathrm{cmax}}$)
  "rd_to_vcmax",          "$b_0$",                                  "unitless",                     "italic(b)[0]*' (-)'",                                              "italic(b)[0]",                   "Unit cost ratio of carboxylation to transpiration" , # Ratio of ($R_{\\mathrm{d25}}$) to the maximum carboxylation rate $V_{\\mathrm{cmax}}$ (both temperature-normalised dark respiration; eq. C8 in Stocker et al. 2020)
  "tau_acclim",           "$\\tau$",                                "days",                         "italic(tau)*' (days)'",                                            "italic(tau)",                    "Acclimation time scale of photosynthesis" ,
  "kc_jmax",              "$c^{*}$",                                "unitless",                     "italic(c)^'*'*' (-)'",                                             "italic(c)^'*'",                  "Unit cost of electron transport" , #  (maintenance of $J_{\\mathrm{max}}$)
  # ERROR PARAMETER:
  "err_gpp",              "$\\sigma_{\\text{GPP}}$",                "\\unit{gC\\,m^{-2}\\,s^{-1}}", "italic(sigma)['GPP']*' (gC m \u207B\u00B2 s\u207B\u00B9)'", "italic(sigma)['GPP']",           "Gaussian error standard deviation of GPP" ,
  "err_bigD13C",          "$\\sigma_{\\text{\\Delta}}$",            "\\unit{\\permil}",             "italic(sigma)[Delta]*' ('*'\u2030'*')'",                           "italic(sigma)[Delta]",           "Gaussian error standard deviation of $\\Delta$" ,
  "err_vj",               "$\\sigma_{\\text{VJ}}$",                 "unitless",                     "italic(sigma)['VJ']*' (-)'",                                       "italic(sigma)['VJ']",            "Gaussian error standard deviation of VJ",
  "errbias_bigD13C",      "$\\delta_{\\text{\\Delta}}$",            "\\unit{\\permil}",             "italic(delta)[Delta]*' (\u2030)'",                                 "italic(delta)[Delta]",           "Bias error term of $\\Delta$ (= mod - obs)",
  "errbias_vj",           "$\\delta_{\\text{VJ}}$",                 "unitless",                     "italic(delta)['VJ']*' (-)'",                                       "italic(delta)['VJ']",            "Bias error term of VJ (= mod - obs)",
  "errscale_gpp",         "$\\kappa_{\\text{GPP}}$",                "unitless",                     "italic(kappa)['GPP']*' (-)'",                                      "italic(kappa)['GPP']",           "Multiplicative bias error term of GPP"
) |> mutate(Parameter = forcats::as_factor(Parameter),   # make factor to keep ordering
            Symbol_tex = forcats::as_factor(Symbol_tex)) # make factor to keep ordering
label_vec_units <- setNames(rsofun_symbol_parname_description$Symbol_R, rsofun_symbol_parname_description$Parameter)
label_vec_short <- setNames(rsofun_symbol_parname_description$Symbol_R2, rsofun_symbol_parname_description$Parameter)
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
ggplot(rsofun_symbol_parname_description |> rename(variable = Parameter)) + geom_point(x=rep(c(1,0,0),5),y=rep(c(1,0,1),5)) + # facet_wrap(~Parameter) +
  facet_wrap( ~ variable , nrow = 2, scales = "free_x", labeller = custom_labeller_variable) +
  theme_bw() + theme(strip.background = element_rect(fill = "green"), strip.text = element_text(margin = margin(0,0,0,0), family = "Helvetica"))

## Re-label Scenarios ----
scenario_labels <-tribble(
  ~scenario, ~setup_label, ~label, ~label_targets,           ~label_targets_full,
  228,       "S1",        "a)",    "'S1: '*Delta",            "'S1: '*Delta^'13'*C",
  227,       "S2",        "b)",    "'S2: '*'VJ'",             "'S2: '*frac(V[cmax], J[max])",
  226,       "S3",        "c)",    "'S3: '*Delta*',VJ'",      "'S3: '*Delta^'13'*C*','*frac(V[cmax], J[max])",
  222,       "S4",        "d)",    "'S4: '*GPP",              "'S4: '*GPP",
  223,       "S5",        "e)",    "'S5: '*Delta*',VJ,'*GPP", "'S5: '*Delta^'13'*C*','*frac(V[cmax], J[max])*','*GPP",
  231,       "S6",        "h)",    "'S6: '*Delta*',VJ,'*GPP", "'S6: '*Delta^'13'*C*','*frac(V[cmax], J[max])*','*GPP",      #  (priors_truncated)

  229,       "S7",        "y)",    "Delta*',VJ,'*GPP",       "'S7: '*Delta^'13'*C*','*frac(V[cmax], J[max])*','*GPP",     #  (priors)
  230,       "S8",        "z)",    "Delta*',VJ,'*GPP",       "'S8: '*Delta^'13'*C*','*frac(V[cmax], J[max])*','*GPP"     #  (fixed)
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

flag_plot_general <- TRUE # possibility to switch this off

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
               mapping = aes(lon, lat)) + # ggtitle("GPP flux sites") +
    annotate("text", label = sprintf("Training\n(n=%d)", site_info |> filter(gpp) |> filter(set == "train") |> nrow()),
              # x = -Inf, y = -Inf, hjust = -0.0, vjust = -0.2, size = 2,
             x = -177, y = -55, hjust = 0, vjust = 0, size = 3) + # size = 2
    ggtext
  pl2_train <- rgeco:::plot_map_simpl(dir_ne = tempdir()) +
    geom_point(size=0.1,shape = 20, color = "red",
               data    = site_info |> filter(vj) |> filter(set == "train"),
               mapping = aes(lon, lat)) + # ggtitle("Vcmax/Jmax sites") +
    annotate("text", label = sprintf("Training\n(n=%d)", site_info |> filter(vj) |> filter(set == "train") |> nrow()),
              # x = -Inf, y = -Inf, hjust = -0.0, vjust = -0.2, size = 2,
             x = -177, y = -55, hjust = 0, vjust = 0, size = 3) + # size = 2
    ggtext
  pl3_train <- rgeco:::plot_map_simpl(dir_ne = tempdir()) +
    geom_point(size=0.1,shape = 20, color = "red",
               data    = site_info |> filter(bigD13C) |> filter(set == "train"),
               mapping = aes(lon, lat)) + # ggtitle("Δ13C sites") +
    annotate("text", label = sprintf("Training\n(n=%d)", site_info |> filter(bigD13C) |> filter(set == "train") |> nrow()),
              # x = -Inf, y = -Inf, hjust = -0.0, vjust = -0.2, size = 2,
             x = -177, y = -55, hjust = 0, vjust = 0, size = 3) + # size = 2
    ggtext

  pl1_test <- rgeco:::plot_map_simpl(dir_ne = tempdir()) +
    geom_point(size=0.1,shape = 20, color = "red",
               data    = site_info |> filter(gpp) |> filter(set == "test"),
               mapping = aes(lon, lat)) + # ggtitle(NULL) +
    annotate("text", label = sprintf("Test\n(n=%d)", site_info |> filter(gpp) |> filter(set == "test") |> nrow()),
              # x = -Inf, y = -Inf, hjust = -0.0, vjust = -0.2, size = 2,
             x = -177, y = -55, hjust = 0, vjust = 0, size = 3) + # size = 2
    ggtext
  pl2_test <- rgeco:::plot_map_simpl(dir_ne = tempdir()) +
    geom_point(size=0.1,shape = 20, color = "red",
               data    = site_info |> filter(vj) |> filter(set == "test"),
               mapping = aes(lon, lat)) + # ggtitle(NULL) +
    annotate("text", label = sprintf("Test\n(n=%d)", site_info |> filter(vj) |> filter(set == "test") |> nrow()),
              # x = -Inf, y = -Inf, hjust = -0.0, vjust = -0.2, size = 2,
             x = -177, y = -55, hjust = 0, vjust = 0, size = 3) + # size = 2
    ggtext
  pl3_test <- rgeco:::plot_map_simpl(dir_ne = tempdir()) +
    geom_point(size=0.1,shape = 20, color = "red",
               data    = site_info |> filter(bigD13C) |> filter(set == "test"),
               mapping = aes(lon, lat)) + # ggtitle(NULL) +
    annotate("text", label = sprintf("Test\n(n=%d)", site_info |> filter(bigD13C) |> filter(set == "test") |> nrow()),
              # x = -Inf, y = -Inf, hjust = -0.0, vjust = -0.2, size = 2,
             x = -177, y = -55, hjust = 0, vjust = 0, size = 3) + # size = 2
    ggtext
  library(cowplot)
  show_layout <- theme(plot.background = element_rect(fill = "red", color="green"))
  remove_labels <- theme(
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    title = element_text(size=6)
    )
  pl_sitemap <- cowplot::plot_grid(
    pl1_train + remove_labels, pl2_train + remove_labels, pl3_train + remove_labels,
    pl1_test  + remove_labels, pl2_test  + remove_labels, pl3_test  + remove_labels,
    ncol = 3, rel_heights = c(1,1), align = "hv",
    labels = c("(a)","(b)","(c)"), label_fontfamily = "Helvetica", hjust = 0, label_x = 0, label_size = 11, vjust=1, label_y = Inf)
  pl_sitemap
  ggsave(
    here::here("fig/fig_C_append_climate_MapTargetTrainingSites.png"),
    pl_sitemap, width=12, height=3.9, units="cm", dpi=300, scale = 1.8)
  figure_2 <- cowplot::plot_grid(
    pl1_train + remove_labels, pl2_train + remove_labels, pl3_train + remove_labels,
    pl1_test  + remove_labels, pl2_test  + remove_labels, pl3_test  + remove_labels,
    ncol = 3, rel_heights = c(1,1), align = "hv",
    labels = c("(a)","(b)","(c)"), label_fontfamily = "Helvetica", hjust = 0, label_x = 0, label_size = 11, vjust=1, label_y = Inf)
  ggsave(
    here::here("fig/figure_2.png"),
    figure_2, width=12, height=3.9, units="cm", dpi=300, scale = 1.8)

  # stats
  #The data set consisted of 50 sites with $\text{GPP}$ flux time series (xxx site-dates in total),
  # 49 sites with a total of XXX individual VJ observations (multiple individual plants and/or species per site sampled),
  # and 325 sites with a total of XXX $\Delta$ observations

  # stats_sites <-
    site_info |> filter(gpp)     |> filter(set %in% c("train","test")) |> select(sitename, data,gpp_flag=gpp,set)         |> unnest(data) |> summarise(n())
    site_info |> filter(vj)      |> filter(set %in% c("train","test")) |> select(sitename, data,vj_flag=vj,set)           |> unnest(data) |> unnest(vj) |> summarise(n())
    site_info |> filter(bigD13C) |> filter(set %in% c("train","test")) |> select(sitename, data,bigD13C_flag=bigD13C,set) |> unnest(data) |> unnest(bigD13C) |> summarise(n())
  # readr::write_csv(here::here("fig/fig_C_append_climate_MapTargetTrainingSites.csv"),
  #                  stats_sites)
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
flag_plot_mcmc <- TRUE # possibility to switch this off

## Load MCMC sampling----
if (flag_plot_mcmc){
  out_calib_s220DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen220_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s221DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen221_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s222DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen222_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s223DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen223_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  # out_calib_s224DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen224_DREAMzs-80000-0iter_8x3chains_on_CPU8x1_continued.rds")) # TODO: change to 100k
  # out_calib_s225DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen225_DREAMzs-80000-0iter_8x3chains_on_CPU8x1_continued.rds")) # TODO: change to 100k
  out_calib_s226DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen226_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s227DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen227_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s228DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen228_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
  out_calib_s229DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen229_DREAMzs-80000-0iter_8x3chains_on_CPU8x1_continued.rds")) # TODO: change to 100k
  out_calib_s230DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen230_DREAMzs-80000-0iter_8x3chains_on_CPU8x1_continued.rds")) # TODO: change to 100k
  out_calib_s231DREAMzs <- readr::read_rds(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/calibrations/out_calib__scen231_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds"))
}



## Figure E: MCMC convergence diagnostics ----
## trace plots (of chains), correlation plots, Gelman-Rubin (r.1.1)
if (flag_plot_mcmc){
  ggsave_figE <- function(gg, fname, scale = 1.8){
    ggsave(plot = gg, filename = here::here(file.path("fig/",fname)), width = 12.0, height = 6, units = "cm", scale = scale)
  }

  plot_mcmc_trace(out_calib_s220DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 30000) |> ggsave_figE("fig_E_MCMCconvergence_trace_s220DREAMzs.png", scale = 2.3)
  plot_mcmc_trace(out_calib_s221DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 30000) |> ggsave_figE("fig_E_MCMCconvergence_trace_s221DREAMzs.png", scale = 2.3)
  plot_mcmc_trace(out_calib_s222DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 30000) |> ggsave_figE("fig_E_MCMCconvergence_trace_s222DREAMzs.png", scale = 2.3)
  plot_mcmc_trace(out_calib_s223DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 30000) |> ggsave_figE("fig_E_MCMCconvergence_trace_s223DREAMzs.png", scale = 2.3)
  # plot_mcmc_trace(out_calib_s224DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 25000) |> ggsave_figE("fig_E_MCMCconvergence_trace_s224DREAMzs.png", scale = 2.3)
  # plot_mcmc_trace(out_calib_s225DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 25000) |> ggsave_figE("fig_E_MCMCconvergence_trace_s225DREAMzs.png", scale = 2.3)
  plot_mcmc_trace(out_calib_s226DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 30000) |> ggsave_figE("fig_E_MCMCconvergence_trace_s226DREAMzs.png", scale = 2.3)
  plot_mcmc_trace(out_calib_s227DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 30000) |> ggsave_figE("fig_E_MCMCconvergence_trace_s227DREAMzs.png", scale = 2.3)
  plot_mcmc_trace(out_calib_s228DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 30000) |> ggsave_figE("fig_E_MCMCconvergence_trace_s228DREAMzs.png", scale = 2.3)
  # plot_mcmc_trace(out_calib_s229DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 25000) |> ggsave_figE("fig_E_MCMCconvergence_trace_s229DREAMzs.png", scale = 2.3)
  # plot_mcmc_trace(out_calib_s230DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 25000) |> ggsave_figE("fig_E_MCMCconvergence_trace_s230DREAMzs.png", scale = 2.3)
  plot_mcmc_trace(out_calib_s231DREAMzs, nr_internal_chains = 3, burnin_to_skip = 0, burnin_to_skip_gelman = 25000) |> ggsave_figE("fig_E_MCMCconvergence_trace_s231DREAMzs.png", scale = 2.3)
        BayesianTools::gelmanDiagnostics(out_calib_s231DREAMzs$mod, start = 1)
        BayesianTools::gelmanDiagnostics(out_calib_s231DREAMzs$mod, start = 5000)
        BayesianTools::gelmanDiagnostics(out_calib_s231DREAMzs$mod, start = 8000)
        BayesianTools::gelmanDiagnostics(out_calib_s231DREAMzs$mod, start = 10000)
        BayesianTools::gelmanDiagnostics(out_calib_s231DREAMzs$mod, start = 20000)
        BayesianTools::gelmanDiagnostics(out_calib_s231DREAMzs$mod, start = 25000)
        BayesianTools::gelmanDiagnostics(out_calib_s231DREAMzs$mod, start = 29000)
        BayesianTools::gelmanDiagnostics(out_calib_s231DREAMzs$mod, start = 30000)
}

## Figure E2: Posterior parameter correlation analysis ----
## correlation plots
source(here::here("R/correlationPlot_expressionLabelled.R"))
my_own_correlation <- function(out_calib, thin, start, filename){
  mcmc <- out_calib$mod

  curr_param_names <- mcmc[[1]]$setup$names
  new_param_names <- unname(label_vec_units[curr_param_names])

  # Update the names in the mcmc sampler list:
  # getSample(mcmc, thin = 5, start = 30000, whichParameters = NULL) |> colnames()
  mcmc[[1]]$setup$names <- new_param_names # sufficient to do it for the firs
  # mcmc[[2]]$setup$names <- new_param_names #
  # mcmc[[3]]$setup$names <- new_param_names #
  # getSample(mcmc, thin = 5, start = 30000, whichParameters = NULL) |> colnames() # This show that it works

  png(filename, width = 24, height = 24, units = "cm", res = 300)
  # correlationPlot(out_calib$mod, thin = thin, start = start)         # This is the default plot
  correlationPlot_expressionLabelled(mcmc, thin = thin, start = start) # This is the adapted plot using expressions for labelling
  dev.off()
}

if (flag_plot_mcmc){

  my_own_correlation(out_calib_s220DREAMzs,thin=5,start=30000, filename = here::here("fig/fig_E2_MCMCconvergence_corr_s220DREAMzs_burnin30000.png"))
  my_own_correlation(out_calib_s221DREAMzs,thin=5,start=30000, filename = here::here("fig/fig_E2_MCMCconvergence_corr_s221DREAMzs_burnin30000.png"))
  my_own_correlation(out_calib_s222DREAMzs,thin=5,start=30000, filename = here::here("fig/fig_E2_MCMCconvergence_corr_s222DREAMzs_burnin30000.png"))
  my_own_correlation(out_calib_s223DREAMzs,thin=5,start=30000, filename = here::here("fig/fig_E2_MCMCconvergence_corr_s223DREAMzs_burnin30000.png"))
  # my_own_correlation(out_calib_s224DREAMzs,thin=5,start=25000, filename = here::here("fig/fig_E2_MCMCconvergence_corr_s224DREAMzs_burnin25000.png"))
  # my_own_correlation(out_calib_s225DREAMzs,thin=5,start=25000, filename = here::here("fig/fig_E2_MCMCconvergence_corr_s225DREAMzs_burnin25000.png"))
  my_own_correlation(out_calib_s226DREAMzs,thin=5,start=30000, filename = here::here("fig/fig_E2_MCMCconvergence_corr_s226DREAMzs_burnin30000.png"))
  my_own_correlation(out_calib_s227DREAMzs,thin=5,start=30000, filename = here::here("fig/fig_E2_MCMCconvergence_corr_s227DREAMzs_burnin30000.png"))
  my_own_correlation(out_calib_s228DREAMzs,thin=5,start=30000, filename = here::here("fig/fig_E2_MCMCconvergence_corr_s228DREAMzs_burnin30000.png"))
  # my_own_correlation(out_calib_s229DREAMzs,thin=5,start=25000, filename = here::here("fig/fig_E2_MCMCconvergence_corr_s229DREAMzs_burnin25000.png"))
  # my_own_correlation(out_calib_s230DREAMzs,thin=5,start=25000, filename = here::here("fig/fig_E2_MCMCconvergence_corr_s230DREAMzs_burnin25000.png"))
  my_own_correlation(out_calib_s231DREAMzs,thin=5,start=30000, filename = here::here("fig/fig_E2_MCMCconvergence_corr_s231DREAMzs_burnin30000.png"))



  ## Figure A: prior, posterior density plot ----
  ## for each scenario x params

  ### indivdiual plots: ----
  plot_and_output_prior_posterior_density <- function(out_calib, burnin_to_skip, fname){
    gg <- plot_prior_posterior_density(out_calib$mod, burnin_to_skip = burnin_to_skip) +
      ggtitle(out_calib$fpath)

    # save plot and return plot
    ggsave_and_return(gg, fname = fname)
  }
  pl_post_s220DR<-plot_and_output_prior_posterior_density(out_calib_s220DREAMzs, burnin_to_skip = 30000, fname = "fig_A_MCMCconvergence_posterior_s220DREAMzs.png")
  pl_post_s221DR<-plot_and_output_prior_posterior_density(out_calib_s221DREAMzs, burnin_to_skip = 30000, fname = "fig_A_MCMCconvergence_posterior_s221DREAMzs.png")
  pl_post_s222DR<-plot_and_output_prior_posterior_density(out_calib_s222DREAMzs, burnin_to_skip = 30000, fname = "fig_A_MCMCconvergence_posterior_s222DREAMzs.png")
  pl_post_s223DR<-plot_and_output_prior_posterior_density(out_calib_s223DREAMzs, burnin_to_skip = 30000, fname = "fig_A_MCMCconvergence_posterior_s223DREAMzs.png")
  # pl_post_s224DR<-plot_and_output_prior_posterior_density(out_calib_s224DREAMzs, burnin_to_skip = 25000, fname = "fig_A_MCMCconvergence_posterior_s224DREAMzs.png")
  # pl_post_s225DR<-plot_and_output_prior_posterior_density(out_calib_s225DREAMzs, burnin_to_skip = 25000, fname = "fig_A_MCMCconvergence_posterior_s225DREAMzs.png")
  pl_post_s226DR<-plot_and_output_prior_posterior_density(out_calib_s226DREAMzs, burnin_to_skip = 30000, fname = "fig_A_MCMCconvergence_posterior_s226DREAMzs.png")
  pl_post_s227DR<-plot_and_output_prior_posterior_density(out_calib_s227DREAMzs, burnin_to_skip = 30000, fname = "fig_A_MCMCconvergence_posterior_s227DREAMzs.png")
  pl_post_s228DR<-plot_and_output_prior_posterior_density(out_calib_s228DREAMzs, burnin_to_skip = 30000, fname = "fig_A_MCMCconvergence_posterior_s228DREAMzs.png")
  # pl_post_s229DR<-plot_and_output_prior_posterior_density(out_calib_s229DREAMzs, burnin_to_skip = 25000, fname = "fig_A_MCMCconvergence_posterior_s229DREAMzs.png")
  # pl_post_s230DR<-plot_and_output_prior_posterior_density(out_calib_s230DREAMzs, burnin_to_skip = 25000, fname = "fig_A_MCMCconvergence_posterior_s230DREAMzs.png")
  pl_post_s231DR<-plot_and_output_prior_posterior_density(out_calib_s231DREAMzs, burnin_to_skip = 30000, fname = "fig_A_MCMCconvergence_posterior_s231DREAMzs.png")

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
                                "Prior 231"= out_calib_s231DREAMzs$mod,
                                "220" = out_calib_s220DREAMzs$mod,
                                "221" = out_calib_s221DREAMzs$mod,
                                "222" = out_calib_s222DREAMzs$mod,
                                "223" = out_calib_s223DREAMzs$mod,
                                # "224" = out_calib_s224DREAMzs$mod,
                                "226" = out_calib_s226DREAMzs$mod,
                                "227" = out_calib_s227DREAMzs$mod,
                                "228" = out_calib_s228DREAMzs$mod,
                                "229" = out_calib_s229DREAMzs$mod,
                                "230"= out_calib_s230DREAMzs$mod,
                                "231"= out_calib_s231DREAMzs$mod
                                )

  pl_post_comparison6f <- plot_prior_posterior_density_compare2(
    named_list_scen =  scenarios_to_compare6[c("Prior 222","Prior 223","Prior 226","Prior 227","Prior 228","Prior 231",
                                                     "222",      "223",      "226",      "227",      "228",      "231")],
    burnin_to_skip  = 30000,
    add_MAP = TRUE, param_order = param_order6, params_not_to_plot = c("rd_to_vcmax", "soilm_betao", "errbias_bigD13C", "errbias_vj", "errscale_gpp"))

  ggsave(plot = pl_post_comparison6f +
           theme(strip.text = element_text(size=12, family = "Helvetica", margin = margin(0,0,0,0)),
                 strip.placement = "outside", strip.background = element_blank(),
                 #legend.position = "top", legend.justification = "right"
                 legend.position = "inside", legend.justification = c(0,1), legend.position.inside = c(0, 1.04), legend.direction = "horizontal", legend.background = element_blank()
                 ),
         filename = here::here(file.path("fig/", "fig_A_MCMCconvergence_posterior_labelled_s222DR_223DR_226DR_227DR_228DR_231DR.png")),
         width = 12, height = 6, units="cm", dpi=300, scale = 1.8)
  figure_3 <- pl_post_comparison6f +
           theme(strip.text = element_text(size=12, family = "Helvetica", margin = margin(0,0,0,0)),
                 strip.placement = "outside", strip.background = element_blank(),
                 #legend.position = "top", legend.justification = "right"
                 legend.position = "inside", legend.justification = c(0,1), legend.position.inside = c(0, 1.04), legend.direction = "horizontal", legend.background = element_blank()
                 )
  ggsave(plot = figure_3,
         filename = here::here(file.path("fig/", "figure_3.png")),
         width = 12, height = 6, units="cm", dpi=300, scale = 1.8)


  pl_post_comparison6g <- plot_prior_posterior_density_compare2(
    named_list_scen =  scenarios_to_compare6[c("Prior 222","Prior 223","Prior 226","Prior 227","Prior 228","Prior 230","Prior 231",
                                                     "222",      "223",      "226",      "227",      "228",      "230",      "231")],
    burnin_to_skip  = 25000,
    add_MAP = TRUE, param_order = param_order6, params_not_to_plot = c("rd_to_vcmax", "soilm_betao", "errbias_bigD13C", "errbias_vj", "errscale_gpp"))
  ggsave_and_return(pl_post_comparison6g, "fig_A_MCMCconvergence_posterior_labelled_s222DR_223DR_226DR_227DR_228DR_230DR_231DR.png",   width = 7.2, height = 3.6)

  pl_post_comparison6h <- plot_prior_posterior_density_compare2(
    named_list_scen =  scenarios_to_compare6[c("Prior 222","Prior 223","Prior 226","Prior 227","Prior 228","Prior 229","Prior 230","Prior 231",
                                                     "222",      "223",      "226",      "227",      "228",      "229",      "230",      "231")],
    burnin_to_skip  = 25000,
    add_MAP = TRUE, param_order = param_order6, params_not_to_plot = c("rd_to_vcmax", "soilm_betao", "errbias_bigD13C", "errbias_vj", "errscale_gpp"))
  ggsave_and_return(pl_post_comparison6h, "fig_A_MCMCconvergence_posterior_labelled_s222DR_223DR_226DR_227DR_228DR_220DR_230DR_231DR.png",   width = 7.2, height = 3.6)

  pl_post_comparison6c <- plot_prior_posterior_density_compare2(
    named_list_scen =  scenarios_to_compare6[c("Prior 221","Prior 222","Prior 223","Prior 227","Prior 228", "221","222","223","227","228")],
    burnin_to_skip  = 25000,
    add_MAP = TRUE, param_order = param_order6, params_not_to_plot = c("rd_to_vcmax", "soilm_betao", "errbias_bigD13C", "errbias_vj", "errscale_gpp"))

  pl_post_comparison6d <- plot_prior_posterior_density_compare2(
    named_list_scen =  scenarios_to_compare6[c("Prior 221","Prior 222","Prior 223","Prior 226","Prior 227","Prior 228", "221","222","223","226","227","228")],
    burnin_to_skip  = 25000,
    add_MAP = TRUE, param_order = param_order6, params_not_to_plot = c("rd_to_vcmax", "soilm_betao", "errbias_bigD13C", "errbias_vj", "errscale_gpp"))

  pl_post_comparison6e <- plot_prior_posterior_density_compare2(
    named_list_scen =  scenarios_to_compare6,
    burnin_to_skip  = 25000,
    add_MAP = TRUE, param_order = param_order6, params_not_to_plot = c("rd_to_vcmax", "soilm_betao", "errbias_bigD13C", "errbias_vj", "errscale_gpp"))

  ggsave_and_return(pl_post_comparison6c, "fig_A_MCMCconvergence_posterior_labelled_s221DR_222DR_223DR_227DR_228DR.png",         width = 7.2, height = 3.6)
  ggsave_and_return(pl_post_comparison6d, "fig_A_MCMCconvergence_posterior_labelled_s221DR_222DR_223DR_226DR_227DR_228DR.png",   width = 7.2, height = 3.6)
  ggsave_and_return(pl_post_comparison6e, "fig_A_MCMCconvergence_posterior_labelled_s220DR_222DR_..._228DR.png",                 width = 7.2, height = 3.6)
}




## Figure F: TBD: comparison of calibration vs GenSA?? ----
## or just using prior estimates from Stocker 2020? (r.1.14)


## Figure XXX: Plot/output runtimes of MCMC samplings (calibrations) ----
if (flag_plot_mcmc){

  timing_files2 <- list.files(file.path("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/","timings"), pattern = "timings_scen.*_2025-09-.*.rds", full.names = T)
  timings2 <- lapply(timing_files2, readr::read_rds) |> bind_rows()

  timings <- timings2 |>
    mutate(scenario = factor(scenario),
           cores    = factor(cores))

  pl_timings <- ggplot(
    timings,
    aes(#x=(iterations-burnin)*n_chains*n_chains_inner,
      x=(iterations),
      y=as.numeric(walltime,"secs")/60,
      color = scenario,
      linetype = cores)) +
    geom_point() + geom_line() +
    geom_text(aes(label = sprintf("(%d,%d)",iterations,burnin)), vjust = 0, show.legend = F) +
    scale_x_log10(minor_breaks=scales::minor_breaks_n(10)) +
    scale_y_log10(minor_breaks=scales::minor_breaks_n(10)) +
    labs(y="walltime (minutes)") + theme_minimal()
  # pl_timings
  # pl_timings$data$scenario |> unique()

  pl_timings %+% (pl_timings$data |> filter(scenario %in% c(231,230,229)))
  pl_timings_to_output <- pl_timings %+% (pl_timings$data |>
                                            filter(iterations>=10000) |>
                                            filter(scenario %in% scenario_labels$scenario))
  ggsave(
      here::here("fig/fig_XXX_timings.png"),
      pl_timings_to_output, width=8.3, height=8.3, units="cm", dpi=300, scale = 1.5)
}





## Table b: prior ranges and MAP of estimated params ----
if (flag_plot_mcmc){

  caption <- paste(
    "Parameter listing including prior and Maximum A Posteriori (MAP) estimates.",
    "The bounds of uniform or truncated normal prior distributions are given in square brackets.",
    "Parameters that were held fixed for the calibration are marked with a single number in brackets and an asterisk (*)")

  # scenarios_to_compare <- scenario_labels |> filter(scenario %in% c(228,227,226,222,223,230))
  scenarios_to_compare <- scenario_labels |> filter(scenario %in% c(228,227,226,222,223,231))

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
    data.frame(MAP = MAP(out_calib_s228DREAMzs$mod, start = 30000)$parametersMAP) |> tibble::rownames_to_column("Parameter") |> mutate(scenario = 228),
    data.frame(MAP = MAP(out_calib_s227DREAMzs$mod, start = 30000)$parametersMAP) |> tibble::rownames_to_column("Parameter") |> mutate(scenario = 227),
    data.frame(MAP = MAP(out_calib_s226DREAMzs$mod, start = 30000)$parametersMAP) |> tibble::rownames_to_column("Parameter") |> mutate(scenario = 226),
    data.frame(MAP = MAP(out_calib_s222DREAMzs$mod, start = 30000)$parametersMAP) |> tibble::rownames_to_column("Parameter") |> mutate(scenario = 222),
    data.frame(MAP = MAP(out_calib_s223DREAMzs$mod, start = 30000)$parametersMAP) |> tibble::rownames_to_column("Parameter") |> mutate(scenario = 223),
    # data.frame(MAP = MAP(out_calib_s229DREAMzs$mod, start = 25000)$parametersMAP) |> tibble::rownames_to_column("Parameter") |> mutate(scenario = 229),
    # data.frame(MAP = MAP(out_calib_s230DREAMzs$mod, start = 25000)$parametersMAP) |> tibble::rownames_to_column("Parameter") |> mutate(scenario = 230),
    data.frame(MAP = MAP(out_calib_s231DREAMzs$mod, start = 30000)$parametersMAP) |> tibble::rownames_to_column("Parameter") |> mutate(scenario = 231)
  ) |> filter(scenario %in% par_fixed_df$scenario)
  stopifnot(all(sort(unique(par_MAP_df$scenario)) == sort(unique(par_fixed_df$scenario))))



  # format the priors:
  par_prior_formatted <- par_priors_df |>
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
    rowwise() |> mutate(prior_value = case_when(!is.na(sd) & is.na(lower)  ~ sprintf("$\\mathcal{N}(%.1f,\\,%.1f^{2})$",mean,sd),
                                                !is.na(sd) & !is.na(lower) ~ sprintf("$\\mathcal{N}(%.1f,\\,%.1f^{2})$\\tnote{a} %s",mean,sd,prior_value),
                                                TRUE                      ~ prior_value))

  # format the fixed params:
  par_fix_formatted <- par_fixed_df |>
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
  par_MAP_formatted <- par_MAP_df |>
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
    "Parameters that were held fixed for the calibration are marked with a single number in brackets and an asterisk (*).")

  table_b_v2 <-
    left_join(
      bind_rows(par_prior_formatted,
                par_fix_formatted |> rename(prior_value = fixed_value)),
      par_MAP_formatted,
      by = join_by(Parameter, scenario)) |>
    # format text
    mutate(cell_text = paste0(MAP, "\\newline", prior_value)) |>
    select(Parameter, scenario, cell_text) |>
    # get correct labels of scenarios and parameters:
      ## replace scenario number with label
      left_join(scenario_labels |> select(scenario, label),
                by = join_by(scenario)) |>
    pivot_wider(names_from = c(scenario,label), values_from = cell_text, names_glue = "Scen. {label}({scenario})\\newline MAP\\newline [Prior]") |>
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




  # get other statistics from posterior for manual reporting in text
  get_statistics_for_report <- function (bayesianOutput, ...) { # inspired from BayesianTools::MAP()
    samples = getSample(bayesianOutput, parametersOnly = F, ...)
    if ("mcmcSamplerList" %in% class(bayesianOutput))
      nPars <- bayesianOutput[[1]]$setup$numPars
    else nPars = bayesianOutput$setup$numPars
      best = which.max(samples[, nPars + 1])

    samples[, 1:nPars] |> as.data.frame() |> pivot_longer(everything(), names_to = "Parameter") |>
      group_by(Parameter) |>
      summarise(mean = mean(value),
                median = median(value),
                p25  = quantile(value, 0.25),
                p75  = quantile(value, 0.75),
                IQR  = p75 - p25)
  }
  par_otherstats_df <- bind_rows(
    get_statistics_for_report(out_calib_s228DREAMzs$mod, start = 30000)|> mutate(scenario = 228),
    get_statistics_for_report(out_calib_s227DREAMzs$mod, start = 30000)|> mutate(scenario = 227),
    get_statistics_for_report(out_calib_s226DREAMzs$mod, start = 30000)|> mutate(scenario = 226),
    get_statistics_for_report(out_calib_s222DREAMzs$mod, start = 30000)|> mutate(scenario = 222),
    get_statistics_for_report(out_calib_s223DREAMzs$mod, start = 30000)|> mutate(scenario = 223),
    # get_statistics_for_report(out_calib_s229DREAMzs$mod, start = 25000)|> mutate(scenario = 229),
    # get_statistics_for_report(out_calib_s230DREAMzs$mod, start = 25000)|> mutate(scenario = 230),
    get_statistics_for_report(out_calib_s231DREAMzs$mod, start = 30000)|> mutate(scenario = 231)
  ) |> filter(scenario %in% par_fixed_df$scenario)
  stopifnot(all(sort(unique(par_otherstats_df$scenario)) == sort(unique(par_fixed_df$scenario))))

  par_allstats_df <- left_join(par_otherstats_df, par_MAP_df) |> select(scenario, Parameter, MAP, mean, median, IQR, p25, p75) |>
    # replace scenario number with label
    left_join(scenario_labels |> select(scenario, label), by = join_by(scenario)) |>
    # append Symbol and Description:
    mutate(Parameter = factor(Parameter, levels = levels(rsofun_symbol_parname_description$Parameter))) |>
    left_join(select(rsofun_symbol_parname_description, Parameter, Symbol_R),
              by = join_by(Parameter)) |>
    select(scenario, label, Parameter, Symbol_R, everything())
  par_allstats_df
  readr::write_csv(par_allstats_df, here::here("fig", "table-c-posterior_params.stats.allscen.csv"))
}


# PREDICTION PLOTS ----
# TO RUN PREDICTIONS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 228 100000 30000 50 10 "_continued.rds"
# TO RUN PREDICTIONS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 227 100000 30000 50 10 "_continued.rds"
# TO RUN PREDICTIONS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 226 100000 30000 50 10 "_continued.rds"
# TO RUN PREDICTIONS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 222 100000 30000 50 10 "_continued.rds"
# TO RUN PREDICTIONS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 223 100000 30000 50 10 "_continued.rds"
# TO RUN PREDICTIONS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 229 80000 25000 50 10 "_continued.rds"
# TO RUN PREDICTIONS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 230 60000 18000 50 10 "_continued.rds"
# TO RUN PREDICTIONS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 231 80000 25000 50 10 "_continued.rds"
# TO RUN PREDICTIONS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 231 100000 30000 50 10 "_continued.rds"

# TO RUN PREDICTIONS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 228 100000 30000 20 3 "_continued.rds"
# TO RUN PREDICTIONS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 227 100000 30000 20 3 "_continued.rds"
# TO RUN PREDICTIONS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 226 100000 30000 20 3 "_continued.rds"
# TO RUN PREDICTIONS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 222 100000 30000 20 3 "_continued.rds"
# TO RUN PREDICTIONS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 223 100000 30000 20 3 "_continued.rds"
# TO RUN PREDICTIONS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 229 80000 25000 20 3 "_continued.rds"
# TO RUN PREDICTIONS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 230 60000 18000 20 3 "_continued.rds"
# TO RUN PREDICTIONS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 231 80000 25000 20 3 "_continued.rds"
# TO RUN PREDICTIONS: ~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 231 100000 30000 20 3 "_continued.rds"



## Figure B2: error distribution predObs scatter plot ----
## for each scenario x target x test

flag_plot_predictions <- TRUE # possibility to switch this off

# define what data to load (and use this as suffix for output)
for (n_post_err in list(
  list(n_post = "N20+MAP", n_err = "_N3errors"),
  list(n_post = "N50+MAP", n_err = "_N10errors"))) {

  n_post <- n_post_err$n_post
  n_err  <- n_post_err$n_err
  outfname_suffix <- paste0(n_post, n_err, "_s222-s231")
  if (flag_plot_predictions){
    # Load sampled posterior params used for predictions
    df_222_params  <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen222_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds"))
    df_223_params  <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen223_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds"))
    df_226_params  <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen226_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds"))
    df_227_params  <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen227_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds"))
    df_228_params  <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen228_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds"))
    # df_229_params  <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_8000burnin__out_calib__scen229_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds"))
    # df_230_params  <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_6000burnin__out_calib__scen230_DREAMzs-20000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds"))
    df_231_params  <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen231_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_params.rds"))

    # Load predictions for plotting
    df_222_vj      <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen222_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds"))
    df_222_bigD13C <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen222_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds"))
    df_222_gpp     <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen222_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds"))
    df_223_vj      <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen223_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds"))
    df_223_bigD13C <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen223_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds"))
    df_223_gpp     <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen223_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds"))
    df_226_vj      <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen226_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds"))
    df_226_bigD13C <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen226_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds"))
    df_226_gpp     <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen226_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds"))
    df_227_vj      <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen227_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds"))
    df_227_bigD13C <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen227_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds"))
    df_227_gpp     <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen227_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds"))
    df_228_vj      <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen228_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds"))
    df_228_bigD13C <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen228_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds"))
    df_228_gpp     <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen228_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds"))
    # df_229_vj      <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_8000burnin__out_calib__scen229_DREAMzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds"))
    # df_229_bigD13C <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_8000burnin__out_calib__scen229_DREAMzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds"))
    # df_229_gpp     <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_8000burnin__out_calib__scen229_DREAMzs-30000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds"))
    # df_230_vj      <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_6000burnin__out_calib__scen230_DREAMzs-20000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds"))
    # df_230_bigD13C <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_6000burnin__out_calib__scen230_DREAMzs-20000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds"))
    # df_230_gpp     <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_6000burnin__out_calib__scen230_DREAMzs-20000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds"))
    df_231_vj      <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen231_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_vj_sampled",n_err,".rds"))
    df_231_bigD13C <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen231_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_bigD13C_sampled",n_err,".rds"))
    df_231_gpp     <- readr::read_rds(paste0("/storage/scratch/giub_geco/fbernhard/rsofun_doc_outputs/data/predictions/out_predict_",n_post,"_30000burnin__out_calib__scen231_DREAMzs-100000-0iter_8x3chains_on_CPU8x1_continued.rds_gpp_sampled",n_err,".rds"))
  }

  source(here::here("R/analyse_modobs3.R"))

  my_own_scatter <- function(df, mod, ...){
    stopifnot(mod %in% c("mod_biasremoved_no_err", "mod_biasremoved_with_err"))
    # stopifnot(nrow(df) > 0)
    if (nrow(df) == 0) {return(list(gg = ggplot() + theme_void()))}

    # (MAP, Posterior, Posterior+Error)
    # MAP:             is posterior_sample_id==0
    #                  e.g. filter(df_gpp, is_train0_test1 == 1,     is_MAP, error_sample_id==1)
    # Posterior:       just take one error sampling, but all (~25 posterior samples)
    #                  e.g. filter(df_gpp, is_train0_test1 == 1,             error_sample_id==1),
    # Posterior+Error: take all errors
    #                  e.g. filter(df_gpp, is_train0_test1 == 1,             error_sample_id>=1)
    analyse_modobs2(df, mod = mod, obs = "obs", type = "hex", ...)
  }

  gpp_labs <- list(labs(x = "Predicted GPP (g C m\u207B\u00B2 s\u207B\u00B9)",
                        y = "Observed GPP (g C m\u207B\u00B2 s\u207B\u00B9)"))
  vj_labs <- list(labs(x = "Predicted VJ (-)",
                       y = "Observed VJ (-)"))
  bigD13C_labs <- list(labs(x = "Predicted Δ (\u2030)",
                            y = "Observed Δ (\u2030)"))

  gpp_labs_xNULL     <- list(labs(x = " ", y = "Observed GPP (g C m\u207B\u00B2 s\u207B\u00B9)"))
  vj_labs_xNULL      <- list(labs(x = " ", y = "Observed VJ (-)"))
  bigD13C_labs_xNULL <- list(labs(x = " ", y = "Observed Δ (\u2030)"))


  if (flag_plot_predictions){

    # compute scatters (and skills) for MAP of test set:
    list_of_scenarios_to_loop_over <- list(
      s228 = list("bigD13C" = df_228_bigD13C, "vj" = df_228_vj, "gpp" = df_228_gpp),
      s227 = list("bigD13C" = df_227_bigD13C, "vj" = df_227_vj, "gpp" = df_227_gpp),
      s226 = list("bigD13C" = df_226_bigD13C, "vj" = df_226_vj, "gpp" = df_226_gpp),
      s222 = list("bigD13C" = df_222_bigD13C, "vj" = df_222_vj, "gpp" = df_222_gpp),
      s223 = list("bigD13C" = df_223_bigD13C, "vj" = df_223_vj, "gpp" = df_223_gpp),
      # s229 = list("bigD13C" = df_229_bigD13C, "vj" = df_229_vj, "gpp" = df_229_gpp)
      # s230 = list("bigD13C" = df_230_bigD13C, "vj" = df_230_vj, "gpp" = df_230_gpp)
      s231 = list("bigD13C" = df_231_bigD13C, "vj" = df_231_vj, "gpp" = df_231_gpp)
    )

    for (parameter_set in c("MAP", "Posterior")){

      my_own_scatter <- function(df, mod, ...){
        stopifnot(mod %in% c("mod_biasremoved_no_err", "mod_biasremoved_with_err"))
        # stopifnot(nrow(df) > 0)
        if (nrow(df) == 0) {return(list(gg = ggplot() + theme_void()))}

        # (MAP, Posterior, Posterior+Error)
        # MAP:             is posterior_sample_id==0
        #                  e.g. filter(df_gpp, is_train0_test1 == 1,     is_MAP, error_sample_id==1)
        # Posterior:       just take one error sampling, but all (~25 posterior samples)
        #                  e.g. filter(df_gpp, is_train0_test1 == 1,             error_sample_id==1),
        # Posterior+Error: take all errors
        #                  e.g. filter(df_gpp, is_train0_test1 == 1,             error_sample_id>=1)
        analyse_modobs3(df, mod = mod, obs = "obs", type = "hex", ...)
      }
      list_of_scatters <- lapply(list_of_scenarios_to_loop_over, function(list_of_targets){
        lapply(list_of_targets, function(df_target_prediction){
          curr_target <- first(df_target_prediction$target)
          my_own_scatter(
            df_target_prediction %>% {
              if (parameter_set == "MAP") filter(., is_MAP) else .
              } %>% filter(error_sample_id==1, is_train0_test1 == 1),
            mod = "mod_biasremoved_no_err",
            shortsubtitle = TRUE,
            lower_xlim = ifelse(curr_target == "gpp", list(0), list(NULL))[[1]])
        })
      })

      ### indivdiual plots for each scenario: ----
      # pl_scatter_228 <- cowplot::plot_grid(
      #   nrow = 1,
      #   list_of_scatters$s228$bigD13C$gg + bigD13C_labs + labs(title = "Scenario 228"),
      #   list_of_scatters$s228$vj$gg      + vj_labs      + labs(title = "Scenario 228"),
      #   list_of_scatters$s228$gpp$gg     + gpp_labs     + labs(title = "Scenario 228"))
      # ggsave(plot = pl_scatter_228, filename = here::here("fig", paste0("fig_B2c_pred-vs-obs_s228_pred",outfname_suffix,"_", parameter_set, ".png")),
      #        width = 12, height = 4, units = "cm", dpi = "print", scale = 2.0)
      # # etc....

      ### single comparison plot: ----
      mark_as_target <- theme(panel.background = element_rect(fill = t_col("darkgreen", 80)))
      pl_scatter_comparison <- cowplot::plot_grid(
        ncol = 3, byrow = TRUE, labels = c("(a)","","", "(b)","","", "(c)","","", "(d)","","", "(e)","","", "(f)","",""),
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

        list_of_scatters$s223$bigD13C$gg + mark_as_target + bigD13C_labs_xNULL ,#+ labs(title = "Scenario 223"),
        list_of_scatters$s223$vj$gg      + mark_as_target + vj_labs_xNULL      ,#+ labs(title = "Scenario 223"),
        list_of_scatters$s223$gpp$gg                      + gpp_labs_xNULL     ,#+ labs(title = "Scenario 223"),

        list_of_scatters$s231$bigD13C$gg + mark_as_target + bigD13C_labs ,#+ labs(title = "Scenario 231"),   # USE LETTER (h) for scenario 231
        list_of_scatters$s231$vj$gg      + mark_as_target + vj_labs      ,#+ labs(title = "Scenario 231"),   # USE LETTER (h) for scenario 231
        list_of_scatters$s231$gpp$gg     + mark_as_target + gpp_labs      #+ labs(title = "Scenario 231")    # USE LETTER (h) for scenario 231
      )

      # ggsave(plot = pl_scatter_comparison,
      #        filename = here::here("fig", paste0("fig_B2d_pred-vs-obs_four-scen_pred",outfname_suffix,"_", parameter_set, ".png")),
      #        width = 12, height = 12/3*6, units = "cm", dpi = "print", scale = 2.0)
      ggsave(plot = pl_scatter_comparison,
             filename = here::here("fig", paste0("figure_5_",gsub("\\+","-",outfname_suffix),"_", parameter_set, ".png")),
             width = 12, height = 12/3*6, units = "cm", dpi = 300, scale = 1.8)


      # Alternative layout
      pl_scatter_comparison2 <- cowplot::plot_grid(
        nrow = 3, byrow=FALSE, labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
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
            list_of_scatters$s231$bigD13C$gg + mark_as_target + bigD13C_labs ,            # USE LETTER (h) for scenario 231
            list_of_scatters$s231$vj$gg      + mark_as_target + vj_labs      ,            # USE LETTER (h) for scenario 231
            list_of_scatters$s231$gpp$gg     + mark_as_target + gpp_labs     )            # USE LETTER (h) for scenario 231
        )
      ggsave(plot = pl_scatter_comparison2,
               filename = here::here("fig", paste0("fig_B2e_pred-vs-obs_four-scen_pred",outfname_suffix,"_", parameter_set, ".png")),
               width = 12, height = 6, units = "cm", dpi = "print", scale = 4.0)
    }
  }


  # compute scatters (and skills) for MAP of train set: (just internal)
  if (flag_plot_predictions){
    for (parameter_set in c("MAP", "Posterior")){

      list_of_train_scatters <- lapply(list_of_scenarios_to_loop_over, function(list_of_targets){
        lapply(list_of_targets, function(df_target_prediction){
          my_own_scatter(
            df_target_prediction %>% {
              if (parameter_set == "MAP") filter(., is_MAP) else .
              } %>% filter(error_sample_id==1, is_train0_test1 == 0),
            mod = "mod_biasremoved_no_err",
            shortsubtitle = TRUE)
        })
      })

      # pl_scatter_comparison2 <- cowplot::plot_grid(nrow = 3, byrow=FALSE, labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"), # TODO:
      pl_scatter_comparison2 <- cowplot::plot_grid(nrow = 6, byrow=FALSE, labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"), # TODO:
        cowplot::plot_grid(ncol = 3, # labels = c("1", "2", "3"),
          list_of_train_scatters$s228$bigD13C$gg + mark_as_target + bigD13C_labs_xNULL ,
          list_of_train_scatters$s228$vj$gg                       + vj_labs_xNULL      ,
          list_of_train_scatters$s228$gpp$gg                      + gpp_labs_xNULL     ),
        cowplot::plot_grid(ncol = 3,
          list_of_train_scatters$s227$bigD13C$gg                  + bigD13C_labs_xNULL ,
          list_of_train_scatters$s227$vj$gg      + mark_as_target + vj_labs_xNULL      ,
          list_of_train_scatters$s227$gpp$gg                      + gpp_labs_xNULL     ),
        cowplot::plot_grid(ncol = 3,
          list_of_train_scatters$s222$bigD13C$gg                  + bigD13C_labs ,
          list_of_train_scatters$s222$vj$gg                       + vj_labs      ,
          list_of_train_scatters$s222$gpp$gg     + mark_as_target + gpp_labs     ),

        cowplot::plot_grid(ncol = 3,
          list_of_train_scatters$s226$bigD13C$gg + mark_as_target + bigD13C_labs_xNULL ,
          list_of_train_scatters$s226$vj$gg      + mark_as_target + vj_labs_xNULL      ,
          list_of_train_scatters$s226$gpp$gg                      + gpp_labs_xNULL     ),
        cowplot::plot_grid(ncol = 3,
          list_of_train_scatters$s223$bigD13C$gg + mark_as_target + bigD13C_labs_xNULL ,
          list_of_train_scatters$s223$vj$gg      + mark_as_target + vj_labs_xNULL      ,
          list_of_train_scatters$s223$gpp$gg     + mark_as_target + gpp_labs_xNULL     ),
        cowplot::plot_grid(ncol = 3,
          list_of_train_scatters$s231$bigD13C$gg + mark_as_target + bigD13C_labs ,
          list_of_train_scatters$s231$vj$gg      + mark_as_target + vj_labs      ,
          list_of_train_scatters$s231$gpp$gg     + mark_as_target + gpp_labs     )
      )
      ggsave(plot = pl_scatter_comparison2,
               filename = here::here("fig", paste0("fig_B2e_pred-vs-obs_four-scen_trainingSet_pred",outfname_suffix,"_", parameter_set, ".png")),
               width = 6, height = 12, units = "cm", dpi = "print", scale = 4.0)
    }
  }




  ## Figure B: error distribution density plot ----
  ## for each scenario x target x test+train
  if (flag_plot_predictions){
    if (TRUE){ # TODO: can we make this simpler?
        # prepare facetted plotting
      # # i) bind together, ii) mutate(Scenario = "0","1","3")    # for FigB: filter(!is.na(obs)) , for FigB3: filter(target == "gpp")
      dfwide_gpp_train <- bind_rows(
        df_222_gpp |> filter(is_train0_test1 == 0) |> mutate(Scenario = "222"),
        df_227_gpp |> filter(is_train0_test1 == 0) |> mutate(Scenario = "227"),
        df_228_gpp |> filter(is_train0_test1 == 0) |> mutate(Scenario = "228"),
        # df_230_gpp |> filter(is_train0_test1 == 0) |> mutate(Scenario = "230"),
        df_231_gpp |> filter(is_train0_test1 == 0) |> mutate(Scenario = "231")
      )
      dfwide_gpp_test <- bind_rows(
        df_222_gpp |> filter(is_train0_test1 == 1) |> mutate(Scenario = "222"),
        df_227_gpp |> filter(is_train0_test1 == 1) |> mutate(Scenario = "227"),
        df_228_gpp |> filter(is_train0_test1 == 1) |> mutate(Scenario = "228"),
        # df_230_gpp |> filter(is_train0_test1 == 1) |> mutate(Scenario = "230"),
        df_231_gpp |> filter(is_train0_test1 == 1) |> mutate(Scenario = "231")
      )
      dfwide_vj <- bind_rows(
        df_222_vj |> mutate(Scenario = "222"),
        df_227_vj |> mutate(Scenario = "227"),
        df_228_vj |> mutate(Scenario = "228"),
        # df_230_vj |> mutate(Scenario = "230"),
        df_231_vj |> mutate(Scenario = "231")
      )
      dfwide_bigD13C <- bind_rows(
        df_222_bigD13C |> mutate(Scenario = "222"),
        df_227_bigD13C |> mutate(Scenario = "227"),
        df_228_bigD13C |> mutate(Scenario = "228"),
        # df_230_bigD13C |> mutate(Scenario = "230"),
        df_231_bigD13C |> mutate(Scenario = "231")
      )
      # dfwide_gpp_train |> select(              date, sitename, target) |> distinct() # 90k observations
      # dfwide_gpp_test  |> select(              date, sitename, target) |> distinct() # 127k observations
      # dfwide_vj        |> select(genus,species,year, sitename, target) |> distinct() # 585 observations
      # dfwide_bigD13C   |> select(      species,year, sitename, target) |> distinct() # 2348 observations
      # rm(df_227_vj); rm(df_227_bigD13C); rm(df_227_gpp)
      # rm(df_228_vj); rm(df_228_bigD13C); rm(df_228_gpp)
      # rm(df_230_vj); rm(df_230_bigD13C); rm(df_230_gpp)


      # make data sets long for plotting
      make_long <- function(dfwide){
        dfwide |>
          # pivot the model_output_types to long
          pivot_longer(c(mod_no_err, mod_biasremoved_no_err, mod_biasremoved_with_err),
                       names_to = "model_output_type", values_to = "modelled") |>
          mutate(model_output_type = factor(
            model_output_type,
            levels = c("mod_no_err","mod_biasremoved_no_err","mod_biasremoved_with_err"),
            labels = c("rsofun",    "bias-corrected",        "with struct. uncert."))) |>
          # derive column `parameters` ("MAP" or "Posterior") from `is_MAP`
          mutate(is_MAP = factor(ifelse(is_MAP, "MAP", "Posterior"))) |> rename(parameters = is_MAP) |>
          # derive column `dataset` ("train" or "test") from column `is_train0_test1`
          mutate(is_train0_test1 = factor(ifelse(is_train0_test1==1, "test", "train"))) |> rename(dataset = is_train0_test1)
      }
      dflong_gpp_train <- dfwide_gpp_train |>
        select(posterior_sample_id, error_sample_id, Scenario, is_train0_test1, is_MAP, sitename, target,
               obs,                 date, # these are target specific observation_metadata
               mod_no_err, mod_biasremoved_no_err, mod_biasremoved_with_err) |>
        make_long()
      dflong_gpp_test <- dfwide_gpp_test |>
        select(posterior_sample_id, error_sample_id, Scenario, is_train0_test1, is_MAP, sitename, target,
               obs,                 date, # these are target specific observation_metadata
               mod_no_err, mod_biasremoved_no_err, mod_biasremoved_with_err) |>
        make_long()
      dflong_vj <- dfwide_vj |>
        select(posterior_sample_id, error_sample_id, Scenario, is_train0_test1, is_MAP, sitename, target,
               obs, genus, species, year, # these are target specific observation_metadata
               mod_no_err, mod_biasremoved_no_err, mod_biasremoved_with_err) |>
        make_long()
      dflong_bigD13C <- dfwide_bigD13C |>
        select(posterior_sample_id, error_sample_id, Scenario, is_train0_test1, is_MAP, sitename, target,
               obs,        species, year, # these are target specific observation_metadata
               mod_no_err, mod_biasremoved_no_err, mod_biasremoved_with_err) |>
        make_long()
      rm(dfwide_gpp_train)
      rm(dfwide_gpp_test)
      rm(dfwide_vj)
      rm(dfwide_bigD13C)

      # manually define what to show as output depending on the scenario:
      df_B1and2and3 <- list(
        # for gpp:
        gpp = bind_rows(dflong_gpp_test, dflong_gpp_train) |>
          # remove the bias-corrected values for gpp since we did not fit a bias
          filter(!(model_output_type %in% c("bias-corrected"))) |>
          # select what to plot and how to name it
          mutate(y_facet = case_when(
            Scenario == "222" & model_output_type == "rsofun" &               parameters == "MAP"       ~ "MAP",
            Scenario == "222" & model_output_type == "rsofun" &               parameters == "Posterior" ~ "Posterior",
            Scenario == "222" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error",
            Scenario == "228" & model_output_type == "rsofun" &               parameters == "MAP"       ~ "MAP",
            Scenario == "228" & model_output_type == "rsofun" &               parameters == "Posterior" ~ "Posterior",
            Scenario == "228" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error", # since gpp was not fitted we don't have an error model
            Scenario == "227" & model_output_type == "rsofun" &               parameters == "MAP"       ~ "MAP",
            Scenario == "227" & model_output_type == "rsofun" &               parameters == "Posterior" ~ "Posterior",
            Scenario == "227" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error", # since gpp was not fitted we don't have an error model
            Scenario == "230" & model_output_type == "rsofun" &               parameters == "MAP"       ~ "MAP",
            Scenario == "230" & model_output_type == "rsofun" &               parameters == "Posterior" ~ "Posterior",
            Scenario == "230" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error",
            Scenario == "231" & model_output_type == "rsofun" &               parameters == "MAP"       ~ "MAP",
            Scenario == "231" & model_output_type == "rsofun" &               parameters == "Posterior" ~ "Posterior",
            Scenario == "231" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error",
            # all else is not plotted
            TRUE ~ "remove") |> factor(levels = c("MAP","Posterior","Post.+Error"))) |>
          filter(y_facet != "remove"),
        # for vj:
        vj = dflong_vj |>
          # select what to plot and how to name it
          mutate(y_facet = case_when(
            Scenario == "222" & model_output_type == "bias-corrected" &       parameters == "MAP"       ~ "MAP",
            Scenario == "222" & model_output_type == "bias-corrected" &       parameters == "Posterior" ~ "Posterior",
            Scenario == "222" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error",
            Scenario == "228" & model_output_type == "bias-corrected" &       parameters == "MAP"       ~ "MAP",
            Scenario == "228" & model_output_type == "bias-corrected" &       parameters == "Posterior" ~ "Posterior",
            Scenario == "228" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error", # since vj was not fitted we don't have an error model
            Scenario == "227" & model_output_type == "bias-corrected" &       parameters == "MAP"       ~ "MAP",
            Scenario == "227" & model_output_type == "bias-corrected" &       parameters == "Posterior" ~ "Posterior",
            Scenario == "227" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error",
            Scenario == "230" & model_output_type == "bias-corrected" &       parameters == "MAP"       ~ "MAP",
            Scenario == "230" & model_output_type == "bias-corrected" &       parameters == "Posterior" ~ "Posterior",
            Scenario == "230" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error",
            Scenario == "231" & model_output_type == "bias-corrected" &       parameters == "MAP"       ~ "MAP",
            Scenario == "231" & model_output_type == "bias-corrected" &       parameters == "Posterior" ~ "Posterior",
            Scenario == "231" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error",
            # all else is not plotted
            TRUE ~ "remove") |> factor(levels = c("MAP","Posterior","Post.+Error"))) |>
          filter(y_facet != "remove"),
        # for bigD13C:
        bigD13C = dflong_bigD13C |>
          # select what to plot and how to name it
          mutate(y_facet = case_when(
            Scenario == "222" & model_output_type == "bias-corrected" &       parameters == "MAP"       ~ "MAP",
            Scenario == "222" & model_output_type == "bias-corrected" &       parameters == "Posterior" ~ "Posterior",
            Scenario == "222" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error",
            Scenario == "228" & model_output_type == "bias-corrected" &       parameters == "MAP"       ~ "MAP",
            Scenario == "228" & model_output_type == "bias-corrected" &       parameters == "Posterior" ~ "Posterior",
            Scenario == "228" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error",
            Scenario == "227" & model_output_type == "bias-corrected" &       parameters == "MAP"       ~ "MAP",
            Scenario == "227" & model_output_type == "bias-corrected" &       parameters == "Posterior" ~ "Posterior",
            Scenario == "227" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error", # since bigD13C was not fitted we don't have an error mode
            Scenario == "230" & model_output_type == "bias-corrected" &       parameters == "MAP"       ~ "MAP",
            Scenario == "230" & model_output_type == "bias-corrected" &       parameters == "Posterior" ~ "Posterior",
            Scenario == "230" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error",
            Scenario == "231" & model_output_type == "bias-corrected" &       parameters == "MAP"       ~ "MAP",
            Scenario == "231" & model_output_type == "bias-corrected" &       parameters == "Posterior" ~ "Posterior",
            Scenario == "231" & model_output_type == "with struct. uncert." & parameters == "Posterior" ~ "Post.+Error",
            # all else is not plotted
            TRUE ~ "remove") |> factor(levels = c("MAP","Posterior","Post.+Error"))) |>
          filter(y_facet != "remove")
      )

      df_B1_density    <- lapply(df_B1and2and3, \(df) df |> filter(!is.na(obs)))                    # remove NA observations
      df_B2_scatter    <- lapply(df_B1and2and3, \(df) df |> filter(!is.na(obs), dataset == "test")) # remove NA observations and test data set
      df_B3_timeseries <- df_B1and2and3["gpp"]                                                      # keep NA observations and test data set, only use gpp

    # derive alternative representation of observations as average observations (e.g. across species, samples, etc...)
    df_B1and2and3_avgObs <- list(
      gpp = df_B1and2and3$gpp |> rename(obs_avg = obs) |> mutate(obs_sd  = NA, obs_n   = 1),
      vj  = df_B1and2and3$vj |>
        group_by(posterior_sample_id, error_sample_id, Scenario, dataset, parameters, sitename, target, model_output_type, y_facet) |>
        # group_by(-c('genus', 'species', 'year')) |>
        summarise(obs_avg  = mean(obs), obs_sd  = sd(obs), obs_n   = n(),
                  # for the sampled error model the error was sampled independently for each observations. Taking mean would be wrong: just take first sample
                  # mod_mean  = mean(modelled), mod_sd  = sd(modelled), mod_n   = length(unique(modelled))
                  modelled = first(modelled)
                  ),
      bigD13C = df_B1and2and3$bigD13C |>
      group_by(posterior_sample_id, error_sample_id, Scenario, dataset, parameters, sitename, target, model_output_type, y_facet) |>
      # group_by(-c('genus', 'species', 'year')) |>
      summarise(obs_avg  = mean(obs), obs_sd  = sd(obs), obs_n   = n(),
                # for the sampled error model the error was sampled independently for each observations. Taking mean would be wrong: just take first sample
                # mod_mean  = mean(modelled), mod_sd  = sd(modelled), mod_n   = length(unique(modelled))
                  modelled = first(modelled)
                )
    )
    # df_B1and2and3_avgObs$gpp
    # df_B1and2and3_avgObs$bigD13C
    # df_B1and2and3_avgObs$vj
    df_B1_density_avgObs <- lapply(df_B1and2and3_avgObs, \(df) df |> filter(!is.na(obs_avg)))                    # remove NA observations
    df_B2_scatter_avgObs <- lapply(df_B1and2and3_avgObs, \(df) df |> filter(!is.na(obs_avg), dataset == "test")) # remove NA observations and test data set
    # df_B1_density$bigD13C |> filter(sitename == "lon_-111.80_lat_+040.77") |> filter(posterior_sample_id==1, error_sample_id==1, y_facet == "Posterior", Scenario %in% c(3,4))
    # df_B1_density_avgObs$bigD13C |> filter(sitename == "lon_-111.80_lat_+040.77") |> filter(posterior_sample_id==1, error_sample_id==1, y_facet == "Posterior", Scenario %in% c(3,4))

    }

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

              # Figure B) variant 1:
              # pl_density_alltargets_v3 <- ggplot(dat_to_plot, aes(x = modelled - obs, y = Scenario)) +
              #   scale_y_discrete(limits = rev) +
              #   # add Posterior (fill):
              #   ggridges::geom_density_ridges( data = function(df) {df |> filter(y_facet == "Posterior")},
              #     mapping = aes(fill = dataset), # linetype = dataset
              #     scale = 0.8) +
              #   # add error (solid):
              #   ggridges::geom_density_ridges( data = function(df) {df |> filter(y_facet == "Post.+Error")},
              #     mapping = aes(color = dataset, linetype = "Post.+Error"),
              #     scale = 0.8, fill = NA, key_glyph = "abline") + # "polygon" or "timeseries"
              #   # # add MAP (dashed):
              #   # ggridges::geom_density_ridges( data = function(df) {df |> filter(y_facet == "MAP")},
              #   #   mapping = aes(color = dataset, linetype = "MAP"),
              #   #   scale = 0.8, fill = NA, key_glyph = "abline") + # "polygon" or "timeseries"
              #   # layout:
              #   scale_fill_manual(NULL, aesthetics = c("fill", "colour"), values = c("test"="#29a274ff", "train" = t_col("#777055ff"))) +
              #   # scale_linetype_manual(NULL, values = c("Post.+Error" = "3313", "MAP"         = "3232", "fixed"       = "solid")) +
              #   scale_linetype_manual(NULL, values = c("Post.+Error" = "solid", "MAP"         = "3232", "fixed"       = "solid")) +
              #   # theme:
              #   theme_classic() +
              #   theme(legend.position        = "inside",
              #         legend.position.inside = c(0.02,0.02),
              #         legend.justification   = c(0,0),
              #         legend.direction       = "vertical",
              #         legend.box             = "horizontal",
              #         legend.background      = element_blank()) +
              #   # add line at 0:
              #   scale_x_continuous(minor_breaks = 0.00001) + # 0 makes it disappear
              #   theme(panel.grid.minor.x = element_line()) +
              #   # axis labels and facet grid labels
              #   labs(x = "Predicted - Observed", linetype = NULL) +
              #   facet_grid(
              #     ~target,
              #     scales = "free_x",
              #     labeller = as_labeller(c("gpp"     = "(a) GPP:",
              #                              "vj"      = "(b) Vcmax/Jmax:",
              #                              "bigD13C" = "(c) Δ13C:"))) +
              #   theme(strip.background = element_blank(),
              #         strip.text       = element_text(hjust = 0, size = 12, face = "bold"))
              #
              # ggsave(here::here(file.path("fig",paste0("fig_B_predObs_errorDensity_",outfname_suffix,".png"))),
              #        plot = pl_density_alltargets_v3, width=12, height=8, units = "cm", scale = 1.3)
              #
              # # redoc plot versus obs_avg instead of obs
              # pl_density_alltargets_v3_avgObs <- (pl_density_alltargets_v3 %+% dat_to_plot_avgObs) +
              #   aes(x = modelled - obs_avg, y = Scenario) +
              #   labs(x = "Predicted - Avg. Observed")
              # ggsave(here::here(file.path("fig",paste0("fig_B1b_predObs_errorDensity_",outfname_suffix,".png"))),
              #        plot = pl_density_alltargets_v3_avgObs, width=12, height=8, units = "cm", scale = 1.3)
              #
              # # and combine both, arranging axes:
              # # Extend x-axis limits of pl_density_alltargets_v3_avgObs to be the same as pl_density_alltargets_v3
              # # by using a geom_blank() layer (source: https://stackoverflow.com/a/21585521/3915004) :
              # pl_density_alltargets_v3_build <- ggplot2::ggplot_build(pl_density_alltargets_v3)
              # dummy <- data.frame(
              #   target = c("gpp", "gpp",
              #              # "vj_obs__", "vj_obs__",
              #              # "bigD13C_obs_permil", "bigD13C_obs_permil") |>
              #              "vj", "vj",
              #              "bigD13C", "bigD13C") |>
              #     factor(levels = c("gpp", "vj", "bigD13C")),
              #   x      = c(pl_density_alltargets_v3_build$layout$get_scales(1)$x$range$range,
              #              pl_density_alltargets_v3_build$layout$get_scales(2)$x$range$range,
              #              pl_density_alltargets_v3_build$layout$get_scales(3)$x$range$range),
              #   y = 1
              # )
              #
              # pl_density_alltargets_v3_avgObs_xlimsExtended <- pl_density_alltargets_v3_avgObs + geom_blank(data = dummy, aes(x=x, y=y))
              # pl_density_alltargets_v3_comparison <- cowplot::plot_grid(
              #   pl_density_alltargets_v3,
              #   pl_density_alltargets_v3_avgObs_xlimsExtended +
              #     facet_grid( ~target, scales = "free_x",
              #     labeller = as_labeller(c("gpp"     = "(d) GPP:",
              #                              "vj"      = "(e) Vcmax/Jmax:",
              #                              "bigD13C" = "(f) Δ13C:"))),
              #   ncol = 1, rel_heights = c(1,1))
              # ggsave(here::here(file.path("fig",paste0("fig_B1c_predObs_errorDensity_",outfname_suffix,".png"))),
              #      plot = pl_density_alltargets_v3_comparison, width=12, height=16, units = "cm", scale = 1.3)




    ## Figure B (variant 2): error distribution density plot: ----
    # make plots for different targets separately
    dat_to_plot2 <- dat_to_plot |>
      mutate(scenario = as.integer(Scenario)) |>
      # ensure plotting order by defining Scenario as factor
      left_join(scenario_labels, by = join_by(scenario)) |>
      arrange(desc(label)) |>
      mutate(Setup = forcats::as_factor(as.character(label_targets)))

    pl_density_singleTarget_base <- ggplot(filter(dat_to_plot2, target == "vj"), aes(x = modelled - obs, y = Setup)) +
      scale_y_discrete("Setup", labels = \(x) parse(text = x)) +
      # add Posterior (fill):
      ggridges::geom_density_ridges( data = function(df) {df |> filter(y_facet == "Posterior")},
        mapping = aes(fill = dataset), scale = 0.8) +
      # add error (solid):
      ggridges::geom_density_ridges( data = function(df) {df |> filter(y_facet == "Post.+Error")},
        mapping = aes(color = dataset, linetype = "Post.+Error"), scale = 0.8, fill = NA, key_glyph = "abline") +
      # layout:
      scale_fill_manual(NULL, aesthetics = c("fill", "colour"), values = c("test"="#29a274ff", "train" = t_col("#777055ff"))) +
      scale_linetype_manual(NULL, values = c("Post.+Error" = "solid", "MAP"         = "3232", "fixed"       = "solid")) +
      # theme:
      theme_classic() +
      theme(axis.text.y = element_text(family = "Helvetica", color = "black", hjust=0)) +
      theme(axis.text.x = element_text(family = "Helvetica", color = "black", size = 9)) +
      theme(legend.position        = "inside",
            legend.position.inside = c(0.01,0.01),
            legend.justification   = c(0,0),
            legend.direction       = "vertical",
            legend.box             = "horizontal",
            legend.background      = element_blank()) +
      # add line at 0:
      scale_x_continuous(minor_breaks = 0.00001) + # 0 makes it disappear
      theme(panel.grid.minor.x = element_line()) +
      # axis labels and facet grid labels
      # coord_cartesian(xlim = c(-1,1)) +
      labs(x = "Predicted - Observed", linetype = NULL) +
      facet_grid(
        ~target,
        scales = "free_x",
        labeller = as_labeller(c("gpp"     = "(c) GPP:",
                                 "vj"      = "(b) VJ:",
                                 "bigD13C" = "(a) Δ:"))) +
      theme(strip.background = element_blank(),
            strip.text       = element_text(hjust = 0, size = 12, face = "bold")) +
      ggtext

    pl_density_bigD13C <- (pl_density_singleTarget_base %+% filter(dat_to_plot2, target == "bigD13C")) +
      #coord_cartesian(xlim = c(-10,10)) +
      scale_x_continuous("Pred. \u2212 Obs. Δ (\u2030)",
                         #expression(paste("Pred. \u2212 Obs. Δ"^13,"C (\u2030)")),
                         minor_breaks = 0.00001,
                         limits = 1.3*c(-10,10)) + coord_cartesian(xlim = c(-10,10)) + # make limits slightly larger for density computation
      labs(linetype = NULL) + theme(strip.text = element_blank())
    pl_density_vj <- (pl_density_singleTarget_base %+% filter(dat_to_plot2, target == "vj")) +
      #coord_cartesian(xlim = c(-1,1)) +
      scale_x_continuous("Pred. \u2212 Obs. VJ (-)",
                         #"Pred. \u2212 Obs. Vcmax/Jmax (-) ",
                         minor_breaks = 0.00001,
                         limits = 1.3*c(-1,1)) + coord_cartesian(xlim = c(-1,1)) + # make limits slightly larger for density computation
      labs(linetype = NULL) + theme(strip.text = element_blank())
    pl_density_gpp <- (pl_density_singleTarget_base %+% filter(dat_to_plot2, target == "gpp")) +
      #coord_cartesian(xlim = c(-5,5)) +
      scale_x_continuous("Pred. \u2212 Obs. GPP (g C m\u207B\u00B2 s\u207B\u00B9)",
                         #expression(paste("Pred. \u2212 Obs. GPP (g C m"^-2, "s"^-1, ")")),
                         #expression(paste("Pred. \u2212 Obs. GPP (g C m"^-2, "s"^-1, ")")),
                         minor_breaks = 0.00001,
                         limits = 1.3*c(-5,5)) + coord_cartesian(xlim = c(-5,5)) + # make limits slightly larger for density computation
      labs(linetype = NULL) + theme(strip.text = element_blank())

    nr_of_scenarios <- length(unique(dat_to_plot2$Setup))
    pl_density_separately_generated <- cowplot::plot_grid(
      pl_density_bigD13C + coord_cartesian(ylim = c(1.5, nr_of_scenarios+0.7)) + theme(legend.position = "inside", legend.position.inside = c(1,1), legend.justification = c(1,1)),
      pl_density_vj      + coord_cartesian(ylim = c(1.5, nr_of_scenarios+0.7)) + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), legend.position = "none"),
      pl_density_gpp     + coord_cartesian(ylim = c(1.5, nr_of_scenarios+0.7)) + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), legend.position = "none"),
      nrow=1, rel_widths = c(1.5,1,1), align = "h",
      labels = c("(a)","(b)","(c)"), label_fontfamily = "Helvetica", label_size = 11)#, hjust = 0, label_x = 0, vjust=1, label_y = Inf)

    ggsave(here::here(file.path("fig",paste0("fig_B1d_predObs_errorDensity_",outfname_suffix,".png"))),
         plot = pl_density_separately_generated,
         width = 12, height = 6, units = "cm", dpi=300, scale = 1.8)
    ggsave(here::here(file.path("fig",paste0("figure_4_",outfname_suffix,".png"))),
         plot = pl_density_separately_generated,
         width = 12, height = 6, units = "cm", dpi=300, scale = 1.8)

    # statistics of preidiction and structural error
    residual_stats <- dat_to_plot2 |>
      group_by(scenario, label, label_targets, Setup, y_facet, target, dataset) |>
      summarise(N = n(),
                mean_modelled  = mean(modelled),
                mean_pred_obs = mean(modelled - obs),
                std_pred_obs = sd(modelled - obs),
                mean_pred_obs_div_by_modelled = mean_pred_obs/mean_modelled,
                std_pred_obs_div_by_modelled = std_pred_obs/mean_modelled)
    residual_stats
    readr::write_csv(residual_stats, here::here(paste0("fig/table-d-residuals.stats.allscen_",outfname_suffix,".csv")))
  }




  ## Figure B3: make a proper gpp time series plot ----
  plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario == "231", dataset == "train"), fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s231_train_",outfname_suffix,".png")))
  plot_predobs_gpp_timeseries3(df_B3_timeseries$gpp |> filter(Scenario == "231", dataset == "test"),  fpath = here::here("fig",paste0("fig_BXY_predObs_gppTimeSeries_s231_test_",outfname_suffix,".png")))

}
