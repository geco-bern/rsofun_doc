# check alternative parallelization strategies

# source: https://cran.r-project.org/web/packages/BayesianTools/vignettes/InterfacingAModel.html#parallelization
# source: https://cran.r-project.org/web/packages/BayesianTools/vignettes/BayesianTools.html
library(BayesianTools)

# Variant 3: parallelize our likelihood (only applies to runread() not the onestep model): this appears not promising

# Variant 1: parallelize internal chains: these are simultaneously run (within-sampler) and in case of DEzs are used for the proposal
#                                         because of this, they are not independent, and require to exchange information
# IMPORTANT NOTE on DE MCMC:    "For sucessful sampling at least 2*d chains, with d being the number of parameters, need to be run in parallel."
# IMPORTANT NOTE on DEzs MCMC:  "These extensions allow for fewer chains (i.e. 3 chains are usually enough for up to 200 parameters) and parallel computing as the current position of each chain is only dependent on the past states of the other chains."

# Variant 2: parallelize multiple overall chains and combine together
#Parallelize the three outer chains
reprex({
library(BayesianTools)
library(doParallel)
library(foreach)

# APPROACH WORKING:
# Your external data
x <- c(1, 2, 3)

ll_factory <- function(my_x) {
  function(param) {sum(dnorm(param - my_x, log=TRUE))}
}

cl <- makeCluster(3)
registerDoParallel(cl)

results <- foreach(i = 1:3, .packages='BayesianTools') %dopar% {
  # inside worker: rebuild the closure so it picks up 'x'
  rebuilt_ll <- ll_factory(x)
  bayesianSetup <- createBayesianSetup(likelihood = rebuilt_ll, lower = c(-10, -10, -10), upper = c(10, 10, 10))
  runMCMC(bayesianSetup, sampler = "DEzs", settings = list(iterations=1000, nrChains=1))
}
stopCluster(cl)

out <- createMcmcSamplerList(results)
# plot(out)
summary(out)
})
# Runtime on M4:    0.102  sec.
# Runtime on dash:  0.936  sec.

# Variant 2+1 (might be slower): Extend this to also parallelize the internal chains:
reprex({
library(BayesianTools)
library(doParallel)
library(foreach)

# APPROACH WORKING:
# Your external data
x <- c(1, 2, 3)

ll_factory <- function(my_x) {
  function(param) {sum(dnorm(param - my_x, log=TRUE))}
}

n_MCMC_parallel  <- 3 # running several MCMCs in parallel: https://cran.r-project.org/web/packages/BayesianTools/vignettes/InterfacingAModel.html#running-several-mcmcs-in-parallel
n_within_sampler <- 3 # within sampler or in-build parallelization:   https://cran.r-project.org/web/packages/BayesianTools/vignettes/InterfacingAModel.html#within-sampler-parallelization as well as https://cran.r-project.org/web/packages/BayesianTools/vignettes/BayesianTools.html#reference-on-creating-likelihoods

cl <- makeCluster(n_MCMC_parallel)
registerDoParallel(cl)

results <- foreach(i = 1:n_MCMC_parallel, .packages='BayesianTools') %dopar% {
  # inside worker: rebuild the closure so it picks up 'x'
  rebuilt_ll <- ll_factory(x)
  bayesianSetup <- createBayesianSetup(
    likelihood = rebuilt_ll,
    lower = c(-10, -10, -10), upper = c(10, 10, 10),
    parallel = n_within_sampler)
  runMCMC(bayesianSetup,
          sampler = 'DEzs',
          settings = list(iterations = 1000, nrChains = 1, # number of independent chains (kept here at 1 to parallelize with foreach and comining as a McmcSamplerList())
                          startValue = n_within_sampler    # number of internal chains to be sampled
          ))
}
stopCluster(cl)

out <- createMcmcSamplerList(results)
# plot(out)
summary(out)
})
# Runtime on M4:    2.822  sec.
# Runtime on dash: 28.363  sec.




library(BayesianTools)
library(doParallel)
library(foreach)

# Your external data
x <- c(1, 2, 3)

# Your likelihood function referring to 'x'
ll <- function(param) sum(dnorm(param - x, log=TRUE))

bayesianSetup <- createBayesianSetup(likelihood = ll, lower = c(-10, -10, -10), upper = c(10, 10, 10))

cl <- makeCluster(4)
registerDoParallel(cl)

# Export 'x' so each worker has access to it
results <- foreach(i = 1:4, .packages='BayesianTools', .export='x') %dopar% {
  runMCMC(bayesianSetup, sampler = "DEzs", settings = list(iterations=1000))
}

stopCluster(cl)


ll_factory <- function(my_x) {
  function(param) {sum(dnorm(param - my_x, log=TRUE))}
}
results <- foreach(i = 1:4, .packages='BayesianTools') %dopar% {
  x <- c(1,2,3)
  bayesianSetup <- createBayesianSetup(likelihood = ll_factory(x), lower = c(-10, -10, -10), upper = c(10, 10, 10))
  runMCMC(bayesianSetup, sampler = "DEzs", settings = list(iterations=1000))
}







library(tidyverse)
library(rpmodel)
library(rgeco) # pak::pkg_install("geco-bern/rgeco")
library(dplyr)
library(purrr)
library(rsofun)  # install from branch simple_pmodel_v2
library(ingestr)
library(BayesianTools)

## Load forcing and targets data ----
bigD13C_vj_gpp_drivers <- read_rds(here::here("data/01_bigD13C-vj-gpp_calibsofun_drivers.rds"))
bigD13C_vj_gpp_obs     <- read_rds(here::here("data/01_bigD13C-vj-gpp_calibsofun_obs.rds"))

## Read test-train split ----
df_test_train_split <- read_csv(here::here("data/01_test_train_split.csv"))

sites_train <- df_test_train_split |> filter(dataset == "train")
sites_test  <- df_test_train_split |> filter(dataset == "test")

## Preprocess observation data (gpp)

# some observations of gpp are negative TODO: filter them out
## for training sites
bigD13C_vj_gpp_obs |> filter(sitename %in% sites_train$sitename) |>
  filter(run_model == "daily") |> unnest(data) |>
  ggplot(aes(x=gpp, color = sitename)) + geom_density()# + facet_wrap(~sitename)
## for testing sites
bigD13C_vj_gpp_obs |> filter(sitename %in% sites_test$sitename) |>
  filter(run_model == "daily") |> unnest(data) |>
  ggplot(aes(x=gpp, color = sitename)) + geom_density()# + facet_wrap(~sitename)

# some observations of gpp are NA, filter them out:
# some observations of gpp are negative, filter those below -2 out
# TODO: document this
bigD13C_vj_gpp_obs <- bind_rows(
  # filter out NAs in gpp observations
  bigD13C_vj_gpp_obs |> filter(run_model == "daily") |>
    unnest(data) |>
    filter(!is.na(gpp)) |>
    filter(gpp > -2) |>
    nest(data = -c(sitename, run_model, targets)),
  # do not filter out anything from the other observations
  bigD13C_vj_gpp_obs |> filter(run_model != "daily")
)

# some model input leads to NA in modeled gpp, filter them out:
# bigD13C_vj_gpp_drivers <- bigD13C_vj_gpp_drivers |> filter(sitename != "US-Bar")
bigD13C_vj_gpp_drivers2 <- bind_rows(
  # correct missing ccov
  bigD13C_vj_gpp_drivers |> filter(sitename == "US-Bar") |>
    unnest(forcing) |>
    mutate(ccov = if_else(is.na(ccov), 0, ccov)) |>
    nest(forcing = -c(sitename, run_model, params_siml, site_info)),
  # keep other unchanged
  bigD13C_vj_gpp_drivers |> filter(sitename != "US-Bar")
)
# NOTE: this appeared to be related only to site "US-Bar"
# TODO: document this
# TODO: find out why. Are there some wrong units in input data? NO, it appears all to be caused by ccov=NA
bigD13C_vj_gpp_drivers |>
  group_by(sitename) |>
  filter(sitename %in% c("US-Bar", "US-Ton")) |>
  unnest(forcing) |> slice(1:10)
# bigD13C_vj_gpp_drivers |> filter(sitename == "US-Bar") |> unnest(forcing) |> filter(is.na(ccov)) # this is only 355 rows
# bigD13C_vj_gpp_drivers |> filter(sitename == "US-Bar") |> unnest(forcing) |> filter(!is.na(ccov))



## Apply test-train split to data ----
train_drivers <- bigD13C_vj_gpp_drivers2 |> filter(sitename %in% sites_train$sitename)
train_obs     <- bigD13C_vj_gpp_obs     |> filter(sitename %in% sites_train$sitename)

test_drivers <- bigD13C_vj_gpp_drivers2 |> filter(sitename %in% sites_test$sitename)
test_obs     <- bigD13C_vj_gpp_obs     |> filter(sitename %in% sites_test$sitename)




# Setup actual calibration:
set.seed(1982)
source(here::here("R/calibration_helpers.R"))

# FROM THE REVISION PLAN:
# Setup 1: global, reduced parameter set (as in initial manuscript version), only GPP as target
# Setup 2: global, full parameter set, only GPP as target
# Setup 3: global, full parameter set, GPP and traits as target
# We expect Setup 2 to yield wider posteriors than from Setup 1, and that posterior distributions will be narrowed again by Setup 3. This experimental design will allow us to demonstrate the robustness (or absence thereof) of the MCMC and the usefulness of using traits for simultaneously calibrating with fluxes.


## Load loglikelihood ----
source(here::here("R/cost_likelihood_pmodel_bigD13C_vj_gpp.R"))

## Setup the settings for the three calibration setups ----
par_setup1 <- list(
  kphio           = list(lower = 0.02, upper = 0.15, init = 0.05),
  kphio_par_a     = list(lower = -0.004, upper = -0.001, init = -0.0025),
  kphio_par_b     = list(lower = 10, upper = 30, init = 20),
  soilm_thetastar = list(lower = 1, upper = 250, init = 40),
  soilm_betao     = list(lower = 0.0, upper = 1.0, init = 0.0),
  err_gpp         = list(lower = 0.1, upper = 3, init = 0.8),
  err_bigD13C     = list(lower = 0.1, upper = 3, init = 0.8), # TODO: without err_bigD13C and err_vj this errors
  err_vj          = list(lower = 0.1, upper = 3, init = 0.8)  # TODO: without err_bigD13C and err_vj this errors
)

#TODO: define ranges for new parameters
par_setup23 <- list(
  kphio           = list(lower = 0.02, upper = 0.15, init = 0.05),
  kphio_par_a     = list(lower = -0.004, upper = -0.001, init = -0.0025),
  kphio_par_b     = list(lower = 10, upper = 30, init = 20),
  soilm_thetastar = list(lower = 1, upper = 250, init = 40),
  soilm_betao     = list(lower = 0.0, upper = 1.0, init = 0.0),
  beta_unitcostratio = as.list(c(lower = 0.1, upper = 3.0, init = 1.0)*146.0),
  rd_to_vcmax        = as.list(c(lower = 0.1, upper = 3.0, init = 1.0)*0.014),      # 0.014 value from Atkin et al. 2015 for C3 herbaceous
  tau_acclim         = as.list(c(lower = 0.1, upper = 3.0, init = 1.0)*20.0),
  kc_jmax            = as.list(c(lower = 0.1, upper = 3.0, init = 1.0)*0.41),
  err_gpp         = list(lower = 0.01, upper = 3, init = 0.8),
  err_bigD13C     = list(lower = 0.01, upper = 3, init = 0.8), # TODO: without err_bigD13C and err_vj this errors
  err_vj          = list(lower = 0.01, upper = 3, init = 0.8)  # TODO: without err_bigD13C and err_vj this errors
)
# From 02_sensitivity_analysis.R:
# # best parameter values (initial values)
# par_cal_best <- c(
#   kphio              = list(lower = 0.03,   upper = 0.15,  init = 0.09423773 ),
#   kphio_par_a        = list(lower = -0.004, upper = 0.001, init = 0.0025     ),
#   kphio_par_b        = list(lower = 10,     upper = 30,    init = 20         ),
#   soilm_thetastar    = list(lower = 0,      upper = 240,   init = 0.6*240    ),
#   soilm_betao        = list(lower = 0,      upper = 1,     init = 0.2        ),
#   beta_unitcostratio = list(lower = 50.0,   upper = 200.0, init = 146.0      ),
#   rd_to_vcmax        = list(lower = 0.01,   upper = 0.1,   init = 0.014      ),
#   tau_acclim         = list(lower = 7.0,    upper = 60.0,  init = 30.0       ),
#   kc_jmax            = list(lower = 0.2,    upper = 0.8,   init = 0.41       ),
#   error_gpp          = list(lower = 0.01    upper = 4,     init = 1          ),
# )


create_settings_and_par_fixed <- function(par, burnin=1, iterations=5, metric = cost_likelihood_pmodel_bigD13C_vj_gpp){
  default_par_fixed <- list(# fix parameter value from previous calibration
    kphio              = 0.04998,
    kphio_par_a        = 0.0,
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover paper setup with soil moisture stress
    soilm_betao        = 0.0,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 20.0,
    kc_jmax            = 0.41
  )

  list(
    settings = list(
      method = "BayesianTools",
      metric = metric,
      control = list(
        sampler = "DEzs",
        settings = list(
          burnin = burnin,     #10000,
          iterations = iterations, #50000,
          nrChains = 3,       # number of independent chains
          startValue = 3      # number of internal chains to be sampled
        )),
      par = par
    ),
    # only keep par_fixed that are not set as par
    par_fixed = default_par_fixed[
      !(names(default_par_fixed) %in% names(par))
    ]
  )
}
#create_settings_and_par_fixed(par_setup1)$settings
#create_settings_and_par_fixed(par_setup1)$par_fixed
#create_settings_and_par_fixed(par_setup23)$settings
#create_settings_and_par_fixed(par_setup23)$par_fixed

## Setup the data (drivers and obs) for the three calibration setups ----

# subset different combination of target variables
# for easier handling do this in combined drivobs-object
drivobs_bigD13C_vj_gpp <- dplyr::inner_join(
  train_drivers,
  train_obs,
  by = join_by(sitename, run_model))

# TODO: check if passing combined drivobs is computationally more efficient for calib_sofun()
drivobs_setup12 <- drivobs_bigD13C_vj_gpp |>
  unnest_wider(targets) |>
  filter(gpp) |>
  nest(targets = c(vj, bigD13C, gpp))

drivobs_setup3 <- drivobs_bigD13C_vj_gpp


## Calibrate parameters ----
# library(profvis)
in_calib_setup23 <- create_settings_and_par_fixed(par_setup23, burnin=1, iterations=5)
# profvis({
  out_calib_setup3 <- calib_sofun(
    drivers   = select(drivobs_setup3, sitename, run_model, params_siml, site_info, forcing),
    obs       = select(drivobs_setup3, sitename, run_model, targets, data),
    settings  = in_calib_setup23$settings,
    # arguments for the cost function
    par_fixed = in_calib_setup23$par_fixed
  )
# })

# TRY TO SETUP A FASTER VERSION
default_par_fixed <- list(# fix parameter value from previous calibration
  kphio              = 0.04998,
  kphio_par_a        = 0.0,
  kphio_par_b        = 1.0,
  soilm_thetastar    = 0.6 * 240,  # to recover paper setup with soil moisture stress
  soilm_betao        = 0.0,
  beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
  tau_acclim         = 20.0,
  kc_jmax            = 0.41
)
curr_par <- par_setup23
curr_par_fixed <- default_par_fixed[!(names(default_par_fixed) %in% names(curr_par))]
in_calib_setup23_v2 <- list(
  settings = list(
    method = "BayesianTools",
    metric = cost_likelihood_pmodel_bigD13C_vj_gpp_v2,
    control = list(
      sampler = "DEzs",
      settings = list(
        burnin     = 1,
        iterations = 5,
        nrChains   = 3, # number of independent chains
        startValue = 3  # number of internal chains to be sampled
      )),
    par = curr_par
  ),
  # OTHER ARGUMEMENTS TO BE HANDED TO cost_likelihood
  # only keep par_fixed that are not set as curr_par
  par_fixed = curr_par_fixed,
  parallel  = FALSE,
  ncores    = 1,
  # NOTE THAT FOR COMPUTATIONAL SPEED ARGUMENTS obs and drivers ARE IGNORED, LL USES BELOW INPUTS:
  daily_drivers   = select(drivobs_setup3, sitename, run_model, params_siml, site_info, forcing) |> filter(run_model == "daily"),
  onestep_drivers = select(drivobs_setup3, sitename, run_model, params_siml, site_info, forcing) |> filter(run_model == "onestep") |> group_by(sitename) |> unnest(c(params_siml, forcing)),
  daily_obs       = select(drivobs_setup3, sitename, run_model, targets, data) |> filter(run_model == "daily") |> select(sitename, run_model, targets, data) |> unnest(c(data)),
  onestep_obs     = select(drivobs_setup3, sitename, run_model, targets, data) |> filter(run_model == "onestep") |> select(sitename, run_model, targets, data) |> unnest(data)
)

# profvis({
  out_calib_setup3_v2 <- calib_sofun_withTime(
    drivers   = data.frame(dummy=1),
    obs       = data.frame(dummy=1),
    settings  = in_calib_setup23_v2$settings,
    # arguments for the cost function
    par_fixed       = in_calib_setup23_v2$par_fixed,
    parallel        = in_calib_setup23_v2$parallel,
    ncores          = in_calib_setup23_v2$ncores,
    daily_drivers   = in_calib_setup23_v2$daily_drivers,
    onestep_drivers = in_calib_setup23_v2$onestep_drivers,
    daily_obs       = in_calib_setup23_v2$daily_obs,
    onestep_obs     = in_calib_setup23_v2$onestep_obs
  )
# })

in_calib_setup23_v3 <- in_calib_setup23_v2
in_calib_setup23_v3$settings$metric <- cost_likelihood_pmodel_bigD13C_vj_gpp_v3
# profvis({
  out_calib_setup3_v3 <- calib_sofun_withTime(
    drivers   = data.frame(dummy=1),
    obs       = data.frame(dummy=1),
    settings  = in_calib_setup23_v3$settings,
    # arguments for the cost function
    par_fixed       = in_calib_setup23_v3$par_fixed,
    parallel        = in_calib_setup23_v3$parallel,
    ncores          = in_calib_setup23_v3$ncores,
    daily_drivers   = in_calib_setup23_v3$daily_drivers,
    onestep_drivers = in_calib_setup23_v3$onestep_drivers,
    daily_obs       = in_calib_setup23_v3$daily_obs,
    onestep_obs     = in_calib_setup23_v3$onestep_obs
  )
# })





# TODO: what to test in terms of parallelization:
  # v2: provide drivers and observers already split for daily and onestep
  # v3: refrain from using dplyr for loglikelihood

get_runtime <- function(out_calib) {# function(settings_calib){
  total_time_secs <- sum(unlist(lapply(
    out_calib$mod,
    function(curr_chain){curr_chain$settings$runtime[["elapsed"]]})))
  return(sprintf("Total runtime: %.0f secs", total_time_secs))
}
get_walltime <- function(out_calib){out_calib$walltime}

print(get_runtime(out_calib_setup3))    # DEzs(5,1,3,3): 44s
print(get_runtime(out_calib_setup3_v2)) # DEzs(5,1,3,3): 43s
print(get_runtime(out_calib_setup3_v3)) # DEzs(5,1,3,3): 72s

print(get_walltime(out_calib_setup3))    # DEzs(5,1,3,3): xxs
print(get_walltime(out_calib_setup3_v2)) # DEzs(5,1,3,3): xxs
print(get_walltime(out_calib_setup3_v3)) # DEzs(5,1,3,3): xxs


