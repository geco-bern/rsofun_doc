library(readr)
library(dplyr)
library(tidyr)
library(BayesianTools)
library(ggplot2)
library(patchwork)

source(here::here("R/calibration_helpers.R"))
source(here::here("analysis/00_define_scenarios.R"))

# timings <- readr::read_rds(here::here("timings_FB_2025-08-20_21h25.rds"))
# timing_files <- list.files(here::here("data","timings"), pattern = "timings_scen.*_2025-08-.*.rds", full.names = T)
timing_files1 <- list.files(file.path("/data_2/scratch/fbernhard/rsofun_doc_outputs/data","timings"), pattern = "timings_scen.*_2025-08-.*.rds", full.names = T)
timings1 <- lapply(timing_files1, readr::read_rds) |>
  bind_rows() |>
  mutate(scenario = as.factor(scenario),
         cores    = as.factor(cores))

timing_files2 <- list.files(file.path("/home/fabian/GitHub/geco-bern/rsofun_doc/data","timings"), pattern = "timings_scen.*_2025-08-.*.rds", full.names = T)
timings2 <- lapply(timing_files2, readr::read_rds) |>
  bind_rows() |>
  mutate(scenario = as.factor(scenario),
         cores    = as.factor(cores))

timings <- bind_rows(timings1, timings2)

ggplot(
    timings,
    aes(#x=(iterations-burnin)*n_chains*n_chains_inner,
      x=(iterations-burnin),
      y=as.numeric(walltime,"secs")/60,
      color = scenario,
      linetype = cores)) +
  geom_point() + geom_line() +
  geom_text(aes(label = sprintf("(%d,%d)",iterations,burnin)), vjust = 0, show.legend = F) +
  scale_x_log10(minor_breaks=scales::minor_breaks_n(10)) +
  scale_y_log10(minor_breaks=scales::minor_breaks_n(10)) +
  labs(y="walltime (minutes)") + theme_minimal()

longest_chains <- timings |> arrange(-iterations) |> group_by(scenario,cores) |> slice(1)

out_calib_s0 <- longest_chains |> filter(scenario == 0, cores == 3) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
out_calib_s1 <- longest_chains |> filter(scenario == 1, cores == 3) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
out_calib_s2 <- longest_chains |> filter(scenario == 2, cores == 3) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
# out_calib_s2b <- longest_chains |> filter(scenario == 2, cores == 10) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
out_calib_s3 <- longest_chains |> filter(scenario == 3, cores == 3) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
out_calib_s3b <- longest_chains |> filter(scenario == 3, cores == 3) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
out_calib_s3d <- longest_chains |> filter(scenario == 3, cores == 8) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
out_calib_s4d <- longest_chains |> filter(scenario == 4, cores == 8) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()

# reload non-parallel versions, to see if there is an issue with parallelization (or if it is with the likelihood)
out_calib_s0_serial <- longest_chains |> filter(scenario == 0, cores == 1) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
out_calib_s1_serial <- longest_chains |> filter(scenario == 1, cores == 1) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
out_calib_s2_serial <- longest_chains |> filter(scenario == 2, cores == 1) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
# out_calib_s3_serial <- longest_chains |> filter(scenario == 3, cores == 1) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds() # TODO

########## MCMC PLOTS: ########### -

# Figure A: prior, posterior density plot ----
## for each scenario x params
# plot_prior_posterior_density(out_calib_s0$mod) + ggtitle("Scenario 0")
# plot_prior_posterior_density(out_calib_s1$mod) + ggtitle("Scenario 1") + ggtitle(out_calib_s1$fpath)
# plot_prior_posterior_density(out_calib_s2$mod) + ggtitle("Scenario 2") + ggtitle(out_calib_s2$fpath)
pl_posterior_s3 <- plot_prior_posterior_density(out_calib_s3$mod) + ggtitle("Scenario 3") + ggtitle(out_calib_s3$fpath)
pl_posterior_s3b <- plot_prior_posterior_density(out_calib_s3b$mod) + ggtitle("Scenario 3") + ggtitle(out_calib_s3$fpath)

pl_posterior_s0 <- plot_prior_posterior_density(out_calib_s0_serial$mod) + ggtitle("Scenario 0 serial") + ggtitle(out_calib_s0_serial$fpath)
pl_posterior_s1 <- plot_prior_posterior_density(out_calib_s1_serial$mod) + ggtitle("Scenario 1 serial") + ggtitle(out_calib_s1_serial$fpath)
pl_posterior_s2 <- plot_prior_posterior_density(out_calib_s2_serial$mod) + ggtitle("Scenario 2 serial") + ggtitle(out_calib_s2_serial$fpath)
# pl_posterior_s3 <- plot_prior_posterior_density(out_calib_s3_serial$mod) + ggtitle("Scenario 3 serial") + ggtitle(out_calib_s3_serial$fpath)
# plot_prior_posterior_density(out_calib_s3_serial$mod) + ggtitle("Scenario 3 serial")

ggsave(here::here("fig/fig_E_MCMCconvergence_posterior_s1.png"), pl_posterior_s1, width=7.2, height=3.6, units="in", scale = 1.6)
# ggsave(here::here("fig/fig_E_MCMCconvergence_posterior_s1.pdf"), pl_posterior_s1, width=7.2, height=3.6, units="in", scale = 1.6)
# write_rds(here::here("fig/fig_E_MCMCconvergence_posterior_s1.rds"), x=pl_posterior_s1)

ggsave(here::here("fig/fig_E_MCMCconvergence_posterior_s2.png"), pl_posterior_s2, width=7.2, height=3.6, units="in", scale = 1.6)
# ggsave(here::here("fig/fig_E_MCMCconvergence_posterior_s2.pdf"), pl_posterior_s2, width=7.2, height=3.6, units="in", scale = 1.6)
# write_rds(here::here("fig/fig_E_MCMCconvergence_posterior_s2.rds"), x=pl_posterior_s2)

ggsave(here::here("fig/fig_E_MCMCconvergence_posterior_s3.png"), pl_posterior_s3, width=7.2, height=3.6, units="in", scale = 1.6)
# ggsave(here::here("fig/fig_E_MCMCconvergence_posterior_s3.pdf"), pl_posterior_s3, width=7.2, height=3.6, units="in", scale = 1.6)
# write_rds(here::here("fig/fig_E_MCMCconvergence_posterior_s3.rds"), x=pl_posterior_s3)



# Figure E: MCMC convergence diagnostics ----
## TBD: correlation plots, Gelman-Rubin (r.1.1)

# coda::gelman.diag(chain)
BayesianTools::gelmanDiagnostics(out_calib_s3$mod) # 535 not converged
BayesianTools::gelmanDiagnostics(out_calib_s2$mod) # 151 not converged
BayesianTools::gelmanDiagnostics(out_calib_s1$mod) # 1.02 looks converged
BayesianTools::gelmanDiagnostics(out_calib_s4d$mod) # 11.1 not yet converged


# BayesianTools::gelmanDiagnostics(out_calib_s3_serial$mod) # 87.5 not converged
# BayesianTools::gelmanDiagnostics(out_calib_s2_serial$mod) # 70.8 not converged
# BayesianTools::gelmanDiagnostics(out_calib_s1_serial$mod) # 1.05 looks converged
# BayesianTools::gelmanDiagnostics(out_calib_s0_serial$mod) # 1.04 looks converged

# chains:
burnin_to_skip = 4000
# what BayesianTools::tracePlot(out_calib_s0_serial$mod) does is:
# BayesianTools::tracePlot(out_calib_s0_serial$mod, start = burnin_to_skip)
out_calib_s0_serial$mod |>
  getSample(coda = T, thin = "auto", start = burnin_to_skip) |>
  coda:::plot.mcmc.list(density = FALSE)
out_calib_s1_serial$mod |>
  getSample(coda = T, thin = "auto", start = burnin_to_skip) |>
  coda:::plot.mcmc.list(density = FALSE)
out_calib_s2_serial$mod |>
  getSample(coda = T, thin = "auto", start = burnin_to_skip) |>
  coda:::plot.mcmc.list(density = FALSE)
# out_calib_s3_serial$mod |>
#   getSample(coda = T, thin = "auto", start = burnin_to_skip) |>
#   coda:::plot.mcmc.list(density = FALSE)

# out_calib_s1$mod |>
#   getSample(coda = T, thin = "auto", start = burnin_to_skip) |>
#   coda:::plot.mcmc.list(density = FALSE)
# out_calib_s1$mod |>
#   getSample(coda = T, thin = "auto") |>
#   coda:::plot.mcmc.list(density = FALSE)
# out_calib_s4d$mod |>
#   getSample(coda = T, thin = "auto", start = burnin_to_skip) |>
#   coda:::plot.mcmc.list(density = FALSE)


# debug(plot_mcmc_trace)
pl_trace_s0_with_burnin <- plot_mcmc_trace(out_calib_s0_serial$mod, nr_internal_chains = 3, burnin_to_skip = 0) + ggtitle(out_calib_s0_serial$fpath)
pl_trace_s1_with_burnin <- plot_mcmc_trace(out_calib_s1$mod, nr_internal_chains = 3, burnin_to_skip = 0) + ggtitle(out_calib_s1$fpath)
pl_trace_s2_with_burnin <- plot_mcmc_trace(out_calib_s2$mod, nr_internal_chains = 3, burnin_to_skip = 0) + ggtitle(out_calib_s2$fpath)
pl_trace_s3_with_burnin <- plot_mcmc_trace(out_calib_s3$mod, nr_internal_chains = 3, burnin_to_skip = 0) + ggtitle(out_calib_s3$fpath) # TODO
pl_trace_s4d_with_burnin <- plot_mcmc_trace(out_calib_s4d$mod, nr_internal_chains = 3, burnin_to_skip = 0) + ggtitle(out_calib_s3$fpath) # TODO

pl_trace_s0 <- plot_mcmc_trace(out_calib_s0_serial$mod, nr_internal_chains = 3, burnin_to_skip = burnin_to_skip) + ggtitle(out_calib_s0_serial$fpath)
pl_trace_s1 <- plot_mcmc_trace(out_calib_s1$mod, nr_internal_chains = 3, burnin_to_skip = burnin_to_skip) + ggtitle(out_calib_s1$fpath)
pl_trace_s2 <- plot_mcmc_trace(out_calib_s2$mod, nr_internal_chains = 3, burnin_to_skip = burnin_to_skip) + ggtitle(out_calib_s2$fpath)
pl_trace_s3 <- plot_mcmc_trace(out_calib_s3$mod, nr_internal_chains = 3, burnin_to_skip = burnin_to_skip) + ggtitle(out_calib_s3$fpath) # TODO
pl_trace_s4d <- plot_mcmc_trace(out_calib_s4d$mod, nr_internal_chains = 3, burnin_to_skip = burnin_to_skip) + ggtitle(out_calib_s3$fpath) # TODO

ggsave(here::here("fig/fig_E_MCMCconvergence_trace_s1.png"), pl_trace_s1, width=7.2, height=3.6, units="in", scale = 1.6)
# ggsave(here::here("fig/fig_E_MCMCconvergence_trace_s1.pdf"), pl_trace_s1, width=7.2, height=3.6, units="in", scale = 1.6)
# write_rds(here::here("fig/fig_E_MCMCconvergence_trace_s1.rds"), x=pl_trace_s1)

ggsave(here::here("fig/fig_E_MCMCconvergence_trace_s2.png"), pl_trace_s2, width=7.2, height=3.6, units="in", scale = 1.6)
# ggsave(here::here("fig/fig_E_MCMCconvergence_trace_s2.pdf"), pl_trace_s2, width=7.2, height=3.6, units="in", scale = 1.6)
# write_rds(here::here("fig/fig_E_MCMCconvergence_trace_s2.rds"), x=pl_trace_s2)

ggsave(here::here("fig/fig_E_MCMCconvergence_trace_s3.png"), pl_trace_s3, width=7.2, height=3.6, units="in", scale = 1.6)
# ggsave(here::here("fig/fig_E_MCMCconvergence_trace_s3.pdf"), pl_trace_s3, width=7.2, height=3.6, units="in", scale = 1.6)
# write_rds(here::here("fig/fig_E_MCMCconvergence_trace_s3.rds"), x=pl_trace_s3)



# # check burnin
# check1 <- timings1|>filter(burnin==0,iterations==24) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
# check2 <- timings1|>filter(burnin==4,iterations==24) |> magrittr::extract2("resultfile") |> here::here() |> readr::read_rds()
# plot_mcmc_trace(check1$mod, nr_internal_chains = 10, burnin_to_skip = 0)
# plot_mcmc_trace(check2$mod, nr_internal_chains = 10, burnin_to_skip = 0)

curr_thin <- 1
x <- getSample(check2$mod, coda = T, thin = curr_thin)




# Parameter correlation analysis
# correlationPlot(out_calib_s0_serial$mod, thin = 1) # the scatter plots with burnin do not make much sense
correlationPlot(out_calib_s0_serial$mod, thin = 1, start = burnin_to_skip)

correlationPlot(out_calib_s1$mod, thin = 1, start = burnin_to_skip)
correlationPlot(out_calib_s2$mod, thin = 1, start = burnin_to_skip)
correlationPlot(out_calib_s3$mod, thin = 1, start = burnin_to_skip)
correlationPlot(out_calib_s4$mod, thin = 1, start = burnin_to_skip)


# Check which parameters are most correlated
samples_s3 <- getSample(out_calib_s3$mod, thin = 1, start = burnin_to_skip)
cor_matrix <- cor(samples_s3)
print("Highly correlated parameters (|r| > 0.7):")
high_cor <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
for(i in 1:nrow(high_cor)) {
  row_idx <- high_cor[i,1]
  col_idx <- high_cor[i,2]
  if(row_idx < col_idx) {  # avoid duplicates
    cat(sprintf("%s - %s: %.3f\n",
                rownames(cor_matrix)[row_idx],
                colnames(cor_matrix)[col_idx],
                cor_matrix[row_idx, col_idx]))
  }
}


              # `BayesianTools` makes it easy to produce the trace plot of the MCMC chains and the posterior density plot for the parameters. Trace plots show the time series of the sampled chains, which should reach a stationary state. One can also choose a burnin visually, to discard the early iterations and keep only the samples from the stationary distribution to which they converge. We set \code{burnin = 3000} above from previous runs, and those iterations are not shown by the following trace plot. The samples after the burnin period should be used for inference.
              # ```{r fig.height = 10, fig.width = 7}
              # plot(par_calib$mod)
              # ```
              #
              # <!-- Internal recommendation: When you run the MCMC simulations with the DEzs sampler, there are two parameters that control the number of chains used: nrChains (documented for runMCMC) and startValue (documented for DEzs). The first dictates the number of independent chains to be run by the algorithm, while the second determines the number of internal chains to be run from a starting population (i.e. a population of initial parameter seeds). As Florian points out in this issue (https://github.com/florianhartig/BayesianTools/issues/224#issuecomment-877416919) the chains from within a population tend to be more correlated than those from independent "chains", and therefore internal chains should not be regarded as independent chains. This supports why for r3PG, they use nrChains=3 and startValue=3 (used by default), leading to 3*3=9 chains being plotted. For the example above, it doesn't make a big difference because the convergence is quite fast. -->
              #
              # The posterior density plots may be lumpy. In this case it's advisable to run the MCMC algorithm for more iterations, in order to get a better estimate of the parameters' posterior distributions. A good posterior should look more gaussian (although it can be skewed). A multimodal density indicates that the MCMC is still exploring the parameter space and hasn't converged yet. The posteriors can be plotted against the priors using `BayesianTools::marginalPlot()`.
              #
              # When convergence has been reached, the oscillation of the time series should look like white noise. It's normal that consecutive MCMC samples are correlated because of the sampling algorithm's nature, but the presence of a more general trend indicates that convergence hasn't been reached.
              #
              # <!-- Furthermore, trace plots can be deceiving and partial autocorrelation plots can throw some light. If autocorrelation is present, this can mean that the sampling is stuck in local maxima and the posterior parameter space may not be explored fully. Sometimes, thinning is used to deal with this autocorrelation. -->
              # ```{r fig.height = 10, fig.width = 7, eval = FALSE, echo = FALSE}
              # # Define function for plotting chains separately
              # plot_acf_mcmc <- function(chains, par_names){
              #   # chains: from the BayesianTools output
              #   n_chains <- length(chains)
              #   n_internal_chains <- length(chains[[1]]$chain)
              #   par(mfrow = c(length(par_names), n_chains))
              #   for(par_name in par_names){
              #     for(i in 1:n_chains){
              #       stopifnot(n_internal_chains<=3); color = c("blue", "red", "darkgreen")
              #       spacing = 0.5/n_internal_chains
              #       for(j in 1:n_internal_chains){
              #         autocorr_internal_chain <- pacf(getSample(chains[[i]]$chain[[j]])[, par_name], plot = FALSE)
              #         if(j==1){
              #           plot(autocorr_internal_chain, col = color[j],
              #                main = sprintf("Series of %s , chain (%i)", par_name, i))
              #         } else {
              #           lines(autocorr_internal_chain$lag + spacing*(j-1),
              #                 autocorr_internal_chain$acf,
              #                 col = color[j], type = "h")
              #         }
              #       }
              #     }
              #   }
              # }
              # plot_acf_mcmc(
              #   par_calib$mod,
              #   c("kphio", "kphio_par_a", "kphio_par_b", "soilm_thetastar", "soilm_betao",  "err_gpp")
              #   )
              # ```
              #
              # Looking at the correlation between chains for different parameters is also helpful because parameter correlation may slow down convergence, or the chains may oscillate in the multivariate posterior space. In this calibration we expect parameter samples to be somewhat correlated, especially `kphio_par_a` and `kphio_par_b` because they specify the shape of the temperature dependence of the quantum yield efficiency, $\varphi_o(T)$. We can also see that `err_gpp` is correlated with `kphio` (to which the P-model is very sensitive), since the error represents how good the model fits the observed GPP.
              #
              # ```{r fig.width=5, fig.height=5}
              # correlationPlot(par_calib$mod, thin = 1)   # use all samples, no thinning
              # ```
              #
              # In addition to visualizations, it's helpful to compute some convergence diagnostics, like the Gelman-Brooks-Rubin (GBR) potential scale factors. This diagnostic compares the variance within chains to that across chains and should progressively get closer to 1. It is common in the literature (Gelman, A., Carlin, J.B., Stern, H.S., Rubin, D.B.: Bayesian Data
              # Analysis, 2nd edn. Chapman & Hall, London (2004)) to accept convergence with a GBR between 1.05 and 1.1.
              # ```{r}
              # gelmanDiagnostics(par_calib$mod)
              # ```
              #
              # Finally, the parameter MAP estimates can be derived from the chains (that converged) after removing the burnin period. They can be seen, next to other statistics, using the `summary` function from the `BayesianTools` library.
              #
              # ```{r}
              # summary(par_calib$mod)
              # ```
              #
              # More details on diagnosing MCMC convergence can be found in [this vignette from BayesianTools](https://florianhartig.github.io/BayesianTools/articles/BayesianTools.html#running-mcmc-and-smc-functions) and [this blogpost](https://theoreticalecology.wordpress.com/2011/12/09/mcmc-chain-analysis-and-convergence-diagnostics-with-coda-in-r/).
              #
              # ### Plotting P-model output after calibration
              #
              # After we have run and checked the calibration, let's see how the model performs.
              #
              # To compute the credible intervals for GPP prediction, we ran the
              # P-model for 600 samples from the posterior distribution of the calibrated
              # parameters. As a result, we obtain the posterior distribution of modeled GPP
              # at each time step and also the posterior distribution of predicted GPP, which
              # incorporates the Gaussian model error.
              #
              # ```{r echo = TRUE, eval = FALSE}
              # # Evaluation of the uncertainty coming from the model parameters' uncertainty
              #
              # # Sample parameter values from the posterior distribution
              # samples_par <- getSample(
              #   par_calib$mod,
              #   thin = 60
              #   ) |>
              #   as.data.frame() |>
              #   dplyr::mutate(mcmc_id = 1:n()) |>
              #   tidyr::nest(.by = mcmc_id, .key = "pars")
              #
              # run_pmodel <- function(par){
              #   # Function that runs the P-model for a sample of parameters
              #   # and also adds the new observation error
              #
              #   out <- runread_pmodel_f(
              #     drivers = p_model_drivers,
              #     par =  list(
              #       kphio              = par$kphio,
              #       kphio_par_a        = par$kphio_par_a,
              #       kphio_par_b        = par$kphio_par_b,
              #       soilm_thetastar    = par$soilm_thetastar,
              #       soilm_betao        = par$soilm_betao,
              #       beta_unitcostratio = 146.0,
              #       rd_to_vcmax        = 0.014,
              #       tau_acclim         = 20.0,
              #       kc_jmax            = 0.41
              #       )
              #   )
              #
              #   # return modelled GPP and prediction for a new GPP observation
              #   gpp <- out$data[[1]][, "gpp"]
              #   out <- data.frame(
              #     gpp = gpp,
              #     gpp_pred = rnorm(
              #       n = length(gpp),
              #       mean = gpp,
              #       sd = par$err_gpp
              #       ),
              #     date = out$data[[1]][, "date"])
              #   return(out)
              # }
              #
              # set.seed(2025)
              #
              # # Run the P-model for each set of parameters
              # pmodel_runs <- samples_par |>
              #   dplyr::mutate(sim = purrr::map(pars, ~run_pmodel(.x))) |>
              #   # format to obtain 90% credible intervals
              #   dplyr::select(mcmc_id, sim) |>
              #   tidyr::unnest(sim) |>
              #   dplyr::group_by(date) |>
              #   # compute quantiles for each day
              #   dplyr::summarise(
              #     gpp_q05 = quantile(gpp, 0.05, na.rm = TRUE),
              #     gpp_q50 = quantile(gpp, 0.5, na.rm = TRUE),          # get median
              #     gpp_q95 = quantile(gpp, 0.95, na.rm = TRUE),
              #     gpp_pred_q05 = quantile(gpp_pred, 0.05, na.rm = TRUE),
              #     gpp_pred_q95 = quantile(gpp_pred, 0.95, na.rm = TRUE)
              #   )
              #
              # # run model with maximum a posteriori parameter estimates
              # pmodel_run_map <- run_pmodel(
              #   MAP(par_calib$mod)$parametersMAP |>
              #     t() |>
              #     as_tibble()
              # )
              # ```
              #
              # ```{r simulate_pmodel_runs, include = FALSE}
              # # TODO: get rid of this and always fully run the vignettes
              # # fake output since calibration isn't run
              # # saveRDS(pmodel_runs, file = "files/pmodel_runs.rds")
              # pmodel_runs <- readRDS("files/pmodel_runs.rds")
              # ```
              #
              # Below we plot the first year of observed GPP (in black) against the predicted
              # GPP (in grey), computed as the median of the posterior distribution of modeled
              # GPP. This information is accompanied by the 90% credible interval for predicted
              # GPP (shaded in blue, very narrow) and the 90% predictive interval
              # (shaded in grey). We can see that the parameter uncertainty captured in the
              # credible interval is quite small, in comparison to the model uncertainty
              # captured by the predictive interval.
              #
              # ```{r fig.width=7, fig.height=5}
              # ## add transparency to color given as a name
              # add_alpha <- function( col, alpha ){
              #   col    <- col2rgb( col, alpha = TRUE )/255
              #   col[4] <- alpha
              #   col    <- rgb(col[1,],col[2,],col[3,],col[4,])
              #   return( col )
              # }
              #
              # # Plot the credible intervals computed above
              # # for the first year only
              # data_to_plot <- pmodel_runs |>
              #   # Plot only first year
              #   dplyr::slice(1:365) |>
              #   dplyr::left_join(
              #     # Merge GPP validation data (first year)
              #     p_model_validation$data[[1]][1:365, ] |>
              #       dplyr::rename(gpp_obs = gpp),
              #     by = "date")
              #
              # plot_gpp_error <- ggplot(data = data_to_plot) +
              #   geom_ribbon(
              #     aes(
              #       ymin = gpp_pred_q05,
              #       ymax = gpp_pred_q95,
              #       x = date,
              #       fill = "Model uncertainty"
              #     )) +
              #   geom_ribbon(
              #     aes(
              #       ymin = gpp_q05,
              #       ymax = gpp_q95,
              #       x = date,
              #       fill = "Parameter uncertainty"
              #     )) +
              #   # Include observations in the plot
              #   geom_point(
              #     aes(
              #       x = date,
              #      y = gpp_obs,
              #      color = "Observations"
              #     ),
              #   ) +
              #   geom_line(
              #     aes(
              #       x = date,
              #       y = gpp_q50,
              #       color = "Predictions"
              #     )
              #   ) +
              #   theme_classic() +
              #   theme(panel.grid.major.y = element_line(),
              #         legend.position = "bottom") +
              #   labs(
              #     x = 'Date',
              #     y = expression(paste("GPP (g C m"^-2, "s"^-1, ")"))
              #   ) +
              #   scale_color_manual(NULL,
              #                      breaks = c("Observations",
              #                                 "Predictions"),
              #                      values = c("black", "tomato")) +
              #   scale_fill_manual(NULL,
              #                     breaks = c("Model uncertainty",
              #                                "Parameter uncertainty"),
              #                     values = c(add_alpha("tomato", 0.5),
              #                                "#1b9e77", 0))
              # plot_gpp_error
              # ```
              #
              # <!-- Below we plot the first year of predicted GPP (in grey) against GPP observations (in black). -->
              # <!-- We ran the P-model using the MAP estimates for all calibrated parameters and  -->
              # <!-- computed a 90% predictive interval for GPP (grey shade), based on the normality assumption -->
              # <!-- with standard deviation `err_gpp`. -->
              #
              # ```{r fig.width=7, fig.height=5, echo = FALSE, eval = FALSE}
              # # Define function to run model for a set of sampled parameters
              # run_pmodel <- function(par){
              #   # Function that runs the P-model for a sample of parameters
              #   out <- runread_pmodel_f(
              #     drivers = p_model_drivers,
              #     par =  list(
              #       kphio              = par$kphio,
              #       kphio_par_a        = par$kphio_par_a,
              #       kphio_par_b        = par$kphio_par_b,
              #       soilm_thetastar    = par$soilm_thetastar,
              #       soilm_betao        = par$soilm_betao,
              #       beta_unitcostratio = 146.0,
              #       rd_to_vcmax        = 0.014,
              #       tau_acclim         = 20.0,
              #       kc_jmax            = 0.41
              #       )
              #   )
              #
              #   return(out)
              # }
              #
              # # Plot observed and predicted GPP, with a 95% confidence interval using err_gpp
              # # Run model with maximum a posteriori parameter estimates (not shown on plot).
              # pmodel_run_map <- run_pmodel(
              #   BayesianTools::MAP(par_calib$mod)$parametersMAP |>
              #     t() |>
              #     as_tibble()
              # ) |>
              #   dplyr::select(-site_info) |>
              #   tidyr::unnest(data)
              #
              # # Plot the credible intervals computed above
              # # for the first year only
              # data_to_plot <- pmodel_run_map |>
              #   # Plot only first year
              #   dplyr::slice(1:365) |>
              #   dplyr::left_join(
              #     # Merge GPP validation data (first year)
              #     p_model_validation$data[[1]][1:365, ] |>
              #       dplyr::rename(gpp_obs = gpp),
              #     by = "date")
              #
              # plot_gpp_error <- ggplot(data = data_to_plot) +
              #   # Include observations in the plot
              #   geom_point(
              #     aes(
              #       x = date,
              #      y = gpp_obs,
              #      color = "Observations"
              #     ),
              #   ) +
              #   geom_line(
              #     aes(
              #       x = date,
              #       y = gpp,
              #       color = "Predictions based on MAP"
              #     )
              #   ) +
              #   theme_classic() +
              #   theme(panel.grid.major.y = element_line(),
              #         legend.position = "bottom") +
              #   labs(
              #     x = 'Date',
              #     y = expression(paste("GPP (g C m"^-2, "s"^-1, ")"))
              #   ) +
              #   scale_color_manual(NULL,
              #                      breaks = c("Observations",
              #                                 "Predictions based on MAP"),
              #                      values = c("black", "tomato"))
              # plot_gpp_error
              # ```


# Figure F: TBD: comparison of calibration vs GenSA?? ----
## or just using prior estimates from Stocker 2020? (r.1.14)

########## PREDICTION PLOTS: ########### -

# Figure B: error distribution density plot ----
## for each scenario x target x test+train

# sample posteriors and run model for each sample parameter set
source(here::here("R/run_prediction_rsofun.R"))
df_predict_s3 <- run_prediction_rsofun(
  mcmc_posterior = out_calib_s3,
  prediction = "train",
  burnin_to_skip = 0,
  n_samples = 2,
  n_cores = 10)

# NOTE: no error term has (yet) been added
df_predict_s3
df_predict_s3
                # POSTPROCESS
                # Aggregate across mcmc_samples


# Figure B2: error distribution predObs scatter plot ----
## for each scenario x target x test

########## SENSITIVITY ANALYSIS: ########### -

# Figure D: sensitivity bar plot ----


########## GENERAL PLOTS: ########### -
# s3_output_to_analyze <- out_calib_s3_serial$mod
# s2_output_to_analyze <- out_calib_s2_serial$mod
# s1_output_to_analyze <- out_calib_s1_serial$mod

source(here::here("R/calibration_helpers.R"))
source(here::here("R/run_mcmc_rsofun.R"), echo = TRUE)

res_s3 <- setup_rsofun_calibration(scenario = 3)

# Figure C: map of sites ----
## for each targets x test+train
site_info <- bind_rows(
  res_s3$drivobs |> mutate(set = "train"),
  res_s3$drivobs_test |> mutate(set = "test")
) |> unnest(site_info) |>
  unnest_wider(targets)


pl1_train <- rgeco:::plot_map_simpl() +
  geom_point(size=0.1,shape = 20, color = "red",
             data    = site_info |> filter(vj) |> filter(set == "train"),
             mapping = aes(lon, lat)) + ggtitle("Vcmax/Jmax sites") + labs(caption = sprintf("Training set (n=%d)", site_info |> filter(vj) |> filter(set == "train") |> nrow()))
pl2_train <- rgeco:::plot_map_simpl() +
  geom_point(size=0.1,shape = 20, color = "red",
             data    = site_info |> filter(bigD13C) |> filter(set == "train"),
             mapping = aes(lon, lat)) + ggtitle("Δ13C sites") + labs(caption = sprintf("Training set (n=%d)", site_info |> filter(bigD13C) |> filter(set == "train") |> nrow()))
pl3_train <- rgeco:::plot_map_simpl() +
  geom_point(size=0.1,shape = 20, color = "red",
             data    = site_info |> filter(gpp) |> filter(set == "train"),
             mapping = aes(lon, lat)) + ggtitle("GPP flux sites") + labs(caption = sprintf("Training set (n=%d)", site_info |> filter(gpp) |> filter(set == "train") |> nrow()))

pl1_test <- rgeco:::plot_map_simpl() +
  geom_point(size=0.1,shape = 20, color = "red",
             data    = site_info |> filter(vj) |> filter(set == "test"),
             mapping = aes(lon, lat)) + ggtitle(NULL) + labs(caption = #Vcmax/Jmax sites",
                                                sprintf("Test set (n=%d)", site_info |> filter(vj) |> filter(set == "test") |> nrow()))
pl2_test <- rgeco:::plot_map_simpl() +
  geom_point(size=0.1,shape = 20, color = "red",
             data    = site_info |> filter(bigD13C) |> filter(set == "test"),
             mapping = aes(lon, lat)) + ggtitle(NULL) + labs(caption = #Δ13C sites",
                                                sprintf("Test set (n=%d)", site_info |> filter(bigD13C) |> filter(set == "test") |> nrow()))
pl3_test <- rgeco:::plot_map_simpl() +
  geom_point(size=0.1,shape = 20, color = "red",
             data    = site_info |> filter(gpp) |> filter(set == "test"),
             mapping = aes(lon, lat)) + ggtitle(NULL) + labs(caption = #"GPP flux sites",
                                                sprintf("Test set (n=%d)", site_info |> filter(gpp) |> filter(set == "test") |> nrow()))

library(cowplot)
remove_labels <- theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
pl_targets <- cowplot::plot_grid(
  pl1_train + remove_labels, pl2_train + remove_labels, pl3_train + remove_labels,
  pl1_test + remove_labels, pl2_test + remove_labels, pl3_test + remove_labels,
  ncol = 3)

ggsave(
  here::here("fig/fig_D_append_climate_MapTargetTrainingSites.png"),
  pl_targets, width=7.2, height=1.8 * 2, units="in")

# Table a: site table [lon, lat, elv, climate, vegtype, train-or-test, targets, Nobs] ----
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
  rowwise() |> mutate(Nobs = nrow(data)) |> mutate(Nobs2 = count_obs(target, data)) |>
  select(sitename, lon, lat, elv, climate, igbp_vegtype, set, target, Nobs, Nobs2)
table_a



# Table b: prior ranges of estimated params ----
res_s3 <- setup_rsofun_calibration(scenario = 3)
res_s2 <- setup_rsofun_calibration(scenario = 2)
res_s1 <- setup_rsofun_calibration(scenario = 1)


par_prior_s1 <- as.data.frame(do.call("rbind", res_s1$par)) |> dplyr::add_rownames("parameter")|> mutate(scenario = "Scenario 1") |> mutate(value = sprintf("[%.3f to %.3f]", lower, upper))
par_prior_s2 <- as.data.frame(do.call("rbind", res_s2$par)) |> dplyr::add_rownames("parameter")|> mutate(scenario = "Scenario 2") |> mutate(value = sprintf("[%.3f to %.3f]", lower, upper))
par_prior_s3 <- as.data.frame(do.call("rbind", res_s3$par)) |> dplyr::add_rownames("parameter")|> mutate(scenario = "Scenario 3") |> mutate(value = sprintf("[%.3f to %.3f]", lower, upper))

par_fix_s1 <- as.data.frame(res_s1$par_fixed) |> pivot_longer(everything(), names_to = "parameter") |> mutate(value = sprintf("[%.2f]*", value))|> mutate(scenario = "Scenario 1")
# par_fix_s2 <- as.data.frame(res_s2$par_fixed) |> pivot_longer(everything(), names_to = "parameter") |> mutate(value = sprintf("[%.2f]*", value)) # NOTE: none are fixed
# par_fix_s3 <- as.data.frame(res_s3$par_fixed) |> pivot_longer(everything(), names_to = "parameter") |> mutate(value = sprintf("[%.2f]*", value)) # NOTE: none are fixed

table_b_v1 <- bind_rows(
  par_prior_s1,
  par_fix_s1,
  par_prior_s2,
  par_prior_s3
) |>
  select(parameter, value, scenario) |> pivot_wider(names_from = scenario, values_from = value)


        # priors_s3 <- s3_output_to_analyze[[1]]$setup$prior # extract prior from any chain
        # priors_s3_tbl <- tibble(par = names(priors_s3$lower),
        #                         lower = priors_s3$lower,
        #                         upper = priors_s3$upper,
        #                         scenario = 3)
        # priors_s2 <- s2_output_to_analyze[[1]]$setup$prior # extract prior from any chain
        # priors_s2_tbl <- tibble(par = names(priors_s2$lower),
        #                         lower = priors_s2$lower,
        #                         upper = priors_s2$upper,
        #                         scenario = 2)
        # priors_s1 <- s1_output_to_analyze[[1]]$setup$prior # extract prior from any chain
        # priors_s1_tbl <- tibble(par = names(priors_s1$lower),
        #                         lower = priors_s1$lower,
        #                         upper = priors_s1$upper,
        #                         scenario = 1)
        #
        # table_b_v2 <- bind_rows(priors_s1_tbl,priors_s2_tbl,priors_s3_tbl) |>
        #   pivot_longer(c(lower, upper)) |> pivot_wider(names_from = c(scenario, name))


table_a |> View()
table_b_v1 |> View()
# TODO: make these into a LaTeX table



