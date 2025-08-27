get_calibration_settings_str <- function(out_calib) {
  # defines a filename-compatible description of calibration settings
  # from 'out_calib', i.e. an output object of calib_sofun()
  stopifnot(is(out_calib$mod, "mcmcSamplerList"))

  # explore what's in a mcmcSamplerList:
  # summary(out_calib$mod)
  # plot(out_calib$mod)
  individual_chains <- out_calib$mod
  nrChains <- length(individual_chains) # number of chains

  # plot(individual_chains[[1]]) # chain 1
  # plot(individual_chains[[2]]) # chain 2
  # plot(individual_chains[[3]]) # chain 3
  # class(individual_chains[[1]]$setup); individual_chains[[1]]$setup # Bayesian Setup
  # individual_chains[[1]]$chain
  # individual_chains[[1]]$X
  # individual_chains[[1]]$Z

  nrInternalChains <- lapply(
    individual_chains,
    function(curr_chain){curr_chain$settings$startValue})  |>
    # function(curr_chain){length(curr_chain$chain)})  |>
    unlist() |>
    unique() |>
    paste0(collapse = "-")

  nrIterations <- lapply(
    individual_chains,
    function(curr_chain){curr_chain$settings$iterations})|>
    unlist() |>
    unique() |>
    paste0(collapse = "-")

  nrBurnin <- lapply(
    individual_chains,
    function(curr_chain){curr_chain$settings$burnin})    |>
    unlist() |>
    unique() |>
    paste0(collapse = "-")

  sampler_name <- lapply(
    individual_chains,
    function(curr_chain){curr_chain$settings$sampler})   |>
    unlist() |>
    unique() |>
    paste0(collapse = "-")

  # create descriptive string of settings for filename
  return(
    sprintf(
      "%s_%s_%s-%siter_%sx%schains",
      out_calib$name,
      sampler_name,
      nrIterations,
      nrBurnin,
      nrChains,
      nrInternalChains
    )
  )
}



# Bayesian calibration output
getSetup <- function(x) {
  classes <- class(x)
  if (any(c('mcmcSampler', 'smcSampler') %in% classes)) x$setup
  else if (any(c('mcmcSamplerList', 'smcSamplerList') %in% classes)) x[[1]]$setup
  else stop('Can not get setup from x')
}
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color

  ## Get RGB values for named color
  rgb.val <- col2rgb(color)

  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)

  ## Save the color
  invisible(t.col)
}
plot_prior_posterior_density <- function(x, burnin_to_skip){
  require(BayesianTools)
  require(dplyr)
  require(tidyr)
  require(ggplot2)

  # Get matrices of prior and posterior samples
  posteriorMat <- getSample(x, parametersOnly = TRUE, start = burnin_to_skip)
  priorMat <-  getSetup(x)$prior$sampler(10000) # nPriorDraws = 10000

  # Parameter names
  parNames <- colnames(posteriorMat)
  # rename columns priorMat
  colnames(priorMat) <- parNames

  # Create data frame for plotting
  df_plot <- rbind(
    data.frame(posteriorMat, distrib = "posterior"),
    data.frame(priorMat, distrib = "prior")
  )
  df_plot$distrib <- as.factor(df_plot$distrib)

  # Plot with facet wrap
  gg <- df_plot |> tibble() |>
    pivot_longer(-c(distrib), names_to = "variable") |>
    mutate(variable = forcats::fct_inorder(variable)) |> # order by appearance
    ggplot(
      aes(x = value, fill = distrib)
    ) +
    geom_density() +
    theme_classic() +
    facet_wrap( ~ variable , nrow = 2, scales = "free") +
    theme(
      legend.position = "bottom",
      axis.title.x = element_text("")
    ) +
    scale_fill_manual(NULL, values = c("#29a274ff", t_col("#777055ff"))) # GECO colors

  return(gg)
}

plot_mcmc_trace <- function(x, nr_internal_chains, burnin_to_skip, dont_thin=FALSE){
  # browser()
  # x <- getSample(x, coda = T, thin = "auto") # TODO: check if we need to scale x-axis
  curr_iter <- x[[1]]$settings$iterations
  if(dont_thin || curr_iter < 10000){
    curr_thin <- 1
  } else {
    curr_thin <- floor(curr_iter / 10000)
  }

  xsample <- getSample(x, coda = T, thin = curr_thin, start = burnin_to_skip)

  # nr_internal_chains will have same color
  dat_to_plot <- lapply(xsample, function(single_chain){
      as_tibble(single_chain) |> mutate(iteration = burnin_to_skip + curr_thin*(1:n()))
    }) |>
    bind_rows(.id = "chain_id") |>
    pivot_longer(-c(iteration, chain_id), names_to = "variable") |>
    # mark inner and outer chains (assumes DEzs):
    mutate(outerChain = as.factor(ceiling(as.numeric(chain_id)/3)),
           innerChain = (as.numeric(chain_id)+2)%%3 + 1,
           innerChain_str = letters[innerChain],
           chain_id_str = paste0(outerChain, letters[innerChain])) |>
    # fix order: in order of appearance
    mutate(variable = forcats::as_factor(variable))
  # dat_to_plot |> select(chain_id, innerChain, outerChain, chain_id_str) |> distinct()

  pl <- ggplot(dat_to_plot,
         aes(x=iteration, y=value, color = outerChain, linetype = innerChain_str)) + geom_line() +
    # geom_rug(sides = "r") +
    theme_classic() +
    facet_wrap(~variable,  nrow = 2, scales = "free_y") +
    theme(
      legend.position = "bottom"
    ) +
    labs(y="", color = "chain", linetype = "internal\nchains")

  # add Gelman Diagnostics
  get_gelman_diag <- function(mcmc, burnin_to_skip){
    gelman_df <- BayesianTools::gelmanDiagnostics(mcmc, start = burnin_to_skip)
    psrf_values <- gelman_df$psrf[,"Point est."]
    psrf_strings <- paste0(substr(names(psrf_values),1,3), "..=", sprintf("%.2f", psrf_values))
    psrf_string <- paste0(psrf_strings, collapse = ",")
    sprintf("GelmanDiagnostics: mpsrf=%.1f\npsrf:%s",
            gelman_df$mpsrf,
            psrf_string)
  }
  pl <- pl + ggtitle(NULL, subtitle = get_gelman_diag(x, burnin_to_skip))

  return(pl)
}

get_runtime <- function(out_calib) {# function(settings_calib){
  total_time_secs <- sum(unlist(lapply(
    out_calib$mod,
    function(curr_chain){curr_chain$settings$runtime[["elapsed"]]})))
  return(sprintf("Total runtime: %.0f secs", total_time_secs))
}
get_runtime_numeric <- function(out_calib) {# function(settings_calib){
  total_time_secs <- sum(unlist(lapply(
    out_calib$mod,
    function(curr_chain){curr_chain$settings$runtime[["elapsed"]]})))
  return(structure(total_time_secs, class = "difftime", units = "secs"))
}
get_walltime <- function(out_calib){out_calib$walltime}






#
# my_own_coda_plotmcmclist <- function (x, trace = TRUE, density = TRUE, smooth = TRUE, bwf,
#                                  auto.layout = TRUE, ask = par("ask"), ...) {
#   ## RGA fixed to use default ask value.
#   oldpar <- NULL
#   on.exit(par(oldpar))
#   if (auto.layout) {
#     mfrow <- coda:::set.mfrow(Nchains = nchain(x), Nparms = coda::nvar(x),
#                               nplots = trace + density)
#     oldpar <- par(mfrow = mfrow)
#   }
#   # browser()
#   for (i in 1:coda::nvar(x)) {
#     if (trace)
#       ## RGA fixed to propagate ... argument.
#       # browser()
#       # length(x) # internal x external chains
#     lapply(x, function(single_chain){as_tibble(single_chain) |> mutate(iteration = 1:n())}) |>
#       bind_rows(.id = "chain_id") |>
#       pivot_longer(-c(iteration, chain_id), names_to = "variable") |>
#       # fix order: in order of appearance
#       mutate(variable = forcats::as_factor(variable)) |>
#       ggplot(aes(x=iteration, y=value, color = chain_id)) + geom_line() +
#       facet_wrap(~variable, scales = "free_y") + theme_classic()
#
#       # coda::traceplot(x[, i, drop = FALSE], smooth = smooth, ...)
#       my_own_codatraceplot(x[, i, drop = FALSE], smooth = smooth, ...)
#     if (density) {
#       if (missing(bwf))
#         ## RGA fixed to propagate ... argument.
#         coda::densplot(x[, i, drop = FALSE], ...)
#       else densplot(x[, i, drop = FALSE], bwf = bwf, ...)
#     }
#     if (i==1)
#       oldpar <- c(oldpar, par(ask = ask))
#   }
# }
# # my_own_codadenseplot <- function (x, smooth = FALSE, col = 1:6, type = "l", xlab = "Iterations", ylab = "", ...){
# #   x <- mcmc.list(x)
# #   args <- list(...)
# #   for (j in 1:nvar(x)) {
# #     xp <- as.vector(time(x))
# #     yp <- if (nvar(x) > 1)
# #       x[, j, drop = TRUE]
# #     else x
# #     yp <- do.call("cbind", yp)
# #     matplot(xp, yp, xlab = xlab, ylab = ylab, type = type,
# #             col = col, ...)
# #     if (!is.null(varnames(x)) && is.null(list(...)$main))
# #       title(paste("Trace of", varnames(x)[j]))
# #     if (smooth) {
# #       scol <- rep(col, length = nchain(x))
# #       for (k in 1:nchain(x)) lines(lowess(xp, yp[, k]),
# #                                    col = scol[k])
# #     }
# #   }
# # }
#
# my_own_codatraceplot <- function (x, smooth = FALSE, col = 1:6, type = "l", xlab = "Iterations", ylab = "", ...) {
#   # browser()
#   x <- coda:::mcmc.list(x)
#   args <- list(...)
#   for (j in 1:coda::nvar(x)) {
#     xp <- as.vector(time(x))
#     # yp <- if (coda::nvar(x) > 1)
#     #   x[, j, drop = TRUE]
#     # else x
#     # yp <- do.call("cbind", yp)
#     yp <- do.call("cbind", x)
#     matplot(xp, yp, xlab = xlab, ylab = ylab, type = type,
#             col = col, ...)
#     if (!is.null(coda::varnames(x)) && is.null(list(...)$main))
#       title(paste("Trace of", coda::varnames(x)[j]))
#     if (smooth) {
#       scol <- rep(col, length = nchain(x))
#       for (k in 1:nchain(x)) lines(lowess(xp, yp[, k]),
#                                    col = scol[k])
#     }
#   }
# }

