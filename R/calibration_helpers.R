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
plot_prior_density <- function(priors, parNames, n=1000){
  priorMat <- priors$sampler(n)
  colnames(priorMat) <- parNames

  # Create data frame for plotting
  df_plot <- rbind(
    # data.frame(posteriorMat, par_estimation = "posterior"),
    data.frame(priorMat, par_estimation = "prior")
  )
  df_plot$par_estimation <- as.factor(df_plot$par_estimation)

  # Plot with facet wrap
  df_plot |> tidyr::tibble() |>
    tidyr::pivot_longer(-c(par_estimation), names_to = "variable") |>
    dplyr::mutate(variable = forcats::fct_inorder(variable)) |> # order by appearance
    ggplot(
      aes(x = value, fill = par_estimation)
    ) +
    geom_density() +
    theme_classic() +
    facet_wrap( ~ variable , nrow = 2, scales = "free") +
    theme(
      legend.position = "bottom",
      axis.title.x = element_text("")
    ) +
    scale_fill_manual(NULL, values = c("#29a274ff", "#777055ff")) # GECO colors
}
# plot_prior_correlations <- function(priors, parNames, n=1000){
#   priorMat <- priors$sampler(n)
#   colnames(priorMat) <- parNames
#
#   # Create data frame for plotting
#   df_plot <- rbind(
#     # data.frame(posteriorMat, par_estimation = "posterior"),
#     data.frame(priorMat, par_estimation = "prior")
#   )
#   df_plot$par_estimation <- as.factor(df_plot$par_estimation)
#
#   library(GGally)
#   library(ggplot2)
#
#   my_hex_fn <- function(data, mapping, ...) {
#     ggplot(data = data, mapping = mapping) +
#       geom_hex()
#   }
#
#   # make scatter plot matrix only for prior
#   dat_to_plot <- tidyr::tibble(df_plot) |> dplyr::filter(par_estimation == "prior") |> dplyr::select(-par_estimation)
#   p <- ggpairs(dat_to_plot,
#                lower = list(continuous = my_hex_fn)) +
#     theme_classic() #+
#   # scale_color_manual(NULL, values = c("#29a274ff", "#777055ff")) # GECO colors
#   return(p)
# }
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
    data.frame(posteriorMat, par_estimation = "posterior"),
    data.frame(priorMat, par_estimation = "prior")
  )
  df_plot$par_estimation <- as.factor(df_plot$par_estimation)

  # Plot with facet wrap
  gg <- df_plot |> tibble() |>
    pivot_longer(-c(par_estimation), names_to = "variable") |>
    mutate(variable = forcats::fct_inorder(variable)) |> # order by appearance
    ggplot(
      aes(x = value, fill = par_estimation)
    ) +
    geom_density() +
    theme_classic() +
    facet_wrap( ~ variable , nrow = 2, scales = "free") +
    theme(
      legend.position = "bottom",
      axis.title.x = element_text(""),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
    ) +
    scale_fill_manual(NULL, values = c("#29a274ff", t_col("#777055ff"))) # GECO colors

  return(gg)
}

plot_prior_posterior_density_compare <- function(
    named_list_scen,
    burnin_to_skip,
    ridges = FALSE,
    add_MAP = FALSE,
    correct_scenarios = c("4"=94, "3"=93, "2"=92, "1" = 91, "0" = 90)  # this is for retrieval of correct scenario definition, despite renaming
  ){
  require(BayesianTools)
  require(dplyr)
  require(tidyr)
  require(ggplot2)

  # Get matrices of prior and posterior samples
  priorMat_list <- lapply(
    named_list_scen[grepl("[Pp]rior", names(named_list_scen))] |> rev(), # rev is for to have later scenarios with more parameters determine order
    function(x){
      priorMat <- getSetup(x)$prior$sampler(10000) # nPriorDraws = 10000
      colnames(priorMat) <- x[[1]]$setup$names
      return(as_tibble(priorMat))
    }
  )
  posteriorMat_list <- lapply(
    named_list_scen[!grepl("[Pp]rior", names(named_list_scen))] |> rev(), # rev is for to have later scenarios with more parameters determine order
    function(x){
      as_tibble(getSample(x, parametersOnly = TRUE, start = burnin_to_skip))
  })
  MAP_list <- lapply(
    named_list_scen[!grepl("[Pp]rior", names(named_list_scen))] |> rev(), # rev is for to have later scenarios with more parameters determine order
    function(x){as_tibble(as.list(BayesianTools::MAP(x)$parametersMAP))}
  )
  # add fixed parameter values
  stopifnot(all(names(MAP_list) %in% names(correct_scenarios)))
  fixed_list <- lapply(names(MAP_list), function(scenario_name){
    curr_scen <- correct_scenarios[scenario_name] # this basically undoes manual renaming that was applied for plotting purposes
    as_tibble(setup_rsofun_calibration(curr_scen)$par_fixed)
  })
  names(fixed_list) <- names(MAP_list) # ensure names are there

  # Create data frame for plotting
  df_plot <- bind_rows(dplyr::bind_rows(priorMat_list,     .id = "par_estimation"),
                       dplyr::bind_rows(posteriorMat_list, .id = "par_estimation"),
                       dplyr::bind_rows(MAP_list,          .id = "par_estimation") |> mutate(par_estimation = paste0("MAP ", par_estimation)),
                       dplyr::bind_rows(fixed_list,        .id = "par_estimation") |> mutate(par_estimation = paste0("Fixed ", par_estimation)) |>
                         select(-any_of("rd_to_vcmax")) # HARDCODED do not plot rd_to_vcmax
                       ) |>
    pivot_longer(-c(par_estimation), names_to = "variable") |>
    mutate(par_estimation  = forcats::fct_inorder(par_estimation),  # order by appearance
           variable = forcats::fct_inorder(variable)) # order by appearance

  # Plot with facet wrap
  if(ridges == TRUE){
    df_plot2 <- df_plot |>
      mutate(Scenario = as.factor(as.integer(gsub("((Prior)|(MAP)|(Fixed)) ", "", par_estimation)))) |>
      mutate(Distribution = case_when(#grepl("Prior ", par_estimation) ~ "Prior",
                                      #grepl("MAP ",   par_estimation) ~ "MAP",
                                      grepl("Fixed ", par_estimation) ~ "Fixed",
                                      TRUE                            ~ "Posterior"))
    gg <- ggplot(df_plot2, aes(x=value, y=Scenario)) +
      theme_classic() +
      geom_density_ridges(
        data =  df_plot2 |> filter(!grepl("((MAP)|(Fixed)) ", par_estimation)),
        mapping = aes(fill = Distribution)) +
      {if (add_MAP) geom_segment(
        data = df_plot2 |> filter(grepl("MAP ", par_estimation)) |> filter(!is.na(value)),
        mapping = aes(yend = as.integer(Scenario) - 0.6, color = Distribution), # minus (-) because of scale reverse
        key_glyph = "vline", linetype = "2121")} + # "dashed"
      {if (add_MAP) geom_segment(
        data = df_plot2 |> filter(grepl("Fixed ", par_estimation)) |> filter(!is.na(value)),
        mapping = aes(yend = as.integer(Scenario) - 0.6, color = Distribution), # minus (-) because of scale reverse
        key_glyph = "vline", linetype = "solid")} +
      # layout:
      scale_y_discrete(limits = rev) +
      facet_wrap( ~ variable , nrow = 2, scales = "free_x") +
      # scale_y_discrete(limits=rev) + # to have scenario 1 on top and 4 at bottom
      theme(legend.position = "bottom") + labs(x=NULL) +
      scale_fill_manual(NULL, aesthetics = c("fill","colour"),
                        values = c("Posterior"="#29a274ff", "Prior" = t_col("#777055ff"),  # GECO colors
                                   "MAP"      = "black",    "Fixed" = "black"))
  } else {
    gg <- ggplot(filter(df_plot, !grepl("MAP ", par_estimation)),
                 aes(x = value, color = par_estimation)) +
      theme_classic() +
      # variant 1: density:
      # geom_density()
      # # variant 2: scaled density:
      geom_density(aes(y = after_stat(scaled))) + theme(axis.ticks.y = element_blank(),
                                                        axis.text.y = element_blank(),
                                                        axis.title.y = element_blank()) +
      # layout:
      facet_wrap( ~ variable , nrow = 2, scales = "free") +
      theme(legend.position = "bottom") +
      labs(x="Parameter value")
  }
  return(gg)
}

plot_mcmc_trace <- function(x, nr_internal_chains, burnin_to_skip, burnin_to_skip_gelman = burnin_to_skip, dont_thin=FALSE, end = NULL){
  curr_iter <- x[[1]]$settings$iterations
  if(dont_thin || curr_iter < 10000){
    curr_thin <- 1
  } else {
    curr_thin <- floor(curr_iter / 10000)
  }

  xsample <- getSample(x, coda = T, thin = curr_thin, start = burnin_to_skip, end = end)

  # nr_internal_chains will have same color
  dat_to_plot <- lapply(xsample, function(single_chain){
      as_tibble(single_chain) |> mutate(iteration = burnin_to_skip + curr_thin*(1:n()))
    }) |>
    bind_rows(.id = "chain_id") |>
    pivot_longer(-c(iteration, chain_id), names_to = "variable") |>
    # mark inner and outer chains (assumes DEzs): # TODO: setup for DREAMzs
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
  get_gelman_diag <- function(mcmc, burnin_to_skip, end){
    gelman_df <- BayesianTools::gelmanDiagnostics(mcmc, start = burnin_to_skip, end = end)
    psrf_values <- gelman_df$psrf[,"Point est."]
    psrf_strings <- paste0(substr(names(psrf_values),1,3), "..=", sprintf("%.2f", psrf_values))
    psrf_string <- paste0(psrf_strings, collapse = ",")
    sprintf("GelmanDiagnostics: mpsrf=%.1f\npsrf:%s",
            gelman_df$mpsrf,
            psrf_string)
  }
  subtitle <- tryCatch(get_gelman_diag(x, burnin_to_skip_gelman + 1, end = end), error = function(e) {e}) # unsure why min burnin of 1 is needed
  pl <- pl + ggtitle(NULL, subtitle = subtitle)

  pl <- pl + geom_vline(xintercept = burnin_to_skip_gelman, color="red", linetype="dashed")

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

