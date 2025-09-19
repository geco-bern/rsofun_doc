# # This is an example from the documentation: ?createPrior
# # see also https://github.com/florianhartig/BayesianTools/issues/180
# density = function(par){
#   d1 = dunif(par[1], -2,6, log =TRUE)
#   d2 = dnorm(par[2], mean= 2, sd = 3, log =TRUE)
#   return(d1 + d2)
# }
# sampler = function(n=1){
#   d1 = runif(n, -2,6)
#   d2 = rnorm(n, mean= 2, sd = 3)
#   return(cbind(d1,d2))
# }


# So we adapt this in the following:


# createOwnPriorSimple <- function(prior_definitions, best = NULL){
#   # check that we have simple case where all prior_definitions are uniform
#   stopifnot(all(unlist(lapply(prior_definitions, function(x) identical(names(x), c("lower","upper","init"))))))
#   stopifnot(length(prior_definitions) == 2)

#   density <- function(par) {sum(
#     dunif(x = par[1], min=prior_definitions[[1]]$lower, max=prior_definitions[[1]]$upper, log=T),
#     dunif(x = par[2], min=prior_definitions[[2]]$lower, max=prior_definitions[[2]]$upper, log=T)
#   )}
#   sampler <- function(n=1) {cbind(
#     runif(n = n, prior_definitions[[1]]$lower, prior_definitions[[1]]$upper),
#     runif(n = n, prior_definitions[[2]]$lower, prior_definitions[[2]]$upper)
#   )}
#   out <- createPrior(density = density, sampler = sampler)
# }


is_uniform_prior        <- function(el){identical(sort(names(el)), c("init", "lower", "upper"))}
is_normal_prior         <- function(el){identical(sort(names(el)), c("mean", "sd"))}
is_lognormal_prior      <- function(el){identical(sort(names(el)), c("meanlog","sdlog"))}
is_truncnormal_prior    <- function(el){identical(sort(names(el)), c("lower", "mean", "sd", "upper"))}
is_trunclognormal_prior <- function(el){identical(sort(names(el)), c("endpoint","meanlog","sdlog"))}
is_beta_prior           <- function(el){identical(sort(names(el)), c("shape1","shape2"))}

createMixedPrior <- function(prior_definitions, best = NULL){

  # parse prior_definitions
  list_unif  <- lapply(prior_definitions, is_uniform_prior)
  list_norm  <- lapply(prior_definitions, is_normal_prior)
  list_lnorm <- lapply(prior_definitions, is_lognormal_prior)
  list_tnorm <- lapply(prior_definitions, is_truncnormal_prior)
  list_tlnorm<- lapply(prior_definitions, is_trunclognormal_prior)
  list_beta  <- lapply(prior_definitions, is_beta_prior)

  stopifnot(length(prior_definitions) == # check that all prior types are uniquely identified
              sum(list_unif==TRUE) +
              sum(list_norm==TRUE) +
              sum(list_lnorm==TRUE) +
              sum(list_tnorm==TRUE) +
              sum(list_tlnorm==TRUE) +
              sum(list_beta==TRUE))

  if (all(unlist(list_unif))) {  # all uniformly distributed: recover previous behavior
    pars  <- as.data.frame(do.call("rbind", prior_definitions))
    out   <- BayesianTools::createUniformPrior(
      lower = unlist(pars$lower),
      upper = unlist(pars$upper),
      best  = unlist(pars$init)
    )
  } else { # mixed distributions in prior

    # prepare definition of sampler and density of prior
    prior_args <- lapply(prior_definitions, \(def){
      if(is_uniform_prior(def)){            return(list(rfct=runif,         dfct=dunif,          args=list(min    =def$lower,   max   =def$upper)))} #def$init is unused
      else if(is_normal_prior(def)){        return(list(rfct=rnorm,         dfct=dnorm,          args=list(mean   =def$mean,    sd    =def$sd)))}
      else if(is_lognormal_prior(def)){     return(list(rfct=rlnorm,        dfct=dlnorm,         args=list(meanlog=def$meanlog, sdlog =def$sdlog)))}
      else if(is_truncnormal_prior(def)){   return(list(rfct=msm::rtnorm,   dfct=msm::dtnorm,    args=list(mean   =def$mean,    sd    =def$sd,    lower    =def$lower, upper =def$upper)))}
      else if(is_trunclognormal_prior(def)){return(list(rfct=ReIns::rtlnorm, dfct=ReIns::dtlnorm,args=list(meanlog=def$meanlog, sdlog =def$sdlog, endpoint =def$endpoint)))}
      else if(is_beta_prior(def)){          return(list(rfct=rbeta,         dfct=dbeta,          args=list(shape1 =def$shape1,  shape2=def$shape2)))}
      else {stop("Unknown prior distribution,")}
    })

    # define and create prior
    density <- function(par) {
      stopifnot(length(prior_args) == length(par))
      sum(unlist(
        # dunif(par[1], min=prior_definitions[[1]]$lower, max=prior_definitions[[1]]$upper, log=T),
        # dunif(par[2], min=prior_definitions[[2]]$lower, max=prior_definitions[[2]]$upper, log=T)
        lapply(seq_along(prior_args), \(i){ do.call(prior_args[[i]]$dfct, c(log=TRUE, x=par[[i]], prior_args[[i]]$args)) }) # unname(par[i]) or par[[i]] needed due to error message: "Problem in the priorError in (function (x, min = 0, max = 1, log = FALSE) : unused argument (x.kphio = 0.0487171059425491)"
      ))}
    sampler <- function(n=1) {do.call(cbind,
                                      # runif(1, prior_definitions[[1]]$lower, prior_definitions[[1]]$upper),
                                      # runif(1, prior_definitions[[2]]$lower, prior_definitions[[2]]$upper)
                                      lapply(prior_args, \(def){ do.call(def$rfct, c(n = n, def$args)) })
    )}
    out <- createPrior(density = density, sampler = sampler, best = best)
  }

  return(out)
}
# TODO: we could now also overload BayesianTools:::print.prior
#       e.g. something using: bind_rows(lapply(settings$par, as_tibble), .id = "parnames")




# NOTE: here are examples to test this

# prior_definitions_uniform <- list(
#   par = list(
#     kphio =               list(lower = 0.02, upper = 0.15, init = 0.05),
#     kphio_par_a =         list(lower = -0.004, upper = -0.001, init = -0.0025),
#     kphio_par_b =         list(lower = 10, upper = 30, init = 20),
#     soilm_thetastar =     list(lower = 1, upper = 250, init = 40),
#     soilm_betao =         list(lower = 0, upper = 1, init = 0),
#     beta_unitcostratio =  list(lower = 14.6, upper = 438, init = 146),
#     rd_to_vcmax =         list(lower = 0.0014, upper = 0.042, init = 0.014),
#     tau_acclim =          list(lower = 2, upper = 60, init = 20),
#     kc_jmax =             list(lower = 0.041, upper = 1.23, init = 0.41),
#     err_gpp =             list(lower = 0.01, upper = 3, init = 0.8),
#     err_bigD13C =         list(lower = 0.01, upper = 3, init = 0.8),
#     err_vj =              list(lower = 0.01, upper = 3, init = 0.8)
#   ))
#
# prior_definitions_mixed <- list(
#   par = list(
#     kphio =              list(mean = 0.0479684950570567, sd = 9.75104729575593e-05),
#     kphio_par_a =        list(mean = -0.00179211384220008, sd = 2.98616456930556e-05),
#     kphio_par_b =        list(mean = 18.4293950588911, sd = 0.102867468875224),
#     soilm_thetastar =    list(mean = 27.0859346061886, sd = 0.762249191490997),
#     soilm_betao =        list(meanlog = -4.65845041863264, sdlog = 1.31209247435319),
#     beta_unitcostratio = list(lower = 14.6, upper = 438, init = 146),
#     rd_to_vcmax =        list(lower = 0.0014, upper = 0.042, init = 0.014),
#     tau_acclim =         list(lower = 2, upper = 60, init = 20),
#     kc_jmax =            list(lower = 0.041, upper = 1.23, init = 0.41),
#     err_gpp =            list(lower = 0.01, upper = 3, init = 0.8),
#     err_bigD13C =        list(lower = 0.01, upper = 3, init = 0.8),
#     err_vj =             list(lower = 0.01, upper = 3, init = 0.8)))
#
#
# priorSimpleInbuilt <- createUniformPrior(lower = c(0.5, 1.5), upper = c(1.5, 2.5))
# plot_prior_density(priorSimpleInbuilt, parNames = c("par1","par2"), n=10000)
# priorSimpleInbuilt$density(c(1,2))
#
# prior_definitions_parSimple <- list(
#   par1 = list(lower=0.5, upper=1.5, init=1.0),
#   par2 = list(lower=2.5, upper=3.5, init=3.0))
# prior_definitions_parMixed <- prior_definitions_mixed$par[c("err_vj","soilm_thetastar","soilm_betao")]
# prior_definitions_parMixed2 <- prior_definitions_mixed$par
#
# # check if it works
# # define:
# priorSimple <- createOwnPriorSimple(prior_definitions_parSimple)
# priorNew    <- createMixedPrior(prior_definitions_parSimple)
# priorMixed  <- createMixedPrior(prior_definitions_parMixed)
# priorMixed2 <- createMixedPrior(prior_definitions_parMixed2)
#
# # check sampling:
# plot_prior_density(priorSimple, parNames = names(prior_definitions_parSimple), n=10000)
# plot_prior_density(priorNew, parNames = names(prior_definitions_parSimple), n=10000)
# plot_prior_density(priorMixed, parNames = names(prior_definitions_parMixed), n=10000)
# # plot_prior_density(priorMixed2, parNames = names(prior_definitions_parMixed2), n=10000)
# plot_prior_correlations(priorMixed, parNames = names(prior_definitions_parMixed), n=10000)
#
# # check density:
# priorSimple$density(c(1.0,3.0))
# priorNew$density(c(1.0,3.0))
# priorMixed$density(c(1.0,27.0,0.01))
# priorMixed2$density(rep(0.1, 12))
# priorMixed2$density(c(0.0478, -0.0018, 18.5, 27, 0.1, 146, 0.014, 35, 0.5, 1, 1, 1))
# priorMixed2$density(c(0.0488, -0.0018, 18.5, 27, 0.1, 146, 0.014, 35, 0.5, 1, 1, 1))
# priorMixed2$density(c(0.0468, -0.0018, 18.5, 27, 0.1, 146, 0.014, 35, 0.5, 1, 1, 1))
#
# # check density:
# hist(priorSimple$density(priorSimple$sampler(10000)))
# hist(priorNew$density(priorNew$sampler(10000)))
# hist(priorMixed$density(priorMixed$sampler(10000)))
# hist(priorMixed2$density(priorMixed2$sampler(10000)))
#
# # see internals:
# # priorMixed2$originalDensity
# # env1 <- rlang::fn_env(priorMixed2$originalDensity)
# # env1$prior_args
# # priorMixed2$density
# # env2 <- rlang::fn_env(priorMixed2$density)
# # env2$checkPrior(x)






# When using the output of createMixedPrior() to `createBayesianSetup` the
# min-max values of the prior are not stored in the fields 'lower' and 'upper'.
# For sensitivity analysis a sensible range of the parameters is needed. Therefore,
# we write the function getPriorMinMaxRanges() to be used in combination with
# sensitivity analysis.
getPriorMinMaxRanges <- function(morrisSetup_prior, settings_par){
  # by default use BayesianTool defined ranges
  inflim_arg <- morrisSetup_prior$lower    # named vector: e.g. c(kphio = 0.02, kphio_par_a = -0.004, err_gpp = 0.1, ... )
  suplim_arg <- morrisSetup_prior$upper    # named vector: e.g. c(kphio = 0.15, kphio_par_a = -0.001, err_gpp = 3,   ... )

  # Fallback if this fails with more complex priors
  if(is.null(inflim_arg) || is.null(suplim_arg)){
    par_names <- names(settings_par)
    # use the currently defined distributions that are compatible with createMixedPrior():
    # - is_uniform_prior        when has names: c("init", "lower", "upper"))}
    # - is_normal_prior         when has names: c("mean", "sd"))}
    # - is_lognormal_prior      when has names: c("meanlog","sdlog"))}
    # - is_truncnormal_prior    when has names: c("lower", "mean", "sd", "upper"))}
    # - is_trunclognormal_prior when has names: c("endpoint","meanlog","sdlog"))}
    # - is_beta_prior           when has names: c("shape1","shape2"))}

    # make a list with lower and upper for each of these distributions:
    inflim_suplim_list <- c(
      settings_par |> purrr::keep(is_uniform_prior       ) |> lapply(function(x) list(inflim = x$lower, suplim = x$upper)),
      # settings_par |> purrr::keep(is_normal_prior        ) |> names(), # TODO: add when needed
      # settings_par |> purrr::keep(is_lognormal_prior     ) |> names(), # TODO: add when needed
      settings_par |> purrr::keep(is_truncnormal_prior   ) |> lapply(function(x) list(inflim = x$lower, suplim = x$upper))
      # settings_par |> purrr::keep(is_trunclognormal_prior) |> names(), # TODO: add when needed
      # settings_par |> purrr::keep(is_beta_prior          ) |> names() # TODO: add when needed
    )
    stopifnot(settings_par |> purrr::keep(is_normal_prior        ) |> length() == 0) # TODO: remove once above is defined
    stopifnot(settings_par |> purrr::keep(is_lognormal_prior     ) |> length() == 0) # TODO: remove once above is defined
    stopifnot(settings_par |> purrr::keep(is_trunclognormal_prior) |> length() == 0) # TODO: remove once above is defined
    stopifnot(settings_par |> purrr::keep(is_beta_prior          ) |> length() == 0) # TODO: remove once above is defined

    # derive two vectors: one for inflim and for suplim
    inflim_arg <- unlist(lapply(inflim_suplim_list, `[[`, "inflim"))
    suplim_arg <- unlist(lapply(inflim_suplim_list, `[[`, "suplim"))

    # ensure correct order
    inflim_arg <- inflim_arg[par_names]
    suplim_arg <- suplim_arg[par_names]
  }
  return(list(inflim = inflim_arg, suplim = suplim_arg))
}
