source(here::here("R/calibration_helpers.R"))

test_mcmc_parallelization_dummyExample <- function(
    n_parallel_independent  = 3,        # number of cores for parallelization of independent chains     https://cran.r-project.org/web/packages/BayesianTools/vignettes/InterfacingAModel.html#running-several-mcmcs-in-parallel
    n_parallel_within_sampler = FALSE,  # number of cores for parallelization of within-sampler chains  https://cran.r-project.org/web/packages/BayesianTools/vignettes/InterfacingAModel.html#within-sampler-parallelization as well as https://cran.r-project.org/web/packages/BayesianTools/vignettes/BayesianTools.html#reference-on-creating-likelihoods

    n_chains_independent    = 3, # number of independent chains
    n_chains_within_sampler = 3, # number of internal chains to be sampled (at least 2 for DEzs)

    burnin = 0,
    iterations = 1000
){
  if(n_parallel_within_sampler==1){n_parallel_within_sampler <- FALSE} # since 1 means no parallel running. Should not be interpreted as TRUE which would lead to all_cores-1

  require(BayesianTools)
  start_time <- Sys.time()

  # Your external data
  x <- c(1, 2, 3)

  if (n_parallel_independent > 1){
    cl <- makeCluster(n_parallel_independent)
    registerDoParallel(cl)

    if (n_parallel_independent != n_chains_independent){
      warning(sprintf(
        "Requested %d indep. chains, but ran %d indep. chains as `n_parallel_independent` takes precedence.",
        n_chains_independent, n_parallel_independent)
      )
    }

    ll_factory <- function(my_x) {function(param) {sum(dnorm(param - my_x, log=TRUE))}}
    indep_chains <- foreach(i = 1:n_parallel_independent, .packages='BayesianTools') %dopar% {
      rebuilt_ll <- ll_factory(x) # inside worker: rebuild the closure so it picks up 'x'
      bayesianSetup <- createBayesianSetup(
        likelihood = rebuilt_ll,
        lower = c(-10, -10, -10), upper = c(10, 10, 10),
        parallel = n_parallel_within_sampler)
      runMCMC(
        bayesianSetup,
        sampler = 'DEzs',
        settings = list(
          burnin = burnin, iterations = iterations,
          nrChains = 1,                        # number of independent chains (kept here at 1 to parallelize with foreach and combining as a McmcSamplerList())
          startValue = n_chains_within_sampler # number of internal chains to be sampled
        )
      )
    }
    stopCluster(cl)

    out <- createMcmcSamplerList(indep_chains) # combine the independent chains

  } else {
    # sequential run of
    ll <- function(param) {sum(dnorm(param - x, log=TRUE))} # this picks up 'x' as a closure
    bayesianSetup <- createBayesianSetup(
      likelihood = ll,
      lower = c(-10, -10, -10), upper = c(10, 10, 10),
      parallel = n_parallel_within_sampler)
    out <- runMCMC(
      bayesianSetup,
      sampler = 'DEzs',
      settings = list(
        burnin = burnin, iterations = iterations,
        nrChains = n_chains_independent,     # number of independent chains
        startValue = n_chains_within_sampler # number of internal chains to be sampled
      )
    )
  }

  end_time <- Sys.time()
  out$walltime <- end_time - start_time
  # print(walltime)
  # summary(out)
  print(get_runtime(out))
  print(get_walltime(out))
}

