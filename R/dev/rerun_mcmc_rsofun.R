        # #!/usr/bin/env Rscript
        #
        # # Script running Bayesian calibration
        #
        # # script is called with three arguments for sampling:
        # # 1. calibration scenario [0,1,2,3]
        # # 2. burnin iterations
        # # 3. total iterations (incl. burnin)
        #
        # # Note that these arguments can be used to distribute over multiple nodes.
        # # Distribution over CPU cores of a single node is handled by multidplyr
        # # and argument ncores in the script.
        #
        # # Example:
        # # Rscript -e 'renv::run("analysis/03_bayesian_calibration.R", project = "../rsofun_doc", args = c(3,11,51))'
        #
        # # # When using this script directly from RStudio, not from the shell, specify
        # # args <- c("3", "11", "51")
        # # args <- c("0", "11", "51")
        #
        # # to receive arguments to script from the shell
        # args = commandArgs(trailingOnly=TRUE)
        # stopifnot(length(args)==3)
        # args <- as.integer(args)
        # names(args) <- c("scenario","burnin","iterations")
        #
        # stopifnot(length(args[["scenario"]])>=1)
        # stopifnot(length(args[["burnin"]])==1)
        # stopifnot(length(args[["iterations"]])==1)
        #
        # print(sprintf(
        #   "Requested scenario #%d, for (%d-%d) iterations",
        #   args[["scenario"]],
        #   args[["iterations"]],
        #   args[["burnin"]]
        # ))
        #
        # # pak::pkg_install("geco-bern/rsofun@ebb6b208e72f83d7cb13c5802239b122f6853a52")

out_calib <- readr::read_rds(here::here("data/out_calib__setup0_DEzs-50-10iter_3x3chains_on_CPU3x1.rds"))

# we might need to continue running a sampler
out_calib$mod # This is the mcmcSamplerList
out_calib$mod[[1]] # This is chain nr1 (of class mcmcSampler)
out_calib$mod[[2]] # This is chain nr2 (of class mcmcSampler)
out_calib$mod[[3]] # This is chain nr3 (of class mcmcSampler)
# undebug(runMCMC)
chain1 <- out_calib$mod[[1]]
# rerun_chain1 <- runMCMC(
#   chain1  # providing a mcmcSampler automatically takes its settings, setup, and sampler and extends the samples
# )
# rererun_chain1 <- runMCMC(rerun_chain1)
# plot(chain1)         # 15 long
# plot(rerun_chain1)   # 30 long
# plot(rererun_chain1) # 45 long
# chain1$settings[1:4]         # burnin=10,iterations=50
# rerun_chain1$settings[1:4]
# rererun_chain1$settings[1:4]
# # Why doe we request 50 iterations (with 10 burnin), but end up with only 15 iterations in the trace.
# # This is because internal to the DEzs function it computes:
#     # Npop = settings$startValue # i.e. number of internal chains
#     # burnin <- settings$burnin/Npop
#     # n.iter <- ceiling(settings$iterations/Npop)
#     # if (n.iter < 2)  {stop("The total number of iterations must be greater than 3")}
#     # lChain <- ceiling((n.iter - burnin)/settings$thin) + 1
#     # which in the above case is:
#         # burnin <- 10/3
#         # n.iter <- ceiling(50/3)
#         # lChain <- ceiling((n.iter - burnin)/settings$thin) + 1 # 15

rerun_chain1b <- runMCMC(
  chain1,  # providing a mcmcSampler automatically takes its settings, setup, and sampler and extends the samples
  settings = list(burnin = 0, iterations = 60) # 60/3 adds 20 samples to each chain
)
# rerun_chain1$chain[[1]]
# rerun_chain1$chain[[1]][14:17,]
rerun_chain1b$chain[[1]][14:17,]

nrow(rerun_chain1b$chain[[1]]) # length of 1 internal chain: 35
plot(rerun_chain1b)   # 35 long




