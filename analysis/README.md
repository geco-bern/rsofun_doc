The scripts here run MCMC parameter sampling to to calibrate the P-model to the 
targets of daily gpp, ratio of Vcmax/Jmax (vj), and of bigD13C.

Input data sets must be provided:
- data/01_bigD13C-vj-gpp_calibsofun_drivers.rds
- data/01_bigD13C-vj-gpp_calibsofun_obs.rds


To run the analysis and generate the figures run:
- analysis/01_subset_test_train_sites.R   # generates data/01_test_train_split.csv
- analysis/02_start_mcmc_bayesian_calibration_DREAMzs.R and continue with 03_continue...
- analysis/04_make-test-train_predictions.R
- analysis/05_make_figures.R

Note that `run_mcmc.sh` and `run_predictions.sh` take care of running the analysis.
Then `renv::run("analysis/05_make_figures.R", project = "../rsofun_doc")` generates the figures.

Setups are defined and described in `analysis/00_define_setups.R`
