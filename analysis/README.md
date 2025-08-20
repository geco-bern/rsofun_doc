The scripts here run MCMC parameter sampling to to calibrate the P-model to the 
targets of ratio of Vcmax/Jmax (vj) and of bigD13C.

Input data sets must be provided:
- data/01_bigD13C-vj-gpp_calibsofun_drivers.rds
- data/01_bigD13C-vj-gpp_calibsofun_obs.rds


To run the analysis and generate the figures run:
- analysis/01_subset_test_train_sites.R   # generates data/01_test_train_split.csv
- analysis/00_dev_parallel2.R
