[![DOI](https://zenodo.org/badge/996792001.svg)](https://doi.org/10.5281/zenodo.17204361)

# rsofun_doc

This repository contains the code for producing results published in Paredes et al. (in press). This study documents the rsofun package and reports results from a calibration of the P-model to GPP and traits data. A first version of the paper is published as a pre-print (see reference below). The study is currently under review at Geoscientific Model Development. Code contained in this repository serves for the revised version of the study.

## Repository contents

- `analysis/` contains scripts for site selection, model calibration, calibration diagnostics, and model evaluation.
- `fig/` contains publication figures
- `data/` contains data objects created by scripts in `data-raw/`.
- `analysis-output/` contains data objects created by scripts in `analysis/`.

## References

Paredes, J. A., Hufkens, K., Marcadella, M., Bernhard, F., and Stocker, B. D.: rsofun v5.0: A model-data integration framework for simulating ecosystem processes, https://doi.org/10.1101/2023.11.24.568574, 8 February 2025.


## Automated workflow

```R
setwd("../rsofun_doc")
renv::restore()        # instantiate the correct environment

# process data
dir.create("data")
renv::run("data-raw/bigD13C_data.R",        name = "bigD13C_data")
renv::run("data-raw/vj_data.R",             name = "vj_data")
renv::run("data-raw/gpp_data.R",            name = "gpp_data")
# wait for the earlier scripts to finish, then run:
renv::run("data-raw/append_climate_data.R", name = "append_climate_data")

# setup data split into training and testing
renv::run("analysis/01_subset_test_train_sites.R", name = "subset_test_train_sites")
```

Run MCMC sampling of various calibration setups (setups are independent and can run in parallel).
This stores the output into a hardcoded path: `/scratch/network/giub_geco/fbernhard/rsofun_doc_outputs`:
```bash
# From RStudio:
#   renv::run("analysis/02_start_mcmc_bayesian_calibration_DREAMzs.R",    name = "bayesian_calibration 220", args = c(220,0,50,8))
# or alternatively (running outside of RStudio directly from tmux-shell can be more robust):
#   cd GitHub/geco-bern/rsofun_doc/
#   Rscript -e 'renv::run("analysis/02_start_mcmc_bayesian_calibration_DREAMzs.R", project = "../rsofun_doc", args = c(231,0,50,8))'
#   etc...

# Eventually, these codes were run on UBELIX for all setups with a SLURM batch script:
sbatch ~/GitHub/geco-bern/rsofun_doc/analysis/run_mcmc.sh # specify the setup through: "--array=220-223,226-231"
```

Sample posterior and predict train and test sets with various calibration setups (setups are independent and can run in parallel).
Setups are defined and described in `analysis/00_define_setups.R`
```bash
# Eventually, these codes were run on UBELIX for the six setups with the SLURM batch script run_prediction.sh:
~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 228 100000 30000 20 3 "_continued.rds"
~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 227 100000 30000 20 3 "_continued.rds"
~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 226 100000 30000 20 3 "_continued.rds"
~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 222 100000 30000 20 3 "_continued.rds"
~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 223 100000 30000 20 3 "_continued.rds"
~/GitHub/geco-bern/rsofun_doc/analysis/run_predictions.sh 231 100000 30000 20 3 "_continued.rds"
```

Generate figures for manuscript:
```R
renv::run("analysis/05_make_figures.R", project = "../rsofun_doc")
```

Archive results from the scratch filesystem to a permanent one, e.g. with:
```bash
rsync -i --info=progress2   -avz --no-owner --omit-dir-times   /scratch/network/giub_geco/fbernhard/rsofun_doc_outputs /storage/capacity/occr_geco/data_2/archive_projects/PRJ_2025_fbernhard_rsofunDoc/
```
