# rsofun_doc

This repository contains the code for producing results published in Paredes et al. (2025). This study documents the rsofun package and reports results from a calibration of the P-model to GPP and traits data. A first version of the paper is published as a pre-print (see reference below). The study is currently under review at Geoscientific Model Development. Code contained in this repository serves for the revised version of the study.

## Repository contents

- `analysis/` contains scripts for site selection, sensitivity analysis, model calibration, calibration diagnostics, and model evaluation.
- `fig/` contains publication figures
- `data/` contains data objects created by scripts in `analysis/` or in `data-raw/`.

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

# run calibration scenarions in sequential order:
renv::run("analysis/03_bayesian_calibration.R",    name = "bayesian_calibration 3", args = c(3,11,51,3))
renv::run("analysis/03_bayesian_calibration.R",    name = "bayesian_calibration 2", args = c(2,11,51,3))
renv::run("analysis/03_bayesian_calibration.R",    name = "bayesian_calibration 1", args = c(1,11,51,3))
renv::run("analysis/03_bayesian_calibration.R",    name = "bayesian_calibration 0", args = c(0,11,51,3))

renv::run("analysis/03_bayesian_calibration.R",    name = "bayesian_calibration 0", args = c(99,0,12,1))

# or alternatively (running outside of RStudio directly from shell can be more robust):
#   cd GitHub/geco-bern/rsofun_doc/
#   tmux
#   # open multiple tmux windows to run:
#   Rscript -e 'renv::run("analysis/03_bayesian_calibration.R", project = "../rsofun_doc", args = c(3,0,5000,8))'
#   Rscript -e 'renv::run("analysis/03_bayesian_calibration.R", project = "../rsofun_doc", args = c(2,0,5000,8))'
#   Rscript -e 'renv::run("analysis/03_bayesian_calibration.R", project = "../rsofun_doc", args = c(1,0,5000,8))'
#   Rscript -e 'renv::run("analysis/03_bayesian_calibration.R", project = "../rsofun_doc", args = c(0,0,5000,8))'


# run sensitivity analysis
# renv::run() # TODO

# generate figures for manuscript
renv::run()
```
