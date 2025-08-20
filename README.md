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
renv::run("analysis/00_bayesian_calibration.R",    name = "bayesian_calibration")

# run sensitivity analysis
# renv::run() # TODO

# generate figures for manuscript
renv::run()
```
