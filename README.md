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
renv::run("analysis/03_bayesian_calibration_DEzs.R",    name = "bayesian_calibration 123", args = c(123,0,50,8))
renv::run("analysis/03_bayesian_calibration_DEzs.R",    name = "bayesian_calibration 122", args = c(122,0,50,8))
renv::run("analysis/03_bayesian_calibration_DEzs.R",    name = "bayesian_calibration 121", args = c(121,0,50,8))
renv::run("analysis/03_bayesian_calibration_DEzs.R",    name = "bayesian_calibration 120", args = c(120,0,50,8))

# or alternatively (running outside of RStudio directly from shell can be more robust):
#   cd GitHub/geco-bern/rsofun_doc/
#   tmux  # open multiple tmux windows to run:
#   Rscript -e 'renv::run("analysis/03_bayesian_calibration_DEzs.R", project = "../rsofun_doc", args = c(123,0,50,8))'
#   Rscript -e 'renv::run("analysis/03_bayesian_calibration_DEzs.R", project = "../rsofun_doc", args = c(122,0,50,8))'
#   Rscript -e 'renv::run("analysis/03_bayesian_calibration_DEzs.R", project = "../rsofun_doc", args = c(121,0,50,8))'
#   Rscript -e 'renv::run("analysis/03_bayesian_calibration_DEzs.R", project = "../rsofun_doc", args = c(120,0,50,8))'

# Eventually, these codes were run on UBELIX with SLURM batch scripts:
# sbatch ~/GitHub/geco-bern/analysis/run_analysis2.sh
# sbatch ~/GitHub/geco-bern/analysis/run_predictions.sh

# run sensitivity analysis
# sbatch ~/GitHub/geco-bern/analysis/run_sensitivity2.sh

# generate figures for manuscript
renv::run("analysis/05d_make_figures.R", project = "../rsofun_doc")
# renv::run("analysis/05_make_figures.R", project = "../rsofun_doc")
# renv::run("analysis/05c_make_prediction_figures_111_113.R", project = "../rsofun_doc")

# eventually results were archived with:
# rsync -i --info=progress2   -avz --no-owner --omit-dir-times   /scratch/network/giub_geco/fbernhard/rsofun_doc_outputs /storage/capacity/occr_geco/data_2/archive_projects/PRJ_2025_fbernhard_rsofunDoc/
```
