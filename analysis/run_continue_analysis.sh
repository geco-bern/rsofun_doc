#! /usr/bin/bash -l
#SBATCH --job-name="calib_continue_scenTASKID"
#SBATCH --time=48:30:00
#SBATCH --partition=icpu-stocker # if you have access, this gives you priority
#SBATCH --array=70-78            # specifies the slurm array job with the number of tasks
#SBATCH --cpus-per-task=9        # nr of threads, used for shared memory jobs that run locally on a single compute node (default: 1)
#SBATCH --mail-user=your.email@unibe.ch
#SBATCH --mail-type=none                     # when do you want to get notified: none, all, begin, end, fail, requeue, array_tasks
#SBATCH --chdir=GitHub/geco-bern/rsofun_doc  # define here the working directory which contains your R-script, and where the output will be written to.
#SBATCH --output=slurm-logs/slurm-%x.%j.txt
#SBATCH --error=slurm-logs/slurm-%x.%j_err.txt

# run this as: xx24axxx@submit04:~$ sbatch GitHub/geco-bern/rsofun_doc/analysis/run_analysis.sh
# check with:  squeue --partition="icpu-stocker" --states="all" -o "%.23i %.9P %.8j %.8u %.8T %.11M %.11l %.6D %.3C %.11m %8z %20R %.19V %16p %o"

export SBATCH_EXPORT=NONE    # source: https://hpc-unibe-ch.github.io/slurm/submission.html#exportnone
export SLURM_EXPORT_ENV=ALL  # source: https://hpc-unibe-ch.github.io/slurm/submission.html#exportnone

echo "Started on: $(date --rfc-3339=seconds)"
echo "Hostname: $(hostname)"
echo "Working directory: $PWD"   # Is most likely the HOME directory. Allows to check in the log.

module load GEOS/3.12.2-GCC-13.3.0
module load PROJ/9.4.1-GCCcore-13.3.0
module load GDAL/3.10.0-foss-2024a
module load UDUNITS/2.2.28-GCCcore-13.3.0
module load SQLite/3.45.3-GCCcore-13.3.0
module load CMake/3.29.3-GCCcore-13.3.0
module load GCC/13.3.0
module load libxml2/2.12.7-GCCcore-13.3.0
module load R/4.4.2-gfbf-2024a

## Continue the Bayesian calibration (MCMC sampling)
PREV_NRUNS=40000 # which previous sampling to continue
NEW_NRUNS=10000  # how many samples to add
# Rscript analysis/03b_continue_mcmc_rsofun.R "out_calib__scen${SLURM_ARRAY_TASK_ID}_DEzs-${PREV_NRUNS}-0iter_8x3chains_on_CPU8x1.rds" "${NEW_NRUNS}"
Rscript analysis/03b_continue_mcmc_rsofun.R "out_calib__scen${SLURM_ARRAY_TASK_ID}_DEzs-${PREV_NRUNS}-0iter_8x3chains_on_CPU8x1_continued.rds" "${NEW_NRUNS}"

echo "Finished on: $(date --rfc-3339=seconds)"
