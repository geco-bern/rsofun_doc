#! /usr/bin/bash -l
#SBATCH --job-name="predict_scenTASKID"
#SBATCH --time=48:30:00
#SBATCH --partition=icpu-stocker # if you have access, this gives you priority
#SBATCH --array=90-98            # specifies the slurm array job with the number of tasks
#SBATCH --cpus-per-task=9        # nr of threads, used for shared memory jobs that run locally on a single compute node (default: 1)
#SBATCH --mem-per-cpu=8G
#SBATCH --mail-user=your.email@unibe.ch
#SBATCH --mail-type=none                     # when do you want to get notified: none, all, begin, end, fail, requeue, array_tasks
#SBATCH --chdir=GitHub/geco-bern/rsofun_doc  # define here the working directory which contains your R-script, and where the output will be written to.
#SBATCH --output=slurm-logs/slurm-%x.%j.txt  ###S-B-A-T-C-H --error=slurm-logs/slurm-%x.%j_err.txt

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
NRUNS=40000      # which MCMC sampling to use
NPREDICTIONS=20 # how many samples of posterior to use for predictions
Rscript analysis/04_make-test-train_predictions.R "out_calib__scen${SLURM_ARRAY_TASK_ID}_DEzs-${NRUNS}-0iter_8x3chains_on_CPU8x1_continued.rds" "12000" "${NPREDICTIONS}" "8"

echo "Finished on: $(date --rfc-3339=seconds)"
