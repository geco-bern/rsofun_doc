The scripts here prepare the input forcing and output target tibbles needed 
to calibrate the P-model to the targets of ratio of Vcmax/Jmax (vj) and of ratio
of Ci/Ca (Chi).

These data sets will be (are) provided in the rsofun package as example data
sets.

Output data set:
- data/chi-vj-gpp_calibsofun_drivers.rds
- data/chi-vj-gpp_calibsofun_obs.rds


To recreate the data set run:
- data-raw/chi_data.R
- data-raw/vj_data.R
- data-raw/append_climate_data.R


Other input data:
- data-raw/GlobV_v2.1_env_open.csv: This is the open-access part of the data used for Smith et al., 2019. Received by Nick Smith by email, 30.06.2025
