The scripts here prepare the input forcing and output target tibbles needed 
to calibrate the P-model to the targets of ratio of Vcmax/Jmax (vj) and of bigD13C.

These data sets will be (are) provided in the rsofun package as example data
sets.

Output data set:
- data/bigD13C-vj-gpp_calibsofun_drivers.rds
- data/bigD13C-vj-gpp_calibsofun_obs.rds


To recreate the data set run:
- data-raw/gpp_data.R
- data-raw/bigD13C_data.R
- data-raw/vj_data.R
- data-raw/append_climate_data.R


Other input data:
- data-raw/GlobV_v2.1_env_open.csv: This is the open-access part of the data used for Smith et al., 2019. Received by Nick Smith by email, 30.06.2025
- to run `append_climate_data.R` following data sets are needed:
    - `/data/archive/koeppengeiger_beck_2018/data/Beck_KG_V1_present_0p5.tif`
    - `/data/archive/landcovermodis_friedl_2015/data/MODIS-C006_MOD15A2_LAI_FPAR_zmaw/MODIS-C006_MOD15A2__LAI_FPAR__LPDAAC__GLOBAL_0.5degree__UHAM-ICDC__2000_2018__MON__fv0.02.nc`
    - `/data/archive/etopo_NA_NA/data/ETOPO1_Bed_g_geotiff.tif`
    - `/data/archive/worldclim_fick_2017/data/wc2.1_30s_*.tif`
    - and internet access to download the Mauna Loa CO2 record (`df_co2_mlo.csv`)