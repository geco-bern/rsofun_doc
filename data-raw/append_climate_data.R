# This script appends climate data to the the input forcings for
# 'df_chi_forcing' and 'df_vj_forcing' by using the {ingestr} package.
#
# It needs access to worldclim data set.
#
# The appended forcing data sets are stored as *.rds files in subfolder data/

rm(list = ls())
library(tidyverse)
library(rpmodel)
library(rgeco) # remotes::install_github("https://github.com/geco-bern/rgeco")
library(dplyr)
library(purrr)
# library(rsofun)  # install from branch simple_pmodel_v2
# devtools::install_github("geco-bern/rsofun@simple_pmodel_v2")
