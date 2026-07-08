################################################################################
# Author: Molly Stroud
# Started 1/20/26
################################################################################

# This script will install all necessary packages to run FLARE 

remotes::install_github('rqthomas/GLM3r')
remotes::install_github('usgs-r/glmtools', force = T, upgrade = 'never')
library(glmtools)
devtools::install_github("FLARE-forecast/ropenmeteo", force = T, upgrade = "never")
library(ropenmeteo)
pacman::p_load('rstac', 'terra', 'stars', 'ggplot2', 'tidyterra', 'viridis', 'yaml', 
               'gdalcubes', 'tmap', 'dplyr', 'tidyverse', 'sf',
               'arrow', 'raster', 'terra', 'elevatr', 'marmap', 'rLakeAnalyzer',
               'httr', 'jsonlite', 'readr')