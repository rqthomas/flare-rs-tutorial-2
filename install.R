################################################################################
# Author: Molly Stroud
# Started 1/20/26
################################################################################

# This script will install all necessary packages to run FLARE
install.packages(c('remotes', 'pacman'))
remotes::install_github('rqthomas/GLM3r')
remotes::install_github("FLARE-forecast/FLAREr")
remotes::install_github('usgs-r/glmtools', force = T, upgrade = 'never')
remotes::install_github("FLARE-forecast/ropenmeteo", force = T, upgrade = "never")


