################################################################################
# Author: Molly Stroud
# Started 1/20/26
################################################################################

# This script will:
# 1. Download remote sensing data
# 2. Download meteorological data
# 3. Grab bathymetry data
# 4. Grab Kw factor
# 5. Estimate sediment zone info
# 6. Create GLM and config yml file
pacman::p_load('rstac', 'terra', 'stars', 'ggplot2', 'tidyterra', 'viridis', 'yaml',
               'gdalcubes', 'tmap', 'dplyr', 'tidyverse', 'sf',
               'arrow', 'raster', 'terra', 'elevatr', 'marmap', 'rLakeAnalyzer',
               'httr', 'jsonlite', 'readr')

library(ropenmeteo)
library(GLM3r)
library(glmtools)

# This need to be set to run each experiment
run_name <- "run"
config_flare_file <- "configure_flare.yml"
config_set_name <- "analysis"
configure_run_file <- "configure_run.yml"
use_s3 <- FALSE

lake_directory <- here::here()
# Point FLARE to GLM location
Sys.setenv('GLM_PATH'=paste0(lake_directory, '/binary/macos/glm'))
options(future.globals.maxSize = 891289600)

# get scripts needed to run this code
inputs <- read_yaml("configure_rs_run.yml")
walk(list.files(file.path(here::here(), "R"), pattern = "\\.R$", full.names = TRUE), source)

num_lakes <- length(inputs$lakes)

lake_num <- 1
start_date <- as_date(inputs$start_datetime)
forecast_start_date <- as_date(inputs$forecast_start_datetime)
forecast_end_date <- as_date(inputs$start_datetime) + days(inputs$forecast_horizon)

site_id <- names(inputs$lakes[lake_num])
bbox_list <- inputs$lakes[lake_num][[1]][[1]]
bbox <- c(left = bbox_list$left,
          bottom = bbox_list$bottom,
          right = bbox_list$right,
          top = bbox_list$top)

focal_point_list <- inputs$lakes[lake_num][[1]][[2]]

kw_list <- inputs$lakes[lake_num][[1]][[3]]


points_df <- data.frame(lon = c(focal_point_list$lon), lat = focal_point_list$lat)
points <- st_as_sf(x = points_df,
                   coords = c("lon", "lat"),
                   crs = 4326)

max_depth_allowed <- inputs$lakes[lake_num][[1]][[4]]

################################################################################
# 1. Download remote sensing data
# Warning: if you are trying to download data over a long period of time (>>1yr)
# or over a large lake, this will take a long time.
# If you are downloading over a very short period of time (<2 weeks), there is
# a high chance no data will be available.
################################################################################
thermaldata <- get_lst(bbox,
        start_date = format(as_datetime(inputs$start_date), "%Y-%m-%dT%H:%M:00Z"),
        end_date = format(as_datetime(inputs$forecast_start_datetime) + days(inputs$forecast_horizon),"%Y-%m-%dT%H:%M:00Z"),
        points)
# see what it looks like!
ggplot() +
  geom_stars(data = thermaldata["thermal_C"]) +
  facet_wrap(~time) +
  theme_classic() +
  scale_fill_viridis(na.value = 'transparent') +
  labs(fill = "Temperature (C)") +
  coord_fixed()
# get values
thermal_vals <- get_vals(points, thermaldata)
output <- clean_data(thermal_vals, site_id)

# create directory for targets file & save
dir.create(paste0('./targets/', site_id, '/'), recursive = T)
write_csv(output, paste0('targets/', site_id, '/', site_id, '-targets-rs.csv'))

## now download SWOT data for changes in lake depth
#swot_data <- get_swot(bbox, start_date, end_date, site_id)
#if(nrow(swot_data) != 0){
#  swot_data$datetime <- as.Date(swot_data$datetime)
#}
#targets <- read_csv(paste0('targets/', site_id, '/', site_id, '-targets-rs.csv'))
#targets <- rbind(swot_data, targets)
#write_csv(targets, paste0('targets/', site_id, '/', site_id, '-targets-rs.csv'))


################################################################################
# 2. Download meteorological data
# Warning: this may take a while depending on length of your date range
# Warning: Python must be installed to run this
################################################################################
# stage 2: calls R/get_met.py directly

start_date <- as_date(inputs$start_datetime)
forecast_start_date <- as_date(inputs$forecast_start_datetime)
forecast_end_date <- as_date(inputs$start_datetime) + days(inputs$forecast_horizon)

.run_get_met_py(
  stage = "stage2",
  site_id = site_id,
  bbox = bbox,
  reference_datetime = forecast_start_date
)

# stage 3: calls R/get_met.py directly
.run_get_met_py(
  stage = "stage3",
  site_id = site_id,
  bbox = bbox,
  reference_datetime = start_date,
  end_date = forecast_start_date
)

################################################################################
# 3. Get bathymetric data
# If you already have existing bathymetry, skip to get_ha function and input
# your bathymetry raster
################################################################################
# get bathymetry from GLOBathy
bathy <- find_matches(bbox)
plot(bathy) # visually check this is the correct lake
ha <- get_ha(bathy, points)

if(!is.na(max_depth_allowed) & !is.null(max_depth_allowed)){
  abs_z = ha$depths[1] - ha$depths
  ha[which(abs_z <= max_depth_allowed), ]
}

################################################################################
# 4. Get Kw factor (light extinction)
# If your lake of interest is in the US, use the function get_kw_US
# If your lake of interest is outside the US, use the function get_kw_global
################################################################################
# Search for lake in LAGOS US database
if(kw_list$type == "us"){
  if(is.na(kw_list$lake_row) || is.null(kw_list$lake_row)){
    kw_list$lake_row <- 1
  }
  mylake_kw <- get_kw_US(bbox, validate = FALSE, lake_row = kw_list$lake_row)
}else if(kw_list$type == "global"){
  mylake_kw <- get_kw_global(bbox, validate = FALSE)
}else{
  if(!is.na(kw_list$value) & !is.null(kw_list$value)){
    mylake_kw <- kw_list$value
  }else{
    stop("missing user specified kw value")
  }
}
################################################################################
# 5. Estimate sediment zone info
# This will be removed when I incorporate the newest version of FLARE, but am
# keeping now for the tutorial
################################################################################
# first get air temperature data over a few years
era5_download <- get_historical_weather(latitude = points_df$lat[1],
                                        longitude = points_df$lon[1],
                                        start_date = Sys.Date() - 2000, # get a long enough date range
                                        end_date = Sys.Date(),
                                        variables = c("temperature_2m"))

sed_data <- get_sed_zone_data(era5_download,
                              depth = (max(values(bathy), na.rm = T) - min(values(bathy), na.rm = T)),
                              start_date)


################################################################################
# 6. Create GLM file and config files
################################################################################
# update configure_flare yml
yml <- yaml::read_yaml(file.path("configuration", config_set_name, config_flare_file))
yml$location$site_id <- site_id
yml$location$latitude <- points_df[[2]][1]
yml$location$longitude <- points_df[[1]][1]
yml$default_init$lake_depth <- rev(max(ha$depths) - min(ha$depths))
modeled_depths <- seq(0, yml$default_init$lake_depth-1)
yml$default_init$temp <- rep(sed_data$water_temp_init[1], times = length(modeled_depths))
yml$default_init$temp_depths <- modeled_depths
yml$model_settings$modeled_depths <- modeled_depths

yaml::write_yaml(yml, file.path("configuration", config_set_name, config_flare_file))

# create list of variable values & names for input to nml
var_list <- list(site_id, mylake_kw, site_id, points_df[[2]][1], points_df[[1]][1],
                 dim(ha)[1], rev(ha$depths), rev(ha$Area.at.z), rev(max(ha$depths) - min(ha$depths))-0.5,
                 sed_data$zone_heights, sed_data$nzones[1])
var_name_list <- list("sim_name", "Kw", "lake_name", "latitude", "longitude",
                      "bsn_vals", "H", "A", "lake_depth",
                      "zone_heights", "n_zones")

# update nml
update_nml(var_list, var_name_list,
           working_directory = file.path("configuration", config_set_name), nml = 'glm3.nml')

run_config <- yaml::read_yaml(file.path("configuration", config_set_name, configure_run_file))
run_config$start_datetime <- inputs$start_datetime
run_config$forecast_horizon <- inputs$forecast_horizon
run_config$forecast_start_datetime <- inputs$forecast_start_datetime
run_config$sim_name <- run_name

dir.create(file.path("./restart", site_id, run_name), recursive = T)

yaml::write_yaml(run_config, file = file.path("restart", site_id, run_name, configure_run_file))

################################################################################
# Now,run FLARE
################################################################################

FLAREr::run_flare(lake_directory, configure_run_file = configure_run_file, config_set_name = config_set_name,
                  clean_start = FALSE, sim_name = run_name)
