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

# get scripts needed to run this code
source("R/install.R")
source("01LakeInfo.R")
walk(list.files(file.path(here::here(), "R"), pattern = "\\.R$", full.names = TRUE), source)

################################################################################
# 1. Download remote sensing data
# Warning: if you are trying to download data over a long period of time (>>1yr) 
# or over a large lake, this will take a long time.
# If you are downloading over a very short period of time (<2 weeks), there is
# a high chance no data will be available.
################################################################################
thermaldata <- get_lst(bbox, 
        paste0(start_date, "T00:00:00Z"), 
        paste0(end_date, "T00:00:00Z"),
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
output <- clean_data(thermal_vals)

# create directory for targets file & save
dir.create(paste0('./targets/', site, '/'), recursive = T)
write_csv(output, paste0('targets/', site, '/', site, '-targets-rs.csv'))

# now download SWOT data for changes in lake depth 
swot_data <- get_swot(bbox, start_date, end_date, site)

# see what it looks like!
ggplot() +
  geom_line(data = swot_data, aes(x = datetime, y = observation)) +
  theme_classic() +
  labs(x = element_blank(), y = element_blank())

targets <- read_csv(paste0('targets/', site, '/', site, '-targets-rs.csv'))
targets <- rbind(swot_data, targets)
write_csv(targets, paste0('targets/', site, '/', site, '-targets-rs.csv'))


################################################################################
# 2. Download meteorological data
# Warning: this may take a while depending on length of your date range
# Warning: Python must be installed to run this 
################################################################################
# stage 2: calls R/get_met.py directly 
.run_get_met_py(c(
  "stage2",
  "--site", site,
  "--bbox", bbox[["left"]], bbox[["bottom"]], bbox[["right"]], bbox[["top"]],
  "--start-date", as.character(as.Date(start_date)),
  "--end-date", as.character(as.Date(end_date))
))
message("Stage 2 data downloaded!")

# stage 3: calls R/get_met.py directly
.run_get_met_py(c(
  "stage3",
  "--site", site,
  "--bbox", bbox[["left"]], bbox[["bottom"]], bbox[["right"]], bbox[["top"]],
  "--start-date", as.character(as.Date(start_date))
))
message("Stage 3 data downloaded!")


################################################################################
# 3. Get bathymetric data
# If you already have existing bathymetry, skip to get_ha function and input
# your bathymetry raster
################################################################################
# get bathymetry from GLOBathy
bathy <- find_matches(bbox)
plot(bathy) # visually check this is the correct lake
ha <- get_ha(bathy, points)


################################################################################
# 4. Get Kw factor (light extinction)
# If your lake of interest is in the US, use the function get_kw_US
# If your lake of interest is outside the US, use the function get_kw_global
################################################################################
# Search for lake in LAGOS US database
mylake_kw <- get_kw_US(bbox)

# Search for lake in global database
#mylake_kw <- get_kw_global(bbox)

# If your lake was unavailable in either of these databases, 
# you may set mylake_kw based on knowledge of your lake of interest

# If your lake is very turbid (Secchi < 1):
# mylake_kw <- 1.7/1

# If your lake is somewhere between turbid and clear (Secchi > 1, < 5):
#ylake_kw <- 1.7/3

# If your lake is very clear (Secchi > 5):
# mylake_kw <-  1.7/5


################################################################################
# 5. Estimate sediment zone info
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
# create list of variable values & names for input to nml
var_list <- list(site, mylake_kw, site, points_df[[2]][1], points_df[[1]][1],
                 dim(ha)[1], rev(ha$depths), rev(ha$Area.at.z), rev(max(ha$depths) - min(ha$depths)), 
                 sed_data$sed_temp, sed_data$sed_amp, sed_data$doy,
                 sed_data$zone_heights, sed_data$nzones[1])
var_name_list <- list("sim_name", "Kw", "lake_name", "latitude", "longitude",
                      "bsn_vals", "H", "A", "lake_depth",
                      "sed_temp_mean", "sed_temp_amplitude", "sed_temp_peak_doy",
                      "zone_heights", "n_zones")
# update nml
update_nml(var_list, var_name_list, 
           working_directory = 'configuration/analysis', nml = 'glm3.nml')

# update configure_flare yml
yml <- yaml::read_yaml("configuration/analysis/configure_flare.yml")
yml$location$site_id <- site
yml$location$latitude <- points_df[[2]][1]
yml$location$longitude <- points_df[[1]][1]
yml$default_init$lake_depth <- (max(ha$depths) - min(ha$depths))
yml$default_init$temp <- rep(sed_data$water_temp_init[1], times = yml$default_init$lake_depth+1)
yml$default_init$temp_depths <- seq(0, yml$default_init$lake_depth)
yml$model_settings$modeled_depths <- seq(0, yml$default_init$lake_depth)

yaml::write_yaml(yml, "configuration/analysis/configure_flare.yml")


################################################################################
# Now, open 03FLARE to run FLARE
################################################################################