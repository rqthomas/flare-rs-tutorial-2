################################################################################
# GLOBathy script
################################################################################
#pacman::p_load(tidyverse, sf, raster, terra, dplyr, elevatr, marmap, rLakeAnalyzer)
################################################################################
# this code creates an index file for GLOBathy, so users can easily find the
# bathymetry file corresponding to their lake of interest
################################################################################
#files <- list.files("Bathymetry_Rasters", full.names = T, recursive = T, pattern = ".tif")

#file_index <- data.frame()
#for(file in files){
  #tif <- raster(file)
  #xmin <- round(xmin(tif), digits = 4)
  #xmax <- round(xmax(tif), digits = 4)
  #ymin <- round(ymin(tif), digits = 4)
  #ymax <- round(ymax(tif), digits = 4)
  #info <- cbind(xmin, xmax, ymin, ymax, file)
  #file_index <- rbind(file_index, info)
  #print(file)
#}
#write_csv(file_index, "Bathymetry_Rasters/index_file.csv")
message("Downloading GLOBathy index file")
index <- read_csv("https://amnh1.osn.mghpcc.org/bio230121-bucket01/GLOBathy/GLOBathy_index.csv")
find_matches <- function(bbox){
  mean_x <- (bbox["left"] + bbox["right"]) / 2
  mean_y <- (bbox["bottom"] + bbox["top"]) / 2
  match <- index |>
    filter(xmin < mean_x & xmax > mean_x) |>
    filter(ymin < mean_y & ymax > mean_y)
  if(nrow(match > 0)){
    print(match)
    bathy <- raster(paste0("https://amnh1.osn.mghpcc.org/bio230121-bucket01/GLOBathy/", match$file))
    return(bathy)
  } else {message("No matches found")}
}

get_ha <- function(bathy_raster, points){
  # convert to df and clean up
  bathy_df <- as.data.frame(bathy_raster, xy = T)
  bathy_df <- na.omit(bathy_df)
  colnames(bathy_df) <- c("Longitude", "Latitude", "Elevation")
  # convert to H/A relationship for GLM
  min_elevation <- min(bathy_df$Elevation)
  
  bathy_df <- bathy_df |> 
    dplyr::mutate(height = Elevation - min(Elevation)) |> 
    dplyr::select(Longitude, Latitude, height) 
  data_grid <- griddify(bathy_df, nlon = ncol(bathy_raster), nlat = nrow(bathy_raster)) 
  area_grid <- raster::area(data_grid, na.rm = TRUE, weights = FALSE)
  # filter out cells with no data
  area_grid <- area_grid[!is.na(area_grid)] 
  surface_area <- length(area_grid)*mean(area_grid) 
  area_layers <- approx.bathy(Zmax = abs(max(bathy_df$height)), 
                              surface_area*1000000,
                              Zmean= mean(bathy_df$height), 
                              method = "cone",
                              zinterval = 1,
                              depths = seq(0, abs(max(bathy_df$height)), by = 1))
  
  # add actual elevation
  elev <- elevatr::get_elev_point(points[1,], src = 'aws')
  if(is.na(elev$elevation) == TRUE){
    q <- readline("elevatr is unavailable. Please type an approximate elevation estimate of your lake: ")
    area_layers$depths <- area_layers$depths + as.numeric(q)
    plot(area_layers$Area.at.z, area_layers$depths, type = 'l', 
         xlab = 'Area at Depth (m2)', ylab = 'Depth (m)', main = 'GLOBathy')
    return(area_layers)
    }
  #print(elev$elevation)
  #convert depth back to elevation
  area_layers$depths <- (area_layers$depths + min_elevation) *-1
  area_layers$depths <- area_layers$depths + elev$elevation
  plot(area_layers$Area.at.z, area_layers$depths, type = 'l',
       xlab = 'Area at Depth (m2)', ylab = 'Depth (m)', main = 'GLOBathy')
  return(area_layers)
}

# calculate GLM's bsn_len and bsn_wid (basin length and width at full pond)
# from the lake extent (non-NA cells) of the bathymetry raster
get_bsn_dims <- function(bathy_raster){
  # non-NA cells represent the water body extent at full water level
  bathy_df <- as.data.frame(bathy_raster, xy = TRUE)
  bathy_df <- na.omit(bathy_df)
  colnames(bathy_df) <- c("Longitude", "Latitude", "Elevation")

  lake_pts <- sf::st_as_sf(bathy_df, coords = c("Longitude", "Latitude"),
                            crs = sf::st_crs(bathy_raster))

  # reproject to the local UTM zone so distances are measured in meters on a planar grid
  centroid <- sf::st_coordinates(sf::st_centroid(sf::st_union(lake_pts)))
  utm_zone <- floor((centroid[1] + 180) / 6) + 1
  utm_epsg <- ifelse(centroid[2] >= 0, 32600, 32700) + utm_zone
  lake_pts_utm <- sf::st_transform(lake_pts, crs = utm_epsg)

  # basin outline at full water level
  hull <- sf::st_convex_hull(sf::st_union(lake_pts_utm))
  hull_coords <- sf::st_coordinates(hull)[, c("X", "Y")]

  # bsn_len = max distance between any two points on the basin outline
  d <- as.matrix(dist(hull_coords))
  max_idx <- which(d == max(d), arr.ind = TRUE)[1, ]
  bsn_len <- max(d)

  # bsn_wid = max extent of the basin perpendicular to the length axis
  p1 <- hull_coords[max_idx[1], ]
  p2 <- hull_coords[max_idx[2], ]
  axis_unit <- (p2 - p1) / sqrt(sum((p2 - p1)^2))
  perp_unit <- c(-axis_unit[2], axis_unit[1])
  perp_proj <- sweep(hull_coords, 2, p1) %*% perp_unit
  bsn_wid <- max(perp_proj) - min(perp_proj)

  return(list(bsn_len = bsn_len, bsn_wid = bsn_wid))
}


