################################################################################
# Code started by Molly Stroud on 11/18/25
################################################################################

################################################################################
## the below code is designed to pull landsat thermal imagery over a specified 
# area and estimate temperature over the reservoir
################################################################################
# get bboxes
#source("NEON_bboxes.R") # or, create your own bbox here

# define stac url
ls = stac("https://planetarycomputer.microsoft.com/api/stac/v1")

################################################################################
# Function to create thermal stars object with specified dates and bbox
################################################################################
get_lst <- function(bbox, start_date, end_date, points) {
  # grab items within dates of interest
  items <- ls |>
    stac_search(collections = "landsat-c2-l2",
                bbox = bbox,
                datetime = paste(start_date, end_date, sep="/"),
                limit = 1000) |>
    ext_query("eo:cloud_cover" < 30) |> #filter for cloud cover
    post_request() |>
    items_sign(sign_fn = sign_planetary_computer()) |>
    items_fetch()
  if(length(items$features) == 0){
    message("There are no cloud-free thermal images of this lake in the specified date range. Consider changing or expanding your date range.")
    return( )
  } else {
    message("Downloading Landsat Thermal data")
    # filter out non LS8/9
    items$features <- Filter(
      function(f) f$properties$platform %in% c("landsat-8", "landsat-9"),
      items$features
    )
    # define the cube space
    cube <- cube_view(srs = "EPSG:4326",
                      extent = list(t0 = start_date, 
                                    t1 = end_date,
                                    left = bbox[1], 
                                    right = bbox[3],
                                    top = bbox[4], 
                                    bottom = bbox[2]),
                      dx = 0.00031, # 30 m resolution
                      dy = 0.00031, 
                      dt = "P1D",
                      aggregation = "median", 
                      resampling = "average")
    # create stac image collection
    col <- stac_image_collection(items$features,
                                 asset_names = c("lwir11", "qa_pixel"),
                                 url_fun = identity)
    # make raster cube
    data <- raster_cube(image_collection = col, 
                        view = cube) |>
      apply_pixel(expr = "((qa_pixel & (1<<7)) != 0) * lwir11", names = "thermal") |> # if not water, set to 0
      apply_pixel(expr = "(thermal * 0.00341802) - 124.15", names = "thermal_C") # convert to C
    # make stars obj
    ls_stars <- st_as_stars(data)
    ls_stars$thermal_C[ls_stars$thermal_C == -124.15] <- NA
    # remove empty dates
    arr <- ls_stars[[1]] # extract raw array (x, y, time)
    non_na_counts <- apply(arr, 3, function(slice) sum(!is.na(slice))) # count non-NA pixels for each time
    valid_idx <- which(non_na_counts > 0) # indices of slices that have at least one real value
    # build cleaned object by stacking only valid slices
    slices <- lapply(valid_idx, function(i) ls_stars[,,, i, drop = FALSE])
    clean_ls_stars <- do.call(c, c(slices, along = "time"))
    if(all(is.na(clean_ls_stars$thermal))){
      message("There are no cloud-free thermal images of this lake in the specified date range. Consider changing or expanding your date range.")
      return( )
    } else {
      vals <- st_extract(clean_ls_stars["thermal_C"], points)
      vals_df <- data.frame(vals)
      if(all(is.na(vals_df$thermal_C))){
        message("There are no cloud-free thermal images of this lake in the specified date range. Consider changing or expanding your date range.")
        return( )
      } else {
        return(clean_ls_stars)
      }
    }
  }
}
################################################################################
# function to extract values and write out csv
################################################################################
get_vals <- function(points, thermal_data){
  vals <- st_extract(thermal_data["thermal_C"], points)
  vals_df <- data.frame(vals)
  if(all(is.na(vals_df$thermal_C))){
    message("There are no cloud-free thermal images of this lake in the specified date range. Consider changing or expanding your date range.")
    return( )
  }
  # if only one point, add back in time column and rearrange to format
  if(length(vals_df) < 3){
    if(nrow(vals_df) > 1){
      vals_df$time <- st_dimensions(thermal_data)$time$values$start
    } else{
      vals_df$time <- st_dimensions(thermal_data)$time$offset
    }
    vals_df <- vals_df |>
      relocate(thermal_C, .after = time)
  }
  # if multiple points, group same date points and get mean temp
  if(dim(points)[1] > 1){
    vals_df <- vals_df |>
      group_by(time) |>
      summarize(mean_thermal_C = mean(thermal_C, na.rm = T))
    return(vals_df)
  } else {
    return(vals_df)
  }
}

################################################################################
# function to clean up data for input to FLARE
################################################################################
clean_data <- function(values){
  values <- na.omit(values)
  if(length(values) > 2){
    values <- values[2:3]
  }
  values$time <- paste0(values$time, "T00:00:00Z")
  values$site_id <- site
  values$depth <- 0
  values$variable <- 'temperature'
  colnames(values)[1] <- "datetime"
  colnames(values)[2] <- "observation"
  values$observation[values$observation < 0] <- 0 # remove likely incorrect #s
  return(values)
}
