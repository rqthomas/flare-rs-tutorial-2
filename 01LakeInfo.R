#################################################################################
# Author: Molly Stroud
# Started 1/29/26
################################################################################
# Input below:
# 1. Your desired bounding box coordinates
# 2. The coordinates of a representative point(s) over your lake or reservoir of interest
# 3. Your start and end dates in the following format: YYYY-DD-MM

################################################################################
# INPUTS
################################################################################

# a four letter site name
# EXAMPLE: SUGG for Lake Suggs
site <- "wald"
# site <- "fcre"

# specify bounding box
bbox <- c(left = -71.3452,
          bottom = 42.4366,
          right = -71.3334,
          top = 42.4421)
# bbox <- c(left = -79.840037, 
#                bottom = 37.301435, 
#                right = -79.833651, 
#                top = 37.311487)

# pick representative point(s) of lake
# for example, if your lake is a perfect circle, a good point would be the
# middle of the circle
points_df <- data.frame(lon = c(-71.3394), lat = c(42.4393))

# points_df <- data.frame(lon = c(-79.837347, -79.838447), 
#                         lat = c(37.303424, 37.304081))
points <- st_as_sf(x = points_df,
                   coords = c("lon", "lat"),
                   crs = 4326)


# dates over which you want to run forecasts
# for the tutorial, pick a date range of ~11-15 days
# you may have to alter these dates depending on data availability
# e.g. data may be sparse in England in the winter due to cloud cover
# **START DATE MUST BE AFTER 2020-10-01**
start_date <- "2025-01-01"
end_date <- "2025-01-05"



