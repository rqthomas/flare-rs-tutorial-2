#################################################################################
# Author: Molly Stroud
# Started 1/29/26
################################################################################
# Input below:
# 1. Your desired bounding box coordinates
# 2. The coordinates of a representative point(s) over your lake or reservoir of interest
# 3. Your start and end dates in the following format: YYYY-MM-DD

################################################################################
# INPUTS
################################################################################

# a four letter site name
site <- "wald" # walden pond
# site <- "crla" # crescent lake
# site <- "lcns" # loch ness

# specify bounding box
# walden pond
bbox <- c(left = -71.3452,
          bottom = 42.4366,
          right = -71.3334,
          top = 42.4421)

# # crescent lake
# bbox <- c(left = -122.042,
#           bottom = 43.457,
#           right = -121.949,
#           top = 43.506)
# 
# # loch ness
# bbox <- c(left = -4.764,
#           bottom = 57.134,
#           right = -4.265,
#           top = 57.417)

# pick representative point(s) of lake
# for example, if your lake is a perfect circle, a good point would be the
# middle of the circle
# wald
points_df <- data.frame(lon = c(-71.3394), lat = c(42.4393))
# crla
# points_df <- data.frame(lon = c(-121.9908), lat = c(43.4772))
# # lcns
# points_df <- data.frame(lon = c(-4.4187), lat = c(57.3319))

points <- st_as_sf(x = points_df,
                   coords = c("lon", "lat"),
                   crs = 4326)


# dates over which you want to run forecasts
# for the tutorial, pick a date range of ~10-15 days
# you may have to alter these dates depending on data availability
# e.g. data may be sparse in England in the winter due to cloud cover
# **START DATE MUST BE AFTER 2020-10-01**
start_date <- "2026-07-01"
#end_date <- "2026-06-15"
end_date <- "2026-07-15"



