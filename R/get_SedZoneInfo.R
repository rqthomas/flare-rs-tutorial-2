################################################################################
# Code started by Molly Stroud on 1/29/26
# Estimate temperature of sediment zone, peak doy, and amplitude
################################################################################
get_sed_zone_data <- function(era5, depth, start_date){
  sed_temp <- mean(era5$prediction)
  # clean up era5
  airtemp <- era5 |>
      dplyr::select(c(datetime, prediction)) |>
      dplyr::mutate(datetime = as.Date(datetime)) |>
      dplyr::group_by(datetime) |>
      dplyr::summarize(Temp = mean(prediction))
  water_temp_init <- airtemp[airtemp$datetime == start_date,]$Temp
  airtemp$doy <- yday(airtemp$datetime)
    # get daily average over the years
  avg_airtemps <- airtemp |>
      dplyr::group_by(doy) |>
      dplyr::summarize(Temp = mean(Temp, na.rm = T))
  airtemp_amp <- (max(avg_airtemps$Temp) - min(avg_airtemps$Temp)) / 2
  airtemp_peakdoy <- which.max(avg_airtemps$Temp)
  if(depth <=5){
    nzones <- 1
    z1 <- depth + 1
    sed_amp <- airtemp_amp
    sed_doy <- airtemp_peakdoy
    return(data.frame(cbind(sed_temp, sed_amp, doy = sed_doy,
                            nzones, zone_heights = z1, water_temp_init)))
  }
  if(depth > 5 & depth <= 10) {
    nzones <- 2
    z1 <- depth + 1
    z2 <- 7
    sed_amp_z1 <- airtemp_amp
    sed_amp_z2 <- airtemp_amp * exp(-z2/depth)
    sed_doy_z1 <- airtemp_peakdoy
    # smooth out the air temp
    avg_airtemps$smoothed <- stats::filter(
      avg_airtemps$Temp,
      rep(1/14, 14),   # 2 week moving average
      sides = 2
    )
    mean_temp <- mean(avg_airtemps$smoothed, na.rm = T)
    sed_doy_z2 <- (sed_doy_z1 + airtemp_peakdoy) / 1.8
    return(data.frame(cbind(sed_temp, sed_amp = c(sed_amp_z2, sed_amp_z1),
                            doy = c(sed_doy_z2, sed_doy_z1),
                            nzones, zone_heights = c(z2, z1),
                            water_temp_init)))
  } else {
    nzones <- 3
    z1 <- depth + 1
    z2 <- z1*2/3
    z3 <- z1*1/3
    sed_amp_z1 <- airtemp_amp
    sed_amp_z2 <- airtemp_amp * exp(-z2/depth)
    sed_amp_z3 <- airtemp_amp * exp(-z3/depth)
    sed_doy_z1 <- airtemp_peakdoy
    # smooth out the air temp
    avg_airtemps$smoothed <- stats::filter(
      avg_airtemps$Temp,
      rep(1/14, 14),   # 2 week moving average
      sides = 2
    )
    mean_temp <- mean(avg_airtemps$smoothed, na.rm = T)
    sed_doy_z3 <- which(diff(avg_airtemps$smoothed > mean_temp) != 0)[-1]
    sed_doy_z2 <- (sed_doy_z1 + airtemp_peakdoy) / 1.8
    return(data.frame(cbind(sed_temp, sed_amp = c(sed_amp_z3, sed_amp_z2, sed_amp_z1),
                 doy = c(sed_doy_z3, sed_doy_z2, sed_doy_z1),
                 nzones, zone_heights = c(z3, z2, z1), water_temp_init)))
  }
}

# THIS SHOULD BE GENERALIZED
set_sed_zones <- function(depth){
  if(depth <=5){
    nzones <- 1
    z1 <- depth + 1
    return(data.frame(cbind(nzones, zone_heights = z1)))
  }
  if(depth > 5 & depth <= 10) {
    nzones <- 2
    z1 <- depth + 1
    z2 <- 7
    return(data.frame(cbind(nzones, zone_heights = c(z2, z1))))
  } else {
    nzones <- 3
    z1 <- depth + 1
    z2 <- z1*2/3
    z3 <- z1*1/3
    return(data.frame(cbind(nzones, zone_heights = c(z3, z2, z1))))
  }
}
