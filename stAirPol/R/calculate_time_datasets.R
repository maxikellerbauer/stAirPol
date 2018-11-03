#' calculate_time_datasets
#'
#' Calculate datasets and grids for wind and rain
#'
#' @return nothing, but 16 datasets get assigned to the global environment
#' with the names:
#' data.wind.p1, data.wind.p2, grid.wind.p1, grid.wind.p2,
#' data.rain.p1, data.rain.p2, grid.rain.p1, grid.rain.p2,
#' data.windhist.p1, data.windhist.p2, grid.windhist.p1, grid.windhist.p2,
#' data.rainhist.p1, data.rainhist.p2, grid.rainhist.p1 and grid.rainhist.p2
#'
#' @import data.table
#' @export
calculate_time_datasets <- function() {
  ids <- get_dwd_sensor_ids(m.plz, path)
  data.wind <- get_wind_data(m.date_pattern, ids, path)
  data.rain <- get_rain_data(m.date_pattern, ids, path)

  data.wind.p1 <<- smooth_space_time_variables(sensor_data = data.wind[, .(value = mean_f, lon, lat, timestamp)],
                                              sensors =  NULL,
                                              grid = sensors[['P1']][, .(lon, lat)])
  grid.wind.p1 <<- smooth_space_time_variables(sensor_data = data.wind[, .(value = mean_f, lon, lat, timestamp)],
                                              sensors =  NULL,
                                              grid = grid.traffic.p1[, .(lon, lat)])
  data.wind.p2 <<- smooth_space_time_variables(sensor_data = data.wind[, .(value = mean_f, lon, lat, timestamp)],
                                              sensors =  NULL,
                                              grid = sensors[['P2']][, .(lon, lat)])
  grid.wind.p2 <<- smooth_space_time_variables(sensor_data = data.wind[, .(value = mean_f, lon, lat, timestamp)],
                                              sensors =  NULL,
                                              grid = grid.traffic.p2[, .(lon, lat)])

  data.rain.p1 <<- smooth_space_time_variables(sensor_data = data.rain[, .(value = mean_R1, lon, lat, timestamp)],
                                              sensors =  NULL,
                                              grid = sensors[['P1']][, .(lon, lat)])
  grid.rain.p1 <<- smooth_space_time_variables(sensor_data = data.rain[, .(value = mean_R1, lon, lat, timestamp)],
                                              sensors =  NULL,
                                              grid = grid.traffic.p1[, .(lon, lat)])
  data.rain.p2 <<- smooth_space_time_variables(sensor_data = data.rain[, .(value = mean_R1, lon, lat, timestamp)],
                                              sensors =  NULL,
                                              grid = sensors[['P2']][, .(lon, lat)])
  grid.rain.p2 <<- smooth_space_time_variables(sensor_data = data.rain[, .(value = mean_R1, lon, lat, timestamp)],
                                              sensors =  NULL,
                                              grid = grid.traffic.p2[, .(lon, lat)])


  par_hist_wind <<- optim_hist_para(get_model_frame(sensor_data[['P1']], sensors[['P1']]), data.wind.p1)
  data.windhist.p1 <<- calculate_hist_grid_points(split(data.wind.p1, paste0(data.wind.p1$lon, data.wind.p1$lat)),
                                                 par_hist_wind$lambda, par_hist_wind$lag)
  grid.windhist.p1 <<- calculate_hist_grid_points(split(grid.wind.p1, paste0(grid.wind.p1$lon, grid.wind.p1$lat)),
                                                 par_hist_wind$lambda, par_hist_wind$lag)
  par_hist_rain <<- optim_hist_para(get_model_frame(sensor_data[['P1']], sensors[['P1']]), data.rain.p1)
  data.rainhist.p1 <<- calculate_hist_grid_points(split(data.rain.p1, paste0(data.rain.p1$lon, data.rain.p1$lat)),
                                                 par_hist_rain$lambda, par_hist_rain$lag)
  grid.rainhist.p1 <<- calculate_hist_grid_points(split(grid.rain.p1, paste0(grid.rain.p1$lon, grid.rain.p1$lat)),
                                                 par_hist_rain$lambda, par_hist_rain$lag)

  par_hist_wind <<- optim_hist_para(get_model_frame(sensor_data[['P2']], sensors[['P2']]), data.wind.p2)
  data.windhist.p2 <<- calculate_hist_grid_points(split(data.wind.p2, paste0(data.wind.p2$lon, data.wind.p2$lat)),
                                                 par_hist_wind$lambda, par_hist_wind$lag)
  grid.windhist.p2 <<- calculate_hist_grid_points(split(grid.wind.p2, paste0(grid.wind.p2$lon, grid.wind.p2$lat)),
                                                 par_hist_wind$lambda, par_hist_wind$lag)
  par_hist_rain <<- optim_hist_para(get_model_frame(sensor_data[['P2']], sensors[['P2']]), data.rain.p2)
  data.rainhist.p2 <<- calculate_hist_grid_points(split(data.rain.p2, paste0(data.rain.p2$lon, data.rain.p2$lat)),
                                                 par_hist_rain$lambda, par_hist_rain$lag)
  grid.rainhist.p2 <<- calculate_hist_grid_points(split(grid.rain.p2, paste0(grid.rain.p2$lon, grid.rain.p2$lat)),
                                                 par_hist_rain$lambda, par_hist_rain$lag)

  check_prediction(grid.rainhist.p2)
  check_prediction(grid.rainhist.p1)
  check_prediction(grid.windhist.p2)
  check_prediction(grid.windhist.p1)
  check_prediction(data.rainhist.p2)
  check_prediction(data.rainhist.p1)
  check_prediction(data.windhist.p2)
  check_prediction(data.windhist.p1)

  check_hist(grid.rainhist.p2)
  check_hist(grid.rainhist.p1)
  check_hist(grid.windhist.p2)
  check_hist(grid.windhist.p1)
  check_hist(data.rainhist.p2)
  check_hist(data.rainhist.p1)
  check_hist(data.windhist.p2)
  check_hist(data.windhist.p1)
}
