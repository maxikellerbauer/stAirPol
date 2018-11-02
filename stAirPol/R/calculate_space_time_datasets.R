#' Calculate datasets and grids for humidity and temperature
#'
#' @return nothing, but 8 datasets get assigned to the global enviroment
#' with the names: data.humi.p1, data.humi.p2, grid.humi.p1, grid.humi.p2,
#' data.temp.p1, data.temp.p2, grid.temp.p1 and grid.temp.p2
#' @export
calculate_space_time_datasets <- function() {
  # humidity ----------------------------------------------------------------
  data_humi <- get_model_frame(sensor_data[['humidity']], sensors[['humidity']])
  data.humi.p1 <<- smooth_space_time_variables(sensor_data = data_humi,
                                              grid = sensors[['P1']][, .(lon, lat)],
                                              agg_info = m.agg_info,
                                              times_IQR = 1.5)
  data.humi.p2 <<- smooth_space_time_variables(sensor_data = data_humi,
                                              grid = sensors[['P2']][, .(lon, lat)],
                                              agg_info = m.agg_info,
                                              times_IQR = 1.5)
  grid.humi.p1 <<- smooth_space_time_variables(sensor_data = data_humi,
                                              grid = grid.traffic.p1[, .(lon, lat)],
                                              agg_info = m.agg_info,
                                              times_IQR = 1.5)
  grid.humi.p2 <<- smooth_space_time_variables(sensor_data = data_humi,
                                              grid = grid.traffic.p2[, .(lon, lat)],
                                              agg_info = m.agg_info,
                                              times_IQR = 1.5)

  # temperature -------------------------------------------------------------
  data_temp <- get_model_frame(sensor_data[['temperature']], sensors[['temperature']])
  data.temp.p1 <<- smooth_space_time_variables(sensor_data = data_temp,
                                              grid = sensors[['P1']][, .(lon, lat)],
                                              agg_info = m.agg_info,
                                              times_IQR = 1.5)
  data.temp.p2 <<- smooth_space_time_variables(sensor_data = data_temp,
                                              grid = sensors[['P2']][, .(lon, lat)],
                                              agg_info = m.agg_info,
                                              times_IQR = 1.5)
  grid.temp.p1 <<- smooth_space_time_variables(sensor_data = data_temp,
                                              grid = grid.traffic.p1[, .(lon, lat)],
                                              agg_info = m.agg_info,
                                              times_IQR = 1.5)
  grid.temp.p2 <<- smooth_space_time_variables(sensor_data = data_temp,
                                              grid = grid.traffic.p2[, .(lon, lat)],
                                              agg_info = m.agg_info,
                                              times_IQR = 1.5)
  check_prediction(data.humi.p1)
  check_prediction(grid.humi.p1)
  check_prediction(data.humi.p2)
  check_prediction(grid.humi.p2)
  check_prediction(data.temp.p1)
  check_prediction(grid.temp.p1)
  check_prediction(data.temp.p2)
  check_prediction(grid.temp.p2)
}



#' Check the output of smooth_space_time_variables
#'
#' @param data \link{data.table} which contains a column with the name prediction
#'
#' @return a warnings if any NAs are detected in the column prediction
#' @export
#'
#' @examples
#' df <-  data.frame(prediction =c (1, 1, NA))
#' check_prediction(df)
check_prediction <- function(data) {
  if (any(is.na(data$prediction))) {
    warning('NAs in prediction, please see the temperature and humidity datasets and grids')
  }
}

#' Check the output of smooth_space_time_variables
#'
#' @param data \link{data.table} which contains a column with the name prediction
#'
#' @return a warnings if any NAs are detected in the column prediction
#' @export
#'
#' @examples
#' df <-  data.frame(prediction =c (1, 1, NA))
#' check_prediction(df)
check_hist <- function(data) {
  if (any(is.na(data$prediction))) {
    warning('NAs in historical aggregation, please see the wind and rain datasets and grids')
  }
}
