#' Combine datasets to a modelframe
#'
#' @param sensor_data data as returned by \link{get_sensor_measured_values}
#' @param sensors data as returned by \link{get_sensors}
#' @param expand should the spatio and temporal dimensions expand that all
#' missing observations are filled by NA_numeric
#'
#' @return a \link{data.table}
#' @importFrom dplyr left_join
#' @export
get_model_frame <- function(sensor_data, sensors, expand = FALSE) {
  data <- sensor_data[,
                         .(value = mean(value, na.rm = TRUE)),
                         by = list(locid,
                                   timestamp =
                                     lubridate::round_date(timestamp - m.agg_info$timeshift,
                                                           m.agg_info$aggregation_interval))]
  data <- merge(sensors, data, by.x = 'id', by.y = 'locid')
  data <- data[, value := mean(value, na.rm = TRUE), by = list(sensor_id, timestamp)]
  data <- data[!duplicated(data, by = c('sensor_id', 'timestamp'))]
  if (expand) {
    d1 <- data.table(expand.grid(timestamp = unique(data$timestamp),
                                 sensor_id = unique(data$sensor_id)))
    d1 <- data.table(dplyr::left_join(d1, unique(data[!duplicated(sensor_id), .(sensor_id, lon, lat,
                                                           variable, sensor_type,
                                                           id, location)]), by = c('sensor_id')))
    data <- data.table(dplyr::left_join(d1, data))
  }
  data$id <- NULL
  data
}
