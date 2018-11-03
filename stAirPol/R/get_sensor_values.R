
#' sql_load_sensor_measured_values
#'
#' read all measured values for one date_pattern variable_type combination
#'
#' @param sensor_id numeric vector of sensor_ids of the luftdaten project
#' @param date_pattern date pattern which spezifies the month in the format YYYY_MM
#' @param path filepath where the informations are stored
#' @param variable_type e.g. P1, P2, temperature, humidity
#'
#' @export
#' @importFrom lubridate ymd_hms
#' @return all measured values
#'
sql_load_sensor_measured_values <- function(sensor_id, date_pattern,
                                            variable_type, path) {
  con <- connection(date_pattern, variable_type, path = path)
  sql <- paste0("select *
                from data
                where locid in (",paste0(sensor_id, collapse = ','),")")
  sensor_data <- data.table(RSQLite::dbGetQuery(con, sql))
  RSQLite::dbDisconnect(con)
  sensor_data$timestamp <- lubridate::ymd_hms(sensor_data$timestamp)
  sensor_data
}


#' get_sensor_measured_values
#'
#' @param date_pattern  character vector in the format '%Y-%m'
#' @param sensors object as returned from the function \link{get_sensors}
#' @param path filepath where the informations are stored
#'
#' @return all measured values
#' @export
get_sensor_measured_values <- function(sensors, date_pattern, path) {
  variable_type <- names(sensors)
  sensor_data <- list()
  for (variable in variable_type) {
    for (date in date_pattern) {
      tmp <- sql_load_sensor_measured_values(sensor_id = sensors[[variable]]$id,
                                             date_pattern = date,
                                             variable_type = variable,
                                             path = path)
      cat(paste0(variable, " ", date,
                   " --->  ", nrow(tmp), " rows extracted \n"))
      tmp$variable <- variable
      sensor_data[[paste0(variable, date)]] <- tmp
    }
  }
  sensor_data <- rbindlist(sensor_data)
  split(sensor_data, sensor_data$variable)
}
