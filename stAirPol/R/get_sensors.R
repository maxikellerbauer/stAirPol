#' get_sensors
#'
#' Extract all informatiions about the sensors which are located in the choosen
#' plz.
#'
#' @param date_pattern character vector in the format Year-Month
#' @param plz a character vector which contains all german postcodes of the
#' area which should be used.
#' @param path filepath where the informations are stored
#'
#' @return \link{data.table} with all sensors in that timeperiod
#' @export
#' @import data.table
#' @import sf
#'
#' @examples
#' get_sensors('2018-01', 80539)
get_sensors <- function(date_pattern, plz, path) {
  sensor_loc <- list()
  for (variable_type in c('P1', 'P2', 'humidity', 'temperature')) {
    for (date in date_pattern) {
      con <- connection(date, variable_type, path)
      sensor_loc[[paste0(date, variable_type)]] <-
        data.table(RSQLite::dbReadTable(con, 'locid'))
      sensor_loc[[paste0(date, variable_type)]]$variable <- variable_type
      RSQLite::dbDisconnect(con)
    }
  }
  sensor_loc <- rbindlist(sensor_loc)
  sensor_loc <- sensor_loc[!duplicated(sensor_loc, by = c('id', 'variable'))]

  plz_shape <- get_plz_shape(plz)
  sensor_points <- format_dt_to_points(sensor_loc)
  points_inside <- points_inside_multipolygon(points = sensor_points,
                                                         multipolygon = plz_shape)
  sensor_coordinates <- sf::st_coordinates(points_inside)
  sensor_loc <- sensor_loc[lon %in% sensor_coordinates[, 1] &
               lat %in% sensor_coordinates[, 2]]
  split(sensor_loc, sensor_loc$variable)
}

