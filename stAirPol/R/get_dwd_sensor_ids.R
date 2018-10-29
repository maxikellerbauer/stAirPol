#' get_dwd_sensor_ids
#'
#' @param plz numeric vector of german post codes
#' @param path filepath where the informations are stored
#'
#' @return all DWD stations inside the plz areas
#'
#' @importFrom dplyr inner_join
#' @importFrom readr read_csv
#' @import data.table
#' @export
#' @examples
#' get_dwd_sensor_ids(80993, './')
get_dwd_sensor_ids <- function(plz, path) {
  dwd_stations <- data.table(readr::read_csv(paste0(path, '/dwd/index.csv')))
  plz_shape <- get_plz_shape(plz)
  dwd_points <- format_dt_to_points(dwd_stations)
  stations_indside <- points_inside_multipolygon(dwd_points, plz_shape)
  if (length(stations_indside) == 0) {
    warning("No DWD Station is inside the modelling area")
    dist <- sapply(1:length(dwd_points), function(x) sf::st_distance(dwd_points[x], plz_shape))
    dwd_stations$dist <- dist
    return(dwd_stations[order(dist)][1:5])
  }
  dplyr::inner_join(dwd_stations, data.frame(lon = sf::st_coordinates(stations_indside)[, 1],
                                             lat = sf::st_coordinates(stations_indside)[, 2]))
}
