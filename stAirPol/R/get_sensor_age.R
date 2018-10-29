


#' get_sensor_age
#'
#' Informations about the age of a sensor in weeks
#'
#' @param path filepath where the informations are stored
#' @param units in which units should the age caculated
#'
#' @return a data.table which contains the age of a sensor in months and the
#' sensor_id
#' @export
#' @importFrom readr read_csv
#' @examples
#' get_sensor_age('./')
get_sensor_age <- function(path, units = 'weeks') {
  d <- data.table(readr::read_csv(paste0(path, '/sensor_age.csv')))
  d$date_pattern_num <- as.numeric(gsub("-", "", d$date_pattern))
  d$sensor_age <- as.numeric(difftime(max(d$date), d$date, units = units))
  d[, .(sensor_id = ids, sensor_age)]
}
