#' SQLite connection
#'
#' Builds a SQLite connection to the spezified database.
#'
#' @param date_pattern date pattern which spezifies the month in the format 'YYYY_MM'
#' @param variable_type which measured variable , P1, P2, pressure, temperature, humidity
#' @param path filepath where the informations are stored
#'
#' @return a RSQLite DB connection
#' @import RSQLite
#' @export
#'
#' @examples
#' connection('2018_01', 'P1')
connection <- function(date_pattern, variable_type, path) {
  if (is.null(path)) {
    stop("No valid path to the databases, please reset the variable data_path correctly")
  }
  RSQLite::dbConnect(RSQLite::SQLite(),
                     paste0(path, '/db/data/',
                            '/', variable_type, '-',date_pattern, '.sqlite'))
}
