#' Setup informations about the age of each sensor
#'
#' The functions stores a csv file in the selected path
#'
#' @param path filepath where the informations should be stored
#'
#' @export
#' @importFrom readr write_csv
#' @importFrom readr read_lines
#'
#' @examples
#' sensor_age(path = './')
download_sensor_age <- function(path) {
  html <- readr::read_lines('http://archive.luftdaten.info/')
  html <- html[grepl('/icons/folder.gif', html)]
  dates <- sapply(html, function(x) {
    try({
      d <- as.Date(substring(strsplit(x, "href")[[1]][2], 3, 12))
      return(d)
    })
    return(NULL)
  })
  names(dates) <- NULL
  dates <- as.Date(unlist(dates), origin = '1970-01-01')
  sensor_dates <- lapply(dates, function(date) {
    try({
    d <- readr::read_lines(paste0('http://archive.luftdaten.info/', date, '/'))
    data <- d
    data <- data[grepl(paste0('href=\"', date), data)]
    data <- gsub("<.{2,70}>", "", data)
    data <- sapply(data, function(x) strsplit(x, '.csv')[[1]][1])
    data <- sapply(data, function(x) {
      s <- strsplit(x, '_')[[1]]
      s[length(s)]
      })
    names(data) <- NULL
    return(data.frame(date = date, ids = data))
    })
    return(NULL)
  })
  sensor_dates <- rbindlist(sensor_dates)
  sensor_dates <- sensor_dates[, .(date = min(date)) , by = ids]
  readr::write_csv(sensor_dates, path = paste0(path, '/sensor_age.csv'))
  NULL
}
