
#' Download DWD Data
#'
#' That functions downloads all recent informations from the DWD
#' (Deutscher Wetter Dienst)
#' @param obs which observation should be downloaded? See
#' \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/} for
#' more informations
#' @param path filepath where the informations should be stored
#' @param type type = 'recent' for about the last two years, type = 'historical'
#' for a longer historical timeframe
#'
#' @export
#' @importFrom readr read_lines
#'
#' @examples
#' download_dvd_data(obs = 'wind')
#' download_dvd_data(obs = 'precipitation')
download_dwd_data <- function(obs, path, type = 'recent') {
  url <- paste0('ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/',
                obs, '/', type, '/')
  wind <- readr::read_lines(url)
  wind <- wind[grepl('.zip', wind)]
  wind <- sapply(wind, function(x) {
    x <- unlist(strsplit(x, " "))
    x[grepl('.zip', x)]
  })
  urls <- paste0(url, as.character(wind))
  sapply(seq_along(urls), function(x) {
    try({
      download.file(urls[x], destfile = paste0(path, wind[x]))
      system(paste0('cd ', path, '; unzip -n ', wind[x]))
    })
  })
  setup_station_table(path)
}


#' setup_station_table
#'
#' Setup a csv file which contains informations about the stations and there
#' coordinates
#'
#' @param path filepath where the informations should be stored
#' @importFrom  readr write_csv
#' @import data.table
setup_station_table <- function(path) {
  f <- list.files(path, pattern = 'Metadaten_Geographie', full.names = TRUE)
  stations <- list()
  for (i in seq_along(f)) {
    stations_id <- as.vector(gsub("[^0-9]", "", f[i]))
    meta <- data.table(readr::read_csv2(paste0(path, 'Metadaten_Geographie_',
                                               stations_id, '.txt')))
    meta <- meta[min(which(!is.na(meta$von_datum) & is.na(meta$bis_datum)))]
    lon <- mean(meta$Geogr.Breite, na.rm = TRUE)
    lat <- mean(meta$Geogr.Laenge, na.rm = TRUE)
    stations[[i]] <- data.frame(stations_id, lon, lat)
  }
  stations <- rbindlist(stations)
  stations$lon <- stations$lon / 1e5; stations$lat <- stations$lat / 1e5
  readr::write_csv(stations,
                   path = paste0(path, '/index.csv'))
}
