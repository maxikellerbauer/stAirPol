
#' get_wind_data
#'
#' read and transform the downloaded DWD data
#'
#' @param date_pattern date pattern which spezifies the month in the format 'YYYY_MM'
#' @param dwd_stat Stations of the DWD as returned in the format of get_dwd_sensor_ids()
#' @param path filepath where the informations are stored
#'
#' @return historical wind data
#' @export
get_wind_data <- function(date_pattern, dwd_stat, path) {
  dates <- seq(as.Date(paste0(min(date_pattern), "-01"), format = "%Y-%m-%d"),
               max(as.Date(paste0(max(date_pattern), "-", 28:31),
                           format = "%Y-%m-%d"), na.rm = TRUE),
               1)
  wind <- list.files(paste0(path, '/dwd/'), pattern = 'produkt_ff_stunde')
  wind <- unlist(sapply(dwd_stat$stations_id, function(x) wind[grepl(paste0( x,".txt"), wind)]))
  wind <- rbindlist(lapply(wind, function(x) readr::read_csv2(paste0(path, '/dwd/', x))))
  data <- wind
  data <- data[MESS_DATUM > gsub('-', '', min(dates) - lubridate::days(20))]
  data <- data[MESS_DATUM < gsub('-', '', max(dates) + lubridate::days(7))]
  data$time <- as.POSIXct(strptime(as.character(data$MESS_DATUM), format = '%Y%m%d%H'), tz = 'UTC')
  data$F <- as.numeric(as.character(data$F))
  data[F < 0]$F <- 0
  data <- data[,.(mean_f = mean(F, na.rm = TRUE)
  ) , by = list(timestamp = lubridate::round_date(time - m.agg_info$timeshift,
                                                  unit = m.agg_info$aggregation_interval),
                STATIONS_ID)]
  dwd_stat$stations_id <- as.numeric(as.character(dwd_stat$stations_id))
  data$STATIONS_ID <- as.numeric(as.character(data$STATIONS_ID))
  data <- merge(data, dwd_stat, by.y = 'stations_id', by.x = 'STATIONS_ID')
  data
}

#' get_rain_data
#'
#' read and transform the downloaded DWD data
#'
#' @param date_pattern date pattern which spezifies the month in the format 'YYYY_MM'
#' @param dwd_stat Stations of the DWD as returned in the format of get_dwd_sensor_ids()
#' @param path filepath where the informations are stored
#'
#' @export
#' @return historical wind data
get_rain_data <- function(date_pattern, dwd_stat, path) {
  dates <- seq(as.Date(paste0(min(date_pattern), "-01"), format = "%Y-%m-%d"),
               max(as.Date(paste0(max(date_pattern), "-", 28:31),
                           format = "%Y-%m-%d"), na.rm = TRUE),
               1)

  wind <- list.files(paste0(path, '/dwd/'), pattern = 'produkt_rr_stunde')
  wind <- unlist(sapply(dwd_stat$stations_id, function(x) wind[grepl(paste0( x,".txt"), wind)]))
  wind <- rbindlist(lapply(wind, function(x) readr::read_csv2(paste0(path, '/dwd/', x))))
  data <- wind
  data <- data[MESS_DATUM > gsub('-', '', min(dates) - lubridate::days(20))]
  data <- data[MESS_DATUM < gsub('-', '', max(dates) + lubridate::days(7))]
  data$time <- as.POSIXct(strptime(as.character(data$MESS_DATUM), format = '%Y%m%d%H'), tz = 'UTC')
  data$R1 <- as.numeric(as.character(data$R1))
  data[R1 < 0]$R1 <- 0
  data <- data[,.(mean_R1 = base::sum(as.numeric(R1), na.rm = TRUE)
  ) , by = list(timestamp = lubridate::round_date(time - m.agg_info$timeshift,
                                                  unit = m.agg_info$aggregation_interval),
                STATIONS_ID)]
  dwd_stat$stations_id <- as.numeric(as.character(dwd_stat$stations_id))
  data$STATIONS_ID <- as.numeric(as.character(data$STATIONS_ID))
  data <- merge(data, dwd_stat, by.y = 'stations_id', by.x = 'STATIONS_ID')
  data
}
