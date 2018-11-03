#' aggregation_information
#'
#' @param timeshift timeshift which is applied to the data
#' @param aggregation_interval aggregation interval for the modelling, for more
#' information about the aggregation_interval units see
#' ?lubridate::round_date
#'
#' @return an object of the class `stAirPol.aggregation_information`
#' @export
#' @examples
#' m.agg_infor <- aggregation_information(timeshift = lubridate::hours(2),
#' aggregation_interval = '8 hours')
#' print(m.agg_infor)
aggregation_information <- function(timeshift, aggregation_interval) {
  agg <- list(timeshift = timeshift,
              aggregation_interval = aggregation_interval)
  class(agg) <- 'stAirPol.aggregation_information'
  agg
}


#' print.stAirPol.aggregation_information
#'
#' Print method for class stAirPol.aggregation_information
#'
#' @param x object of the class stAirPol.aggregation_information
#'
#' @export
#' @importFrom lubridate minutes
#' @importFrom lubridate floor_date
#' @importFrom lubridate ceiling_date
#'
#' @examples
#' m.agg_infor <- aggregation_information(timeshift = lubridate::hours(2),
#' aggregation_interval = '8 hours')
#' print(m.agg_infor)
print.stAirPol.aggregation_information <- function(x) {
  cat('Timeshift:              ', as.character(x$timeshift), ' \n')
  cat('Aggregation interval:   ', as.character(x$aggregation_interval), ' \n')
  dates <- Sys.Date() + lubridate::minutes(0:(24*3*60))
  borders <- data.frame(
    lower = unique(
      lubridate::floor_date(dates + x$timeshift,
                            unit = x$aggregation_interval) - x$timeshift
    ),
    upper = unique(
      lubridate::ceiling_date(dates + x$timeshift,
                              unit = x$aggregation_interval) - x$timeshift
    )
  )
  cat('--------------------------------------------------\n')
  cat('Intervalls during the next 72 hours: \n')
  for (i in seq_len(min(5,nrow(borders)))) {
    cat("    [", as.character(borders$lower[i]), ",",
             as.character(borders$upper[i]), "] \n")
  }
  if (5 < nrow(borders)) {
    cat("                         ...\n")
    cat("    [", as.character(borders$lower[nrow(borders)]), ",",
        as.character(borders$upper[nrow(borders)]), "] \n")
  }
}

