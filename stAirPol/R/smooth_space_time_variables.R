#' Spatial Interpolation with Kriging
#'
#' The functions takes the delivered informations of sensors and sensor_data
#' and calculates a spatial interpolation with kriging for each unique
#' timestamp. For the Kriging we use \link{automap::autoKrige}
#'
#' @param sensor_data data as returned by \link{get_sensor_measured_values}
#' @param sensors data as returned by \link{get_sensors}
#' @inheritParams clean_model_data
#' @param grid an object as returned by \link{make_grid_traffic}
#' @param agg_info an object as returnet by \link{aggregation_information}
#' @param times_IQR remove outliers, which are more then times_IQR * IQR away
#' from the median
#' @param mc.cores how much cores should be used for parallelization, default is
#' one core less your maximum number of detected cores.
#' @param ... arguments passed to \link{automap::autoKrige}
#'
#' @return the interpolated dataset around the delivered grid
#' @export
#' @importFrom sp coordinates
#' @importFrom automap autoKrige
#' @importFrom lubridate round_date
#' @importFrom parallel detectCores
smooth_space_time_variables <- function(sensor_data, sensors = NULL, grid,
                                        agg_info,
                                        mc.cores = parallel::detectCores() - 1,
                                        quantiles_to_use = c(0.000, 1),
                                        times_IQR = 2.5, ...) {
  if (!is.null(sensors)) {
    sensor_data <- sensor_data[,
                        .(value = mean(value, na.rm = TRUE)),
                        by = list(locid,
                                  timestamp =
                                    lubridate::round_date(timestamp - agg_info$timeshift,
                                                          agg_info$aggregation_interval))]
      model_data <- merge(sensors, sensor_data, by.x = 'id', by.y = 'locid')
  } else {
    model_data <- sensor_data
  }
  model_data <- clean_model_data(model_data, times_IQR)
  times <- unique(model_data$timestamp)
  pred <- pbmcapply::pbmclapply(times, function(i) {
    try({
      mdata <- model_data[timestamp == i]
      mdata <- mdata[!is.na(value)]
      mdata <- mdata[!duplicated(mdata, by = c('lon', 'lat'))]
      mdata <- mdata[value <= quantile(value, quantiles_to_use[2],
                                       na.rm = TRUE) &
                       value >= quantile(value, quantiles_to_use[1],
                                         na.rm = TRUE)]
      g <- unique(grid[, .(lon, lat)])
      if (nrow(mdata) < 2) {
        mdata <- model_data[timestamp == i]
        mdata <- mdata[!duplicated(mdata, by = c('lon', 'lat'))]
        mdata <- mdata[!is.na(value)]
        g$prediction <- mean(mdata$value, na.rm = TRUE)
        g$timestamp <- as.POSIXct(i, origin = '1970-01-01 00:00:00')
        return(g)
      } else {
        sp::coordinates(g) =~ lon+lat
        sp::coordinates(mdata) =~ lon+lat
        a <- automap::autoKrige(value ~ 1,  mdata,  g, ...)
        g <-  unique(grid[, .(lon, lat)])
        g$prediction <- as.numeric(a$krige_output$var1.pred)
        g$timestamp <- as.POSIXct(i, origin = '1970-01-01 00:00:00')
      }
      if (any(is.na(g$prediction))) stop('NA prediction')
      return(g)
    })
    mdata <- model_data[timestamp == i]
    g <-  unique(grid[, .(lon, lat)])
    g$prediction <- mean(mdata$value, na.rm = TRUE)
    g$timestamp <- as.POSIXct(i, origin = '1970-01-01 00:00:00')
    return(g)
  }, mc.cores = mc.cores)
  prediction <- rbindlist(pred)
}
