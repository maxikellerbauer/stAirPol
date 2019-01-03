#' Smooth the trafficvolumne around one point
#'
#' @param roads an object as returned by \link{get_opentransportmap_data}
#' @param point on element as returned by \link{format_dt_to_points}
#' @param lambda smoothing parameter
#'
#' @return numerical value of the estimated trafficvolumne
#' @export
smooth_loaction <- function(roads, point, lambda = 0.05) {
  roads$distance <- sf::st_distance(point, roads)[1, ]
  sum(exp(-lambda * as.numeric(roads$distance)) * log(roads$trafficvol), na.rm = TRUE)
}

#' Optimize a lambda parameter for the traffic volume
#'
#' @param sensor_data data as returned by \link{get_sensor_measured_values}
#' @param sensors data as returned by \link{get_sensors}
#' @param validation_plot Plot a validation chart? default is TRUE
#' @param lambda_range over which range should lambda be optimized
#' @param mc.cores how much cores should be used for parallelization, default is
#' one core less your maximum number of detected cores.
#' @param roads as returned by \link{get_opentransportmap_data}
#'
#' @return the lambda which maximizes the correlation between the measured
#' PM values and the trafficvolumne data
#' @export
optim_lambda <- function(sensor_data, sensors, roads, validation_plot = TRUE,
                         lambda_range = seq(0.001, 0.02, length.out = 10),
                         mc.cores = parallel::detectCores() - 1) {
  sensor_data <- sensor_data[, .(value = mean(value)), by = list(locid)]
  sensor_data <- data.table(merge(sensor_data, sensors,
                                     by.y = 'id', by.x = 'locid', all.y = TRUE))
  sensor_points <- spAirPol:::format_dt_to_points(sensors)
  optim_me <- function(lambda) {
    ll <- lapply(1:length(sensor_points), function(x) {
      sum(sapply(roads,
                 function(y) smooth_loaction(y, sensor_points[x], lambda = lambda)))
    })
    data_traffic <- sensors
    data_traffic$trafficvol <- unlist(ll)
    d <- data.table(dplyr::inner_join(data_traffic, sensor_data))
    print(abs(cor(d$trafficvol, d$value, use = 'pair')))
    cor(d$trafficvol, d$value, use = 'pair')
  }
  r <- pbmcapply::pbmclapply(lambda_range, optim_me,
                             mc.cores = parallel::detectCores() - 1)
  if (validation_plot) {
    print(ggplot(data = data.frame(cor = unlist(r), lambda = lambda_range),
                 aes(x = lambda, y = cor)) +
      geom_point() +
      geom_line() +
      theme_classic())
  }
  lambda_range[which.max(unlist(r))]
}
