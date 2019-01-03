#' make_data_traffic
#'
#' @param sensors data as returned by \link{get_sensors}
#' @param lambda smoothing parameter
#' @param scale numeric value to scale the traffic volume, default is 100
#' @param mc.cores how much cores should be used for parallelization, default is
#' one core less your maximum number of detected cores.
#'
#' @description Calculate for each sensor the traffic volume data
#' @return \link{data.table} of the sensors with a new column which contains
#' informations about the traffic volume
#'
#' @importFrom pbmcapply pbmclapply
#'
#' @export
make_data_traffic <- function(sensors, lambda, scale = 100,
                              mc.cores = parallel::detectCores() - 1) {
  sensor_points <- format_dt_to_points(sensors)
  ll <- pbmcapply::pbmclapply(1:length(sensor_points), function(x) {
    sum(sapply(roads, function(y) smooth_loaction(y, sensor_points[x],
                                                  lambda = lambda)))
  }, mc.cores = mc.cores)
  sensors$trafficvol <- unlist(ll) / scale
  sensors
}
