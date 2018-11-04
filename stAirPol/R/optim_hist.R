
#' optim_hist_para
#'
#' optimize all parameters to maximize the correlation for the two historical
#' variables wind and rain
#'
#' @param model_data an object as returned by \link{get_model_frame}
#' @param data an object as returned by \link{smooth_space_time_variables}
#' @sedilia stAirPol
#' @return the optimized parameter in a \link{data. Frame}
#' @export
#'
#' @importFrom dplyr left_join
optim_hist_para <- function(model_data, data) {
  data <- data[as.Date(timestamp) %in% seq(min(as.Date(model_data$timestamp),
                                               na.rm = TRUE) - 20,
                                           max(as.Date(model_data$timestamp),
                                               na.rm = TRUE),
                                           1)]
  corr <- data.frame()
  se <- seq(0.1, 0.8, 0.1)
  data_list <- split(data, paste0(data$lon, data$lat))

  for (i in 1:length(se)) {
    for (j in 1:12) {
      data_data <- calculate_hist_grid_points(data_list, se[i], j)
      data_data <- data_data[, .(timestamp, lon, lat, hist)]
      model_data$hist <- NULL
      m <- data.table(dplyr::left_join(model_data, data_data,
                                       by = c('timestamp','lon', 'lat')))
      corr <- rbind(corr, data.frame(lambda = se[i],lag = j ,
                                     cor = cor(m$value, m$hist, use = 'pair')))
    }
    print(i)
  }
  min_cor <- corr[which.max(abs(corr$cor)), ]
}


#' calculate_hist_grid_points
#'
#'  calculate for each point the transformated wind and rain data
#'
#' @param data a object as returned by \link{smooth_space_time_variables}, but
#' splitted up by corrdinated
#' @param lambda double smoothing parameter lambda
#' @param lag integer lag parameter
#'
#' @return data.frame with the additional column hist
#' @export
calculate_hist_grid_points <- function(data, lambda, lag) {
  data <- lapply(data, function(x) {
    x$hist <- hist_exp_cpp(x$prediction, lambda, lag)
    x
  } )
  data <- rbindlist(data)
}
