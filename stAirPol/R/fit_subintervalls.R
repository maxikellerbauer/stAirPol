#' Title
#'
#' @param data
#' @param grid
#' @param training_set
#' @param unit
#' @param nItr
#' @param nBurn
#' @param return
#' @param tol
#' @param retry_count
#' @param mc.cores
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
fit_subintervalls <- function(data, grid = NULL, training_set = NULL, unit = '1 week',
                              nItr = 5000, nBurn = 1000, return = 'prediction', tol = 0.01,
                              retry_count = 5,
                              mc.cores = parallel::detectCores() - 1, ...) {
  if (!is.null(grid)) {
    data.fit <- data
    data.predict <- grid
  } else if (!is.null(training_set)) {
    data.fit <- data[sensor_id %in% training_set$train]
    data.predict <- data[sensor_id %in% training_set$test]
  } else {
    stop("grid or training_set must be not NULL")
  }
  data.fit_list <-
    split(data.fit,
          lubridate::floor_date(data.fit$timestamp, unit = unit))
  data.predict_list <-
    split(
      data.predict,
      lubridate::floor_date(data.predict$timestamp, unit = unit)
    )
  pred_list <- pbmcapply::pbmclapply(seq_along(data.fit_list), function(i) {
    attempt <- 1
    #' retry model fitting and prediction in cause of a strange behavior
    #' sometimes
    while (attempt <= retry_count) {
      attempt <- attempt + 1
      t <- try({
        s <- data.fit_list[[i]][!is.na(value), .N, by = sensor_id][N > floor(max(N)/3)]
        # s <- data.fit_list[[i]][!is.na(value), .N, by = sensor_id]
        d <- data.fit_list[[i]][sensor_id %in% s$sensor_id]
        d <- data.fit_list[[i]]
        model <- fit_sp_model(data = d, training_set = NULL, tol = tol,
                           nItr = nItr, nBurn = nBurn, ...)
        if (return == 'prediction') {
          p <- rbindlist(lapply(unique(data.predict_list[[i]]$sensor_id), function(x) {
            predict(model, newdata  = data.predict_list[[i]][sensor_id %in% x],
                    training_set = NULL, ...)}))
          if (RMSE(p$value, p$prediction) < 100) {
            return(p)
          } else {
            stop('No Covergence, RMSE above 2000')
          }
          if (mean(p$prediction) > 500) stop('retry')
          if (is.null(training_set) & !is.null(grid)) return(p)
        } else {
          m <- model$parameter
          if (any(abs(m$Mean) > 1000)) {
            stop('No Covergence, Paramaters Mean above 1000')
          }
          m$name <- row.names(model$parameter)
          m$split <- i
          return(m)
        }
      })
    }
    return(t)
  }, mc.cores = mc.cores )
  pred_list <- lapply(1:length(pred_list), function(x) {
    if ('try-error' %in% class(pred_list[[x]])) {
      print(paste0('ERROR IN TIMEINTERVAL', x))
      print(pred_list[[x]])
      return(NULL)
    } else {
      return(pred_list[[x]])
    }
  })
  newdata <- rbindlist(pred_list)
  class(newdata) <- c('stAirPol.prediction', class(newdata))
  newdata
}
