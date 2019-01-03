#' RMSE
#'
#' @param z observed values
#' @param zhat predicted values
#'
#' @export
#'
#' @examples
#' data("mini_dataset")
#' mini_dataset <- clean_model_data(mini_dataset)
#' formula = value ~ humi + temp + rainhist + windhist +
#'   trafficvol + log(sensor_age)
#' training_set <- get_test_and_training_set(mini_dataset, sampel_size = 0.75,
#'                                           random.seed = 220292)
#' model.gp <- fit_sp_model(data = mini_dataset, formula = formula,
#'                          model = 'GP', training_set = training_set)
#' pred.gp <- predict(model.gp, mini_dataset, training_set)
#' RMSE(pred.gp$value, pred.gp$prediction)
RMSE <- function(z, zhat) {
  z <- as.matrix(z)
  zhat <- as.matrix(zhat)
  x <- c(z - zhat)
  u <- x[!is.na(x)]
  round(sqrt(sum(u^2)/length(u)), 4)
}



#' MAE
#'
#' @param z observed values
#' @param zhat predicted values
#'
#' @export
#'
#' @examples
#' data("mini_dataset")
#' mini_dataset <- clean_model_data(mini_dataset)
#' formula = value ~ humi + temp + rainhist + windhist +
#'   trafficvol + log(sensor_age)
#' training_set <- get_test_and_training_set(mini_dataset, sampel_size = 0.75,
#'                                           random.seed = 220292)
#' model.gp <- fit_sp_model(data = mini_dataset, formula = formula,
#'                          model = 'GP', training_set = training_set)
#' pred.gp <- predict(model.gp, mini_dataset, training_set)
#' MAE(pred.gp$value, pred.gp$prediction)
MAE <- function(z, zhat) {
  z <- as.matrix(z)
  zhat <- as.matrix(zhat)
  x <- abs(c(zhat - z))
  u <- x[!is.na(x)]
  round(sum(u)/length(u), 4)
}

#' BIAS
#'
#' @param z observed values
#' @param zhat predicted values
#'
#' @export
#'
#' @examples
#' data("mini_dataset")
#' mini_dataset <- clean_model_data(mini_dataset)
#' formula = value ~ humi + temp + rainhist + windhist +
#'   trafficvol + log(sensor_age)
#' training_set <- get_test_and_training_set(mini_dataset, sampel_size = 0.75,
#'                                           random.seed = 220292)
#' model.gp <- fit_sp_model(data = mini_dataset, formula = formula,
#'                          model = 'GP', training_set = training_set)
#' pred.gp <- predict(model.gp, mini_dataset, training_set)
#' BIAS(pred.gp$value, pred.gp$prediction)
BIAS <- function(z, zhat) {
  z <- as.matrix(z)
  zhat <- as.matrix(zhat)
  x <- c(zhat - z)
  u <- x[!is.na(x)]
  round(sum(u)/length(u), 4)
}


#' Coverage
#'
#' @param zhat predicted values
#' @param up upper quantile
#' @param low lower quantile
#'
#' @export
#'
#' @examples
#' data("mini_dataset")
#' mini_dataset <- clean_model_data(mini_dataset)
#' formula = value ~ humi + temp + rainhist + windhist +
#'   trafficvol + log(sensor_age)
#' training_set <- get_test_and_training_set(mini_dataset, sampel_size = 0.75,
#'                                           random.seed = 220292)
#' model.gp <- fit_sp_model(data = mini_dataset, formula = formula,
#'                          model = 'GP', training_set = training_set)
#' pred.gp <- predict(model.gp, mini_dataset, training_set)
#' Coverage(pred.gp$value, pred.gp$up, pred.gp$low)
Coverage <- function(zhat, up, low) {
  mean(zhat <= up & zhat >= low, na.rm = TRUE)
}

#' evaluate_prediction
#'
#' @param pred an object as returned by \link{predict.stAirPol.model}
#' @param round digits for round the measures
#' @param ...
#'
#' @export
#'
#' @examples
#' data("mini_dataset")
#' mini_dataset <- clean_model_data(mini_dataset)
#' formula = value ~ humi + temp + rainhist + windhist +
#'   trafficvol + log(sensor_age)
#' training_set <- get_test_and_training_set(mini_dataset, sampel_size = 0.75,
#'                                           random.seed = 220292)
#' model.gp <- fit_sp_model(data = mini_dataset, formula = formula,
#'                          model = 'GP', training_set = training_set)
#' pred.gp <- predict(model.gp, mini_dataset, training_set)
#' evaluate_prediction(pred.gp)
evaluate_prediction <- function(pred, round = 2, ...) {
  ev <- data.frame(
    RMSE = RMSE(pred$value, pred$prediction),
    MAE = MAE(pred$value, pred$prediction),
    BIAS = BIAS(pred$value, pred$prediction),
    Coverage = Coverage(pred$value, pred$up, pred$low),
    `R2` = cor(pred$value, pred$prediction, use = 'pair')
  )
  ev <- apply(ev, 2, function(x) round(x, round))
  return(data.frame(t(ev)))
}
