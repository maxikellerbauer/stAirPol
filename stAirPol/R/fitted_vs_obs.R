#' plot.stAirPol.prediction
#'
#' Plot the results of a crossvalidation.
#'
#' @param prediction a object as returned by \link{predict.stAirPol.model}
#'
#' @return a \link{ggplot2} object which contains the plot
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
#' plot(pred.gp)
#' plot(pred.gp, time_dimension = TRUE)
plot.stAirPol.prediction <- function(prediction, time_dimension = FALSE) {
  if (!time_dimension) {
    g <- ggplot(data = prediction, aes(x = value, y = prediction)) +
      geom_point() +
      theme_classic() +
      scale_x_continuous(limits = c(0, max(prediction$value))) +
      scale_y_continuous(limits = c(0, max(prediction$prediction))) +
      geom_abline(slope = 1)
  } else {
    g <- ggplot(data = melt(prediction[, .(obs_value = value, prediction, timestamp)],
                       id.vars = c('timestamp')),
           aes(x = timestamp, y = value, col = variable)) +
      geom_point() +
      theme(legend.position = 'bottom') +
      theme_classic() +
      geom_hline(yintercept = 0, lty = 2) +
      xlab(NULL) +
      xlab(NULL)
  }
  g
}
