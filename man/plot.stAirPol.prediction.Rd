% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitted_vs_obs.R
\name{plot.stAirPol.prediction}
\alias{plot.stAirPol.prediction}
\title{plot.stAirPol.prediction}
\usage{
plot.stAirPol.prediction(prediction, time_dimension = FALSE)
}
\arguments{
\item{prediction}{an object as returned by \link{predict.stAirPol.model}}
}
\value{
a \link{ggplot2} object which contains the plot
}
\description{
Plot the results of a cross validation.
}
\examples{
data("mini_dataset")
mini_dataset <- clean_model_data(mini_dataset)
formula = value ~ humi + temp + rainhist + windhist +
  trafficvol + log(sensor_age)
training_set <- get_test_and_training_set(mini_dataset, sampel_size = 0.75,
                                          random.seed = 220292)
model.gp <- fit_sp_model(data = mini_dataset, formula = formula,
                         model = 'GP', training_set = training_set)
pred.gp <- predict(model.gp, mini_dataset, training_set)
plot(pred.gp)
plot(pred.gp, time_dimension = TRUE)
}
