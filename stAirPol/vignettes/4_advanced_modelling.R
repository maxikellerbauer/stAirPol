#' ---
#' title: "Advanced Modelling"
#' author: "Maxi Kellerbauer"
#' date: "November 5th, 2018"
#' ---
#+ messages=FALSE,warning=FALSE, echo=FALSE, results='hide'



require(stAirPol)
require(spTimer)
require(data.table)
require(ggplot2)


# Read the data -----------------------------------------------------------
#' # Read the data
#' We load the daily data of Munich for PM2.5
data("muc_airPol_p2")
#' remove outliers in the data
data <- clean_model_data(muc_airPol_p2, timesIQR = 1.5)

#' We specify a trainings set for crossvalidation
training_set <- get_test_and_training_set(data, sampel_size = 0.75,
                                          random.seed = 220292)

#' We take the GPP model of 3_simple_modelling_and_crossvalidation.R
#' as base model, and want try to beat the predictive performance.
#' For speeding up the computation we only use the GPP model.
spatial.decay = spT.decay(distribution = Gamm(a = 2, b = 1), tuning = 0.25)
scale.transform = "SQRT"
cov.fnc = "exponential"
priors.gpp <- spT.priors(model = "GPP", inv.var.prior = Gamm(a = 2, b = 1),
                         beta.prior = Norm(0, 10^4))
formula = value ~ humi + temp + rainhist + windhist +
  trafficvol + log(sensor_age)
model.gpp.p2 <- fit_sp_model(data = data,
                             formula = formula,
                             model = 'GPP',
                             priors = priors.gpp,
                             cov.fnc = cov.fnc,
                             knots_count = 20,
                             knots_method = 'random',
                             training_set = training_set,
                             scale.transform = scale.transform,
                             spatial.decay = spatial.decay)

pred.gpp.p2 <- predict(model.gpp.p2, data, training_set)
evaluate_prediction(pred.gpp.p2)



# Manipulate the covariables and the fitting range ------------------------
#' # Manipulate the covariables and the fitting range

#' First we try to manipulate the Covariables. there are a lot of different ways.
#' We will add individual Intercept for each weekday
formula.wdays = value ~ humi + temp + rainhist + windhist +
  trafficvol + log(sensor_age) + stAirPol::weekday(timestamp) - 1
model.gpp.wdays <- fit_sp_model(data = data,
                               formula = formula.wdays,
                               model = 'GPP',
                               priors = priors.gpp,
                               cov.fnc = cov.fnc,
                               knots_count = 4,
                               report = 5,
                               training_set = training_set,
                               scale.transform = scale.transform,
                               spatial.decay = spatial.decay)
pred.gpp.wdays <- predict(model.gpp, data, training_set)
summary(model.gpp)
model_result_list <- list('pred.gpp.p2' = pred.gpp.p2,
                          'pred.gpp.wdays' = pred.gpp.wdays)
evaluate_prediction_table(model_result_list)

#' Now we want to train a single GPP model for each week.
#' Therefore, we use the fit_subintervalls() function.
#' The fit_subintervalls() functions splits the data set into the chosen
#' unit, here for example 1 week.
#' After splitting, it performs the model fitting and predictions to every
#' single week and combine the results to the known format like the predict()
#' function.
formula = value ~ humi + temp + rainhist + windhist +
  trafficvol + log(sensor_age)
pred.gpp.1week <- fit_subintervalls(data = data,
                          formula = formula,
                          model = 'GPP',
                          priors = priors.gpp,
                          cov.fnc = cov.fnc,
                          knots_count = 4,
                          training_set = training_set,
                          unit = '1 week',
                          scale.transform = scale.transform,
                          spatial.decay = spatial.decay)
model_result_list[['pred.gpp.1week']] <- pred.gpp.1week
evaluate_prediction_table(model_result_list)

#' we are able to train every subintervall we want, here for example 3 days
set.seed(2202)
pred.gpp.3days <- fit_subintervalls(data = data,
                          formula = formula,
                          model = 'GPP',
                          priors = priors.gpp,
                          cov.fnc = cov.fnc,
                          knots_count = 4,
                          training_set = training_set,
                          unit = '3 days',
                          scale.transform = scale.transform,
                          spatial.decay = spatial.decay)
model_result_list[['pred.gpp.3days']] <- pred.gpp.3days
evaluate_prediction_table(model_result_list)





# Initial values for the models -------------------------------------------
#' # Initial values for the models

#' We can specify initial values for the modelling
init <- spT.initials(model = 'GPP', sig2eps = 0.5, sig2eta = 0.5, rho = 0.01,
             beta = rep(0, 7), phi = 0.5)
model.gpp.p2 <- fit_sp_model(data = data,
                             formula = formula,
                             model = 'GPP',
                             priors = priors.gpp,
                             cov.fnc = cov.fnc,
                             knots_count = 4,
                             initials = init,
                             training_set = training_set,
                             scale.transform = scale.transform,
                             spatial.decay = spatial.decay)
pred.gpp.p2 <- predict(model.gpp.p2, data, training_set)
evaluate_prediction(pred.gpp.p2)


# Iterations of the MCMC-Runs ---------------------------------------------
#' # Iterations of the MCMC-Runs

#' We are also able to set the number of Iterations and the Burnin
#' Defauld are 5000 Iterations and a Burnin of 1000.
model.gpp.p2 <- fit_sp_model(data = data,
                             formula = formula,
                             model = 'GPP',
                             priors = priors.gpp,
                             cov.fnc = cov.fnc,
                             nItr = 10000,
                             nBurn = 2000,
                             knots_count = 4,
                             training_set = training_set,
                             scale.transform = scale.transform,
                             spatial.decay = spatial.decay)
pred.gpp.p2 <- predict(model.gpp.p2, data, training_set)
evaluate_prediction(pred.gpp.p2)



# Modelling intraday data -------------------------------------------------
#' # Modelling intraday data
#' We load the intraday data of Munich for PM2.5 in 8h intervalls
data("muc_airPol_p2_8h")
#' remove outliers in the data
data <- clean_model_data(muc_airPol_p2_8h, timesIQR = 1.5)

#' We add intercepts for the weekday and the hour
formula = value ~ humi + temp + rainhist + windhist +
  trafficvol + log(sensor_age) + stAirPol::weekday(timestamp) +
  stAirPol::hour(timestamp)- 1
model.gpp <- fit_sp_model(data = data,
                          formula = formula,
                          model = 'GPP',
                          priors = priors.gpp,
                          cov.fnc = cov.fnc,
                          knots_count = 4,
                          report = 5,
                          training_set = training_set,
                          scale.transform = scale.transform,
                          spatial.decay = spatial.decay)

pred.gpp <- predict(model.gpp, data, training_set)
evaluate_prediction(pred.gpp)



# Analyze fitted parameters -----------------------------------------------
#' # Analyze fitted parameters
#' back to daily data
#' We load the daily data of Munich for PM2.5
data("muc_airPol_p2")
#' remove outliers in the data
data <- clean_model_data(muc_airPol_p2, timesIQR = 1.5)
#' We use the return parameter of fit_subintervalls() for letting the function
#' return the model parameters by setting return = 'parameters'.
formula = value ~ humi + temp + rainhist + windhist +
  trafficvol + log(sensor_age)
para.1week <- fit_subintervalls(data = data,
                                return = 'parameters',
                                formula = formula,
                                model = 'GPP',
                                priors = priors.gpp,
                                cov.fnc = cov.fnc,
                                knots_count = 4,
                                training_set = training_set,
                                unit = '11 days',
                                scale.transform = scale.transform,
                                spatial.decay = spatial.decay)
#' plot boxplots of the posteriori Mean for each parameter
#' we used a splitinterval of 11 days, to avoid that the 31. is in a single
#' interval
plot(para.1week)



# How to choose the knots --------------------------------------------------
#' # How to choose the knots

#' There are three methods how to specify the knots
#'          (1) Use a matrix of your favorite knots coordinates
#'          (2) Specify a regular grid by choosing the column and row count
#'          (3) Choose a random knots location based on the density of the
#'              position of the sensors.
#' For more details see the master-thesis:
#'       Spatio-temporal modelling of air pollution open data


#' (1): I don't have my favorite locations in Munich in coordinates, so I choose
#' randomly 20 grid points from the dataset muc_airPol_p2_grid
data("muc_airPol_p2_grid")
set.seed(2202)
count <- nrow(unique(muc_airPol_p2_grid[, .(lon, lat)]))
knots <- as.matrix(unique(muc_airPol_p2_grid[, .(lon, lat)])[sample(1:count, 20)])
model.gpp.selected <- fit_sp_model(data = data,
                             formula = formula,
                             model = 'GPP',
                             priors = priors.gpp,
                             cov.fnc = cov.fnc,
                             knots = knots,
                             knots_plot = TRUE,
                             training_set = training_set,
                             scale.transform = scale.transform,
                             spatial.decay = spatial.decay)

pred.gpp.selected <- predict(model.gpp.selected, data, training_set)

#' (2) I want a 5 times 4 grid, so I set knots_count = c(5,4)
model.gpp.grid <- fit_sp_model(data = data,
                          formula = formula,
                          model = 'GPP',
                          priors = priors.gpp,
                          cov.fnc = cov.fnc,
                          knots_count = c(5,4),
                          knots_plot = TRUE,
                          training_set = training_set,
                          scale.transform = scale.transform,
                          spatial.decay = spatial.decay)
pred.gpp.grid <- predict(model.gpp.grid, data, training_set)

#' (3) I want to set 20 random knots based on the sensor density
#'
model.gpp.random <- fit_sp_model(data = data,
                          formula = formula,
                          model = 'GPP',
                          priors = priors.gpp,
                          cov.fnc = cov.fnc,
                          knots_count = 20,
                          knots_method = 'random',
                          knots_plot = TRUE,
                          knots_seed = 220292,
                          training_set = training_set,
                          scale.transform = scale.transform,
                          spatial.decay = spatial.decay)
pred.gpp.random <- predict(model.gpp.random, data, training_set)

#' Compair the results
evaluate_prediction_table(list('pred.gpp.selected' = pred.gpp.selected,
                               'pred.gpp.grid' = pred.gpp.grid,
                               'pred.gpp.random' = pred.gpp.random))
