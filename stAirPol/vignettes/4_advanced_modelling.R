require(stAirPol)
require(spTimer)


# Read the data -----------------------------------------------------------
data("muc_airPol_p2")
data <- clean_model_data(muc_airPol_p2)
data <- data[timestamp != max(timestamp)]

training_set <- get_test_and_training_set(data, sampel_size = 0.75,
                                          random.seed = 220292)

#' First we try to manipulate the Kovariables. there are a lot of diffent ways.
#' We will add induvidual Intercept for each weekday
formula = value ~ humi + temp + rainhist + windhist +
  trafficvol + log(sensor_age) + stAirPol::weekday(timestamp) - 1


#' For speeding up the computation we only use the GPP model
priors.gpp <- spT.priors(model = "GPP", inv.var.prior = Gamm(a = 2, b = 1),
                         beta.prior = Norm(0, 10^4))
spatial.decay = spT.decay(distribution = Gamm(a = 2, b = 1), tuning = 0.25)
scale.transform = "SQRT"
cov.fnc = "exponential"
model.gpp <- fit_sp_model(data = data,
                             formula = formula,
                             model = 'GPP',
                             priors = priors.gpp,
                             cov.fnc = cov.fnc,
                             knots_count = 25,
                             knots_method = 'random',
                             report = 5,
                             training_set = training_set,
                             scale.transform = scale.transform,
                             spatial.decay = spatial.decay)

pred.gpp <- predict(model.gpp, data, training_set)
summary(model.gpp)
evaluate_prediction(pred.gpp)
plot(pred.gpp)
plot(pred.gpp, time_dimension = TRUE)

#' Now we want to train a single GPP model for each week
formula = value ~ humi + temp + rainhist + windhist +
  trafficvol + log(sensor_age)
pred.gpp <- fit_subintervalls(data = data,
                          formula = formula,
                          model = 'GPP',
                          priors = priors.gpp,
                          cov.fnc = cov.fnc,
                          knots_count = 3,
                          knots_method = 'grid',
                          training_set = training_set,
                          unit = '1 week',
                          scale.transform = scale.transform,
                          spatial.decay = spatial.decay)

evaluate_prediction(pred.gpp)
plot(pred.gpp)
plot(pred.gpp, time_dimension = TRUE)

#' we are able to train every subintervall we want, here for example 3 days
set.seed(2202)
pred.gpp <- fit_subintervalls(data = data,
                          formula = formula,
                          model = 'GPP',
                          priors = priors.gpp,
                          cov.fnc = cov.fnc,
                          knots_count = 3,
                          knots_method = 'grid',
                          training_set = training_set,
                          nItr = 10000,
                          nBurn = 2000,
                          unit = '3 days',
                          scale.transform = scale.transform,
                          spatial.decay = spatial.decay)

evaluate_prediction(pred.gpp)
plot(pred.gpp)
plot(pred.gpp, time_dimension = TRUE)

#' we are able to train every subintervall we want, here for example 3 days
pred.gpp <- fit_subintervalls(data = data,
                          formula = formula,
                          model = 'GPP',
                          priors = priors.gpp,
                          cov.fnc = cov.fnc,
                          knots_count = 4,
                          knots_method = 'grid',
                          training_set = training_set,
                          nItr = 10000,
                          nBurn = 2000,
                          unit = '1 day',
                          scale.transform = scale.transform,
                          spatial.decay = spatial.decay)

evaluate_prediction(pred.gpp)
plot(pred.gpp)
plot(pred.gpp, time_dimension = TRUE)

