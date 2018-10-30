require(stAirPol)
require(spTimer)


# Read the data -----------------------------------------------------------
data("muc_airPol_p2")
data <- clean_model_data(muc_airPol_p2)
data <- data[timestamp != max(timestamp)]
data.fit <- data[timestamp < quantile(timestamp, 0.75)]
data.predict <- data[timestamp >= quantile(timestamp, 0.75)]



formula = value ~ humi + temp + rainhist + windhist +
  trafficvol + log(sensor_age)


#' For speeding up the computation we only use the GPP model
priors.gpp <- spT.priors(model = "GPP", inv.var.prior = Gamm(a = 2, b = 1),
                         beta.prior = Norm(0, 10^4))
spatial.decay = spT.decay(distribution = Gamm(a = 2, b = 1), tuning = 0.25)
scale.transform = "SQRT"
cov.fnc = "exponential"
model.gpp <- fit_sp_model(data = data.fit,
                          formula = formula,
                          model = 'GPP',
                          priors = priors.gpp,
                          cov.fnc = cov.fnc,
                          knots_count = 20,
                          knots_method = 'random',
                          report = 5,
                          scale.transform = scale.transform,
                          spatial.decay = spatial.decay)

forecast <- predict(model.gpp, newdata = data.predict,
                    type = 'temporal', foreStep = length(unique(data.predict$timestamp)))

evaluate_prediction(forecast)
plot(forecast, time_dimension = TRUE)
