## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 8,
  fig.height = 6,
  comment = "#>"
)

## ----messages=FALSE,warning=FALSE, echo=FALSE, results='hide'------------

require(stAirPol)
require(spTimer)
require(data.table)

## ------------------------------------------------------------------------
# Read the data -----------------------------------------------------------

## ------------------------------------------------------------------------
data("muc_airPol_p2")
data <- clean_model_data(muc_airPol_p2)
data.fit <- data[timestamp < quantile(timestamp, 0.5)]
data.predict <- data[timestamp >= quantile(timestamp, 0.5) &
                       timestamp < quantile(timestamp, 0.8)]

## ------------------------------------------------------------------------
training_set <- get_test_and_training_set(data, sampel_size = 0.75,
                                          random.seed = 220292)

## ------------------------------------------------------------------------
formula = value ~ humi + temp + rainhist + windhist +
  trafficvol + log(sensor_age)
priors.gpp <- spT.priors(model = "GPP", inv.var.prior = Gamm(a = 2, b = 1),
                         beta.prior = Norm(0, 10^4))
spatial.decay = spT.decay(distribution = Gamm(a = 2, b = 1), tuning = 0.25)
scale.transform = "SQRT"
cov.fnc = "exponential"
model.gpp.random <- fit_sp_model(data = data.fit,
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
pred <- predict(model.gpp.random, newdata = data.fit,
                    training_set = training_set)
forecast <- predict(model.gpp.random, newdata = data.predict,
                    type = 'temporal', training_set = training_set,
                    foreStep = length(unique(data.predict$timestamp)))

## ------------------------------------------------------------------------
evaluate_prediction(forecast)
evaluate_prediction(pred)

## ------------------------------------------------------------------------
gridExtra::grid.arrange(grobs = list(
  plot(pred, time_dimension = TRUE) + ggtitle('spatio prediction') +
    theme(legend.position = 'bottom'),
  plot(forecast, time_dimension = TRUE) + ggtitle('spatiotemporal prediction') +
    theme(legend.position = 'bottom')
), ncol = 2)





# forcasting intraday data ------------------------------------------------

## ------------------------------------------------------------------------
data("muc_airPol_p2_8h")
data <- clean_model_data(muc_airPol_p2_8h)
data.fit <- data[timestamp < quantile(timestamp, 0.5)]
data.predict <- data[timestamp >= quantile(timestamp, 0.5) &
                       timestamp < quantile(timestamp, 0.75)]

## ----eval=FALSE----------------------------------------------------------
#  formula = value ~ humi + temp + rainhist + windhist +
#    trafficvol + log(sensor_age) + stAirPol::hour(timestamp) - 1
#  
#  priors.gpp <- spT.priors(model = "GPP", inv.var.prior = Gamm(a = 2, b = 1),
#                           beta.prior = Norm(0, 10^4))
#  spatial.decay = spT.decay(distribution = Gamm(a = 2, b = 1), tuning = 0.25)
#  scale.transform = "SQRT"
#  cov.fnc = "exponential"
#  model.gpp.random <- fit_sp_model(data = data.fit,
#                                   formula = formula,
#                                   model = 'GPP',
#                                   priors = priors.gpp,
#                                   cov.fnc = cov.fnc,
#                                   knots_count = 20,
#                                   knots_method = 'random',
#                                   knots_plot = TRUE,
#                                   knots_seed = 220292,
#                                   training_set = training_set,
#                                   scale.transform = scale.transform,
#                                   spatial.decay = spatial.decay)

## ----eval=FALSE----------------------------------------------------------
#  pred <- predict(model.gpp.random, newdata = data.fit,
#                  training_set = training_set)

## ----eval=FALSE----------------------------------------------------------
#  forecast <- predict(model.gpp.random, newdata = data.predict,
#                      type = 'temporal', training_set = training_set,
#                      foreStep = length(unique(data.predict$timestamp)))
#  
#  evaluate_prediction(forecast)
#  evaluate_prediction(pred)
#  
#  gridExtra::grid.arrange(grobs = list(
#    plot(pred, time_dimension = TRUE) + ggtitle('spatial  prediction') +
#      theme(legend.position = 'bottom'),
#    plot(forecast, time_dimension = TRUE) + ggtitle('spatial and temporal prediction') +
#      theme(legend.position = 'nonbottom')
#    ), ncol = 2)

