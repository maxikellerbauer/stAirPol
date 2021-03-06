## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 10,
  fig.height = 6,
  comment = "#>"
)

## ----messages=FALSE,warning=FALSE, echo=FALSE, results='hide'------------
require(stAirPol)
require(spTimer)
require(data.table)
require(ggplot2)

## ------------------------------------------------------------------------
data("muc_airPol_p2")

## ------------------------------------------------------------------------
data <- clean_model_data(muc_airPol_p2, timesIQR = 1.5)

## ------------------------------------------------------------------------
training_set <- get_test_and_training_set(data, sampel_size = 0.75,
                                          random.seed = 220292)

## ------------------------------------------------------------------------
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


## ------------------------------------------------------------------------
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
pred.gpp.wdays <- predict(model.gpp.wdays, data, training_set)
summary(pred.gpp.wdays)
model_result_list <- list('pred.gpp.p2' = pred.gpp.p2,
                          'pred.gpp.wdays' = pred.gpp.wdays)
evaluate_prediction_table(model_result_list)

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
data("muc_airPol_p2_8h")

## ------------------------------------------------------------------------
data <- clean_model_data(muc_airPol_p2_8h, timesIQR = 1.5)

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
data("muc_airPol_p2")

## ------------------------------------------------------------------------
data <- clean_model_data(muc_airPol_p2, timesIQR = 1.5)

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
plot(para.1week)

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
evaluate_prediction_table(list('pred.gpp.selected' = pred.gpp.selected,
                               'pred.gpp.grid' = pred.gpp.grid,
                               'pred.gpp.random' = pred.gpp.random))

