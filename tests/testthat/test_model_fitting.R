
library(stAirPol)
library(testthat)


assignInNamespace("cedta.override",
                  c(data.table:::cedta.override,"<stAirPol>"),
                  "data.table")


context("fit_model")

test_that("fit model", {
  data("mini_dataset")
  mini_dataset <- clean_model_data(mini_dataset)
  formula <-  value ~ rainhist + windhist + trafficvol
  model.gpp <- fit_sp_model(data = mini_dataset, formula = formula, model = 'GPP')
  expect_equal(model.gpp$parameter$Mean[1], 44, tol = 1e-1)
})

test_that("fit subintervalls I", {
  data("mini_dataset")
  mini_dataset <- clean_model_data(mini_dataset)
  formula = value ~ humi + temp
  training_set <- get_test_and_training_set(mini_dataset, sampel_size = 0.75,
                                            random.seed = 220292)
  prediction.gp <- fit_subintervalls(data = mini_dataset, formula = formula,
                              model = 'GPP', training_set = training_set,
                              unit = '1 day')
  expect_equal(nrow(prediction.gp), 108)
  expect_equal(sum(prediction.gp$value, na.rm = TRUE), 1523, tol = 1e-1)
  expect_equal(sum(prediction.gp$prediction), 1810, tol = 1e01)
})

test_that("fit subintervalls II", {
  data("mini_dataset")
  mini_dataset <- clean_model_data(mini_dataset)
  formula = value ~ humi + temp
  training_set <- get_test_and_training_set(mini_dataset, sampel_size = 0.75,
                                            random.seed = 220292)
  prediction.gp <- fit_subintervalls(data = mini_dataset, formula = formula,
                              model = 'GPP', training_set = training_set,
                              unit = '3 days')
  expect_equal(nrow(prediction.gp), 108)
  expect_equal(sum(prediction.gp$value, na.rm = TRUE), 1523, tol = 1e-1)
  expect_equal(sum(prediction.gp$prediction), 1828, tol = 1e-1)
})
