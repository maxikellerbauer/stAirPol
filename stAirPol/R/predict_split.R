#' predict_split
#'
#' Perform all predictions on subsample datasets to avoid a shortage of RAM.
#'
#' @param model a spatio-temporal model as returned by \link{fit_sp_model}
#' @param newdata a dataset with informations of the covariable
#' @param sample_count how much subsamples?, default is 100
#' @param mc.cores how much cores should be used for parallelization, default is
#' one core less your maximum number of detected cores.
#' @param ...
#'
#' @return the newdata with added columns of the prediction calculations
#' @export
#' @importFrom pbmcapply pbmclapply
#'
#' @seealso \link{fit_subintervalls}, \link{predict.stAirPol.model},
#' \link{predict_split} \link{fit_model}
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
#' pred.gp <- predict_split(model.gp, mini_dataset, training_set)
predict_split <- function(model, new_data, sample_count = 100,
                          mc.cores = parallel::detectCores() - 1, ...) {
  new_data.list <- split(new_data, new_data$sensor_id %% 100)
  prediction.list <- pbmcapply::pbmclapply(1:length(new_data.list), function(i) {
    predict(model, new_data.list[[i]])
  }, mc.cores = mc.cores)
  rbindlist(prediction.list)
}
