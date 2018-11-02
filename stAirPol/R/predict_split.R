#' Title
#'
#' @param model
#' @param newdata
#' @param sample_count
#' @param mc.cores
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
predict_split <- function(model, new_data, sample_count = 100,
                          mc.cores = parallel::detectCores() - 1, ...) {
  new_data.list <- split(new_data, new_data$sensor_id %% 100)
  prediction.list <- pbmcapply::pbmclapply(1:length(new_data.list), function(i) {
    predict(model, new_data.list[[i]])
  }, mc.cores = mc.cores)
  rbindlist(prediction.list)
}
