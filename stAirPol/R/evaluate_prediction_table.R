#' evaluate_prediction_table
#'
#' Formats a table of diffrent prediction data.frames
#'
#' @param list a named list with elements of objects as returned by
#' \link{predict.stAirPol.model}
#'
#' @return a data.table
#' @import data.table
#' @export
evaluate_prediction_table <- function(list) {
  l <- lapply(list, function(x) evaluate_prediction(pred = x))
  l <- rbindlist(l)
  l$model <- names(list)
  l[, c(6,1:5)]
}
