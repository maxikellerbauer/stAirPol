evaluate_prediction_table <- function(list) {
  l <- lapply(list, function(x) evaluate_prediction(pred = x))
  l <- rbindlist(l)
  l$model <- names(list)
  l[, c(6,1:5)]
}
