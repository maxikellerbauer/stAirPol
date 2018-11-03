#' clean_model_data
#'
#' Remove outliers from the dataset. If an outlier is detected, the entry in
#' the column value is set to NA.
#'
#' @param model_data Dataset as a data.table which one-time dimension in the
#' variable timestamp and one numeric variable with the name values
#' @param times_IQR remove outliers, which are more then times_IQR * IQR away
#' from the median
#'
#' @return model_data, with all values, which are outside the timesIQR plus minus
#' the median, are in one timestamp set to NA
#' @export
clean_model_data <- function(model_data, timesIQR = 1.5) {
  model_data[, median := median(value, na.rm = TRUE), by = timestamp]
  model_data[, IQR := IQR(value, na.rm = TRUE), by = timestamp]
  model_data[ (value <= median - timesIQR * IQR |
                value >= median + timesIQR * IQR) & IQR > 0]$value <- NA
  model_data$IQR <- NULL
  model_data$median <- NULL
  model_data
}
