#' Rename all datasets
#'
#' set the target value to the correct name
#' @export
#'
#' @examples
#' rename_datasets()
rename_datasets <- function() {
  setnames(data.rainhist.p1, "prediction", 'rain')
  setnames(grid.rainhist.p1, "prediction", 'rain')
  setnames(data.rainhist.p2, "prediction", 'rain')
  setnames(grid.rainhist.p2, "prediction", 'rain')
  setnames(data.windhist.p1, "prediction", 'wind')
  setnames(grid.windhist.p1, "prediction", 'wind')
  setnames(data.windhist.p2, "prediction", 'wind')
  setnames(grid.windhist.p2, "prediction", 'wind')
  setnames(data.rainhist.p1, "hist", 'rainhist')
  setnames(grid.rainhist.p1, "hist", 'rainhist')
  setnames(data.rainhist.p2, "hist", 'rainhist')
  setnames(grid.rainhist.p2, "hist", 'rainhist')
  setnames(data.windhist.p1, "hist", 'windhist')
  setnames(grid.windhist.p1, "hist", 'windhist')
  setnames(data.windhist.p2, "hist", 'windhist')
  setnames(grid.windhist.p2, "hist", 'windhist')
  setnames(data.humi.p2, "prediction", 'humi')
  setnames(grid.humi.p2, "prediction", 'humi')
  setnames(data.temp.p2, "prediction", 'temp')
  setnames(grid.temp.p2, "prediction", 'temp')
  setnames(data.humi.p1, "prediction", 'humi')
  setnames(grid.humi.p1, "prediction", 'humi')
  setnames(data.temp.p1, "prediction", 'temp')
  setnames(grid.temp.p1, "prediction", 'temp')
}
