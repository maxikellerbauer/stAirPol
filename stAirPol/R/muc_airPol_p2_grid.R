#' muc_airPol_p2_grid
#'
#' A dataset which contains a grid with covariables in
#' Munich December 2017 for predictions of PM2.5.
#'
#'
#' @docType data
#'
#' @format A data table with 129828 rows and 12 variables:
#'
#' \describe{
#'   \item{lon}{longitudinal value of the position}
#'   \item{lat}{latitudinal value value of the position}
#'   \item{humi}{humiditiy in percent}
#'   \item{timestamp}{timestamp rounded down}
#'   \item{temp}{temperature in degree Celsius}
#'   \item{rain}{rain in millimeters}
#'   \item{rainhist}{transformed historical rain}
#'   \item{wind}{wind velocity in m/s}
#'   \item{windhist}{ transformed historical wind}
#'   \item{trafficvol}{calculated trafficvolumne}
#'   \item{sensor_id}{numeric sensor_id}
#'   \item{sensor_age}{sensor age in weeks}
#' }

#'
#' @usage data(muc_airPol_p2_grid)
#'
#' @keywords datasets
#'
#'
#' @examples
#' data(muc_airPol_p2_grid)
"muc_airPol_p2_grid"
