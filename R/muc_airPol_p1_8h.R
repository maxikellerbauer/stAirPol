#' muc_airPol_p1
#'
#' A dataset which contains observations of PM10 and a lot of covariables in
#' Munich December 2017.
#'
#'
#' @format A data table with 7812 rows and 17 variables
#' \describe{
#'   \item{timestamp}{timestamp rounded down}
#'   \item{sensor_id}{numeric sensor_id}
#'   \item{lon}{longitudinal value of the position}
#'   \item{lat}{latitudinal value value of the position}
#'   \item{variable}{Variable Type, P1 for PM10 or P2 for PM2.5}
#'   \item{sensor_type}{sensor_type, currently nearly always SDS011}
#'   \item{location}{location indicator}
#'   \item{value}{measured value of PM}
#'   \item{humi}{humiditiy in percent}
#'   \item{temp}{temperature in degree Celsius}
#'   \item{rain}{rain in millimeters}
#'   \item{rainhist}{transformed historical rain}
#'   \item{wind}{wind velocity in m/s}
#'   \item{windhist}{transformed historical wind}
#'   \item{id}{numeric id}
#'   \item{trafficvol}{calculated trafficvolumne}
#'   \item{sensor_age}{sensor age in weeks}
#' }
#'
#' @docType data
#'
#' @usage data(muc_airPol_p1_8h)
#'
#' @keywords datasets
#'
#'
#' @examples
#' data(muc_airPol_p1_8h)
"muc_airPol_p1_8h"
