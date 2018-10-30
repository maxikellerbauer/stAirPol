#' hour
#'
#' @param x a timestamp
#'
#' @export
hour <- function(x) {
  as.factor(lubridate::hour(x))
}


#' weekday
#'
#' @param x a timestamp
#'
#' @export
weekday <- function(x) {
  as.factor(lubridate::wday(x))
}

#' intercept
#'
#' @param x a timestamp
#'
#' @export
intercept <- function(x) {
  as.factor(x)
}
