#' get_nuts_for_plz
#'
#' Map plz to NUTS_3
#'
#' @param plz numeric vector of German postcodes
#'
#' @return character vectors of unique NUTS_3
#' @export
#' @examples
#' get_nuts_for_plz(80993)
#' get_nuts_for_plz(c(80993, 83487))
get_nuts_for_plz <- function(plz) {
  data('NUTS_3')
  NUTS_3 <- NUTS_3[CODE %in% plz]
  NUTS_3 <- NUTS_3[!duplicated(NUTS_3)]
  NUTS_3$NUTS_3 <- gsub("'", "", NUTS_3$NUTS_3)
  nut3 <- NUTS_3$NUTS_3
  nut3
}
