#' get_shape_for_plz
#'
#' @param plz a character vector which contains all German postcodes of the
#' area.
#'
#' @return shapefile for that German postcodes
#' @export
#'
#' @examples
#' get_shape_for_plz(80993)
#' get_shape_for_plz(83487)
get_plz_shape <- function(plz) {
  data(plz_shape)
  plz_shape <- plz_shape[plz_shape$plz %in% plz, ]
  plz_shape <- plz_shape[!duplicated(plz_shape$ID_4), ]
}
