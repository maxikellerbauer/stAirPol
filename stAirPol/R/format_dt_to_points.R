#' format_dt_to_points
#'
#' Format a data.table with the columns lon and lat to a set of \link{sf::st_point}
#'
#' @param dt a datatable contains lon and lat as variable
#' @import sf
#' @export
#'
#' @return Geometry set of all points
format_dt_to_points <- function(dt) {
  points <-
    lapply(1:nrow(dt), function(x)
      sf::st_point(as.matrix(dt[x, .(X = lon, Y = lat)])))
  points <- sf::st_sfc(points)
  points <- sf::st_set_crs(points, '+proj=longlat +datum=WGS84 +no_defs')
  points
}


#' points_inside_multipolygon
#'
#' check which points are contained by the multipolygon and return them.
#'
#'
#' @param points points of type sfc
#' @param multipolygon polygon of type MULTIPOLYGON
#' @export
#'
#' @return points which are inside the multipolygon
points_inside_multipolygon <- function(points, multipolygon) {
  points <- sf::st_set_crs(points, '+proj=longlat +datum=WGS84 +no_defs')
  multipolygon <- sf::st_set_crs(multipolygon, '+proj=longlat +datum=WGS84 +no_defs')
  grid_points_inside <- lapply(1:nrow(multipolygon),
                               function(x)
                                 which(
                                   sf::st_intersects(points,
                                                     multipolygon$geometry[x],
                                                     sparse = FALSE)
                                 ))
  points[unique(unlist(grid_points_inside))]
}
