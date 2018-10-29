#' make_grid
#'
#' @param lambda numerical smoothing parameter
#' @param plz numerical vector of german post codes
#' @param scale numericvalue to scale the traffic volumen, defauld is 100
#' @param mc.cores how much cores should be used for parallelisation, default is
#' one core less you maximum number of detected cores.
#'
#' @return a regular grid which covered the plz areas
#' @export
#'
#' @import pbmcapply
#' @import data.table
#' @importFrom parallel detectCores
#'
#' @examples
#' make_grid(plz = 80993, lambda = 0.01)
make_grid_traffic <- function(lambda, plz, scale = 100,
                      mc.cores = parallel::detectCores() - 1) {
  plz_shape <- get_plz_shape(plz)
  grid <- sf::st_make_grid(plz_shape, cellsize = m.grid_cellsize,
                           what = 'centers')
  plz_shape <- sf::st_set_crs(plz_shape, '+proj=longlat +datum=WGS84 +no_defs')
  grid <- sf::st_set_crs(grid, '+proj=longlat +datum=WGS84 +no_defs')
  grid <- points_inside_multipolygon(grid, plz_shape)
  grid <- sf::st_sf(grid)
  ll <- pbmcapply::pbmclapply(1:nrow(grid), function(x) {
    sum(sapply(roads, function(y) smooth_loaction(y, grid[x,], lambda = lambda)))
  }, mc.cores = mc.cores)
  grid$trafficvol <- unlist(ll) / scale
  grid_cords <- data.table(sf::st_coordinates(grid$grid))
  grid$lon <- grid_cords$X
  grid$lat <- grid_cords$Y
  grid <- as.data.table(grid)
  grid$grid <- NULL
  grid
}



#' estimate_grid_size
#'
#' Estimate the size of the choosen grid parameters
#'
#' @param plz a character vector which contains all german postcodes of the
#' area which should be used.
#' @param grid_cellsize  \link{sf::st_make_grid} for informations about the
#' cellsize parameter
#'
#' @export
#' @import sf
#' @examples
#' estimate_grid_size('80993', 0.01)
estimate_grid_size <- function(plz, grid_cellsize) {
  plz_shape <- get_plz_shape(plz)
  grid <- sf::st_make_grid(plz_shape, cellsize = m.grid_cellsize,
                           what = 'centers')
  plz_shape <- sf::st_set_crs(plz_shape, '+proj=longlat +datum=WGS84 +no_defs')
  grid <- sf::st_set_crs(grid, '+proj=longlat +datum=WGS84 +no_defs')
  grid <- spAirPol:::points_inside_multipolygon(grid, plz_shape)
  grid <- sf::st_sf(grid)
  cat('Estimated grid size:  ', nrow(grid), 'points \n')
  cat('Area per gridpoint:  ', round(sum(sf::st_area(plz_shape)) / nrow(grid) / 1000^2, 3), "km^2")
}
