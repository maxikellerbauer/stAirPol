#' get_opentransportmap_data
#'
#' @param trafficvol_treshold A threshold, which roads should be ignored, default
#' is 1, due to the logarithmical transformation of the data
#' @param plz a vector a German postcode for that the traffic data will be
#' @param path filepath where the informations are stored
#' @param mc.cores how much cores should be used for parallelization, default is
#' one core less your maximum number of detected cores.
#'
#' @return a sfc object of the roads
#' @export
#' @import pbmcapply
#'
#' @examples
#' get_opentransportmap_data('80993')
get_opentransportmap_data <- function(plz, path, trafficvol_treshold = 1,
                                      mc.cores = parallel::detectCores() - 1) {
  roads <- pbmcapply::pbmclapply(get_nuts_for_plz(plz), function(x) {
                                 load_opentransportmap_data_nut3(nut3 = x,
                                                                 path = path)},
                                 mc.cores = mc.cores)
  lapply(roads, function(x) x[x$trafficvol > trafficvol_treshold, ])
}


#' load_opentransportmap_data_nut3
#'
#' @param nut3 a character vector of nuts3, for more informations see
#' \url{https://de.wikipedia.org/wiki/NUTS}

#'
#' @return a sfc object of the roads
#' @examples
#' load_opentransportmap_data_nut3("DE212", "./")
load_opentransportmap_data_nut3 <- function(nut3, path) {
  if (!file.exists(paste0(path,
                          '/otm/roadlinks_', nut3, '.shp'))) {
    system(paste0('cd ', path,
                  '/otm/; unzip -n ', nut3, '.zip'))
  }
  roads <- sf::read_sf(paste0(path, '/otm/roadlinks_',
                              nut3, '.shp'))
}
