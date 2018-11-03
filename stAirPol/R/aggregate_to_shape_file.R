#' aggregate_to_shape_file
#'
#' Aggregate the spatio-temporal prediction to the elements of a shape file
#'
#' @param prediction a spatio-temporal prediction as returned by \link{predict.stAirPol.model}
#' @param shape a shapefile of the class sd
#' @param scale_bar_dist Kilometers of the scalebar
#' @param mc.cores how much cores should be used for parallelisation, default is
#' one core less you maximum number of detected cores.
#'
#' @importFrom dplyr left_join
#' @importFrom sf st_crs
#' @importFrom pbmcapply pbmclapply
#' @importFrom sf st_intersects
#' @importFrom sf st_point
#' @importFrom sf st_bbox
#' @importFrom ggsn scalebar
#' @import ggplot2
#'
#' @return a ggplot object with the aggregation map
#' @export
aggregate_to_shape_file <- function(prediction, shape, scale_bar_dist = 3,
                                    mc.cores = parallel::detectCores() - 1) {
  grid_coords <- unique(prediction[, .(lon, lat, sensor_id)])
  grid_coords$shape <- as.numeric(pbmcapply::pbmclapply(1:nrow(grid_coords),
                  function(i) {
                    which(sf::st_intersects(sf::st_point(c(grid_coords[i]$lon,
                                                           grid_coords[i]$lat)),
                                            shape$geometry, sparse = FALSE))
                  }, mc.cores = 4))
  prediction <- data.table(dplyr::left_join(prediction, grid_coords))
  prediction.agg <- prediction[, .(prediction = mean(prediction)), by = shape]
  #' remove points which does not contain in the shapefile
  prediction.agg <- prediction.agg[!is.na(shape)][order(shape)]
  shape$mean <- 0
  shape$mean[prediction.agg$shape] <- prediction.agg$prediction
  #' remove shapes where no gridpoints are detected
  shape <- shape[shape$mean != 0, ]
  #' Plot the calculated aggregation
  g <- ggplot(data = shape) +
    geom_sf(aes(fill = mean)) +
    coord_sf(crs = sf::st_crs(shape), datum = NA) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1) +
    guides(fill = guide_colorbar(barwidth = 1.5, barheight = 20),
           shape = "colorbar") +
    theme_void() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.title = element_blank(),
          panel.background = element_blank()) +
    ggsn::scalebar(dist = scale_bar_dist, dd2km = TRUE, model = 'WGS84',
                   location = "bottomleft", st.dist = 0.05,
                   x.min = sf::st_bbox(shape)[1],
                   y.min = sf::st_bbox(shape)[2],
                   x.max = sf::st_bbox(shape)[3],
                   y.max = sf::st_bbox(shape)[4])
  g
}
