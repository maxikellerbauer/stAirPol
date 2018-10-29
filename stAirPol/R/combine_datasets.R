#' Combine all calculated datases
#'
#' @return nothing, but four objects are assigned to the global envrioment
#' @export
#'
#' @examples
#' combine_datasets()
combine_datasets <- function() {
  data.p1 <- get_model_frame(sensor_data[['P1']], sensors[['P1']], expand = TRUE)
  data.final.p1 <<- data.table(Reduce(function(x, y) dplyr::left_join(x, y),
                                     list(data.p1, data.humi.p1, data.temp.p1,
                                          data.rainhist.p1, data.windhist.p1,
                                          data.traffic.p1, sensor_age)))
  grid.final.p1 <- data.table(Reduce(function(x, y) dplyr::left_join(x, y),
                                     list(grid.humi.p1, grid.temp.p1,
                                          grid.rainhist.p1, grid.windhist.p1,
                                          grid.traffic.p1)))

  data.p2 <- get_model_frame(sensor_data[['P2']], sensors[['P2']], expand = TRUE)
  data.final.p2 <<- data.table(Reduce(function(x, y) dplyr::left_join(x, y),
                                     list(data.p2, data.humi.p2, data.temp.p2,
                                          data.rainhist.p2, data.windhist.p2,
                                          data.traffic.p2, sensor_age)))
  grid.final.p2 <<- data.table(Reduce(function(x, y) dplyr::left_join(x, y),
                                     list(grid.humi.p2, grid.temp.p2,
                                          grid.rainhist.p2, grid.windhist.p2,
                                          grid.traffic.p2)))
}
