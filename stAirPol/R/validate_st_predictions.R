#' get_test_and_training_set
#'
#' Split the sensors into a traning a test set
#'
#' @param sensors modeling \libnk{data.table}, should contain informations about
#' the sensor_id and the coordianted in the columns \code{lon} and \code{lat}
#' @param sampel_size double beween 0 and 1, which specifies the percentage of
#' training sensors
#' @param random.seed for reproducibly reasons
#'
#' @export
#'
#' @examples
#' data("mini_dataset")
#' get_test_and_training_set(mini_dataset)
get_test_and_training_set <- function(sensors, sampel_size = 0.75,
                                      random.seed = NULL) {
  sensors <- sensors[!duplicated(sensor_id)][, .(id, sensor_id, lon , lat)]
  if (!is.null(random.seed)) set.seed(random.seed)
  train <- sample(sensors$sensor_id, round(length(sensors$sensor_id) * sampel_size))
  test <- sensors[!(sensor_id %in% train)]$sensor_id
  test_set <- list(test = test, train = train, data = sensors, sampel_size = sampel_size)
  class(test_set) <- 'spAirPol.valdidation_set'
  test_set
}

#' print.spAirPol.valdidation_set
#'
#' @param set a object as returned by \link{get_test_and_training_set}
#'
#' @return Informations about the training set
#' @export
#'
#' @examples
#' data("mini_dataset")
#' training_set <- get_test_and_training_set(mini_dataset)
#' print(training_set)
print.spAirPol.valdidation_set <- function(set) {
  cat("Count Ids  :  ", length(set$train) + length(set$test) ,"\n")
  cat("Sample Size:  ", set$sampel_size, '% \n')
  cat("----------------------------------------------------------\n\n")
  cat("Train Sensor_id:  ", set$train, '\n \n')
  cat("Test Sensor_id:  ", set$test, '\n')
}

#' plot.spAirPol.valdidation_set
#'
#' Plot the locations of the training and test sensors
#'
#' @param set a object as returned by \link{get_test_and_training_set}
#'
#' @export
#'
#' @examples
#' data("mini_dataset")
#' training_set <- get_test_and_training_set(mini_dataset)
#' plot(training_set)
plot.spAirPol.valdidation_set <- function(set) {
  data <- set$data
  data$type <- NA_character_
  data[sensor_id %in% set$test]$type <- 'test'
  data[sensor_id %in% set$train]$type <- 'train'
  g <- ggplot(data = data, aes(x = lon, y = lat, pch = type, col = type)) +
    geom_point() +
    theme_classic()
  print(g)
  g
}

