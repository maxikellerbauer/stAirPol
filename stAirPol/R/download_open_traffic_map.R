#' Download Open Transport Map Data
#'
#' That functions downloads all informations about traffic volumen in Germany.
#' See \url{http://opentransportmap.info/} for more informations.
#' @param path filepath where the informations should be stored
#'
#' @export
#'
#' @examples
#' download_OTM(path = './')
download_OTM <- function(path) {
  data("NUTS_3")
  sapply(unique(gsub("'", "", unique(NUTS_3$NUTS_3))), function(i) {
    try({
      download.file(paste0('http://opentransportmap.info/download/nuts-3/', i),
                    destfile = paste0(path, i, '.zip'))
      Sys.sleep(time = runif(1,0.07,0.19))
    })
  })
  NULL
}
