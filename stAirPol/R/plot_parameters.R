#' Title
#'
#' @param parmeter
#'
#' @return
#' @export
#'
#' @examples
plot.stAirPol.parameters <- function(parameter) {
  g2 <- ggplot(data = parameter, aes(x = name, group = name, y = Mean)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, col = 'red', lty = 2) +
    facet_wrap(name ~ ., scales = 'free') +
    theme_classic() +
    theme(legend.position = 'none') +
    xlab(NULL) +
    ylab('est. parameter')
  g2
}
