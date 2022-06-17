#' Title
#'
#' @param object
#' @param option
#' @param datasources
#'
#' @return
#' @export
#'
#' @examples
ds.boundedCovarianceDP <- function(datasources, x, y, epsilon, x_min, x_max, y_min, y_max) {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  cally <- paste0("boundedCovarianceDP(", x, ", ", y,  ", ", epsilon, ", ", x_min, ", ", x_max, ", ", y_min, ", ", y_max, ")")
  result <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  return(result)
}
