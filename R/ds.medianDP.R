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
ds.medianDP <- function(datasources, input_data, epsilon, lower_bound, upper_bound) {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  cally <- paste0("medianDP(", input_data, ", ", epsilon, ", ", lower_bound, ", ", upper_bound, ")")
  result <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  return(result)
}