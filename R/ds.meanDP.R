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
ds.meanDP <- function(object, option, datasources = NULL) {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  cally <- paste0("basicFunctionDS(", object, ", ", option, ")")
  result <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  return(result)

}
