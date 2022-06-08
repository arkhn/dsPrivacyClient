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
ds.meanDP <- function(object, datasources = NULL) {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  cally <- paste0("meanDPDS(", object, ")")
  result <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  return(result)

}
