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
ds.boundedSumDP <- function(datasources, input_data, epsilon, lower_bound, upper_bound, type="combine") {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  cally <- paste0("boundedSumDP(", input_data, ", ", epsilon, ", ", lower_bound, ", ", upper_bound, ")")
  res <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  combined <- sum(unlist(res))

  if (type=="combine") return(list(Sum.by.Study=res))
  if (type=="split") return(list(Global.Sum=combined))
  if (type=="both") return(list(Sum.by.Study=res,Global.Sum=combined))
}
