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
ds.maxDP <- function(datasources, input_data, epsilon, lower_bound, upper_bound, type="combine") {

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  cally <- paste0("maxDP(", input_data, ", ", epsilon, ", ", lower_bound, ", ", upper_bound, ")")
  res <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  combined <- max(unlist(res))

  if (type=="combine") return(list(Max.by.Study=res))
  if (type=="split") return(list(Global.Max=combined))
  if (type=="both") return(list(Max.by.Study=res,Global.Max=combined))
}
